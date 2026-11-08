;;;; gui.lisp — Tk front end (via a `wish` subprocess), view, rendering, events.

(in-package :fancy-pencil)

(defvar *wish-process* nil)
(defvar *wish-in* nil "Stream to wish's stdin.")
(defvar *wish-out* nil "Stream from wish's stdout.")
(defvar *event-buffer* nil)
(defvar *gui-lock* (make-lock* "gui"))

;;; view state
(defvar *mouse-sx* -100) (defvar *mouse-sy* -100) (defvar *mouse-in* nil)
(defvar *view-history* nil)
(defvar *drawn-modcount* -1)
(defvar *hl* (make-hash-table :test #'eq) "Highlighted entities.")
(defvar *grdraw-items* nil)
(defvar *menu-queue* nil "Commands picked from menus while another command was running.")
(defvar *pan-start* nil)
(defvar *req* nil)
(defvar *acq-result* nil)
(defvar *snap-bit* nil)
(defvar *gui-dir* nil)
(defvar *at-line-start* t)
(defvar *palette-shown* nil)
(defvar *prompt-owner* nil "The input request whose prompt the command line is showing.")
(defvar *edit-target* nil "Text object to edit directly (set by a double-click on it).")
(defvar *pick-shift* nil "True when the last left-click was made with Shift held.")
(defvar *press-pos* nil "Screen position of the last left-button press (for press-drag-release windows).")

;;; ---------- process ----------

(defun script-directory ()
  (or *gui-dir*
      (let ((f (or *load-truename* *compile-file-truename*)))
        (and f (directory-namestring f)))))

(defun run-wish (program script)
  #+sbcl
  (let ((p (sb-ext:run-program program (list script "-name" "fancypencil") :search t :wait nil
                                                     :input :stream :output :stream :error t
                                                     :external-format :utf-8)))
    (values p (sb-ext:process-input p) (sb-ext:process-output p)))
  #+ecl
  (multiple-value-bind (stream code p)
      (ext:run-program program (list script "-name" "fancypencil") :input :stream :output :stream :error nil :wait nil
                                             :external-format :utf-8)
    (declare (ignore code))
    (values p stream stream))
  #+ccl
  (let ((p (ccl:run-program program (list script "-name" "fancypencil") :wait nil :input :stream :output :stream :error nil
                                                  :external-format :utf-8)))
    (values p (ccl:external-process-input-stream p) (ccl:external-process-output-stream p))))

(defun start-wish ()
  (let ((script (namestring (merge-pathnames "gui.tcl" (script-directory))))
        (candidates (remove nil (list (getenv "FANCYPENCIL_WISH") "wish" "wish8.6" "wish86" "wish86t" "wish9.0" "wish8.7"))))
    (dolist (c candidates (error "Could not start Tk. Install it (e.g. `sudo apt install tk`) or set FANCYPENCIL_WISH to the wish executable."))
      (multiple-value-bind (p in outs) (ignore-errors (run-wish c script))
        (when (and p in outs)
          (setf *wish-process* p *wish-in* in *wish-out* outs)
          (let ((first-line (ignore-errors (read-line outs nil nil))))
            (when (equal first-line "READY") (return t)))
          (setf *wish-process* nil))))))

(defun tk-send-string (s)
  (with-lock* (*gui-lock*)
    (write-string s *wish-in*)
    (terpri *wish-in*)
    (force-output *wish-in*)))
(defun tk (fmt &rest args) (tk-send-string (apply #'format nil fmt args)))

(defun tq (s)
  "Quote S as a Tcl double-quoted word."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across (string s) do
      (case c
        ((#\\ #\" #\$ #\[ #\]) (write-char #\\ o) (write-char c o))
        (#\Newline (write-string "\\n" o))
        (#\Return nil)
        (#\Tab (write-string "\\t" o))
        (t (write-char c o))))
    (write-char #\" o)))

(defun read-event-line ()
  (let ((l (read-line *wish-out* nil :eof)))
    (when (eq l :eof) (out-console "Tk window closed.") (quit-program))
    l))

(defun next-event ()
  "Next event line; consecutive motion events are coalesced."
  (let ((ev (if *event-buffer* (pop *event-buffer*) (read-event-line))))
    (loop while (and (member (event-type ev) '("M" "D") :test #'string=)
                     (null *event-buffer*) (listen *wish-out*))
          do (let ((nx (read-event-line)))
               (if (string= (event-type nx) (event-type ev))
                   (setf ev nx)
                   (progn (setf *event-buffer* (list nx)) (return)))))
    ev))

(defun event-type (ev) (subseq ev 0 (or (position #\Space ev) (length ev))))
(defun event-rest (ev) (let ((p (position #\Space ev))) (if p (subseq ev (1+ p)) "")))
(defun event-ints (ev)
  (let ((r (event-rest ev)))
    (loop with start = 0 for pos = (position #\Space r :start start)
          for tok = (subseq r start pos)
          collect (or (ignore-errors (round (let ((*read-default-float-format* 'double-float))
                                              (read-from-string tok))))
                      0)
          while pos do (setf start (1+ pos)))))

(defvar *ask-counter* 0)
(defun tk-ask (script)
  "Evaluate SCRIPT in Tk and wait for its result string (other events are kept)."
  (let ((id (incf *ask-counter*)) (kept nil))
    (tk "ask ~D ~A" id (tq script))
    (loop
      (let* ((ev (if *event-buffer* (pop *event-buffer*) (read-event-line))))
        (if (and (string= (event-type ev) "A")
                 (let ((r (event-rest ev))) (eql (ignore-errors (parse-integer r :end (position #\Space r))) id)))
            (let ((r (event-rest ev)))
              (setf *event-buffer* (append *event-buffer* (nreverse kept)))
              (return (let ((p (position #\Space r))) (if p (subseq r (1+ p)) ""))))
            (unless (member (event-type ev) '("M" "X") :test #'string=) (push ev kept)))))))

(defun out-console (s) (format *error-output* "~A~%" s) (force-output *error-output*))

;;; ---------- view transform ----------

(defun vdot3 (a b) (+ (* (px a) (px b)) (* (py a) (py b)) (* (pz a) (pz b))))

(defun view-project (p)
  "View coordinates (u v) of world point P: X,Y in plan view, else along the view's right/up."
  (if (projected-view-p)
      (values (vdot3 p (second *view*)) (vdot3 p (third *view*)))
      (values (px p) (py p))))

(defun view-unproject (u v)
  "The world point under view coordinates U,V: on the ground (XY) plane, or, when that plane is
seen edge-on (Front/Back/Left/Right), on the plane facing the viewer through the origin."
  (let* ((r (second *view*)) (up (third *view*)) (dir (fourth *view*))
         (b (v+ (v* r u) (v* up v))))
    (if (> (abs (pz dir)) 1d-9)
        (v+ b (v* dir (- (/ (pz b) (pz dir)))))
        b)))

(defun w->s (p)
  (multiple-value-bind (u v) (view-project p)
    (values (+ *tx* (/ *cw* 2d0) (* (- u *vx*) *vscale*))
            (- (+ *ty* (/ *ch* 2d0)) (* (- v *vy*) *vscale*)))))
(defun s->uv (sx sy)
  "Screen pixel -> view coordinates (the same as world X,Y in plan view)."
  (pt (+ *vx* (/ (- sx *tx* (/ *cw* 2d0)) *vscale*))
      (- *vy* (/ (- sy *ty* (/ *ch* 2d0)) *vscale*))))
(defun s->w (sx sy)
  (let ((q (s->uv sx sy)))
    (if (projected-view-p) (view-unproject (px q) (py q)) q)))
(defun w->uv (p) (multiple-value-bind (u v) (view-project p) (pt u v)))
(defun gui-pixels->world (n) (/ n *vscale*))
(defun view-rect (&optional (margin 40))
  (let ((hw (/ (+ (/ *cw* 2d0) margin) *vscale*)) (hh (/ (+ (/ *ch* 2d0) margin) *vscale*)))
    (values (- *vx* hw) (- *vy* hh) (+ *vx* hw) (+ *vy* hh))))

(defun push-view () (push (list *vx* *vy* *vscale*) *view-history*)
  (when (> (length *view-history*) 30) (setf *view-history* (subseq *view-history* 0 30))))
(defun zoom-to-box (x0 y0 x1 y1 &optional (fill 0.92d0))
  (let ((w (max 1d-9 (- x1 x0))) (h (max 1d-9 (- y1 y0))))
    (push-view)
    (setf *vx* (/ (+ x0 x1) 2) *vy* (/ (+ y0 y1) 2)
          *vscale* (max 1d-9 (* fill (min (/ *cw* w) (/ *ch* h)))))
    (gui-regen)))
(defun view-extents ()
  "Extents of the drawing in view coordinates (projected, with heights and thickness)."
  (when (and (paper-space-p) (fboundp 'layout-extents))
    (return-from view-extents (funcall 'layout-extents)))
  (if (not (projected-view-p))
      (drawing-extents)
      (let ((u0 nil) (v0 nil) (u1 nil) (v1 nil))
        (dolist (e (live-entities))
          (when (entity-visible-p e)
            (let ((b (ignore-errors (entity-bbox e))) (data (ent-data e)))
              ;; an object in another plane, a solid or a 3D line: its own 3D edges
              (when (or (offplane-p data) (wcs-type-p (dxf 0 data)))
                (dolist (pc (entity-pieces e))
                  (dolist (p (case (first pc) ((:seg :gseg) (list (second pc) (third pc))) (:pt (list (second pc)))))
                    (multiple-value-bind (u v) (view-project (pt3 p))
                      (setf u0 (if u0 (min u0 u) u) u1 (if u1 (max u1 u) u)
                            v0 (if v0 (min v0 v) v) v1 (if v1 (max v1 v) v)))))
                (setf b nil))
              (when b
                (let* ((z0 (entity-elevation data)) (z1 (+ z0 (d (or (dxf 39 data) 0)))))
                  (dolist (x (list (first b) (third b)))
                    (dolist (y (list (second b) (fourth b)))
                      (dolist (z (list z0 z1))
                        (multiple-value-bind (u v) (view-project (pt x y z))
                          (setf u0 (if u0 (min u0 u) u) u1 (if u1 (max u1 u) u)
                                v0 (if v0 (min v0 v) v) v1 (if v1 (max v1 v) v)))))))))))
        (and u0 (list u0 v0 u1 v1)))))

(defun entity-elevation (data)
  "Height of a flat object: LWPOLYLINE's elevation, else the Z of its first point."
  (if (equal (dxf 0 data) "LWPOLYLINE")
      (d (or (dxf 38 data) 0))
      (let ((p (dxf 10 data))) (if (consp p) (pz p) 0d0))))

(defun zoom-extents ()
  (let ((b (view-extents)))
    (if b
        (destructuring-bind (x0 y0 x1 y1) b
          (let ((pad (* 0.02 (max 1d-6 (- x1 x0) (- y1 y0)))))
            (zoom-to-box (- x0 pad) (- y0 pad) (+ x1 pad) (+ y1 pad) 0.96d0)))
        (zoom-to-box 0 0 12 9))))
(defvar *regen-pending* nil)
(defun zoom-at (sx sy factor &optional quick)
  (let ((w (s->uv sx sy)))
    (setf *vscale* (min 1d9 (max 1d-9 (* *vscale* factor))))
    (setf *vx* (- (px w) (/ (- sx *tx* (/ *cw* 2d0)) *vscale*))
          *vy* (+ (py w) (/ (- sy *ty* (/ *ch* 2d0)) *vscale*)))
    (if quick
        ;; instant feedback: scale what is on the canvas; exact regen follows on IDLE
        (progn (tk-send-string (format nil "foreach tg {~A} {.c scale $tg ~D ~D ~,5F ~,5F}~%grips_follow ~D ~D ~,5F"
                                       (quick-tags) sx sy factor factor sx sy factor))
               (setf *regen-pending* t)
               (draw-temp))
        (gui-regen))))
(defun zoom-previous ()
  (let ((v (pop *view-history*)))
    (when v (destructuring-bind (x y s) v (setf *vx* x *vy* y *vscale* s)) (gui-regen))))

;;; ---------- colors ----------

(defun hsv->hex (h s v)
  (let* ((c (* v s)) (hp (/ (mod h 360) 60)) (x (* c (- 1 (abs (- (mod hp 2) 1))))) (m (- v c))
         (rgb (cond ((< hp 1) (list c x 0)) ((< hp 2) (list x c 0)) ((< hp 3) (list 0 c x))
                    ((< hp 4) (list 0 x c)) ((< hp 5) (list x 0 c)) (t (list c 0 x)))))
    (format nil "#~{~2,'0X~}" (mapcar (lambda (k) (round (* 255 (+ k m)))) rgb))))

(defun hex-rgb (h)
  (if (and (stringp h) (= (length h) 7) (char= (char h 0) #\#))
      (list (parse-integer h :start 1 :end 3 :radix 16) (parse-integer h :start 3 :end 5 :radix 16)
            (parse-integer h :start 5 :end 7 :radix 16))
      (list 33 40 48)))
(defun rgb-hex (r g b) (format nil "#~2,'0X~2,'0X~2,'0X" (round r) (round g) (round b)))

(defun background-color ()
  ;; on a layout tab everything is drawn on white paper
  (when (drawing-ctab *dwg*) (return-from background-color "#ffffff"))
  (let ((c (sysvar "BKGCOLOR"))) (if (and (stringp c) (= (length c) 7)) c "#212830")))

(defun light-background-p ()
  "True when the drawing background is light (white objects are then shown black, as in AutoCAD)."
  (destructuring-bind (r g b) (hex-rgb (background-color))
    (> (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 140)))

(defun bg-mix (amount &optional tint)
  "The background moved AMOUNT (0..1) toward its contrast color (or toward TINT, an rgb list)."
  (destructuring-bind (r g b) (hex-rgb (background-color))
    (destructuring-bind (tr tg tb) (or tint (if (light-background-p) '(0 0 0) '(255 255 255)))
      (rgb-hex (+ r (* amount (- tr r))) (+ g (* amount (- tg g))) (+ b (* amount (- tb b)))))))

(defun grid-color (kind)
  (case kind
    (:minor (bg-mix 0.05)) (:major (bg-mix 0.11))
    (:x-axis (bg-mix 0.22 '(210 75 75))) (:y-axis (bg-mix 0.22 '(75 191 90)))))

(defun aci->hex (i)
  (when (and (eql i 7) (light-background-p)) (return-from aci->hex "#000000"))
  (case i
    (1 "#FF0000") (2 "#FFFF00") (3 "#00FF00") (4 "#00FFFF") (5 "#0000FF") (6 "#FF00FF")
    (7 "#FFFFFF") (8 "#808080") (9 "#C0C0C0")
    (250 "#333333") (251 "#505050") (252 "#696969") (253 "#828282") (254 "#BEBEBE") (255 "#FFFFFF")
    (t (if (and (integerp i) (<= 10 i 249))
           (let* ((hue (* 15 (floor (- i 10) 10))) (k (mod i 10))
                  (v (nth (floor k 2) '(1d0 0.8d0 0.6d0 0.5d0 0.3d0)))
                  (s (if (evenp k) 1d0 0.5d0)))
             (hsv->hex hue s v))
           "#FFFFFF"))))

;;; ---------- rendering ----------

(defun fmtn (x) (format nil "~,1F" x))
(defun arc-sweep* (a1 a2 kind) (if (eq kind :circle) +2pi+ (arc-sweep a1 a2)))

(defun clip-seg (x1 y1 x2 y2 xmin ymin xmax ymax)
  "Liang-Barsky. Returns clipped coordinates as 4 values or NIL."
  (let ((t0 0d0) (t1 1d0) (dx (- x2 x1)) (dy (- y2 y1)))
    (flet ((edge (p q)
             (cond ((zerop p) (>= q 0))
                   (t (let ((r (/ q p)))
                        (if (< p 0)
                            (if (> r t1) nil (progn (when (> r t0) (setf t0 r)) t))
                            (if (< r t0) nil (progn (when (< r t1) (setf t1 r)) t))))))))
      (when (and (edge (- dx) (- x1 xmin)) (edge dx (- xmax x1))
                 (edge (- dy) (- y1 ymin)) (edge dy (- ymax y1)))
        (list (+ x1 (* t0 dx)) (+ y1 (* t0 dy)) (+ x1 (* t1 dx)) (+ y1 (* t1 dy)))))))

(defun screen-line (pts color tags &key dash (width 1))
  "Create canvas lines through world points PTS (clipped to the view)."
  (let ((out nil) (m 2000d0))
    (loop for (a b) on pts while b
          do (multiple-value-bind (x1 y1) (w->s a)
               (multiple-value-bind (x2 y2) (w->s b)
                 (let ((c (clip-seg x1 y1 x2 y2 (- m) (- m) (+ *full-cw* m) (+ *full-ch* m))))
                   (when c
                     (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -width ~A~A -tags {~A}"
                                   (fmtn (first c)) (fmtn (second c)) (fmtn (third c)) (fmtn (fourth c))
                                   color width (if dash " -dash {6 3}" "") tags)
                           out))))))
    (nreverse out)))

(defun piece-visible-p (pc)
  (when (projected-view-p) (return-from piece-visible-p t))
  (multiple-value-bind (x0 y0 x1 y1) (view-rect 50)
    (let ((b (pieces-bbox (list pc))))
      (and b (not (or (> (first b) x1) (< (third b) x0) (> (second b) y1) (< (fourth b) y0)))))))

(defun arc-points-3d (c r a1 a2 n)
  "ARC-POINTS at the height of the centre C."
  (let ((z (pz c))) (mapcar (lambda (p) (pt (px p) (py p) z)) (arc-points c r a1 a2 n))))

(defun render-piece (pc color tags &key dash (width 1))
  (when (and (projected-view-p) (member (first pc) '(:circle :arc)))
    ;; seen at an angle a circle is an ellipse: draw it as a fine polyline
    (return-from render-piece
      (destructuring-bind (c r &optional (a1 0d0) (a2 +2pi+)) (rest pc)
        (screen-line (arc-points-3d c r a1 a2 (max 24 (min 360 (ceiling (* r *vscale* (arc-sweep* a1 a2 (first pc)) 0.15)))))
                     color tags :dash dash :width width))))
  (when (piece-visible-p pc)
    (ecase (first pc)
      (:seg (screen-line (list (second pc) (third pc)) color tags :dash dash :width width))
      (:gseg nil)
      (:pt (multiple-value-bind (x y) (w->s (second pc))
             (render-point-glyph x y color tags)))
      (:circle
       (destructuring-bind (c r) (rest pc)
         (if (< (* r *vscale*) 12000)
             (multiple-value-bind (x y) (w->s c)
               (let ((rr (* r *vscale*)))
                 (list (format nil ".c create oval ~A ~A ~A ~A -outline ~A -width ~A~A -tags {~A}"
                               (fmtn (- x rr)) (fmtn (- y rr)) (fmtn (+ x rr)) (fmtn (+ y rr))
                               color width (if dash " -dash {6 3}" "") tags))))
             (screen-line (arc-points c r 0d0 +2pi+ 720) color tags :dash dash :width width))))
      (:arc
       (destructuring-bind (c r a1 a2) (rest pc)
         (if (< (* r *vscale*) 12000)
             (multiple-value-bind (x y) (w->s c)
               (let ((rr (* r *vscale*)) (sw (deg (arc-sweep a1 a2))))
                 (list (format nil ".c create arc ~A ~A ~A ~A -style arc -start ~,4F -extent ~,4F -outline ~A -width ~A~A -tags {~A}"
                               (fmtn (- x rr)) (fmtn (- y rr)) (fmtn (+ x rr)) (fmtn (+ y rr))
                               (deg a1) (if (>= sw 359.999d0) 359.999d0 sw)
                               color width (if dash " -dash {6 3}" "") tags))))
             (screen-line (arc-points c r a1 a2 720) color tags :dash dash :width width)))))))

(defun point-glyph-half-size ()
  "Half the size of a point's symbol, in pixels: PDSIZE 0 = 5% of the drawing area's height,
negative = that percentage of it, positive = drawing units (as in AutoCAD)."
  (let ((ps (d (or (sysvar "PDSIZE") 0))))
    (max 2 (min 400 (cond ((zerop ps) (* 0.025 *ch*))
                          ((minusp ps) (* (/ (- ps) 200) *ch*))
                          (t (* ps 0.5 *vscale*)))))))

(defun point-glyph-cmds (x y mode s color tags &optional (canvas ".c"))
  "Canvas commands for a point symbol of PDMODE MODE, half-size S pixels, at X,Y."
  (let ((base (mod mode 32)) (out nil))
    (flet ((ln (x1 y1 x2 y2)
             (push (format nil "~A create line ~,1F ~,1F ~,1F ~,1F -fill ~A -tags {~A}" canvas x1 y1 x2 y2 color tags) out)))
      (case base
        (0 (push (format nil "~A create rectangle ~,1F ~,1F ~,1F ~,1F -outline ~A -fill ~A -tags {~A}"
                         canvas (- x 1) (- y 1) (+ x 1) (+ y 1) color color tags) out))
        (1 nil)
        (2 (ln (- x s) y (+ x s) y) (ln x (- y s) x (+ y s)))
        (3 (ln (- x s) (- y s) (+ x s) (+ y s)) (ln (- x s) (+ y s) (+ x s) (- y s)))
        (4 (ln x y x (- y s)))
        (t nil)))
    (when (logtest mode 32)
      (push (format nil "~A create oval ~,1F ~,1F ~,1F ~,1F -outline ~A -tags {~A}" canvas (- x s) (- y s) (+ x s) (+ y s) color tags) out))
    (when (logtest mode 64)
      (push (format nil "~A create rectangle ~,1F ~,1F ~,1F ~,1F -outline ~A -tags {~A}" canvas (- x s) (- y s) (+ x s) (+ y s) color tags) out))
    out))

(defun render-point-glyph (x y color tags)
  (point-glyph-cmds (round x) (round y) (or (sysvar "PDMODE") 0) (point-glyph-half-size) color tags))

(defun text-anchor (data)
  (let ((type (dxf 0 data)))
    (if (equal type "MTEXT")
        (let ((a (or (dxf 71 data) 1)))
          (values (nth (1- (max 1 (min 9 a))) '("nw" "n" "ne" "w" "center" "e" "sw" "s" "se"))
                  (case (mod (1- a) 3) (0 "left") (1 "center") (t "right"))))
        (let ((h (or (dxf 72 data) 0)) (v (or (dxf 73 data) 0)))
          (values (cond ((= h 4) "center")
                        (t (let ((vert (case v (3 "n") (2 "") (t "s")))
                                 (horiz (case h ((1) "") ((2) "e") (t "w"))))
                             (let ((s (concatenate 'string vert horiz))) (if (string= s "") "center" s)))))
                  "left")))))

(defun render-text (data color tags hl)
  (when (projected-view-p)
    ;; seen at an angle, text is drawn with line strokes lying in its plane
    (return-from render-text
      (loop for pc in (ignore-errors (text-stroke-pieces data))
            append (render-piece (cons :seg (rest pc)) (if hl "#8fc4ff" color) tags))))
  (let* ((h (text-effective-height data))
         (px-size (round (/ (* h *vscale*) 0.72d0)))
         (p (text-insertion data))
         (rot (or (dxf 50 data) 0d0))
         (s (text-string-display (or (dxf 1 data) ""))))
    (when (and p (plusp (length s)))
      (multiple-value-bind (x y) (w->s p)
        (cond
          ((< px-size 3)
           ;; too small to read: a short stroke for each line, inside the text's own box (so a
           ;; long MTEXT note stays a block of lines where it is, not one streak across the sheet)
           (text-greek-cmds data (if hl "#8fc4ff" color) tags))
          ((> px-size 3000) nil)
          ((or (< x -4000) (> x (+ *full-cw* 4000)) (< y -4000) (> y (+ *full-ch* 4000))) nil)
          ((mtext-rich-p data) (list (render-rich-mtext data x y color tags hl)))
          (t (multiple-value-bind (anchor justify) (text-anchor data)
               (list (format nil ".c create text ~A ~A -text ~A -anchor ~A -justify ~A -angle ~,3F -font {~A} -fill ~A -tags {~A}~A"
                             (fmtn x) (fmtn y) (tq s) anchor justify (deg rot) (text-font-spec data px-size)
                             (if hl "#8fc4ff" color) tags
                             (let ((w (and (equal (dxf 0 data) "MTEXT") (dxf 41 data))))
                               (if (and w (plusp w)) (format nil " -width ~D" (max 1 (round (* w *vscale*)))) "")))))))))))

(defun text-greek-cmds (data color tags)
  (let* ((c (text-box data)) (o (first c))
         (ex (v- (second c) o)) (ey (v- (fourth c) o))
         (w (dist2d (first c) (second c))) (hh (dist2d (first c) (fourth c)))
         (lines (text-lines data)) (n (max 1 (length lines)))
         (h (text-effective-height data))
         (spacing-px (* (/ hh n) *vscale*))
         (step (max 1 (ceiling 2 (max spacing-px 1d-9))))       ; keep strokes at least 2 pixels apart
         (attach (or (and (equal (dxf 0 data) "MTEXT") (dxf 71 data)) 1))
         (out nil))
    (when (and (> w 0) (> hh 0))
      (multiple-value-bind (bx0 by0 bx1 by1) (values (reduce #'min c :key #'px) (reduce #'min c :key #'py)
                                                     (reduce #'max c :key #'px) (reduce #'max c :key #'py))
        (multiple-value-bind (vx0 vy0 vx1 vy1) (view-rect 0)
          (when (or (> bx0 vx1) (< bx1 vx0) (> by0 vy1) (< by1 vy0)) (return-from text-greek-cmds nil))))
      (loop for line in lines for i from 0
            when (and (zerop (mod i step)) (plusp (length (string-trim " " line))))
              do (let* ((len (min 1d0 (/ (* 0.6d0 h (length line)) w)))
                        (start (case (mod (1- attach) 3) (0 0d0) (1 (/ (- 1 len) 2)) (t (- 1 len))))
                        (fy (- 1 (/ (+ i 0.6d0) n)))
                        (a (v+ o (v+ (v* ex start) (v* ey fy))))
                        (b (v+ a (v* ex len))))
                   (multiple-value-bind (x1 y1) (w->s a)
                     (multiple-value-bind (x2 y2) (w->s b)
                       (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}"
                                     (fmtn x1) (fmtn y1) (fmtn (if (< (abs (- x2 x1)) 1) (+ x1 1) x2)) (fmtn y2) color tags)
                             out))))))
    (nreverse out)))

(defun sub-color (sub parent-aci)
  (let ((c (dxf 62 sub)))
    (cond ((eql c 0) parent-aci)
          ((and c (/= c 256)) (abs c))
          ((equal (dxf 8 sub) "0") parent-aci)
          (t (let ((l (find-layer (dxf 8 sub)))) (if l (abs (layer-color l)) parent-aci))))))

(defun piece-polyline (pc)
  "World-space points along a piece, finely enough for dashing at the current zoom."
  (ecase (first pc)
    (:seg (list (second pc) (third pc)))
    (:circle (let ((n (max 24 (min 720 (ceiling (* (third pc) *vscale* 0.6))))))
               (arc-points (second pc) (third pc) 0d0 +2pi+ n)))
    (:arc (destructuring-bind (c r a1 a2) (rest pc)
            (let ((n (max 8 (min 720 (ceiling (* r *vscale* (arc-sweep a1 a2) 0.1))))))
              (arc-points c r a1 a2 n))))))

(defun render-linetyped (pc pattern scale color tags hl width)
  "Render piece PC with a dash PATTERN, or plain when the dashes would be too small or too many."
  (let* ((period (* scale (reduce #'+ (mapcar #'abs pattern))))
         (len (case (first pc)
                (:seg (dist2d (second pc) (third pc)))
                (:circle (* +2pi+ (third pc)))
                (:arc (* (third pc) (arc-sweep (fourth pc) (fifth pc))))
                (t 0d0))))
    (if (or (member (first pc) '(:pt :gseg))
            (< (* period *vscale*) 3)
            (> (/ len (max period 1d-12)) 4000)
            (not (piece-visible-p pc)))
        (render-piece pc color tags :dash hl :width width)
        (loop for run in (dash-runs (piece-polyline pc) pattern scale)
              append (screen-line run color tags :dash hl :width width)))))

(defun render-data (data aci tags hl &optional xf (depth 0) parent-lt parent-lw (parent-lts 1d0))
  (let ((type (dxf 0 data)) (color (aci->hex aci)))
    (cond
      ((> depth 8) nil)
      ;; an object in another plane: drawn by its 3D edges
      ((offplane-p data)
       (let ((w (max 1 (lineweight-pixels (entity-lineweight data parent-lw)))))
         (when (member type '("CIRCLE" "ARC" "LWPOLYLINE" "ELLIPSE" "SPLINE") :test #'equal)
           ;; outlines with a linetype: dashed along each edge, in its own plane
           (return-from render-data
             (let ((pattern (ltype-pattern-for (entity-ltype-name data parent-lt)))
                   (scale (* (ltype-scale-for data parent-lts) (if xf (xf-scale xf) 1))))
               (loop for poly in (offplane-polylines data)
                     append (screen-line-typed (if xf (mapcar (lambda (p) (xf-point xf p)) poly) poly)
                                               pattern scale color tags hl w)))))
         (loop for pc in (if (member type '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal)
                             ;; text in another plane: its letters, drawn in that plane
                             (offplane-pieces data (ignore-errors (text-stroke-pieces (rem-dxf 210 data))))
                             (data-pieces data))
               append (loop for q in (if xf (transform-piece pc xf) (list pc))
                            append (render-piece (if (eq (first q) :gseg) (cons :seg (rest q)) q)
                                                 color tags :dash hl :width w)))))
      ((equal type "IMAGE") (funcall 'image-render-cmds (if xf (xform-data data xf) data) color tags hl))
      ((member type '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal)
       (render-text (if xf (xform-data data xf) data) color tags hl))
      ((member type '("INSERT" "DIMENSION") :test #'equal)
       (let ((b (find-block (dxf 2 data))))
         (when b
           (let* ((cxf (if (equal type "INSERT")
                           (let ((ixf (insert-xf data (block-def-base b)))) (if xf (xf-compose ixf xf) ixf))
                           xf))
                  (lt (entity-ltype-name data parent-lt))
                  (lw (entity-lineweight data parent-lw))
                  (lts (* parent-lts (d (or (dxf 48 data) 1)))))
             (loop for sub in (block-def-entities b)
                   append (render-data sub (sub-color sub aci) tags hl cxf (1+ depth) lt lw lts))))))
      ((equal type "UNDERLAY")
       (loop for sub in (underlay-display-entities (if xf (xform-data data xf) data))
             append (render-data sub (sub-color sub aci) tags hl nil (1+ depth) nil nil parent-lts)))
      ((equal type "MULTILEADER")
       (let ((lt (entity-ltype-name data parent-lt)) (lw (entity-lineweight data parent-lw)))
         (loop for sub in (mleader-display-entities (if xf (xform-data data xf) data))
               append (render-data sub (sub-color sub aci) tags hl nil (1+ depth) lt lw parent-lts))))
      ((equal type "LEADER")
       (let ((lt (entity-ltype-name data parent-lt)) (lw (entity-lineweight data parent-lw)))
         (loop for sub in (leader-display-entities (if xf (xform-data data xf) data))
               append (render-data sub (sub-color sub aci) tags hl nil (1+ depth) lt lw parent-lts))))
      ((equal type "SOLID")
       (let* ((pts (remove nil (list (dxf 10 data) (dxf 11 data) (dxf 13 data) (dxf 12 data))))
              (pts (if xf (mapcar (lambda (p) (xf-point xf (pt3 p))) pts) (mapcar #'pt3 pts)))
              (coords (loop for p in pts append (multiple-value-list (w->s p)))))
         (when (and (every (lambda (c) (< -30000 c 30000)) coords)
                    (some #'piece-visible-p (loop for (a b) on pts while b collect (list :seg a b))))
           (list (format nil ".c create polygon ~{~A~^ ~} -fill ~A -outline ~A -tags {~A}"
                         (mapcar #'fmtn coords) color color tags)))))
      ((equal type "HATCH") (render-hatch data color tags hl xf))
      ((and (projected-view-p) (member type '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POINT") :test #'equal))
       (render-3d-object data color tags hl xf (max 1 (lineweight-pixels (entity-lineweight data parent-lw)))
                         (ltype-pattern-for (entity-ltype-name data parent-lt))
                         (* (ltype-scale-for data parent-lts) (if xf (xf-scale xf) 1))))
      (t
       (let* ((pw (if (and (equal type "LWPOLYLINE") (dxf 43 data) (plusp (dxf 43 data)))
                      (max 1 (round (* (dxf 43 data) *vscale* (if xf (xf-scale xf) 1))))
                      0))
              (width (max pw (lineweight-pixels (entity-lineweight data parent-lw))))
              (pattern (ltype-pattern-for (entity-ltype-name data parent-lt)))
              (scale (* (ltype-scale-for data parent-lts) (if xf (xf-scale xf) 1))))
         (if (and (or (not pattern)
                      ;; dashes too small to see at this zoom: drawn solid anyway
                      (< (* scale (reduce #'+ (mapcar #'abs pattern)) *vscale*) 3))
                  (member type '("LWPOLYLINE" "SPLINE" "ELLIPSE" "LEADER" "HELIX") :test #'equal))
             ;; many short straight pieces: one canvas line per connected run (far fewer items)
             (render-chained (loop for pc in (data-pieces data)
                                   append (if xf (transform-piece pc xf) (list pc)))
                             color tags hl width)
             (loop for pc in (display-pieces data xf)
                   append (loop for tp in (if xf (transform-piece pc xf) (list pc))
                                append (if pattern
                                           (render-linetyped tp pattern scale color tags hl width)
                                           (render-piece tp color tags :dash hl :width width))))))))))

(defun screen-line-typed (poly pattern scale color tags hl width)
  "3D polyline POLY with linetype PATTERN (at SCALE), or solid when there is none or the dashes
would be too small or too many to see."
  (let ((period (and pattern (* scale (reduce #'+ (mapcar #'abs pattern))))))
    (if (or (null pattern) (< (* period *vscale*) 3)
            (> (/ (polyline-length poly) (max period 1d-12)) 4000))
        (screen-line poly color tags :dash hl :width width)
        (loop for run in (dash-runs poly pattern scale)
              append (screen-line run color tags :dash hl :width width)))))

(defun render-3d-object (data color tags hl xf width &optional pattern (scale 1d0))
  "A line, arc, circle or polyline in a 3D view, at its elevation and extruded by its thickness
(as AutoCAD draws walls made with THICKNESS)."
  (let* ((elev (if (equal (dxf 0 data) "LWPOLYLINE") (d (or (dxf 38 data) 0)) 0d0))
         (th (d (or (dxf 39 data) 0)))
         (pieces (loop for pc in (data-pieces data) append (if xf (transform-piece pc xf) (list pc))))
         (polys (loop for pc in pieces
                      unless (member (first pc) '(:gseg :pt))
                        collect (mapcar (lambda (p) (pt (px p) (py p) (+ (pz p) elev)))
                                        (case (first pc)
                                          (:seg (list (second pc) (third pc)))
                                          (:circle (arc-points-3d (second pc) (third pc) 0d0 +2pi+ (max 72 (min 720 (ceiling (* (third pc) *vscale* 0.6))))))
                                          (:arc (destructuring-bind (c r a1 a2) (rest pc) (arc-points-3d c r a1 a2 36)))))))
         (out nil))
    (dolist (pc pieces)
      (when (eq (first pc) :pt) (setf out (nconc out (render-piece pc color tags)))))
    (dolist (poly polys)
      (setf out (nconc out (screen-line-typed poly pattern scale color tags hl width)))
      (unless (zerop th)
        (let ((top (mapcar (lambda (p) (v+ p (pt 0 0 th))) poly)))
          (setf out (nconc out (screen-line-typed top pattern scale color tags hl width)))
          ;; vertical edges: at the ends, and around curves every eighth of a turn
          (let ((n (length poly)))
            (loop for p in poly for i from 0
                  when (or (= i 0) (= i (1- n)) (and (> n 3) (zerop (mod i (max 1 (floor n 8))))))
                    do (setf out (nconc out (screen-line-typed (list p (v+ p (pt 0 0 th))) pattern scale color tags hl width))))))))
    out))

(defun render-chained (pieces color tags hl width)
  "Draw connected straight pieces as multi-point canvas lines, dropping points under a pixel apart;
other pieces (arcs, circles) are drawn one by one."
  (let ((out nil) (run nil) (last-end nil) (m 2000d0))
    (labels ((flush ()
               (when (>= (length run) 4)
                 (push (format nil ".c create line ~{~A~^ ~} -fill ~A -width ~A~A -tags {~A}"
                               (mapcar #'fmtn (reverse run)) color width (if hl " -dash {6 3}" "") tags)
                       out))
               (setf run nil last-end nil))
             (add-xy (x y)
               (if (and run (< (+ (abs (- x (second run))) (abs (- y (first run)))) 0.75d0))
                   nil
                   (setf run (list* y x run)))))
      (dolist (pc pieces)
        (if (eq (first pc) :seg)
            (multiple-value-bind (x1 y1) (w->s (second pc))
              (multiple-value-bind (x2 y2) (w->s (third pc))
                (let ((c (clip-seg x1 y1 x2 y2 (- m) (- m) (+ *full-cw* m) (+ *full-ch* m))))
                  (cond
                    ((null c) (flush))
                    (t
                     (let ((joined (and last-end (< (dist2d last-end (second pc)) 1d-9)
                                        (= (first c) x1) (= (second c) y1))))
                       (unless joined
                         (flush)
                         (add-xy (first c) (second c))))
                     (add-xy (third c) (fourth c))
                     ;; keep the last point even when it was close to the previous one
                     (unless (and (= (first run) (fourth c)) (= (second run) (third c)))
                       (setf run (list* (fourth c) (third c) (cddr run))))
                     (setf last-end (if (and (= (third c) x2) (= (fourth c) y2)) (third pc) nil))
                     (unless last-end (flush)))))))
            (progn (flush)
                   (setf out (append (reverse (render-piece pc color tags :dash hl :width width)) out)))))
      (flush)
      (nreverse out))))

(defun entity-render-cmds (e)
  "Canvas commands for entity E. Level of detail, as in AutoCAD: objects outside the view are
skipped, and an object smaller than about a pixel and a half is drawn as one tiny mark."
  (let* ((data (ent-data e))
         (tags (format nil "ent h~A" (ename-handle e)))
         (b (ignore-errors (entity-bbox e))))
    (if (and b (not (projected-view-p)) (not (member (dxf 0 data) '("TEXT" "MTEXT" "ATTDEF" "ATTRIB" "POINT") :test #'equal)))
        (multiple-value-bind (vx0 vy0 vx1 vy1) (view-rect 50)
          (destructuring-bind (x0 y0 x1 y1) b
            (cond
              ((or (> x0 vx1) (< x1 vx0) (> y0 vy1) (< y1 vy0)) nil)
              ((and (< (* (- x1 x0) *vscale*) 1.5d0) (< (* (- y1 y0) *vscale*) 1.5d0))
               (multiple-value-bind (sx sy) (w->s (pt (/ (+ x0 x1) 2) (/ (+ y0 y1) 2)))
                 (list (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}"
                               (fmtn sx) (fmtn sy) (fmtn (+ sx 1)) (fmtn sy)
                               (if (gethash e *hl*) "#8fc4ff" (aci->hex (entity-aci data))) tags))))
              (t (render-data data (entity-aci data) tags (gethash e *hl*))))))
        (render-data data (entity-aci data) tags (gethash e *hl*)))))

(defun iso-grid-cmds ()
  "The isometric grid: lines at 30 and 150 degrees and verticals through the snap lattice."
  (multiple-value-bind (x0 y0 x1 y1) (view-rect 0)
    (let* ((g (max 1d-9 (d (second (sysvar "GRIDUNIT"))))) (out nil) (tn (tan (/ pi 6))) (c (cos (/ pi 6))))
      (loop while (< (* g *vscale*) 12) do (setf g (* g 5)))
      (loop while (> (* g *vscale*) 240) do (setf g (/ g 5)))
      (flet ((seg (a b col) (setf out (nconc out (screen-line (list a b) col "grid")))))
        ;; y = slope*x + k*g, for slope +tan30 and -tan30
        (dolist (slope (list tn (- tn)))
          (let* ((ks (mapcar (lambda (x y) (/ (- y (* slope x)) g)) (list x0 x0 x1 x1) (list y0 y1 y0 y1))))
            (loop for k from (floor (reduce #'min ks)) to (ceiling (reduce #'max ks))
                  do (seg (pt x0 (+ (* slope x0) (* k g))) (pt x1 (+ (* slope x1) (* k g)))
                          (if (zerop (mod k 5)) (grid-color :major) (grid-color :minor))))))
        (loop for k from (floor x0 (* g c)) to (ceiling x1 (* g c))
              do (seg (pt (* k g c) y0) (pt (* k g c) y1) (if (zerop (mod k 5)) (grid-color :major) (grid-color :minor)))))
      out)))

(defun grid-area ()
  "Where the grid is drawn: the view, or only inside the drawing limits (GRIDDISPLAY 0)."
  (multiple-value-bind (x0 y0 x1 y1) (view-rect 0)
    (if (eql (sysvar "GRIDDISPLAY") 0)
        (let ((lo (sysvar "LIMMIN")) (hi (sysvar "LIMMAX")))
          (values (max x0 (px lo)) (max y0 (py lo)) (min x1 (px hi)) (min y1 (py hi))))
        (values x0 y0 x1 y1))))

(defun grid-cmds ()
  (when (and (eql (sysvar "GRIDMODE") 1) (not (projected-view-p)))
    (when (iso-mode-p) (return-from grid-cmds (iso-grid-cmds)))
    (multiple-value-bind (x0 y0 x1 y1) (grid-area)
      (let* ((g (max 1d-9 (d (first (sysvar "GRIDUNIT"))))) (major 5) (out nil))
        (loop while (< (* g *vscale*) 8) do (setf g (* g 5)))
        (loop while (> (* g *vscale*) 200) do (setf g (/ g 5)))
        (flet ((vline (x col) (when (<= (- x0 1d-9) x (+ x1 1d-9)) (multiple-value-bind (sx sy0) (w->s (pt x y0))
                                (multiple-value-bind (sx1 sy1) (w->s (pt x y1)) (declare (ignore sx1))
                                  (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags grid"
                                                (fmtn sx) (fmtn (max -2 sy1)) (fmtn sx) (fmtn (min (+ *full-ch* 2) sy0)) col) out)))))
               (hline (y col) (when (<= (- y0 1d-9) y (+ y1 1d-9)) (multiple-value-bind (sx0 sy) (w->s (pt x0 y))
                                (multiple-value-bind (sx1) (w->s (pt x1 y))
                                  (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags grid"
                                                (fmtn (max -2 sx0)) (fmtn sy) (fmtn (min (+ *full-cw* 2) sx1)) (fmtn sy) col) out))))))
          (loop for i from (floor x0 g) to (ceiling x1 g)
                do (vline (* i g) (cond ((zerop i) (grid-color :x-axis)) ((zerop (mod i major)) (grid-color :major)) (t (grid-color :minor)))))
          (loop for i from (floor y0 g) to (ceiling y1 g)
                do (hline (* i g) (cond ((zerop i) (grid-color :y-axis)) ((zerop (mod i major)) (grid-color :major)) (t (grid-color :minor))))))
        out))))

(defun ucs-icon-cmds ()
  (when (paper-space-p)
    ;; paper space: AutoCAD's triangle
    (return-from ucs-icon-cmds
      (let ((x 30) (y (- *ch* 28)))
        (list (format nil ".c create polygon ~D ~D ~D ~D ~D ~D -fill {} -outline #30363d -width 2 -tags ucs" x y (+ x 40) y x (- y 40))
              (format nil ".c create text ~D ~D -text X -fill #30363d -font {Helvetica 9 bold} -tags ucs" (+ x 48) y)
              (format nil ".c create text ~D ~D -text Y -fill #30363d -font {Helvetica 9 bold} -tags ucs" x (- y 49))
              (format nil ".c create text ~D ~D -anchor sw -text PAPER -fill #30363d -font {Helvetica 10 bold} -tags ucs" 70 (- *ch* 6))))))
  (when *view*
    ;; the world axes as they point in this view, with the view's name
    (return-from ucs-icon-cmds
      (let ((x (+ *tx* 44)) (y (+ *ty* (- *ch* 44))) (out nil))
        (loop for (axis col label) in '(((1 0 0) "#d24b4b" "X") ((0 1 0) "#4bbf5a" "Y") ((0 0 1) "#4b8fd2" "Z"))
              do (multiple-value-bind (u v) (view-project axis)
                   (let ((ex (+ x (* 34 u))) (ey (- y (* 34 v))))
                     (when (> (+ (* u u) (* v v)) 1d-6)
                       (push (format nil ".c create line ~D ~D ~,1F ~,1F -fill ~A -width 2 -arrow last -tags ucs" x y ex ey col) out)
                       (push (format nil ".c create text ~,1F ~,1F -text ~A -fill ~A -font {Helvetica 9 bold} -tags ucs"
                                     (+ ex (* 9 u)) (- ey (* 9 v)) label col) out)))))
        (push (format nil ".c create text ~D ~D -anchor sw -text ~A -fill #c9ced6 -font {Helvetica 10 bold} -tags ucs"
                      (+ *tx* 10) (+ *ty* (- *ch* 6)) (tq (string-upcase (first *view*)))) out)
        (nreverse out))))
  (let ((x (+ *tx* 34)) (y (+ *ty* (- *ch* 30))))
    (list (format nil ".c create line ~D ~D ~D ~D -fill #d24b4b -width 2 -arrow last -tags ucs" x y (+ x 44) y)
          (format nil ".c create line ~D ~D ~D ~D -fill #4bbf5a -width 2 -arrow last -tags ucs" x y x (- y 44))
          (format nil ".c create text ~D ~D -text X -fill #d24b4b -font {Helvetica 9 bold} -tags ucs" (+ x 52) y)
          (format nil ".c create text ~D ~D -text Y -fill #4bbf5a -font {Helvetica 9 bold} -tags ucs" x (- y 53))
          (format nil ".c create rectangle ~D ~D ~D ~D -outline #8a94a3 -tags ucs" (- x 3) (- y 3) (+ x 3) (+ y 3)))))

(defun entity-in-view-p (e)
  (when (projected-view-p) (return-from entity-in-view-p t))
  (let ((b (entity-bbox e)))
    (multiple-value-bind (x0 y0 x1 y1) (view-rect 50)
      (and b (not (or (> (first b) x1) (< (third b) x0) (> (second b) y1) (< (fourth b) y0)))))))

(defun model-body-cmds ()
  "Canvas commands for the grid and the objects of the space being worked in."
  (let ((cmds (grid-cmds)))
    (if (and (fboundp 'visual-style) (not (equal (funcall 'visual-style) "Wireframe")))
        (setf cmds (nconc cmds (funcall 'visual-style-cmds)))
        (dolist (e (live-entities))
          (when (and (entity-visible-p e) (entity-in-view-p e))
            (setf cmds (nconc cmds (entity-render-cmds e))))))
    cmds))

(defvar *canvas-bg-shown* nil)
(defun canvas-bg ()
  "The canvas color: the drawing background, or the gray around the sheet on a layout tab."
  (if (drawing-ctab *dwg*) "#5a616c" (background-color)))

(defun quick-tags ()
  "Canvas tags moved or scaled at once while panning and zooming (before the exact redraw)."
  (cond ((null (drawing-ctab *dwg*)) "ent grd grid")
        ((drawing-active-vp *dwg*) "ent grd grid")
        (t "ent grd grid sheet vpc")))

(defun gui-regen ()
  (when (eq *ui* :gui)
    (when (fboundp 'apply-tile-geometry) (funcall 'apply-tile-geometry))
    (when (and (drawing-ctab *dwg*) (fboundp 'layout-prepare)) (funcall 'layout-prepare))
    (unless (equal *canvas-bg-shown* (canvas-bg))
      (setf *canvas-bg-shown* (canvas-bg))
      (tk ".c configure -background ~A" (canvas-bg)))
    (let ((cmds (list ".c delete all")))
      (setf cmds (nconc cmds (if (and (drawing-ctab *dwg*) (fboundp 'layout-body-cmds))
                                 (funcall 'layout-body-cmds)
                                 (model-body-cmds))))
      ;; the Model tab split into viewports: the others over the active one's drawing
      (when (and (fboundp 'tiled-p) (funcall 'tiled-p))
        (setf cmds (nconc cmds (funcall 'tiles-overlay-cmds))))
      (dolist (g *grdraw-items*)
        (destructuring-bind (a b aci) g
          (setf cmds (nconc cmds (screen-line (list a b) (aci->hex aci) "grd")))))
      (setf cmds (nconc cmds (ucs-icon-cmds)))
      (when (fboundp 'block-editor-banner-cmds) (setf cmds (nconc cmds (funcall 'block-editor-banner-cmds))))
      (tk-send-string (format nil "~{~A~%~}" cmds))
      (refresh-grips)
      (setf *drawn-modcount* (drawing-modcount *dwg*))
      (when (fboundp 'refresh-layout-tabs) (funcall 'refresh-layout-tabs))
      (draw-temp))))

(defun next-drawn-entity (e)
  "The next object after E in draw order that is on the screen (its canvas items lie above E's)."
  (let* ((v (drawing-entities *dwg*)) (k (position e v)))
    (when k
      (loop for i from (1+ k) below (length v)
            for x = (aref v i)
            when (and (not (ename-deleted x)) (entity-visible-p x)) return x))))

(defun rerender-entity (e)
  ;; redrawn in its place in the draw order (new canvas items would otherwise go on top)
  (let ((nx (next-drawn-entity e)))
    (tk-send-string (format nil ".c delete h~A~%~{~A~%~}~@[catch {.c lower h~A h~A}~%~].c raise pover~%.c raise grip~%.c raise tmp"
                            (ename-handle e)
                            (and (entity-visible-p e) (entity-render-cmds e))
                            (and nx (ename-handle e)) (and nx (ename-handle nx))))))

(defun gui-highlight (ents on)
  (dolist (e ents)
    (if on (setf (gethash e *hl*) t) (remhash e *hl*))
    (when (ent-live-p e) (rerender-entity e))))

(defun sweep-highlights ()
  "Drop every highlight that isn't the current noun/verb selection. Run back at Command:, and on
Escape, so nothing stays dashed after a command (e.g. an object erased while selected, then undone)."
  (let (stale)
    (maphash (lambda (e v) (declare (ignore v)) (unless (member e *pickfirst*) (push e stale))) *hl*)
    (when stale (gui-highlight stale nil))))

;;; ---------- temporary graphics (cursor, snaps, rubber band) ----------

(defparameter *snap-names*
  '((1 . "Endpoint") (2 . "Midpoint") (4 . "Center") (8 . "Node") (16 . "Quadrant") (32 . "Intersection")
    (64 . "Insertion") (128 . "Perpendicular") (256 . "Tangent") (512 . "Nearest")
    (:dimline . "Lined up with dimension")))

(defun snap-glyph (x y bit)
  (let ((s (max 2 (min 20 (or (sysvar "AUTOSNAPSIZE") 6)))) (c "#3fd25f") (tags "tmp") (x (round x)) (y (round y)))
     (case bit
       (1 (list (format nil ".c create rectangle ~A ~A ~A ~A -outline ~A -width 2 -tags ~A" (- x s) (- y s) (+ x s) (+ y s) c tags)))
       (2 (list (format nil ".c create polygon ~A ~A ~A ~A ~A ~A -outline ~A -fill {} -width 2 -tags ~A" (- x s) (+ y s) (+ x s) (+ y s) x (- y s) c tags)))
       ((4 8) (list (format nil ".c create oval ~A ~A ~A ~A -outline ~A -width 2 -tags ~A" (- x s) (- y s) (+ x s) (+ y s) c tags)))
       (16 (list (format nil ".c create polygon ~A ~A ~A ~A ~A ~A ~A ~A -outline ~A -fill {} -width 2 -tags ~A" (- x s) y x (- y s) (+ x s) y x (+ y s) c tags)))
       (32 (list (format nil ".c create line ~A ~A ~A ~A -fill ~A -width 2 -tags ~A" (- x s) (- y s) (+ x s) (+ y s) c tags)
                 (format nil ".c create line ~A ~A ~A ~A -fill ~A -width 2 -tags ~A" (- x s) (+ y s) (+ x s) (- y s) c tags)))
       (128 (list (format nil ".c create line ~A ~A ~A ~A ~A ~A -fill ~A -width 2 -tags ~A" (- x s) (- y s) (- x s) (+ y s) (+ x s) (+ y s) c tags)))
       (t (list (format nil ".c create polygon ~A ~A ~A ~A ~A ~A ~A ~A -outline ~A -fill {} -width 2 -tags ~A" (- x s) (- y s) (+ x s) (- y s) (- x s) (+ y s) (+ x s) (+ y s) c tags))))))

(defun tooltip-cmds (x y text &aux (x (round x)) (y (round y)))
  (list (format nil ".c create text ~A ~A -text ~A -anchor nw -fill #1b1f24 -font {Helvetica 9} -tags {tmp tip}"
                (+ x 14) (+ y 14) (tq text))
        ".c create rectangle [.c bbox tip] -fill #f1e7a8 -outline #b9a95a -tags {tmp tipbg}"
        ".c lower tipbg tip"))

(defvar *polar-hit* nil "The polar tracking angle the cursor is on, or NIL.")

;;; ---------- dynamic input (F12, DYNMODE) ----------
;; Beside the cursor: the prompt (DYNPROMPT), and boxes showing where the cursor is -- X and Y
;; for a first point, length and angle from the last point for the next one. What is typed shows
;; in the active box; Tab keeps it there (the cursor is held to it) and moves to the other box.

(defvar *dyn-lock* (list nil nil) "Values typed into the two dynamic input boxes and kept with Tab.")
(defvar *dyn-active* 0 "Which dynamic input box takes what is typed.")

(defun dyn-on-p () (plusp (or (sysvar "DYNMODE") 0)))
(defun dyn-locked-p () (or (first *dyn-lock*) (second *dyn-lock*)))

(defun dyn-mode (req)
  (let ((kind (inreq-kind req)) (base (inreq-base req)))
    (case kind
      (:point (if (pointp base) :dim :pointer))
      (:corner (cond ((inreq-nosnap req) :prompt) ((pointp base) :relative) (t :pointer)))
      (:distance :distance)
      (:angle :angle)
      (:command :command)
      ((:entity :select-point) :prompt)
      (t :value))))

(defun dyn-two-boxes-p (mode) (member mode '(:pointer :dim :relative)))

(defun dyn-point (req cur)
  "The point the dynamic input boxes give: the values kept with Tab, the cursor's for the rest."
  (let ((l0 (first *dyn-lock*)) (l1 (second *dyn-lock*)) (base (inreq-base req)))
    (case (dyn-mode req)
      (:pointer (pt (or l0 (px cur)) (or l1 (py cur)) (pz cur)))
      (:relative (pt (+ (px base) (or l0 (- (px cur) (px base)))) (+ (py base) (or l1 (- (py cur) (py base)))) (pz (pt3 base))))
      (:dim (let* ((b (pt3 base))
                   (a (or l1 (if (< (dist2d b cur) 1d-12) 0d0 (ang b cur))))
                   (d (or l0 (dist2d b cur))))
              (let ((q (polar b a d))) (pt (px q) (py q) (pz b)))))
      (t nil))))

(defun dyn-parse (mode index text)
  (let ((text (string-trim " " text)))
    (and (plusp (length text))
         (if (and (eq mode :dim) (= index 1)) (angtof text) (distof text)))))

(defun dyn-take-text (req text)
  "Put TEXT into the active box (split at , or < into both). Returns NIL if it isn't a number."
  (let* ((mode (dyn-mode req)) (sep (if (eq mode :dim) #\< #\,)) (k (position sep text)))
    (if k
        (let ((a (dyn-parse mode 0 (subseq text 0 k))) (b (dyn-parse mode 1 (subseq text (1+ k)))))
          (when (and a b) (setf *dyn-lock* (list a b)) t))
        (let ((v (dyn-parse mode *dyn-active* text)))
          (when v (setf (nth *dyn-active* *dyn-lock*) v) t)))))

(defun dyn-tab (req text)
  (when (and (dyn-on-p) (dyn-two-boxes-p (dyn-mode req)))
    (let ((text (string-trim " " text)))
      (when (and (plusp (length text)) (not (dyn-take-text req text)))
        (out (format nil "~A is not a number.~%" text))))
    (setf *dyn-active* (- 1 *dyn-active*))))

(defun dyn-enter-point (req text)
  "Enter after values were kept with Tab: the point they give (what was just typed goes in the
active box). NIL when the line should be read as usual."
  (when (and req (dyn-on-p) (dyn-locked-p) (dyn-two-boxes-p (dyn-mode req)))
    (let ((tx (string-trim " " text)))
      (when (or (zerop (length tx)) (dyn-take-text req tx))
        (dyn-point req *cursor-world*)))))

(defun dyn-cmd (req)
  "The Tk command that shows (or clears) the dynamic input boxes."
  (if (not (and req *mouse-in* (dyn-on-p)))
      "dyn_clear"
      (let* ((mode (dyn-mode req)) (cur *cursor-world*) (base (inreq-base req))
             (b (and (pointp base) (pt3 base)))
             (l0 (first *dyn-lock*)) (l1 (second *dyn-lock*))
             (prompt (if (and (eql (sysvar "DYNPROMPT") 1) (not (eq mode :command)))
                         (string-trim '(#\Space #\Newline #\Tab) (inreq-prompt req)) ""))
             (fields
               (case mode
                 (:pointer (list (list "" (fmt-len (or l0 (px cur))) (if l0 1 0))
                                 (list "," (fmt-len (or l1 (py cur))) (if l1 1 0))))
                 (:relative (list (list "" (fmt-len (or l0 (- (px cur) (px b)))) (if l0 1 0))
                                  (list "," (fmt-len (or l1 (- (py cur) (py b)))) (if l1 1 0))))
                 (:dim (list (list "" (fmt-len (or l0 (dist2d b cur))) (if l0 1 0))
                             (list "<" (fmt-ang (or l1 (if (< (dist2d b cur) 1d-12) 0d0 (ang b cur)))) (if l1 1 0))))
                 (:distance (list (list "" (if b (fmt-len (dist2d b cur)) "") 0)))
                 (:angle (list (list "" (if b (fmt-ang (if (< (dist2d b cur) 1d-12) 0d0 (ang b cur))) "") 0)))
                 ((:command :value) (list (list "" "" 0)))
                 (t nil)))
             (sep (case mode (:dim "<") ((:pointer :relative) ",") (t ""))))
        (format nil "dyn_show ~D ~D ~A {~{~A~^ ~}} ~D ~A ~D"
                *mouse-sx* (+ *mouse-sy* (if (or *snap-bit* *polar-hit*) 20 0)) (tq prompt)
                (mapcar (lambda (f) (format nil "{~A ~A ~D}" (tq (first f)) (tq (second f)) (third f))) fields)
                (if fields *dyn-active* -1) (tq sep) (if (eq mode :command) 1 0)))))

(defun drag-item-cmds (item)
  (case (first item)
    (:rect (destructuring-bind (a b style) (rest item)
             (multiple-value-bind (x1 y1) (w->s a)
               (multiple-value-bind (x2 y2) (w->s b)
                 (list (format nil ".c create rectangle ~A ~A ~A ~A -outline ~A -fill ~A -stipple gray25~A -tags tmp"
                               (fmtn x1) (fmtn y1) (fmtn x2) (fmtn y2)
                               (case style (:crossing "#52c46a") (:window "#5a9cf0") (t "#d8dde4"))
                               (case style (:crossing "#2e6b3b") (:window "#2c4d7a") (t "{}"))
                               (if (eq style :crossing) " -dash {4 3}" "")))))))
    (:data (destructuring-bind (data) (rest item)
             (render-data data (entity-aci data) "tmp" nil)))
    ;; a solid's face (PRESSPULL ...): shaded orange with a heavy outline
    (:face (let ((xy (loop for p in (second item) append (multiple-value-bind (x y) (w->s p) (list (fmtn x) (fmtn y))))))
             (when (>= (length xy) 6)
               (list (format nil ".c create polygon ~{~A~^ ~} -fill #ff9f1a -stipple gray25 -outline #ff9f1a -width 3 -tags tmp" xy)))))
    (:edge (multiple-value-bind (x1 y1) (w->s (second item))
             (multiple-value-bind (x2 y2) (w->s (third item))
               (list (format nil ".c create line ~A ~A ~A ~A -fill #ff9f1a -width 4 -tags tmp" (fmtn x1) (fmtn y1) (fmtn x2) (fmtn y2))))))
    (:hotgrip (multiple-value-bind (x y) (w->s (second item))
                (let ((s (grip-pixel-size)))
                  (list (format nil ".c create rectangle ~D ~D ~D ~D -fill #ff2a2a -outline #6b0000 -tags tmp"
                                (round (- x s)) (round (- y s)) (round (+ x s)) (round (+ y s)))))))
    (t (render-piece item "#d8dde4" "tmp"))))

(defun crosshair-screen-angles ()
  "Screen directions (radians, counterclockwise from right) of the two crosshair lines."
  (cond
    ((and (iso-mode-p) (not (projected-view-p))) (isoplane-axes))
    ((projected-view-p)
     (loop for axis in '((1 0 0) (0 1 0))
           collect (multiple-value-bind (u v) (view-project axis)
                     (if (< (+ (* u u) (* v v)) 1d-9) (/ pi 2) (atan v u)))))
    (t (list 0d0 (/ pi 2)))))

(defun cursor-cmds ()
  (when *mouse-in*
    (let* ((x *mouse-sx*) (y *mouse-sy*) (kind (and *req* (inreq-kind *req*)))
           (pick (member kind '(:command :entity :select-point)))
           (cross (not (member kind '(:entity :select-point))))
           (pb (or (sysvar "PICKBOX") 5)) (out nil) (col (cursor-color)))
      (when cross
        (let ((len (let ((cs (or (sysvar "CURSORSIZE") 100)))
                     (if (>= cs 100) 10000 (round (* (/ cs 200) (max *cw* *ch*)))))))
          ;; the crosshairs follow the drawing axes: the isoplane's in isometric drafting,
          ;; the world X and Y in a 3D view, else plain horizontal and vertical
          (dolist (a (crosshair-screen-angles))
            (let ((cx (cos a)) (cy (- (sin a))))
              (push (format nil ".c create line ~,1F ~,1F ~,1F ~,1F -fill ~A -tags tmp"
                            (- x (* len cx)) (- y (* len cy)) (- x (* pb cx)) (- y (* pb cy)) col) out)
              (push (format nil ".c create line ~,1F ~,1F ~,1F ~,1F -fill ~A -tags tmp"
                            (+ x (* pb cx)) (+ y (* pb cy)) (+ x (* len cx)) (+ y (* len cy)) col) out)))))
      (when (or pick (not cross))
        (push (format nil ".c create rectangle ~D ~D ~D ~D -outline ~A -tags tmp" (- x pb) (- y pb) (+ x pb) (+ y pb) col) out))
      (nreverse out))))

(defun draw-temp ()
  (when (eq *ui* :gui)
    (let ((cmds (list ".c delete tmp")) (req *req*))
      (when (and req *mouse-in*)
        (let ((base (inreq-base req)) (kind (inreq-kind req)) (cur *cursor-world*))
          (cond
            ((inreq-drag req)
             (dolist (it (ignore-errors (funcall (inreq-drag req) cur)))
               (setf cmds (nconc cmds (drag-item-cmds it))))
             (when (and base (member kind '(:point :distance :angle)))
               (setf cmds (nconc cmds (screen-line (list base cur) "#9aa3ad" "tmp" :dash t)))))
            ((and base (eq kind :corner))
             (setf cmds (nconc cmds (drag-item-cmds (list :rect base cur :plain)))))
            ((and base (member kind '(:point :distance :angle)))
             (setf cmds (nconc cmds (screen-line (list base cur) "#d8dde4" "tmp")))))
          (when (and (eq *snap-bit* :dimline) *dimline-match*)
            ;; a dashed guide along the dimension line it lines up with
            (setf cmds (nconc cmds (screen-line (list (first *dimline-match*) cur) "#3fd25f" "tmp" :dash t))))
          (when (and *polar-hit* base (member kind '(:point :distance :angle)))
            (let* ((b (pt3 base)) (far (polar b *polar-hit* (gui-pixels->world (* 3 (+ *cw* *ch*))))))
              (setf cmds (nconc cmds (screen-line (list b far) "#3fd25f" "tmp" :dash t)))
              (multiple-value-bind (x y) (w->s cur)
                (setf cmds (nconc cmds (tooltip-cmds x y (format nil "Polar: ~A < ~A" (fmt-len (dist2d b cur)) (fmt-ang *polar-hit*))))))))
          (when (and *snap-bit* (member kind '(:point :corner :distance :angle)))
            (multiple-value-bind (x y) (w->s cur)
              (setf cmds (nconc cmds (snap-glyph x y *snap-bit*)
                                (tooltip-cmds x y (cdr (assoc *snap-bit* *snap-names*)))))))))
      (when (and req (eq (inreq-kind req) :command)) (setf cmds (nconc cmds (grip-hover-cmds))))
      (setf cmds (nconc cmds (cursor-cmds) (list (dyn-cmd req))))
      (tk-send-string (format nil "~{~A~%~}" cmds)))))

(defun update-coords ()
  (let ((p *cursor-world*))
    (tk "setxy ~A" (tq (format nil "~A, ~A, ~A" (fmt-len (px p)) (fmt-len (py p)) (fmt-len (pz p)))))))

(defun gui-sync ()
  "Refresh layer combo, toggles and title."
  (let* ((names (mapcar #'layer-name (drawing-layers *dwg*))))
    (multiple-value-bind (shown color) (layer-box-display)
      (tk "setlayers {~{~A~^ ~}} ~A ~A" (mapcar #'tq names) (tq shown) color))
    (tk "settoggles ~D ~D ~D ~D ~D ~D ~A ~D ~D" (sysvar "GRIDMODE") (sysvar "SNAPMODE") (sysvar "ORTHOMODE")
        (if (logtest (sysvar "OSMODE") 16384) 0 (if (plusp (sysvar "OSMODE")) 1 0))
        (sysvar "LWDISPLAY") (if (iso-mode-p) 1 0)
        (tq (cond ((iso-mode-p) (format nil "ISO ~A" (string-upcase (isoplane-name)))) (t "ISO")))
        (if (dyn-on-p) 1 0) (if (polar-on-p) 1 0))
    (tk "setviewname ~A" (tq (if *view* (first *view*) "Top")))
    (tk "set ::keepprof ~D" (if (eql (sysvar "DELOBJ") 0) 1 0))
    (sync-current-props)
    (tk "settitle ~A" (tq (format nil "Fancy Pencil — ~A~A" (drawing-display-name)
                                  (if (/= (drawing-modcount *dwg*) (drawing-saved-modcount *dwg*)) " *" ""))))))

(defun layer-box-display ()
  "What the layer drop-down shows: with objects selected, their layer (\"VARIES\" when they are on
several), else the current layer. Only shows it; the current layer is not changed."
  (let* ((sel (remove-if-not #'ent-live-p *pickfirst*))
         (layers (and sel (remove-duplicates (mapcar #'ent-layer sel) :test #'string-equal))))
    (cond ((and layers (rest layers)) (values "VARIES" "#8b949e"))
          (t (let* ((name (if layers (first layers) (sysvar "CLAYER")))
                    (l (find-layer name)))
               (values (if l (layer-name l) name) (aci->hex (if l (abs (layer-color l)) 7))))))))

(defun refresh-layer-box ()
  "Show the selection's layer in the layer drop-down (called whenever the selection changes)."
  (when (eq *ui* :gui)
    (multiple-value-bind (shown color) (layer-box-display)
      (tk "setlayerbox ~A ~A" (tq shown) color))))

(defun drawing-display-name ()
  (let ((f (drawing-filename *dwg*))) (if f (file-namestring f) (untitled-name))))

;;; ---------- event handling ----------

(defun update-cursor (sx sy)
  (setf *mouse-sx* sx *mouse-sy* sy *mouse-in* t *polar-hit* nil)
  (let* ((kind (and *req* (inreq-kind *req*)))
         ;; in a 3D view, points land on the work plane through the base point (if there is one)
         (raw (cond ((and (projected-view-p) (member kind '(:point :corner :distance :angle)))
                     (view-work-point sx sy (inreq-base *req*)))
                    ;; in a working plane, a free point is at the depth of the base point
                    ((and (ucs) (member kind '(:point :corner :distance :angle)) (pointp (inreq-base *req*)))
                     (let ((q (s->w sx sy))) (pt (px q) (py q) (pz (pt3 (inreq-base *req*))))))
                    (t (s->w sx sy)))))
    (cond ((and (member kind '(:point :corner)) (inreq-nosnap *req*))
           (setf *cursor-world* raw *snap-bit* nil))
          ((member kind '(:point :corner :distance :angle))
           ;; ortho constrains lines, never the opposite corner of a rectangle or window
           (multiple-value-bind (q bit) (resolve-cursor-point raw (inreq-base *req*) :ortho (not (eq kind :corner)))
             (setf *cursor-world* q *snap-bit* bit)
             ;; no object snap here: line up with another dimension's line if close to one
             (unless bit
               (let ((dq (and *dimline-snap* (member kind '(:point)) (apply-dimline-snap q))))
                 (when dq (setf *cursor-world* dq *snap-bit* :dimline))))
             ;; polar tracking: close to one of the tracking directions from the base point,
             ;; the cursor goes onto it
             (let ((base (inreq-base *req*)))
               (when (and (null *snap-bit*) (polar-on-p) (pointp base) (not (eq kind :corner))
                          (not (eql (sysvar "ORTHOMODE") 1)) (not (projected-view-p)) (not (iso-mode-p)))
                 (multiple-value-bind (tp ta) (polar-track raw (pt3 base) (gui-pixels->world 10))
                   (when tp (setf *cursor-world* tp *polar-hit* ta)))))))
          (t (setf *cursor-world* raw *snap-bit* nil)))
    ;; values typed into the dynamic input boxes (Tab) hold the cursor to them
    (when (and *req* (dyn-locked-p))
      (let ((q (dyn-point *req* *cursor-world*)))
        (when q (setf *cursor-world* q *snap-bit* nil *polar-hit* nil))))))

(defun echo-input (text)
  (out (format nil "~:[~%~;~]~A~A~%" *at-line-start* (inreq-prompt *req*) text)))

(defun toggle-key (k)
  (flet ((flip (var msg)
           (setf (sysvar var) (if (eql (sysvar var) 1) 0 1))
           (out (format nil "<~A ~A>~%" msg (if (eql (sysvar var) 1) "on" "off")))))
    (cond ((string= k "F2") (funcall 'history-dialog))
          ((string= k "F3")
           (let ((m (sysvar "OSMODE")))
             (setf (sysvar "OSMODE") (if (logtest m 16384) (logand m 16383) (logior m 16384)))
             (when (zerop (logand (sysvar "OSMODE") 16383)) (setf (sysvar "OSMODE") 4133))
             (out (format nil "<Osnap ~A>~%" (if (logtest (sysvar "OSMODE") 16384) "off" "on")))
             (ignore-errors (save-prefs))))
          ((string= k "F7") (flip "GRIDMODE" "Grid") (ignore-errors (save-prefs)) (gui-regen))
          ((string= k "F8") (flip "ORTHOMODE" "Ortho")
           ;; ortho and polar tracking take turns, as in AutoCAD
           (when (and (eql (sysvar "ORTHOMODE") 1) (polar-on-p))
             (setf (sysvar "AUTOSNAP") (logandc2 (sysvar "AUTOSNAP") 8))
             (ignore-errors (save-prefs))))
          ((string= k "F10")
           (setf (sysvar "AUTOSNAP") (logxor (or (sysvar "AUTOSNAP") 0) 8))
           (when (polar-on-p) (setf (sysvar "ORTHOMODE") 0))
           (out (format nil "<Polar ~:[off~;on~]>~%" (polar-on-p)))
           (ignore-errors (save-prefs)))
          ((and (> (length k) 9) (string= (subseq k 0 9) "POLARANG "))
           (let ((v (ignore-errors (distof (subseq k 9) 2))))
             (when (and v (> v 0))
               (setf (sysvar "POLARANG") (d v) (sysvar "AUTOSNAP") (logior (or (sysvar "AUTOSNAP") 0) 8)
                     (sysvar "ORTHOMODE") 0)
               (out (format nil "<Polar on, every ~A degrees>~%" (fmt-plain v)))
               (ignore-errors (save-prefs)))))
          ((string= k "F9") (flip "SNAPMODE" "Snap"))
          ((string= k "F5") (cycle-isoplane))
          ((string= k "F12")
           ;; as in AutoCAD, off is the setting made negative, so turning it on again restores it
           (let ((v (or (sysvar "DYNMODE") 3)))
             (setf (sysvar "DYNMODE") (cond ((plusp v) (- v)) ((minusp v) (- v)) (t 3))))
           (out (format nil "<Dynamic Input ~:[off~;on~]>~%" (dyn-on-p)))
           (ignore-errors (save-prefs))
           (draw-temp))
          ((string= k "ISO") (set-iso (not (iso-mode-p)))
           (out (format nil "<Isometric drafting ~:[off~;on, isoplane ~A~]>~%" (iso-mode-p) (isoplane-name))))
          ((string= k "LWT") (setf (sysvar "LWDISPLAY") (if (eql (sysvar "LWDISPLAY") 1) 0 1)) (gui-regen)))
    (gui-sync)))

(defun handle-common-event (ev)
  "Events that mean the same thing at every prompt. Returns T if handled."
  (let ((ty (event-type ev)))
    ;; with the Model tab split into viewports, a click, wheel or pan in another viewport makes it
    ;; the active one first (a click there does nothing else)
    (when (and (member ty '("B" "W" "P" "DBL") :test #'string=) (fboundp 'tile-switch-at))
      (destructuring-bind (&optional x y &rest ignore) (event-ints ev)
        (declare (ignore ignore))
        (when (and x y (funcall 'tile-switch-at x y) (member ty '("B" "DBL") :test #'string=))
          (return-from handle-common-event t))))
    (cond
      ((string= ty "W") (destructuring-bind (x y dir) (event-ints ev)
                          (zoom-at x y (if (plusp dir) 1.25d0 0.8d0) t) t))
      ((string= ty "IDLE") (when *regen-pending* (setf *regen-pending* nil) (gui-regen)) t)
      ((string= ty "P") (destructuring-bind (x y) (event-ints ev)
                          (push-view) (setf *pan-start* (list x y *vx* *vy*) *mouse-sx* x *mouse-sy* y) t))
      ((string= ty "D") (when *pan-start*
                          (destructuring-bind (x y) (event-ints ev)
                            (destructuring-bind (x0 y0 vx vy) *pan-start*
                              (let ((ox *mouse-sx*) (oy *mouse-sy*))
                                (setf *vx* (- vx (/ (- x x0) *vscale*)) *vy* (+ vy (/ (- y y0) *vscale*)))
                                (setf *mouse-sx* x *mouse-sy* y)
                                ;; move existing items now; regen when the button is released
                                (tk-send-string (format nil "foreach tg {~A grip} {.c move $tg ~D ~D}" (quick-tags) (- x ox) (- y oy)))
                                (draw-temp)))))
                        t)
      ((string= ty "Q") (setf *pan-start* nil) (gui-regen) t)
      ((string= ty "C") (destructuring-bind (w h) (event-ints ev)
                          (setf *full-cw* (max 50 w) *full-ch* (max 50 h) *cw* *full-cw* *ch* *full-ch*)
                          (when (fboundp 'apply-tile-geometry) (funcall 'apply-tile-geometry))
                          (gui-regen) t))
      ((string= ty "X") (setf *mouse-in* nil) (draw-temp) t)
      ((string= ty "K") (toggle-key (event-rest ev)) t)
      ((string= ty "LAYERSET") (let ((n (event-rest ev)) (sel (remove-if-not #'ent-live-p *pickfirst*)))
                                 (when (find-layer n)
                                   (if (and sel (eq (inreq-kind *req*) :command))
                                       ;; objects selected: the layer box moves them to that layer, as in AutoCAD
                                       (with-undo-group (lambda () (change-layer-of sel n)))
                                       (setf (sysvar "CLAYER") (layer-name (find-layer n)))))
                                 (gui-sync) t))
      ((string= ty "LAYERACT") (layer-dialog-action (event-rest ev)) t)
      ((string= ty "HISTACT") (let* ((r (event-rest ev)) (sp (position #\Space r)))
                                (funcall 'history-action (if sp (subseq r 0 sp) r) (and sp (subseq r (1+ sp)))))
                              t)
      ((string= ty "A") t)
      ((string= ty "AUTOSAVE") (ignore-errors (funcall 'autosave-tick)) t)
      ((string= ty "QC") (let* ((r (event-rest ev)) (sp (position #\Space r)))
                           (funcall 'qc-event (if sp (subseq r 0 sp) r) (if sp (subseq r (1+ sp)) "")))
                         t)
      ((string= ty "H") (destructuring-bind (x y) (event-ints ev) (handle-rollover x y)) t)
      ((string= ty "TCLERR") (out (format nil "; window error: ~A~%" (event-rest ev))) t)
      ((string= ty "TREEICONS")
       (setf (sysvar "TREEICONS") (if (string= (event-rest ev) "0") 0 1)) (save-prefs) t)
      ((string= ty "PALETTE") (setf *palette-shown* (string= (event-rest ev) "1")) (refresh-palette) t)
      ((string= ty "PALTYPE") (set-palette-type (event-rest ev)) t)
      ((string= ty "QPCLOSE") (setf *quick-shown* nil) t)
      ((string= ty "CURPROP") (apply-current-prop (event-rest ev)) t)
      (t nil))))

(defun handle-event (ev)
  (let ((ty (event-type ev)) (kind (inreq-kind *req*)))
    (cond
      ((handle-common-event ev) nil)
      ((string= ty "M") (destructuring-bind (x y) (event-ints ev)
                          (update-cursor x y) (draw-temp) (update-coords)))
      ((string= ty "B")
       (destructuring-bind (x y &optional shift) (event-ints ev)
         (setf *press-pos* (list x y) *pick-shift* (eql shift 1))
         (update-cursor x y)
         (case kind
           ((:point :corner :distance :angle)
            (echo-input "") (setf *acq-result* (list :point *cursor-world*)))
           ((:entity :select-point)
            (echo-input "") (setf *acq-result* (list :pick (s->w x y))))
           (:command (command-prompt-pick (s->w x y)))
           (t nil))))
      ((string= ty "U")
       (destructuring-bind (x y) (event-ints ev)
         (when (and (eq kind :corner) (inreq-nosnap *req*) *press-pos*
                    (> (+ (abs (- x (first *press-pos*))) (abs (- y (second *press-pos*)))) 6))
           (update-cursor x y)
           (echo-input "") (setf *acq-result* (list :point *cursor-world*)))
         (setf *press-pos* nil)))
      ((string= ty "DBL")
       ;; double-click on text at the Command: prompt edits it in place, as in AutoCAD; on a layout,
       ;; double-click inside a viewport works in it, outside goes back to the sheet
       (when (eq kind :command)
         (destructuring-bind (x y) (event-ints ev)
           (let* ((e (pick-entity-at (s->w x y)))
                  (vcmd (and (drawing-ctab *dwg*) (fboundp 'viewport-dblclick) (funcall 'viewport-dblclick x y e))))
             (when vcmd
               (take-pickfirst) (sweep-highlights) (refresh-grips) (refresh-palette)
               (setf *acq-result* (list :text vcmd))
               (setf e nil))
             (when (and e (member (ent-type e) '("TEXT" "MTEXT" "ATTDEF") :test #'equal))
               (take-pickfirst) (sweep-highlights) (refresh-grips) (refresh-palette)
               (setf *edit-target* e
                     *acq-result* (list :text "TEXTEDIT")))
             ;; double-click on a hatch: Hatch Edit, as in AutoCAD
             (when (and e (equal (ent-type e) "HATCH"))
               (take-pickfirst) (sweep-highlights) (refresh-grips) (refresh-palette)
               (setf *edit-target* e
                     *acq-result* (list :text "HATCHEDIT")))))))
      ((string= ty "R")
       ;; right-click: a menu at the Command: prompt, Shift+right-click a one-time osnap while
       ;; picking points, otherwise Enter
       (destructuring-bind (&optional x y rx ry shift) (event-ints ev)
         (declare (ignore x y))
         (cond ((and (eql shift 1) rx (member kind '(:point :corner :distance :angle)))
                (tk "osnapmenu ~D ~D" rx ry))
               ((and (eq kind :command) rx (plusp (length *lisp-buffer*))) nil)
               ((and (eq kind :command) rx) (show-command-context-menu rx ry))
               (t (echo-input "") (setf *acq-result* (list :text ""))))))
      ((string= ty "L") (let* ((s (event-rest ev)) (p (dyn-enter-point *req* s)))
                          (echo-input s)
                          (setf *acq-result* (if p (list :point p) (list :text s)))))
      ((string= ty "DYNTAB") (dyn-tab *req* (event-rest ev))
                             (when *mouse-in* (update-cursor *mouse-sx* *mouse-sy*))
                             (draw-temp))
      ((string= ty "E")
       (if (eq kind :command)
           (progn (when *pickfirst* (take-pickfirst) (refresh-grips) (refresh-palette))
                  (sweep-highlights)
                  (out (format nil "~A*Cancel*~%" (inreq-prompt *req*))))
           (progn (out (format nil "~A*Cancel*~%" (inreq-prompt *req*)))
                  (error 'alisp-cancel))))
      ((string= ty "PROP")
       (if (eq kind :command)
           (progn (apply-palette-edit (event-rest ev)) (setf *drawn-modcount* -1)
                  (when (/= *drawn-modcount* (drawing-modcount *dwg*)) (gui-regen)) (refresh-palette))
           (out (format nil "Finish the current command before editing properties.~%"))))
      ((string= ty "MENU")
       (let ((name (event-rest ev)))
         (cond ((and (plusp (length name)) (char= (char name 0) #\'))
                (transparent-menu (subseq name 1)))
               ((eq kind :command) (setf *acq-result* (list :text name)))
               (t (setf *menu-queue* (append *menu-queue* (list name)))
                  (out (format nil "~A*Cancel*~%" (inreq-prompt *req*)))
                  (error 'alisp-cancel)))))
      (t nil))))

(defun transparent-menu (name)
  (cond ((string-equal name "ZOOMEXT") (zoom-extents))
        ((string-equal name "OSNAP") (osnap-dialog))
        ((string-equal name "REGEN") (gui-regen))))

(defun command-prompt-pick (p)
  "Click at the Command: prompt: grip editing, noun/verb selection, implicit windows."
  (let ((g (grip-at p)))
    (when g
      (handler-case (progn (grip-edit g) (gui-regen))
        (alisp-cancel () (refresh-grips)))
      (return-from command-prompt-pick nil)))
  (let ((e (pick-entity-at p)))
    (if e
        (unless (member e *pickfirst*)
          (setf *pickfirst* (append *pickfirst* (list e)))
          (gui-highlight (list e) t)
          (refresh-grips) (refresh-palette))
        (let ((p2 (handler-case
                      (get-user-input :corner :base p :prompt "Specify opposite corner: "
                                              :drag (window-selection-drag p) :nosnap t)
                    (alisp-cancel () nil))))
          (when (pointp p2)
            (let ((new (remove-if (lambda (x) (member x *pickfirst*))
                                  (entities-in-window p p2 (< (px p2) (px p))))))
              (setf *pickfirst* (append *pickfirst* new))
              (gui-highlight new t)
              (refresh-grips) (refresh-palette)))))))

(defun gui-acquire (req)
  (let ((*req* req) (*acq-result* nil) (*dyn-lock* (list nil nil)) (*dyn-active* 0))
    (when (/= *drawn-modcount* (drawing-modcount *dwg*)) (gui-regen) (gui-sync))
    (when (eq (inreq-kind req) :command)
      (setf *pickfirst* (remove-if-not #'ent-live-p *pickfirst*))
      (sweep-highlights)
      (refresh-tabs)
      (refresh-grips)
      (refresh-palette))
    (tk "setprompt ~A ~D" (tq (string-right-trim '(#\Newline) (inreq-prompt req))) (if (inreq-spaces req) 1 0))
    (when *mouse-in* (update-cursor *mouse-sx* *mouse-sy*))
    (draw-temp)
    (setf *prompt-owner* req)
    (unwind-protect
         (loop until *acq-result*
               do (handle-event (next-event))
                  ;; a nested prompt (window corner, grip edit...) changed the label: put ours back
                  (unless (or *acq-result* (eq *prompt-owner* req))
                    (tk "setprompt ~A ~D" (tq (string-right-trim '(#\Newline) (inreq-prompt req))) (if (inreq-spaces req) 1 0))
                    (setf *prompt-owner* req)
                    (draw-temp)))
      (setf *snap-bit* nil))
    *acq-result*))

;;; ---------- AutoLISP GUI functions ----------

(defun gui-grread (track)
  (let ((*req* (make-inreq :kind :grread :prompt "")))
    (tk "setgrread 1")
    (unwind-protect
         (loop
           (let* ((ev (next-event)) (ty (event-type ev)))
             (cond
               ((handle-common-event ev) nil)
               ((string= ty "M") (destructuring-bind (x y) (event-ints ev)
                                   (update-cursor x y) (draw-temp) (update-coords)
                                   (when track (return (list 5 (pt3 *cursor-world*))))))
               ((string= ty "B") (destructuring-bind (x y &rest shift) (event-ints ev)
                                   (declare (ignore shift))
                                   (return (list 3 (s->w x y)))))
               ((string= ty "R") (destructuring-bind (x y &rest more) (event-ints ev)
                                   (declare (ignore more))
                                   (return (list 25 (s->w x y)))))
               ((string= ty "G") (return (list 2 (or (ignore-errors (parse-integer (event-rest ev))) 0))))
               ((string= ty "L") (return (list 2 13)))
               ((string= ty "E") (error 'alisp-cancel))
               ((string= ty "MENU") (return (list 11 0))))))
      (tk "setgrread 0"))))

(defsubr "grread" (&optional track allkeys cursor)
  (declare (ignore allkeys cursor))
  (if (eq *ui* :gui) (gui-grread track)
      (let ((s (get-user-input :string :prompt ""))) (list 2 (if (plusp (length s)) (char-code (char s 0)) 13)))))
(defsubr "grdraw" (from to color &optional hl)
  (declare (ignore hl))
  (push (list (pt3 from) (pt3 to) (if (minusp color) 7 color)) *grdraw-items*)
  (when (eq *ui* :gui)
    (tk-send-string (format nil "~{~A~%~}" (screen-line (list (pt3 from) (pt3 to)) (aci->hex (if (minusp color) 7 color)) "grd"))))
  nil)
(defsubr "grvecs" (vlist &optional mat)
  (declare (ignore mat))
  (let ((color 7))
    (loop while vlist do
      (let ((x (pop vlist)))
        (if (integerp x) (setf color x)
            (let ((y (pop vlist)))
              (push (list (pt3 x) (pt3 y) color) *grdraw-items*)))))
    (when (eq *ui* :gui) (gui-regen))
    nil))
(defsubr "redraw" (&optional e mode)
  (cond ((and e (eq *ui* :gui) (ename-p e))
         (case mode
           (3 (gui-highlight (list e) t))
           (4 (gui-highlight (list e) nil))
           (t (rerender-entity e))))
        ((null e) (setf *grdraw-items* nil) (when (eq *ui* :gui) (gui-regen))))
  nil)
(defsubr "alert" (msg)
  (if (eq *ui* :gui)
      (tk-ask (format nil "tk_messageBox -parent . -title {Fancy Pencil} -message ~A" (tq (lisp-repr msg nil))))
      (out (format nil "** ~A **~%" msg)))
  nil)
(defsubr "getfiled" (title default ext flags)
  (if (eq *ui* :gui)
      (let* ((ext (if (and ext (plusp (length (astr ext)))) (astr ext) "*"))
             (r (tk-ask (format nil "filedlg ~D ~A ~A ~A" (if (logtest (or flags 0) 1) 1 0)
                                (tq title) (tq (or default "")) (tq ext)))))
        (if (string= r "") nil r))
      (let ((s (get-user-input :string :prompt (format nil "~A: " title) :spaces t)))
        (if (string= s "") nil s))))
(defsubr "menucmd" (s) (declare (ignore s)) nil)

(defun file-dialog (save title &key (ext "dxf") initial)
  (if (eq *ui* :gui)
      (let ((r (tk-ask (format nil "filedlg ~D ~A ~A ~A" (if save 1 0) (tq title)
                               (tq (if initial (namestring initial) "")) (tq ext)))))
        ;; (ext may be several, e.g. "dwg dxf")
        (if (string= r "") nil r))
      (let ((s (get-user-input :string :prompt (format nil "~A: " title) :spaces t)))
        (if (string= s "") nil s))))

(defun gui-confirm (msg &optional (type "yesno"))
  "Returns \"yes\", \"no\" or \"cancel\"."
  (tk-ask (format nil "tk_messageBox -parent . -icon question -type ~A -title {Fancy Pencil} -message ~A" type (tq msg))))

;;; ---------- layer dialog ----------

(defun layer-rows ()
  (loop for l in (drawing-layers *dwg*)
        collect (format nil "{~A ~D ~D ~D ~D ~D ~A ~A ~D}" (tq (layer-name l)) (if (layer-on l) 1 0) (if (layer-frozen l) 1 0)
                        (if (layer-locked l) 1 0) (abs (layer-color l))
                        (if (string-equal (layer-name l) (sysvar "CLAYER")) 1 0)
                        (tq (ltype-label (layer-ltype l))) (tq (lineweight-label (layer-lineweight l)))
                        (if (layer-plot l) 1 0))))
(defun show-layer-dialog ()
  (tk "layerdlg {~{~A~^ ~}} ~A ~A" (layer-rows)
      (tcl-list (append '("Continuous") (mapcar #'ltype-name (drawing-ltypes *dwg*))
                        (remove-if (lambda (n) (find-ltype n)) (mapcar #'first *standard-linetypes*))))
      (tcl-list (cons "Default" (mapcar #'lineweight-label *lineweights*)))))

(defun unique-layer-name ()
  (loop for i from 1 for n = (format nil "Layer~D" i) unless (find-layer n) return n))

(defun layer-dialog-action (rest)
  (let* ((sp (position #\Space rest))
         (act (subseq rest 0 sp))
         (arg (if sp (string-left-trim " " (subseq rest (1+ sp))) ""))
         (color nil))
    (when (member act '("ltype" "lweight") :test #'string=)
      (let ((sp2 (position #\Space arg)))
        (setf color (substitute #\Space #\_ (subseq arg 0 sp2))
              arg (if sp2 (subseq arg (1+ sp2)) ""))))
    (when (string= act "color")
      (let ((sp2 (position #\Space arg)))
        (setf color (ignore-errors (parse-integer arg :end sp2))
              arg (if sp2 (subseq arg (1+ sp2)) ""))))
    (when (string= act "pickcolor")             ; the Color button / Color cell: the swatch dialog
      (let ((l (find-layer arg)))
        (setf act "color"
              color (and l (color-dialog (abs (layer-color l)) nil)))))
    (when (string= act "rename")             ; "old<US>new" from the name cell
      (let* ((us (position (code-char 31) arg))
             (old (if us (subseq arg 0 us) arg)) (new (if us (subseq arg (1+ us)) "")))
        (let ((msg (rename-object "Layer" old new)))
          (if msg (tell-user msg) (out (format nil "Layer ~A renamed to ~A.~%" old new))))
        (touch) (show-layer-dialog) (gui-regen) (gui-sync)
        (return-from layer-dialog-action nil)))
    (let ((l (find-layer arg)))
      (cond
        ((string= act "new")
         ;; a new layer opens with its name ready to type over, as in AutoCAD
         (let ((n (unique-layer-name)))
           (ensure-layer n)
           (touch) (show-layer-dialog) (gui-sync)
           (tk "ld_edit_named ~A" (tq n))
           (return-from layer-dialog-action nil)))
        ((null l) nil)
        ((string= act "current")
         (if (layer-frozen l) (out (format nil "Cannot set a frozen layer current.~%"))
             (setf (sysvar "CLAYER") (layer-name l))))
        ((string= act "on") (setf (layer-on l) (not (layer-on l))))
        ((string= act "freeze")
         (if (string-equal (layer-name l) (sysvar "CLAYER"))
             (out (format nil "Cannot freeze the current layer.~%"))
             (setf (layer-frozen l) (not (layer-frozen l)))))
        ((string= act "lock") (setf (layer-locked l) (not (layer-locked l))))
        ((string= act "plot") (setf (layer-plot l) (not (layer-plot l))))
        ((string= act "color") (when (and color (<= 1 color 255)) (setf (layer-color l) color)))
        ((string= act "ltype") (when (ensure-ltype color)
                                 (setf (layer-ltype l) (if (continuous-name-p color) "CONTINUOUS" color))))
        ((string= act "lweight") (let ((lw (parse-lweight-label color))) (when lw (setf (layer-lineweight l) lw))))
        ((string= act "delete")
         (cond ((string-equal (layer-name l) "0") (out (format nil "Layer 0 cannot be deleted.~%")))
               ((string-equal (layer-name l) (sysvar "CLAYER")) (out (format nil "The current layer cannot be deleted.~%")))
               ((some (lambda (e) (string-equal (ent-layer e) (layer-name l))) (all-live-entities))
                (out (format nil "Layer ~A has objects on it and cannot be deleted.~%" (layer-name l))))
               (t (setf (drawing-layers *dwg*) (remove l (drawing-layers *dwg*))))))))
    (touch)
    (show-layer-dialog)
    (gui-regen)
    (gui-sync)))

;;; ---------- startup ----------

(defun gui-out (s)
  (when (plusp (length s))
    (tk "hist ~A" (tq s))
    (setf *at-line-start* (char= (char s (1- (length s))) #\Newline)))
  s)

(defun gui-init ()
  (unless (start-wish) (error "Tk did not start."))
  (setf *ui* :gui *output-fn* #'gui-out)
  ;; wait for the first Configure event so we know the canvas size
  (loop for i from 0 below 400
        for ev = (next-event)
        do (if (string= (event-type ev) "C")
               (progn (handle-common-event ev) (return))
               (push ev *event-buffer*)))
  (setf *event-buffer* (nreverse *event-buffer*))
  ;; installed font families, so text in a missing font falls back to a look-alike
  (setf *tk-families* (ignore-errors (split-string (tk-ask "join [font families] \x1f") (code-char 31))))
  (gui-sync)
  (refresh-tabs t))

;;; ---------- generic dialogs ----------

(defun split-on (s ch)
  (loop with start = 0 for pos = (position ch s :start start)
        collect (subseq s start pos) while pos do (setf start (1+ pos))))

(defun tcl-list (items) (format nil "{~{~A~^ ~}}" (mapcar #'tq items)))

(defun gui-form (title rows ok-label)
  "ROWS are (key label value kind [choices]). Returns an alist of (key . string) or NIL if cancelled."
  (let* ((rowstr (format nil "{~{~A~^ ~}}"
                         (mapcar (lambda (r)
                                   (destructuring-bind (key label value kind &optional choices) r
                                     (format nil "{~A ~A ~A ~A ~A}" (tq key) (tq label)
                                             (tq (if (stringp value) value (lisp-repr value nil)))
                                             (if (eq kind :combo) "combo" "entry")
                                             (tcl-list (or choices nil)))))
                                 rows)))
         (r (tk-ask (format nil "formdlg ~A ~A ~A" (tq title) rowstr (tq ok-label)))))
    (unless (string= r "")
      (loop for part in (split-on r (code-char #x1f))
            for kv = (split-on part (code-char #x1e))
            when (and (= (length kv) 2) (plusp (length (first kv))))
              collect (cons (first kv) (second kv))))))

;;; ---------- toolbar Properties panel ----------

(defparameter *color-choices*
  '("ByLayer" "ByBlock" "Red" "Yellow" "Green" "Cyan" "Blue" "Magenta" "White" "8" "9" "Select Color..."))

(defun color-label (c)
  (cond ((or (null c) (eql c 256) (equalp c "BYLAYER")) "ByLayer")
        ((or (eql c 0) (equalp c "BYBLOCK")) "ByBlock")
        (t (let ((n (if (stringp c) (or (ignore-errors (parse-integer c)) 7) c)))
             (case n (1 "Red") (2 "Yellow") (3 "Green") (4 "Cyan") (5 "Blue") (6 "Magenta") (7 "White")
               (t (princ-to-string n)))))))

(defun color-dialog (&optional (current 7) (by t))
  "AutoCAD's Select Color dialog: the 255 index colors as swatches (plus ByLayer and ByBlock when BY).
Returns 1-255, 256 (ByLayer), 0 (ByBlock), or NIL if cancelled."
  (let* ((cur (cond ((null current) 256) ((equalp current "BYLAYER") 256) ((equalp current "BYBLOCK") 0)
                    ((stringp current) (or (ignore-errors (parse-integer current)) 7)) (t current)))
         (r (tk-ask (format nil "colordlg ~D ~D {~{~A~^ ~}}" cur (if by 1 0)
                            (loop for i from 0 to 255 collect (aci->hex i))))))
    (when (and (> (length r) 3) (string= (subseq r 0 3) "OK "))
      (let ((v (string-trim " " (subseq r 3))))
        (cond ((string-equal v "BYLAYER") 256) ((string-equal v "BYBLOCK") 0)
              (t (let ((n (ignore-errors (parse-integer v)))) (and n (<= 1 n 255) n))))))))

(defun parse-color-label (s &optional current)
  (cond ((string-equal s "Select Color...") (color-dialog current))
        (t (parse-color s))))

(defun ltype-choices ()
  (append '("ByLayer" "ByBlock" "Continuous") (mapcar #'ltype-name (drawing-ltypes *dwg*)) '("Load...")))

(defun lweight-choices ()
  (append '("ByLayer" "ByBlock" "Default") (mapcar #'lineweight-label *lineweights*)))

(defun parse-lweight-label (s)
  (cond ((string-equal s "ByLayer") -1) ((string-equal s "ByBlock") -2) ((string-equal s "Default") -3)
        (t (let ((v (parse-number-text (string-right-trim "m" s)))) (and v (round (* 100 v)))))))

(defun ltype-label (n) (cond ((or (null n) (string-equal n "BYLAYER")) "ByLayer") ((string-equal n "BYBLOCK") "ByBlock")
                             ((string-equal n "CONTINUOUS") "Continuous") (t n)))

(defun sync-current-props ()
  (let* ((sel (remove-if-not #'ent-live-p *pickfirst*))
         (one (lambda (f) (let ((vals (remove-duplicates (mapcar f sel) :test #'equalp)))
                            (if (= (length vals) 1) (first vals) "")))))
    (tk "setcurprops ~A ~A ~A ~A ~A ~A"
        (tcl-list *color-choices*)
        (tq (if sel (funcall one (lambda (e) (color-label (or (dxf 62 (ent-data e)) 256)))) (color-label (sysvar "CECOLOR"))))
        (tcl-list (ltype-choices))
        (tq (if sel (funcall one (lambda (e) (ltype-label (dxf 6 (ent-data e))))) (ltype-label (sysvar "CELTYPE"))))
        (tcl-list (lweight-choices))
        (tq (if sel (funcall one (lambda (e) (lineweight-label (or (dxf 370 (ent-data e)) -1)))) (lineweight-label (sysvar "CELWEIGHT")))))))

(defun load-ltype-interactively ()
  (let ((r (gui-form "Load Linetype"
                     (list (list "N" "Linetype" "DASHED" :combo (mapcar #'first *standard-linetypes*))) "Load")))
    (when r (let ((n (cdr (first r)))) (when (ensure-ltype n) n)))))

(defun apply-current-prop (rest)
  "Toolbar Properties panel: change the selected objects, or the current setting when nothing is selected."
  (let* ((sp (position #\Space rest)) (key (subseq rest 0 sp)) (val (if sp (subseq rest (1+ sp)) ""))
         (sel (remove-if-not #'ent-live-p *pickfirst*)))
    (flet ((apply-to (fn) (with-undo-group (lambda () (dolist (e (filter-editable sel)) (set-entity-data e (funcall fn (ent-data e))))))
                            (gui-regen)))
      (cond
        ((string= key "COLOR")
         (let ((c (parse-color-label val)))
           (when c (if sel (apply-to (lambda (d) (set-entity-color d c)))
                       (setf (sysvar "CECOLOR") (case c (256 "BYLAYER") (0 "BYBLOCK") (t (princ-to-string c))))))))
        ((string= key "LTYPE")
         (let ((n (if (string-equal val "Load...") (load-ltype-interactively) val)))
           (when n
             (ensure-ltype n)
             (let ((v (cond ((string-equal n "ByLayer") "BYLAYER") ((string-equal n "ByBlock") "BYBLOCK") (t n))))
               (if sel (apply-to (lambda (d) (if (string= v "BYLAYER") (rem-dxf 6 d) (set-dxf 6 v d))))
                   (setf (sysvar "CELTYPE") v))))))
        ((string= key "LWEIGHT")
         (let ((lw (parse-lweight-label val)))
           (when lw (if sel (apply-to (lambda (d) (if (= lw -1) (rem-dxf 370 d) (set-dxf 370 lw d))))
                        (setf (sysvar "CELWEIGHT") lw))))))
      (gui-sync)
      (refresh-palette))))
