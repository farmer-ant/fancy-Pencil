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

(defun w->s (p)
  (values (+ (/ *cw* 2d0) (* (- (px p) *vx*) *vscale*))
          (- (/ *ch* 2d0) (* (- (py p) *vy*) *vscale*))))
(defun s->w (sx sy)
  (pt (+ *vx* (/ (- sx (/ *cw* 2d0)) *vscale*))
      (- *vy* (/ (- sy (/ *ch* 2d0)) *vscale*))))
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
(defun zoom-extents ()
  (let ((b (drawing-extents)))
    (if b
        (destructuring-bind (x0 y0 x1 y1) b
          (let ((pad (* 0.02 (max 1d-6 (- x1 x0) (- y1 y0)))))
            (zoom-to-box (- x0 pad) (- y0 pad) (+ x1 pad) (+ y1 pad) 0.96d0)))
        (zoom-to-box 0 0 12 9))))
(defvar *regen-pending* nil)
(defun zoom-at (sx sy factor &optional quick)
  (let ((w (s->w sx sy)))
    (setf *vscale* (min 1d9 (max 1d-9 (* *vscale* factor))))
    (setf *vx* (- (px w) (/ (- sx (/ *cw* 2d0)) *vscale*))
          *vy* (+ (py w) (/ (- sy (/ *ch* 2d0)) *vscale*)))
    (if quick
        ;; instant feedback: scale what is on the canvas; exact regen follows on IDLE
        (progn (tk-send-string (format nil "foreach tg {ent grd grid} {.c scale $tg ~D ~D ~,5F ~,5F}" sx sy factor factor))
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

(defun aci->hex (i)
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
                 (let ((c (clip-seg x1 y1 x2 y2 (- m) (- m) (+ *cw* m) (+ *ch* m))))
                   (when c
                     (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -width ~A~A -tags {~A}"
                                   (fmtn (first c)) (fmtn (second c)) (fmtn (third c)) (fmtn (fourth c))
                                   color width (if dash " -dash {6 3}" "") tags)
                           out))))))
    (nreverse out)))

(defun piece-visible-p (pc)
  (multiple-value-bind (x0 y0 x1 y1) (view-rect 50)
    (let ((b (pieces-bbox (list pc))))
      (and b (not (or (> (first b) x1) (< (third b) x0) (> (second b) y1) (< (fourth b) y0)))))))

(defun render-piece (pc color tags &key dash (width 1))
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

(defun render-point-glyph (x y color tags)
  (let* ((x (round x)) (y (round y))(mode (or (sysvar "PDMODE") 0)) (s 5) (base (mod mode 32)) (out nil))
    (case base
      (0 (push (format nil ".c create rectangle ~A ~A ~A ~A -outline ~A -fill ~A -tags {~A}"
                       (fmtn (- x 1)) (fmtn (- y 1)) (fmtn (+ x 1)) (fmtn (+ y 1)) color color tags) out))
      (1 nil)
      (2 (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}" (- x s) y (+ x s) y color tags) out)
         (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}" x (- y s) x (+ y s) color tags) out))
      (t (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}" (- x s) (- y s) (+ x s) (+ y s) color tags) out)
         (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}" (- x s) (+ y s) (+ x s) (- y s) color tags) out)))
    (when (logtest mode 32)
      (push (format nil ".c create oval ~A ~A ~A ~A -outline ~A -tags {~A}" (- x s) (- y s) (+ x s) (+ y s) color tags) out))
    (when (logtest mode 64)
      (push (format nil ".c create rectangle ~A ~A ~A ~A -outline ~A -tags {~A}" (- x s) (- y s) (+ x s) (+ y s) color tags) out))
    out))

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
  (let* ((h (text-effective-height data))
         (px-size (round (/ (* h *vscale*) 0.72d0)))
         (p (text-insertion data))
         (rot (or (dxf 50 data) 0d0))
         (s (text-string-display (or (dxf 1 data) ""))))
    (when (and p (plusp (length s)))
      (multiple-value-bind (x y) (w->s p)
        (cond
          ((< px-size 3)
           (when (and (< -50 x (+ *cw* 50)) (< -50 y (+ *ch* 50)))
             (list (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}"
                           (fmtn x) (fmtn y) (fmtn (+ x (* (cos rot) (max 2 (* 0.6 h *vscale* (length s))))))
                           (fmtn (- y (* (sin rot) (max 2 (* 0.6 h *vscale* (length s)))))) color tags))))
          ((> px-size 3000) nil)
          ((or (< x -4000) (> x (+ *cw* 4000)) (< y -4000) (> y (+ *ch* 4000))) nil)
          (t (multiple-value-bind (anchor justify) (text-anchor data)
               (list (format nil ".c create text ~A ~A -text ~A -anchor ~A -justify ~A -angle ~,3F -font {~A} -fill ~A -tags {~A}~A"
                             (fmtn x) (fmtn y) (tq s) anchor justify (deg rot) (text-font-spec data px-size)
                             (if hl "#8fc4ff" color) tags
                             (let ((w (and (equal (dxf 0 data) "MTEXT") (dxf 41 data))))
                               (if (and w (plusp w)) (format nil " -width ~D" (max 1 (round (* w *vscale*)))) "")))))))))))

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
                  (member type '("LWPOLYLINE" "SPLINE" "ELLIPSE" "LEADER") :test #'equal))
             ;; many short straight pieces: one canvas line per connected run (far fewer items)
             (render-chained (loop for pc in (data-pieces data)
                                   append (if xf (transform-piece pc xf) (list pc)))
                             color tags hl width)
             (loop for pc in (data-pieces data)
                   append (loop for tp in (if xf (transform-piece pc xf) (list pc))
                                append (if pattern
                                           (render-linetyped tp pattern scale color tags hl width)
                                           (render-piece tp color tags :dash hl :width width))))))))))

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
                (let ((c (clip-seg x1 y1 x2 y2 (- m) (- m) (+ *cw* m) (+ *ch* m))))
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
  (let* ((data (ename-data e))
         (tags (format nil "ent h~A" (ename-handle e)))
         (b (ignore-errors (entity-bbox e))))
    (if (and b (not (member (dxf 0 data) '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal)))
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

(defun grid-cmds ()
  (when (eql (sysvar "GRIDMODE") 1)
    (multiple-value-bind (x0 y0 x1 y1) (view-rect 0)
      (let* ((g (max 1d-9 (d (first (sysvar "GRIDUNIT"))))) (major 5) (out nil))
        (loop while (< (* g *vscale*) 8) do (setf g (* g 5)))
        (loop while (> (* g *vscale*) 200) do (setf g (/ g 5)))
        (flet ((vline (x col) (multiple-value-bind (sx) (w->s (pt x 0))
                                (push (format nil ".c create line ~A 0 ~A ~D -fill ~A -tags grid" (fmtn sx) (fmtn sx) *ch* col) out)))
               (hline (y col) (multiple-value-bind (sx sy) (w->s (pt 0 y)) (declare (ignore sx))
                                (push (format nil ".c create line 0 ~A ~D ~A -fill ~A -tags grid" (fmtn sy) *cw* (fmtn sy) col) out))))
          (loop for i from (floor x0 g) to (ceiling x1 g)
                do (vline (* i g) (cond ((zerop i) "#45343a") ((zerop (mod i major)) "#323a46") (t "#282f39"))))
          (loop for i from (floor y0 g) to (ceiling y1 g)
                do (hline (* i g) (cond ((zerop i) "#2f4536") ((zerop (mod i major)) "#323a46") (t "#282f39")))))
        out))))

(defun ucs-icon-cmds ()
  (let ((x 34) (y (- *ch* 30)))
    (list (format nil ".c create line ~D ~D ~D ~D -fill #d24b4b -width 2 -arrow last -tags ucs" x y (+ x 44) y)
          (format nil ".c create line ~D ~D ~D ~D -fill #4bbf5a -width 2 -arrow last -tags ucs" x y x (- y 44))
          (format nil ".c create text ~D ~D -text X -fill #d24b4b -font {Helvetica 9 bold} -tags ucs" (+ x 52) y)
          (format nil ".c create text ~D ~D -text Y -fill #4bbf5a -font {Helvetica 9 bold} -tags ucs" x (- y 53))
          (format nil ".c create rectangle ~D ~D ~D ~D -outline #8a94a3 -tags ucs" (- x 3) (- y 3) (+ x 3) (+ y 3)))))

(defun entity-in-view-p (e)
  (let ((b (entity-bbox e)))
    (multiple-value-bind (x0 y0 x1 y1) (view-rect 50)
      (and b (not (or (> (first b) x1) (< (third b) x0) (> (second b) y1) (< (fourth b) y0)))))))

(defun gui-regen ()
  (when (eq *ui* :gui)
    (let ((cmds (list ".c delete all")))
      (setf cmds (nconc cmds (grid-cmds)))
      (dolist (e (live-entities))
        (when (and (entity-visible-p e) (entity-in-view-p e))
          (setf cmds (nconc cmds (entity-render-cmds e)))))
      (dolist (g *grdraw-items*)
        (destructuring-bind (a b aci) g
          (setf cmds (nconc cmds (screen-line (list a b) (aci->hex aci) "grd")))))
      (setf cmds (nconc cmds (ucs-icon-cmds)))
      (when (fboundp 'block-editor-banner-cmds) (setf cmds (nconc cmds (funcall 'block-editor-banner-cmds))))
      (tk-send-string (format nil "~{~A~%~}" cmds))
      (refresh-grips)
      (setf *drawn-modcount* (drawing-modcount *dwg*))
      (draw-temp))))

(defun rerender-entity (e)
  (tk-send-string (format nil ".c delete h~A~%~{~A~%~}.c raise tmp"
                          (ename-handle e)
                          (and (entity-visible-p e) (entity-render-cmds e)))))

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
    (:hotgrip (multiple-value-bind (x y) (w->s (second item))
                (let ((s (grip-pixel-size)))
                  (list (format nil ".c create rectangle ~D ~D ~D ~D -fill #ff2a2a -outline #6b0000 -tags tmp"
                                (round (- x s)) (round (- y s)) (round (+ x s)) (round (+ y s)))))))
    (t (render-piece item "#d8dde4" "tmp"))))

(defun cursor-cmds ()
  (when *mouse-in*
    (let* ((x *mouse-sx*) (y *mouse-sy*) (kind (and *req* (inreq-kind *req*)))
           (pick (member kind '(:command :entity :select-point)))
           (cross (not (member kind '(:entity :select-point))))
           (pb (or (sysvar "PICKBOX") 5)) (out nil) (col (cursor-color)))
      (when cross
        (let ((len (let ((cs (or (sysvar "CURSORSIZE") 100)))
                     (if (>= cs 100) 10000 (round (* (/ cs 200) (max *cw* *ch*)))))))
          (push (format nil ".c create line ~D ~D ~D ~D -fill ~A -tags tmp" (- x len) y (- x pb) y col) out)
          (push (format nil ".c create line ~D ~D ~D ~D -fill ~A -tags tmp" (+ x pb) y (+ x len) y col) out)
          (push (format nil ".c create line ~D ~D ~D ~D -fill ~A -tags tmp" x (- y len) x (- y pb) col) out)
          (push (format nil ".c create line ~D ~D ~D ~D -fill ~A -tags tmp" x (+ y pb) x (+ y len) col) out)))
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
          (when (and *snap-bit* (member kind '(:point :corner :distance :angle)))
            (multiple-value-bind (x y) (w->s cur)
              (setf cmds (nconc cmds (snap-glyph x y *snap-bit*)
                                (tooltip-cmds x y (cdr (assoc *snap-bit* *snap-names*)))))))))
      (when (and req (eq (inreq-kind req) :command)) (setf cmds (nconc cmds (grip-hover-cmds))))
      (setf cmds (nconc cmds (cursor-cmds)))
      (tk-send-string (format nil "~{~A~%~}" cmds)))))

(defun update-coords ()
  (let ((p *cursor-world*))
    (tk "setxy ~A" (tq (format nil "~A, ~A, ~A" (fmt-len (px p)) (fmt-len (py p)) (fmt-len (pz p)))))))

(defun gui-sync ()
  "Refresh layer combo, toggles and title."
  (let* ((names (mapcar #'layer-name (drawing-layers *dwg*)))
         (cur (sysvar "CLAYER"))
         (l (find-layer cur)))
    (tk "setlayers {~{~A~^ ~}} ~A ~A" (mapcar #'tq names) (tq cur) (aci->hex (if l (abs (layer-color l)) 7)))
    (tk "settoggles ~D ~D ~D ~D ~D" (sysvar "GRIDMODE") (sysvar "SNAPMODE") (sysvar "ORTHOMODE")
        (if (logtest (sysvar "OSMODE") 16384) 0 (if (plusp (sysvar "OSMODE")) 1 0))
        (sysvar "LWDISPLAY"))
    (sync-current-props)
    (tk "settitle ~A" (tq (format nil "Fancy Pencil — ~A~A" (drawing-display-name)
                                  (if (/= (drawing-modcount *dwg*) (drawing-saved-modcount *dwg*)) " *" ""))))))

(defun drawing-display-name ()
  (let ((f (drawing-filename *dwg*))) (if f (file-namestring f) (untitled-name))))

;;; ---------- event handling ----------

(defun update-cursor (sx sy)
  (setf *mouse-sx* sx *mouse-sy* sy *mouse-in* t)
  (let* ((raw (s->w sx sy)) (kind (and *req* (inreq-kind *req*))))
    (cond ((and (member kind '(:point :corner)) (inreq-nosnap *req*))
           (setf *cursor-world* raw *snap-bit* nil))
          ((member kind '(:point :corner :distance :angle))
           ;; ortho constrains lines, never the opposite corner of a rectangle or window
           (multiple-value-bind (q bit) (resolve-cursor-point raw (inreq-base *req*) :ortho (not (eq kind :corner)))
             (setf *cursor-world* q *snap-bit* bit)
             ;; no object snap here: line up with another dimension's line if close to one
             (unless bit
               (let ((dq (and *dimline-snap* (member kind '(:point)) (apply-dimline-snap q))))
                 (when dq (setf *cursor-world* dq *snap-bit* :dimline))))))
          (t (setf *cursor-world* raw *snap-bit* nil)))))

(defun echo-input (text)
  (out (format nil "~:[~%~;~]~A~A~%" *at-line-start* (inreq-prompt *req*) text)))

(defun toggle-key (k)
  (flet ((flip (var msg)
           (setf (sysvar var) (if (eql (sysvar var) 1) 0 1))
           (out (format nil "<~A ~A>~%" msg (if (eql (sysvar var) 1) "on" "off")))))
    (cond ((string= k "F3")
           (let ((m (sysvar "OSMODE")))
             (setf (sysvar "OSMODE") (if (logtest m 16384) (logand m 16383) (logior m 16384)))
             (when (zerop (logand (sysvar "OSMODE") 16383)) (setf (sysvar "OSMODE") 4133))
             (out (format nil "<Osnap ~A>~%" (if (logtest (sysvar "OSMODE") 16384) "off" "on")))))
          ((string= k "F7") (flip "GRIDMODE" "Grid") (gui-regen))
          ((string= k "F8") (flip "ORTHOMODE" "Ortho"))
          ((string= k "F9") (flip "SNAPMODE" "Snap"))
          ((string= k "LWT") (setf (sysvar "LWDISPLAY") (if (eql (sysvar "LWDISPLAY") 1) 0 1)) (gui-regen)))
    (gui-sync)))

(defun handle-common-event (ev)
  "Events that mean the same thing at every prompt. Returns T if handled."
  (let ((ty (event-type ev)))
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
                                (tk-send-string (format nil "foreach tg {ent grd grid} {.c move $tg ~D ~D}" (- x ox) (- y oy)))
                                (draw-temp)))))
                        t)
      ((string= ty "Q") (setf *pan-start* nil) (gui-regen) t)
      ((string= ty "C") (destructuring-bind (w h) (event-ints ev)
                          (setf *cw* (max 50 w) *ch* (max 50 h)) (gui-regen) t))
      ((string= ty "X") (setf *mouse-in* nil) (draw-temp) t)
      ((string= ty "K") (toggle-key (event-rest ev)) t)
      ((string= ty "LAYERSET") (let ((n (event-rest ev)))
                                 (when (find-layer n)
                                   (setf (sysvar "CLAYER") (layer-name (find-layer n))))
                                 (gui-sync) t))
      ((string= ty "LAYERACT") (layer-dialog-action (event-rest ev)) t)
      ((string= ty "A") t)
      ((string= ty "H") (destructuring-bind (x y) (event-ints ev) (handle-rollover x y)) t)
      ((string= ty "TCLERR") (out (format nil "; window error: ~A~%" (event-rest ev))) t)
      ((string= ty "PALETTE") (setf *palette-shown* (string= (event-rest ev) "1")) (refresh-palette) t)
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
       ;; double-click on text at the Command: prompt edits it in place, as in AutoCAD
       (when (eq kind :command)
         (destructuring-bind (x y) (event-ints ev)
           (let ((e (pick-entity-at (s->w x y))))
             (when (and e (member (ent-type e) '("TEXT" "MTEXT" "ATTDEF") :test #'equal))
               (take-pickfirst) (sweep-highlights) (refresh-grips) (refresh-palette)
               (setf *edit-target* e
                     *acq-result* (list :text "TEXTEDIT")))))))
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
      ((string= ty "L") (let ((s (event-rest ev))) (echo-input s) (setf *acq-result* (list :text s))))
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
  (let ((*req* req) (*acq-result* nil))
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
    (let ((l (find-layer arg)))
      (cond
        ((string= act "new") (ensure-layer (unique-layer-name)))
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
               ((some (lambda (e) (string-equal (ent-layer e) (layer-name l))) (live-entities))
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

(defun parse-color-label (s)
  (cond ((string-equal s "Select Color...")
         (let ((r (gui-form "Select Color" (list (list "C" "AutoCAD Color Index (1-255)" "7" :entry)) "OK")))
           (and r (parse-color (cdr (first r))))))
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
