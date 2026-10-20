;;;; gui.lisp — Tk front end (via a `wish` subprocess), view, rendering, events.

(in-package :lispdraft)

(defvar *wish-process* nil)
(defvar *wish-in* nil "Stream to wish's stdin.")
(defvar *wish-out* nil "Stream from wish's stdout.")
(defvar *event-buffer* nil)
(defvar *gui-lock* (make-lock* "gui"))

;;; view state
(defvar *vx* 6d0) (defvar *vy* 4.5d0) (defvar *vscale* 60d0)
(defvar *cw* 1000) (defvar *ch* 600)
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

;;; ---------- process ----------

(defun script-directory ()
  (or *gui-dir*
      (let ((f (or *load-truename* *compile-file-truename*)))
        (and f (directory-namestring f)))))

(defun run-wish (program script)
  #+sbcl
  (let ((p (sb-ext:run-program program (list script) :search t :wait nil
                                                     :input :stream :output :stream :error nil
                                                     :external-format :utf-8)))
    (values p (sb-ext:process-input p) (sb-ext:process-output p)))
  #+ecl
  (multiple-value-bind (stream code p)
      (ext:run-program program (list script) :input :stream :output :stream :error nil :wait nil
                                             :external-format :utf-8)
    (declare (ignore code))
    (values p stream stream))
  #+ccl
  (let ((p (ccl:run-program program (list script) :wait nil :input :stream :output :stream :error nil
                                                  :external-format :utf-8)))
    (values p (ccl:external-process-input-stream p) (ccl:external-process-output-stream p))))

(defun start-wish ()
  (let ((script (namestring (merge-pathnames "gui.tcl" (script-directory))))
        (candidates (remove nil (list (getenv "LISPDRAFT_WISH") "wish" "wish8.6" "wish86" "wish86t" "wish9.0" "wish8.7"))))
    (dolist (c candidates (error "Could not start Tk. Install it (e.g. `sudo apt install tk`) or set LISPDRAFT_WISH to the wish executable."))
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
  (let* ((h (or (dxf 40 data) 0.2d0))
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
               (list (format nil ".c create text ~A ~A -text ~A -anchor ~A -justify ~A -angle ~,3F -font {Helvetica -~D} -fill ~A -tags {~A}"
                             (fmtn x) (fmtn y) (tq s) anchor justify (deg rot) px-size
                             (if hl "#8fc4ff" color) tags)))))))))

(defun sub-color (sub parent-aci)
  (let ((c (dxf 62 sub)))
    (cond ((eql c 0) parent-aci)
          ((and c (/= c 256)) (abs c))
          ((equal (dxf 8 sub) "0") parent-aci)
          (t (let ((l (find-layer (dxf 8 sub)))) (if l (abs (layer-color l)) parent-aci))))))

(defun render-data (data aci tags hl &optional xf (depth 0))
  (let ((type (dxf 0 data)) (color (aci->hex aci)))
    (cond
      ((> depth 8) nil)
      ((member type '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal)
       (render-text (if xf (xform-data data xf) data) color tags hl))
      ((equal type "INSERT")
       (let ((b (find-block (dxf 2 data))))
         (when b
           (let ((ixf (insert-xf data (block-def-base b))))
             (let ((cxf (if xf (xf-compose ixf xf) ixf)))
               (loop for sub in (block-def-entities b)
                     append (render-data sub (sub-color sub aci) tags hl cxf (1+ depth))))))))
      ((equal type "DIMENSION")
       (let ((b (find-block (dxf 2 data))))
         (when b
           (loop for sub in (block-def-entities b)
                 append (render-data sub (sub-color sub aci) tags hl xf (1+ depth))))))
      ((equal type "SOLID")
       (let* ((pts (remove nil (list (dxf 10 data) (dxf 11 data) (dxf 13 data) (dxf 12 data))))
              (pts (if xf (mapcar (lambda (p) (xf-point xf (pt3 p))) pts) (mapcar #'pt3 pts)))
              (coords (loop for p in pts append (multiple-value-list (w->s p)))))
         (when (and (every (lambda (c) (< -30000 c 30000)) coords)
                    (some #'piece-visible-p (loop for (a b) on pts while b collect (list :seg a b))))
           (list (format nil ".c create polygon ~{~A~^ ~} -fill ~A -outline ~A -tags {~A}"
                         (mapcar #'fmtn coords) color color tags)))))
      (t
       (let ((width (if (and (equal type "LWPOLYLINE") (dxf 43 data))
                        (max 1 (round (* (dxf 43 data) *vscale* (if xf (xf-scale xf) 1))))
                        1)))
         (loop for pc in (data-pieces data)
               append (loop for tp in (if xf (transform-piece pc xf) (list pc))
                            append (render-piece tp color tags :dash hl :width width))))))))

(defun entity-render-cmds (e)
  (let ((data (ename-data e)))
    (render-data data (entity-aci data) (format nil "ent h~A" (ename-handle e)) (gethash e *hl*))))

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
      (tk-send-string (format nil "~{~A~%~}" cmds))
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

;;; ---------- temporary graphics (cursor, snaps, rubber band) ----------

(defparameter *snap-names*
  '((1 . "Endpoint") (2 . "Midpoint") (4 . "Center") (8 . "Node") (16 . "Quadrant") (32 . "Intersection")
    (64 . "Insertion") (128 . "Perpendicular") (256 . "Tangent") (512 . "Nearest")))

(defun snap-glyph (x y bit)
  (let ((s 6) (c "#3fd25f") (tags "tmp") (x (round x)) (y (round y)))
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
    (t (render-piece item "#d8dde4" "tmp"))))

(defun cursor-cmds ()
  (when *mouse-in*
    (let* ((x *mouse-sx*) (y *mouse-sy*) (kind (and *req* (inreq-kind *req*)))
           (pick (member kind '(:command :entity :select-point)))
           (cross (not (member kind '(:entity :select-point))))
           (pb (or (sysvar "PICKBOX") 5)) (out nil) (col "#c9ced6"))
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
          (when (and *snap-bit* (member kind '(:point :corner :distance :angle)))
            (multiple-value-bind (x y) (w->s cur)
              (setf cmds (nconc cmds (snap-glyph x y *snap-bit*)
                                (tooltip-cmds x y (cdr (assoc *snap-bit* *snap-names*)))))))))
      (setf cmds (nconc cmds (cursor-cmds)))
      (tk-send-string (format nil "~{~A~%~}" cmds)))))

(defun update-coords ()
  (let ((p *cursor-world*) (prec (or (sysvar "LUPREC") 4)))
    (tk "setxy ~A" (tq (format nil "~A, ~A, ~A" (rtos (px p) 2 prec) (rtos (py p) 2 prec) (rtos (pz p) 2 prec))))))

(defun gui-sync ()
  "Refresh layer combo, toggles and title."
  (let* ((names (mapcar #'layer-name (drawing-layers *dwg*)))
         (cur (sysvar "CLAYER"))
         (l (find-layer cur)))
    (tk "setlayers {~{~A~^ ~}} ~A ~A" (mapcar #'tq names) (tq cur) (aci->hex (if l (abs (layer-color l)) 7)))
    (tk "settoggles ~D ~D ~D ~D" (sysvar "GRIDMODE") (sysvar "SNAPMODE") (sysvar "ORTHOMODE")
        (if (logtest (sysvar "OSMODE") 16384) 0 (if (plusp (sysvar "OSMODE")) 1 0)))
    (tk "settitle ~A" (tq (format nil "LispDraft — ~A~A" (drawing-display-name)
                                  (if (/= (drawing-modcount *dwg*) (drawing-saved-modcount *dwg*)) " *" ""))))))

(defun drawing-display-name ()
  (let ((f (drawing-filename *dwg*))) (if f (file-namestring f) "Drawing1.dxf")))

;;; ---------- event handling ----------

(defun update-cursor (sx sy)
  (setf *mouse-sx* sx *mouse-sy* sy *mouse-in* t)
  (let* ((raw (s->w sx sy)) (kind (and *req* (inreq-kind *req*))))
    (if (member kind '(:point :corner :distance :angle))
        (multiple-value-bind (q bit) (resolve-cursor-point raw (inreq-base *req*))
          (setf *cursor-world* q *snap-bit* bit))
        (setf *cursor-world* raw *snap-bit* nil))))

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
          ((string= k "F9") (flip "SNAPMODE" "Snap")))
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
      (t nil))))

(defun handle-event (ev)
  (let ((ty (event-type ev)) (kind (inreq-kind *req*)))
    (cond
      ((handle-common-event ev) nil)
      ((string= ty "M") (destructuring-bind (x y) (event-ints ev)
                          (update-cursor x y) (draw-temp) (update-coords)))
      ((string= ty "B")
       (destructuring-bind (x y) (event-ints ev)
         (update-cursor x y)
         (case kind
           ((:point :corner :distance :angle)
            (echo-input "") (setf *acq-result* (list :point *cursor-world*)))
           ((:entity :select-point)
            (echo-input "") (setf *acq-result* (list :pick (s->w x y))))
           (:command (command-prompt-pick (s->w x y)))
           (t nil))))
      ((string= ty "R") (echo-input "") (setf *acq-result* (list :text "")))
      ((string= ty "L") (let ((s (event-rest ev))) (echo-input s) (setf *acq-result* (list :text s))))
      ((string= ty "E")
       (if (eq kind :command)
           (progn (when *pickfirst* (take-pickfirst)) (out (format nil "~A*Cancel*~%" (inreq-prompt *req*))))
           (progn (out (format nil "~A*Cancel*~%" (inreq-prompt *req*)))
                  (error 'alisp-cancel))))
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
        ((string-equal name "REGEN") (gui-regen))))

(defun command-prompt-pick (p)
  "Click at the Command: prompt: noun/verb selection, implicit windows."
  (let ((e (pick-entity-at p)))
    (if e
        (unless (member e *pickfirst*)
          (setf *pickfirst* (append *pickfirst* (list e)))
          (gui-highlight (list e) t))
        (let ((p2 (handler-case
                      (get-user-input :corner :base p :prompt "Specify opposite corner: "
                                              :drag (window-selection-drag p))
                    (alisp-cancel () nil))))
          (when (pointp p2)
            (let ((new (remove-if (lambda (x) (member x *pickfirst*))
                                  (entities-in-window p p2 (< (px p2) (px p))))))
              (setf *pickfirst* (append *pickfirst* new))
              (gui-highlight new t)))))))

(defun gui-acquire (req)
  (let ((*req* req) (*acq-result* nil))
    (when (/= *drawn-modcount* (drawing-modcount *dwg*)) (gui-regen) (gui-sync))
    (tk "setprompt ~A ~D" (tq (string-right-trim '(#\Newline) (inreq-prompt req))) (if (inreq-spaces req) 1 0))
    (when *mouse-in* (update-cursor *mouse-sx* *mouse-sy*))
    (draw-temp)
    (unwind-protect
         (loop until *acq-result* do (handle-event (next-event)))
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
               ((string= ty "B") (destructuring-bind (x y) (event-ints ev)
                                   (return (list 3 (s->w x y)))))
               ((string= ty "R") (destructuring-bind (x y) (event-ints ev)
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
      (tk-ask (format nil "tk_messageBox -parent . -title LispDraft -message ~A" (tq (lisp-repr msg nil))))
      (out (format nil "** ~A **~%" msg)))
  nil)
(defsubr "getfiled" (title default ext flags)
  (if (eq *ui* :gui)
      (let ((r (tk-ask (format nil "~A -parent . -title ~A -initialfile ~A -filetypes {{{~A files} {.~A}} {{All files} *}}~A"
                               (if (logtest (or flags 0) 1) "tk_getSaveFile" "tk_getOpenFile")
                               (tq title) (tq (file-namestring (or default ""))) ext ext
                               (if (logtest (or flags 0) 1) (format nil " -defaultextension .~A" ext) "")))))
        (if (string= r "") nil r))
      (let ((s (get-user-input :string :prompt (format nil "~A: " title) :spaces t)))
        (if (string= s "") nil s))))
(defsubr "menucmd" (s) (declare (ignore s)) nil)

(defun file-dialog (save title &key (ext "dxf") initial)
  (if (eq *ui* :gui)
      (let ((r (tk-ask (format nil "~A -parent . -title ~A ~A -filetypes {{{~A files} {.~A}} {{All files} *}}~A"
                               (if save "tk_getSaveFile" "tk_getOpenFile") (tq title)
                               (if initial (format nil "-initialfile ~A" (tq (file-namestring initial))) "")
                               (string-upcase ext) ext
                               (if save (format nil " -defaultextension .~A" ext) "")))))
        (if (string= r "") nil r))
      (let ((s (get-user-input :string :prompt (format nil "~A: " title) :spaces t)))
        (if (string= s "") nil s))))

(defun gui-confirm (msg &optional (type "yesno"))
  "Returns \"yes\", \"no\" or \"cancel\"."
  (tk-ask (format nil "tk_messageBox -parent . -icon question -type ~A -title LispDraft -message ~A" type (tq msg))))

;;; ---------- layer dialog ----------

(defun layer-rows ()
  (loop for l in (drawing-layers *dwg*)
        collect (format nil "{~A ~D ~D ~D ~D ~D}" (tq (layer-name l)) (if (layer-on l) 1 0) (if (layer-frozen l) 1 0)
                        (if (layer-locked l) 1 0) (abs (layer-color l))
                        (if (string-equal (layer-name l) (sysvar "CLAYER")) 1 0))))
(defun show-layer-dialog () (tk "layerdlg {~{~A~^ ~}}" (layer-rows)))

(defun unique-layer-name ()
  (loop for i from 1 for n = (format nil "Layer~D" i) unless (find-layer n) return n))

(defun layer-dialog-action (rest)
  (let* ((sp (position #\Space rest))
         (act (subseq rest 0 sp))
         (arg (if sp (string-left-trim " " (subseq rest (1+ sp))) ""))
         (color nil))
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
        ((string= act "color") (when (and color (<= 1 color 255)) (setf (layer-color l) color)))
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
  (gui-sync))
