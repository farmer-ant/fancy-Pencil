;;;; navigate.lisp — 3D navigation: orbit (constrained, free, continuous), adjust distance, swivel,
;;;; zoom and pan, as on AutoCAD's 3D Navigation tools.
;;;;
;;;; 3DORBIT (3DO, ORBIT)  turns the view around a target; up stays up (Z), so it moves along the
;;;;                       XY plane (left-right) and the Z axis (up-down): constrained orbit.
;;;; 3DFORBIT              free orbit: inside the green circle drag turns the view any way; outside
;;;;                       it, drag rolls the view about your line of sight; the small circles on the
;;;;                       left and right turn it about the vertical, top and bottom about the horizontal.
;;;; 3DCORBIT              continuous orbit: drag and let go and the view keeps turning that way
;;;;                       (faster the faster you dragged); click to stop, drag again to change.
;;;; 3DDISTANCE            drag up to come closer (objects larger), down to move away.
;;;; 3DSWIVEL              the camera stays where it is and turns, like turning your head: the target
;;;;                       moves. Left-right along the XY plane, up-down along the Z axis.
;;;; 3DZOOM                drag up to zoom in, down to zoom out.
;;;; 3DPAN                 drag the view.
;;;;
;;;; At their prompts each of them can switch to any other (Constrained/Free/Continuous/Distance/
;;;; Swivel/Zoom/Pan). While you drag, the target point shows as a small dark sphere. Where the target
;;;; is follows ORBITAUTOTARGET: 1 (the default) the middle of the selected objects, or of the whole
;;;; drawing when nothing is selected; 0 the point in the middle of the screen.
;;;; Shift + middle-button drag orbits at any time (also inside a command); Ctrl + Shift + middle
;;;; drag is a free orbit.

(in-package :fancy-pencil)

(defvar *nav-distance* nil "Camera distance for SWIVEL / 3DDISTANCE (NIL: from the view's width).")
(defvar *nav-spin* nil "Continuous orbit in progress: (daz del) per tick, or (:free ax ay) for free orbit.")

;;; ---------- where things are ----------

(defun nav-entity-points (e)
  "World points bounding entity E (its 3D edges, or its plan box at its height and thickness)."
  (let ((data (ent-data e)))
    (if (or (offplane-p data) (wcs-type-p (dxf 0 data)))
        (loop for pc in (ignore-errors (entity-pieces e))
              append (case (first pc)
                       ((:seg :gseg) (list (pt3 (second pc)) (pt3 (third pc))))
                       (:pt (list (pt3 (second pc))))))
        (let ((b (ignore-errors (entity-bbox e))))
          (when b
            (let* ((z0 (entity-elevation data)) (z1 (+ z0 (d (or (dxf 39 data) 0)))))
              (loop for x in (list (first b) (third b))
                    append (loop for y in (list (second b) (fourth b))
                                 append (list (pt x y z0) (pt x y z1))))))))))

(defun nav-box-centre (ents)
  (let ((lo nil) (hi nil))
    (dolist (e ents)
      (when (layer-visible-p (find-layer (ent-layer e)))
        (dolist (p (nav-entity-points e))
          (setf lo (if lo (pt (min (px lo) (px p)) (min (py lo) (py p)) (min (pz lo) (pz p))) p)
                hi (if hi (pt (max (px hi) (px p)) (max (py hi) (py p)) (max (pz hi) (pz p))) p)))))
    (and lo (v* (v+ lo hi) 0.5d0))))

(defun nav-screen-centre ()
  (values (+ *tx* (/ *cw* 2d0)) (+ *ty* (/ *ch* 2d0))))

(defun nav-screen-centre-point ()
  "The world point in the middle of the screen (on the work plane through the drawing's middle)."
  (multiple-value-bind (sx sy) (nav-screen-centre)
    (let ((mid (nav-box-centre (live-entities))))
      (if (and mid (projected-view-p)) (view-work-point sx sy mid) (s->w sx sy)))))

(defun nav-auto-target ()
  "The point the view turns around (ORBITAUTOTARGET)."
  (or (and (not (eql (sysvar "ORBITAUTOTARGET") 0))
           (or (nav-box-centre (remove-if-not #'ent-live-p *pickfirst*))
               (nav-box-centre (live-entities))))
      (nav-screen-centre-point)))

(defun nav-screen-of (p)
  (multiple-value-bind (x y) (w->s p) (values x y)))

(defun nav-keep (target sx sy)
  "Move the view so world point TARGET shows at screen pixel SX,SY."
  (multiple-value-bind (u v) (view-project target)
    (setf *vx* (- u (/ (- sx *tx* (/ *cw* 2d0)) *vscale*))
          *vy* (+ v (/ (- sy *ty* (/ *ch* 2d0)) *vscale*)))))

(defun nav-camera-distance ()
  (or *nav-distance* (* 1.2d0 (/ *cw* (max 1d-9 *vscale*)))))

;;; ---------- the views ----------

(defun current-frame-rud ()
  "Right, up and toward-viewer vectors of the view on screen (Top view when plan)."
  (if (projected-view-p)
      (values (pt3 (second *view*)) (pt3 (third *view*)) (pt3 (fourth *view*)))
      (values (pt 1 0 0) (pt 0 1 0) (pt 0 0 1))))

(defun frame-view (r u d)
  (list "Custom" (unit3 r) (unit3 u) (unit3 d) :orbit))

(defun nav-set-view (v)
  (setf *view* v)
  (clear-geom-cache))

(defun nav-redraw ()
  (unless *orbit-redrawing*
    (let ((*orbit-redrawing* t)) (gui-regen))))

;;; ---------- one drag, in each mode ----------

(defun nav-arcball-radius () (* 0.38d0 (min *cw* *ch*)))

(defun nav-free-zone (sx sy)
  "Where a free-orbit drag starts: :inside the circle, :roll outside it, or :horizontal / :vertical
on one of the small circles (left/right turn about the vertical, top/bottom about the horizontal)."
  (multiple-value-bind (cx cy) (nav-screen-centre)
    (let* ((rr (nav-arcball-radius)) (dx (- sx cx)) (dy (- sy cy)) (dd (sqrt (+ (* dx dx) (* dy dy)))))
      (cond ((or (< (sqrt (+ (expt (- sx (- cx rr)) 2) (expt dy 2))) 14)
                 (< (sqrt (+ (expt (- sx (+ cx rr)) 2) (expt dy 2))) 14)) :horizontal)
            ((or (< (sqrt (+ (expt dx 2) (expt (- sy (- cy rr)) 2))) 14)
                 (< (sqrt (+ (expt dx 2) (expt (- sy (+ cy rr)) 2))) 14)) :vertical)
            ((<= dd rr) :inside)
            (t :roll)))))

(defun nav-apply (mode st sx sy)
  "Set the view for MODE from the drag state ST (the view at the start) and the mouse at SX,SY."
  (let ((dx (- sx (getf st :sx))) (dy (- sy (getf st :sy))))
    (ecase mode
      ((:constrained :continuous)
       (nav-set-view (orbit-view (- (getf st :az) (* dx 0.01d0)) (+ (getf st :el) (* dy 0.01d0))))
       (multiple-value-call #'nav-keep (getf st :target) (values (getf st :tx) (getf st :ty))))
      (:free
       (destructuring-bind (r u d) (getf st :frame)
         (let ((zone (getf st :zone)))
           (flet ((turn (axis a) (setf r (rotate-about r axis a) u (rotate-about u axis a) d (rotate-about d axis a))))
             (case zone
               (:roll
                (multiple-value-bind (cx cy) (nav-screen-centre)
                  (let ((a0 (atan (- (getf st :sy) cy) (- (getf st :sx) cx)))
                        (a1 (atan (- sy cy) (- sx cx))))
                    (turn d (- a1 a0)))))
               (t
                (let ((ax (if (eq zone :vertical) 0 dx)) (ay (if (eq zone :horizontal) 0 dy)))
                  (turn (getf st :u0) (- (* ax 0.01d0)))
                  (turn (getf st :r0) (- (* ay 0.01d0)))))))
           (nav-set-view (frame-view r u d))
           (nav-keep (getf st :target) (getf st :tx) (getf st :ty)))))
      (:swivel
       ;; the camera (target + distance toward the viewer) stays; the view direction turns
       (let* ((dist (getf st :dist))
              (cam (v+ (getf st :target) (v* (getf st :d0) dist)))
              (v (orbit-view (- (getf st :az) (* dx 0.004d0)) (+ (getf st :el) (* dy 0.004d0))))
              (newt (v- cam (v* (fourth v) dist))))
         (nav-set-view v)
         (multiple-value-bind (cx cy) (nav-screen-centre) (nav-keep newt cx cy))))
      ((:distance :zoom)
       (let ((f (exp (- (* dy 0.006d0)))))
         (setf *vscale* (max 1d-9 (* (getf st :scale) f)))
         (when (eq mode :distance) (setf *nav-distance* (/ (getf st :dist) f)))
         (multiple-value-bind (cx cy) (nav-screen-centre) (nav-keep (getf st :centre) cx cy))))
      (:pan
       (setf *vx* (- (getf st :vx) (/ dx *vscale*)) *vy* (+ (getf st :vy) (/ dy *vscale*)))))))

(defun nav-start-state (mode sx sy)
  (multiple-value-bind (az el) (view-angles *view*)
    (multiple-value-bind (r u d) (current-frame-rud)
      (let ((target (if (member mode '(:constrained :continuous :free)) (nav-auto-target) (nav-screen-centre-point))))
        (multiple-value-bind (tx ty) (nav-screen-of target)
          (list :sx sx :sy sy :az az :el el :frame (list r u d) :r0 r :u0 u :d0 d
                :target target :tx tx :ty ty :zone (and (eq mode :free) (nav-free-zone sx sy))
                :dist (nav-camera-distance) :scale *vscale* :vx *vx* :vy *vy*
                :centre (nav-screen-centre-point)))))))

;;; ---------- what is drawn over the drawing ----------

(defun nav-target-cmds (p)
  "A small dark sphere at world point P."
  (when p
    (multiple-value-bind (x y) (w->s p)
      (list (format nil ".c create oval ~,1F ~,1F ~,1F ~,1F -fill #262b31 -outline #0b0d10 -width 1 -tags tmp" (- x 7) (- y 7) (+ x 7) (+ y 7))
            (format nil ".c create oval ~,1F ~,1F ~,1F ~,1F -fill #59626e -outline {} -tags tmp" (- x 4) (- y 5) (- x 0.5) (- y 1.5))))))

(defun nav-arcball-cmds ()
  (multiple-value-bind (cx cy) (nav-screen-centre)
    (let ((rr (nav-arcball-radius)))
      (cons (format nil ".c create oval ~,1F ~,1F ~,1F ~,1F -outline #3fd25f -width 2 -tags tmp" (- cx rr) (- cy rr) (+ cx rr) (+ cy rr))
            (loop for (ox oy) in (list (list (- rr) 0) (list rr 0) (list 0 (- rr)) (list 0 rr))
                  collect (format nil ".c create oval ~,1F ~,1F ~,1F ~,1F -outline #3fd25f -width 2 -fill {} -tags tmp"
                                  (+ cx ox -9) (+ cy oy -9) (+ cx ox 9) (+ cy oy 9)))))))

;;; ---------- the commands ----------

(defparameter *nav-modes*
  '((:constrained "Constrained orbit" "3DORBIT") (:free "Free orbit" "3DFORBIT") (:continuous "Continuous orbit" "3DCORBIT")
    (:distance "Adjust distance" "3DDISTANCE") (:swivel "Swivel" "3DSWIVEL") (:zoom "Zoom" "3DZOOM") (:pan "Pan" "3DPAN")))

(defun nav-hint (mode)
  (case mode
    (:constrained "Drag to orbit (left-right turns about the Z axis, up-down tilts).")
    (:free "Drag inside the green circle to turn any way, outside it to roll; the small circles turn about one axis.")
    (:continuous "Drag and let go: the view keeps turning. Click to stop.")
    (:distance "Drag up to come closer, down to move away.")
    (:swivel "Drag to turn the camera where it stands (the target moves).")
    (:zoom "Drag up to zoom in, down to zoom out.")
    (:pan "Drag to pan the view.")))

(defun nav-overlay-for (mode target)
  (lambda ()
    (append (when (eq mode :free) (nav-arcball-cmds))
            (when target (nav-target-cmds target)))))

(defun stop-spin ()
  (when *nav-spin* (setf *nav-spin* nil) (when (gui-p) (tk "catch {after cancel $::nav_tick}"))))

(defun nav-spin-tick ()
  "One step of the continuous orbit (called from the timer in the Tk side)."
  (when (and *nav-spin* (gui-p))
    (destructuring-bind (daz del target tx ty) *nav-spin*
      (multiple-value-bind (az el) (view-angles *view*)
        (let ((el2 (+ el del)) (lim (- (/ pi 2) 0.02d0)))
          ;; bounce off straight up / straight down
          (when (> (abs el2) lim) (setf (second *nav-spin*) (- del)) (setf el2 (max (- lim) (min lim el2))))
          (nav-set-view (orbit-view (+ az daz) el2))
          (nav-keep target tx ty)
          (nav-redraw)
          (draw-temp)
          (tk "set ::nav_tick [after 30 {emit NAVTICK}]"))))))

(defun nav-run (mode)
  "The 3D navigation loop, starting in MODE."
  (unless (gui-p)
    (out (format nil "3D navigation needs the graphical interface.~%"))
    (return-from nav-run nil))
  (unwind-protect
       (loop
         (let* ((entry (assoc mode *nav-modes*))
                (kws '("Constrained" "Free" "coNtinuous" "Distance" "Swivel" "Zoom" "Pan" "eXit")))
           (setf *nav-overlay* (nav-overlay-for mode nil))
           (draw-temp)
           (out (format nil "~A: ~A~%" (second entry) (nav-hint mode)))
           (let ((p (get-user-input :point :nosnap t :keywords kws
                                    :prompt "Press and drag, or [Constrained/Free/coNtinuous/Distance/Swivel/Zoom/Pan/eXit] <eXit>: ")))
             (stop-spin)
             (cond
               ((null p) (return))
               ((stringp p)
                (let ((k (string-downcase p)))
                  (cond ((string= k "exit") (return))
                        (t (setf mode (cdr (assoc k '(("constrained" . :constrained) ("free" . :free) ("continuous" . :continuous)
                                                       ("distance" . :distance) ("swivel" . :swivel) ("zoom" . :zoom) ("pan" . :pan))
                                                  :test #'string=)))))))
               ((pointp p)
                (let* ((st (nav-start-state mode *mouse-sx* *mouse-sy*))
                       (samples nil)
                       (show (and (member mode '(:constrained :continuous :free)) (getf st :target))))
                  (setf *nav-overlay* (nav-overlay-for mode show))
                  (let ((*release-ends-drag* t))
                   (get-user-input :corner :base p :nosnap t :prompt "Release to stop: "
                                  :drag (lambda (cur)
                                          (declare (ignore cur))
                                          (push (list (get-internal-real-time) *mouse-sx* *mouse-sy*) samples)
                                          (nav-apply mode st *mouse-sx* *mouse-sy*)
                                          (nav-redraw)
                                          nil)))
                  ;; continuous: keep turning at the speed of the end of the drag
                  (when (and (eq mode :continuous) samples)
                    (let* ((now (first samples))
                           (old (or (find-if (lambda (s) (> (- (first now) (first s)) (* 0.12 internal-time-units-per-second))) samples)
                                    (car (last samples))))
                           (ms (max 1 (/ (* 1000 (- (first now) (first old))) internal-time-units-per-second)))
                           (vx (/ (- (second now) (second old)) ms)) (vy (/ (- (third now) (third old)) ms)))
                      (when (> (+ (abs vx) (abs vy)) 0.02)
                        (setf *nav-spin* (list (- (* vx 30 0.01d0)) (* vy 30 0.01d0)
                                               (getf st :target) (getf st :tx) (getf st :ty)))
                        (setf *nav-overlay* (nav-overlay-for mode (getf st :target)))
                        (tk "set ::nav_tick [after 30 {emit NAVTICK}]"))))
                  (unless *nav-spin* (setf *nav-overlay* (nav-overlay-for mode nil)))
                  (clear-geom-cache)
                  (gui-regen) (gui-sync)))))))
    (stop-spin)
    (setf *nav-overlay* nil)
    (when (gui-p) (clear-geom-cache) (gui-regen) (refresh-grips) (gui-sync)))
  (out (format nil "View: ~A.~%" (if *view* (first *view*) "Top"))))

(defcmd "3DORBIT" ("3DO" "ORBIT") "Turns the view around a target point (constrained orbit: up stays up)."
  (nav-run :constrained))
(defcmd "3DFORBIT" () "Free orbit: turns the view in any direction, without keeping up up."
  (nav-run :free))
(defcmd "3DCORBIT" () "Continuous orbit: drag and let go, and the view keeps turning."
  (nav-run :continuous))
(defcmd "3DDISTANCE" () "Moves the camera closer to the objects or farther away (drag up and down)."
  (nav-run :distance))
(defcmd "3DSWIVEL" () "Turns the camera where it stands, like turning your head: the target moves."
  (nav-run :swivel))
(defcmd "3DZOOM" () "Zooms in a 3D view (drag up to zoom in)." (nav-run :zoom))
(defcmd "3DPAN" () "Pans a 3D view (drag)." (nav-run :pan))

;;; ---------- Shift + middle-button orbit, at any time ----------

(defun nav-mouse-event (ty ev)
  "OP x y free / OD x y / OQ: orbit with Shift (+ Ctrl: free) and the middle button."
  (let ((args (split-string (event-rest ev) #\Space)))
    (cond
      ((string= ty "OP")
       (let* ((x (parse-integer (first args))) (y (parse-integer (second args)))
              (mode (if (equal (third args) "1") :free :constrained))
              (st (nav-start-state mode x y)))
         (setf (getf st :zone) (if (eq mode :free) :inside nil))
         (setf *nav-mouse* (list mode st))
         (setf *nav-overlay* (nav-overlay-for mode (getf st :target)))
         (draw-temp)))
      ((string= ty "OD")
       (when *nav-mouse*
         (let ((x (parse-integer (first args))) (y (parse-integer (second args))))
           (setf *mouse-sx* x *mouse-sy* y)
           (destructuring-bind (mode st) *nav-mouse*
             (nav-apply mode st x y))
           (nav-redraw))))
      ((string= ty "OQ")
       (when *nav-mouse*
         (setf *nav-mouse* nil *nav-overlay* nil)
         (clear-geom-cache)
         (gui-regen) (refresh-grips) (gui-sync))))
    t))
