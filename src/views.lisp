;;;; views.lisp — the standard views (Top, Bottom, Left, Right, Front, Back and the four
;;;; isometric views, as on AutoCAD's View > 3D Views menu) and isometric drafting (ISODRAFT,
;;;; ISOPLANE, F5).
;;;;
;;;; Drawings are 2D with optional heights: objects can sit at a Z elevation and have a THICKNESS
;;;; (they then show as walls in the 3D views). Objects can be picked, window-selected, snapped to
;;;; and edited in every view (see "picking ... in the 3D / side views" at the end of this file).

(in-package :fancy-pencil)


(defparameter *standard-views*
  ;; name  direction toward the viewer  (right up), or NIL to work them out
  '(("Top" (0 0 1) ((1 0 0) (0 1 0)))
    ("Bottom" (0 0 -1) ((1 0 0) (0 -1 0)))
    ("Front" (0 -1 0) ((1 0 0) (0 0 1)))
    ("Back" (0 1 0) ((-1 0 0) (0 0 1)))
    ("Right" (1 0 0) ((0 1 0) (0 0 1)))
    ("Left" (-1 0 0) ((0 -1 0) (0 0 1)))
    ("SW Isometric" (-1 -1 1) nil)
    ("SE Isometric" (1 -1 1) nil)
    ("NE Isometric" (1 1 1) nil)
    ("NW Isometric" (-1 1 1) nil)))

(defun make-view (name)
  (let ((spec (assoc name *standard-views* :test #'string-equal)))
    (when spec
      (destructuring-bind (nm dir axes) spec
        (if (string-equal nm "Top")
            nil
            (let* ((d (unit3 (pt3 dir)))
                   (r (if axes (pt3 (first axes)) (unit3 (cross3 (pt 0 0 1) d))))
                   (u (if axes (pt3 (second axes)) (unit3 (cross3 d r)))))
              (list nm r u d)))))))

(defun edge-on-view-p () (and *view* (< (abs (pz (fourth *view*))) 1d-9)))

(defun set-view (name)
  (let ((spec (assoc name *standard-views* :test #'string-equal)))
    (if (null spec)
        (out (format nil "Unknown view ~A.~%" name))
        (progn
          (setf *view* (make-view (first spec)))
          (setf *view-history* nil)
          (clear-geom-cache)
          (when (gui-p) (zoom-extents) (refresh-grips) (gui-sync))
          (out (format nil "~A view.~A~%" (first spec)
                       (cond ((null *view*) "")
                             ((ucs)
                              (format nil "~%You are drawing in this view's plane (X right, Y up, Z toward you): every command works~%~
                                           here. Objects in other planes show edge-on; move, copy, rotate or mirror them in 3D."))
                             (t "  You can draw and edit on the ground plane here."))))))))

(defparameter *view-keywords*
  '(("Top" "Top") ("Bottom" "Bottom") ("Left" "Left") ("Right" "Right") ("Front" "Front") ("BAck" "Back")
    ("SWiso" "SW Isometric") ("SEiso" "SE Isometric") ("NEiso" "NE Isometric") ("NWiso" "NW Isometric")))

(defcmd "-VIEW" ("VIEW" "V" "VIEWS") "Changes to a standard view: Top, Bottom, Left, Right, Front, Back or an isometric view."
  (let ((k (gkw (format nil "Enter a view [Top/Bottom/Left/Right/Front/BAck/SWiso/SEiso/NEiso/NWiso] <~A>: "
                        (if *view* (first *view*) "Top"))
                (mapcar #'first *view-keywords*))))
    (when k (set-view (second (assoc k *view-keywords* :test #'string-equal))))))

(defcmd "PLAN" () "Back to the plan (Top) view." (set-view "Top"))

;;; ---------- heights: THICKNESS and ELEVATION for new objects ----------

(defun apply-new-object-height (data)
  "New objects take THICKNESS (extrusion) and ELEVATION (height above the ground plane)."
  (let ((th (sysvar "THICKNESS")) (el (sysvar "ELEVATION")) (type (dxf 0 data)))
    (when (and (realp th) (/= th 0) (member type '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POINT") :test #'equal)
               (not (assoc 39 data)))
      (setf data (append data (list (cons 39 (d th))))))
    (when (and (realp el) (/= el 0) (member type '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POINT" "TEXT" "MTEXT" "INSERT") :test #'equal))
      (if (equal type "LWPOLYLINE")
          (unless (assoc 38 data) (setf data (append data (list (cons 38 (d el))))))
          (setf data (map-points data '(10 11) (lambda (p) (if (zerop (pz p)) (pt (px p) (py p) el) p))))))
    data))

(defcmd "THICKNESS" ("TH") "Sets the thickness (extrusion height) for new lines, arcs, circles and polylines."
  (let ((v (gdist (format nil "Enter new value for THICKNESS <~A>: " (fmt-len (or (sysvar "THICKNESS") 0d0))))))
    (when (realp v) (setf (sysvar "THICKNESS") (d v)))))

(defcmd "ELEV" ("ELEVATION") "Sets the elevation and thickness for new objects."
  (let ((e (gdist (format nil "Specify new default elevation <~A>: " (fmt-len (or (sysvar "ELEVATION") 0d0))))))
    (when (realp e) (setf (sysvar "ELEVATION") (d e))))
  (let ((v (gdist (format nil "Specify new default thickness <~A>: " (fmt-len (or (sysvar "THICKNESS") 0d0))))))
    (when (realp v) (setf (sysvar "THICKNESS") (d v)))))

;;; ---------- isometric drafting ----------

(defun set-iso (on &optional plane)
  (setf (sysvar "SNAPSTYL") (if on 1 0))
  (when plane (setf (sysvar "SNAPISOPAIR") plane))
  (when (gui-p) (gui-regen) (gui-sync)))

(defcmd "ISODRAFT" () "Turns isometric drafting on (choosing an isoplane) or off."
  (let ((k (gkw (format nil "Enter option [Orthographic/isoplaneLeft/isoplaneTop/isoplaneRight] <~A>: "
                        (if (iso-mode-p) "Orthographic" (format nil "isoplane~A" (isoplane-name))))
                '("Orthographic" "isoplaneLeft" "isoplaneTop" "isoplaneRight")
                (if (iso-mode-p) "Orthographic" (format nil "isoplane~A" (isoplane-name))))))
    (cond ((string-equal k "Orthographic") (set-iso nil) (out (format nil "Isometric drafting off.~%")))
          (t (set-iso t (cond ((string-equal k "isoplaneLeft") 0) ((string-equal k "isoplaneRight") 2) (t 1)))
             (out (format nil "Isometric drafting on: isoplane ~A. F5 switches isoplanes; ELLIPSE > Isocircle draws circles.~%"
                          (isoplane-name)))))))

(defun cycle-isoplane ()
  ;; AutoCAD's order: Left -> Top -> Right -> Left
  (setf (sysvar "SNAPISOPAIR") (mod (1+ (isoplane)) 3))
  (out (format nil "<Isoplane ~A>~%" (isoplane-name)))
  (when (gui-p) (draw-temp) (gui-sync)))

(defcmd "ISOPLANE" () "Chooses the isoplane for isometric drafting (Left, Top or Right)."
  (let ((k (gkw (format nil "Enter isometric plane setting [Left/Top/Right] <~A>: "
                        (isoplane-name (mod (1+ (isoplane)) 3)))
                '("Left" "Top" "Right"))))
    (if (null k) (cycle-isoplane)
        (progn (setf (sysvar "SNAPISOPAIR") (cond ((string-equal k "Left") 0) ((string-equal k "Right") 2) (t 1)))
               (out (format nil "Current isoplane: ~A~%" (isoplane-name)))
               (when (gui-p) (draw-temp) (gui-sync))))))

;;; ---------- the view picker (a small panel of cube pictures, top right of the drawing) ----------

(defparameter *cube-faces*
  ;; normal, corner indices (cube corners are (+-1 +-1 +-1)), color
  '(((0 0 1) ((-1 -1 1) (1 -1 1) (1 1 1) (-1 1 1)) "#7fb2e5")       ; top: blue, like AutoCAD's
    ((0 0 -1) ((-1 -1 -1) (-1 1 -1) (1 1 -1) (1 -1 -1)) "#56606c")
    ((0 -1 0) ((-1 -1 -1) (1 -1 -1) (1 -1 1) (-1 -1 1)) "#d9dde3")   ; front
    ((0 1 0) ((1 1 -1) (-1 1 -1) (-1 1 1) (1 1 1)) "#9aa3ad")
    ((1 0 0) ((1 -1 -1) (1 1 -1) (1 1 1) (1 -1 1)) "#b8c0c9")       ; right
    ((-1 0 0) ((-1 1 -1) (-1 -1 -1) (-1 -1 1) (-1 1 1)) "#8a939e")))

(defun view-cube-picture (name)
  "Polygons (color x1 y1 ...) of a cube seen in view NAME, fitted to about 22 pixels, y down."
  (let* ((v (make-view name))
         (r (if v (second v) (pt 1 0 0))) (u (if v (third v) (pt 0 1 0))) (dir (if v (fourth v) (pt 0 0 1)))
         (scale (if (and v (> (abs (pz dir)) 1d-9) (> (+ (abs (px dir)) (abs (py dir))) 1d-9)) 6.4d0 9.5d0)))
    (loop for (n corners col) in *cube-faces*
          when (> (vdot3 (pt3 n) dir) 1d-6)
            collect (cons col (loop for c in corners
                                    for p = (pt3 c)
                                    append (list (round (* scale (vdot3 p r))) (round (- (* scale (vdot3 p u))))))))))

(defun send-view-picker ()
  (when (gui-p)
    (tk "viewcube_set {~{~A~^ ~}}"
        (loop for (kw name) in *view-keywords*
              collect (format nil "{~A ~A {~{~A~^ ~}}}" (tq kw) (tq name)
                              (mapcar (lambda (poly) (format nil "{~{~A~^ ~}}" poly)) (view-cube-picture name)))))))

;;; ---------- Point Style (DDPTYPE) ----------

(defparameter *point-modes* '(0 1 2 3 4 32 33 34 35 36 64 65 66 67 68 96 97 98 99 100))

(defun point-style-dialog ()
  (let* ((ps (d (or (sysvar "PDSIZE") 0)))
         (rel (<= ps 0))
         (size (cond ((zerop ps) 5d0) ((minusp ps) (- ps)) (t ps)))
         (r (tk-ask (format nil "ptypedlg ~D ~A ~D" (or (sysvar "PDMODE") 0) (tq (fmt-plain size)) (if rel 1 0)))))
    (when (and (> (length r) 3) (string= (subseq r 0 3) "OK "))
      (destructuring-bind (m sz rl) (split-string (subseq r 3) #\Space)
        (let ((mode (ignore-errors (parse-integer m))) (n (parse-number-text sz)))
          (when (and mode (member mode *point-modes*))
            (setf (sysvar "PDMODE") mode))
          (when (and n (plusp n))
            (setf (sysvar "PDSIZE") (if (string= rl "1") (- (d n)) (d n))))
          (touch)
          (gui-regen)
          (out (format nil "Point style: PDMODE=~D  PDSIZE=~A~%" (sysvar "PDMODE") (fmt-plain (sysvar "PDSIZE")))))))))

(defcmd "DDPTYPE" ("PTYPE" "POINTSTYLE") "Point Style dialog: how points are shown and how big."
  (if (and (gui-p) (not (from-lisp-p)))
      (point-style-dialog)
      (progn
        (let ((m (gint (format nil "Enter new value for PDMODE (0,1,2,3,4 plus 32 circle, 64 square) <~D>: " (sysvar "PDMODE")))))
          (when (and m (member m *point-modes*)) (setf (sysvar "PDMODE") m)))
        (let ((s (greal (format nil "Enter new value for PDSIZE (0 = 5% of screen, negative = % of screen) <~A>: "
                                (fmt-plain (sysvar "PDSIZE"))))))
          (when (realp s) (setf (sysvar "PDSIZE") (d s)))))))

;;; ---------- picking, snapping and points in the 3D / side views ----------
;;; In plan view objects are found by their X,Y. In any other view (Front, Right, the isometric
;;; views ...) everything is measured on the screen instead: an object is picked, window-selected
;;; or snapped to where it is drawn — at its height, with its thickness — and points you click
;;; land on a work plane through the base point (the grip being dragged, the MOVE base point ...),
;;; so dragging in a side view changes heights, and dragging in the plan-like views keeps them.

(defun view-object-height (data)
  "(values elevation thickness) the 3D views draw DATA with."
  ;; an object in another plane has its height and thickness in its pieces already
  (when (offplane-p data) (return-from view-object-height (values 0d0 0d0)))
  (values (if (equal (dxf 0 data) "LWPOLYLINE") (d (or (dxf 38 data) 0)) 0d0)
          (if (member (dxf 0 data) '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POINT") :test #'equal)
              (d (or (dxf 39 data) 0)) 0d0)))

(defun lift (p z) (pt (px p) (py p) z))

(defun piece-polyline-3d (pc)
  "World points along piece PC (arcs and circles at the height of their centre)."
  (case (first pc)
    ((:seg :gseg) (list (pt3 (second pc)) (pt3 (third pc))))
    (:circle (arc-points-3d (pt3 (second pc)) (third pc) 0d0 +2pi+ 48))
    (:arc (destructuring-bind (c r a1 a2) (rest pc) (arc-points-3d (pt3 c) r a1 a2 24)))
    (:pt (list (pt3 (second pc)) (pt3 (second pc))))
    (t (loop for sp in (tessellate-piece pc) when (eq (first sp) :seg) append (list (pt3 (second sp)) (pt3 (third sp)))))))

(defun entity-view-segs (e)
  "E as it is drawn in the current view, as 2D segments in view coordinates."
  (multiple-value-bind (elev th) (view-object-height (ent-data e))
    (let ((out nil))
      (flet ((uv (p dz) (w->uv (pt (px p) (py p) (+ (pz p) elev dz)))))
        (dolist (pc (pick-pieces e) out)
          (let ((pts (piece-polyline-3d pc)))
            (loop for (a b) on pts while b
                  do (push (list :seg (uv a 0) (uv b 0)) out)
                     (unless (zerop th) (push (list :seg (uv a th) (uv b th)) out)))
            (unless (zerop th)
              (dolist (p (list (first pts) (car (last pts))))
                (push (list :seg (uv p 0) (uv p th)) out))))))
      out)))

(defun entity-view-bbox (e)
  "E's extent in view coordinates (u0 v0 u1 v1), from its plan extent and height."
  (let ((b (ignore-errors (entity-bbox e))))
    (when b
      (multiple-value-bind (elev th) (view-object-height (ent-data e))
        (let* ((zs (if (offplane-p (ent-data e))
                       nil
                       (let ((z0 (+ elev (let ((p (dxf 10 (ent-data e)))) (if (and (consp p) (realp (pz p))) (pz p) 0d0)))))
                         (list z0 (+ z0 th)))))
               (u0 nil) (v0 nil) (u1 nil) (v1 nil))
          ;; objects with points at several heights (3D lines): use their real points too
          (dolist (pc (entity-pieces e))
            (dolist (p (case (first pc) ((:seg :gseg) (list (second pc) (third pc))) (t nil)))
              (push (+ elev (pz (pt3 p))) zs)))
          ;; a solid: its lowest and highest corners (its curved sides may have no drawn edges)
          (let ((vs (and (equal (ent-type e) "POLYFACE") (mesh-verts (ent-data e)))))
            (when vs
              (push (reduce #'min vs :key #'pz) zs) (push (reduce #'max vs :key #'pz) zs)))
          (dolist (x (list (first b) (third b)))
            (dolist (y (list (second b) (fourth b)))
              (dolist (z zs)
                (multiple-value-bind (u v) (view-project (pt x y z))
                  (setf u0 (if u0 (min u0 u) u) u1 (if u1 (max u1 u) u)
                        v0 (if v0 (min v0 v) v) v1 (if v1 (max v1 v) v))))))
          (list u0 v0 u1 v1))))))

(defun view-candidates (q tol)
  "Visible objects whose view extent is within TOL of view point Q."
  (loop for e in (live-entities)
        when (and (layer-visible-p (find-layer (ent-layer e)))
                  (let ((b (entity-view-bbox e)))
                    (and b (<= (- (first b) tol) (px q) (+ (third b) tol))
                         (<= (- (second b) tol) (py q) (+ (fourth b) tol)))))
          collect e))

(defun view-pick-entity-at (p tol)
  (let ((q (w->uv p)) (best nil) (bd nil))
    (dolist (e (view-candidates q tol) best)
      (dolist (sg (entity-view-segs e))
        (let ((dd (piece-dist sg q)))
          (when (<= dd tol)
            ;; a solid has edges everywhere: a line, circle or text under the same spot wins
            (when (equal (ent-type e) "POLYFACE") (incf dd (* 2 tol)))
            (when (or (null bd) (< dd bd)) (setf best e bd dd))))))))

(defun view-entities-in-window (p1 p2 crossing)
  (let* ((a (w->uv p1)) (b (w->uv p2))
         (x0 (min (px a) (px b))) (x1 (max (px a) (px b)))
         (y0 (min (py a) (py b))) (y1 (max (py a) (py b))))
    (loop for e in (live-entities)
          when (and (layer-visible-p (find-layer (ent-layer e)))
                    (let ((bb (entity-view-bbox e)))
                      (and bb
                           (or (and (<= x0 (first bb)) (<= (third bb) x1) (<= y0 (second bb)) (<= (fourth bb) y1))
                               (and crossing
                                    (not (or (> (first bb) x1) (< (third bb) x0) (> (second bb) y1) (< (fourth bb) y0)))
                                    (some (lambda (sg) (piece-crosses-rect-p sg x0 y0 x1 y1)) (entity-view-segs e)))))))
            collect e)))

(defun view-dist (a b)
  "Distance on the screen (in drawing units) between world points A and B."
  (if (projected-view-p) (dist2d (w->uv a) (w->uv b)) (dist2d a b)))

(defun view-osnap-candidates (p bits tol)
  "Object snap candidates near P measured on the screen: endpoints, midpoints, centres, nodes,
quadrants and nearest, at the objects' real heights (tops of thick objects too)."
  (let ((q (w->uv p)) (cands nil))
    (flet ((add (w bit) (when (and w (<= (dist2d (w->uv w) q) tol)) (push (cons w bit) cands))))
      (dolist (e (view-candidates q tol) cands)
        ;; solids and circles in other planes: centres and quadrants of their round shapes
        (when (and (fboundp 'round-snap-candidates) (member (ent-type e) '("POLYFACE" "CIRCLE" "ARC") :test #'equal))
          (setf cands (append (funcall 'round-snap-candidates e p bits tol #'w->uv) cands)))
        (when (some (lambda (sg) (<= (piece-dist sg q) tol)) (entity-view-segs e))
          (multiple-value-bind (elev th) (view-object-height (ent-data e))
            (dolist (pc (snap-pieces e))
              (let* ((z0 (+ elev (pz (pt3 (if (member (first pc) '(:seg :pt :circle :arc)) (second pc) (pt 0 0)))))))
                (dolist (dz (if (zerop th) '(0d0) (list 0d0 th)))
                  (flet ((up (w z) (lift w (+ z dz))))
                    (case (first pc)
                      (:seg (let ((a (pt3 (second pc))) (b (pt3 (third pc))))
                              (when (logtest bits 1) (add (up a (+ elev (pz a))) 1) (add (up b (+ elev (pz b))) 1))
                              (when (logtest bits 2) (add (up (midpt a b) (+ elev (/ (+ (pz a) (pz b)) 2))) 2))
                              (when (logtest bits 512)
                                (let* ((ua (w->uv (up a (+ elev (pz a))))) (ub (w->uv (up b (+ elev (pz b)))))
                                       (k (max 0d0 (min 1d0 (seg-param q ua ub)))))
                                  (add (v+ (up a (+ elev (pz a))) (v* (v- (up b (+ elev (pz b))) (up a (+ elev (pz a)))) k)) 512)))))
                      (:pt (when (logtest bits 8) (add (up (second pc) z0) 8)))
                      ((:arc :circle)
                       (when (logtest bits 1) (dolist (w (piece-ends pc)) (add (up w z0) 1)))
                       (when (logtest bits 2) (dolist (w (piece-mids pc)) (add (up w z0) 2)))
                       (when (logtest bits 4) (push (cons (up (second pc) z0) 4) cands))
                       (when (logtest bits 16) (dolist (w (piece-quads pc)) (add (up w z0) 16)))))))))))))))

(defun view-work-point (sx sy base)
  "The world point under screen pixel SX,SY in a 3D view, on the work plane through BASE: the
plane facing you in Front/Back/Left/Right (so dragging up and down changes heights), else the
level plane at BASE's height."
  (let* ((q (s->uv sx sy)) (u (px q)) (v (py q))
         (r (second *view*)) (up (third *view*)) (dir (fourth *view*))
         (b (v+ (v* r u) (v* up v)))
         (base (and base (pt3 base))))
    (cond ((null base) (view-unproject u v))
          ((> (abs (pz dir)) 1d-9) (v+ b (v* dir (/ (- (pz base) (pz b)) (pz dir)))))
          (t (v+ b (v* dir (vdot3 base dir)))))))

(defun view-ortho (p base)
  "Ortho in a 3D view: along the X or Y axis of the work plane through BASE. In an isometric
(tilted) view that plane is level, so lines stay at BASE's height; in Front/Back/Left/Right it is
the plane facing you, whose axes are the screen's horizontal and vertical."
  (let* ((dv (v- (pt3 p) (pt3 base))) (r (second *view*)) (up (third *view*)) (dir (fourth *view*)))
    (if (and (> (abs (pz dir)) 1d-9) (not (eq (fifth *view*) :ucs)))
        (if (> (abs (px dv)) (abs (py dv)))
            (v+ (pt3 base) (pt (px dv) 0 0))
            (v+ (pt3 base) (pt 0 (py dv) 0)))
        (let ((du (vdot3 dv r)) (dvv (vdot3 dv up)))
          (if (> (abs du) (abs dvv)) (v+ (pt3 base) (v* r du)) (v+ (pt3 base) (v* up dvv)))))))

(defun view-grid-snap (p ux uy)
  "Grid snap in a 3D view: in a tilted view the grid lies on the level plane at P's height, so X
and Y round to it and the height stays; in Front/Back/Left/Right round the screen coordinates."
  (let ((dir (fourth *view*)) (p (pt3 p)))
    (if (and (> (abs (pz dir)) 1d-9) (not (eq (fifth *view*) :ucs)))
        (pt (* ux (fround (/ (px p) ux))) (* uy (fround (/ (py p) uy))) (pz p))
        (multiple-value-bind (u v) (view-project p)
          (let ((du (- (* ux (fround (/ u ux))) u)) (dv (- (* uy (fround (/ v uy))) v)))
            (v+ p (v+ (v* (second *view*) du) (v* (third *view*) dv))))))))

;;; ---------- UCS: any plane as the working plane ----------
;;; UCS sets up a working plane anywhere - on a slanted panel, through three points, on an object -
;;; and turns the view to look straight at it, so everything draws and edits in that plane as in
;;; the Front or Top view. UCS World (or PLAN, or a standard view) goes back to the world planes.

(defvar *ucs-history* nil "Earlier working planes, for UCS Previous.")

(defun current-frame ()
  "(x y z origin) of the working plane in use, in world coordinates (world: the Top view's)."
  (or (ucs) (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1) (pt 0 0 0))))

(defun set-ucs-frame (x y z o &optional (name "UCS"))
  "Make the plane through O with axes X, Y (Z toward you) the working plane, and look at it."
  (push *view* *ucs-history*)
  (when (> (length *ucs-history*) 10) (setf *ucs-history* (subseq *ucs-history* 0 10)))
  (setf *view* (if (and (< (dist (pt3 x) (pt 1 0 0)) 1d-9) (< (dist (pt3 y) (pt 0 1 0)) 1d-9)
                        (< (dist (pt3 o) (pt 0 0 0)) 1d-9))
                   nil
                   (list name (unit3 x) (unit3 y) (unit3 z) :ucs (pt3 o))))
  (after-view-change)
  (out (format nil "~:[World coordinates (Top view).~;Working plane set: origin ~A, Z axis ~A. Everything you draw or edit lies in this plane.~]~%"
               *view*
               (and *view* (format nil "~{~A~^,~}" (mapcar #'fmt-num (pt3 o))))
               (and *view* (format nil "~{~A~^,~}" (mapcar #'fmt-num (unit3 z)))))))

(defun after-view-change ()
  (setf *view-history* nil)
  (clear-geom-cache)
  (when (gui-p) (zoom-extents) (refresh-grips) (gui-sync)))

(defun frame-from-x-and-z (o x z)
  "A right-handed frame at O with X along X (made square to Z) and Z along Z."
  (let* ((z (unit3 z))
         (x (v- x (v* z (vdot3 x z))))
         (x (if (< (vdot3 x x) 1d-18) (any-perpendicular z) (unit3 x))))
    (list x (unit3 (cross3 z x)) z o)))

(defun object-frame (e)
  "The working plane of object E: its own plane, origin at its first point, X along its first edge."
  (let* ((w (let ((*ucs-off* t)) (ent-data e)))
         (type (dxf 0 w)))
    (cond
      ((equal type "LINE")
       (let* ((a (pt3 (dxf 10 w))) (b (pt3 (dxf 11 w)))
              (zv (third (current-frame))))
         (frame-from-x-and-z a (v- b a) (let ((d (unit3 (v- b a))))
                                         (if (> (abs (vdot3 d zv)) 0.999d0) (any-perpendicular d) zv)))))
      ((wcs-type-p type) (let ((f (current-frame))) (list (first f) (second f) (third f) (pt3 (dxf 10 w)))))
      (t
       (multiple-value-bind (ax ay az) (ocs-axes (entity-normal w))
         (flet ((wp (p) (ocs->w (pt3 p) ax ay az)))
           (cond
             ((equal type "LWPOLYLINE")
              (let* ((el (d (or (dxf 38 w) 0)))
                     (vs (mapcar (lambda (v) (wp (pt (px (first v)) (py (first v)) el))) (lwpoly-vertices w))))
                (frame-from-x-and-z (first vs) (if (second vs) (v- (second vs) (first vs)) ax) az)))
             (t
              (let* ((o (wp (or (dxf 10 w) (pt 0 0))))
                     (a (if (member type '("TEXT" "MTEXT" "INSERT" "ATTDEF" "ATTRIB") :test #'equal) (d (or (dxf 50 w) 0)) 0d0)))
                (frame-from-x-and-z o (v+ (v* ax (cos a)) (v* ay (sin a))) az))))))))))

(defun rotate-about (v k a)
  "Vector V turned by angle A about unit axis K (right-hand rule)."
  (let ((c (cos a)) (s (sin a)))
    (v+ (v+ (v* v c) (v* (cross3 k v) s)) (v* k (* (vdot3 k v) (- 1 c))))))

(defcmd "UCS" ("WORKPLANE" "WP") "Sets the working plane: through three points, on an object's plane, at an origin, turned about an axis, facing the view, or back to World."
  (let ((r (gpoint "Specify origin of UCS or [3point/OBject/Face/View/X/Y/Z/Previous/World] <World>: "
                   :keywords '("3point" "OBject" "Face" "View" "X" "Y" "Z" "Previous" "World"))))
    (cond
      ((or (null r) (equal r "World"))
       (push *view* *ucs-history*) (setf *view* nil) (after-view-change)
       (out (format nil "World coordinates (Top view).~%")))
      ((pointp r)
       ;; a new origin: same directions
       (destructuring-bind (x y z o) (current-frame)
         (declare (ignore o))
         (set-ucs-frame x y z (ucs-point-to-world r))))
      ((equal r "3point")
       (let* ((o (gpoint "Specify new origin point <0,0,0>: "))
              (o (if (pointp o) o (pt 0 0 0)))
              (px (gpoint "Specify point on positive portion of X-axis: " :base o))
              (py (and (pointp px) (gpoint "Specify point on positive-Y portion of the UCS XY plane: " :base o))))
         (when (pointp py)
           (let* ((wo (ucs-point-to-world o)) (wx (v- (ucs-point-to-world px) wo)) (wy (v- (ucs-point-to-world py) wo))
                  (z (cross3 wx wy)))
             (if (< (vdot3 z z) 1d-18)
                 (out (format nil "The three points are in a line: no plane.~%"))
                 (destructuring-bind (x y z o) (frame-from-x-and-z wo wx z)
                   (set-ucs-frame x y z o)))))))
      ((member r '("OBject" "Face") :test #'equal)
       (let* ((sel (get-user-input :entity :prompt "Select object or a face of a solid (its plane becomes the working plane): "))
              (e (sel-entity sel)))
         (cond ((and e (equal (ent-type e) "POLYFACE"))
                ;; a solid: the flat face under the cursor
                (multiple-value-bind (pts n) (face-loop e (sel-point sel))
                  (if (null pts) (out (format nil "No face there.~%"))
                      (let* ((wp (mapcar #'ucs-point-to-world pts))
                             (wn (ucs-vector-to-world n)))
                        (destructuring-bind (x y z o) (frame-from-x-and-z (first wp) (v- (second wp) (first wp)) wn)
                          (set-ucs-frame x y z o))))))
               (e (destructuring-bind (x y z o) (object-frame e) (set-ucs-frame x y z o))))))
      ((equal r "View")
       (destructuring-bind (x y z o) (current-frame)
         (declare (ignore x y z))
         (if *view*
             (set-ucs-frame (second *view*) (third *view*) (unit3 (cross3 (second *view*) (third *view*))) o)
             (set-ucs-frame (pt 1 0 0) (pt 0 1 0) (pt 0 0 1) o))))
      ((member r '("X" "Y" "Z") :test #'equal)
       (let ((a (gangle (format nil "Specify rotation angle about ~A axis <90>: " r))))
         (destructuring-bind (x y z o) (current-frame)
           (let* ((a (or a (/ pi 2)))
                  (k (cond ((equal r "X") x) ((equal r "Y") y) (t z))))
             (set-ucs-frame (rotate-about x k a) (rotate-about y k a) (rotate-about z k a) o)))))
      ((equal r "Previous")
       (if (null *ucs-history*)
           (out (format nil "No previous working plane.~%"))
           (progn (setf *view* (pop *ucs-history*)) (after-view-change)
                  (out (format nil "~A.~%" (if *view* (first *view*) "World coordinates (Top view)")))))))))
