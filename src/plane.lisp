;;;; plane.lisp — objects in any plane, and the working plane of the side views.
;;;;
;;;; Every object except LINE and POINT lies in a plane given by its extrusion direction (DXF
;;;; group 210, straight up when absent) and keeps its coordinates in that plane's own system
;;;; (the OCS, by AutoCAD's "arbitrary axis" rule). LINE and POINT are plain 3D points.
;;;;
;;;; The views that face a side of the model (Front, Back, Left, Right, Bottom) work like an
;;;; AutoCAD UCS: while one is showing, the drawing is presented to every command in that view's
;;;; coordinates — X to the right, Y up, Z toward you — so drawing, snapping, trimming, offsetting,
;;;; filleting ... work there exactly as in Top view, and new objects are made in the view's plane.
;;;; Objects lying in other planes show edge-on (or slanted); they can be selected, snapped to,
;;;; erased and moved, copied, rotated, mirrored and scaled in 3D (rotating one is how a flat panel
;;;; is stood up), but not trimmed or offset until a view faces them.

(in-package :fancy-pencil)

(defun unit3 (v) (let ((l (sqrt (vdot3 v v)))) (if (< l 1d-12) v (v* v (/ 1 l)))))
(defun cross3 (a b)
  (pt (- (* (py a) (pz b)) (* (pz a) (py b)))
      (- (* (pz a) (px b)) (* (px a) (pz b)))
      (- (* (px a) (py b)) (* (py a) (px b)))))

(defparameter +world-z+ (pt 0 0 1))

(defun ocs-axes (n)
  "AutoCAD's arbitrary axis algorithm: (values ax ay az) of the plane system whose Z is N."
  (let* ((az (unit3 (pt3 n)))
         (ax (unit3 (if (and (< (abs (px az)) (/ 1d0 64)) (< (abs (py az)) (/ 1d0 64)))
                        (cross3 (pt 0 1 0) az)
                        (cross3 (pt 0 0 1) az))))
         (ay (unit3 (cross3 az ax))))
    (values ax ay az)))

(defun ocs->w (p ax ay az)
  (v+ (v+ (v* ax (px p)) (v* ay (py p))) (v* az (pz p))))
(defun w->ocs (p ax ay az)
  (pt (vdot3 p ax) (vdot3 p ay) (vdot3 p az)))

(defun up-normal-p (n)
  (and (< (abs (px n)) 1d-7) (< (abs (py n)) 1d-7) (plusp (pz n))))
(defun down-normal-p (n)
  (and (< (abs (px n)) 1d-7) (< (abs (py n)) 1d-7) (minusp (pz n))))

(defun wcs-type-p (type) (member type '("LINE" "POINT" "POLYFACE") :test #'equal))

(defun entity-normal (data)
  (let ((n (dxf 210 data)))
    (if (and (consp n) (realp (car n))) (unit3 (pt3 n)) +world-z+)))

(defun offplane-p (data)
  "True when DATA lies in a plane other than the X,Y plane of the coordinates in use."
  (and (not (wcs-type-p (dxf 0 data)))
       (let ((n (dxf 210 data))) (and (consp n) (realp (car n)) (not (up-normal-p (pt3 n)))))))

(defun with-normal (data n)
  (if (up-normal-p n) (rem-dxf 210 data) (set-dxf 210 (pt3 n) (rem-dxf 210 data))))

(defparameter +flip-x-xf+
  ;; seen from below: (x y z) -> (-x y -z) turns a plane facing down into one facing up
  (make-xf :fn (lambda (p) (pt (- (px p)) (py p) (- (pz p))))
           :ang (lambda (a) (norm-ang (- +pi+ a))) :mirror t))

(defun normalize-down-normal (data)
  "An object whose plane faces straight down, rewritten facing up (same place and shape)."
  (if (and (not (wcs-type-p (dxf 0 data))) (down-normal-p (entity-normal data)))
      (let ((out (xform-data (rem-dxf 210 data) +flip-x-xf+)))
        (if (and (realp (dxf 39 data)) (/= (dxf 39 data) 0)) (set-dxf 39 (- (dxf 39 data)) out) out))
      data))

(defun xform-planar (data xf)
  "Transform DATA (an object with a plane) by the 3D transform XF — a rigid motion, rotation,
mirror or uniform scale in any direction. The object moves into the transformed plane."
  (multiple-value-bind (ax ay az) (ocs-axes (entity-normal data))
    (let* ((fn (xf-fn xf))
           (o (funcall fn (pt 0 0 0)))
           (ax2 (v- (funcall fn ax) o)) (ay2 (v- (funcall fn ay) o)) (az2 (v- (funcall fn az) o))
           (s (sqrt (vdot3 ax2 ax2)))
           (n2 (unit3 (cross3 ax2 ay2))))
      (multiple-value-bind (bx by bz) (ocs-axes n2)
        (let* ((theta (atan (vdot3 ax2 by) (vdot3 ax2 bx)))
               (f2 (lambda (p) (w->ocs (funcall fn (ocs->w (pt3 p) ax ay az)) bx by bz)))
               (xf2 (make-xf :fn f2 :ang (lambda (a) (norm-ang (+ a theta))) :scale (d s)
                             :mirror nil :uniform (xf-uniform xf)))
               (out (xform-data (rem-dxf 210 data) xf2))
               (th (dxf 39 data)))
          (when (and (realp th) (/= th 0))
            (setf out (set-dxf 39 (* th (vdot3 az2 n2)) out)))
          (normalize-down-normal (with-normal out n2)))))))

(defun xform-any (data xf)
  "XF applied to DATA in 3D, whatever plane DATA lies in."
  (if (wcs-type-p (dxf 0 data)) (xform-data data xf) (xform-planar data xf)))

;;; ---------- pieces of an object in another plane ----------

(defun offplane-pieces (data &optional flat-pieces)
  "DATA's outline as 3D pieces in the coordinates in use: straight edges as :seg (they snap),
curves and text as :gseg, with the elevation and thickness of the object. FLAT-PIECES, when
given, are used instead of DATA's own pieces (in its plane's coordinates)."
  (multiple-value-bind (ax ay az) (ocs-axes (entity-normal data))
    (let* ((type (dxf 0 data))
           (flat (rem-dxf 210 data))
           (elev (if (equal type "LWPOLYLINE") (d (or (dxf 38 data) 0)) 0d0))
           (th (if (member type '("CIRCLE" "ARC" "LWPOLYLINE") :test #'equal) (d (or (dxf 39 data) 0)) 0d0))
           (out nil))
      (flet ((m (p dz) (ocs->w (pt (px p) (py p) (+ (pz (pt3 p)) elev dz)) ax ay az)))
        (dolist (pc (or flat-pieces (data-pieces flat)))
          (let* ((kind (case (first pc) ((:seg :pt) (first pc)) (t :gseg)))
                 (pts (case (first pc)
                        ((:seg :gseg) (list (pt3 (second pc)) (pt3 (third pc))))
                        (:pt (list (pt3 (second pc))))
                        (:circle (let ((c (pt3 (second pc))))
                                   (mapcar (lambda (q) (pt (px q) (py q) (pz c))) (arc-points c (third pc) 0d0 +2pi+ 48))))
                        (:arc (destructuring-bind (c r a1 a2) (rest pc)
                                (let ((c (pt3 c))) (mapcar (lambda (q) (pt (px q) (py q) (pz c))) (arc-points c r a1 a2 24)))))
                        (t (loop for sp in (tessellate-piece pc) when (member (first sp) '(:seg :gseg))
                                 append (list (pt3 (second sp)) (pt3 (third sp))))))))
            (dolist (dz (if (zerop th) '(0d0) (list 0d0 th)))
              (if (eq kind :pt)
                  (push (list :pt (m (first pts) dz)) out)
                  (loop for (a b) on pts while b do (push (list kind (m a dz) (m b dz)) out))))
            (unless (or (zerop th) (null pts))
              (let ((n (length pts)))
                (loop for p in pts for i from 0
                      when (or (= i 0) (= i (1- n)) (and (> n 3) (zerop (mod i (max 1 (floor n 8))))))
                        do (push (list :seg (m p 0d0) (m p th)) out))))))
        (nreverse out)))))

(defun offplane-grips (data)
  "Grips of an object in another plane: its usual grip points; dragging one moves the object."
  (multiple-value-bind (ax ay az) (ocs-axes (entity-normal data))
    (let ((flat (rem-dxf 210 data)))
      (remove-duplicates
       (loop for (p . nil) in (entity-grips flat)
             collect (cons (ocs->w (pt3 p) ax ay az) :offplane))
       :key #'car :test (lambda (a b) (< (dist a b) 1d-9))))))

;;; ---------- the working plane of the side views ----------

(defvar *ucs-off* nil "Bound true where entity data must be seen in world coordinates (files, clipboard).")
(defvar *ucs-cache-view* :none)
(defvar *ucs-cache* nil)

(defun facing-view-p (v)
  "A view looking straight along an axis (not the isometric ones), or one set up by UCS."
  (or (eq (fifth v) :ucs)
      (and (not (eq (fifth v) :orbit))
       (let ((dir (fourth v)))
        (= 2 (count-if (lambda (c) (< (abs c) 1d-9)) (list (px dir) (py dir) (pz dir))))))))

(defun ucs ()
  "The working plane in use: (x y z) axes in world coordinates, or NIL for world (Top view and
the isometric views)."
  (cond (*ucs-off* nil)
        ((null *view*) nil)
        ((eq *view* *ucs-cache-view*) *ucs-cache*)
        (t (setf *ucs-cache-view* *view*
                 *ucs-cache* (and (facing-view-p *view*)
                                  (list (pt3 (second *view*)) (pt3 (third *view*))
                                        (unit3 (cross3 (second *view*) (third *view*)))
                                        (if (eq (fifth *view*) :ucs) (pt3 (sixth *view*)) (pt 0 0 0))))))))

(defun projected-view-p ()
  "A 3D view drawn by projection (the isometric views); the facing views are drawn flat in
their working plane."
  (and *view* (not (ucs))))

(defun ucs-xf (u &optional to-world)
  "World -> working-plane coordinates for frame U = (x y z origin) (or back with TO-WORLD)."
  (destructuring-bind (x y z &optional (o (pt 0 0 0))) u
    (make-xf :fn (if to-world
                     (lambda (p) (v+ (ocs->w (pt3 p) x y z) o))
                     (lambda (p) (w->ocs (v- (pt3 p) o) x y z))))))

(defun ucs-point-to-world (p)
  "A point picked in the working plane, in world coordinates."
  (let ((u (ucs))) (if u (funcall (xf-fn (ucs-xf u t)) p) p)))
(defun ucs-vector-to-world (v)
  (let ((u (ucs))) (if u (destructuring-bind (x y z &rest r) u (declare (ignore r)) (ocs->w (pt3 v) x y z)) (pt3 v))))

(defun data-to-ucs (data u) (if u (normalize-down-normal (xform-any data (ucs-xf u))) data))
(defun data-from-ucs (data u) (if u (xform-any data (ucs-xf u t)) data))


(defun local-data (e u)
  (let ((c (gethash e *local-cache*)) (w (ename-data e)))
    (if (and c (eq (first c) u) (eq (second c) w))
        (third c)
        (let ((l (ignore-errors (data-to-ucs w u))))
          (if l
              (progn (setf (gethash e *local-cache*) (list u w l)) l)
              w)))))

(defun offplane-polylines (data)
  "DATA (an object in another plane) as connected 3D polylines - one per edge, arc or circle, plus
its top and side edges when it has a thickness - for drawing with its linetype."
  (multiple-value-bind (ax ay az) (ocs-axes (entity-normal data))
    (let* ((type (dxf 0 data))
           (flat (rem-dxf 210 data))
           (elev (if (equal type "LWPOLYLINE") (d (or (dxf 38 data) 0)) 0d0))
           (th (if (member type '("CIRCLE" "ARC" "LWPOLYLINE") :test #'equal) (d (or (dxf 39 data) 0)) 0d0))
           (out nil))
      (flet ((m (p dz) (ocs->w (pt (px p) (py p) (+ (pz (pt3 p)) elev dz)) ax ay az)))
        (dolist (pc (data-pieces flat))
          (let ((pts (case (first pc)
                       ((:seg :gseg) (list (pt3 (second pc)) (pt3 (third pc))))
                       (:circle (let ((c (pt3 (second pc))))
                                  (mapcar (lambda (q) (pt (px q) (py q) (pz c))) (arc-points c (third pc) 0d0 +2pi+ 96))))
                       (:arc (destructuring-bind (c r a1 a2) (rest pc)
                               (let ((c (pt3 c))) (mapcar (lambda (q) (pt (px q) (py q) (pz c))) (arc-points c r a1 a2 48)))))
                       (t nil))))
            (when (cdr pts)
              (push (mapcar (lambda (p) (m p 0d0)) pts) out)
              (unless (zerop th)
                (push (mapcar (lambda (p) (m p th)) pts) out)
                (dolist (p (list (first pts) (car (last pts))))
                  (push (list (m p 0d0) (m p th)) out)))))))
      (nreverse out))))
