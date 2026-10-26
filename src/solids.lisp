;;;; solids.lisp - 3D solids made as meshes: BOX, WEDGE, CYLINDER, CONE, SPHERE, TORUS, and the
;;;; solids made from 2D shapes: EXTRUDE, REVOLVE, SWEEP, LOFT and PRESSPULL. Everything is built
;;;; in the working plane (Top, a side view or a UCS), so a box drawn in the Front view stands on
;;;; its side. All of them are saved as AutoCAD polyface meshes.

(in-package :fancy-pencil)

(defparameter *solid-segments* 36 "Facets around a full circle.")

;;; ---------- the builder: rings of points joined into a mesh ----------

(defun turn-angle (a b c)
  "Angle the path A->B->C turns at B (0 when straight)."
  (let* ((u (v- b a)) (v (v- c b)) (lu (sqrt (vdot3 u u))) (lv (sqrt (vdot3 v v))))
    (if (or (< lu 1d-12) (< lv 1d-12)) 0d0
        (acos (max -1d0 (min 1d0 (/ (vdot3 u v) (* lu lv))))))))

(defun sharp-indices (pts closed)
  "Indices of PTS where the outline turns sharply (corners), plus the ends of an open outline."
  (let* ((n (length pts)) (v (coerce pts 'vector)) (out nil))
    (dotimes (j n (nreverse out))
      (cond ((and (not closed) (or (= j 0) (= j (1- n)))) (push j out))
            ((or closed (< 0 j (1- n)))
             (when (> (turn-angle (aref v (mod (1- j) n)) (aref v j) (aref v (mod (1+ j) n))) (/ pi 9))
               (push j out)))))))

(defun isoline-indices (n count &optional closed)
  "COUNT evenly spaced indices among N (the extra lines that show a curved side)."
  (if (<= n 1) (list 0)
      (remove-duplicates (loop for k from 0 below count
                               collect (min (1- n) (round (* k (/ (if closed n (1- n)) count))))))))

(defun polygon-2d-in-plane (pts)
  "PTS (a flat 3D loop) as 2D points in its own plane, and that plane's normal."
  (let ((n (or (newell-normal pts) (pt 0 0 1))))
    (multiple-value-bind (ax ay az) (ocs-axes n)
      (declare (ignore az))
      (values (mapcar (lambda (p) (pt (vdot3 p ax) (vdot3 p ay))) pts) n))))

(defun triangulate (pts2)
  "Ear-clipping triangulation of a simple polygon PTS2 (2D). Returns index triples (0-based)."
  (let* ((n (length pts2)) (v (coerce pts2 'vector))
         (idx (loop for i below n collect i))
         (area (loop for i below n sum (let ((a (aref v i)) (b (aref v (mod (1+ i) n))))
                                         (- (* (px a) (py b)) (* (px b) (py a))))))
         (ccw (> area 0))
         (tris nil))
    (unless ccw (setf idx (reverse idx)))
    (flet ((cross (o a b) (- (* (- (px a) (px o)) (- (py b) (py o))) (* (- (py a) (py o)) (- (px b) (px o)))))
           (inside (p a b c)
             ;; A B C counterclockwise: P is inside (or on an edge) when it is left of every edge
             (flet ((cr (o a b) (- (* (- (px a) (px o)) (- (py b) (py o))) (* (- (py a) (py o)) (- (px b) (px o))))))
               (and (>= (cr a b p) -1d-14) (>= (cr b c p) -1d-14) (>= (cr c a p) -1d-14)))))
      (loop with guard = 0
            while (and (> (length idx) 3) (< guard (* 4 n n)))
            do (incf guard)
               (let ((m (length idx)) (cut nil))
                 (loop for k below m
                       for i0 = (nth (mod (1- k) m) idx) for i1 = (nth k idx) for i2 = (nth (mod (1+ k) m) idx)
                       do (when (and (> (cross (aref v i0) (aref v i1) (aref v i2)) 1d-14)
                                     (notany (lambda (j) (and (not (member j (list i0 i1 i2)))
                                                              (inside (aref v j) (aref v i0) (aref v i1) (aref v i2))))
                                             idx))
                            (push (list i0 i1 i2) tris)
                            (setf idx (remove i1 idx) cut t)
                            (return)))
                 (unless cut               ; nothing clipped (degenerate): fan the rest
                   (loop for k from 1 below (1- (length idx))
                         do (push (list (first idx) (nth k idx) (nth (1+ k) idx)) tris))
                   (setf idx nil))))
      (when (= (length idx) 3) (push idx tris))
      (nreverse tris))))

(defun rings-mesh (rings &key closed-ring closed-path cap-start cap-end ring-visible long-visible)
  "A mesh joining RINGS (lists of 3D points, all the same length) side by side. CLOSED-RING: each
ring is a loop; CLOSED-PATH: the last ring joins the first. CAP-START / CAP-END close the ends
with flat faces. RING-VISIBLE (ring index -> boolean) and LONG-VISIBLE (point index -> boolean)
choose which edges show."
  (let* ((m (length rings)) (n (length (first rings)))
         (verts (apply #'append rings))
         (faces nil))
    (flet ((ix (i j) (1+ (+ (* (mod i m) n) (mod j n))))
           (vis (idx show) (if show idx (- idx))))
      (loop for i from 0 below (if closed-path m (1- m))
            do (loop for j from 0 below (if closed-ring n (1- n))
                     do (let ((ring-a (funcall ring-visible i))
                              (ring-b (funcall ring-visible (mod (1+ i) m)))
                              (long-j (funcall long-visible j))
                              (long-j1 (funcall long-visible (mod (1+ j) n))))
                          (push (list (vis (ix i j) ring-a)
                                      (vis (ix i (1+ j)) long-j1)
                                      (vis (ix (1+ i) (1+ j)) ring-b)
                                      (vis (ix (1+ i) j) long-j))
                                faces))))
      ;; flat ends: triangles whose edges stay hidden (the ring edges outline them)
      (flet ((cap (i flip)
               (let ((tris (triangulate (polygon-2d-in-plane (nth i rings)))))
                 (dolist (tr tris)
                   (let ((f (mapcar (lambda (j) (- (ix i j))) tr)))
                     (push (if flip (reverse f) f) faces))))))
        (when cap-start (cap 0 t))
        (when cap-end (cap (1- m) nil))))
    (make-mesh-data verts (nreverse faces))))

(defun ring-shape-visibility (profile closed &optional (iso 4))
  "Point indices of PROFILE whose lines along the solid show: its corners, or a few isolines on a
smooth shape."
  (let ((sharp (sharp-indices profile closed)))
    (if (and closed (null sharp))
        (isoline-indices (length profile) iso t)
        (union sharp (if (> (length profile) 12) (isoline-indices (length profile) iso closed) nil)))))

;;; ---------- primitives ----------

(defun circle-ring (c r n &optional (z 0d0))
  (loop for k below n
        collect (let ((a (* +2pi+ (/ k n)))) (pt (+ (px c) (* r (cos a))) (+ (py c) (* r (sin a))) (+ (pz c) z)))))

(defun prism-mesh (base dir &key (closed t))
  "BASE (a loop of 3D points) moved along DIR: a prism, or a tube side when not CLOSED."
  (let* ((top (mapcar (lambda (p) (v+ p dir)) base))
         (show (ring-shape-visibility base closed)))
    (rings-mesh (list base top) :closed-ring closed :cap-start closed :cap-end closed
                :ring-visible (constantly t) :long-visible (lambda (j) (member j show)))))

(defun box-mesh (p1 p2 h)
  (let ((z (pz (pt3 p1))))
    (prism-mesh (list (pt (px p1) (py p1) z) (pt (px p2) (py p1) z) (pt (px p2) (py p2) z) (pt (px p1) (py p2) z))
                (pt 0 0 h))))

(defun wedge-mesh (p1 p2 h)
  "A wedge: full height along the edge at P1's X, sloping down to nothing at P2's X."
  (let* ((z (pz (pt3 p1)))
         (a (pt (px p1) (py p1) z)) (b (pt (px p2) (py p1) z)) (c (pt (px p2) (py p2) z)) (d (pt (px p1) (py p2) z))
         (e (pt (px p1) (py p1) (+ z h))) (f (pt (px p1) (py p2) (+ z h))))
    (make-mesh-data (list a b c d e f)
                    (list (list 1 4 3 2) (list 1 5 6 4) (list 5 2 3 6) (list 1 2 5) (list 4 6 3)))))

(defun cone-mesh (c r1 r2 h &optional (n *solid-segments*))
  "A cylinder (R1 = R2), cone (R2 = 0) or frustum on the circle at C, height H."
  (let* ((base (circle-ring c r1 n))
         (top (if (< r2 1d-12) (make-list n :initial-element (v+ (pt3 c) (pt 0 0 h))) (circle-ring c r2 n h))))
    (rings-mesh (list base top) :closed-ring t :cap-start t :cap-end (> r2 1d-12)
                :ring-visible (constantly t) :long-visible (let ((iso (isoline-indices n 4 t))) (lambda (j) (member j iso))))))

(defun sphere-mesh (c r &optional (n *solid-segments*) (m 18))
  (let ((rings (loop for i from 0 to m
                     collect (let* ((phi (- (* pi (/ i m)) (/ pi 2))) (rr (* r (cos phi))) (z (* r (sin phi))))
                               (circle-ring c (max rr 0d0) n z)))))
    (rings-mesh rings :closed-ring t
                :ring-visible (lambda (i) (and (< 0 i m) (zerop (mod i 3))))
                :long-visible (let ((iso (isoline-indices n 8 t))) (lambda (j) (member j iso))))))

(defun torus-mesh (c big small &optional (n *solid-segments*) (m 18))
  (let ((rings (loop for i below n
                     collect (let* ((th (* +2pi+ (/ i n))) (dir (pt (cos th) (sin th) 0)))
                               (loop for k below m
                                     collect (let ((ph (* +2pi+ (/ k m))))
                                               (v+ (pt3 c) (v+ (v* dir (+ big (* small (cos ph)))) (pt 0 0 (* small (sin ph)))))))))))
    (rings-mesh rings :closed-ring t :closed-path t
                :ring-visible (let ((iso (isoline-indices n 8 t))) (lambda (i) (member i iso)))
                :long-visible (let ((iso (isoline-indices m 4 t))) (lambda (j) (member j iso))))))

;;; ---------- asking for sizes ----------

(defun ask-height (prompt base &optional default)
  "A height: typed (may be negative) or picked (its distance from BASE)."
  (let ((r (get-user-input :distance :prompt (if default (format nil "~A <~A>: " prompt (fmt-len default)) (format nil "~A: " prompt))
                                     :base base)))
    (cond ((realp r) (d r)) ((pointp r) (dist2d base r)) (t default))))

(defun ask-radius (prompt center &optional default)
  (let ((r (get-user-input :distance :prompt (format nil "~A or [Diameter]~@[ <~A>~]: " prompt (and default (fmt-len default)))
                                     :base center :keywords '("Diameter"))))
    (cond ((equal r "Diameter")
           (let ((dd (get-user-input :distance :prompt (format nil "Specify diameter~@[ <~A>~]: " (and default (fmt-len (* 2 default)))) :base center)))
             (cond ((realp dd) (/ (d dd) 2)) ((pointp dd) (/ (dist2d center dd) 2)) (t default))))
          ((realp r) (d r)) ((pointp r) (dist2d center r))
          (t default))))

(defvar *last-solid-height* 1d0)

(defun add-solid (mesh what)
  (let ((e (add mesh)))
    (out (format nil "~A created~@[ (volume ~A)~].~%" what
                 (let ((v (mesh-volume mesh))) (and (> v 1d-12) (fmt-num v)))))
    (when (gui-p) (gui-regen))
    e))

(defcmd "BOX" () "Creates a 3D solid box in the working plane."
  (let ((p1 (gpoint "Specify first corner or [Center]: " :keywords '("Center"))) (p2 nil))
    (when (equal p1 "Center")
      (let* ((c (gpoint "Specify center: "))
             (q (and (pointp c) (get-user-input :corner :base c :prompt "Specify corner: "))))
        (setf p1 (and (pointp q) (v- (v* (pt3 c) 2) (pt3 q))) p2 q)))
    (when (pointp p1)
      (unless p2
        (setf p2 (get-user-input :corner :base p1 :prompt "Specify other corner or [Cube/Length]: "
                                         :keywords '("Cube" "Length"))))
      (cond
        ((equal p2 "Cube")
         (let ((l (ask-height "Specify length" p1)))
           (when l (add-solid (box-mesh p1 (v+ (pt3 p1) (pt l l 0)) l) "Box"))))
        ((equal p2 "Length")
         (let* ((l (ask-height "Specify length" p1)) (wd (and l (ask-height "Specify width" p1)))
                (h (and wd (ask-height "Specify height" p1 *last-solid-height*))))
           (when h (setf *last-solid-height* h) (add-solid (box-mesh p1 (v+ (pt3 p1) (pt l wd 0)) h) "Box"))))
        ((pointp p2)
         (let ((h (ask-height "Specify height" p2 *last-solid-height*)))
           (when h (setf *last-solid-height* h) (add-solid (box-mesh p1 p2 h) "Box"))))))))

(defcmd "WEDGE" ("WE") "Creates a 3D solid wedge: full height at the first corner, sloping to the opposite side."
  (let ((p1 (gpoint "Specify first corner: ")))
    (when (pointp p1)
      (let ((p2 (get-user-input :corner :base p1 :prompt "Specify other corner: ")))
        (when (pointp p2)
          (let ((h (ask-height "Specify height" p2 *last-solid-height*)))
            (when h (setf *last-solid-height* h) (add-solid (wedge-mesh p1 p2 h) "Wedge"))))))))

(defcmd "CYLINDER" ("CYL") "Creates a 3D solid cylinder standing on the working plane."
  (let ((c (gpoint "Specify center point of base: ")))
    (when (pointp c)
      (let ((r (ask-radius "Specify base radius" c)))
        (when (and r (plusp r))
          (let ((h (ask-height "Specify height" c *last-solid-height*)))
            (when h (setf *last-solid-height* h) (add-solid (cone-mesh (pt3 c) r r h) "Cylinder"))))))))

(defcmd "CONE" () "Creates a 3D solid cone (or a cut-off cone with a top radius)."
  (let ((c (gpoint "Specify center point of base: ")))
    (when (pointp c)
      (let ((r (ask-radius "Specify base radius" c)))
        (when (and r (plusp r))
          (let ((top 0d0) (h nil))
            (loop
              (let ((x (get-user-input :distance :prompt (format nil "Specify height or [Top radius] <~A>: " (fmt-len *last-solid-height*))
                                                 :base c :keywords '("Top"))))
                (cond ((equal x "Top")
                       (setf top (or (ask-height "Specify top radius" c 0d0) 0d0)))
                      ((realp x) (setf h (d x)) (return))
                      ((pointp x) (setf h (dist2d c x)) (return))
                      (t (setf h *last-solid-height*) (return)))))
            (setf *last-solid-height* h)
            (add-solid (cone-mesh (pt3 c) r (abs top) h) "Cone")))))))

(defcmd "SPHERE" () "Creates a 3D solid sphere."
  (let ((c (gpoint "Specify center point: ")))
    (when (pointp c)
      (let ((r (ask-radius "Specify radius" c)))
        (when (and r (plusp r)) (add-solid (sphere-mesh (pt3 c) r) "Sphere"))))))

(defcmd "TORUS" ("TOR") "Creates a 3D solid torus (a ring), lying in the working plane."
  (let ((c (gpoint "Specify center point: ")))
    (when (pointp c)
      (let ((big (ask-radius "Specify radius" c)))
        (when (and big (plusp big))
          (let ((small (ask-radius "Specify tube radius" c)))
            (when (and small (plusp small)) (add-solid (torus-mesh (pt3 c) big small) "Torus"))))))))

;;; ---------- shapes (profiles and paths) from objects ----------

(defun bulge-points (p q b &optional (per-turn 36))
  "Points from P to Q along a polyline segment with bulge B (P excluded, Q included)."
  (if (< (abs b) 1d-12) (list q)
      (let* ((sweep (* 4 (atan b)))
             (k (max 2 (ceiling (* per-turn (/ (abs sweep) +2pi+)))))
             (theta (abs sweep))
             (chord (dist2d p q))
             (r (/ chord (* 2 (sin (/ theta 2)))))
             (h (/ chord (* 2 (tan (/ theta 2)))))
             (c (polar (midpt p q) (+ (ang p q) (if (> b 0) (/ pi 2) (- (/ pi 2)))) h))
             (a0 (ang c p)))
        (loop for i from 1 to k
              collect (let ((a (+ a0 (* sweep (/ i k))))) (pt (+ (px c) (* r (cos a))) (+ (py c) (* r (sin a))) (pz (pt3 p))))))))

(defun flat-shape-points (data)
  "Ordered points of DATA in its own plane's coordinates: (values points closed-p)."
  (let ((type (dxf 0 data)) (n *solid-segments*))
    (cond
      ((equal type "CIRCLE")
       (values (circle-ring (pt3 (dxf 10 data)) (d (dxf 40 data)) n) t))
      ((equal type "ARC")
       (let ((c (pt3 (dxf 10 data))))
         (values (mapcar (lambda (q) (pt (px q) (py q) (pz c)))
                         (arc-points c (d (dxf 40 data)) (d (dxf 50 data)) (d (dxf 51 data))
                                     (max 4 (ceiling (* n (/ (arc-sweep (dxf 50 data) (dxf 51 data)) +2pi+))))))
                 nil)))
      ((equal type "LWPOLYLINE")
       (let* ((el (d (or (dxf 38 data) 0)))
              (vs (mapcar (lambda (v) (list (pt (px (first v)) (py (first v)) el) (or (second v) 0d0))) (lwpoly-vertices data)))
              (closed (or (lwpoly-closed-p data)
                          (and (> (length vs) 2) (< (dist (first (first vs)) (first (car (last vs)))) 1d-9))))
              (vs (if (and closed (not (lwpoly-closed-p data))) (butlast vs) vs))
              (pts (list (first (first vs)))))
         (loop for (a b) on (if closed (append vs (list (first vs))) vs) while b
               do (setf pts (append pts (bulge-points (first a) (first b) (second a)))))
         (when closed (setf pts (butlast pts)))
         (values pts closed)))
      ((equal type "ELLIPSE")
       (let* ((full (< (abs (- (arc-sweep (or (dxf 41 data) 0d0) (or (dxf 42 data) +2pi+)) +2pi+)) 1d-6))
              (pts (ellipse-points data (* 2 n))))
         (values (if full (butlast pts) pts) full)))
      ((equal type "SPLINE")
       (let* ((pts (mapcar #'pt3 (spline-points data)))
              (closed (or (logtest 1 (or (dxf 70 data) 0)) (< (dist (first pts) (car (last pts))) 1d-9))))
         (values (if (and closed (< (dist (first pts) (car (last pts))) 1d-9)) (butlast pts) pts) closed)))
      ((equal type "LINE") (values (list (pt3 (dxf 10 data)) (pt3 (dxf 11 data))) nil))
      (t (values nil nil)))))

(defun shape-of (e)
  "Object E as a shape in the coordinates in use: (values points closed-p plane-normal)."
  (let* ((data (ent-data e)))
    (if (wcs-type-p (dxf 0 data))
        (multiple-value-bind (pts closed) (flat-shape-points data)
          (values pts closed (or (and (cddr pts) (newell-normal pts)) (pt 0 0 1))))
        (multiple-value-bind (ax ay az) (ocs-axes (entity-normal data))
          (multiple-value-bind (pts closed) (flat-shape-points (rem-dxf 210 data))
            (values (mapcar (lambda (p) (ocs->w (pt3 p) ax ay az)) pts) closed az))))))

(defun pick-shapes (prompt)
  "Selected objects that can be shapes, with a note about the others."
  (let* ((ents (get-selection prompt))
         (ok (remove-if-not (lambda (e) (member (ent-type e) '("CIRCLE" "ARC" "LWPOLYLINE" "ELLIPSE" "SPLINE" "LINE") :test #'equal)) ents)))
    (when (< (length ok) (length ents))
      (out (format nil "~D object~:P skipped (use circles, arcs, polylines, ellipses, splines or lines).~%" (- (length ents) (length ok)))))
    ok))

(defun delete-profiles (ents)
  "Remove the shapes a solid was made from (DELOBJ, as AutoCAD: on unless set to 0)."
  (unless (eql (sysvar "DELOBJ") 0) (dolist (e ents) (delete-entity e))))

(defun outward-loop (pts normal)
  "PTS ordered counterclockwise seen from the side NORMAL points to."
  (let ((n (newell-normal pts))) (if (and n (minusp (vdot3 n normal))) (reverse pts) pts)))

;;; ---------- EXTRUDE ----------

(defun extrude-mesh (pts closed dir)
  (if (and closed (minusp (vdot3 (or (newell-normal pts) dir) dir)))
      (prism-mesh (reverse pts) dir :closed closed)
      (prism-mesh pts dir :closed closed)))

(defcmd "EXTRUDE" ("EXT") "Extrudes closed shapes into solids (open ones into surfaces): by a height, along a direction, or along a path."
  (let ((ents (pick-shapes "Select objects to extrude: ")))
    (when ents
      (let ((r (get-user-input :distance :prompt (format nil "Specify height of extrusion or [Direction/Path] <~A>: " (fmt-len *last-solid-height*))
                                         :keywords '("Direction" "Path"))))
        (cond
          ((equal r "Path")
           (let ((pe (sel-entity (get-user-input :entity :prompt "Select extrusion path: "))))
             (when pe (sweep-shapes ents pe nil))))
          (t
           (let ((dirfn
                   (cond ((equal r "Direction")
                          (let* ((a (gpoint "Specify start point of direction: "))
                                 (b (and (pointp a) (gpoint "Specify end point of direction: " :base a))))
                            (and (pointp b) (let ((v (v- (pt3 b) (pt3 a)))) (lambda (n) (declare (ignore n)) v)))))
                         (t (let ((h (cond ((realp r) (d r)) ((pointp r) (dist2d (pt 0 0) r)) (t *last-solid-height*))))
                              (setf *last-solid-height* h)
                              (lambda (n) (v* n h)))))))
             (when dirfn
               (let ((n 0))
                 (dolist (e ents)
                   (multiple-value-bind (pts closed normal) (shape-of e)
                     (when (cdr pts)
                       (add (extrude-mesh pts closed (funcall dirfn normal))) (incf n))))
                 (delete-profiles ents)
                 (out (format nil "~D object~:P extruded.~%" n))
                 (when (gui-p) (gui-regen)))))))))))

;;; ---------- REVOLVE ----------

(defun revolve-mesh (pts closed origin axis angle)
  (let* ((full (> (abs angle) (- +2pi+ 1d-9)))
         (steps (max 4 (ceiling (* *solid-segments* (/ (abs angle) +2pi+)))))
         (k (unit3 axis))
         (rings (loop for i from 0 below (if full steps (1+ steps))
                      collect (let ((a (* angle (/ i steps))))
                                (mapcar (lambda (p) (v+ (pt3 origin) (rotate-about (v- (pt3 p) (pt3 origin)) k a))) pts))))
         (show (sharp-indices pts closed)))
    (rings-mesh rings :closed-ring closed :closed-path full
                :cap-start (and closed (not full)) :cap-end (and closed (not full))
                :ring-visible (let ((iso (isoline-indices (length rings) 4 full)))
                                (lambda (i) (or (member i iso) (and (not full) (or (= i 0) (= i (1- (length rings))))))))
                :long-visible (let ((s (if (or show (not closed)) show (isoline-indices (length pts) 4 t))))
                                (lambda (j) (member j s))))))

(defcmd "REVOLVE" ("REV") "Revolves shapes about an axis to make solids (or surfaces from open shapes)."
  (let ((ents (pick-shapes "Select objects to revolve: ")))
    (when ents
      (let* ((r (gpoint "Specify axis start point or define axis by [Object/X/Y/Z] <Object>: " :keywords '("Object" "X" "Y" "Z")))
             (axis (cond ((pointp r)
                          (let ((b (gpoint "Specify axis endpoint: " :base r)))
                            (and (pointp b) (list (pt3 r) (v- (pt3 b) (pt3 r))))))
                         ((equal r "X") (list (pt 0 0 0) (pt 1 0 0)))
                         ((equal r "Y") (list (pt 0 0 0) (pt 0 1 0)))
                         ((equal r "Z") (list (pt 0 0 0) (pt 0 0 1)))
                         (t (let ((ae (sel-entity (get-user-input :entity :prompt "Select an object (a line) for the axis: "))))
                              (when ae (multiple-value-bind (pts) (shape-of ae)
                                         (and (cdr pts) (list (first pts) (v- (car (last pts)) (first pts)))))))))))
        (when axis
          (let* ((a (get-user-input :angle :prompt "Specify angle of revolution <360>: " :base (first axis)))
                 (angle (if (realp a) (d a) +2pi+))
                 (angle (if (< (abs angle) 1d-9) +2pi+ angle))
                 (n 0))
            (dolist (e ents)
              (multiple-value-bind (pts closed) (shape-of e)
                (when (cdr pts) (add (revolve-mesh pts closed (first axis) (second axis) angle)) (incf n))))
            (delete-profiles ents)
            (out (format nil "~D object~:P revolved.~%" n))
            (when (gui-p) (gui-regen))))))))

;;; ---------- SWEEP ----------

(defun shape-frame (pts normal)
  "Centre and in-plane axes (u v) of a flat shape."
  (let* ((c (v* (reduce #'v+ pts) (/ 1d0 (length pts)))))
    (multiple-value-bind (ax ay) (ocs-axes normal) (values c ax ay))))

(defun sweep-mesh (profile closed normal path path-closed)
  "PROFILE (a flat shape with plane NORMAL) carried along the 3D polyline PATH, kept square to
it, with mitred corners."
  (multiple-value-bind (c u v) (shape-frame profile normal)
    (let* ((local (mapcar (lambda (p) (let ((d (v- (pt3 p) c))) (cons (vdot3 d u) (vdot3 d v)))) profile))
           (path (if (and path-closed (< (dist (first path) (car (last path))) 1d-9)) (butlast path) path))
           (m (length path))
           (seg-t (loop for i below (if path-closed m (1- m))
                        collect (unit3 (v- (nth (mod (1+ i) m) path) (nth i path)))))
           (t0 (first seg-t))
           ;; the profile's own axes turned so its plane is square to the path's start
           (rot0 (rotation-taking (unit3 normal) t0))
           (a (mat-apply rot0 u))
           (frames nil))
      ;; carry the axes along each segment (parallel transport)
      (loop for i from 0 below (length seg-t)
            for ti = (nth i seg-t)
            do (when (> i 0)
                 (let ((r (rotation-taking (nth (1- i) seg-t) ti)))
                   (setf a (mat-apply r a))))
               (let* ((a1 (unit3 (v- a (v* ti (vdot3 a ti))))) (b1 (cross3 ti a1)))
                 (push (list ti a1 b1) frames)))
      (setf frames (coerce (nreverse frames) 'vector))
      (flet ((ring-at (i)
               ;; the ring at path point I: on the mitre plane between the segments meeting there
               (let* ((p (nth i path)) (ns (length frames))
                      (in (cond ((> i 0) (aref frames (1- i))) (path-closed (aref frames (1- ns))) (t nil)))
                      (outf (cond ((< i ns) (aref frames i)) (t nil)))
                      (fr (or outf in))
                      (miter (if (and in outf) (unit3 (v+ (first in) (first outf))) (first fr))))
                 (mapcar (lambda (xy)
                           (let* ((off (v+ (v* (second fr) (car xy)) (v* (third fr) (cdr xy))))
                                  (s (if (> (abs (vdot3 (first fr) miter)) 1d-9)
                                         (- (/ (vdot3 off miter) (vdot3 (first fr) miter))) 0d0)))
                             (v+ (v+ p off) (v* (first fr) s))))
                         local))))
        (let ((rings (loop for i below m collect (ring-at i)))
              (show (ring-shape-visibility profile closed)))
          (rings-mesh rings :closed-ring closed :closed-path path-closed
                      :cap-start (and closed (not path-closed)) :cap-end (and closed (not path-closed))
                      :ring-visible (let ((sharp (sharp-indices path path-closed)))
                                      (lambda (i) (or (member i sharp) (and (not path-closed) (or (= i 0) (= i (1- m)))))))
                      :long-visible (lambda (j) (member j show))))))))

(defun sweep-shapes (ents path-e &optional (delete t))
  (multiple-value-bind (path pclosed) (shape-of path-e)
    (when (cdr path)
      (let ((n 0))
        (dolist (e ents)
          (unless (eq e path-e)
            (multiple-value-bind (pts closed normal) (shape-of e)
              (when (cdr pts) (add (sweep-mesh pts closed normal path pclosed)) (incf n)))))
        (when delete (delete-profiles (cons path-e (remove path-e ents))))
        (out (format nil "~D object~:P swept.~%" n))
        (when (gui-p) (gui-regen))))))

(defcmd "SWEEP" () "Sweeps shapes along a path (line, arc, polyline, spline, helix) to make solids or surfaces."
  (let ((ents (pick-shapes "Select objects to sweep: ")))
    (when ents
      (let ((pe (sel-entity (get-user-input :entity :prompt "Select sweep path: "))))
        (when pe (sweep-shapes ents pe))))))

;;; ---------- LOFT ----------

(defun resample-loop (pts closed n)
  "N points evenly spaced along PTS (a loop when CLOSED)."
  (let* ((path (if closed (append pts (list (first pts))) pts))
         (len (polyline-length path)))
    (loop for i below n
          collect (point-along path (* len (/ i (if closed n (max 1 (1- n)))))))))

(defun align-loop (prev cur closed)
  "CUR turned (and reversed if needed) to line up with PREV, so the loft does not twist."
  (if (not closed) (if (< (dist (first prev) (car (last cur))) (dist (first prev) (first cur))) (reverse cur) cur)
      (let* ((np (newell-normal prev)) (nc (newell-normal cur))
             (cur (if (and np nc (minusp (vdot3 np nc))) (reverse cur) cur))
             (n (length cur)) (best 0) (bd nil))
        (dotimes (k n)
          (let ((s (loop for j below n sum (dist (nth j prev) (nth (mod (+ j k) n) cur)))))
            (when (or (null bd) (< s bd)) (setf bd s best k))))
        (append (nthcdr best cur) (subseq cur 0 best)))))

(defcmd "LOFT" () "Makes a solid (or surface) through cross sections picked in order."
  (out (format nil "Select cross sections in lofting order.~%"))
  (let ((ents (pick-shapes "Select cross sections in lofting order: ")))
    (if (< (length ents) 2)
        (out (format nil "Loft needs at least two cross sections.~%"))
        (let* ((shapes (mapcar (lambda (e) (multiple-value-list (shape-of e))) ents))
               (closed (every #'second shapes))
               (n 64)
               (rings (let ((prev nil))
                        (mapcar (lambda (s)
                                  (let ((r (resample-loop (first s) closed n)))
                                    (setf r (if prev (align-loop prev r closed) r) prev r)))
                                shapes))))
          (add (rings-mesh rings :closed-ring closed :cap-start closed :cap-end closed
                           :ring-visible (constantly t)
                           :long-visible (let ((iso (isoline-indices n 8 closed))) (lambda (j) (member j iso)))))
          (delete-profiles ents)
          (out (format nil "Lofted through ~D cross sections.~%" (length ents)))
          (when (gui-p) (gui-regen))))))

;;; ---------- PRESSPULL ----------

(defun view-depth (p)
  "How near point P is to the viewer (bigger = nearer)."
  (if (projected-view-p) (vdot3 (pt3 p) (fourth *view*)) (pz (pt3 p))))

(defun view-frame ()
  "(right up toward-viewer) for the view: the screen directions in the coordinates objects are
seen in (world for the isometric views, the working plane's otherwise)."
  (if (projected-view-p)
      (list (pt3 (second *view*)) (pt3 (third *view*)) (pt3 (fourth *view*)))
      (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1))))

(defun screen-uv (p frame) (pt (vdot3 (pt3 p) (first frame)) (vdot3 (pt3 p) (second frame))))

(defun oriented-faces (data)
  "Faces of solid DATA as (points outward-normal), the points running counterclockwise seen from
outside. Second value: whether the solid is closed."
  (multiple-value-bind (faces closed) (mesh-display-faces data)
    (values (loop for (ps n) in faces
                  when n collect (let ((nn (newell-normal ps)))
                                   (list (if (and nn (minusp (vdot3 nn n))) (reverse ps) ps) (unit3 n))))
            closed)))

(defun face-hit (e pick)
  "The face of solid E under screen point PICK: (values points outward-normal depth) for the
nearest face turned toward the viewer, or NIL."
  (let* ((data (ent-data e)) (frame (view-frame)) (dir (third frame))
         (q (screen-uv pick frame)) (best nil) (bn nil) (bd nil))
    (multiple-value-bind (faces closed) (oriented-faces data)
      (dolist (f faces)
        (destructuring-bind (ps n) f
          (let ((facing (vdot3 n dir)))
            (when (and (> (abs facing) 1d-9) (or (not closed) (plusp facing))
                       (point-in-polygon-p q (mapcar (lambda (p) (screen-uv p frame)) ps)))
              ;; how near the face is at the cursor: where the line of sight meets its plane
              (let* ((base (v+ (v* (first frame) (px q)) (v* (second frame) (py q))))
                     (depth (/ (- (vdot3 n (first ps)) (vdot3 n base)) facing)))
                (when (or (null bd) (> depth bd)) (setf best ps bn n bd depth)))))))
      (and best (values best bn bd)))))

(defun face-under (e pick) (face-hit e pick))

(defun edge-key (a b)
  (let ((ka (weld-key a)) (kb (weld-key b)))
    (if (string< (prin1-to-string ka) (prin1-to-string kb)) (list ka kb) (list kb ka))))

(defun flat-face-group (data seed n)
  "The faces of solid DATA in the plane of face SEED (outward normal N) and joined to it by edges."
  (let* ((d0 (vdot3 n (first seed)))
         (coplanar (remove-if-not (lambda (f) (and (> (vdot3 (second f) n) 0.9999d0)
                                                   (< (abs (- (vdot3 n (first (first f))) d0)) 1d-6)))
                                  (oriented-faces data)))
         (edge-faces (make-hash-table :test #'equal))
         (seed-keys (mapcar #'weld-key seed)))
    (dolist (f coplanar)
      (loop for (a b) on (append (first f) (list (first (first f)))) while b
            do (push f (gethash (edge-key a b) edge-faces))))
    (let* ((start (or (find-if (lambda (f) (every (lambda (p) (member (weld-key p) seed-keys :test #'equal)) (first f))) coplanar)
                      (first coplanar)))
           (group (and start (list start))) (todo group))
      ;; spread from the seed across shared edges
      (loop while todo
            do (let ((f (pop todo)))
                 (loop for (a b) on (append (first f) (list (first (first f)))) while b
                       do (dolist (g (gethash (edge-key a b) edge-faces))
                            (unless (member g group :test #'eq) (push g group) (push g todo))))))
      group)))

(defun face-outline (group)
  "The outer boundary of a group of coplanar faces, counterclockwise seen from outside."
  (let ((dir (make-hash-table :test #'equal)) (pts (make-hash-table :test #'equal)))
    (dolist (f group)
      (loop for (a b) on (append (first f) (list (first (first f)))) while b
            do (let ((ka (weld-key a)) (kb (weld-key b)))
                 (setf (gethash ka pts) a (gethash kb pts) b)
                 (push kb (gethash ka dir)))))
    ;; boundary edges: no other face of the group runs back along them
    (let ((next (make-hash-table :test #'equal)) (loops nil) (used (make-hash-table :test #'equal)))
      (maphash (lambda (ka kbs)
                 (dolist (kb kbs) (unless (member ka (gethash kb dir) :test #'equal) (setf (gethash ka next) kb))))
               dir)
      (maphash (lambda (k v) (declare (ignore v))
                 (unless (gethash k used)
                   (let ((lp nil) (cur k))
                     (loop while (and cur (not (gethash cur used)) (< (length lp) 10000))
                           do (setf (gethash cur used) t) (push (gethash cur pts) lp) (setf cur (gethash cur next)))
                     (when (>= (length lp) 3) (push (nreverse lp) loops)))))
               next)
      ;; the outer loop is the longest one (the others go round holes)
      (first (sort loops #'> :key (lambda (lp) (loop for (a b) on (append lp (list (first lp))) while b sum (dist a b))))))))

(defun simplify-loop (pts)
  "Drop points lying on a straight line between their neighbours."
  (let ((n (length pts)))
    (if (< n 4)
        pts
        (or (loop for i below n
                  for a = (nth (mod (1- i) n) pts) for b = (nth i pts) for c = (nth (mod (1+ i) n) pts)
                  unless (< (dist (cross3 (v- b a) (v- c b)) (pt 0 0 0)) (* 1d-9 (max 1d0 (expt (dist a c) 2))))
                    collect b)
            pts))))

(defun whole-face (e ps n)
  (let ((outline (face-outline (flat-face-group (ent-data e) ps n))))
    (list e (simplify-loop (or outline ps)) n)))

(defun solid-face-at (pick)
  "The flat face under screen point PICK on any solid, nearest the viewer: (solid outline normal)."
  (let ((best nil) (bd nil))
    (dolist (e (live-entities))
      (when (and (equal (ent-type e) "POLYFACE") (entity-visible-p e))
        (multiple-value-bind (ps n depth) (face-hit e pick)
          (when (and ps (or (null bd) (> depth bd))) (setf best (list e ps n) bd depth)))))
    (and best (apply #'whole-face best))))

(defun face-loop (e pick)
  "The whole flat face of solid E under PICK: (values outline outward-normal)."
  (multiple-value-bind (ps n) (face-hit e pick)
    (when ps (values-list (rest (whole-face e ps n))))))

;;; picking an edge, then one of the faces that meet there

(defun dist-point-segment-2d (p a b)
  (let* ((ab (v- b a)) (l2 (+ (expt (px ab) 2) (expt (py ab) 2)))
         (tt (if (< l2 1d-24) 0d0 (max 0d0 (min 1d0 (/ (+ (* (- (px p) (px a)) (px ab)) (* (- (py p) (py a)) (py ab))) l2))))))
    (dist2d p (pt (+ (px a) (* tt (px ab))) (+ (py a) (* tt (py ab)))))))

(defun solid-edge-at (pick)
  "The solid edge nearest screen point PICK, within the pick box: (solid a b)."
  (let* ((frame (view-frame)) (q (screen-uv pick frame))
         (tol (gui-pixels->world (+ 3 (or (sysvar "PICKBOX") 5)))) (best nil) (bd nil))
    (dolist (e (live-entities) best)
      (when (and (equal (ent-type e) "POLYFACE") (entity-visible-p e))
        (dolist (f (mesh-display-faces (ent-data e)))
          (destructuring-bind (ps n vis) f
            (declare (ignore n))
            (loop for (a b) on (append ps (list (first ps))) while b for v in vis
                  when v do (let ((dd (dist-point-segment-2d q (screen-uv a frame) (screen-uv b frame))))
                              (when (and (< dd tol) (or (null bd) (< dd bd))) (setf best (list e a b) bd dd))))))))))

(defun faces-at-edge (e a b)
  "The flat faces (solid outline normal) of solid E that edge AB borders, the one facing the viewer first."
  (let ((ka (weld-key a)) (kb (weld-key b)) (out nil) (dir (third (view-frame))))
    (dolist (f (oriented-faces (ent-data e)))
      (let ((keys (mapcar #'weld-key (first f))))
        (when (and (member ka keys :test #'equal) (member kb keys :test #'equal)
                   (notany (lambda (o) (> (vdot3 (third o) (second f)) 0.9999d0)) out))
          (push (whole-face e (first f) (second f)) out))))
    (sort out #'> :key (lambda (o) (vdot3 (third o) dir)))))

(defun outline-pieces (pts) (loop for (a b) on (append pts (list (first pts))) while b collect (list :seg (pt3 a) (pt3 b))))

(defvar *pp-face-choice* nil "The face shown while choosing among the faces at an edge.")

(defun pick-face-by-edge ()
  "Edge: pick an edge of a solid, then choose between the faces that meet there (solid outline normal)."
  (let ((r (gpoint "Select an edge of a solid: "
                   :drag (lambda (cur) (let ((ed (solid-edge-at cur)))
                                         (and ed (list (list :edge (second ed) (third ed)))))))))
    (when (pointp r)
      (let* ((ed (solid-edge-at r)) (faces (and ed (faces-at-edge (first ed) (second ed) (third ed)))))
        (if (null faces)
            (progn (out (format nil "No solid edge there.~%")) nil)
            (let ((i 0))
              (loop
                (setf *pp-face-choice* (nth i faces))
                (let ((k (get-user-input :keyword
                                         :prompt (format nil "Face ~D of ~D highlighted. [Next/Accept] <Accept>: " (1+ i) (length faces))
                                         :keywords '("Next" "Accept")
                                         :drag (lambda (cur) (declare (ignore cur)) (list (list :face (second *pp-face-choice*)))))))
                  (if (equal k "Next")
                      (setf i (mod (1+ i) (length faces)))
                      (return (nth i faces)))))))))))

(defvar *pp-drag-h* nil)

(defun signed-height (base normal cur)
  "The height along NORMAL from BASE that the cursor shows: how far it is along the normal's
direction on the screen (straight distance when the normal points at the viewer)."
  (let* ((frame (view-frame)) (nuv (screen-uv normal frame)) (l2 (+ (expt (px nuv) 2) (expt (py nuv) 2)))
         (dv (v- (screen-uv cur frame) (screen-uv base frame))))
    (if (< l2 0.04d0)
        (* (if (minusp (vdot3 normal (third frame))) -1 1) (dist2d (pt 0 0) dv))
        (/ (+ (* (px dv) (px nuv)) (* (py dv) (py nuv))) l2))))

(defun prism-preview (pts normal h)
  (let* ((off (v* (unit3 normal) h)) (top (mapcar (lambda (p) (v+ (pt3 p) off)) pts)))
    (append (outline-pieces top) (loop for p in pts for q in top collect (list :seg (pt3 p) q)))))

(defcmd "PRESSPULL" ("PRESS") "Click inside a closed area or on a flat face of a solid (or pick an Edge) and pull it into a solid."
  (setf *pp-drag-h* nil)
  (let* ((p (gpoint "Click inside bounded areas or on a face of a solid or [Edge]: " :keywords '("Edge")
                    ;; the face that a click would take lights up under the cursor
                    :drag (lambda (cur) (let ((f (ignore-errors (solid-face-at cur))))
                                          (and f (list (list :face (second f))))))))
         (picked (cond ((equal p "Edge") (pick-face-by-edge))
                       ((pointp p) (solid-face-at p)))))
    (when (or (pointp p) picked)
      (let* ((solid (first picked))
             (shape (and (not picked) (pointp p)
                         (let ((loops (ignore-errors (detect-boundary p))))
                           (and loops (mapcar (lambda (p2) (pt (px p2) (py p2) (pz (pt3 p))))
                                              (loop-polygon-directed (first loops) 24))))))
             (pts (if picked (second picked) shape))
             (normal (if picked (third picked) (pt 0 0 1))))
        (if (null pts)
            (out (format nil "No closed area or solid face there.~%"))
            (let* ((pts (if (< (dist (first pts) (car (last pts))) 1d-9) (butlast pts) pts))
                   (base (v* (reduce #'v+ (mapcar #'pt3 pts)) (/ 1d0 (length pts))))
                   (r (get-user-input :distance
                                      :prompt (format nil "Specify extrusion height (negative pushes in) <~A>: " (fmt-len *last-solid-height*))
                                      :base base
                                      :drag (lambda (cur)
                                              (let ((h (signed-height base normal cur)))
                                                (setf *pp-drag-h* h)
                                                (cons (list :face pts) (prism-preview pts normal h))))))
                   ;; a click gives the height the preview showed (negative pushes in); typed, as typed
                   (h (cond ((null r) *last-solid-height*)
                            ((and (realp r) *pp-drag-h* (pointp *cursor-world*)
                                  (< (abs (- r (dist2d base *cursor-world*))) 1d-9))
                             *pp-drag-h*)
                            ((realp r) (d r)))))
              (when (and h (> (abs h) 1d-12))
                (setf *last-solid-height* h)
                (let ((prism (extrude-mesh pts t (v* (unit3 normal) h))))
                  (if solid
                      ;; on a solid's face: pulling out adds to it, pushing in cuts into it
                      (replace-with-result (list solid)
                                           (boolean-meshes (if (minusp h) :subtract :union) (list (ent-data solid)) (list prism))
                                           (if (minusp h) "Push" "Pull"))
                      (add prism)))
                (out (format nil "Pulled ~A ~A.~%" (if solid "the face" "the area") (fmt-len h)))
                (when (gui-p) (gui-regen)))))))))
