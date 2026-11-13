;;;; snap3d.lisp — object snaps on solids and on circles lying in other planes.
;;;;
;;;; A solid is kept as a mesh of flat faces, so its round edges are many short straight ones and
;;;; it has no "circle" to snap to. Here the shapes are found again from the mesh itself:
;;;;   * round edges of flat faces (a cylinder's end, a hole through a flat face): Center and
;;;;     Quadrant snaps, and Center of any arc-shaped stretch of an edge (a slot's rounded end);
;;;;   * round walls (a cylinder's side, a hole bored through a round body): Center at both ends of
;;;;     the axis;
;;;;   * spheres: Center.
;;;; Circles and arcs drawn in another plane (say in the Front view, snapped to from Top or an
;;;; isometric view) get Center and Quadrant snaps the same way. It works for solids opened from
;;;; files too, since nothing has to be remembered about how the solid was made.

(in-package :fancy-pencil)

(defun circumcenter3 (a b c)
  "Center of the circle through 3D points A B C, or NIL if they are in a line."
  (let* ((ab (v- b a)) (ac (v- c a)) (n (cross3 ab ac)) (n2 (vdot3 n n)))
    (when (> n2 1d-24)
      (v+ a (v* (v+ (v* (cross3 n ab) (vdot3 ac ac)) (v* (cross3 ac n) (vdot3 ab ab))) (/ 1d0 (* 2 n2)))))))

(defun loop-is-circle (v)
  "(:circle center radius normal) when all the points of vector V lie on one circle."
  (let ((n (length v)))
    (when (>= n 8)
      (let ((c (circumcenter3 (aref v 0) (aref v (floor n 3)) (aref v (floor (* 2 n) 3)))))
        (when c
          (let* ((r (dist (aref v 0) c)) (nrm (newell-normal (coerce v 'list))) (tol (* 1d-4 (max r 1d-6))))
            (when (and nrm
                       (every (lambda (p) (and (< (abs (- (dist p c) r)) tol)
                                               (< (abs (vdot3 (unit3 nrm) (v- p c))) tol)))
                              v))
              (list :circle c r (unit3 nrm)))))))))

(defun loop-arcs (v)
  "(:arc center radius normal mid) for each stretch of 5 or more points of vector V on one circle."
  (let ((n (length v)) (out nil) (i 0))
    (loop while (< i n)
          do (let* ((c (circumcenter3 (aref v i) (aref v (mod (+ i 1) n)) (aref v (mod (+ i 2) n))))
                    (r (and c (dist (aref v i) c)))
                    (j (+ i 2)))
               (if (null c)
                   (incf i)
                   (progn
                     (loop while (and (< (- j i) (1- n))
                                      (< (abs (- (dist (aref v (mod (1+ j) n)) c) r)) (* 1d-4 (max r 1d-6))))
                           do (incf j))
                     (if (>= (- j i) 4)
                         (progn
                           (push (list :arc c r
                                       (unit3 (cross3 (v- (aref v (mod (1+ i) n)) (aref v i)) (v- (aref v (mod j n)) (aref v i))))
                                       (aref v (mod (floor (+ i j) 2) n)))
                                 out)
                           (setf i j))
                         (incf i))))))
    out))

(defun loop-round-features (pts)
  "Round features of closed loop PTS: the whole circle, or the arc-shaped stretches of it."
  (when (>= (length pts) 5)
    (let* ((v (coerce pts 'vector)) (whole (loop-is-circle v)))
      (if whole (list whole) (loop-arcs v)))))

(defun all-face-loops (group)
  "Every boundary loop of a group of coplanar faces (outer edge and holes)."
  (let ((dir (make-hash-table :test #'equal)) (pts (make-hash-table :test #'equal)))
    (dolist (f group)
      (loop for (a b) on (append (first f) (list (first (first f)))) while b
            do (let ((ka (weld-key a)) (kb (weld-key b)))
                 (setf (gethash ka pts) a (gethash kb pts) b)
                 (push kb (gethash ka dir)))))
    (let ((next (make-hash-table :test #'equal)) (loops nil) (used (make-hash-table :test #'equal)))
      (maphash (lambda (ka kbs) (dolist (kb kbs) (unless (member ka (gethash kb dir) :test #'equal) (setf (gethash ka next) kb)))) dir)
      (maphash (lambda (k v) (declare (ignore v))
                 (unless (gethash k used)
                   (let ((lp nil) (cur k))
                     (loop while (and cur (not (gethash cur used)) (< (length lp) 20000))
                           do (setf (gethash cur used) t) (push (gethash cur pts) lp) (setf cur (gethash cur next)))
                     (when (>= (length lp) 3) (push (nreverse lp) loops)))))
               next)
      loops)))

(defun planar-face-groups (faces)
  "FACES (points normal) grouped by plane, then by being joined at edges."
  (let ((by-plane (make-hash-table :test #'equal)) (out nil))
    (dolist (f faces)
      (destructuring-bind (ps n) f
        (let ((key (list (round (px n) 1d-4) (round (py n) 1d-4) (round (pz n) 1d-4) (round (vdot3 n (pt3 (first ps))) 1d-5))))
          (push f (gethash key by-plane)))))
    (maphash (lambda (k fs) (declare (ignore k))
               (let ((edge-faces (make-hash-table :test #'equal)) (seen (make-hash-table :test #'eq)))
                 (dolist (f fs)
                   (loop for (a b) on (append (first f) (list (first (first f)))) while b
                         do (push f (gethash (edge-key a b) edge-faces))))
                 (dolist (f fs)
                   (unless (gethash f seen)
                     (let ((group nil) (todo (list f)))
                       (setf (gethash f seen) t)
                       (loop while todo
                             do (let ((g (pop todo)))
                                  (push g group)
                                  (loop for (a b) on (append (first g) (list (first (first g)))) while b
                                        do (dolist (h (gethash (edge-key a b) edge-faces))
                                             (unless (gethash h seen) (setf (gethash h seen) t) (push h todo))))))
                       (push group out))))))
             by-plane)
    out))

(defun smooth-regions (data)
  "Faces of mesh DATA grouped where they meet without a visible edge (one curved surface each):
lists of (points normal)."
  (let* ((faces (mesh-display-faces data)) (edge-faces (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'eq)) (out nil))
    (dolist (f faces)
      (destructuring-bind (ps n vis) f
        (when n
          (loop for (a b) on (append ps (list (first ps))) while b for v in vis
                unless v do (push f (gethash (edge-key a b) edge-faces))))))
    (dolist (f faces)
      (when (and (second f) (not (gethash f seen)))
        (let ((group nil) (todo (list f)))
          (setf (gethash f seen) t)
          (loop while todo
                do (let ((g (pop todo)))
                     (push (list (first g) (unit3 (second g))) group)
                     (loop for (a b) on (append (first g) (list (first (first g)))) while b for v in (third g)
                           unless v do (dolist (h (gethash (edge-key a b) edge-faces))
                                         (unless (gethash h seen) (setf (gethash h seen) t) (push h todo))))))
          (when (>= (length group) 6) (push group out)))))
    out))

(defun fit-circle-2d (pts)
  "Least-squares circle through 2D points: (values center radius worst-error)."
  (let ((n (length pts)) (sx 0d0) (sy 0d0) (sxx 0d0) (syy 0d0) (sxy 0d0) (sz 0d0) (sxz 0d0) (syz 0d0))
    (dolist (p pts)
      (let* ((x (px p)) (y (py p)) (z (+ (* x x) (* y y))))
        (incf sx x) (incf sy y) (incf sxx (* x x)) (incf syy (* y y)) (incf sxy (* x y))
        (incf sz z) (incf sxz (* x z)) (incf syz (* y z))))
    ;; x^2 + y^2 + D x + E y + F = 0, solved by least squares (3x3 normal equations)
    (let* ((m (list (list sxx sxy sx) (list sxy syy sy) (list sx sy (d n))))
           (rhs (list (- sxz) (- syz) (- sz)))
           (sol (solve3 m rhs)))
      (when sol
        (destructuring-bind (dd ee ff) sol
          (let* ((c (pt (/ dd -2) (/ ee -2))) (r2 (- (+ (* (px c) (px c)) (* (py c) (py c))) ff)))
            (when (plusp r2)
              (let ((r (sqrt r2)))
                (values c r (reduce #'max (mapcar (lambda (p) (abs (- (dist2d p c) r))) pts)))))))))))

(defun solve3 (m b)
  "Solve the 3x3 linear system M x = B (Cramer); NIL if singular."
  (flet ((det (a) (destructuring-bind ((a1 a2 a3) (b1 b2 b3) (c1 c2 c3)) a
                    (+ (* a1 (- (* b2 c3) (* b3 c2))) (- (* a2 (- (* b1 c3) (* b3 c1)))) (* a3 (- (* b1 c2) (* b2 c1)))))))
    (let ((dm (det m)))
      (when (> (abs dm) 1d-18)
        (loop for k below 3
              collect (/ (det (loop for row in m for bv in b
                                    collect (loop for x in row for j from 0 collect (if (= j k) bv x))))
                         dm))))))

(defun region-axis-feature (region)
  "A round wall's axis ends (:axis p0 p1 radius) or a sphere's centre (:sphere centre radius), or NIL."
  (let* ((normals (mapcar #'second region))
         (pts (remove-duplicates (loop for f in region append (first f)) :test #'equalp))
         (tensor (let ((m (make-array '(3 3) :initial-element 0d0)))
                   (dolist (n normals)
                     (let ((c (list (px n) (py n) (pz n))))
                       (dotimes (i 3) (dotimes (j 3) (incf (aref m i j) (* (nth i c) (nth j c)))))))
                   (loop for i below 3 collect (loop for j below 3 collect (aref m i j)))))
         (axis (second (first (jacobi-eigen tensor)))))
    (cond
      ;; a cylinder: every face normal square to one direction
      ((every (lambda (n) (< (abs (vdot3 n axis)) 0.02d0)) normals)
       (multiple-value-bind (ax ay) (ocs-axes (unit3 axis))
         (let ((flat (mapcar (lambda (p) (pt (vdot3 p ax) (vdot3 p ay))) pts)))
           (multiple-value-bind (c r err) (fit-circle-2d flat)
             (when (and c (< err (* 2d-3 r)))
               (let* ((ts (mapcar (lambda (p) (vdot3 p axis)) pts))
                      (base (v+ (v* ax (px c)) (v* ay (py c)))))
                 (list :axis (v+ base (v* axis (reduce #'min ts))) (v+ base (v* axis (reduce #'max ts))) r)))))))
      ;; a sphere: every face normal through one point
      (t
       (let ((a (make-array '(3 3) :initial-element 0d0)) (b (list 0d0 0d0 0d0)))
         (dolist (f region)
           (let* ((n (second f)) (p (reduce #'v+ (first f))) (p (v* p (/ 1d0 (length (first f)))))
                  (nc (list (px n) (py n) (pz n))) (pc (list (px p) (py p) (pz p))))
             (dotimes (i 3)
               (dotimes (j 3)
                 (let ((mij (- (if (= i j) 1d0 0d0) (* (nth i nc) (nth j nc)))))
                   (incf (aref a i j) mij)
                   (incf (nth i b) (* mij (nth j pc))))))))
         (let ((sol (solve3 (loop for i below 3 collect (loop for j below 3 collect (aref a i j))) b)))
           (when sol
             (let* ((c0 (pt (first sol) (second sol) (third sol)))
                    ;; exact centre: the sphere fitted to the surface's own points
                    (c (or (fit-sphere pts) c0))
                    (ds (mapcar (lambda (p) (dist p c)) pts)) (r (/ (reduce #'+ ds) (length ds))))
               (when (and (> r 1d-9) (every (lambda (x) (< (abs (- x r)) (* 0.03d0 r))) ds)
                          (> (length region) 20))
                 (list :sphere c r))))))))))

(defun solve-n (m b)
  "Solve the linear system M x = B (lists) by Gaussian elimination; NIL if singular."
  (let* ((n (length b)) (a (make-array (list n (1+ n)))))
    (loop for row in m for bv in b for i from 0
          do (loop for x in row for j from 0 do (setf (aref a i j) (d x))) (setf (aref a i n) (d bv)))
    (dotimes (k n)
      (let ((piv (loop with best = k for i from k below n
                       when (> (abs (aref a i k)) (abs (aref a best k))) do (setf best i) finally (return best))))
        (when (< (abs (aref a piv k)) 1d-14) (return-from solve-n nil))
        (dotimes (j (1+ n)) (rotatef (aref a k j) (aref a piv j)))
        (loop for i from 0 below n unless (= i k)
              do (let ((f (/ (aref a i k) (aref a k k))))
                   (loop for j from k to n do (decf (aref a i j) (* f (aref a k j))))))))
    (loop for i below n collect (/ (aref a i n) (aref a i i)))))

(defun fit-sphere (pts)
  "Least-squares sphere through PTS: its centre, or NIL."
  (let ((m (make-array '(4 4) :initial-element 0d0)) (b (make-array 4 :initial-element 0d0)))
    ;; x^2+y^2+z^2 + D x + E y + F z + G = 0
    (dolist (p pts)
      (let ((row (list (px p) (py p) (pz p) 1d0)) (rhs (- (vdot3 p p))))
        (dotimes (i 4)
          (incf (aref b i) (* (nth i row) rhs))
          (dotimes (j 4) (incf (aref m i j) (* (nth i row) (nth j row)))))))
    (let ((sol (solve-n (loop for i below 4 collect (loop for j below 4 collect (aref m i j)))
                        (loop for i below 4 collect (aref b i)))))
      (and sol (pt (/ (first sol) -2) (/ (second sol) -2) (/ (third sol) -2))))))

(defun sphere-through (ps)
  "Centre of the sphere through four points, or NIL."
  (destructuring-bind (p0 p1 p2 p3) ps
    (let ((sol (solve3 (mapcar (lambda (q) (let ((dv (v* (v- q p0) 2d0))) (list (px dv) (py dv) (pz dv)))) (list p1 p2 p3))
                       (mapcar (lambda (q) (- (vdot3 q q) (vdot3 p0 p0))) (list p1 p2 p3)))))
      (and sol (pt (first sol) (second sol) (third sol))))))

(defvar *round-cache* (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key))

(defun mesh-round-features (data)
  "Round features of solid DATA (see the top of this file), cached per mesh."
  (or (gethash data *round-cache*)
      (setf (gethash data *round-cache*)
            (let ((out nil))
              (ignore-errors
               (dolist (g (planar-face-groups (oriented-faces data)))
                 (dolist (lp (all-face-loops g))
                   ;; (points added on straight edges by the boolean operations are dropped first)
                   (setf out (append (loop-round-features (simplify-loop lp)) out)))))
              (ignore-errors
               (dolist (r (smooth-regions data))
                 (let ((f (region-axis-feature r))) (when f (push f out)))))
              ;; the same shape found twice (a wall split in two by a hole) counts once
              (setf out (remove-duplicates out :test (lambda (a b) (and (eq (first a) (first b))
                                                                         (equalp (mapcar #'round-feature-key (rest a))
                                                                                 (mapcar #'round-feature-key (rest b)))))))
              (or out (list :none))))))

(defun round-feature-key (x)
  (cond ((realp x) (round x 1d-6)) ((consp x) (mapcar #'round-feature-key x)) (t x)))

(defun data-round-features (data)
  "Round features of an object for snapping in 3D: solids, and circles/arcs in another plane."
  (let ((type (dxf 0 data)))
    (cond ((equal type "POLYFACE") (remove :none (mesh-round-features data)))
          ((and (member type '("CIRCLE" "ARC") :test #'equal) (offplane-p data))
           (let* ((n (unit3 (entity-normal data))))
             (multiple-value-bind (ax ay az) (ocs-axes n)
               (let ((c (ocs->w (pt3 (dxf 10 data)) ax ay az)) (r (d (dxf 40 data))))
                 (if (equal type "CIRCLE")
                     (list (list :circle c r n))
                     (list (list :arc c r n (ocs->w (let ((a1 (dxf 50 data)) (a2 (dxf 51 data)))
                                                      (pt3 (polar (pt3 (dxf 10 data)) (+ a1 (/ (arc-sweep a1 a2) 2)) r)))
                                                    ax ay az))))))))
          (t nil))))

(defun round-feature-snaps (f)
  "(point . bit) snaps of round feature F: centres (4), quadrants (16), arc midpoints (2)."
  (case (first f)
    (:circle (destructuring-bind (c r n) (rest f)
               (multiple-value-bind (ax ay) (ocs-axes n)
                 (cons (cons c 4)
                       (list (cons (v+ c (v* ax r)) 16) (cons (v- c (v* ax r)) 16)
                             (cons (v+ c (v* ay r)) 16) (cons (v- c (v* ay r)) 16))))))
    (:arc (destructuring-bind (c r n mid) (rest f) (declare (ignore r n)) (list (cons c 4) (cons mid 2))))
    (:axis (destructuring-bind (p0 p1 r) (rest f) (declare (ignore r)) (list (cons p0 4) (cons p1 4))))
    (:sphere (list (cons (second f) 4)))))

(defun round-feature-ring (f)
  "Points along F where the cursor counts as 'on' it (its edge, or its axis and ends)."
  (case (first f)
    ((:circle :arc) (destructuring-bind (c r n &rest ignore) (rest f)
                      (declare (ignore ignore))
                      (multiple-value-bind (ax ay) (ocs-axes n)
                        (loop for i below 48 for a = (* +2pi+ (/ i 48))
                              collect (v+ c (v+ (v* ax (* r (cos a))) (v* ay (* r (sin a)))))))))
    (:axis (destructuring-bind (p0 p1 r) (rest f)
             (let* ((ax (unit3 (v- p1 p0))))
               (multiple-value-bind (ux uy) (ocs-axes ax)
                 (loop for p in (list p0 p1)
                       append (loop for i below 32 for a = (* +2pi+ (/ i 32))
                                    collect (v+ p (v+ (v* ux (* r (cos a))) (v* uy (* r (sin a)))))))))))
    (:sphere (destructuring-bind (c r) (rest f)
               (loop for i below 48 for a = (* +2pi+ (/ i 48))
                     append (let ((fr (if (projected-view-p) (list (view-r) (view-up)) (list (pt 1 0 0) (pt 0 1 0)))))
                              (list (v+ c (v+ (v* (first fr) (* r (cos a))) (v* (second fr) (* r (sin a))))))))))))

(defun on-round-shape-p (q shape)
  "True when Q lies on SHAPE from *RING-CIRCLES*: a round edge (:circle c r n) or a round wall (:cyl p0 p1 r)."
  (let ((q (pt3 q)))
    (ecase (first shape)
      (:circle (destructuring-bind (c r n) (rest shape)
                 (let ((dv (v- q c)))
                   (and (< (abs (vdot3 dv n)) (* 1d-6 (max r 1d0)))
                        (< (abs (- (dist q c) r)) (* 0.02d0 r))))))
      (:cyl (destructuring-bind (p0 p1 r) (rest shape)
              (let* ((ax (v- p1 p0)) (len (dist p0 p1)) (u (if (> len 1d-12) (v* ax (/ 1 len)) ax))
                     (tt (vdot3 (v- q p0) u)) (foot (v+ p0 (v* u tt))))
                (and (< (* -0.5 r) tt (+ len (* 0.5 r)))
                     (< (abs (- (dist q foot) r)) (* 0.02d0 r)))))))))

(defun round-snap-candidates (e p bits tol to-screen)
  "Snaps from E's round features when the cursor P is near one of them. TO-SCREEN maps a point to
the 2D place distances are measured in (the plane in flat views, the screen in isometric ones)."
  (when (logtest bits (logior 2 4 16))
    (let ((q (funcall to-screen p)) (out nil))
      (labels ((sc (w) (funcall to-screen w))
               (on-ring (ring) (some (lambda (a b) (<= (dist-point-segment-2d q (sc a) (sc b)) tol))
                                     ring (append (rest ring) (list (first ring)))))
               (take (pt bit) (when (logtest bits bit)
                                (let ((c (cons pt bit)))
                                  (when (= bit 4) (push c *ring-centers*))
                                  (push c out)))))
        (dolist (f (data-round-features (ent-data e)) out)
          (case (first f)
            ((:circle :arc)
             (let ((snaps (round-feature-snaps f)) (ring (round-feature-ring f)))
               (when (or (on-ring ring) (some (lambda (s) (<= (dist2d (sc (car s)) q) tol)) snaps))
                 (push (list :circle (second f) (third f) (fourth f)) *ring-circles*)
                 (dolist (s snaps)
                   ;; the centre counts once you are on the edge; quadrants and midpoints when close
                   (when (or (= (cdr s) 4) (<= (dist2d (sc (car s)) q) tol)) (take (car s) (cdr s)))))))
            (:axis
             ;; a round wall (a hole, a cylinder's side): the centre of an end when the cursor is
             ;; over that end of it
             (destructuring-bind (p0 p1 r) (rest f)
               (let ((ax (unit3 (v- p1 p0))) (hit nil))
                 (dolist (end (list p0 p1))
                   (let* ((ring (round-feature-ring (list :circle end r ax)))
                          (rs (reduce #'max (mapcar (lambda (w) (dist2d (sc w) (sc end))) ring))))
                     (when (or (on-ring ring) (<= (dist2d q (sc end)) (+ (* 1.15d0 rs) tol)))
                       (setf hit t)
                       (take end 4))))
                 (when hit (push (list :cyl p0 p1 r) *ring-circles*)))))
            (:sphere
             (let ((ring (round-feature-ring f)))
               (when (or (on-ring ring) (<= (dist2d (sc (second f)) q) tol))
                 (take (second f) 4))))))))))

;;; ---------- keeping the shapes solids are made from ----------

(defcmd "KEEPPROFILES" () "Keeps (On) or removes (Off) the circles, polylines and paths that EXTRUDE, REVOLVE, SWEEP and LOFT use (DELOBJ)."
  (let ((r (gkw (format nil "Keep the shapes solids are made from? [On/Off] <~:[Off~;On~]>: " (eql (sysvar "DELOBJ") 0))
                '("On" "Off") (if (eql (sysvar "DELOBJ") 0) "On" "Off"))))
    (setf (sysvar "DELOBJ") (if (equal r "On") 0 3))
    (save-prefs)
    (when (gui-p) (tk "set ::keepprof ~D" (if (eql (sysvar "DELOBJ") 0) 1 0)))
    (out (format nil "~:[Shapes are removed once a solid is made from them~;Shapes are kept after a solid is made from them (DELOBJ 0)~].~%"
                 (eql (sysvar "DELOBJ") 0)))))

;;; ---------------------------------------------------------------- snaps on faces, and in 3D
;;; Besides its edges, a solid's faces snap: CENter finds the middle of the flat face under the
;;; cursor, NEArest the point on the face under the cursor (when no edge is near), so you can
;;; draw straight onto a solid. In the 3D views INTersection finds where edges really meet in
;;; space, and PERpendicular the foot of the perpendicular on the edge under the cursor.

(defvar *mesh-faces-cache* (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key))

(defun mesh-snap-faces (data)
  "Faces of solid DATA for snapping: ((centre-or-NIL triangles) ...), CENTRE (the middle of the
face's area) only for flat faces. TRIANGLES are lists of three points."
  (or (gethash data *mesh-faces-cache*)
      (setf (gethash data *mesh-faces-cache*)
            (loop for g in (ignore-errors (mesh-face-groups data))
                  collect (let* ((tris (loop for f in g
                                             for ps = (mesh-face-points data f)
                                             append (loop for i from 1 below (1- (length ps))
                                                          collect (list (first ps) (nth i ps) (nth (1+ i) ps)))))
                                 (area 0d0) (c (pt 0 0 0)))
                            (when (facets-flat-p data g)
                              (dolist (tr tris)
                                (let ((a (* 0.5d0 (vlen3 (cross3 (v- (second tr) (first tr)) (v- (third tr) (first tr)))))))
                                  (incf area a)
                                  (setf c (v+ c (v* (v+ (v+ (first tr) (second tr)) (third tr)) (/ a 3)))))))
                            (list (and (> area 1d-12) (v* c (/ 1d0 area))) tris))))))

(defun ray-triangle (o dir tr)
  "Where the line through O along DIR meets triangle TR, or NIL."
  (destructuring-bind (a b c) tr
    (let* ((n (cross3 (v- b a) (v- c a))) (den (vdot3 n dir)))
      (when (> (abs den) 1d-14)
        (let* ((x (v+ o (v* dir (/ (vdot3 n (v- a o)) den)))))
          (flet ((side (p q) (vdot3 n (cross3 (v- q p) (v- x p)))))
            (let ((s1 (side a b)) (s2 (side b c)) (s3 (side c a)) (eps (* -1d-12 (vdot3 n n))))
              (when (or (and (>= s1 eps) (>= s2 eps) (>= s3 eps)) (and (<= s1 (- eps)) (<= s2 (- eps)) (<= s3 (- eps))))
                x))))))))

(defun view-ray (p)
  "The line of sight through the point under the cursor P: (values origin toward-viewer)."
  (if (projected-view-p)
      (let ((q (w->uv p))) (values (view-unproject (px q) (py q)) (view-dir)))
      (values (pt (px p) (py p) 0) (pt 0 0 1))))

(defun face-under-cursor (e p)
  "(values point centre) where the line of sight through P first meets solid E: the point on its
nearest face, and that face's centre if it is flat."
  (multiple-value-bind (o dir) (view-ray p)
    (let ((best nil) (bc nil) (bd nil))
      (dolist (f (mesh-snap-faces (ent-data e)))
        (dolist (tr (second f))
          (let ((x (ray-triangle o dir tr)))
            (when x
              (let ((dp (vdot3 x dir)))
                (when (or (null bd) (> dp bd)) (setf best x bc (first f) bd dp)))))))
      (values best bc))))

(defun face-snap-candidates (ents p bits have-nearest)
  "CENter and NEArest snaps on the faces of the solids in ENTS under the cursor P."
  (let ((cands nil) (best nil) (bc nil) (bd nil))
    (when (logtest bits (logior 4 512))
      (multiple-value-bind (o dir) (view-ray p)
        (declare (ignore o))
        (dolist (e ents)
          (when (equal (ent-type e) "POLYFACE")
            (multiple-value-bind (x c) (face-under-cursor e p)
              (when x
                (let ((dp (vdot3 x dir)))
                  (when (or (null bd) (> dp bd)) (setf best x bc c bd dp)))))))))
    (when (and best (logtest bits 512) (not have-nearest)) (push (cons best 512) cands))
    (when (and bc (logtest bits 4))
      (let ((cand (cons bc 4))) (push cand *face-centers*) (push cand cands)))
    cands))

(defun seg-seg-3d (a b c d)
  "Where segments AB and CD meet in space, or NIL."
  (let* ((u (v- b a)) (v (v- d c)) (w (v- a c))
         (uu (vdot3 u u)) (uv (vdot3 u v)) (vv (vdot3 v v)) (uw (vdot3 u w)) (vw (vdot3 v w))
         (den (- (* uu vv) (* uv uv))))
    (when (and (> uu 1d-18) (> vv 1d-18) (> den (* 1d-12 uu vv)))
      (let ((s (/ (- (* uv vw) (* vv uw)) den)) (tt (/ (- (* uu vw) (* uv uw)) den)))
        (when (and (<= -1d-9 s (+ 1 1d-9)) (<= -1d-9 tt (+ 1 1d-9)))
          (let ((p (v+ a (v* u s))) (q (v+ c (v* v tt))))
            (when (< (dist p q) (* 1d-7 (max 1d0 (sqrt uu) (sqrt vv))))
              (v* (v+ p q) 0.5d0))))))))

(defun entity-snap-segs-3d (e)
  "E's straight edges as 3D segments (a b), at the heights the 3D views draw them."
  (multiple-value-bind (elev th) (view-object-height (ent-data e))
    (loop for pc in (snap-pieces e)
          when (eq (first pc) :seg)
            append (let ((a (pt3 (second pc))) (b (pt3 (third pc))))
                     (loop for dz in (if (zerop th) '(0d0) (list 0d0 th))
                           collect (list (lift a (+ (pz a) elev dz)) (lift b (+ (pz b) elev dz))))))))

(defun seg-face-candidates (ents segs q tol)
  "INTersection snaps where SEGS (3D segments (a b) of other objects) pass through the faces of
the solids among ENTS, near view point Q."
  (let ((cands nil) (solids (remove-if-not (lambda (e) (equal (ent-type e) "POLYFACE")) ents)))
    (dolist (sol solids cands)
      (let ((own (entity-snap-segs-3d sol)))
        (dolist (s segs)
          (unless (member s own :test #'equal)
            (let* ((a (first s)) (dir (v- (second s) a)) (ll (vdot3 dir dir)))
              (when (> ll 1d-18)
                (dolist (f (mesh-snap-faces (ent-data sol)))
                  (dolist (tr (second f))
                    (let ((x (ray-triangle a dir tr)))
                      (when x
                        (let ((k (/ (vdot3 (v- x a) dir) ll)))
                          (when (and (<= -1d-9 k (+ 1 1d-9)) (<= (dist2d (w->uv x) q) tol))
                            (push (cons x 32) cands)))))))))))))))

(defun view-int-perp-candidates (ents p bits base tol)
  "INTersection (edges meeting in space) and PERpendicular snaps in a 3D view."
  (let* ((q (w->uv p))
         (segs (loop for e in ents
                     append (loop for s in (entity-snap-segs-3d e)
                                  when (<= (piece-dist (list :seg (w->uv (first s)) (w->uv (second s))) q) tol)
                                    collect s)))
         (cands nil))
    (when (logtest bits 32)
      (loop for (s . rest) on segs
            do (dolist (s2 rest)
                 (let ((x (seg-seg-3d (first s) (second s) (first s2) (second s2))))
                   (when (and x (<= (dist2d (w->uv x) q) tol)) (push (cons x 32) cands))))))
    ;; a line passing through the face of a solid
    (when (logtest bits 32)
      (setf cands (append (seg-face-candidates ents segs q tol) cands)))
    (when (and base (logtest bits 128))
      (dolist (s segs)
        (let* ((a (first s)) (u (v- (second s) a)) (uu (vdot3 u u)))
          (when (> uu 1d-18)
            (let ((k (/ (vdot3 (v- (pt3 base) a) u) uu)))
              (when (<= 0 k 1) (push (cons (v+ a (v* u k)) 128) cands)))))))
    cands))
