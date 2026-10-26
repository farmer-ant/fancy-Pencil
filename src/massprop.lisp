;;;; massprop.lisp — MASSPROP: mass properties of regions and solids, as in AutoCAD.
;;;;
;;;; Regions are closed flat objects: closed polylines (arcs included), circles, ellipses and closed
;;;; splines. Several are added together; a closed object lying inside another selected one is a
;;;; hole and is taken away (a plate with holes). Solids are closed 3D solid meshes, density 1, so
;;;; their mass equals their volume. Values are in the working plane's coordinates (world in Top).
;;;; Curved edges are summed exactly enough to agree with the true values to well beyond the shown
;;;; precision (two subdivisions combined by Richardson extrapolation).

(in-package :fancy-pencil)

;;; ---------- regions ----------

(defun region-loop (data m)
  "Closed boundary of flat object DATA as a list of points (curves subdivided, M = 1 or 2 times as
finely), or NIL if DATA is not a closed flat shape."
  (let ((type (dxf 0 data)) (k 64))
    (when (offplane-p data) (return-from region-loop nil))
    (cond
      ((equal type "CIRCLE")
       (let ((c (pt3 (dxf 10 data))) (r (d (dxf 40 data))) (n (* m 4 k)))
         (loop for i below n collect (polar c (* +2pi+ (/ i n)) r))))
      ((equal type "ELLIPSE")
       (let ((t0 (or (dxf 41 data) 0d0)) (t1 (or (dxf 42 data) +2pi+)))
         (when (< (abs (- (norm-ang (- t1 t0)) 0)) 1d-9)        ; full ellipses only
           (butlast (ellipse-points data (* m 4 k))))))
      ((equal type "LWPOLYLINE")
       (let* ((vs (lwpoly-vertices data)) (n (length vs))
              (closed (or (lwpoly-closed-p data)
                          (and (> n 2) (< (dist2d (first (first vs)) (first (car (last vs)))) 1d-9)))))
         (when (and closed (>= n 2))
           (when (and (not (lwpoly-closed-p data)) (< (dist2d (first (first vs)) (first (car (last vs)))) 1d-9))
             (setf vs (butlast vs) n (length vs)))
           (loop for i below n
                 for (p b) = (nth i vs) for q = (first (nth (mod (1+ i) n) vs))
                 append (if (or (zerop b) (< (dist2d p q) 1d-12))
                            (list (pt (px p) (py p)))
                            (multiple-value-bind (c) (bulge->arc p q b)
                              (let* ((theta (* 4 (atan b))) (a0 (ang c p)) (r (dist2d c p))
                                     (steps (* m (max 2 (ceiling (* k (abs theta)) +2pi+)))))
                                (loop for j below steps collect (polar c (+ a0 (* theta (/ j steps))) r)))))))))
      ((equal type "SPLINE")
       (let ((pts (spline-points data)))
         (when (and (> (length pts) 3) (or (logtest 1 (or (dxf 70 data) 0)) (< (dist2d (first pts) (car (last pts))) 1d-9)))
           (butlast pts))))
      (t nil))))

(defun loop-integrals (pts)
  "Signed (area sx sy ixx iyy ixy perimeter) of closed polygon PTS: area, first moments
(integrals of x and y), second moments (integrals of y^2, x^2 and xy) and its length."
  (let ((a 0d0) (sx 0d0) (sy 0d0) (ixx 0d0) (iyy 0d0) (ixy 0d0) (per 0d0) (n (length pts)))
    (loop for i below n
          for p = (nth i pts) for q = (nth (mod (1+ i) n) pts)
          for x0 = (px p) for y0 = (py p) for x1 = (px q) for y1 = (py q)
          for cr = (- (* x0 y1) (* x1 y0))
          do (incf a cr)
             (incf sx (* (+ x0 x1) cr)) (incf sy (* (+ y0 y1) cr))
             (incf ixx (* (+ (* y0 y0) (* y0 y1) (* y1 y1)) cr))
             (incf iyy (* (+ (* x0 x0) (* x0 x1) (* x1 x1)) cr))
             (incf ixy (* (+ (* x0 y1) (* 2 x0 y0) (* 2 x1 y1) (* x1 y0)) cr))
             (incf per (dist2d p q)))
    (list (/ a 2) (/ sx 6) (/ sy 6) (/ ixx 12) (/ iyy 12) (/ ixy 24) per)))

(defun richardson (f1 f2) (mapcar (lambda (a b) (/ (- (* 4 b) a) 3)) f1 f2))

(defun region-properties (datas)
  "Summed properties of the closed flat objects DATAS (holes taken away). Returns a plist or NIL."
  (let* ((loops (remove nil (mapcar (lambda (d) (let ((l1 (region-loop d 1))) (and l1 (list d l1)))) datas)))
         (areas (mapcar (lambda (x) (abs (first (loop-integrals (second x))))) loops)))
    (when loops
      (let ((tot (make-list 7 :initial-element 0d0)) (pts-all nil) (holes 0))
        (loop for (d l1) in loops for ai in areas
              do (let* ((depth (loop for (d2 l2) in loops for aj in areas
                                     count (and (not (eq d d2)) (> aj ai) (point-in-polygon-p (first l1) l2))))
                        (sgn (if (evenp depth) 1 -1))
                        (f (richardson (loop-integrals l1) (loop-integrals (region-loop d 2))))
                        ;; every loop counted counterclockwise, then added or (a hole) taken away
                        (orient (if (minusp (first f)) -1 1)))
                   (when (oddp depth) (incf holes))
                   (setf pts-all (append pts-all l1))
                   (setf tot (mapcar #'+ tot
                                     (append (mapcar (lambda (v) (* sgn orient v)) (butlast f))
                                             (list (car (last f))))))))
        (destructuring-bind (a sx sy ixx iyy ixy per) tot
          (let* ((cx (/ sx a)) (cy (/ sy a))
                 ;; about the centroid
                 (icx (- ixx (* a cy cy))) (icy (- iyy (* a cx cx))) (icxy (- ixy (* a cx cy)))
                 ;; principal axes: the moment about axis u is u.T.u for T = [[Ix -Pxy] [-Pxy Iy]]
                 (principal (mapcar (lambda (ev) (list (first ev) (pt (px (second ev)) (py (second ev)))))
                                    (remove-if (lambda (ev) (> (abs (pz (second ev))) 0.5d0))
                                               (jacobi-eigen (list (list icx (- icxy) 0d0) (list (- icxy) icy 0d0) (list 0d0 0d0 0d0))))))
                 (b (points-bbox pts-all)))
            (list :area a :perimeter per :bbox b :centroid (pt cx cy)
                  :ix ixx :iy iyy :ixy ixy :rx (sqrt (/ (abs ixx) a)) :ry (sqrt (/ (abs iyy) a))
                  :principal principal
                  :count (length loops) :holes holes)))))))

;;; ---------- solids ----------

(defun jacobi-eigen (m)
  "Eigenvalues and unit eigenvectors of symmetric 3x3 M (list of rows): ((value vector) ...)."
  (let ((a (make-array '(3 3))) (v (make-array '(3 3) :initial-element 0d0)))
    (dotimes (i 3) (dotimes (j 3) (setf (aref a i j) (d (nth j (nth i m))))) (setf (aref v i i) 1d0))
    (loop with scale = (max 1d-300 (abs (aref a 0 0)) (abs (aref a 1 1)) (abs (aref a 2 2)))
          repeat 60
          do (let ((p 0) (q 1) (big 0d0))
               (loop for i below 3 do (loop for j from (1+ i) below 3
                                            do (when (> (abs (aref a i j)) big) (setf big (abs (aref a i j)) p i q j))))
               (when (< big (* 1d-12 scale)) (return))
               (let* ((th (/ (- (aref a q q) (aref a p p)) (* 2 (aref a p q))))
                      (tt (/ (if (minusp th) -1 1) (+ (abs th) (sqrt (1+ (* th th))))))
                      (c (/ 1 (sqrt (1+ (* tt tt))))) (s (* tt c)))
                 (dotimes (k 3)
                   (let ((akp (aref a k p)) (akq (aref a k q)))
                     (setf (aref a k p) (- (* c akp) (* s akq)) (aref a k q) (+ (* s akp) (* c akq)))))
                 (dotimes (k 3)
                   (let ((apk (aref a p k)) (aqk (aref a q k)))
                     (setf (aref a p k) (- (* c apk) (* s aqk)) (aref a q k) (+ (* s apk) (* c aqk)))))
                 (dotimes (k 3)
                   (let ((vkp (aref v k p)) (vkq (aref v k q)))
                     (setf (aref v k p) (- (* c vkp) (* s vkq)) (aref v k q) (+ (* s vkp) (* c vkq))))))))
    (tidy-axes (sort (loop for i below 3 collect (list (aref a i i) (pt (aref v 0 i) (aref v 1 i) (aref v 2 i)))) #'< :key #'first))))

(defun tidy-axes (evs)
  "Equal moments leave the directions free: show them along the world axes when that fits."
  (let* ((scale (max 1d-300 (reduce #'max (mapcar (lambda (e) (abs (first e))) evs))))
         (axes (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1)))
         (near-axis (lambda (v) (find-if (lambda (ax) (> (abs (vdot3 v ax)) (- 1 1d-9))) axes))))
    (flet ((same (a b) (< (abs (- (first a) (first b))) (* 1d-9 scale))))
      (cond ((and (same (first evs) (second evs)) (same (second evs) (third evs)))
             (mapcar (lambda (e ax) (list (first e) ax)) evs axes))
            (t (loop for (i j k) in '((0 1 2) (1 2 0) (0 2 1))
                     do (when (and (same (nth i evs) (nth j evs)) (funcall near-axis (second (nth k evs))))
                          (let* ((kax (funcall near-axis (second (nth k evs))))
                                 (rest (remove kax axes :test #'equalp)))
                            (return (let ((out (copy-list evs)))
                                      (setf (nth i out) (list (first (nth i evs)) (first rest))
                                            (nth j out) (list (first (nth j evs)) (second rest))
                                            (nth k out) (list (first (nth k evs)) kax))
                                      out))))
                     finally (return evs)))))))

(defun solid-properties (datas)
  "Summed properties of closed solid meshes DATAS (density 1). Returns a plist or NIL."
  (let ((vol 0d0) (s (list 0d0 0d0 0d0)) (jj (make-array '(3 3) :initial-element 0d0)) (pts nil) (n 0))
    (dolist (data datas)
      (when (and (equal (dxf 0 data) "POLYFACE") (fourth (multiple-value-list (mesh-orientation data))))
        (incf n)
        (setf pts (append pts (mapcar #'pt3 (mesh-verts data))))
        (dolist (cp (mesh-polygons data))
          (loop with poly = (cpoly-verts cp)
                for i from 1 below (1- (length poly))
                for a = (pt3 (first poly)) for b = (pt3 (nth i poly)) for c = (pt3 (nth (1+ i) poly))
                do (let* ((d6 (vdot3 a (cross3 b c))) (v (/ d6 6))
                          (sum (v+ a (v+ b c))))
                     (incf vol v)
                     (setf s (mapcar (lambda (acc k) (+ acc (* v (/ k 4)))) s (list (px sum) (py sum) (pz sum))))
                     ;; integral of x_i x_j over the tetrahedron (0 a b c)
                     (dotimes (i 3)
                       (dotimes (j 3)
                         (let ((ci (lambda (p) (nth i (list (px p) (py p) (pz p)))))
                               (cj (lambda (p) (nth j (list (px p) (py p) (pz p))))))
                           (incf (aref jj i j)
                                 (* (/ v 20)
                                    (+ (loop for p in (list a b c) sum (* (funcall ci p) (funcall cj p)))
                                       (* (funcall ci sum) (funcall cj sum)))))))))))))
    (when (and (plusp n) (> (abs vol) 1d-12))
      (let* ((m vol) (c (mapcar (lambda (x) (/ x m)) s))
             (xx (aref jj 0 0)) (yy (aref jj 1 1)) (zz (aref jj 2 2))
             (xy (aref jj 0 1)) (yz (aref jj 1 2)) (zx (aref jj 2 0))
             (ix (+ yy zz)) (iy (+ zz xx)) (iz (+ xx yy))
             (cx (first c)) (cy (second c)) (cz (third c))
             ;; inertia tensor about the centroid
             (cxx (- xx (* m cx cx))) (cyy (- yy (* m cy cy))) (czz (- zz (* m cz cz)))
             (cxy (- xy (* m cx cy))) (cyz (- yz (* m cy cz))) (czx (- zx (* m cz cx)))
             (tensor (list (list (+ cyy czz) (- cxy) (- czx))
                           (list (- cxy) (+ czz cxx) (- cyz))
                           (list (- czx) (- cyz) (+ cxx cyy)))))
        (list :mass m :volume m :bbox3 (points-bbox3 pts) :centroid (pt cx cy cz)
              :ix ix :iy iy :iz iz :pxy xy :pyz yz :pzx zx
              :rx (sqrt (/ ix m)) :ry (sqrt (/ iy m)) :rz (sqrt (/ iz m))
              :principal (jacobi-eigen tensor) :count n)))))

(defun points-bbox3 (pts)
  (list (reduce #'min pts :key #'px) (reduce #'min pts :key #'py) (reduce #'min pts :key #'pz)
        (reduce #'max pts :key #'px) (reduce #'max pts :key #'py) (reduce #'max pts :key #'pz)))

;;; ---------- report ----------

(defun mp-num (x)
  (let ((v (if (< (abs x) (* 1d-12 1)) 0d0 x)))
    (fmt-decimal v (max 4 (or (sysvar "LUPREC") 4)))))

(defun region-report (r)
  (with-output-to-string (o)
    (format o "----------------   REGIONS   ----------------~%~%")
    (when (> (getf r :count) 1)
      (format o "~D closed objects~@[ (~D inside another, taken away as a hole~:P)~]~%~%"
              (getf r :count) (and (plusp (getf r :holes)) (getf r :holes))))
    (let ((b (getf r :bbox)) (c (getf r :centroid)))
      (format o "Area:                    ~A~%" (mp-num (getf r :area)))
      (format o "Perimeter:               ~A~%" (mp-num (getf r :perimeter)))
      (format o "Bounding box:         X: ~A  --  ~A~%" (mp-num (first b)) (mp-num (third b)))
      (format o "                      Y: ~A  --  ~A~%" (mp-num (second b)) (mp-num (fourth b)))
      (format o "Centroid:             X: ~A~%" (mp-num (px c)))
      (format o "                      Y: ~A~%" (mp-num (py c)))
      (format o "Moments of inertia:   X: ~A~%" (mp-num (getf r :ix)))
      (format o "                      Y: ~A~%" (mp-num (getf r :iy)))
      (format o "Product of inertia:  XY: ~A~%" (mp-num (getf r :ixy)))
      (format o "Radii of gyration:    X: ~A~%" (mp-num (getf r :rx)))
      (format o "                      Y: ~A~%" (mp-num (getf r :ry)))
      (format o "Principal moments and X-Y directions about centroid:~%")
      (loop for (val dir) in (getf r :principal) for name in '("I" "J")
            do (format o "                      ~A: ~A along [~A ~A]~%" name (mp-num val) (mp-num (px dir)) (mp-num (py dir)))))))

(defun solid-report (r)
  (with-output-to-string (o)
    (format o "----------------   SOLIDS    ----------------~%~%")
    (when (> (getf r :count) 1) (format o "~D solids together~%~%" (getf r :count)))
    (let ((b (getf r :bbox3)) (c (getf r :centroid)))
      (format o "Mass:                    ~A~%" (mp-num (getf r :mass)))
      (format o "Volume:                  ~A~%" (mp-num (getf r :volume)))
      (format o "Bounding box:         X: ~A  --  ~A~%" (mp-num (first b)) (mp-num (fourth b)))
      (format o "                      Y: ~A  --  ~A~%" (mp-num (second b)) (mp-num (fifth b)))
      (format o "                      Z: ~A  --  ~A~%" (mp-num (third b)) (mp-num (sixth b)))
      (format o "Centroid:             X: ~A~%" (mp-num (px c)))
      (format o "                      Y: ~A~%" (mp-num (py c)))
      (format o "                      Z: ~A~%" (mp-num (pz c)))
      (format o "Moments of inertia:   X: ~A~%" (mp-num (getf r :ix)))
      (format o "                      Y: ~A~%" (mp-num (getf r :iy)))
      (format o "                      Z: ~A~%" (mp-num (getf r :iz)))
      (format o "Products of inertia: XY: ~A~%" (mp-num (getf r :pxy)))
      (format o "                     YZ: ~A~%" (mp-num (getf r :pyz)))
      (format o "                     ZX: ~A~%" (mp-num (getf r :pzx)))
      (format o "Radii of gyration:    X: ~A~%" (mp-num (getf r :rx)))
      (format o "                      Y: ~A~%" (mp-num (getf r :ry)))
      (format o "                      Z: ~A~%" (mp-num (getf r :rz)))
      (format o "Principal moments and X-Y-Z directions about centroid:~%")
      (loop for (val dir) in (getf r :principal) for name in '("I" "J" "K")
            do (format o "                      ~A: ~A along [~A ~A ~A]~%" name (mp-num val)
                       (mp-num (px dir)) (mp-num (py dir)) (mp-num (pz dir))))
      (format o "~%(Density 1: the mass equals the volume.)~%"))))

(defun massprop-report (ents)
  "The MASSPROP text for entities ENTS, and how many of them were left out."
  (let* ((datas (mapcar #'ent-data ents))
         (solids (remove-if-not (lambda (d) (equal (dxf 0 d) "POLYFACE")) datas))
         (flat (remove-if (lambda (d) (equal (dxf 0 d) "POLYFACE")) datas))
         (sr (and solids (solid-properties solids)))
         (rr (and flat (region-properties flat)))
         (used (+ (if sr (getf sr :count) 0) (if rr (getf rr :count) 0))))
    (values (concatenate 'string (if rr (region-report rr) "") (if (and rr sr) (string #\Newline) "") (if sr (solid-report sr) ""))
            (- (length ents) used))))

(defcmd "MASSPROP" ("MASSPROPERTIES") "Mass properties of regions (closed flat objects) and solids: area, volume, centroid, moments of inertia ..."
  (let ((sel (get-selection "Select objects: ")))
    (when sel
      (multiple-value-bind (text skipped) (massprop-report sel)
        (if (string= text "")
            (out (format nil "Select closed polylines, circles, ellipses, closed splines or closed solids.~%"))
            (progn
              (out (format nil "~%~A" text))
              (when (plusp skipped)
                (out (format nil "~D object~:P left out: not closed, not flat, or an open surface.~%" skipped)))
              (when (use-inplace-editor-p) (tk "textwin ~A ~A" (tq "Mass Properties") (tq text)))
              (when (equal "Yes" (gkw "Write analysis to a file? [Yes/No] <N>: " '("Yes" "No") "No"))
                (let ((f (ask-file t "Create Mass and Area Properties File"
                                   (let ((fn (drawing-filename *dwg*))) (if fn (namestring (make-pathname :type "mpr" :defaults (pathname fn))) "massprop.mpr"))
                                   "mpr")))
                  (when f
                    (let ((f (ensure-ext f "mpr")))
                      (with-open-file (o f :direction :output :if-exists :supersede)
                        (write-string text o))
                      (out (format nil "Written to ~A~%" f))))))))))))
