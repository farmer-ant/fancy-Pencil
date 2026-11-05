;;;; spline.lisp — the SPLINE command (as AutoCAD 2012 and later) and making B-splines.
;;;; Method Fit: the curve passes through the points picked (global cubic interpolation, P&T A9.1,
;;;; with chord-length, square-root or uniform parameters, optional end tangents); the fit points are
;;;; kept, so their grips reshape it. Method CV: the points are control vertices of a spline of the
;;;; chosen degree. Object turns a polyline into a spline through its vertices.
;;;; A spline is AutoCAD's: 70 flags (1 closed, 8 planar), 71 degree, 40 knots, 10 control points,
;;;; 11 fit points, 12 / 13 end tangents, 44 fit tolerance.

(in-package :fancy-pencil)

(defvar *spline-method* :fit)
(defvar *spline-knots* :chord)
(defvar *spline-degree* 3)

;;; ---------- B-spline basis and solving ----------

(defun bspline-basis (p kv u)
  "Every basis function N(i,p) at U for knot vector KV: a vector of (length KV) - p - 1 values."
  (let* ((m (length kv)) (n (- m p 1)) (nb (make-array (1- m) :initial-element 0d0)))
    ;; degree 0, with the last span closed at its right end
    (let ((last-span (loop for i from (- m 2) downto 0 when (< (aref kv i) (aref kv (1+ i))) return i)))
      (dotimes (i (1- m))
        (setf (aref nb i) (if (or (and (<= (aref kv i) u) (< u (aref kv (1+ i))))
                                  (and (eql i last-span) (= u (aref kv (1+ i)))))
                              1d0 0d0))))
    (loop for q from 1 to p
          do (dotimes (i (- m q 1))
               (let* ((d1 (- (aref kv (+ i q)) (aref kv i))) (d2 (- (aref kv (+ i q 1)) (aref kv (1+ i))))
                      (a (if (zerop d1) 0d0 (* (/ (- u (aref kv i)) d1) (aref nb i))))
                      (b (if (zerop d2) 0d0 (* (/ (- (aref kv (+ i q 1)) u) d2) (aref nb (1+ i))))))
                 (setf (aref nb i) (+ a b)))))
    (subseq nb 0 n)))

(defun solve-points (a rhs)
  "Solve A x = RHS (A an n x n array of reals, RHS a list of points) by Gaussian elimination."
  (let* ((n (array-dimension a 0))
         (m (make-array (list n n) :initial-contents (loop for i below n collect (loop for j below n collect (d (aref a i j))))))
         (b (coerce (mapcar #'pt3 rhs) 'vector)))
    (dotimes (c n)
      (let ((piv (loop with best = c for r from c below n
                       when (> (abs (aref m r c)) (abs (aref m best c))) do (setf best r)
                       finally (return best))))
        (when (< (abs (aref m piv c)) 1d-14) (error "These points can't be fitted."))
        (unless (= piv c)
          (dotimes (j n) (rotatef (aref m c j) (aref m piv j)))
          (rotatef (aref b c) (aref b piv)))
        (loop for r from (1+ c) below n
              do (let ((f (/ (aref m r c) (aref m c c))))
                   (unless (zerop f)
                     (loop for j from c below n do (decf (aref m r j) (* f (aref m c j))))
                     (setf (aref b r) (v- (aref b r) (v* (aref b c) f))))))))
    (let ((x (make-array n)))
      (loop for r from (1- n) downto 0
            do (let ((s (aref b r)))
                 (loop for j from (1+ r) below n do (setf s (v- s (v* (aref x j) (aref m r j)))))
                 (setf (aref x r) (v* s (/ 1 (aref m r r))))))
      (coerce x 'list))))

(defun fit-parameters (pts kind)
  "Parameters 0..1 for points PTS: by chord length, its square root, or evenly."
  (let* ((steps (loop for (a b) on pts while b
                      collect (let ((dd (dist a b))) (case kind (:sqrt (sqrt dd)) (:uniform 1d0) (t dd)))))
         (total (reduce #'+ steps)))
    (if (< total 1d-12)
        (loop for i below (length pts) collect (/ (d i) (max 1 (1- (length pts)))))
        (let ((acc 0d0))
          (append (list 0d0)
                  (loop for s in steps for i from 1
                        collect (if (= i (length steps)) 1d0 (/ (incf acc s) total))))))))

(defun interpolate-spline (pts &key (kind :chord) start-tan end-tan)
  "A clamped B-spline through PTS: (values degree knots control-points). START-TAN / END-TAN, if
given, are the directions it leaves the first point and reaches the last."
  (let* ((pts (mapcar #'pt3 pts))
         (n (1- (length pts)))
         (p (min 3 (+ n (if start-tan 1 0) (if end-tan 1 0))))
         (tk (fit-parameters pts kind))
         (tv (coerce tk 'vector))
         (chord (max 1d-12 (reduce #'+ (loop for (a b) on pts while b collect (dist a b)))))
         (ndv (+ (if start-tan 1 0) (if end-tan 1 0)))
         (ncp (+ n 1 ndv))
         (knots
           (let ((inner
                   (if (zerop ndv)
                       (loop for j from 1 to (- n p) collect (/ (loop for i from j below (+ j p) sum (aref tv i)) p))
                       ;; with end tangents (P&T 9.22): u(j+p+1) = the average of t(j) .. t(j+p-1)
                       (let ((k (- ncp p 1)))
                         (loop for j from 0 below k
                               collect (/ (loop for i from j below (+ j p) sum (aref tv (min n i))) p))))))
             (coerce (append (make-list (1+ p) :initial-element 0d0)
                             (sort (remove-if (lambda (x) (or (<= x 0d0) (>= x 1d0))) inner) #'<)
                             (make-list (1+ p) :initial-element 1d0))
                     'vector))))
    ;; the knot count must match the control points: pad or trim inner knots evenly if rounding
    ;; removed any (repeated parameters)
    (loop while (< (length knots) (+ ncp p 1))
          do (let* ((v (coerce knots 'list)) (i (+ p (floor (- (length v) (* 2 (1+ p))) 2))))
               (setf knots (coerce (append (subseq v 0 (1+ i)) (list (/ (+ (nth i v) (nth (1+ i) v)) 2)) (subseq v (1+ i))) 'vector))))
    (when (<= p 1)
      (return-from interpolate-spline
        (values 1 (coerce (append (list 0d0 0d0) (subseq tk 1 n) (list 1d0 1d0)) 'list) pts)))
    (let ((a (make-array (list ncp ncp) :initial-element 0d0)) (rhs nil) (row 0))
      (flet ((put-basis (u)
               (let ((nb (bspline-basis p knots u)))
                 (dotimes (i ncp) (setf (aref a row i) (aref nb i)))
                 (incf row))))
        (put-basis 0d0) (push (first pts) rhs)
        (when start-tan                    ; P1 - P0 = (u(p+1)/p) * D0
          (setf (aref a row 0) -1d0 (aref a row 1) 1d0)
          (push (v* (vunit start-tan) (/ (* chord (aref knots (1+ p))) p)) rhs) (incf row))
        (loop for k from 1 below n do (put-basis (aref tv k)) (push (nth k pts) rhs))
        (when end-tan                      ; Pm - Pm-1 = ((1 - u(m-p-1)) / p) * Dn
          (setf (aref a row (- ncp 2)) -1d0 (aref a row (1- ncp)) 1d0)
          (push (v* (vunit end-tan) (/ (* chord (- 1 (aref knots (- (length knots) p 2)))) p)) rhs) (incf row))
        (put-basis 1d0) (push (car (last pts)) rhs))
      (values p (coerce knots 'list) (solve-points a (nreverse rhs))))))

(defun vunit (v) (let ((l (sqrt (vdot3 (pt3 v) (pt3 v))))) (if (< l 1d-12) (pt 1 0 0) (v* (pt3 v) (/ 1 l)))))

(defun clamped-knots (p n)
  "Knots for a clamped spline of degree P with N control points, evenly spaced inside."
  (let ((inner (- n p 1)))
    (append (make-list (1+ p) :initial-element 0d0)
            (loop for i from 1 to inner collect (/ (d i) (1+ inner)))
            (make-list (1+ p) :initial-element 1d0))))

(defun spline-data (degree knots ctrl &key fit closed start-tan end-tan (tol 0d0))
  (append (list (cons 0 "SPLINE")
                (cons 70 (logior 8 (if closed 1 0)))
                (cons 71 degree) (cons 72 (length knots)) (cons 73 (length ctrl)) (cons 74 (length fit))
                (cons 42 1d-10) (cons 43 1d-10) (cons 44 (d tol)))
          (and start-tan (list (cons 12 (pt3 start-tan))))
          (and end-tan (list (cons 13 (pt3 end-tan))))
          (mapcar (lambda (k) (cons 40 (d k))) knots)
          (mapcar (lambda (c) (cons 10 (pt3 c))) ctrl)
          (mapcar (lambda (f) (cons 11 (pt3 f))) fit)))

(defun fit-spline-data (pts &key closed start-tan end-tan (kind *spline-knots*) (tol 0d0))
  "A spline through points PTS (closed: back to the first, smoothly)."
  (let* ((pts (remove-duplicates (mapcar #'pt3 pts) :test (lambda (a b) (< (dist a b) 1d-10)) :from-end t)))
    (when (< (length pts) 2) (return-from fit-spline-data nil))
    (if (and closed (>= (length pts) 3))
        (let* ((n (length pts))
               (tan (v- (nth 1 pts) (nth (1- n) pts))))      ; the same direction at the join
          (multiple-value-bind (p kv cv) (interpolate-spline (append pts (list (first pts))) :kind kind
                                                                                           :start-tan tan :end-tan tan)
            (spline-data p kv cv :fit pts :closed t :start-tan tan :end-tan tan :tol tol)))
        (multiple-value-bind (p kv cv) (interpolate-spline pts :kind kind :start-tan start-tan :end-tan end-tan)
          (spline-data p kv cv :fit pts :start-tan start-tan :end-tan end-tan :tol tol)))))

(defun cv-spline-data (pts degree &key closed)
  "A spline with control vertices PTS (closed: wrapped round so it joins smoothly)."
  (let* ((pts (mapcar #'pt3 pts)))
    (when (< (length pts) 2) (return-from cv-spline-data nil))
    (if (and closed (>= (length pts) 3))
        (let* ((p (min degree (1- (length pts))))
               (cv (append pts (subseq pts 0 p)))           ; periodic: the first P repeated
               (m (+ (length cv) p 1))
               (kv (loop for i below m collect (d i))))
          (spline-data p kv cv :closed t))
        (let ((p (min degree (1- (length pts)))))
          (spline-data p (clamped-knots p (length pts)) pts)))))

;;; ---------- the command ----------

(defun spline-fit-points (data) (loop for p in data when (and (consp p) (eql (car p) 11)) collect (pt3 (cdr p))))

(defun refit-spline (data fit)
  "DATA (a fit spline) passing through FIT instead."
  (let ((nd (fit-spline-data fit :closed (logtest 1 (or (dxf 70 data) 0))
                                 :start-tan (dxf 12 data) :end-tan (dxf 13 data) :tol (or (dxf 44 data) 0d0))))
    (if nd
        (append nd (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 48 370 420)))) data))
        data)))

(defun spline-preview (pts cursor &key closed start-tan)
  (let ((all (append pts (and cursor (list (pt3 cursor))))))
    (when (>= (length all) 2)
      (let ((d (ignore-errors (if (eq *spline-method* :fit)
                                  (fit-spline-data all :closed closed :start-tan start-tan)
                                  (cv-spline-data all *spline-degree* :closed closed)))))
        (append (and d (list (list :data d)))
                (and (eq *spline-method* :cv) (list (list :data (make-lwpoly-data all nil)))))))))

(defcmd "SPLINE" ("SPL") "Draws a smooth curve through (Fit) or controlled by (CV) the points you pick."
  (out (format nil "Current settings: Method=~:[CV~;Fit~]  ~:[Degree=~D~;Knots=~A~]~%"
               (eq *spline-method* :fit) (eq *spline-method* :fit)
               (if (eq *spline-method* :fit)
                   (case *spline-knots* (:sqrt "Square root") (:uniform "Uniform") (t "Chord"))
                   *spline-degree*)))
  (let ((pts nil) (start-tan nil) (end-tan nil) (tol 0d0) (closed nil))
    ;; the first point, or settings
    (loop
      (let ((r (get-user-input :point :prompt (if (eq *spline-method* :fit)
                                                  "Specify first point or [Method/Knots/Object]: "
                                                  "Specify first point or [Method/Degree/Object]: ")
                                      :keywords (if (eq *spline-method* :fit) '("Method" "Knots" "Object") '("Method" "Degree" "Object")))))
        (cond ((null r) (return-from command-body nil))
              ((pointp r) (push (pt3 r) pts) (return))
              ((equal r "Method")
               (let ((m (get-user-input :keyword :prompt "Enter spline creation method [Fit/CV] <Fit>: " :keywords '("Fit" "CV"))))
                 (setf *spline-method* (if (equal m "CV") :cv :fit))))
              ((equal r "Knots")
               (let ((k (get-user-input :keyword :prompt "Enter knot parameterization [Chord/Square root/Uniform] <Chord>: "
                                                 :keywords '("Chord" "Square" "Uniform"))))
                 (setf *spline-knots* (cond ((equal k "Square") :sqrt) ((equal k "Uniform") :uniform) (t :chord)))))
              ((equal r "Degree")
               (let ((dg (get-user-input :int :prompt (format nil "Enter degree of spline <~D>: " *spline-degree*))))
                 (when (and (integerp dg) (<= 1 dg 10)) (setf *spline-degree* dg))))
              ((equal r "Object")
               (let ((ents (select-objects :prompt "Select polylines to convert to splines: ")) (n 0))
                 (dolist (e ents)
                   (let ((dd (ename-data e)))
                     (when (equal (dxf 0 dd) "LWPOLYLINE")
                       (let* ((vs (loop for p in dd when (and (consp p) (eql (car p) 10)) collect (pt3 (cdr p))))
                              (nd (fit-spline-data vs :closed (logtest 1 (or (dxf 70 dd) 0)))))
                         (when nd
                           (set-entity-data e (append nd (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370)))) dd)))
                           (incf n))))))
                 (out (format nil "~D polyline~:P converted to spline~:P.~%" n))
                 (return-from command-body nil))))))
    ;; the next points
    (loop
      (let* ((n (length pts))
             (fit (eq *spline-method* :fit))
             (kws (append (if fit (if (= n 1) '("Tangency" "toLerance") '("Tangency" "toLerance")) nil)
                          (and (> n 1) '("Undo")) (and (> n 2) '("Close"))))
             (prompt (format nil "Enter next point or [~{~A~^/~}]: "
                             (append (and fit (list (if (= n 1) "start Tangency" "end Tangency") "toLerance"))
                                     (and (> n 1) (list "Undo")) (and (> n 2) (list "Close")))))
             (r (get-user-input :point :base (first pts) :prompt prompt :keywords kws
                                       :drag (lambda (c) (spline-preview (reverse pts) c :start-tan start-tan)))))
        (cond ((null r) (return))
              ((pointp r) (push (pt3 r) pts))
              ((equal r "Undo") (pop pts) (when (null pts) (return-from command-body nil)))
              ((equal r "Close") (setf closed t) (return))
              ((equal r "toLerance")
               (let ((v (get-user-input :distance :prompt (format nil "Specify fit tolerance <~A>: " (fmt-len tol)))))
                 (when (realp v) (setf tol (d v)))))
              ((equal r "Tangency")
               (let ((q (get-user-input :point :base (if (= n 1) (first pts) (first pts))
                                                :prompt (if (= n 1) "Specify start tangent: " "Specify end tangent: "))))
                 (when (pointp q)
                   (if (= n 1)
                       (setf start-tan (v- (pt3 q) (first pts)))
                       (progn (setf end-tan (v- (pt3 q) (first pts))) (return)))))))))
    (let* ((pts (reverse pts))
           (data (if (eq *spline-method* :fit)
                     (fit-spline-data pts :closed closed :start-tan start-tan :end-tan end-tan :tol tol)
                     (cv-spline-data pts *spline-degree* :closed closed))))
      (if data (add-entity data) (out (format nil "A spline needs at least two points.~%"))))))
