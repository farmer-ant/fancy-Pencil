;;;; acis.lisp — AutoCAD's 3D solids, surfaces and regions (3DSOLID, SURFACE, REGION, BODY).
;;;;
;;;; AutoCAD keeps their exact shape in ACIS form: a "boundary representation" — faces lying on
;;;; planes, cones and cylinders, spheres, tori or spline surfaces, each bounded by loops of edges
;;;; (lines, circles and ellipses, spline curves). In a DXF file from AutoCAD 2013 on, it sits in
;;;; the ACDSDATA section in ACIS's binary form (SAB), one record per object, found by the object's
;;;; handle. This file reads that, and turns each face into triangles: the face's edges are
;;;; divided into short pieces, the outline is laid out flat in the surface's own coordinates and
;;;; cut into triangles, and triangles on a curved surface are divided until they follow it. The
;;;; result is a Fancy Pencil mesh (POLYFACE) showing the object's real edges.

(in-package :fancy-pencil)

;;; ---------- reading SAB ----------

(defun sab-records (bytes)
  "The records of an ACIS binary (SAB) BYTES: a vector of token vectors. Each token is
(tag . value) with tag :i :d :s :b :p :T :v :n :e :{ :}; the first :T token names the record."
  (let ((i 0) (n (length bytes)) (records (make-array 0 :adjustable t :fill-pointer 0))
        (rec (make-array 0 :adjustable t :fill-pointer 0)) (prefix nil))
    (labels ((u8 () (prog1 (aref bytes i) (incf i)))
             (i32 () (let ((v (logior (aref bytes i) (ash (aref bytes (+ i 1)) 8) (ash (aref bytes (+ i 2)) 16) (ash (aref bytes (+ i 3)) 24))))
                       (incf i 4) (if (logbitp 31 v) (- v (ash 1 32)) v)))
             (f64 () (let ((bits 0))
                       (dotimes (k 8) (setf bits (logior bits (ash (aref bytes (+ i k)) (* 8 k)))))
                       (incf i 8)
                       (bits-double bits)))
             (str (len) (prog1 (map 'string #'code-char (subseq bytes i (+ i len))) (incf i len)))
             (tok (tag v) (vector-push-extend (cons tag v) rec)))
      ;; "ACIS BinaryFile", then version, record count, body count, flags
      ;; (AutoCAD 2013 writes "ACIS BinaryFile"; 2018 and later "ASM BinaryFile4", laid out the same)
      (unless (and (> n 31) (member (map 'string #'code-char (subseq bytes 0 15)) '("ACIS BinaryFile" "ASM BinaryFile4") :test #'string=))
        (return-from sab-records nil))
      (setf i 15)
      (let ((version (i32))) (i32) (i32) (i32)
        (loop while (< i n)
              do (let ((tag (u8)))
                   (case tag
                     (4 (tok :i (i32)))
                     ((6 #x17) (tok :d (f64)))
                     (7 (tok :s (str (u8))))
                     (#x0A (tok :b t))
                     (#x0B (tok :b nil))
                     (#x0C (tok :p (i32)))
                     (#x0D (push (str (u8)) prefix)
                      (tok :T (format nil "~{~A~^-~}" (reverse prefix))) (setf prefix nil))
                     (#x0E (push (str (u8)) prefix))
                     (#x0F (tok :{ nil))
                     (#x10 (tok :} nil))
                     (#x11 (vector-push-extend (coerce rec 'simple-vector) records)
                      (setf rec (make-array 0 :adjustable t :fill-pointer 0)))
                     (#x12 (tok :s (str (i32))))
                     (#x13 (tok :v (pt (f64) (f64) (f64))))
                     (#x14 (tok :n (pt (f64) (f64) (f64))))
                     (#x15 (tok :e (i32)))
                     (t (return)))))
        (values (coerce records 'simple-vector) version)))))

(defun bits-double (bits)
  (let* ((s (if (logbitp 63 bits) -1d0 1d0)) (e (ldb (byte 11 52) bits)) (f (ldb (byte 52 0) bits)))
    (cond ((= e 0) (* s (scale-float (d f) -1074)))
          ((= e 2047) 0d0)
          (t (* s (scale-float (d (logior f (ash 1 52))) (- e 1075)))))))

;;; record access: tokens by position (0 is the record's name)
(defvar *sab* #() "The records being read.")
(defun rec (k) (and (integerp k) (<= 0 k) (< k (length *sab*)) (aref *sab* k)))
(defun rec-name (r) (and r (> (length r) 0) (eq (car (aref r 0)) :T) (cdr (aref r 0))))
(defun tokv (r k) (and r (< k (length r)) (cdr (aref r k))))
(defun tokp (r k) (let ((v (tokv r k))) (and (integerp v) (>= v 0) v)))

;;; ---------- curves ----------

(defun read-bspline (r start surface)
  "The B-spline (nubs or nurbs) whose name token is at START of record R. A curve: (degree knots
ctrl periodic); a surface: (du dv ku kv nu nv ctrl periodic-u periodic-v). Control points are
homogeneous (x*w y*w z*w w); knot vectors are full (end knots repeated degree+1 times)."
  (let ((i (1+ start)) (rational (equal (tokv r start) "nurbs")))
    (labels ((next () (prog1 (tokv r i) (incf i)))
             (knots (count) (let ((ks nil))
                              (dotimes (k count) (let ((kv (d (next))) (m (next))) (dotimes (j m) (push kv ks))))
                              (let ((ks (nreverse ks))) (append (list (first ks)) ks (last ks)))))
             (cp () (let* ((x (d (next))) (y (d (next))) (z (d (next))) (w (if rational (d (next)) 1d0)))
                      (vector (* x w) (* y w) (* z w) w))))
      (if (not surface)
          (let* ((deg (next)) (closure (next)) (nk (next)) (ks (knots nk))
                 (n (- (length ks) deg 1))
                 (ctrl (coerce (loop repeat n collect (cp)) 'simple-vector)))
            (list deg (coerce ks 'simple-vector) ctrl (eql closure 2)))
          (let* ((du (next)) (dv (next)) (cu (next)) (cv (next)) (su (next)) (sv (next)) (nku (next)) (nkv (next))
                 (ku (knots nku)) (kv (knots nkv))
                 (nu (- (length ku) du 1)) (nv (- (length kv) dv 1))
                 (ctrl (coerce (loop repeat (* nu nv) collect (cp)) 'simple-vector)))
            (declare (ignore su sv))
            (list du dv (coerce ku 'simple-vector) (coerce kv 'simple-vector) nu nv ctrl (eql cu 2) (eql cv 2)))))))

(defun find-bspline (r)
  (loop for k from 1 below (length r)
        when (and (eq (car (aref r k)) :T) (member (cdr (aref r k)) '("nubs" "nurbs") :test #'equal))
          return k))

(defun deboor-h (deg ks ctrl-fn n tt)
  "Homogeneous point at TT of the B-spline with degree DEG, knots KS and N control points given
by CTRL-FN (index -> 4-vector)."
  (let* ((lo (aref ks deg)) (hi (aref ks n))
         (tt (max lo (min hi tt)))
         (k (or (loop for j from deg below n when (and (<= (aref ks j) tt) (< tt (aref ks (1+ j)))) return j)
                (1- n)))
         (dd (make-array (1+ deg))))
    (dotimes (j (1+ deg)) (setf (aref dd j) (copy-seq (funcall ctrl-fn (+ j (- k deg))))))
    (loop for r from 1 to deg
          do (loop for j from deg downto r
                   do (let* ((i (+ j (- k deg))) (den (- (aref ks (+ i (- deg r) 1)) (aref ks i)))
                             (a (if (zerop den) 0d0 (/ (- tt (aref ks i)) den)))
                             (p (aref dd (1- j))) (q (aref dd j)))
                        (setf (aref dd j) (map 'vector (lambda (x y) (+ (* (- 1 a) x) (* a y))) p q)))))
    (aref dd deg)))

(defun wrap-param (tt lo hi periodic)
  (if (and periodic (> hi lo))
      (+ lo (mod (- tt lo) (- hi lo)))
      tt))

(defun bspline-curve-fn (bs)
  (destructuring-bind (deg ks ctrl periodic) bs
    (let ((n (length ctrl)))
      (lambda (tt)
        (let ((h (deboor-h deg ks (lambda (j) (aref ctrl j)) n (wrap-param tt (aref ks deg) (aref ks n) periodic))))
          (pt (/ (aref h 0) (aref h 3)) (/ (aref h 1) (aref h 3)) (/ (aref h 2) (aref h 3))))))))

(defun curve-fn (r)
  "Point at parameter t on curve record R, or NIL for a curve kind not known."
  (let ((name (rec-name r)))
    (cond
      ((equal name "straight-curve")
       (let ((o (tokv r 4)) (dir (tokv r 5))) (lambda (tt) (v+ o (v* dir tt)))))
      ((equal name "ellipse-curve")
       (let* ((c (tokv r 4)) (nrm (unit3 (tokv r 5))) (maj (tokv r 6)) (ratio (d (tokv r 7)))
              (mn (v* (cross3 nrm maj) ratio)))
         (lambda (tt) (v+ c (v+ (v* maj (cos tt)) (v* mn (sin tt)))))))
      ((equal name "intcurve-curve")
       (let ((k (find-bspline r))) (and k (bspline-curve-fn (read-bspline r k nil)))))
      (t nil))))

;;; ---------- surfaces ----------

(defstruct asurf kind eval invert period scale singular)
;; EVAL (u v) -> point; INVERT point -> (values u v); PERIOD of u (or NIL); SCALE: how many
;; drawing units one unit of u is worth (so u and v are laid out evenly for cutting into triangles)

(defun plane-asurf (o nrm udir)
  (let* ((nrm (unit3 nrm)) (ux (unit3 (v- udir (v* nrm (vdot3 udir nrm))))) (vy (cross3 nrm ux)))
    (make-asurf :kind :plane
                :eval (lambda (u v) (v+ o (v+ (v* ux u) (v* vy v))))
                :invert (lambda (p) (let ((dd (v- p o))) (values (vdot3 dd ux) (vdot3 dd vy))))
                :scale 1d0)))

(defun surface-of (r loops3d)
  "The surface record R as an ASURF (LOOPS3D give a flat face's plane when R is unknown)."
  (let ((name (rec-name r)))
    (cond
      ((equal name "plane-surface") (plane-asurf (tokv r 4) (tokv r 5) (tokv r 6)))
      ((equal name "cone-surface")
       (let* ((c (tokv r 4)) (ax (unit3 (tokv r 5))) (maj (tokv r 6)) (ratio (d (tokv r 7)))
              (big (sqrt (vdot3 maj maj))) (mu (unit3 maj)) (mv (cross3 ax mu))
              (sn (or (tokv r 10) 0d0)) (cs (or (tokv r 11) 1d0))
              (slope (if (> (abs cs) 1d-12) (/ sn cs) 0d0)))
         (make-asurf :kind :cone
                     :eval (lambda (u h) (let ((s (/ (+ big (* h slope)) big)))
                                           (v+ c (v+ (v* ax h) (v* (v+ (v* mu (* big (cos u))) (v* mv (* big ratio (sin u)))) s)))))
                     :invert (lambda (p) (let* ((dd (v- p c)) (h (vdot3 dd ax)))
                                           (values (atan (/ (vdot3 dd mv) (max ratio 1d-9)) (vdot3 dd mu)) h)))
                     :period (* 2 pi) :scale big
                     :singular (lambda (p) (let* ((dd (v- p c)) (r (v- dd (v* ax (vdot3 dd ax))))) (< (vlen3 r) (* 0.01 big)))))))
      ((equal name "sphere-surface")
       (let* ((c (tokv r 4)) (rad (abs (d (tokv r 5))))
              (ux (let ((v (tokv r 6))) (if (consp v) (unit3 v) (pt 1 0 0))))
              (pole (let ((v (tokv r 7))) (if (consp v) (unit3 v) (pt 0 0 1))))
              (uy (cross3 pole ux)))
         (make-asurf :kind :sphere
                     :eval (lambda (u v) (v+ c (v* (v+ (v* (v+ (v* ux (cos u)) (v* uy (sin u))) (cos v)) (v* pole (sin v))) rad)))
                     :invert (lambda (p) (let ((dd (unit3 (v- p c))))
                                           (values (atan (vdot3 dd uy) (vdot3 dd ux)) (asin (max -1d0 (min 1d0 (vdot3 dd pole)))))))
                     :period (* 2 pi) :scale rad
                     :singular (lambda (p) (> (abs (vdot3 (unit3 (v- p c)) pole)) (- 1 1d-6))))))
      ((equal name "torus-surface")
       (let* ((c (tokv r 4)) (ax (unit3 (tokv r 5))) (big (d (tokv r 6))) (small (abs (d (tokv r 7))))
              (ux (let ((v (tokv r 8))) (if (consp v) (unit3 v) (any-perpendicular ax)))) (uy (cross3 ax ux)))
         (make-asurf :kind :torus
                     :eval (lambda (u v) (let ((radial (v+ (v* ux (cos u)) (v* uy (sin u)))))
                                           (v+ c (v+ (v* radial (+ big (* small (cos v)))) (v* ax (* small (sin v)))))))
                     :invert (lambda (p) (let* ((dd (v- p c)) (u (atan (vdot3 dd uy) (vdot3 dd ux)))
                                                (radial (v+ (v* ux (cos u)) (v* uy (sin u))))
                                                (q (v- dd (v* radial big))))
                                           (values u (atan (vdot3 q ax) (vdot3 q radial)))))
                     :period (* 2 pi) :scale (+ big small))))
      ((equal name "spline-surface")
       (let ((k (find-bspline r)))
         (if k (spline-asurf (read-bspline r k t)) (flat-asurf loops3d))))
      (t (flat-asurf loops3d)))))

(defun flat-asurf (loops3d)
  "A face whose surface isn't known: laid flat on the best plane through its outline."
  (let* ((pts (first loops3d)) (nrm (or (newell-normal pts) (pt 0 0 1))))
    (multiple-value-bind (ax ay) (ocs-axes nrm)
      (declare (ignore ay))
      (let ((s (plane-asurf (first pts) nrm ax))) (setf (asurf-kind s) :flat) s))))

(defun spline-asurf (bs)
  (destructuring-bind (du dv ku kv nu nv ctrl pu pv) bs
    (let* ((u0 (aref ku du)) (u1 (aref ku nu)) (v0 (aref kv dv)) (v1 (aref kv nv))
           (ev (lambda (u v)
                 (let* ((u (wrap-param u u0 u1 pu)) (v (wrap-param v v0 v1 pv))
                        (rows (coerce (loop for b below nv
                                            collect (deboor-h du ku (lambda (a) (aref ctrl (+ (* b nu) a))) nu u))
                                      'simple-vector))
                        (h (deboor-h dv kv (lambda (b) (aref rows b)) nv v)))
                   (pt (/ (aref h 0) (aref h 3)) (/ (aref h 1) (aref h 3)) (/ (aref h 2) (aref h 3))))))
           (grid (let ((g nil) (m 24))
                   (dotimes (a (1+ m) g)
                     (dotimes (b (1+ m))
                       (let ((u (+ u0 (* (- u1 u0) (/ a m)))) (v (+ v0 (* (- v1 v0) (/ b m)))))
                         (push (list u v (funcall ev u v)) g))))))
           (size (let ((ps (mapcar #'third grid)))
                   (max 1d-9 (dist (reduce (lambda (a b) (pt (min (px a) (px b)) (min (py a) (py b)) (min (pz a) (pz b)))) ps)
                                   (reduce (lambda (a b) (pt (max (px a) (px b)) (max (py a) (py b)) (max (pz a) (pz b)))) ps))))))
      (make-asurf :kind :spline :eval ev
                  :invert (lambda (p) (spline-invert ev p grid u0 u1 v0 v1 pu pv))
                  :period (and pu (- u1 u0))
                  :scale (/ size (max 1d-9 (- u1 u0) (- v1 v0)))))))

(defun spline-invert (ev p grid u0 u1 v0 v1 pu pv)
  "The (u v) of the point on the surface nearest P: from the nearest grid point, by Newton steps."
  (let* ((best (reduce (lambda (a b) (if (< (dist (third a) p) (dist (third b) p)) a b)) grid))
         (u (first best)) (v (second best))
         (hu (* 1d-5 (- u1 u0))) (hv (* 1d-5 (- v1 v0))))
    (dotimes (it 12)
      (let* ((s (funcall ev u v)) (r (v- p s))
             (su (v* (v- (funcall ev (+ u hu) v) s) (/ 1 hu)))
             (sv (v* (v- (funcall ev u (+ v hv)) s) (/ 1 hv)))
             (a11 (vdot3 su su)) (a12 (vdot3 su sv)) (a22 (vdot3 sv sv))
             (b1 (vdot3 su r)) (b2 (vdot3 sv r))
             (det (- (* a11 a22) (* a12 a12))))
        (when (< (abs det) 1d-30) (return))
        (let ((du (/ (- (* a22 b1) (* a12 b2)) det)) (dv (/ (- (* a11 b2) (* a12 b1)) det)))
          ;; keep each step modest, and inside the surface
          (let ((lim-u (* 0.25 (- u1 u0))) (lim-v (* 0.25 (- v1 v0))))
            (setf du (max (- lim-u) (min lim-u du)) dv (max (- lim-v) (min lim-v dv))))
          (setf u (if pu (+ u du) (max u0 (min u1 (+ u du))))
                v (if pv (+ v dv) (max v0 (min v1 (+ v dv)))))
          (when (and (< (abs du) (* 1d-9 (- u1 u0))) (< (abs dv) (* 1d-9 (- v1 v0)))) (return)))))
    (values (if pu (wrap-param u u0 u1 t) u) v)))

;;; ---------- edges ----------

(defvar *edge-points* nil "Edge record -> its points from its start vertex to its end vertex.")
(defvar *acis-debug* nil)
(defvar *acis-tol* 1d-3 "How closely curved edges and surfaces are followed (drawing units).")

(defun vertex-point (vk) (tokv (rec (tokp (rec vk) 6)) 4))

(defun edge-points (ek)
  (or (gethash ek *edge-points*)
      (setf (gethash ek *edge-points*)
            (let* ((e (rec ek))
                   (p0 (vertex-point (tokp e 4))) (p1 (vertex-point (tokp e 6)))
                   (t0 (d (or (tokv e 5) 0))) (t1 (d (or (tokv e 7) 1)))
                   (cr (rec (tokp e 9)))
                   (f (and cr (not (equal (rec-name cr) "straight-curve")) (curve-fn cr))))
              (if (or (null f) (null p0) (null p1))
                  (remove nil (list p0 p1))
                  ;; reversed edges run the curve backwards (parameter -t): use whichever fits
                  (let* ((g (if (and (< (dist (funcall f t0) p0) (* 10 *acis-tol*)) (< (dist (funcall f t1) p1) (* 10 *acis-tol*)))
                                f
                                (let ((h (lambda (tt) (funcall f (- tt)))))
                                  (if (< (+ (dist (funcall h t0) p0) (dist (funcall h t1) p1))
                                         (+ (dist (funcall f t0) p0) (dist (funcall f t1) p1)))
                                      h f))))
                         (ts (adaptive-params g t0 t1)))
                    (append (list p0) (mapcar g (butlast (rest ts))) (list p1))))))))

(defun adaptive-params (g t0 t1)
  "Parameters from T0 to T1 so the polyline through G's points follows the curve within the tolerance."
  (let ((ts (loop for k from 0 to 8 collect (+ t0 (* (- t1 t0) (/ k 8))))))
    (dotimes (pass 5 ts)
      (let ((out (list (first ts))) (changed nil))
        (loop for (a b) on ts while b
              do (let* ((m (/ (+ a b) 2)) (pa (funcall g a)) (pb (funcall g b))
                        (dev (dist (funcall g m) (v* (v+ pa pb) 0.5d0))))
                   (when (and (> dev *acis-tol*) (< (length ts) 512))
                     (push m out) (setf changed t))
                   (push b out)))
        (setf ts (nreverse out))
        (unless changed (return ts))))))

(defun loop-segments (lk)
  "Loop LK as its coedges in order: a list of (points closed-p), each run the loop's way."
  (let* ((l (rec lk)) (c0 (tokp l 5)) (c c0) (out nil) (guard 0))
    (loop
      (let* ((co (rec c)) (e (rec (tokp co 7))) (pts (edge-points (tokp co 7))))
        (push (list (if (tokv co 8) (reverse pts) pts) (eql (tokp e 4) (tokp e 6))) out)
        (setf c (tokp co 4))
        (when (or (null c) (= c c0) (> (incf guard) 10000)) (return))))
    (nreverse out)))

(defun segments-points (segs)
  "The closed outline of a loop's segments (each segment's last point is the next one's first)."
  (loop for (pts) in segs append (butlast pts)))

;;; ---------- triangles from a face ----------

(defun tri-area2 (a b c) (- (* (- (px b) (px a)) (- (py c) (py a))) (* (- (py b) (py a)) (- (px c) (px a)))))

(defun polygon-area2 (pts)
  (loop for (a b) on (append pts (list (first pts))) while b sum (- (* (px a) (py b)) (* (px b) (py a)))))

(defun ear-clip (pts)
  "Triangles (index triples into PTS, a counterclockwise 2D polygon that may touch itself at
bridges) by cutting off ears."
  (let* ((n (length pts)) (v (coerce pts 'vector)) (idx (loop for i below n collect i)) (out nil))
    (flet ((inside (p a b c) (and (>= (tri-area2 a b p) -1d-14) (>= (tri-area2 b c p) -1d-14) (>= (tri-area2 c a p) -1d-14))))
      (loop while (> (length idx) 3)
            do (let ((m (length idx)) (cut nil))
                 (dotimes (k m)
                   (let* ((ia (nth (mod (+ k m -1) m) idx)) (ib (nth k idx)) (ic (nth (mod (1+ k) m) idx))
                          (a (aref v ia)) (b (aref v ib)) (c (aref v ic)))
                     (when (and (> (tri-area2 a b c) 1d-18)
                                (notany (lambda (j) (and (/= j ia) (/= j ib) (/= j ic)
                                                         (let ((p (aref v j)))
                                                           (and (not (and (< (dist2d p a) 1d-12))) (not (< (dist2d p b) 1d-12)) (not (< (dist2d p c) 1d-12))
                                                                (inside p a b c)))))
                                        idx))
                       (push (list ia ib ic) out)
                       (setf idx (remove ib idx :count 1) cut t)
                       (return))))
                 (unless cut
                   ;; nothing clean to cut (rounding): drop the flattest corner
                   (let ((k (let ((best 0) (bv nil))
                              (dotimes (k m best)
                                (let ((ar (abs (tri-area2 (aref v (nth (mod (+ k m -1) m) idx)) (aref v (nth k idx)) (aref v (nth (mod (1+ k) m) idx))))))
                                  (when (or (null bv) (< ar bv)) (setf bv ar best k)))))))
                     (let ((ia (nth (mod (+ k m -1) m) idx)) (ib (nth k idx)) (ic (nth (mod (1+ k) m) idx)))
                       (when (> (tri-area2 (aref v ia) (aref v ib) (aref v ic)) 0) (push (list ia ib ic) out))
                       (setf idx (remove ib idx :count 1)))))))
      (when (= (length idx) 3)
        (let ((tr (list (first idx) (second idx) (third idx))))
          (when (> (tri-area2 (aref v (first tr)) (aref v (second tr)) (aref v (third tr))) 0) (push tr out))))
      out)))

(defun segments-cross-p (a b c e)
  "Segments A-B and C-E cross at an inner point of both."
  (let ((d1 (tri-area2 a b c)) (d2 (tri-area2 a b e)) (d3 (tri-area2 c e a)) (d4 (tri-area2 c e b)))
    (and (< (* d1 d2) 0) (< (* d3 d4) 0))))

(defun bridge-holes (outer holes)
  "One polygon from OUTER (counterclockwise) and HOLES (clockwise), each a list of vertex
records (uv ...), joined by cuts from each hole's rightmost corner to a corner of the outline it
can see."
  (let ((poly outer))
    (dolist (h (sort (copy-list holes) #'> :key (lambda (h) (reduce #'max h :key (lambda (r) (px (first r)))))))
      (let* ((m (position (reduce #'max h :key (lambda (r) (px (first r)))) h :key (lambda (r) (px (first r)))))
             (mp (first (nth m h)))
             (edges (loop for ring in (cons poly holes)
                          append (loop for (a b) on (append ring (list (first ring))) while b collect (cons (first a) (first b)))))
             (best nil) (bd nil))
        (loop for r in poly for k from 0
              do (let ((q (first r)))
                   (when (and (or (null bd) (< (dist2d q mp) bd))
                              (notany (lambda (e) (segments-cross-p mp q (car e) (cdr e))) edges))
                     (setf best k bd (dist2d q mp)))))
        (let* ((k (or best 0))
               (hole-run (append (nthcdr m h) (subseq h 0 m) (list (nth m h)))))
          (setf poly (append (subseq poly 0 (1+ k)) hole-run (list (nth k poly)) (nthcdr (1+ k) poly))))))
    poly))

(defun unwrap-loop (uvs period)
  "UVS of a loop with u made continuous (period PERIOD): (values uvs winding)."
  (if (null period)
      (values uvs 0)
      (let ((out (list (first uvs))))
        (dolist (q (rest uvs))
          (let* ((prev (px (first out))) (u (px q)))
            (setf u (+ u (* period (round (- prev u) period))))
            (push (pt u (py q)) out)))
        (let* ((out (nreverse out))
               (closing (let* ((prev (px (car (last out)))) (u (px (first out))))
                          (+ u (* period (round (- prev u) period))))))
          (values out (round (- closing (px (first out))) period))))))

(defun loop-on-surface (segs surf)
  "A loop's points with their surface coordinates: (uvs points winding). On a surface that goes
all round, a closed edge (a full circle) could run either way; when the loop would wind round
more than once, such edges are turned so it doesn't."
  (let ((period (asurf-period surf)))
    (flet ((lay-out (segs)
             (let* ((pts (segments-points segs))
                    (uvs (mapcar (lambda (p) (multiple-value-bind (u v) (funcall (asurf-invert surf) p) (pt u v))) pts))
                    (sing (mapcar (lambda (p) (and (asurf-singular surf) (funcall (asurf-singular surf) p))) pts)))
               (if (or (notany #'identity sing) (every #'identity sing))
                   (multiple-value-bind (uw w) (unwrap-loop uvs period)
                     (list uw pts w (make-list (length pts) :initial-element t)))
                   ;; the tip of a cone (a pole of a sphere) has no direction of its own: where the
                   ;; loop passes through it, it takes the direction of the edge coming in and of
                   ;; the edge going out, joined by a short run along the tip
                   (let* ((n (length pts)) (pv (coerce pts 'vector)) (uv (coerce uvs 'vector)) (sg (coerce sing 'vector))
                          (start (position nil sing))
                          (order (loop for k from 0 below n collect (mod (+ start k) n)))
                          (plain (remove-if (lambda (i) (aref sg i)) order)))
                     (multiple-value-bind (uw w) (unwrap-loop (mapcar (lambda (i) (aref uv i)) plain) period)
                       (let ((u-of (make-hash-table)) (out-uv nil) (out-p nil) (out-f nil))
                         (loop for i in plain for q in uw do (setf (gethash i u-of) (px q)))
                         (loop for k from 0 below n
                               for i = (nth k order)
                               do (if (not (aref sg i))
                                      (progn (push (pt (gethash i u-of) (py (aref uv i))) out-uv) (push (aref pv i) out-p) (push t out-f))
                                      (let ((u-in (gethash (nth (mod (1- k) n) order) u-of (gethash start u-of)))
                                            (u-out (gethash (nth (mod (1+ k) n) order) u-of (gethash start u-of))))
                                        (push (pt u-in (py (aref uv i))) out-uv) (push (aref pv i) out-p) (push nil out-f)
                                        (push (pt u-out (py (aref uv i))) out-uv) (push (aref pv i) out-p) (push t out-f))))
                         (list (nreverse out-uv) (nreverse out-p) w (nreverse out-f)))))))))
      (let ((l (lay-out segs)))
        (when (and period (>= (abs (third l)) 2))
          (let ((segs (copy-list segs)))
            (loop for k from 0 below (length segs)
                  while (>= (abs (third l)) 2)
                  do (when (second (nth k segs))
                       (let* ((pts (first (nth k segs)))
                              (uvs (mapcar (lambda (p) (multiple-value-bind (u v) (funcall (asurf-invert surf) p) (pt u v))) pts))
                              (uw (unwrap-loop uvs period))
                              (turn (round (- (px (car (last uw))) (px (first uw))) period)))
                         (when (= (signum turn) (signum (third l)))
                           (setf (nth k segs) (list (reverse pts) t))
                           (setf l (lay-out segs))))))))
        l))))

(defun spread-repeats (l sc)
  "Loop L (uvs points winding flags) with corners that landed on the same spot of the surface
(edges a little off it) moved apart, evenly between their neighbours: a polygon can't have two
corners in one place."
  (destructuring-bind (uvs pts w flags) l
    (let* ((v (coerce uvs 'vector)) (n (length v))
           (span (max 1d-12 (* sc (- (reduce #'max uvs :key #'px) (reduce #'min uvs :key #'px)))
                      (- (reduce #'max uvs :key #'py) (reduce #'min uvs :key #'py))))
           (eps (* 1d-7 span)))
      (flet ((same (a b) (< (sqrt (+ (expt (* sc (- (px a) (px b))) 2) (expt (- (py a) (py b)) 2))) eps)))
        (when (> n 3)
          (let ((k 1))
            (loop while (< k n)
                  do (if (same (aref v k) (aref v (1- k)))
                         ;; a run of repeats from K-1 to J-1: spread them toward the next distinct corner
                         (let ((j k))
                           (loop while (and (< j n) (same (aref v j) (aref v (1- k)))) do (incf j))
                           (let* ((a (aref v (1- k))) (b (aref v (mod j n))) (count (- j k -1)))
                             (loop for i from k below j
                                   do (setf (aref v i) (v+ a (v* (v- b a) (/ (- i k -1) (+ count 1d0)))))))
                           (setf k (max (1+ k) j)))
                         (incf k))))))
      (list (coerce v 'list) pts w flags))))

(defun face-polygon (loops3d surf)
  "The face as one polygon in the surface's coordinates: a list of (uv-scaled uv point real-p),
real-p saying the side to the next corner is a real edge. LOOPS3D are the loops' segments. NIL
if it can't be laid out."
  (let* ((period (asurf-period surf)) (sc (or (asurf-scale surf) 1d0))
         (loops (mapcar (lambda (segs) (spread-repeats (loop-on-surface segs surf) (or (asurf-scale surf) 1d0))) loops3d))
         (wrapping (remove-if (lambda (l) (zerop (third l))) loops))
         (plain (remove-if-not (lambda (l) (zerop (third l))) loops)))
    (labels ((rec4 (uv p real) (list (pt (* sc (px uv)) (py uv)) uv p real))
             (recs (uvs pts &optional flags) (mapcar (lambda (uv p f) (rec4 uv p f)) uvs pts (or flags (make-list (length uvs) :initial-element t))))
             (ccw (rs) (if (minusp (polygon-area2 (mapcar #'first rs))) (reverse-ring rs) rs))
             (cw (rs) (if (plusp (polygon-area2 (mapcar #'first rs))) (reverse-ring rs) rs))
             (shift-u (uvs du) (mapcar (lambda (q) (pt (+ (px q) du) (py q))) uvs))
             (center-u (uvs) (/ (reduce #'+ uvs :key #'px) (length uvs)))
             (align (l u-mid) (if period
                                  (shift-u (first l) (* period (round (- u-mid (center-u (first l))) period)))
                                  (first l)))
             (holes (u-mid) (mapcar (lambda (l) (cw (recs (align l u-mid) (second l) (fourth l)))) plain))
             (open-ring (l)
               ;; a wrapping loop as a path from its first point once round to the same point
               (destructuring-bind (uvs pts w flags) l
                 (append (mapcar #'list uvs pts flags)
                         (list (list (pt (+ (px (first uvs)) (* w period)) (py (first uvs))) (first pts) t))))))
      (cond
        ;; a band all round (a cylinder between two circles): cut open along a seam
        ((and period (= (length wrapping) 2))
         (destructuring-bind (a b) wrapping
           (let* ((pa (open-ring a)) (pb (open-ring b))
                  ;; run B back the other way, so A and B make a closed outline
                  (pb (if (= (signum (third a)) (signum (third b))) (reverse pb) pb))
                  (du (* period (round (- (px (first (car (last pa)))) (px (first (first pb)))) period)))
                  (ra (mapcar (lambda (r) (rec4 (first r) (second r) (third r))) pa))
                  ;; B runs backwards: each side's flag belongs to the corner before it
                  (rb (let ((rs (mapcar (lambda (r) (rec4 (pt (+ (px (first r)) du) (py (first r))) (second r) (third r))) pb)))
                        (if (= (signum (third a)) (signum (third b)))
                            (append (mapcar (lambda (r nxt) (list (first r) (second r) (third r) (fourth nxt))) (butlast rs) (rest rs)) (last rs))
                            rs))))
             ;; the two joins between A and B are the seam, not edges
             (setf (fourth (car (last ra))) nil (fourth (car (last rb))) nil)
             (let ((outer (ccw (append ra rb))))
               (bridge-holes outer (holes (center-u (mapcar #'second outer))))))))
        ;; one loop all round (a cone up to its point, a dome): close it across the point
        ((and period (= (length wrapping) 1))
         (let* ((pa (open-ring (first wrapping)))
                (v-end (pole-v surf (mapcar #'first pa)))
                (u-first (px (first (first pa)))) (u-last (px (first (car (last pa)))))
                (apex (funcall (asurf-eval surf) u-first v-end))
                (ra (mapcar (lambda (r) (rec4 (first r) (second r) (third r))) pa))
                (steps (max 1 (min 64 (length pa))))
                ;; the point itself, as a row of corners along the top (they all meet there)
                (ring (append ra (loop for k from 0 to steps
                                       collect (rec4 (pt (+ u-last (* (- u-first u-last) (/ k steps))) v-end) apex nil)))))
           (setf (fourth (car (last ra))) nil)
           (let ((outer (ccw ring)))
             (bridge-holes outer (holes (/ (+ u-first u-last) 2))))))
        ((null plain) nil)
        (t
         ;; the outline is the loop with the largest area; the others are holes in it
         (let* ((areas (mapcar (lambda (l) (abs (polygon-area2 (mapcar (lambda (q) (pt (* sc (px q)) (py q))) (first l))))) plain))
                (outer-l (nth (position (reduce #'max areas) areas) plain))
                (mid (center-u (first outer-l)))
                (outer (ccw (recs (first outer-l) (second outer-l) (fourth outer-l)))))
           (setf plain (remove outer-l plain))
           (bridge-holes outer (holes mid))))))))

(defun reverse-ring (rs)
  "Ring RS run the other way, keeping each side's real-p with its side."
  (let* ((v (coerce rs 'vector)) (n (length v)))
    (loop for k from (1- n) downto 0
          collect (let ((r (copy-list (aref v k))))
                    (setf (fourth r) (fourth (aref v (mod (1- k) n))))
                    r))))

(defun pole-v (surf uvs)
  "Where a cone comes to its point (or a sphere to its pole): the v to close a single wrapping loop."
  (case (asurf-kind surf)
    (:sphere (if (plusp (/ (reduce #'+ uvs :key #'py) (length uvs))) (/ pi 2) (- (/ pi 2))))
    (t ;; find the height where the cone's radius is zero
     (let* ((u (px (first uvs))) (v0 (py (first uvs)))
            (c0 (funcall (asurf-eval surf) u v0)) (c1 (funcall (asurf-eval surf) (+ u pi) v0))
            (r0 (/ (dist c0 c1) 2))
            (dv (if (< (abs v0) 1d-9) 1d0 (abs v0)))
            (r1 (/ (dist (funcall (asurf-eval surf) u (+ v0 dv)) (funcall (asurf-eval surf) (+ u pi) (+ v0 dv))) 2))
            (slope (/ (- r1 r0) dv)))
       (if (> (abs slope) 1d-12) (- v0 (/ r0 slope)) v0)))))

(defun refine-triangles (verts tris fixed surf tol max-tris)
  "Divide TRIS (index triples into VERTS, a vector of (uv point)) until they follow SURF within
TOL. A triangle off the surface has its longest side split (never a real edge, in FIXED), which
keeps the triangles well shaped as they get smaller."
  (let ((verts (make-array (length verts) :adjustable t :fill-pointer t :initial-contents verts))
        (tris (make-array (length tris) :adjustable t :fill-pointer t :initial-contents tris))
        (edge-tris (make-hash-table :test #'equal))
        (sc (or (asurf-scale surf) 1d0))
        (head nil) (tail nil))
    (labels ((key (a b) (if (< a b) (cons a b) (cons b a)))
             (enqueue (k) (let ((cell (list k))) (if tail (setf (cdr tail) cell tail cell) (setf head cell tail cell))))
             (dequeue () (prog1 (car head) (setf head (cdr head)) (unless head (setf tail nil))))
             (add-tri (k tr) (loop for (a b) on (append tr (list (first tr))) while b do (push k (gethash (key a b) edge-tris)))
                         (enqueue k))
             (drop-tri (k tr) (loop for (a b) on (append tr (list (first tr))) while b
                                    do (setf (gethash (key a b) edge-tris) (remove k (gethash (key a b) edge-tris)))))
             (uv (i) (first (aref verts i)))
             (p3 (i) (second (aref verts i)))
             (on-surface (u) (funcall (asurf-eval surf) (px u) (py u)))
             (edge-err (a b) (dist (on-surface (v* (v+ (uv a) (uv b)) 0.5d0)) (v* (v+ (p3 a) (p3 b)) 0.5d0)))
             (tri-err (tr) (destructuring-bind (a b c) tr
                             (max (edge-err a b) (edge-err b c) (edge-err c a)
                                  (dist (on-surface (v* (v+ (uv a) (v+ (uv b) (uv c))) (/ 1d0 3)))
                                        (v* (v+ (p3 a) (v+ (p3 b) (p3 c))) (/ 1d0 3))))))
             (len (a b) (let ((du (* sc (- (px (uv a)) (px (uv b))))) (dv (- (py (uv a)) (py (uv b))))) (sqrt (+ (* du du) (* dv dv)))))
             (split (a b)
               (let* ((u (v* (v+ (uv a) (uv b)) 0.5d0))
                      (m (vector-push-extend (list u (on-surface u)) verts)))
                 (dolist (k (copy-list (gethash (key a b) edge-tris)))
                   (let* ((tr (aref tris k))
                          ;; turn the triangle so the split side comes first, same way round
                          (tr (cond ((and (member (first tr) (list a b)) (member (second tr) (list a b))) tr)
                                    ((and (member (second tr) (list a b)) (member (third tr) (list a b))) (list (second tr) (third tr) (first tr)))
                                    (t (list (third tr) (first tr) (second tr)))))
                          (x (first tr)) (y (second tr)) (z (third tr)))
                     (drop-tri k (aref tris k))
                     (setf (aref tris k) (list x m z))
                     (add-tri k (aref tris k))
                     (let ((k2 (vector-push-extend (list m y z) tris))) (add-tri k2 (aref tris k2)))))
                 (remhash (key a b) edge-tris))))
      (dotimes (k (length tris)) (add-tri k (aref tris k)))
      (loop while (and head (< (length tris) max-tris))
            do (let* ((k (dequeue)) (tr (aref tris k)))
                 (when (> (tri-err tr) tol)
                   ;; split the side furthest off the surface; when the sides are close and only
                   ;; the middle is off, the longest side
                   (let ((best nil) (be -1d0) (longest nil) (bl -1d0))
                     (loop for (a b) on (append tr (list (first tr))) while b
                           do (unless (gethash (key a b) fixed)
                                (let ((e (edge-err a b)) (l (len a b)))
                                  (when (> e be) (setf be e best (cons a b)))
                                  (when (> l bl) (setf bl l longest (cons a b))))))
                     (let ((pick (if (> be (* 0.5 tol)) best longest)))
                       (when pick (split (car pick) (cdr pick)))))))))
    (values verts tris)))

(defun delaunay-flips (pts tris)
  "TRIS (index triples, counterclockwise, into the 2D points PTS) with long thin triangles traded
for well shaped ones: where two triangles share a side and the far corner of one lies inside the
other's circle, the shared side is turned the other way. Sides of the outline never change."
  (let* ((tris (coerce (mapcar #'copy-list tris) 'vector)) (v (coerce pts 'vector))
         (edge-tris (make-hash-table :test #'equal)))
    (labels ((key (a b) (if (< a b) (cons a b) (cons b a)))
             (index () (clrhash edge-tris)
               (loop for tr across tris for k from 0
                     do (loop for (a b) on (append tr (list (first tr))) while b do (push k (gethash (key a b) edge-tris)))))
             (other (tr a b) (find-if (lambda (x) (and (/= x a) (/= x b))) tr))
             (incircle (a b c d)
               (let* ((ax (- (px a) (px d))) (ay (- (py a) (py d))) (bx (- (px b) (px d))) (by (- (py b) (py d)))
                      (cx (- (px c) (px d))) (cy (- (py c) (py d))))
                 (> (- (+ (* (+ (* ax ax) (* ay ay)) (- (* bx cy) (* cx by)))
                          (* (+ (* cx cx) (* cy cy)) (- (* ax by) (* bx ay))))
                       (* (+ (* bx bx) (* by by)) (- (* ax cy) (* cx ay))))
                    1d-12))))
      (dotimes (pass 400)
        (index)
        (let ((flipped 0))
          (maphash (lambda (e ks)
                     (when (= (length ks) 2)
                       (let* ((k1 (first ks)) (k2 (second ks)) (t1 (aref tris k1)) (t2 (aref tris k2)))
                         (when (and (member (car e) t1) (member (cdr e) t1) (member (car e) t2) (member (cdr e) t2))
                           ;; T1 = (a b c) counterclockwise with the shared side a-b; D the far corner of T2
                           (let* ((r (let ((x t1))
                                       (dotimes (i 3 x)
                                         (when (equal (key (first x) (second x)) e) (return x))
                                         (setf x (append (rest x) (list (first x)))))))
                                  (a (first r)) (b (second r)) (c (third r)) (dd (other t2 a b)))
                             (when (and dd (incircle (aref v a) (aref v b) (aref v c) (aref v dd))
                                        ;; the new triangles must both turn the right way
                                        (> (tri-area2 (aref v c) (aref v a) (aref v dd)) 1d-18)
                                        (> (tri-area2 (aref v dd) (aref v b) (aref v c)) 1d-18))
                               (setf (aref tris k1) (list c a dd) (aref tris k2) (list dd b c))
                               (incf flipped)
                               ;; the sides of these two triangles changed: wait for the next pass
                               (setf (gethash e edge-tris) nil)
                               (dolist (s2 (list (key a c) (key b c) (key a dd) (key b dd))) (setf (gethash s2 edge-tris) nil))))))))
                   edge-tris)
          (when (zerop flipped) (return)))))
    (coerce tris 'list)))

(defun grid-step (surf poly tol)
  "Spacing (in the laid-out coordinates) of the points put inside a curved face, so its
triangles follow the surface: (values step-u step-v)."
  (let* ((us (mapcar (lambda (r) (px (first r))) poly)) (vs (mapcar (lambda (r) (py (first r))) poly))
         (wu (max 1d-12 (- (reduce #'max us) (reduce #'min us))))
         (wv (max 1d-12 (- (reduce #'max vs) (reduce #'min vs))))
         (sc (or (asurf-scale surf) 1d0))
         (turn (lambda (r) (* r 2 (acos (max -1d0 (min 1d0 (- 1 (/ tol (max r 1d-12))))))))))  ; arc per chord
    (case (asurf-kind surf)
      (:cone (let ((su (max (/ wu 96) (funcall turn sc))))
               (values su (max su (/ wv 6)))))
      ((:sphere :torus) (let ((st (max (/ (max wu wv) 96) (funcall turn sc)))) (values st st)))
      (t (values (/ wu 16) (/ wv 16))))))

(defun point-in-ring-p (p ring)
  (let ((inside nil) (x (px p)) (y (py p)))
    (loop for (a b) on (append ring (list (first ring))) while b
          do (when (and (not (eq (> (py a) y) (> (py b) y)))
                        (< x (+ (px a) (/ (* (- y (py a)) (- (px b) (px a))) (- (py b) (py a))))))
               (setf inside (not inside))))
    inside))

(defun seg-distance-2d (p a b)
  (let* ((dx (- (px b) (px a))) (dy (- (py b) (py a))) (l2 (+ (* dx dx) (* dy dy)))
         (tt (if (zerop l2) 0 (max 0 (min 1 (/ (+ (* (- (px p) (px a)) dx) (* (- (py p) (py a)) dy)) l2))))))
    (dist2d p (pt (+ (px a) (* tt dx)) (+ (py a) (* tt dy))))))

(defun insert-into-triangles (pts tris new)
  "Put each 2D point of NEW into the triangulation (PTS a vector, TRIS index triples): the
triangle it falls in is split in three. (values pts tris)."
  (let ((pts (make-array (length pts) :adjustable t :fill-pointer t :initial-contents pts))
        (tris (coerce tris 'list)))
    (dolist (p new)
      (let ((hit (find-if (lambda (tr) (destructuring-bind (a b c) tr
                                         (and (> (tri-area2 (aref pts a) (aref pts b) p) 1d-14)
                                              (> (tri-area2 (aref pts b) (aref pts c) p) 1d-14)
                                              (> (tri-area2 (aref pts c) (aref pts a) p) 1d-14))))
                          tris)))
        (when hit
          (let ((m (vector-push-extend p pts)))
            (destructuring-bind (a b c) hit
              (setf tris (list* (list a b m) (list b c m) (list c a m) (remove hit tris :test #'eq))))))))
    (values pts tris)))

(defun face-triangles (loops3d surf tol)
  "Triangles of a face: (values triangles real-edges), triangles as point triples, real-edges as
point pairs."
  (let ((poly (ignore-errors (face-polygon loops3d surf))))
    (when (and poly (>= (length poly) 3))
      (let* ((flat (member (asurf-kind surf) '(:plane :flat)))
             (ring (mapcar #'first poly))
             (sc (or (asurf-scale surf) 1d0))
             (tris (ear-clip ring))
             (verts (coerce (mapcar (lambda (r) (list (second r) (third r))) poly) 'vector))
             (fixed (make-hash-table :test #'equal))
             (n (length poly)))
        (loop for r in poly for k from 0
              when (fourth r) do (let ((a k) (b (mod (1+ k) n))) (setf (gethash (if (< a b) (cons a b) (cons b a)) fixed) t)))
        (unless flat
          ;; points spread inside a curved face, so the triangles can bend with it
          (multiple-value-bind (su sv) (grid-step surf poly tol)
            (let* ((x0 (reduce #'min ring :key #'px)) (x1 (reduce #'max ring :key #'px))
                   (y0 (reduce #'min ring :key #'py)) (y1 (reduce #'max ring :key #'py))
                   (margin (* 0.3 (min su sv)))
                   (inner (loop for x from (+ x0 (/ su 2)) below x1 by su
                                append (loop for y from (+ y0 (/ sv 2)) below y1 by sv
                                             for p = (pt x y)
                                             when (and (point-in-ring-p p ring)
                                                       (loop for (a b) on (append ring (list (first ring))) while b
                                                             never (< (seg-distance-2d p a b) margin)))
                                               collect p))))
              (when (and inner (< (length inner) 20000))
                (multiple-value-bind (pts2 tris2) (insert-into-triangles (coerce ring 'vector) tris inner)
                  (setf tris tris2
                        verts (concatenate 'vector verts
                                           (map 'list (lambda (p) (let ((uv (pt (/ (px p) sc) (py p))))
                                                                    (list uv (funcall (asurf-eval surf) (px uv) (py uv)))))
                                                (subseq pts2 n)))
                        ring (coerce pts2 'list)))))))
        (setf tris (delaunay-flips ring tris))
        (multiple-value-bind (vs ts)
            (if flat
                (values verts (coerce tris 'vector))
                ;; edges may sit a little off their surface (AutoCAD allows it): follow the surface
                ;; no closer than its edges do, or the dividing would never end
                (let ((off (loop for r in poly maximize (dist (funcall (asurf-eval surf) (px (second r)) (py (second r))) (third r)))))
                  (refine-triangles verts tris fixed surf (max tol (* 1.5 off)) 8000)))
          (when *acis-debug* (setf *acis-debug* (list poly vs ts sc)))
          (values (map 'list (lambda (tr) (mapcar (lambda (i) (second (aref vs i))) tr)) ts)
                  (loop for r in poly for k from 0
                        when (fourth r) collect (list (third r) (third (nth (mod (1+ k) n) poly))))))))))

;;; ---------- whole bodies ----------

(defun whole-surface-triangles (surf tol)
  "Triangles covering all of a closed surface: a sphere (pole to pole) or a torus."
  (let* ((r (or (asurf-scale surf) 1d0))
         (step (* 2 (acos (max -1d0 (min 1d0 (- 1 (/ tol (max r 1d-12))))))))
         (nu (max 12 (min 192 (ceiling (* 2 pi) step))))
         (sphere (eq (asurf-kind surf) :sphere))
         (v0 (if sphere (- (/ pi 2)) 0d0)) (v1 (if sphere (/ pi 2) (* 2 pi)))
         (nv (max 6 (min 192 (ceiling (- v1 v0) step))))
         (grid (make-array (list (1+ nu) (1+ nv)))))
    (dotimes (i (1+ nu))
      (dotimes (j (1+ nv))
        (setf (aref grid i j) (funcall (asurf-eval surf) (* 2 pi (/ (mod i nu) nu)) (+ v0 (* (- v1 v0) (/ (if sphere j (mod j nv)) nv)))))))
    (let ((out nil))
      (dotimes (i nu out)
        (dotimes (j nv)
          (let ((a (aref grid i j)) (b (aref grid (1+ i) j)) (c (aref grid (1+ i) (1+ j))) (e (aref grid i (1+ j))))
            ;; at a sphere's poles the four corners close up into a triangle
            (unless (< (dist a b) 1d-12) (push (list a b c) out))
            (unless (< (dist c e) 1d-12) (push (list a c e) out))))))))

(defun sab-faces ()
  "Every face record of the body (all lumps and shells)."
  (loop for k from 0 below (length *sab*)
        when (equal (rec-name (rec k)) "face") collect k))

(defun sab-transform ()
  "The body's own placement (a function on points), or NIL."
  (let* ((body (loop for k below (length *sab*) when (equal (rec-name (rec k)) "body") return (rec k)))
         (tr (and body (rec (tokp body 6)))))
    (when (and tr (equal (rec-name tr) "transform"))
      (let ((ds (loop for k from 1 below (length tr) when (eq (car (aref tr k)) :d) collect (cdr (aref tr k)))))
        (when (>= (length ds) 13)
          (destructuring-bind (a b c dd e f g h i tx ty tz s &rest more) ds
            (declare (ignore more))
            (lambda (p) (pt (+ (* s (+ (* (px p) a) (* (py p) dd) (* (pz p) g))) tx)
                            (+ (* s (+ (* (px p) b) (* (py p) e) (* (pz p) h))) ty)
                            (+ (* s (+ (* (px p) c) (* (py p) f) (* (pz p) i))) tz)))))))))

(defun sab-mesh (bytes &optional (closed t))
  "A Fancy Pencil mesh (POLYFACE data) for the ACIS object in BYTES, or NIL."
  (let ((*sab* (sab-records bytes)))
    (when (and *sab* (plusp (length *sab*)))
      (let* ((pts (loop for k below (length *sab*) when (equal (rec-name (rec k)) "point") collect (tokv (rec k) 4)))
             (size (if pts
                       (max 1d-6 (loop for p in pts maximize (dist p (first pts))))
                       ;; no corners at all (a whole ball or ring): its own size
                       (or (loop for k below (length *sab*)
                                 for r = (rec k)
                                 when (equal (rec-name r) "sphere-surface") return (* 2 (abs (d (tokv r 5))))
                                 when (equal (rec-name r) "torus-surface") return (* 2 (+ (abs (d (tokv r 6))) (abs (d (tokv r 7))))))
                           1d0)))
             (*acis-tol* (* size 0.0015))
             (*edge-points* (make-hash-table))
             (tris nil) (edges nil))
        (dolist (fk (sab-faces))
          (let* ((f (rec fk))
                 (loops (loop for lk = (tokp f 5) then (tokp (rec lk) 4)
                              while lk collect (loop-segments lk)))
                 ;; a loop that is only a point (the tip of a cone) adds nothing to lay out
                 (loops (remove-if (lambda (l) (< (length (remove-duplicates (segments-points l) :test #'equalp)) 2)) loops))
                 (outlines (mapcar #'segments-points loops)))
            ;; a face with no edges at all: the whole of a closed surface (a ball, a ring)
            (unless loops
              (let ((surf (ignore-errors (surface-of (rec (tokp f 8)) nil))))
                (when (and surf (member (asurf-kind surf) '(:sphere :torus)))
                  (setf tris (nconc tris (whole-surface-triangles surf *acis-tol*))))))
            (when loops
              (let ((surf (or (ignore-errors (surface-of (rec (tokp f 8)) outlines)) (flat-asurf outlines))))
                (multiple-value-bind (ts es) (face-triangles loops surf *acis-tol*)
                  (setf tris (nconc tris ts) edges (nconc edges es)))))))
        (when tris
          (let ((xf (sab-transform)))
            (when xf
              (setf tris (mapcar (lambda (tr) (mapcar xf tr)) tris)
                    edges (mapcar (lambda (e) (mapcar xf e)) edges))))
          (triangles-to-acis-mesh tris edges closed (* size 1d-7)))))))

(defun triangles-to-acis-mesh (tris edges closed tol)
  "Mesh data from triangles: corners shared, faces turned the same way round (outward for a
closed solid), only the object's real edges showing."
  (multiple-value-bind (verts faces) (index-triangles tris tol)
    (let* ((vt (coerce verts 'vector))
           (h (make-hash-table :test #'equal))
           (key (lambda (p) (list (round (px p) tol) (round (py p) tol) (round (pz p) tol))))
           (index-of (lambda (p) (let ((k (funcall key p))) (loop for i below (length vt) when (equal (funcall key (aref vt i)) k) return i)))))
      (declare (ignorable index-of))
      ;; real edges, by the corners at their ends
      (let ((corner (make-hash-table :test #'equal)))
        (loop for i below (length vt) do (setf (gethash (funcall key (aref vt i)) corner) i))
        (dolist (e edges)
          (let ((a (gethash (funcall key (first e)) corner)) (b (gethash (funcall key (second e)) corner)))
            (when (and a b (/= a b)) (setf (gethash (if (< a b) (cons a b) (cons b a)) h) t)))))
      (let ((faces (orient-triangles (coerce faces 'vector))))
        (when (and closed (minusp (loop for f across faces
                                        sum (let ((a (aref vt (first f))) (b (aref vt (second f))) (c (aref vt (third f))))
                                              (vdot3 a (cross3 b c))))))
          (setf faces (map 'vector #'reverse faces)))
        (make-mesh-data (coerce vt 'list)
                        (loop for f across faces
                              collect (loop for (a b) on (append f (list (first f))) while b
                                            collect (if (gethash (if (< a b) (cons a b) (cons b a)) h) (1+ a) (- (1+ a))))))))))

(defun orient-triangles (faces)
  "FACES (index triples) turned so neighbours agree: each one run so a shared edge goes opposite
ways in the two triangles beside it."
  (let ((edge-faces (make-hash-table :test #'equal)) (done (make-array (length faces) :initial-element nil)))
    (loop for f across faces for k from 0
          do (loop for (a b) on (append f (list (first f))) while b
                   do (push k (gethash (if (< a b) (cons a b) (cons b a)) edge-faces))))
    (dotimes (start (length faces))
      (unless (aref done start)
        (setf (aref done start) t)
        (let ((queue (list start)))
          (loop while queue
                do (let* ((k (pop queue)) (f (aref faces k)))
                     (loop for (a b) on (append f (list (first f))) while b
                           do (dolist (j (gethash (if (< a b) (cons a b) (cons b a)) edge-faces))
                                (unless (aref done j)
                                  (let* ((g (aref faces j))
                                         (same (loop for (x y) on (append g (list (first g))) while y thereis (and (= x a) (= y b)))))
                                    (when same (setf (aref faces j) (reverse g)))
                                    (setf (aref done j) t)
                                    (push j queue))))))))))
    faces))

;;; ---------- from a DXF file ----------


(defun hex-bytes (strings)
  (let* ((total (reduce #'+ strings :key (lambda (s) (floor (length (string-trim " " s)) 2))))
         (out (make-array total :element-type '(unsigned-byte 8))) (k 0))
    (dolist (s strings out)
      (let ((s (string-trim " " s)))
        (loop for i from 0 below (1- (length s)) by 2
              do (setf (aref out k) (parse-integer s :start i :end (+ i 2) :radix 16)) (incf k))))))

(defun acds-data (section)
  "ACDSDATA SECTION pairs -> hash of handle -> ACIS bytes."
  (let ((h (make-hash-table :test #'equalp)) (handle nil) (chunks nil))
    (flet ((finish ()
             (when (and handle chunks)
               (let ((bytes (ignore-errors (hex-bytes (reverse chunks)))))
                 (when (and bytes (> (length bytes) 15) (= (aref bytes 0) 65))   ; "ACIS ..."
                   (setf (gethash (princ-to-string handle) h) bytes))))
             (setf handle nil chunks nil)))
      (dolist (p section)
        (case (car p)
          (0 (finish))
          (320 (setf handle (cdr p)))
          (310 (push (princ-to-string (cdr p)) chunks))))
      (finish))
    h))

(defun sat-text-bytes (g)
  "An older DXF's ACIS (text, in groups 1 and 3 of the object itself): not read yet."
  (declare (ignore g))
  nil)

(defun acis-entity-from-group (g)
  "POLYFACE data for a 3DSOLID / REGION / SURFACE / BODY group G, or NIL."
  (let* ((type (cdr (first g)))
         (handle (dxf 5 g))
         (bytes (and *acis-data* handle (gethash (princ-to-string handle) *acis-data*))))
    (when bytes
      (let ((mesh (ignore-errors (sab-mesh bytes (member type '("3DSOLID" "BODY") :test #'equal)))))
        (when mesh
          (append mesh
                  (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370 48 420)))) g)
                  (unless (equal type "3DSOLID")
                    (list (cons :kind (or (second (assoc type '(("REGION" "Region") ("SURFACE" "Surface") ("BODY" "Body")
                                                                    ("EXTRUDEDSURFACE" "Extruded surface") ("REVOLVEDSURFACE" "Revolved surface")
                                                                    ("LOFTEDSURFACE" "Lofted surface") ("SWEPTSURFACE" "Swept surface")
                                                                    ("PLANESURFACE" "Planar surface") ("NURBSURFACE" "NURBS surface"))
                                                         :test #'equal))
                                          "Surface"))))))))))
