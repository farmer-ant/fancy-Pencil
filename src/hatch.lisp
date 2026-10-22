;;;; hatch.lisp — HATCH entities: patterns, boundaries, rendering, DXF import, boundary detection.
;;;;
;;;; Internal form of a HATCH alist:
;;;;   (0 . "HATCH") (8 . layer) (2 . pattern-name) (70 . 1 if solid) (41 . scale) (52 . angle-radians)
;;;;   (:loops . (loop ...))      each loop is a closed list of (point bulge)
;;;;   (:pattern . (line ...))    optional, only for hatches read from DXF: each line is
;;;;                              (angle base-point offset-vector dashes) in world terms.

(in-package :fancy-pencil)

;;; ---------- pattern library (acad.pat, imperial) ----------
;;; Each line: (angle-degrees x0 y0 dx dy &rest dashes) where (dx dy) is in the line's own frame.

(defparameter *hatch-patterns*
  '(("SOLID" "Solid fill")
    ("ANSI31" "ANSI Iron, Brick, Stone masonry" (45 0 0 0 .125))
    ("ANSI32" "ANSI Steel" (45 0 0 0 .375) (45 .176776695 0 0 .375))
    ("ANSI33" "ANSI Bronze, Brass, Copper" (45 0 0 0 .25) (45 .176776695 0 0 .25 .125 -.0625))
    ("ANSI34" "ANSI Plastic, Rubber" (45 0 0 0 .75) (45 .176776695 0 0 .75) (45 .353553391 0 0 .75) (45 .530330086 0 0 .75))
    ("ANSI35" "ANSI Fire brick, Refractory material" (45 0 0 0 .25) (45 .176776695 0 0 .25 .3125 -.0625 0 -.0625))
    ("ANSI36" "ANSI Marble, Slate, Glass" (45 0 0 .21875 .125 .3125 -.0625 0 -.0625))
    ("ANSI37" "ANSI Lead, Zinc, Magnesium, Sound/Heat/Elec Insulation" (45 0 0 0 .125) (135 0 0 0 .125))
    ("ANSI38" "ANSI Aluminum" (45 0 0 0 .125) (135 0 0 .25 .125 .3125 -.1875))
    ("LINE" "Parallel horizontal lines" (0 0 0 0 .125))
    ("NET" "Horizontal / vertical grid" (0 0 0 0 .125) (90 0 0 0 .125))
    ("NET3" "Network pattern 0-60-120" (0 0 0 0 .125) (60 0 0 0 .125) (120 0 0 0 .125))
    ("DASH" "Dashed lines" (0 0 0 .125 .125 .125 -.125))
    ("DOTS" "A series of dots" (0 0 0 .03125 .0625 0 -.0625))
    ("BRICK" "Brick or masonry-type surface" (0 0 0 0 .25) (90 0 0 .25 .25 .25 -.25))
    ("SQUARE" "Small aligned squares" (0 0 0 0 .125 .125 -.125) (90 0 0 0 .125 .125 -.125))
    ("CROSS" "A series of crosses" (0 0 0 .25 .25 .125 -.375) (90 .0625 -.0625 .25 .25 .125 -.375))
    ("ANGLE" "Angle steel" (0 0 0 0 .275 .2 -.075) (90 0 0 0 .275 .2 -.075))
    ("HONEY" "Honeycomb pattern" (0 0 0 .1875 .108253175 .125 -.25) (120 0 0 .1875 .108253175 .125 -.25)
     (60 .125 0 .1875 .108253175 .125 -.25))
    ("ZIGZAG" "Staircase effect" (0 0 0 .125 .125 .125 -.125) (90 .125 0 .125 .125 .125 -.125))
    ("EARTH" "Earth or ground (subterranean)"
     (0 0 0 .25 .25 .25 -.25) (0 0 .09375 .25 .25 .25 -.25) (0 0 .1875 .25 .25 .25 -.25)
     (90 .03125 .21875 .25 .25 .25 -.25) (90 .125 .21875 .25 .25 .25 -.25) (90 .21875 .21875 .25 .25 .25 -.25))
    ("STEEL" "Steel material" (45 0 0 0 .125) (45 0 .0625 0 .125))
    ("INSUL" "Insulation material" (0 0 0 0 .375) (0 0 .125 0 .375 .125 -.125) (0 0 .25 0 .375 .125 -.125))))

(defun hatch-pattern-def (name) (assoc name *hatch-patterns* :test #'string-equal))
(defun solid-pattern-p (name) (string-equal name "SOLID"))

(defun pattern-lines-world (name scale angle)
  "Pattern line families for NAME as (angle base offset dashes) in world units."
  (let ((def (hatch-pattern-def name)))
    (loop for (adeg x0 y0 dx dy . dashes) in (cddr def)
          for a = (+ (rad adeg) angle)
          collect (list (norm-ang a)
                        (rot-pt (pt (* x0 scale) (* y0 scale)) (pt 0 0) angle)
                        (rot-pt (pt (* dx scale) (* dy scale)) (pt 0 0) a)
                        (mapcar (lambda (x) (* (d x) scale)) dashes)))))

(defun hatch-pattern-lines (data)
  (or (dxf :pattern data)
      (pattern-lines-world (or (dxf 2 data) "ANSI31") (d (or (dxf 41 data) 1)) (d (or (dxf 52 data) 0)))))

;;; ---------- loops ----------

(defun loop-pieces (lp)
  (let ((n (length lp)))
    (loop for i from 0 below n
          for (p b) = (nth i lp)
          for q = (first (nth (mod (1+ i) n) lp))
          unless (< (dist2d p q) 1d-12)
            collect (if (< (abs b) 1d-12) (list :seg p q)
                        (multiple-value-bind (c r a1 a2) (bulge->arc p q b) (list :arc c r a1 a2))))))

(defun loop-polygon-directed (lp &optional (arc-n 24))
  "Like LOOP-POLYGON but arcs are traversed in the loop's own direction."
  (let ((n (length lp)) (out nil))
    (loop for i from 0 below n
          for (p b) = (nth i lp)
          for q = (first (nth (mod (1+ i) n) lp))
          do (if (< (abs b) 1d-12)
                 (push p out)
                 (multiple-value-bind (c r) (bulge->arc p q b)
                   (let* ((a0 (ang c p)) (sweep (* 4 (atan b))))
                     (loop for k from 0 below arc-n
                           do (push (polar c (+ a0 (* sweep (/ k arc-n))) r) out))))))
    (nreverse out)))

(defun hatch-loops (data) (dxf :loops data))

(defun hatch-pieces (data)
  (loop for lp in (hatch-loops data) append (loop-pieces lp)))

(defun point-in-hatch-p (p data)
  (let ((inside nil))
    (dolist (lp (hatch-loops data) inside)
      (when (point-in-polygon-p p (loop-polygon-directed lp 16)) (setf inside (not inside))))))

(defun loop-signed-area (poly)
  (/ (loop for (a b) on (append poly (list (first poly))) while b
           sum (- (* (px a) (py b)) (* (px b) (py a))))
     2))

;;; ---------- clipping a family of parallel lines against the loops ----------

(defun hatch-line-intervals (p0 u polys)
  "Even-odd inside intervals (t1 t2) of the line P0 + t*U against closed polygons POLYS."
  (let ((ts nil))
    (dolist (poly polys)
      (loop for (a b) on (append poly (list (first poly))) while b
            do (let* ((ex (- (px b) (px a))) (ey (- (py b) (py a)))
                      (den (- (* (px u) ey) (* (py u) ex))))
                 (when (> (abs den) 1d-15)
                   (let* ((wx (- (px a) (px p0))) (wy (- (py a) (py p0)))
                          (tt (/ (- (* wx ey) (* wy ex)) den))
                          (s (/ (- (* wx (py u)) (* wy (px u))) den)))
                     ;; half-open edge test avoids double counting at vertices
                     (when (and (>= s 0) (< s 1)) (push tt ts)))))))
    (setf ts (sort ts #'<))
    (loop for (a b) on ts by #'cddr while b collect (list a b))))

(defun hatch-family-segments (family polys bbox &optional (max-lines 3000))
  "World segments for one pattern line family, or :too-dense."
  (destructuring-bind (a base off dashes) family
    (let* ((u (pt (cos a) (sin a))) (nrm (pt (- (sin a)) (cos a)))
           (spacing (+ (* (px off) (px nrm)) (* (py off) (py nrm))))
           (shift (+ (* (px off) (px u)) (* (py off) (py u))))
           (corners (destructuring-bind (x0 y0 x1 y1) bbox
                      (list (pt x0 y0) (pt x1 y0) (pt x1 y1) (pt x0 y1))))
           (proj (mapcar (lambda (c) (+ (* (- (px c) (px base)) (px nrm)) (* (- (py c) (py base)) (py nrm)))) corners)))
      (when (< (abs spacing) 1d-12) (return-from hatch-family-segments nil))
      (when (and (eq *ui* :gui) (not *plotting*) (< (* (abs spacing) *vscale*) 2d0))
        (return-from hatch-family-segments :too-dense))
      (let ((k0 (floor (/ (reduce #'min proj) spacing))) (k1 (ceiling (/ (reduce #'max proj) spacing))))
        (when (> k0 k1) (rotatef k0 k1))
        (when (> (- k1 k0) max-lines) (return-from hatch-family-segments :too-dense))
        (let ((period (reduce #'+ (mapcar #'abs dashes))) (out nil))
          (loop for k from k0 to k1
                for p0 = (v+ base (v+ (v* nrm (* k spacing)) (v* u (* k shift))))
                do (dolist (iv (hatch-line-intervals p0 u polys))
                     (destructuring-bind (t1 t2) iv
                       (if (or (null dashes) (< period 1d-12))
                           (push (list (v+ p0 (v* u t1)) (v+ p0 (v* u t2))) out)
                           (let ((n0 (floor t1 period)))
                             (loop for cyc from n0
                                   for start = (* cyc period)
                                   while (< start t2)
                                   do (let ((pos start))
                                        (dolist (dl dashes)
                                          (let ((len (abs dl)))
                                            (when (>= dl 0)
                                              (let ((s1 (max t1 pos)) (s2 (min t2 (+ pos (max len (/ 0.75d0 *vscale*))))))
                                                (when (< s1 s2)
                                                  (push (list (v+ p0 (v* u s1)) (v+ p0 (v* u s2))) out))))
                                            (incf pos len))))
                                   when (> (- cyc n0) 20000) do (return)))))))
          out)))))

(defun hatch-world-polys (data xf)
  (mapcar (lambda (lp)
            (let ((poly (loop-polygon-directed lp 24)))
              (if xf (mapcar (lambda (p) (xf-point xf p)) poly) poly)))
          (hatch-loops data)))

(defun render-hatch (data color tags hl xf)
  (let* ((polys (remove-if (lambda (p) (< (length p) 3)) (hatch-world-polys data xf)))
         (bbox (points-bbox (apply #'append polys)))
         (out nil))
    (when (and polys bbox (piece-visible-p (list :seg (pt (first bbox) (second bbox)) (pt (third bbox) (fourth bbox)))))
      (cond
        ;; solid, single loop: a filled polygon is exact and fast
        ((and (or (eql (dxf 70 data) 1) (solid-pattern-p (or (dxf 2 data) ""))) (= (length polys) 1))
         (let ((coords (loop for p in (first polys) append (multiple-value-list (w->s p)))))
           (when (every (lambda (c) (< -30000 c 30000)) coords)
             (push (format nil ".c create polygon ~{~A~^ ~} -fill ~A -outline ~A~A -tags {~A}"
                           (mapcar #'fmtn coords) color color (if hl " -stipple gray50" "") tags)
                   out))))
        ;; solid with islands: scanlines in world space, two pixels apart
        ((or (eql (dxf 70 data) 1) (solid-pattern-p (or (dxf 2 data) "")))
         (let ((segs (hatch-family-segments (list 0d0 (pt 0 (+ (second bbox) (/ 1d0 *vscale*))) (pt 0 (/ 2d0 *vscale*)) nil) polys bbox 4000)))
           (unless (eq segs :too-dense)
             (dolist (s segs) (setf out (nconc out (screen-line s color tags :width 2)))))))
        (t
         (dolist (fam (if xf
                          (mapcar (lambda (f) (destructuring-bind (a base off dashes) f
                                                (list (funcall (xf-ang xf) a) (xf-point xf base)
                                                      (v- (xf-point xf (v+ (pt 0 0) off)) (xf-point xf (pt 0 0)))
                                                      (mapcar (lambda (x) (* x (xf-scale xf))) dashes))))
                                  (hatch-pattern-lines data))
                          (hatch-pattern-lines data)))
           (let ((segs (hatch-family-segments fam polys bbox)))
             (if (eq segs :too-dense)
                 ;; too fine to draw at this zoom: show a light fill like AutoCAD's dense hatch
                 (let ((coords (loop for p in (first polys) append (multiple-value-list (w->s p)))))
                   (when (every (lambda (c) (< -30000 c 30000)) coords)
                     (push (format nil ".c create polygon ~{~A~^ ~} -fill ~A -outline {} -stipple gray25 -tags {~A}"
                                   (mapcar #'fmtn coords) color tags) out)))
                 (dolist (s segs) (setf out (nconc out (screen-line s color tags :dash hl))))))))))
    out))

;;; ---------- DXF import ----------

(defun hatch-from-dxf (g &key (degrees t))
  "Convert raw HATCH group pairs (code . value) to the internal form.
DEGREES is true for DXF files and NIL for AutoLISP lists (angles already in radians)."
  (let ((pairs (coerce g 'vector)) (i 0) (n (length g))
        (common nil) (loops nil) (loop-flags nil) (pattern nil) (name "ANSI31") (solid 0) (scale 1d0) (angle 0d0)
        (style nil))
    (labels ((peek () (and (< i n) (car (aref pairs i))))
             (next () (prog1 (cdr (aref pairs i)) (incf i)))
             (take (code) (when (eql (peek) code) (next)))
             (xy () (let ((x (or (take 10) (take 11) 0d0)) (y (or (take 20) (take 21) 0d0))) (pt x y)))
             (din (x) (if degrees (rad x) x)))
      ;; header until the first boundary path (code 91)
      (loop while (and (< i n) (not (eql (peek) 91)))
            do (let ((c (peek)))
                 (case c
                   ((8 62 6 370 48 420 440) (push (cons c (next)) common))
                   (2 (setf name (next)))
                   (70 (setf solid (next)))
                   (t (next)))))
      (let ((nloops (or (take 91) 0)))
        (dotimes (li nloops)
          (let ((flags (or (take 92) 0)) (verts nil))
            (if (logtest flags 2)
                (let ((has-bulge (eql (take 72) 1)) (cnt (progn (take 73) (or (take 93) 0))))
                  (dotimes (k cnt)
                    (let ((p (pt (or (take 10) 0d0) (or (take 20) 0d0)))
                          (b (if has-bulge (or (take 42) 0d0) 0d0)))
                      (push (list p b) verts))))
                (let ((ne (or (take 93) 0)))
                  (dotimes (k ne)
                    (let ((et (take 72)))
                      (case et
                        (1 (let ((a (pt (or (take 10) 0d0) (or (take 20) 0d0)))
                                 (b (pt (or (take 11) 0d0) (or (take 21) 0d0))))
                             (declare (ignore b))
                             (push (list a 0d0) verts)))
                        (2 (let* ((c (pt (or (take 10) 0d0) (or (take 20) 0d0)))
                                  (r (or (take 40) 0d0)) (s (din (or (take 50) 0d0))) (e (din (or (take 51) 0d0)))
                                  (ccw (eql (take 73) 1)))
                             (cond
                               ;; a whole circle (0 to 360): two half arcs, or it would collapse to a point
                               ((>= (abs (- e s)) (- +2pi+ 1d-6))
                                (let ((b (if ccw 1d0 -1d0)) (a0 (if ccw s (- s))))
                                  (push (list (polar c a0 r) b) verts)
                                  (push (list (polar c (+ a0 +pi+) r) b) verts)))
                               (ccw
                                (push (list (polar c s r) (tan (/ (arc-sweep s e) 4))) verts))
                               (t
                                (let ((a1 (- s)) (a2 (- e)))
                                  (push (list (polar c a1 r) (- (tan (/ (norm-ang (- a1 a2)) 4)))) verts))))))
                        (3 (let* ((c (pt (or (take 10) 0d0) (or (take 20) 0d0)))
                                  (m (pt (or (take 11) 0d0) (or (take 21) 0d0)))
                                  (ratio (or (take 40) 1d0)) (s (din (or (take 50) 0d0))) (e (din (or (take 51) +2pi+)))
                                  (ccw (eql (take 73) 1))
                                  (pts (ellipse-points (list (cons 10 c) (cons 11 m) (cons 40 ratio)
                                                             (cons 41 (if ccw s (- e))) (cons 42 (if ccw e (- s))))
                                                       32)))
                             (dolist (p (butlast (if ccw pts (reverse pts)))) (push (list p 0d0) verts))))
                        (4 (let* ((deg (or (take 94) 3)) (knots nil) (ctrl nil))
                             (take 73) (take 74)
                             (let ((nk (or (take 95) 0)) (nc (or (take 96) 0)))
                               (dotimes (k nk) (push (cons 40 (or (take 40) 0d0)) knots))
                               (dotimes (k nc)
                                 (push (list 10 (or (take 10) 0d0) (or (take 20) 0d0) 0d0) ctrl)
                                 (take 42)))
                             (let ((nf (or (take 97) 0)))
                               (dotimes (k nf) (take 11) (take 21)))
                             (take 12) (take 22) (take 13) (take 23)
                             (dolist (p (butlast (spline-points (append (list (cons 71 deg)) (reverse knots) (reverse ctrl)))))
                               (push (list (pt3 p) 0d0) verts))))
                        (t nil))))))
            ;; source boundary objects
            (let ((ns (or (take 97) 0))) (dotimes (k ns) (take 330)))
            (when (> (length verts) 1) (push (nreverse verts) loops) (push flags loop-flags)))))
      ;; pattern data
      (loop while (< i n)
            do (let ((c (peek)))
                 (case c
                   (75 (setf style (next)))
                   (52 (setf angle (din (next))))
                   (41 (setf scale (next)))
                   (78 (let ((nl (next)))
                         (dotimes (k nl)
                           (let* ((a (din (or (take 53) 0d0)))
                                  (base (pt (or (take 43) 0d0) (or (take 44) 0d0)))
                                  (off (pt (or (take 45) 0d0) (or (take 46) 0d0)))
                                  (nd (or (take 79) 0))
                                  (dashes (loop repeat nd collect (or (take 49) 0d0))))
                             (push (list a base off dashes) pattern)))))
                   (t (next))))))
    (append (list (cons 0 "HATCH"))
            (nreverse common)
            (list (cons 2 name) (cons 70 solid) (cons 41 (d scale)) (cons 52 (d angle))
                  (cons :loops (nreverse loops)))
            ;; island style (75) and each boundary's flags (outer, text box ...), written back as found
            (when style (list (cons 75 style)))
            (when (some (lambda (f) (logtest f (logior 1 8 16))) loop-flags)
              (list (cons :loop-flags (nreverse loop-flags))))
            (when (and pattern (not (eql solid 1))) (list (cons :pattern (nreverse pattern)))))))

;;; ---------- transforms ----------

(defun xform-hatch (data xf)
  (let* ((fp (xf-fn xf))
         (loops (mapcar (lambda (lp)
                          (let ((l (mapcar (lambda (v) (list (funcall fp (pt3 (first v)))
                                                             (if (xf-mirror xf) (- (second v)) (second v))))
                                           lp)))
                            l))
                        (hatch-loops data)))
         (o (funcall fp (pt 0 0)))
         (pattern (dxf :pattern data))
         (d1 (set-dxf :loops loops data)))
    (setf d1 (set-dxf 41 (* (d (or (dxf 41 data) 1)) (xf-scale xf)) d1))
    (setf d1 (set-dxf 52 (norm-ang (if (xf-mirror xf) (dxf 52 data) (funcall (xf-ang xf) (or (dxf 52 data) 0d0)))) d1))
    (if pattern
        (set-dxf :pattern (mapcar (lambda (f) (destructuring-bind (a base off dashes) f
                                                (list (funcall (xf-ang xf) a) (funcall fp base)
                                                      (v- (funcall fp off) o)
                                                      (mapcar (lambda (x) (* x (xf-scale xf))) dashes))))
                                  pattern)
                 d1)
        d1)))

;;; ---------- boundary detection for "pick internal point" ----------

(defun boundary-segments (ents)
  "All boundary objects as straight segments (curves tessellated), split at intersections."
  (let ((segs nil))
    (dolist (e ents)
      (dolist (pc (snap-pieces e))
        (case (first pc)
          (:seg (push (list (second pc) (third pc)) segs))
          ((:arc :circle)
           (let ((pts (if (eq (first pc) :circle)
                          (arc-points (second pc) (third pc) 0d0 +2pi+ 64)
                          (arc-points (second pc) (third pc) (fourth pc) (fifth pc)
                                      (max 6 (ceiling (* 32 (/ (arc-sweep (fourth pc) (fifth pc)) +pi+))))))))
             (loop for (a b) on pts while b do (push (list a b) segs)))))))
    (let* ((v (coerce segs 'vector)) (cuts (make-array (length v) :initial-element nil)))
      ;; record intersection parameters on every segment
      (loop for i from 0 below (length v)
            do (loop for j from (1+ i) below (length v)
                     do (destructuring-bind (a b) (aref v i)
                          (destructuring-bind (c dd) (aref v j)
                            (when (and (<= (min (px a) (px b)) (max (px c) (px dd)))
                                       (<= (min (px c) (px dd)) (max (px a) (px b)))
                                       (<= (min (py a) (py b)) (max (py c) (py dd)))
                                       (<= (min (py c) (py dd)) (max (py a) (py b))))
                              (let ((q (line-line-int a b c dd t)))
                                (when q
                                  (push (seg-param q a b) (aref cuts i))
                                  (push (seg-param q c dd) (aref cuts j)))))))))
      (loop for i from 0 below (length v)
            append (destructuring-bind (a b) (aref v i)
                     (let ((ts (sort (remove-duplicates (cons 0d0 (cons 1d0 (remove-if-not (lambda (x) (<= 0 x 1)) (aref cuts i))))
                                                        :test (lambda (x y) (< (abs (- x y)) 1d-9)))
                                     #'<)))
                       (loop for (t1 t2) on ts while t2
                             for p = (at-param a b t1) for q = (at-param a b t2)
                             when (> (dist2d p q) 1d-9) collect (list p q))))))))

(defun node-key (p) (cons (round (px p) 1d-7) (round (py p) 1d-7)))

(defun trace-face (segs pick)
  "Closed polygon of the smallest face of the segment graph that contains PICK, or NIL."
  (let ((adj (make-hash-table :test #'equal)) (coords (make-hash-table :test #'equal)))
    (dolist (s segs)
      (let ((ka (node-key (first s))) (kb (node-key (second s))))
        (unless (equal ka kb)
          (setf (gethash ka coords) (first s) (gethash kb coords) (second s))
          (pushnew kb (gethash ka adj) :test #'equal)
          (pushnew ka (gethash kb adj) :test #'equal))))
    ;; nearest edge hit by a ray from PICK towards +X
    (let ((best nil) (bestx nil))
      (dolist (s segs)
        (destructuring-bind (a b) s
          (when (not (eq (> (py a) (py pick)) (> (py b) (py pick))))
            (let ((x (+ (px a) (/ (* (- (py pick) (py a)) (- (px b) (px a))) (- (py b) (py a))))))
              (when (and (> x (px pick)) (or (null bestx) (< x bestx)))
                (setf bestx x best s))))))
      (when best
        ;; orient the edge so PICK lies on its left
        (destructuring-bind (a b) best
          (let* ((u (if (> (cross2 a b pick) 0) (node-key a) (node-key b)))
                 (w (if (> (cross2 a b pick) 0) (node-key b) (node-key a)))
                 (start (cons u w)) (poly (list (gethash u coords))) (prev u) (cur w))
            (loop repeat 20000
                  do (push (gethash cur coords) poly)
                     (let* ((cp (gethash cur coords))
                            (back (ang cp (gethash prev coords)))
                            (cands (remove prev (gethash cur adj) :test #'equal))
                            (cands (or cands (list prev)))
                            ;; next edge: first one clockwise from the way we came in
                            (nxt (first (sort (copy-list cands) #'<
                                              :key (lambda (k) (let ((dlt (norm-ang (- back (ang cp (gethash k coords))))))
                                                                 (if (< dlt 1d-12) +2pi+ dlt)))))))
                       (setf prev cur cur nxt)
                       (when (equal (cons prev cur) start) (return)))
                  finally (return-from trace-face nil))
            (let ((poly (nreverse (cdr poly))))
              (when (and (> (length poly) 2) (> (loop-signed-area poly) 0) (point-in-polygon-p pick poly))
                poly))))))))

(defun closed-entity-loop (e)
  "A loop for closed objects (circle, closed polyline, full ellipse), else NIL."
  (let* ((data (ent-data e)) (type (dxf 0 data)))
    (cond ((equal type "CIRCLE")
           (let ((c (pt3 (dxf 10 data))) (r (dxf 40 data)))
             (list (list (polar c 0d0 r) 1d0) (list (polar c +pi+ r) 1d0))))
          ((and (equal type "LWPOLYLINE") (lwpoly-closed-p data)) (lwpoly-vertices data))
          ((and (equal type "LWPOLYLINE") (> (length (lwpoly-vertices data)) 2)
                (< (dist2d (first (first (lwpoly-vertices data))) (first (car (last (lwpoly-vertices data))))) 1d-9))
           (butlast (lwpoly-vertices data)))
          ((and (equal type "ELLIPSE") (< (abs (- (arc-sweep (or (dxf 41 data) 0d0) (or (dxf 42 data) +2pi+)) +2pi+)) 1d-6))
           (mapcar (lambda (p) (list p 0d0)) (butlast (ellipse-points data 64))))
          (t nil))))

(defun loop-inside-loop-p (inner outer)
  (let ((op (loop-polygon-directed outer 16)))
    (every (lambda (v) (point-in-polygon-p (first v) op)) inner)))

(defun detect-boundary (pick)
  "Loops for a hatch around PICK: the enclosing face plus closed islands inside it."
  (let* ((ents (remove-if (lambda (e) (member (ent-type e) '("TEXT" "MTEXT" "HATCH" "DIMENSION" "POINT") :test #'equal))
                          (remove-if-not #'entity-visible-p (live-entities))))
         ;; quick path: smallest closed object containing the point
         (closed (loop for e in ents
                       for lp = (closed-entity-loop e)
                       when (and lp (point-in-polygon-p pick (loop-polygon-directed lp 24)))
                         collect lp))
         (outer-poly (trace-face (boundary-segments (entities-near-region ents pick)) pick))
         (outer (cond (outer-poly (mapcar (lambda (p) (list p 0d0)) outer-poly))
                      (closed (first (sort closed #'< :key (lambda (lp) (abs (loop-signed-area (loop-polygon-directed lp 16)))))))))
         (outer (if (and outer-poly closed)
                    ;; prefer the exact closed object (keeps arcs) when it is the same region
                    (let ((best (first (sort closed #'< :key (lambda (lp) (abs (loop-signed-area (loop-polygon-directed lp 16))))))))
                      (if (< (abs (- (abs (loop-signed-area (loop-polygon-directed best 32)))
                                     (abs (loop-signed-area outer-poly))))
                             (* 0.02 (abs (loop-signed-area outer-poly))))
                          best outer))
                    outer)))
    (when outer
      (let ((islands (loop for e in ents
                           for lp = (closed-entity-loop e)
                           when (and lp (not (point-in-polygon-p pick (loop-polygon-directed lp 24)))
                                     (loop-inside-loop-p lp outer))
                             collect lp)))
        (cons outer islands)))))

(defun entities-near-region (ents pick)
  "Entities whose extents could bound a region around PICK (keeps the graph small)."
  (declare (ignore pick))
  (if (> (length ents) 800)
      (multiple-value-bind (x0 y0 x1 y1) (if (eq *ui* :gui) (view-rect 0) (values -1d30 -1d30 1d30 1d30))
        (remove-if-not (lambda (e) (let ((b (entity-bbox e)))
                                     (and b (not (or (> (first b) x1) (< (third b) x0) (> (second b) y1) (< (fourth b) y0))))))
                       ents))
      ents))

(defun make-hatch-data (loops &key (pattern (sysvar "HPNAME")) (scale (sysvar "HPSCALE")) (angle (sysvar "HPANG")))
  (list (cons 0 "HATCH") (cons 2 (string-upcase pattern)) (cons 70 (if (solid-pattern-p pattern) 1 0))
        (cons 41 (d scale)) (cons 52 (d angle)) (cons :loops loops)))

;;; ---------- AutoLISP view of a hatch (DXF-style groups, angles in radians) ----------

(defun hatch-to-groups (data)
  (let* ((name (or (dxf 2 data) "ANSI31"))
         (solid (or (eql (dxf 70 data) 1) (solid-pattern-p name)))
         (loops (hatch-loops data))
         (out nil))
    (flet ((g (code v) (push (cons code v) out)))
      (push (cons 0 "HATCH") out)
      (dolist (p data) (when (and (consp p) (member (car p) '(5 8 6 62 370 48))) (push p out)))
      (push (list 10 0d0 0d0 0d0) out) (push (list 210 0d0 0d0 1d0) out)
      (g 2 name) (g 70 (if solid 1 0)) (g 71 0) (g 91 (length loops))
      (loop for lp in loops for first = t then nil
            do (let ((bulgy (some (lambda (v) (/= (second v) 0)) lp)))
                 (g 92 (if first 3 2)) (g 72 (if bulgy 1 0)) (g 73 1) (g 93 (length lp))
                 (dolist (v lp)
                   (push (list 10 (px (first v)) (py (first v))) out)
                   (when bulgy (g 42 (d (second v)))))
                 (g 97 0)))
      (g 75 0) (g 76 1)
      (unless solid
        (let ((lines (hatch-pattern-lines data)))
          (g 52 (d (or (dxf 52 data) 0))) (g 41 (d (or (dxf 41 data) 1))) (g 77 0) (g 78 (length lines))
          (dolist (l lines)
            (destructuring-bind (a base off dashes) l
              (g 53 a) (g 43 (px base)) (g 44 (py base)) (g 45 (px off)) (g 46 (py off)) (g 79 (length dashes))
              (dolist (x dashes) (g 49 (d x)))))))
      (g 98 0))
    (nreverse out)))

(defun explode-coords (groups)
  "Split (10 x y [z]) pairs back into (10 . x) (20 . y) [(30 . z)]."
  (loop for p in groups
        append (if (and (consp p) (integerp (car p)) (consp (cdr p)) (realp (cadr p)))
                   (let ((c (car p)) (vals (cdr p)))
                     (loop for v in vals for k from 0 collect (cons (+ c (* 10 k)) (d v))))
                   (list p))))

(defun hatch-from-lisp (lst)
  "Internal hatch data from an AutoLISP (DXF-style) list, keeping layer/color properties."
  (let ((h (hatch-from-dxf (explode-coords (remove-if (lambda (p) (and (consp p) (member (car p) '(-1 5 330 100)))) lst))
                           :degrees nil)))
    h))
