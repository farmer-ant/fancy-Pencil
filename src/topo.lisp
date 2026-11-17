;;;; topo.lisp — terrain from survey elevation text, after the AutoCAD add-in TopoBuilder
;;;; (github.com/Vovenzza/Autocad_Topomaker), written afresh for Fancy Pencil:
;;;;   TOPOMODEL  every TEXT/MTEXT like a sample (same layer and color) gives a point: X,Y where
;;;;              the text is, Z the number it says. The points are triangulated (Delaunay) into a
;;;;              surface mesh (layer Topo_Surface_Visualization) and a 3D solid down to a base.
;;;;   ALTXT      moves elevation text onto the centre of the survey mark it belongs to (circles,
;;;;              arcs or blocks like a sample), so the points land where they were surveyed.

(in-package :fancy-pencil)

;;; ---------------------------------------------------------------- reading the elevations

(defun text-plain (data)
  "The words of a TEXT or MTEXT, without formatting codes."
  (let ((s (or (dxf 1 data) "")))
    (if (equal (dxf 0 data) "MTEXT")
        (substitute #\Space #\Newline (mtext-plain-text s))
        s)))

(defun first-number (s)
  "The first number written in S (a comma may be the decimal point), or NIL."
  (let ((start (position-if (lambda (c) (or (digit-char-p c) (and (member c '(#\- #\+)) ))) s)))
    (loop while start
          do (let* ((sign (and (member (char s start) '(#\- #\+)) (char s start)))
                    (i (if sign (1+ start) start))
                    (j (position-if-not #'digit-char-p s :start i)))
               (if (and (< i (length s)) (digit-char-p (char s i)))
                   (let* ((j (or j (length s)))
                          (k (if (and (< j (length s)) (member (char s j) '(#\. #\,))
                                      (< (1+ j) (length s)) (digit-char-p (char s (1+ j))))
                                 (or (position-if-not #'digit-char-p s :start (1+ j)) (length s))
                                 j))
                          (txt (substitute #\. #\, (subseq s i k)))
                          (v (ignore-errors (let ((*read-default-float-format* 'double-float))
                                              (read-from-string txt)))))
                     (return (and (realp v) (d (if (eql sign #\-) (- v) v)))))
                   (setf start (position-if (lambda (c) (or (digit-char-p c) (member c '(#\- #\+)))) s :start (1+ start))))))))

(defun topo-points (sample)
  "Survey points from the texts like SAMPLE: (values points scanned matched unreadable stacked)."
  (let* ((layer (dxf 8 sample)) (aci (entity-aci sample))
         (tol (if (eql (sysvar "INSUNITS") 4) 0.1d0 0.001d0))
         (best (make-hash-table :test #'equal)) (scanned 0) (matched 0) (bad 0) (stacked 0))
    (dolist (e (live-entities))
      (let ((d (ent-data e)))
        (when (member (dxf 0 d) '("TEXT" "MTEXT") :test #'equal)
          (incf scanned)
          (when (and (string-equal (or (dxf 8 d) "0") layer) (= (entity-aci d) aci))
            (incf matched)
            (let ((z (first-number (text-plain d))) (p (dxf 10 d)))
              (if (null z)
                  (incf bad)
                  (let* ((key (cons (round (px p) tol) (round (py p) tol))) (old (gethash key best)))
                    ;; points on top of each other: the highest one
                    (when old (incf stacked))
                    (when (or (null old) (> z (pz old)))
                      (setf (gethash key best) (pt (px p) (py p) z))))))))))
    (values (loop for p being the hash-values of best collect p) scanned matched bad stacked)))

;;; ---------------------------------------------------------------- Delaunay triangulation

(defun delaunay (pts)
  "Triangles ((i j k) ...) joining points PTS (a vector, in plan) so that no point lies inside any
triangle's circumcircle (Bowyer-Watson). Each triangle runs counterclockwise. Each point is found
by walking across the triangles from the last one made, and only the triangles round it are
looked at, so tens of thousands of points take seconds."
  (let* ((n (length pts)))
    (when (< n 3) (return-from delaunay nil))
    (let* ((xs (map 'list #'px pts)) (ys (map 'list #'py pts))
           (x0 (reduce #'min xs)) (x1 (reduce #'max xs)) (y0 (reduce #'min ys)) (y1 (reduce #'max ys))
           (span (max (- x1 x0) (- y1 y0) 1d-9)) (cx (/ (+ x0 x1) 2)) (cy (/ (+ y0 y1) 2))
           (m (+ n 3))
           ;; a triangle round everything, its corners numbered n, n+1, n+2
           (all (concatenate 'vector pts
                             (list (pt (- cx (* 40 span)) (- cy (* 20 span)))
                                   (pt (+ cx (* 40 span)) (- cy (* 20 span)))
                                   (pt cx (+ cy (* 40 span))))))
           (tris (make-hash-table))            ; id -> triangle
           (edges (make-hash-table))           ; a*m+b -> id of the triangle with edge a->b
           (next-id 0) (last nil))
      (labels ((key (a b) (+ (* a m) b))
               (orient (a b p)              ; > 0 when P is left of a->b
                 (let ((pa (aref all a)) (pb (aref all b)))
                   (- (* (- (px pb) (px pa)) (- (py p) (py pa))) (* (- (py pb) (py pa)) (- (px p) (px pa))))))
               (put (a b c)
                 (let ((tr (make-tri all a b c)) (id (incf next-id)))
                   (setf (gethash id tris) tr last id)
                   (destructuring-bind (a b c) (first tr)
                     (setf (gethash (key a b) edges) id (gethash (key b c) edges) id (gethash (key c a) edges) id))))
               (drop (id)
                 (destructuring-bind (a b c) (first (gethash id tris))
                   (remhash (key a b) edges) (remhash (key b c) edges) (remhash (key c a) edges))
                 (remhash id tris))
               (locate (p)
                 ;; walk towards P; give up after many steps and look everywhere
                 (let ((id last))
                   (loop repeat (+ 100 (* 4 (isqrt (hash-table-count tris))) (hash-table-count tris))
                         do (let ((tr (gethash id tris)))
                              (unless tr (return))
                              (destructuring-bind (a b c) (first tr)
                                (let ((across (loop for (u v) in (list (list a b) (list b c) (list c a))
                                                    when (minusp (orient u v p)) return (gethash (key v u) edges))))
                                  (if across (setf id across) (return-from locate id))))))
                   (loop for id being the hash-keys of tris using (hash-value tr)
                         when (in-circumcircle-p tr p) return id))))
        (put n (1+ n) (+ n 2))
        ;; points in rows, back and forth, so each is near the one before
        (let* ((rows (max 1 (isqrt (max 1 (floor n 8)))))
               (order (sort (loop for i below n collect i) #'<
                            :key (lambda (i)
                                   (let* ((p (aref pts i)) (r (min (1- rows) (floor (* rows (- (py p) y0)) (+ span 1d-300)))))
                                     (+ (* r 4 span) (if (evenp r) (- (px p) x0) (- x1 (px p)))))))))
          (dolist (i order)
            (let* ((p (aref all i)) (start (locate p)))
              (when (and start (in-circumcircle-p (gethash start tris) p))
                ;; the triangles whose circumcircle holds P, found by spreading from the one it is in
                (let ((bad (make-hash-table)) (todo (list start)) (rim nil))
                  (setf (gethash start bad) t)
                  (loop while todo
                        do (let ((id (pop todo)))
                             (destructuring-bind (a b c) (first (gethash id tris))
                               (loop for (u v) in (list (list a b) (list b c) (list c a))
                                     do (let ((nb (gethash (key v u) edges)))
                                          (cond ((and nb (gethash nb bad)))
                                                ((and nb (in-circumcircle-p (gethash nb tris) p))
                                                 (setf (gethash nb bad) t) (push nb todo))
                                                (t (push (cons u v) rim))))))))
                  ;; (an edge between two bad triangles may have been put on the rim before its neighbour turned bad)
                  (setf rim (remove-if (lambda (e) (let ((nb (gethash (key (cdr e) (car e)) edges))) (and nb (gethash nb bad)))) rim))
                  (loop for id being the hash-keys of bad do (drop id))
                  (dolist (e rim) (put (car e) (cdr e) i)))))))
        (loop for tr being the hash-values of tris
              for (a b c) = (first tr)
              unless (or (>= a n) (>= b n) (>= c n)
                         ;; (points all in a row make no triangle)
                         (< (abs (orient a b (aref all c))) (* 1d-12 span span)))
                collect (list a b c))))))

(defun make-tri (all a b c)
  "A triangle (corners circumcentre-x circumcentre-y radius-squared), counterclockwise."
  (let* ((pa (aref all a)) (pb (aref all b)) (pc (aref all c))
         (cross (- (* (- (px pb) (px pa)) (- (py pc) (py pa))) (* (- (py pb) (py pa)) (- (px pc) (px pa))))))
    (when (minusp cross) (rotatef b c) (rotatef pb pc))
    (let* ((ax (px pa)) (ay (py pa)) (bx (px pb)) (by (py pb)) (cx (px pc)) (cy (py pc))
           (dd (* 2 (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by)))))
           (ux (if (zerop dd) ax (/ (+ (* (+ (* ax ax) (* ay ay)) (- by cy)) (* (+ (* bx bx) (* by by)) (- cy ay))
                                       (* (+ (* cx cx) (* cy cy)) (- ay by))) dd)))
           (uy (if (zerop dd) ay (/ (+ (* (+ (* ax ax) (* ay ay)) (- cx bx)) (* (+ (* bx bx) (* by by)) (- ax cx))
                                       (* (+ (* cx cx) (* cy cy)) (- bx ax))) dd))))
      (list (list a b c) ux uy (if (zerop dd) most-positive-double-float
                                   (+ (expt (- ax ux) 2) (expt (- ay uy) 2)))))))

(defun in-circumcircle-p (tr p)
  (destructuring-bind (corners ux uy r2) tr
    (declare (ignore corners))
    (< (+ (expt (- (px p) ux) 2) (expt (- (py p) uy) 2)) (* r2 (- 1 1d-12)))))

;;; ---------------------------------------------------------------- the surface and the solid

(defun outline-edges (tris)
  "Edges (a . b) that only one triangle has, in that triangle's (counterclockwise) direction."
  (let ((count (make-hash-table :test #'equal)))
    (dolist (tr tris)
      (destructuring-bind (a b c) tr
        (dolist (e (list (cons a b) (cons b c) (cons c a)))
          (incf (gethash (cons (min (car e) (cdr e)) (max (car e) (cdr e))) count 0)))))
    (loop for tr in tris
          append (destructuring-bind (a b c) tr
                   (loop for e in (list (cons a b) (cons b c) (cons c a))
                         when (= 1 (gethash (cons (min (car e) (cdr e)) (max (car e) (cdr e))) count))
                           collect e)))))

(defun topo-surface (pts tris)
  "The terrain surface: a mesh of the triangles, every edge showing."
  (make-mesh-data (coerce pts 'list) (mapcar (lambda (tr) (mapcar #'1+ tr)) tris)))

(defun topo-solid (pts tris base &optional top-faces)
  "The ground under the surface down to elevation BASE, as a closed solid: the surface on top, the
same triangles at BASE underneath, and walls round the outline. TOP-FACES, when given, are the top's
faces as the surface mesh has them (with edges hidden)."
  (let* ((n (length pts))
         (verts (append (coerce pts 'list) (map 'list (lambda (p) (pt (px p) (py p) base)) pts)))
         (outline (outline-edges tris))
         (on-outline (let ((h (make-hash-table :test #'equal)))
                       (dolist (e outline h) (setf (gethash (cons (min (car e) (cdr e)) (max (car e) (cdr e))) h) t))))
         (faces nil))
    (flet ((corner (i vis) (if vis (1+ i) (- (1+ i)))))  ; (edge from corner I to the next shows if VIS)
      (dolist (tr tris)
        (destructuring-bind (a b c) tr
          ;; top: counterclockwise from above, every edge showing
          (unless top-faces (push (list (1+ a) (1+ b) (1+ c)) faces))
          ;; bottom: the other way round; only the outline shows
          (flet ((vis (i j) (gethash (cons (min i j) (max i j)) on-outline)))
            (push (list (corner (+ n c) (vis c b)) (corner (+ n b) (vis b a)) (corner (+ n a) (vis a c))) faces))))
      ;; walls: for outline edge a->b (counterclockwise), the outside is on the right
      (dolist (e outline)
        (let ((a (car e)) (b (cdr e)))
          ;; b, a along the top, down a, along the bottom, up b: every edge showing
          (push (list (1+ b) (1+ a) (1+ (+ n a)) (1+ (+ n b))) faces))))
    (make-mesh-data verts (append top-faces (nreverse faces)))))

(defun default-base (lo hi)
  "A base a little under the lowest point (a fifth of the rise, at least 1), in round numbers."
  (let* ((drop (max 1d0 (* 0.2d0 (- hi lo))))
         (step (expt 10d0 (floor (log drop 10)))))
    (d (* step (floor (- lo drop) step)))))

(defun contour-points (sample)
  "Points along the polylines on SAMPLE's layer, each at its polyline's elevation, no closer together
than about a three-hundredth of their extent: (values points lines)."
  (let* ((layer (dxf 8 sample))
         (lines (remove-if-not (lambda (e) (and (equal (ent-type e) "LWPOLYLINE")
                                                (string-equal (or (dxf 8 (ent-data e)) "0") layer)))
                               (live-entities)))
         (raw (loop for e in lines
                    for d = (ent-data e) for z = (d (or (dxf 38 d) 0))
                    append (mapcar (lambda (v) (pt (px (first v)) (py (first v)) z)) (lwpoly-vertices d)))))
    (if (null raw)
        (values nil 0)
        (let* ((bb (points-bbox raw))
               (cell (/ (max (- (third bb) (first bb)) (- (fourth bb) (second bb)) 1d-9) 300))
               (best (make-hash-table :test #'equal)))
          (dolist (p raw)
            (let ((k (cons (floor (px p) cell) (floor (py p) cell))))
              (unless (gethash k best) (setf (gethash k best) p))))
          (values (loop for p being the hash-values of best collect p) (length lines))))))

(defun build-terrain (pts &key (layer "Topo_Surface_Visualization"))
  "Asks for the base, then adds the surface mesh of PTS (and the solid down to the base)."
  (let* ((zs (mapcar #'pz pts)) (lo (reduce #'min zs))
         (dflt (default-base lo (reduce #'max zs)))
         (b (get-user-input :real :keywords '("Surface")
                                  :prompt (format nil "Specify base elevation for the solid or [Surface only] <~A>: "
                                                  (fmt-plain dflt))))
         (base (cond ((equal b "Surface") nil) ((realp b) (d b)) (t dflt)))
         (v (coerce pts 'vector))
         (tris (delaunay v)))
    (out (format nil "Delaunay triangulation: ~D triangle~:P.~%" (length tris)))
    (when tris
      (let ((surf (topo-surface v tris)))
        (ensure-layer layer)
        (add (append surf (list (cons 8 layer) (cons 62 256)))))
      (cond ((null base) (out (format nil "Surface made (layer ~A).~%" layer)))
            ((> base lo)
             (out (format nil "Surface made; no solid: the base (~A) must be below the lowest point (~A).~%"
                          (fmt-plain base) (fmt-plain lo))))
            (t (let ((solid (topo-solid v tris base)))
                 (add solid)
                 (out (format nil "Surface made (layer ~A), and a 3D solid down to ~A, volume ~A.~%"
                              layer (fmt-plain base) (fmt-plain (abs (mesh-signed-volume solid))))))))
      (when (gui-p) (gui-regen)))))

(defcmd "TOPOMODEL" () "Builds a terrain from survey elevation text or contour lines: a surface mesh and a 3D solid."
  (let* ((sel (get-user-input :entity :prompt "Select sample height mark (TEXT or MTEXT) or contour line: "))
         (e (and sel (sel-entity sel))))
    (cond
      ((null e) nil)
      ((equal (ent-type e) "LWPOLYLINE")
       (let ((sample (ent-data e)))
         (multiple-value-bind (pts lines) (contour-points sample)
           (out (format nil "~D contour line~:P on layer \"~A\": ~D point~:P.~%" lines (dxf 8 sample) (length pts)))
           (cond ((< (length pts) 3) (out (format nil "At least three points are needed for a surface.~%")))
                 ((< (- (reduce #'max pts :key #'pz) (reduce #'min pts :key #'pz)) 1d-9)
                  (out (format nil "The lines are all at one height (give them their elevations).~%")))
                 (t (build-terrain pts))))))
      ((not (member (ent-type e) '("TEXT" "MTEXT") :test #'equal))
       (out (format nil "Invalid selection: select a TEXT, MTEXT or contour polyline.~%")))
      (t
       (let ((sample (ent-data e)))
         (out (format nil "Points from texts on layer \"~A\", color ~A.~%" (dxf 8 sample) (color-label (entity-aci sample))))
         (multiple-value-bind (pts scanned matched bad stacked) (topo-points sample)
           (out (format nil "~D TEXT/MTEXT scanned, ~D on that layer in that color~[~:;, ~:*~D without a number~]. ~
                             ~D point~:P~[~:;, ~:*~D stacked (the highest kept)~].~%"
                        scanned matched bad (length pts) stacked))
           (if (< (length pts) 3)
               (out (format nil "At least three points are needed for a surface.~%"))
               (build-terrain pts))))))))

;;; ---------------------------------------------------------------- ALTXT

(defun mark-centre (d)
  (cond ((member (dxf 0 d) '("CIRCLE" "ARC") :test #'equal) (pt3 (dxf 10 d)))
        ((equal (dxf 0 d) "INSERT") (pt3 (dxf 10 d)))))

(defun mark-reach (e d tmax)
  "How far from survey mark E its text can be and still belong to it."
  (cond ((member (dxf 0 d) '("CIRCLE" "ARC") :test #'equal) (+ (* 3 (dxf 40 d)) tmax))
        (t (let ((b (ignore-errors (entity-bbox e))))
             (+ tmax (if b (* 1.5 (dist2d (pt (first b) (second b)) (pt (third b) (fourth b)))) 100d0))))))

(defun same-mark-p (d s)
  (and (equal (dxf 0 d) (dxf 0 s))
       (string-equal (or (dxf 8 d) "0") (or (dxf 8 s) "0"))
       (= (entity-aci d) (entity-aci s))
       (if (equal (dxf 0 s) "INSERT")
           (string-equal (dxf 2 d) (dxf 2 s))
           (< (abs (- (dxf 40 d) (dxf 40 s))) 1d-9))))

(defun same-label-p (d s)
  (and (member (dxf 0 d) '("TEXT" "MTEXT") :test #'equal)
       (string-equal (or (dxf 8 d) "0") (or (dxf 8 s) "0"))
       (= (entity-aci d) (entity-aci s))
       (string-equal (or (dxf 7 d) "Standard") (or (dxf 7 s) "Standard"))
       (< (abs (- (or (dxf 40 d) 0) (or (dxf 40 s) 0))) 0.001)))

(defcmd "ALTXT" () "Moves elevation text onto the centre of its survey mark (circles, arcs or blocks like a sample), ready for TOPOMODEL."
  (let* ((m (sel-entity (get-user-input :entity :prompt "Select sample circle, arc or block: ")))
         (m (and m (if (member (ent-type m) '("CIRCLE" "ARC" "INSERT") :test #'equal) m
                       (progn (out (format nil "That is not a circle, arc or block.~%")) nil))))
         (tx (and m (sel-entity (get-user-input :entity :prompt "Select sample text/MTEXT: "))))
         (tx (and tx (if (member (ent-type tx) '("TEXT" "MTEXT") :test #'equal) tx
                         (progn (out (format nil "That is not a TEXT or MTEXT.~%")) nil)))))
    (when (and m tx)
      (let* ((ms (ent-data m)) (ts (ent-data tx))
             (marks (remove-if-not (lambda (e) (same-mark-p (ent-data e) ms)) (live-entities)))
             (texts (remove-if-not (lambda (e) (same-label-p (ent-data e) ts)) (live-entities)))
             ;; the largest text, so that a mark's reach allows for it
             (tmax (reduce #'max (mapcar (lambda (e) (let ((b (ignore-errors (entity-bbox e))))
                                                       (if b (max (- (third b) (first b)) (- (fourth b) (second b))) 0d0)))
                                         texts)
                           :initial-value 0d0))
             (moved 0))
        (dolist (te texts)
          (let* ((td (ent-data te)) (p (pt3 (dxf 10 td))) (best nil) (bd nil))
            (dolist (mk marks)
              (let ((dd (dist2d (mark-centre (ent-data mk)) p)))
                (when (or (null bd) (< dd bd)) (setf best mk bd dd))))
            (when (and best (<= bd (mark-reach best (ent-data best) tmax)) (> bd 0.001d0))
              (let ((c (mark-centre (ent-data best))))
                (set-entity-data te (xform-data td (xf-move (pt (- (px c) (px p)) (- (py c) (py p)) 0))))
                (incf moved)))))
        (out (format nil "~D survey mark~:P, ~D text~:P like the sample: moved ~D text object~:P.~%"
                     (length marks) (length texts) moved))
        (when (gui-p) (gui-regen))))))
