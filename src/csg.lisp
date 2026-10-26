;;;; csg.lisp - union, subtract and intersect of solids (meshes), by BSP trees (the method of
;;;; csg.js, by Evan Wallace), then cleaned up for drawing: points welded, edges split where a
;;;; point of a neighbouring face lies on them (no gaps), and only real edges shown - sharp
;;;; edges, the solids' own outlines and isolines, and the new lines where the solids meet.

(in-package :fancy-pencil)

(defconstant +csg-eps+ 1d-6)

(defstruct (cpoly (:constructor %make-cpoly (verts n w))) verts n w)

(defun make-cpoly (verts &optional n w)
  (let ((n (or n (newell-normal verts))))
    (and n (%make-cpoly verts n (or w (vdot3 n (first verts)))))))

(defun cpoly-flip (p) (%make-cpoly (reverse (cpoly-verts p)) (v* (cpoly-n p) -1d0) (- (cpoly-w p))))

(defun split-cpoly (pn pw poly cofront coback front back)
  "Split POLY by the plane (PN, PW). Pushes onto the four place lists (conses of a list)."
  (let* ((vs (cpoly-verts poly))
         (ts (mapcar (lambda (v) (- (vdot3 pn v) pw)) vs))
         (types (mapcar (lambda (tt) (cond ((< tt (- +csg-eps+)) 2) ((> tt +csg-eps+) 1) (t 0))) ts))
         (ptype (reduce #'logior types)))
    (case ptype
      (0 (if (> (vdot3 pn (cpoly-n poly)) 0) (push poly (car cofront)) (push poly (car coback))))
      (1 (push poly (car front)))
      (2 (push poly (car back)))
      (t (let ((f nil) (b nil) (n (length vs)))
           (loop for i below n
                 for vi = (nth i vs) for vj = (nth (mod (1+ i) n) vs)
                 for ti = (nth i types) for tj = (nth (mod (1+ i) n) types)
                 for di = (nth i ts) for dj = (nth (mod (1+ i) n) ts)
                 do (unless (= ti 2) (push vi f))
                    (unless (= ti 1) (push vi b))
                    (when (= (logior ti tj) 3)
                      (let* ((tt (/ di (- di dj))) (v (v+ vi (v* (v- vj vi) tt))))
                        (push v f) (push v b))))
           (when (>= (length f) 3) (push (%make-cpoly (nreverse f) (cpoly-n poly) (cpoly-w poly)) (car front)))
           (when (>= (length b) 3) (push (%make-cpoly (nreverse b) (cpoly-n poly) (cpoly-w poly)) (car back))))))))

(defstruct bsp n w front back (polys nil))

(defun bsp-build (node polys)
  ;; iterative over a work list, so deep trees cannot overflow the stack
  (let ((work (list (cons node polys))))
    (loop while work do
      (destructuring-bind (nd . ps) (pop work)
        (when ps
          (unless (bsp-n nd) (setf (bsp-n nd) (cpoly-n (first ps)) (bsp-w nd) (cpoly-w (first ps))))
          (let ((co (list (bsp-polys nd))) (f (list nil)) (b (list nil)))
            (dolist (p ps) (split-cpoly (bsp-n nd) (bsp-w nd) p co co f b))
            (setf (bsp-polys nd) (car co))
            (when (car f) (unless (bsp-front nd) (setf (bsp-front nd) (make-bsp))) (push (cons (bsp-front nd) (car f)) work))
            (when (car b) (unless (bsp-back nd) (setf (bsp-back nd) (make-bsp))) (push (cons (bsp-back nd) (car b)) work)))))))
  node)

(defun bsp-nodes (node)
  (let ((out nil) (stack (list node)))
    (loop while stack do
      (let ((nd (pop stack)))
        (when nd (push nd out) (push (bsp-front nd) stack) (push (bsp-back nd) stack))))
    out))

(defun bsp-invert (node)
  (dolist (nd (bsp-nodes node))
    (setf (bsp-polys nd) (mapcar #'cpoly-flip (bsp-polys nd)))
    (when (bsp-n nd) (setf (bsp-n nd) (v* (bsp-n nd) -1d0) (bsp-w nd) (- (bsp-w nd))))
    (rotatef (bsp-front nd) (bsp-back nd)))
  node)

(defun bsp-clip-polygons (node polys)
  "The parts of POLYS outside the solid of NODE."
  (if (or (null node) (null (bsp-n node))) (copy-list polys)
      (let ((f (list nil)) (b (list nil)))
        (dolist (p polys) (split-cpoly (bsp-n node) (bsp-w node) p f b f b))
        (let ((fr (if (bsp-front node) (bsp-clip-polygons (bsp-front node) (car f)) (car f)))
              (bk (if (bsp-back node) (bsp-clip-polygons (bsp-back node) (car b)) nil)))
          (nconc fr bk)))))

(defun bsp-clip-to (node other)
  (dolist (nd (bsp-nodes node))
    (setf (bsp-polys nd) (bsp-clip-polygons other (bsp-polys nd)))))

(defun bsp-all (node) (loop for nd in (bsp-nodes node) append (bsp-polys nd)))

(defun csg-op (op pa pb)
  "Polygons of A OP B (OP :union :subtract :intersect)."
  (let ((a (bsp-build (make-bsp) pa)) (b (bsp-build (make-bsp) pb)))
    (ecase op
      (:union (bsp-clip-to a b) (bsp-clip-to b a) (bsp-invert b) (bsp-clip-to b a) (bsp-invert b)
       (bsp-build a (bsp-all b)) (bsp-all a))
      (:subtract (bsp-invert a) (bsp-clip-to a b) (bsp-clip-to b a) (bsp-invert b) (bsp-clip-to b a) (bsp-invert b)
       (bsp-build a (bsp-all b)) (bsp-invert a) (bsp-all a))
      (:intersect (bsp-invert a) (bsp-clip-to b a) (bsp-invert b) (bsp-clip-to a b) (bsp-clip-to b a)
       (bsp-build a (bsp-all b)) (bsp-invert a) (bsp-all a)))))

;;; ---------- meshes -> polygons (outward, consistent) ----------

(defun weld-key (p) (list (round (px p) 1d-7) (round (py p) 1d-7) (round (pz p) 1d-7)))

(defun mesh-orientation (data)
  "Outward orientation of each face of mesh DATA: (values faces flips positions closed-p), where
FACES are the faces as canonical point indices (NIL for a degenerate face), FLIPS says which must
be turned over, and CLOSED-P is true for a closed solid."
  (let* ((vs (coerce (mesh-verts data) 'vector))
         (canon (make-hash-table :test #'equal))
         (cidx (map 'vector (lambda (p) (or (gethash (weld-key p) canon) (setf (gethash (weld-key p) canon) (hash-table-count canon)))) vs))
         (fv (coerce (mapcar (lambda (f) (let ((ix (mapcar (lambda (i) (aref cidx (1- (abs i)))) f)) (out nil))
                                           (dolist (i ix) (unless (eql i (first out)) (push i out)))
                                           (when (and (cdr out) (eql (first out) (car (last out)))) (pop out))
                                           (and (>= (length out) 3) (nreverse out))))
                             (mesh-faces data))
                     'vector))
         (pos (let ((v (make-array (hash-table-count canon))))
                (loop for p across vs for c across cidx do (setf (aref v c) (pt3 p)))
                v))
         (nf (length fv))
         (edge-faces (make-hash-table :test #'equal))
         (flip (make-array nf :initial-element nil))
         (seen (make-array nf :initial-element nil))
         (comp (make-array nf :initial-element -1)))
    (flet ((edges (f) (loop for (a b) on (append f (list (first f))) while b collect (cons a b))))
      (dotimes (i nf)
        (dolist (e (and (aref fv i) (edges (aref fv i))))
          (push i (gethash (if (< (car e) (cdr e)) e (cons (cdr e) (car e))) edge-faces))))
      ;; make neighbours agree: a shared edge runs opposite ways in the two faces
      (let ((ncomp 0))
        (dotimes (s nf)
          (unless (or (aref seen s) (null (aref fv s)))
            (setf (aref seen s) t (aref comp s) ncomp)
            (let ((queue (list s)))
              (loop while queue do
                (let* ((i (pop queue))
                       (fi (if (aref flip i) (reverse (aref fv i)) (aref fv i))))
                  (dolist (e (edges fi))
                    (dolist (j (gethash (if (< (car e) (cdr e)) e (cons (cdr e) (car e))) edge-faces))
                      (unless (aref seen j)
                        (setf (aref seen j) t (aref comp j) ncomp)
                        (when (member e (edges (aref fv j)) :test #'equal) (setf (aref flip j) t))
                        (push j queue)))))))
            (incf ncomp)))
        ;; each connected piece facing outward: positive volume
        (dotimes (c ncomp)
          (let ((vol (loop for i below nf when (= (aref comp i) c)
                           sum (let ((ps (map 'list (lambda (k) (aref pos k)) (if (aref flip i) (reverse (aref fv i)) (aref fv i)))))
                                 (loop for k from 1 below (1- (length ps))
                                       sum (vdot3 (first ps) (cross3 (nth k ps) (nth (1+ k) ps))))))))
            (when (minusp vol)
              (dotimes (i nf) (when (= (aref comp i) c) (setf (aref flip i) (not (aref flip i))))))))))
    (values fv flip pos
            (let ((closed t)) (maphash (lambda (k v) (declare (ignore k)) (unless (= (length v) 2) (setf closed nil))) edge-faces) closed))))

(defun mesh-polygons (data)
  "Faces of mesh DATA as outward-facing polygons (orientation made consistent across the mesh)."
  (multiple-value-bind (fv flip pos) (mesh-orientation data)
    ;; polygons; non-flat quads become two triangles
    (loop for i below (length fv)
          for f0 = (aref fv i)
          when f0
            append (let* ((f (if (aref flip i) (reverse f0) f0))
                          (ps (mapcar (lambda (k) (aref pos k)) f)))
                     (if (and (= (length ps) 4)
                              (let ((n (newell-normal ps)))
                                (or (null n) (> (abs (- (vdot3 n (fourth ps)) (vdot3 n (first ps)))) 1d-9)
                                    (> (abs (- (vdot3 n (third ps)) (vdot3 n (first ps)))) 1d-9))))
                         (remove nil (list (make-cpoly (list (first ps) (second ps) (third ps)))
                                           (make-cpoly (list (first ps) (third ps) (fourth ps)))))
                         (let ((p (make-cpoly ps))) (and p (list p))))))))

(defvar *display-faces-cache* (make-hash-table :test #'eq :weakness :key))

(defun mesh-display-faces (data)
  "For drawing: ((points outward-normal edge-visible-flags) ...) and whether the mesh is closed."
  (let ((c (gethash data *display-faces-cache*)))
    (if c (values (car c) (cdr c))
        (multiple-value-bind (fv flip pos closed) (mesh-orientation data)
          (declare (ignore pos))
          (let* ((vs (coerce (mesh-verts data) 'vector))
                 ;; an edge shows if any face says so (each face draws it when it faces the viewer)
                 (shown (make-hash-table :test #'equal))
                 (key (lambda (a b) (let ((ka (weld-key (aref vs (1- (abs a))))) (kb (weld-key (aref vs (1- (abs b))))))
                                      (if (string< (prin1-to-string ka) (prin1-to-string kb)) (list ka kb) (list kb ka))))))
            (dolist (f (mesh-faces data))
              (loop for (a b) on (append f (list (first f))) while b
                    do (when (plusp a) (setf (gethash (funcall key a b) shown) t))))
            (let ((out (loop for f in (mesh-faces data) for i from 0
                             when (aref fv i)
                               collect (let* ((ps (mapcar (lambda (k) (aref vs (1- (abs k)))) f))
                                              (vis (loop for (a b) on (append f (list (first f))) while b
                                                         collect (gethash (funcall key a b) shown)))
                                              (n (newell-normal ps)))
                                         (list ps (if (and n (aref flip i)) (v* n -1d0) n) vis)))))
            (setf (gethash data *display-faces-cache*) (cons out closed))
            (values out closed)))))))

;;; ---------- polygons -> a clean mesh ----------

(defun point-on-segment-p (p a b &optional (eps 1d-7))
  "True when P lies strictly inside segment AB."
  (let* ((ab (v- b a)) (l2 (vdot3 ab ab)))
    (and (> l2 1d-24)
         (let ((tt (/ (vdot3 (v- p a) ab) l2)))
           (and (> tt 1d-9) (< tt (- 1 1d-9))
                (< (dist p (v+ a (v* ab tt))) eps))))))

(defun polygons-to-mesh (polys visible-segments)
  "A mesh from polygons: welded points, edges split at neighbours' points (no gaps), and edges
shown when they are sharp (over 20 degrees), open, or lie on one of VISIBLE-SEGMENTS."
  (let* ((canon (make-hash-table :test #'equal)) (pts (make-array 0 :adjustable t :fill-pointer t))
         (faces nil))
    (flet ((ix (p) (let ((k (weld-key p)))
                     (or (gethash k canon) (progn (vector-push-extend (pt3 p) pts) (setf (gethash k canon) (1- (length pts))))))))
      (dolist (pl polys)
        (let ((f nil))
          (dolist (v (cpoly-verts pl)) (let ((i (ix v))) (unless (eql i (first f)) (push i f))))
          (when (and (cdr f) (eql (first f) (car (last f)))) (pop f))
          (when (>= (length f) 3) (push (cons (nreverse f) (cpoly-n pl)) faces)))))
    ;; split edges where another point lies on them
    (let ((grid (make-hash-table :test #'equal)) (cell 0d0))
      (let ((ext (loop for p across pts maximize (max (abs (px p)) (abs (py p)) (abs (pz p))))))
        (setf cell (max 1d-6 (/ (max ext 1d0) 64))))
      (flet ((key (p) (list (floor (px p) cell) (floor (py p) cell) (floor (pz p) cell))))
        (loop for p across pts for i from 0 do (push i (gethash (key p) grid)))
        (setf faces
              (mapcar (lambda (fc)
                        (let* ((f (car fc)) (out nil))
                          (loop for (a b) on (append f (list (first f))) while b
                                do (push a out)
                                   (let* ((pa (aref pts a)) (pb (aref pts b))
                                          (lo (mapcar #'min (key pa) (key pb))) (hi (mapcar #'max (key pa) (key pb)))
                                          (between nil))
                                     (if (< (reduce #'* (mapcar (lambda (l h) (1+ (- h l))) lo hi)) 4000)
                                         (loop for x from (first lo) to (first hi) do
                                           (loop for y from (second lo) to (second hi) do
                                             (loop for z from (third lo) to (third hi) do
                                               (dolist (k (gethash (list x y z) grid))
                                                 (when (and (/= k a) (/= k b) (point-on-segment-p (aref pts k) pa pb))
                                                   (pushnew k between))))))
                                         ;; a long edge: check every point
                                         (loop for k from 0 below (length pts)
                                               do (when (and (/= k a) (/= k b) (point-on-segment-p (aref pts k) pa pb))
                                                    (pushnew k between))))
                                     (dolist (k (sort between #'< :key (lambda (k) (dist (aref pts k) pa))))
                                       (push k out))))
                          (cons (nreverse out) (cdr fc))))
                      faces))))
    ;; which edges show
    (let ((edge-normals (make-hash-table :test #'equal)))
      (dolist (fc faces)
        (loop for (a b) on (append (car fc) (list (first (car fc)))) while b
              do (push (cdr fc) (gethash (if (< a b) (cons a b) (cons b a)) edge-normals))))
      (flet ((visible (a b)
               (let ((ns (gethash (if (< a b) (cons a b) (cons b a)) edge-normals)))
                 (cond ((/= (length ns) 2) t)
                       ((< (vdot3 (first ns) (second ns)) (cos (/ pi 9))) t)
                       ((> (vdot3 (first ns) (second ns)) (- 1 1d-9)) nil)        ; flat: never
                       (t (let ((m (midpt (aref pts a) (aref pts b))))
                            (some (lambda (sg) (or (point-on-segment-p m (car sg) (cdr sg) 1d-6)
                                                   (< (dist m (midpt (car sg) (cdr sg))) 1d-7)))
                                  visible-segments)))))))
        (let ((out-faces nil))
          (dolist (fc faces)
            (let* ((f (car fc)) (n (length f))
                   (vis (loop for (a b) on (append f (list (first f))) while b collect (visible a b))))
              (if (<= n 4)
                  (push (loop for i in f for v in vis collect (if v (1+ i) (- (1+ i)))) out-faces)
                  ;; bigger flat faces: triangles; inside edges hidden
                  (let* ((ps (mapcar (lambda (i) (aref pts i)) f))
                         (tris (or (ignore-errors (triangulate (polygon-2d-in-plane ps)))
                                   (loop for k from 1 below (1- n) collect (list 0 k (1+ k))))))
                    (dolist (tr tris)
                      (push (loop for k below 3
                                  for i = (nth k tr) for j = (nth (mod (1+ k) 3) tr)
                                  collect (let ((boundary (= (mod (- j i) n) 1)))
                                            (if (and boundary (nth i vis)) (1+ (nth i f)) (- (1+ (nth i f))))))
                            out-faces))))))
          (make-mesh-data (coerce pts 'list) (nreverse out-faces)))))))

(defun mesh-visible-segments (data) (mesh-edges data))

(defun boolean-meshes (op meshes-a meshes-b)
  "Mesh of (A1 + A2 ...) OP (B1 + B2 ...), keeping the look of the originals' edges."
  (let* ((visible (loop for m in (append meshes-a meshes-b) append (mesh-visible-segments m)))
         (merge (lambda (ms) (reduce (lambda (acc m) (if acc (csg-op :union acc (mesh-polygons m)) (mesh-polygons m)))
                                     ms :initial-value nil)))
         (pa (funcall merge meshes-a))
         (polys (if (eq op :union)
                    (csg-op :union pa (funcall merge meshes-b))
                    (reduce (lambda (acc m) (csg-op op acc (mesh-polygons m))) meshes-b :initial-value pa))))
    (and polys (polygons-to-mesh polys visible))))

;;; ---------- commands ----------

(defun pick-solids (prompt)
  (let* ((ents (get-selection prompt))
         (ok (remove-if-not (lambda (e) (equal (ent-type e) "POLYFACE")) ents)))
    (when (< (length ok) (length ents))
      (out (format nil "~D object~:P skipped (not solids).~%" (- (length ents) (length ok)))))
    ok))

(defun replace-with-result (ents mesh what)
  "Put MESH (with the first object's layer and colour) in place of ENTS."
  (if (or (null mesh) (null (mesh-faces mesh)))
      (progn (dolist (e ents) (delete-entity e))
             (out (format nil "~A left nothing (the solids do not overlap); they were removed.~%" what)))
      (let* ((first-data (ent-data (first ents)))
             (props (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370 48)))) first-data)))
        (dolist (e ents) (delete-entity e))
        (add (append mesh props))
        (out (format nil "~A done~@[ (volume ~A)~].~%" what
                     (let ((v (mesh-volume mesh))) (and (> v 1d-12) (fmt-num v)))))))
  (when (gui-p) (gui-regen)))

(defcmd "UNION" ("UNI") "Joins solids into one."
  (let ((ents (pick-solids "Select solids to join: ")))
    (if (< (length ents) 2) (out (format nil "Select at least two solids.~%"))
        (replace-with-result ents (boolean-meshes :union (list (ent-data (first ents))) (mapcar #'ent-data (rest ents))) "Union"))))

(defcmd "SUBTRACT" ("SU") "Removes solids from other solids."
  (out (format nil "Select solids to subtract from...~%"))
  (let ((keep (pick-solids "Select solids to subtract from: ")))
    (when keep
      (out (format nil "Select solids to subtract...~%"))
      (let ((cut (remove-if (lambda (e) (member e keep)) (pick-solids "Select solids to subtract: "))))
        (if (null cut) (out (format nil "Nothing to subtract.~%"))
            (progn
              (replace-with-result keep (boolean-meshes :subtract (mapcar #'ent-data keep) (mapcar #'ent-data cut)) "Subtract")
              (dolist (e cut) (delete-entity e))
              (when (gui-p) (gui-regen))))))))

(defcmd "INTERSECT" ("IN") "Keeps only the part that solids share."
  (let ((ents (pick-solids "Select solids to intersect: ")))
    (if (< (length ents) 2) (out (format nil "Select at least two solids.~%"))
        (replace-with-result ents (boolean-meshes :intersect (list (ent-data (first ents))) (mapcar #'ent-data (rest ents))) "Intersect"))))

(defun half-space-mesh (o n size)
  "A big box filling the side of the plane (O, N) that N points to, SIZE across."
  (destructuring-bind (x y z oo) (frame-from-x-and-z (pt3 o) (any-perpendicular (unit3 n)) (unit3 n))
    (declare (ignore oo))
    (let* ((c (pt3 o))
           (corner (lambda (a b h) (v+ c (v+ (v* x a) (v+ (v* y b) (v* z h))))))
           (base (list (funcall corner (- size) (- size) 0) (funcall corner size (- size) 0)
                       (funcall corner size size 0) (funcall corner (- size) size 0))))
      (prism-mesh base (v* z (* 2 size))))))

(defcmd "SLICE" ("SL") "Cuts solids with a plane, keeping one side or both."
  (let ((ents (pick-solids "Select objects to slice: ")))
    (when ents
      (let* ((r (gpoint "Specify start point of slicing plane or [XY/YZ/ZX/3points] <3points>: " :keywords '("XY" "YZ" "ZX" "3points")))
             (plane
               (flet ((thru (n) (let ((p (gpoint "Specify a point on the plane <0,0,0>: "))) (list (if (pointp p) (pt3 p) (pt 0 0 0)) n))))
                 (cond ((equal r "XY") (thru (pt 0 0 1)))
                       ((equal r "YZ") (thru (pt 1 0 0)))
                       ((equal r "ZX") (thru (pt 0 1 0)))
                       ((pointp r)
                        ;; two points: a plane through them, standing square to the working plane
                        (let ((b (gpoint "Specify second point on plane: " :base r)))
                          (when (pointp b)
                            (list (pt3 r) (unit3 (cross3 (v- (pt3 b) (pt3 r)) (pt 0 0 1)))))))
                       (t (let* ((a (gpoint "Specify first point on plane: "))
                                 (b (and (pointp a) (gpoint "Specify second point on plane: " :base a)))
                                 (c (and (pointp b) (gpoint "Specify third point on plane: " :base b))))
                            (and (pointp c) (let ((n (cross3 (v- (pt3 b) (pt3 a)) (v- (pt3 c) (pt3 a)))))
                                              (and (> (vdot3 n n) 1d-18) (list (pt3 a) (unit3 n)))))))))))
        (when plane
          (let* ((side (get-user-input :point :prompt "Specify a point on desired side or [keep Both sides] <Both>: "
                                              :keywords '("Both")))
                 (o (first plane)) (n (second plane))
                 (size (* 4 (loop for e in ents maximize (loop for v in (mesh-verts (ent-data e)) maximize (dist v o)))))
                 (sides (cond ((pointp side) (list (if (minusp (vdot3 (v- (pt3 side) o) n)) (v* n -1d0) n)))
                              (t (list n (v* n -1d0)))))
                 (made 0))
            (dolist (e ents)
              (let ((data (ent-data e))
                    (props (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370 48)))) (ent-data e))))
                (dolist (sn sides)
                  (let ((m (boolean-meshes :intersect (list data) (list (half-space-mesh o sn size)))))
                    (when (and m (mesh-faces m)) (add (append m props)) (incf made))))
                (delete-entity e)))
            (out (format nil "Sliced: ~D piece~:P.~%" made))
            (when (gui-p) (gui-regen))))))))
