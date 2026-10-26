;;;; mesh.lisp - the 3D mesh object (a "PolyFace Mesh" in AutoCAD's terms) that every solid in
;;;; Fancy Pencil is made of: a list of corner points and a list of faces (3 or 4 corners each).
;;;; As in the DXF format, a face's corner number is negative when the edge from that corner to
;;;; the next one is hidden, so a curved side shows only its outline and a few lines (isolines),
;;;; the way AutoCAD's wireframe shows solids, instead of every little facet.
;;;;
;;;; Stored as  (0 . "POLYFACE") (:verts p1 p2 ...) (:faces (1 2 -3) (3 4 1 2) ...)  in world
;;;; coordinates, written to DXF as a POLYLINE with flag 64 and its VERTEX records.

(in-package :fancy-pencil)

(defun mesh-verts (data) (cdr (assoc :verts data)))
(defun mesh-faces (data) (cdr (assoc :faces data)))

(defun make-mesh-data (verts faces)
  (list (cons 0 "POLYFACE") (cons :verts (mapcar #'pt3 verts)) (cons :faces faces)))

(defun mesh-edges (data)
  "Visible edges ((a . b) ...) of mesh DATA, as point pairs, each edge once."
  (let* ((vs (coerce (mesh-verts data) 'vector))
         (n (length vs))
         (seen (make-hash-table :test #'equal))
         (out nil))
    (dolist (f (mesh-faces data) (nreverse out))
      (let ((k (length f)))
        (loop for i from 0 below k
              for a = (nth i f) for b = (nth (mod (1+ i) k) f)
              do (when (plusp a)
                   (let ((ia (1- (abs a))) (ib (1- (abs b))))
                     (when (and (< -1 ia n) (< -1 ib n) (/= ia ib))
                       (let ((key (if (< ia ib) (cons ia ib) (cons ib ia))))
                         (unless (gethash key seen)
                           (setf (gethash key seen) t)
                           (push (cons (aref vs ia) (aref vs ib)) out)))))))))))

(defun mesh-pieces (data)
  (mapcar (lambda (e) (list :seg (car e) (cdr e))) (mesh-edges data)))

(defun mesh-centroid (data)
  (let ((vs (mesh-verts data)))
    (if vs (v* (reduce #'v+ vs) (/ 1d0 (length vs))) (pt 0 0 0))))

(defun xform-mesh (data xf)
  (let ((fn (xf-fn xf)))
    (cons (cons 0 "POLYFACE")
          (mapcar (lambda (p) (if (and (consp p) (eq (car p) :verts))
                                  (cons :verts (mapcar (lambda (v) (pt3 (funcall fn v))) (cdr p)))
                                  p))
                  (rest data)))))

(defun mesh-face-points (data f)
  (let ((vs (mesh-verts data))) (mapcar (lambda (i) (nth (1- (abs i)) vs)) f)))

(defun newell-normal (pts)
  "Unit normal of the (roughly flat) polygon PTS, by Newell's method; NIL when degenerate."
  (let ((nx 0d0) (ny 0d0) (nz 0d0))
    (loop for (a b) on (append pts (list (first pts))) while b
          do (incf nx (* (- (py a) (py b)) (+ (pz a) (pz b))))
             (incf ny (* (- (pz a) (pz b)) (+ (px a) (px b))))
             (incf nz (* (- (px a) (px b)) (+ (py a) (py b)))))
    (let ((l (sqrt (+ (* nx nx) (* ny ny) (* nz nz)))))
      (and (> l 1d-12) (pt (/ nx l) (/ ny l) (/ nz l))))))

(defun mesh-volume (data)
  "Volume of a closed mesh (sum of signed tetrahedra); 0 for an open surface."
  (abs (loop for f in (mesh-faces data)
             for ps = (mesh-face-points data f)
             sum (loop for i from 1 below (1- (length ps))
                       sum (/ (vdot3 (first ps) (cross3 (nth i ps) (nth (1+ i) ps))) 6d0)))))

;;; ---------- DXF ----------

(defun mesh-from-polyface (head verts)
  "A POLYFACE from a DXF polyface POLYLINE (flag 64) and its VERTEX groups."
  (let ((pts nil) (faces nil))
    (dolist (v verts)
      (let* ((vd (group-coords (clean-group v))) (fl (or (dxf 70 vd) 0)))
        (cond ((and (logtest fl 128) (not (logtest fl 64)))      ; face record
               (let ((idx (remove 0 (mapcar (lambda (c) (or (dxf c vd) 0)) '(71 72 73 74)))))
                 (when (>= (length idx) 3) (push idx faces))))
              (t (push (pt3 (dxf 10 vd)) pts)))))
    (append (make-mesh-data (nreverse pts) (nreverse faces))
            (remove nil (list (assoc 8 head) (assoc 62 head) (assoc 6 head) (assoc 370 head))))))

(defun mesh-from-polygon-mesh (head verts)
  "A POLYFACE from a DXF M x N polygon mesh (POLYLINE flag 16)."
  (let* ((flags (or (dxf 70 head) 0))
         (m (or (dxf 71 head) 0)) (n (or (dxf 72 head) 0))
         (pts (mapcar (lambda (v) (pt3 (dxf 10 (group-coords (clean-group v))))) verts))
         (closed-m (logtest 1 flags)) (closed-n (logtest 32 flags))
         (faces nil))
    (when (and (> m 1) (> n 1) (>= (length pts) (* m n)))
      (loop for i from 0 below (if closed-m m (1- m))
            do (loop for j from 0 below (if closed-n n (1- n))
                     do (let ((i2 (mod (1+ i) m)) (j2 (mod (1+ j) n)))
                          (push (list (1+ (+ (* i n) j)) (1+ (+ (* i n) j2)) (1+ (+ (* i2 n) j2)) (1+ (+ (* i2 n) j))) faces))))
      (append (make-mesh-data (subseq pts 0 (* m n)) (nreverse faces))
              (remove nil (list (assoc 8 head) (assoc 62 head) (assoc 6 head)))))))

(defun mesh-from-3dface (data)
  "A one-face POLYFACE from a 3DFACE (its hidden-edge flags kept)."
  (let* ((ps (mapcar (lambda (c) (pt3 (or (dxf c data) (dxf 12 data)))) '(10 11 12 13)))
         (tri (< (dist (third ps) (fourth ps)) 1d-12))
         (ps (if tri (butlast ps) ps))
         (fl (or (dxf 70 data) 0)))
    (append (make-mesh-data ps (list (loop for i from 1 to (length ps)
                                           collect (if (logtest (ash 1 (1- i)) fl) (- i) i))))
            (remove nil (list (assoc 8 data) (assoc 62 data) (assoc 6 data) (assoc 370 data))))))

(defun write-mesh-2000 (out data handle owner)
  (let ((layer (or (dxf 8 data) "0")) (vs (mesh-verts data)) (fs (mesh-faces data)))
    (write-common out (cons (cons 0 "POLYLINE") (rest data)) handle owner)
    (w out 100 "AcDbPolyFaceMesh") (w out 66 1) (wp out 10 (pt 0 0 0))
    (w out 70 64) (w out 71 (length vs)) (w out 72 (length fs))
    (flet ((vertex-head (sub)
             (w out 0 "VERTEX") (w out 5 (h2)) (w out 330 handle) (w out 100 "AcDbEntity") (when *write-paper* (w out 67 1)) (w out 8 layer)
             (let ((c (dxf 62 data))) (when c (w out 62 c)))
             (w out 100 sub)))
      (dolist (v vs)
        (vertex-head "AcDbVertex") (w out 100 "AcDbPolyFaceMeshVertex")
        (wp out 10 (pt3 v)) (w out 70 192))
      (dolist (f fs)
        (vertex-head "AcDbFaceRecord")
        (wp out 10 (pt 0 0 0)) (w out 70 128)
        (loop for i in f for c from 71 do (w out c i))))
    (w out 0 "SEQEND") (w out 5 (h2)) (w out 330 handle) (w out 100 "AcDbEntity") (when *write-paper* (w out 67 1)) (w out 8 layer)))

(defun write-mesh-r12 (out data)
  (let ((layer (or (dxf 8 data) "0")) (vs (mesh-verts data)) (fs (mesh-faces data)))
    (wr out 0 "POLYLINE") (wr out 8 layer)
    (when (dxf 62 data) (wr out 62 (dxf 62 data)))
    (wr out 66 1) (wr-pt out 10 (pt 0 0 0)) (wr out 70 64) (wr out 71 (length vs)) (wr out 72 (length fs))
    (dolist (v vs) (wr out 0 "VERTEX") (wr out 8 layer) (wr-pt out 10 (pt3 v)) (wr out 70 192))
    (dolist (f fs)
      (wr out 0 "VERTEX") (wr out 8 layer) (wr-pt out 10 (pt 0 0 0)) (wr out 70 128)
      (loop for i in f for c from 71 do (wr out c i)))
    (wr out 0 "SEQEND") (wr out 8 layer)))
