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

;;; outlines of curved sides: the hidden edges where the surface turns away from the viewer
(defvar *mesh-folds* (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key))

(defun mesh-folds (data)
  "Hidden edges between two faces: a vector of (a b normal1 normal2)."
  (or (gethash data *mesh-folds*)
      (setf (gethash data *mesh-folds*)
            (let* ((vs (mesh-verts-vector data)) (n (length vs))
                   (edges (make-hash-table :test #'equal)))
              (dolist (f (mesh-faces data))
                (let ((nrm (newell-normal (mapcar (lambda (i) (aref vs (1- (abs i)))) f))) (k (length f)))
                  (when nrm
                    (loop for i below k
                          for a = (nth i f) for b = (nth (mod (1+ i) k) f)
                          for ia = (1- (abs a)) for ib = (1- (abs b))
                          do (when (and (< -1 ia n) (< -1 ib n) (/= ia ib))
                               (let* ((key (if (< ia ib) (cons ia ib) (cons ib ia)))
                                      (rec (or (gethash key edges) (setf (gethash key edges) (list nil nil)))))
                                 (when (plusp a) (setf (first rec) t))      ; drawn by some face
                                 (push nrm (second rec))))))))
              (let ((out nil))
                (maphash (lambda (key rec)
                           (when (and (not (first rec)) (= (length (second rec)) 2))
                             (push (list (aref vs (car key)) (aref vs (cdr key)) (first (second rec)) (second (second rec))) out)))
                         edges)
                (coerce out 'vector))))))

(defun mesh-silhouette-pieces (data toward)
  "The outline of DATA's curved sides seen looking along -TOWARD."
  (loop for (a b n1 n2) across (mesh-folds data)
        when (minusp (* (vdot3 n1 toward) (vdot3 n2 toward)))
          collect (list :seg a b)))

(defun display-pieces (data &optional xf)
  "Pieces to draw for DATA: a solid's edges plus the outline of its curved sides in this view."
  (if (and (null xf) (equal (dxf 0 data) "POLYFACE") (fboundp 'view-toward-viewer))
      (append (data-pieces data) (mesh-silhouette-pieces data (funcall 'view-toward-viewer)))
      (data-pieces data)))

(defun pick-pieces (e)
  "E's pieces for picking: with a solid, the outline of its curved sides as seen now too."
  (if (and (equal (ent-type e) "POLYFACE") (fboundp 'view-toward-viewer))
      (append (entity-pieces e) (mesh-silhouette-pieces (ent-data e) (funcall 'view-toward-viewer)))
      (entity-pieces e)))

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

(defvar *verts-vectors* (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key))
(defun mesh-verts-vector (data)
  "The corners of mesh DATA as a vector (kept, so big meshes don't walk the list for every face)."
  (let ((vs (mesh-verts data)))
    (or (gethash vs *verts-vectors*) (setf (gethash vs *verts-vectors*) (coerce vs 'vector)))))
(defun mesh-face-points (data f)
  (let ((vs (mesh-verts-vector data))) (mapcar (lambda (i) (aref vs (1- (abs i)))) f)))

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

;;; ---------------------------------------------------------------- exploding solids
;;; EXPLODE on a 3D solid gives its faces, as AutoCAD does: each flat face becomes a region and
;;; each curved one a surface (a cylinder: two round regions and one surface). The mesh's facets
;;; are gathered into faces across their hidden edges, and across edges where the surface bends
;;; less than a solid's own facets do (20 degrees). Exploding a region or surface gives its edges
;;; as lines.

(defparameter *mesh-smooth-angle* (+ (/ pi 9) 1d-6) "Facets meeting at less than this belong to one face.")
(defparameter *mesh-kept-props* '(8 62 6 370 48 420 440) "Properties the exploded parts keep.")

(defun mesh-kept-props (data)
  (remove-if-not (lambda (p) (and (consp p) (member (car p) *mesh-kept-props*))) data))

(defun mesh-face-groups (data)
  "The facets of mesh DATA gathered into faces: a list of lists of facets."
  (let* ((faces (coerce (mesh-faces data) 'vector)) (nf (length faces))
         (vs (mesh-verts-vector data))
         ;; corners at the same place count as one (imported solids repeat them)
         (weld (make-hash-table :test #'equal))
         (vid (map 'vector (lambda (p) (let ((k (list (round (px p) 1d-7) (round (py p) 1d-7) (round (pz p) 1d-7))))
                                         (or (gethash k weld) (setf (gethash k weld) (hash-table-count weld)))))
                   vs))
         (normals (map 'vector (lambda (f) (newell-normal (mesh-face-points data f))) faces))
         (parent (make-array nf :initial-contents (loop for i below nf collect i)))
         (edges (make-hash-table :test #'equal)))
    (labels ((root (i) (if (= (aref parent i) i) i (setf (aref parent i) (root (aref parent i)))))
             (join (a b) (let ((ra (root a)) (rb (root b))) (unless (= ra rb) (setf (aref parent ra) rb))))
             (bend (a b) (let ((na (aref normals a)) (nb (aref normals b)))
                           (if (and na nb) (acos (max -1d0 (min 1d0 (vdot3 na nb)))) 0d0))))
      (dotimes (fi nf)
        (let* ((f (aref faces fi)) (k (length f)))
          (dotimes (i k)
            (let* ((a (nth i f)) (b (nth (mod (1+ i) k) f))
                   (va (aref vid (1- (abs a)))) (vb (aref vid (1- (abs b)))))
              (unless (= va vb)
                (push (cons fi (minusp a)) (gethash (if (< va vb) (cons va vb) (cons vb va)) edges)))))))
      (maphash (lambda (key users)
                 (declare (ignore key))
                 (loop for (u . more) on users
                       do (dolist (w more)
                            (when (or (and (cdr u) (cdr w)) (< (bend (car u) (car w)) *mesh-smooth-angle*))
                              (join (car u) (car w))))))
               edges)
      (let ((groups (make-hash-table)))
        (dotimes (fi nf) (push (aref faces fi) (gethash (root fi) groups)))
        (sort (loop for g being the hash-values of groups collect (nreverse g)) #'> :key #'length)))))

(defun mesh-from-facets (data facets)
  "A mesh of FACETS (faces of mesh DATA), with just the corners they use. Its outline (edges
that only one of the facets has) is made visible."
  (let* ((vs (mesh-verts-vector data)) (map (make-hash-table)) (verts nil) (n 0)
         (uses (make-hash-table :test #'equal)))
    (let ((weld (make-hash-table :test #'equal)) (ids (make-hash-table)))
     (flet ((key (a b)
             (flet ((id (i) (or (gethash (abs i) ids)
                                (setf (gethash (abs i) ids)
                                      (let* ((p (aref vs (1- (abs i))))
                                             (k (list (round (px p) 1d-7) (round (py p) 1d-7) (round (pz p) 1d-7))))
                                        (or (gethash k weld) (setf (gethash k weld) (hash-table-count weld))))))))
               (let ((ia (id a)) (ib (id b)))
                 (cond ((= ia ib) nil) ((< ia ib) (cons ia ib)) (t (cons ib ia)))))))
      (dolist (f facets)
        (loop for (a b) on (append f (list (first f))) while b
              do (let ((k (key a b))) (when k (incf (gethash k uses 0))))))
      (let ((fs (mapcar (lambda (f)
                          (loop for (i nxt) on (append f (list (first f))) while nxt
                                collect (let* ((j (or (gethash (abs i) map)
                                                      (progn (push (aref vs (1- (abs i))) verts)
                                                             (setf (gethash (abs i) map) (incf n)))))
                                               (k (key i nxt))
                                               (outline (and k (= (gethash k uses 0) 1))))
                                          (if (and (minusp i) (not outline)) (- j) j))))
                        facets)))
        (make-mesh-data (nreverse verts) fs))))))

(defun facets-flat-p (data facets)
  (let ((n0 nil))
    (every (lambda (f)
             (let ((n (newell-normal (mesh-face-points data f))))
               (or (null n) (if n0 (> (abs (vdot3 n n0)) (- 1 1d-6)) (setf n0 n)))))
           facets)))

(defun mesh-edge-lines (data)
  "The visible edges of mesh DATA as LINEs."
  (let ((props (mesh-kept-props data)))
    (loop for (a . b) in (mesh-edges data)
          collect (append (list (cons 0 "LINE") (cons 10 a) (cons 11 b)) props))))

(defun explode-mesh (data)
  "A 3D solid (or body): its faces as regions and surfaces. A region or surface: its edges as lines."
  (let* ((kind (dxf :kind data))
         (groups (and (or (null kind) (string-equal kind "Body")) (mesh-face-groups data))))
    (if groups
        (let ((props (mesh-kept-props data)))
          (loop for g in groups
                collect (append (mesh-from-facets data g)
                                props
                                (list (cons :kind (if (facets-flat-p data g) "Region" "Surface"))))))
        (mesh-edge-lines data))))
