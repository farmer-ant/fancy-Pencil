;;;; polyhedra.lisp — the five Platonic solids (tetrahedron 4, cube 6, octahedron 8, dodecahedron 12,
;;;; icosahedron 20 faces) and the truncated icosahedron (the soccer ball: 12 pentagons, 20 hexagons),
;;;; as 3D solids.
;;;;
;;;; POLYHEDRON asks which, then where: the solid stands on one of its faces, that face centred on
;;;; the point you give in the working plane (or [Center]: its middle at the point). Its size is
;;;; given as the length of an edge, or [Radius] (to its corners). The commands TETRAHEDRON,
;;;; OCTAHEDRON, DODECAHEDRON, ICOSAHEDRON and SOCCERBALL go straight to one shape.

(in-package :fancy-pencil)

(defparameter +phi+ (/ (+ 1 (sqrt 5d0)) 2) "The golden ratio.")

(defun signed-variants (triple)
  "All sign combinations of TRIPLE (zeros left alone), without repeats."
  (let ((out (list nil)))
    (dolist (c triple)
      (setf out (loop for o in out
                      append (if (zerop c) (list (cons 0d0 o)) (list (cons (d c) o) (cons (d (- c)) o))))))
    (remove-duplicates (mapcar (lambda (l) (apply #'pt (reverse l))) out) :test #'equalp)))

(defun even-permutations (p)
  (list (pt (px p) (py p) (pz p)) (pt (py p) (pz p) (px p)) (pt (pz p) (px p) (py p))))

(defun polyhedron-vertices (kind)
  (let ((f +phi+))
    (ecase kind
      (:tetrahedron (list (pt 1 1 1) (pt 1 -1 -1) (pt -1 1 -1) (pt -1 -1 1)))
      (:cube (signed-variants (list 1 1 1)))
      (:octahedron (loop for p in (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1)) append (list p (v* p -1d0))))
      (:dodecahedron (append (signed-variants (list 1 1 1))
                             (loop for tr in (list (list 0 (/ 1 f) f) (list (/ 1 f) f 0) (list f 0 (/ 1 f)))
                                   append (signed-variants tr))))
      (:icosahedron (loop for tr in (list (list 0 1 f) (list 1 f 0) (list f 0 1)) append (signed-variants tr)))
      (:soccer
       (remove-duplicates
        (loop for tr in (list (list 0 1 (* 3 f)) (list 1 (+ 2 f) (* 2 f)) (list f 2 (+ (* 2 f) 1)))
              append (loop for p in (signed-variants tr) append (even-permutations p)))
        :test (lambda (a b) (< (dist a b) 1d-9)))))))

(defun hull-faces (vs)
  "Faces of the convex solid with corners VS: lists of 0-based indices, counterclockwise seen from outside."
  (let* ((v (coerce vs 'vector)) (n (length v))
         (centre (v* (reduce #'v+ vs) (/ 1d0 n)))
         (size (reduce #'max (mapcar (lambda (p) (dist p centre)) vs)))
         (eps (* 1d-7 size)) (planes nil) (faces nil))
    (loop for i below n do
      (loop for j from (1+ i) below n do
        (loop for k from (1+ j) below n do
          (let* ((a (aref v i)) (nrm (cross3 (v- (aref v j) a) (v- (aref v k) a))) (len (sqrt (vdot3 nrm nrm))))
            (when (> len 1d-9)
              (let* ((nu (v* nrm (/ 1d0 len))) (nu (if (< (vdot3 nu (v- a centre)) 0) (v* nu -1d0) nu))
                     (off (vdot3 nu a)))
                (when (and (notany (lambda (pl) (and (< (dist (first pl) nu) 1d-6) (< (abs (- (second pl) off)) eps))) planes)
                           (loop for q across v always (<= (vdot3 nu q) (+ off eps))))
                  (push (list nu off) planes))))))))
    (dolist (pl planes faces)
      (destructuring-bind (nu off) pl
        (let* ((idx (loop for q across v for m from 0 when (< (abs (- (vdot3 nu q) off)) eps) collect m))
               (c (v* (reduce #'v+ (mapcar (lambda (m) (aref v m)) idx)) (/ 1d0 (length idx))))
               (ax (unit3 (v- (aref v (first idx)) c))) (ay (cross3 nu ax)))
          ;; counterclockwise about the outward normal
          (push (sort idx #'< :key (lambda (m) (let ((w (v- (aref v m) c))) (atan (vdot3 w ay) (vdot3 w ax)))))
                faces))))))

(defun turning-onto (a b)
  "A function turning unit vector A onto unit vector B."
  (let* ((axis (cross3 a b)) (s (sqrt (vdot3 axis axis))) (c (vdot3 a b)))
    (cond ((> s 1d-12) (let ((k (v* axis (/ 1d0 s))) (ang (atan s c))) (lambda (p) (rotate-about p k ang))))
          ((> c 0) #'identity)
          (t (let ((k (any-perpendicular a))) (lambda (p) (rotate-about p k pi)))))))

(defun polyhedron-mesh (kind origin &key edge radius (stand t))
  "A KIND polyhedron with the given EDGE length (or RADIUS to its corners). STAND: resting on its
largest face, that face centred on ORIGIN; else centred on ORIGIN."
  (let* ((vs (polyhedron-vertices kind))
         (faces (hull-faces vs))
         (v (coerce vs 'vector))
         (unit-edge (loop for i below (length v)
                          minimize (loop for j below (length v) unless (= i j) minimize (dist (aref v i) (aref v j)))))
         (unit-rad (reduce #'max (mapcar (lambda (p) (sqrt (vdot3 p p))) vs)))
         (k (if radius (/ radius unit-rad) (/ (or edge 1d0) unit-edge)))
         (vs (mapcar (lambda (p) (v* p k)) vs))
         (v (coerce vs 'vector)))
    (when stand
      ;; turn it so its largest face (the hexagon of the soccer ball) points straight down
      (let* ((base (first (sort (copy-list faces) #'> :key #'length)))
             (pts (mapcar (lambda (m) (aref v m)) base))
             (c (v* (reduce #'v+ pts) (/ 1d0 (length pts))))
             (rot (turning-onto (unit3 c) (pt 0 0 -1)))
             (turned (mapcar rot vs))
             (bc (funcall rot c)))
        (setf vs (mapcar (lambda (p) (v- p (pt (px bc) (py bc) (pz bc)))) turned))
        ;; and square to the axes: the first edge of the base along X
        (let* ((a (nth (first base) vs)) (b (nth (second base) vs))
               (ang (- (atan (- (py b) (py a)) (- (px b) (px a))))))
          (setf vs (mapcar (lambda (p) (rotate-about p (pt 0 0 1) ang)) vs)))))
    (make-mesh-data (mapcar (lambda (p) (v+ p (pt3 origin))) vs)
                    (mapcar (lambda (f) (mapcar #'1+ f)) faces))))

(defparameter *polyhedra*
  '((:tetrahedron "Tetrahedron" "4 triangles")
    (:cube "Cube" "6 squares")
    (:octahedron "Octahedron" "8 triangles")
    (:dodecahedron "Dodecahedron" "12 pentagons")
    (:icosahedron "Icosahedron" "20 triangles")
    (:soccer "Truncated icosahedron" "soccer ball: 12 pentagons, 20 hexagons")))

(defvar *polyhedron-wireframe* nil "Make polyhedra as wireframes (lines in a block) rather than solids;
:frame for solid struts along the edges.")

(defun mesh-edge-segments (mesh)
  (let ((vs (coerce (mesh-verts mesh) 'vector)))
    (remove-duplicates
     (loop for f in (mesh-faces mesh)
           append (loop for (a b) on (append f (list (first f))) while b
                        collect (let ((i (min (abs a) (abs b))) (j (max (abs a) (abs b))))
                                  (cons (aref vs (1- i)) (aref vs (1- j))))))
     :test #'equalp)))

(defun unique-block-name (base)
  (if (not (find-block base)) base
      (loop for n from 2 for nm = (format nil "~A-~D" base n) unless (find-block nm) return nm)))

(defun add-polyhedron-wireframe (mesh name origin)
  "MESH's edges as 3D lines, gathered in a block NAME (one INSERT at ORIGIN): EXPLODE gives the lines."
  (let* ((vs (coerce (mesh-verts mesh) 'vector))
         (edges (remove-duplicates
                 (loop for f in (mesh-faces mesh)
                       append (loop for (a b) on (append f (list (first f))) while b
                                    collect (if (< (abs a) (abs b)) (cons (abs a) (abs b)) (cons (abs b) (abs a)))))
                 :test #'equal))
         (ents (loop for (a . b) in edges
                     collect (add (list (cons 0 "LINE") (cons 10 (aref vs (1- a))) (cons 11 (aref vs (1- b)))))))
         (bn (unique-block-name name)))
    (let ((*output-fn* (lambda (s) s))) (make-block bn origin ents :convert))
    (out (format nil "~A wireframe created: ~D edges, block ~A (EXPLODE it for separate lines).~%" name (length ents) bn))
    (when (gui-p) (gui-regen))
    (car (last (live-entities)))))

(defun polyhedron-command (kind)
  (let* ((info (assoc kind *polyhedra*)) (p nil) (stand t))
    (loop
      (setf p (gpoint (format nil "~A (~A), ~A. Specify center of the face it stands on or [Center/Solid/Wireframe/Frame]: "
                              (second info) (third info)
                              (case *polyhedron-wireframe* ((nil) "solid") (:frame "frame of solid struts") (t "wireframe")))
                      :keywords '("Center" "Solid" "Wireframe" "Frame")))
      (cond ((equal p "Wireframe") (setf *polyhedron-wireframe* t))
            ((equal p "Solid") (setf *polyhedron-wireframe* nil))
            ((equal p "Frame") (setf *polyhedron-wireframe* :frame))
            (t (return))))
    (when (equal p "Center")
      (setf stand nil p (gpoint "Specify center point: ")))
    (when (pointp p)
      (let ((r (get-user-input :distance :base p :keywords '("Radius")
                               :prompt "Specify edge length or [Radius]: ")))
        (let ((edge nil) (radius nil))
          (cond ((equal r "Radius")
                 (let ((x (ask-radius "Specify radius (center to a corner)" p)))
                   (when (and x (plusp x)) (setf radius x))))
                ((realp r) (setf edge (d r)))
                ((pointp r) (setf edge (dist p r))))
          (when (and (or edge radius) (plusp (or edge radius)))
            (let ((mesh (polyhedron-mesh kind (pt3 p) :edge edge :radius radius :stand stand)))
              (case *polyhedron-wireframe*
                ((nil) (add-solid mesh (second info)))
                (:frame (out (format nil "~A frame: ~D edges.~%" (second info) (length (mesh-edge-segments mesh))))
                        (struts-for (mesh-edge-segments mesh)
                                    :centre (v* (reduce #'v+ (mesh-verts mesh)) (/ 1d0 (length (mesh-verts mesh))))))
                (t (add-polyhedron-wireframe mesh (second info) (pt3 p)))))))))))

(defcmd "POLYHEDRON" ("PLATONIC") "Creates a Platonic solid (tetrahedron, cube, octahedron, dodecahedron, icosahedron) or a truncated icosahedron (soccer ball)."
  (let ((k (gkw "Enter shape [Tetrahedron/Cube/Octahedron/Dodecahedron/Icosahedron/Soccer ball] <Tetrahedron>: "
                '("Tetrahedron" "Cube" "Octahedron" "Dodecahedron" "Icosahedron" "Soccer"))))
    (polyhedron-command
     (cond ((null k) :tetrahedron)
           ((string-equal k "Soccer") :soccer)
           (t (intern (string-upcase k) :keyword))))))

(defcmd "TETRAHEDRON" ("TETRA") "Creates a tetrahedron: 4 triangular faces." (polyhedron-command :tetrahedron))
(defcmd "OCTAHEDRON" ("OCTA") "Creates an octahedron: 8 triangular faces." (polyhedron-command :octahedron))
(defcmd "DODECAHEDRON" ("DODECA") "Creates a dodecahedron: 12 pentagonal faces." (polyhedron-command :dodecahedron))
(defcmd "ICOSAHEDRON" ("ICOSA") "Creates an icosahedron: 20 triangular faces." (polyhedron-command :icosahedron))
(defcmd "SOCCERBALL" ("TRUNCICOSA") "Creates a truncated icosahedron (a soccer ball): 12 pentagons and 20 hexagons." (polyhedron-command :soccer))
