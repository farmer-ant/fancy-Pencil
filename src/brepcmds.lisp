;;;; brepcmds.lisp - commands that need exact solids (brep.lisp): FILLETEDGE, CHAMFEREDGE,
;;;; STEPOUT and STEPIN, and SOLIDENGINE, which says which engine makes solids.

(in-package :fancy-pencil)

(defun brep-edge-number (data a b)
  "The number of DATA's exact edge that runs through the drawn edge A-B."
  (let ((m (midpt (pt3 a) (pt3 b))) (best nil) (bd nil))
    (loop for pts across (brep-edges-of data) for k from 0
          when (cdr pts)
            do (loop for (p q) on pts while q
                     do (let ((dd (dist-point-segment-3d m p q)))
                          (when (or (null bd) (< dd bd)) (setf best k bd dd)))))
    best))

(defun dist-point-segment-3d (p a b)
  (let* ((ab (v- b a)) (l2 (vdot3 ab ab))
         (tt (if (< l2 1d-24) 0d0 (max 0d0 (min 1d0 (/ (vdot3 (v- p a) ab) l2))))))
    (dist p (v+ a (v* ab tt)))))

(defun need-exact (what)
  (cond ((brep-available-p) t)
        (t (out (format nil "~A needs the exact solid engine (Open CASCADE), which isn't running.~%~@[  ~A~%~]~
                             Installing Fancy Pencil from its .deb brings it in; see SOLIDENGINE.~%" what *brep-why*))
           nil)))

(defvar *filletedge-radius* 1d0)
(defvar *chamferedge-distance* 1d0)

(defun pick-solid-edges (prompt option ask-size)
  "Edges picked on exact solids until Enter: ((solid . edge-number) ...). OPTION is the keyword that
changes the size (ASK-SIZE asks it)."
  (let ((picked nil))
    (loop
      (let ((r (gpoint (format nil "~A or [~A]: " prompt option) :keywords (list option)
                       :drag (lambda (cur) (let ((ed (solid-edge-at cur)))
                                             (and ed (list (list :edge (second ed) (third ed)))))))))
        (cond ((equal r option) (funcall ask-size))
              ((pointp r)
               (let ((ed (solid-edge-at r)))
                 (cond ((null ed) (out (format nil "No solid edge there.~%")))
                       ((not (brep-data-p (ent-data (first ed))))
                        (out (format nil "That solid is made of facets only (from a mesh or an older file): it has no exact edges.~%")))
                       (t (let ((k (brep-edge-number (ent-data (first ed)) (second ed) (third ed))))
                            (if (find (cons (first ed) k) picked :test #'equal)
                                (out (format nil "Already selected.~%"))
                                (progn (push (cons (first ed) k) picked)
                                       (out (format nil "~D edge~:P selected.~%" (length picked))))))))))
              (t (return (nreverse picked))))))))

(defun edge-op (op size picked what)
  "Fillet or chamfer the PICKED edges (grouped by solid) at SIZE."
  (let ((done 0))
    (dolist (e (remove-duplicates (mapcar #'car picked)))
      (let* ((data (ent-data e)) (edges (mapcar #'cdr (remove-if-not (lambda (p) (eq (car p) e)) picked))))
        (with-brep ()
          (let ((id (brep-new-id)))
            (brep-request (format nil "~A ~D ~D ~A ~{~D~^ ~}" op id (brep-load data) (brep-num size) edges))
            (let ((props (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370 48 420 440)))) data)))
              (set-entity-data e (brep-result id props))
              (incf done (length edges)))))))
    (when (plusp done) (out (format nil "~D edge~:P ~A.~%" done what)))
    (when (gui-p) (gui-regen))))

(defcmd "FILLETEDGE" ("FILLETEDGES") "Rounds the edges of exact solids."
  (when (need-exact "FILLETEDGE")
    (out (format nil "Radius = ~A~%" (fmt-len *filletedge-radius*)))
    (let ((picked (pick-solid-edges "Select an edge" "Radius"
                                    (lambda () (let ((r (get-user-input :distance :prompt (format nil "Enter fillet radius <~A>: " (fmt-len *filletedge-radius*)))))
                                                 (when (and (realp r) (plusp r)) (setf *filletedge-radius* (d r))))))))
      (when picked (edge-op "fillet" *filletedge-radius* picked "filleted")))))

(defcmd "CHAMFEREDGE" ("CHAMFEREDGES") "Bevels the edges of exact solids."
  (when (need-exact "CHAMFEREDGE")
    (out (format nil "Distance = ~A~%" (fmt-len *chamferedge-distance*)))
    (let ((picked (pick-solid-edges "Select an edge" "Distance"
                                    (lambda () (let ((r (get-user-input :distance :prompt (format nil "Specify distance <~A>: " (fmt-len *chamferedge-distance*)))))
                                                 (when (and (realp r) (plusp r)) (setf *chamferedge-distance* (d r))))))))
      (when picked (edge-op "chamfer" *chamferedge-distance* picked "chamfered")))))

;;; ---------------------------------------------------------------- facets into exact solids

(defun facets-to-brep (data)
  "The helper's number for a solid of flat faces made from mesh DATA (world coordinates)."
  (let ((id (brep-new-id)) (vs (mesh-verts-vector data)))
    (apply #'brep-request (format nil "polyhedron ~D ~D" id (length (mesh-faces data)))
           (mapcar (lambda (f) (format nil "~D ~{~A~^ ~}" (length f) (mapcar (lambda (i) (brep-pt (aref vs (1- (abs i))))) f)))
                   (mesh-faces data)))
    id))

(defcmd "CONVTOSOLID" ("CONVERTTOSOLID") "Turns facet solids (meshes, STL imports, older solids) into exact solids made of their flat faces."
  (when (need-exact "CONVTOSOLID")
    (let ((ents (remove-if (lambda (e) (or (not (equal (ent-type e) "POLYFACE")) (brep-data-p (ename-data e))))
                           (get-selection "Select facet solids to convert: ")))
          (made 0) (open 0))
      (dolist (e ents)
        (with-brep ()
          (let* ((data (ename-data e)) (id (facets-to-brep data))
                 (solid (multiple-value-bind (ls ok) (brep-request (format nil "check ~D" id)) (declare (ignore ls))
                          (plusp (or (ignore-errors (parse-integer ok :start (1+ (position #\Space ok)))) 0))))
                 (props (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370 48 420 440 :kind)))) data)))
            (if solid
                (progn
                  ;; (in world coordinates, as the mesh was read)
                  (setf (ename-data e) (append (brep-result id) props))
                  (touch)
                  (incf made))
                (incf open)))))
      (out (format nil "~D solid~:P converted.~[~:; ~:*~D not converted: not closed (gaps in the mesh), so not a solid.~]~%" made open))
      (when (gui-p) (gui-regen)))))

;;; ---------------------------------------------------------------- STEP

(defun step-path (prompt default-type)
  (let ((f (get-user-input :string :prompt prompt)))
    (when (and (stringp f) (plusp (length (string-trim " " f))))
      (let ((f (string-trim " \"" f)))
        (if (pathname-type f) f (format nil "~A.~A" f default-type))))))

(defun step-scale-id (id k)
  "A copy of shape ID scaled by K about the origin."
  (if (< (abs (- k 1)) 1d-12) id
      (let ((id2 (brep-new-id)))
        (brep-request (format nil "trsf ~D ~D ~A" id2 id (brep-nums k 0 0 0 0 k 0 0 0 0 k 0)))
        id2)))

(defun default-step-name ()
  (let ((f (drawing-filename *dwg*)))
    (if f (namestring (make-pathname :type "step" :defaults f)) "drawing.step")))

(defcmd "STEPOUT" ("EXPORTSTEP" "STEPEXPORT") "Saves solids as a STEP file (.step), the exchange format of 3D CAD programs and machine shops."
  (when (need-exact "STEPOUT")
    (let* ((ents (remove-if-not (lambda (e) (equal (ent-type e) "POLYFACE")) (get-selection "Select solids to export: ")))
           (path (and ents (let ((f (ask-file t "Export STEP" (default-step-name) "step stp")))
                             (and f (ensure-ext f "step"))))))
      (when (and ents path)
        (with-brep ()
          (let ((exact 0) (faceted 0) (ids nil))
            (dolist (e ents)
              (let ((data (ename-data e)))
                (if (brep-data-p data)
                    (progn (push (brep-load (ename-data e)) ids) (incf exact))
                    ;; a facet solid: written as a solid of flat faces
                    (progn (push (facets-to-brep (ename-data e)) ids) (incf faceted)))))
            ;; STEP files are in millimetres
            (let ((k (file-unit-scale :mm)))
              (setf ids (mapcar (lambda (id) (step-scale-id id k)) ids)))
            (brep-request (format nil "stepwrite ~{~D~^ ~}" (reverse ids)) (namestring (merge-pathnames path)))
            (out (format nil "Wrote ~A: ~D solid~:P~[~:; (~:*~D of them as flat faces)~].~%" path (+ exact faceted) faceted))))))))

(defcmd "STEPIN" ("IMPORTSTEP" "STEPIMPORT") "Brings the solids from a STEP file (.step, .stp) into the drawing."
  (when (need-exact "STEPIN")
    (let ((path (ask-file nil "Import STEP" nil "step stp")))
      (when (and path (probe-file path))
        (with-brep ()
          (let* ((first (brep-new-id))
                 (ids (multiple-value-bind (ls ok) (brep-request (format nil "stepread ~D" first) (namestring (truename path)))
                        (declare (ignore ls))
                        (with-input-from-string (s ok) (loop for x = (read s nil nil) while x collect x)))))
            (setf *brep-next-id* (max *brep-next-id* (+ first (length ids) 1)))
            ;; (STEP files are in millimetres)
            (let ((k (/ 1d0 (file-unit-scale :mm))))
              (dolist (id ids) (add-entity (brep-result (step-scale-id id k)))))
            (out (format nil "~D solid~:P brought in from ~A.~%" (length ids) (file-namestring path)))
            (when (gui-p) (funcall 'zoom-extents))))))))

(defcmd "SOLIDENGINE" () "Says which engine makes 3D solids: exact (Open CASCADE) or facets."
  (if (brep-available-p)
      (multiple-value-bind (ls ok) (brep-request "ping") (declare (ignore ls))
        (out (format nil "Solids are exact: ~A (Open CASCADE).~%~
                          Set SOLIDMODE to 0 to make facet solids instead.~%" ok)))
      (out (format nil "Solids are made of facets: the exact solid engine (Open CASCADE) ~:[isn't running~;is turned off (SOLIDMODE is 0)~].~%~@[  ~A~%~]"
                   (eql (sysvar "SOLIDMODE") 0) (and (not (eql (sysvar "SOLIDMODE") 0)) *brep-why*)))))
