;;;; strays.lisp - STRAYS: finds objects far away from the rest of the drawing (a 3D move that
;;;; went a hundred million units up, a circle drawn miles across) and lets you pick each group
;;;; out, so you can move it back or erase it. Such strays make ZOOM Extents show the drawing as
;;;; a speck, and 3D views look at it from very far away.

(in-package :fancy-pencil)

(defun entity-box3 (e)
  "E's 3D box (x0 y0 z0 x1 y1 z1), or NIL."
  (let* ((data (ent-data e)) (pts nil))
    (if (or (offplane-p data) (wcs-type-p (dxf 0 data)))
        (dolist (pc (ignore-errors (entity-pieces e)))
          (case (first pc)
            ((:seg :gseg) (push (pt3 (second pc)) pts) (push (pt3 (third pc)) pts))
            (:pt (push (pt3 (second pc)) pts))))
        (let ((b (ignore-errors (entity-bbox e))))
          (when b
            (let* ((z0 (entity-elevation data)) (z1 (+ z0 (d (or (dxf 39 data) 0)))))
              (setf pts (list (pt (first b) (second b) z0) (pt (third b) (fourth b) z1)))))))
    (and pts (points-bbox3 pts))))

(defun box3-size (b) (max (- (fourth b) (first b)) (- (fifth b) (second b)) (- (sixth b) (third b))))

(defun gap-split (items gap)
  "ITEMS (entity . box) split wherever, along X, Y or Z, nothing comes within GAP: groups of items."
  (labels ((split-axis (items k)
             (let ((sorted (sort (copy-list items) #'< :key (lambda (it) (nth k (cdr it)))))
                   (groups nil) (cur nil) (hi nil))
               (dolist (it sorted)
                 (let ((lo (nth k (cdr it))) (top (nth (+ k 3) (cdr it))))
                   (when (and cur (> lo (+ hi gap)))
                     (push cur groups) (setf cur nil hi nil))
                   (push it cur)
                   (setf hi (if hi (max hi top) top))))
               (when cur (push cur groups))
               groups))
           (split (items)
             (loop for k below 3
                   for parts = (split-axis items k)
                   when (cdr parts) return (mapcan #'split parts)
                   finally (return (list items)))))
    (split items)))

(defun box3-union (boxes)
  (list (reduce #'min boxes :key #'first) (reduce #'min boxes :key #'second) (reduce #'min boxes :key #'third)
        (reduce #'max boxes :key #'fourth) (reduce #'max boxes :key #'fifth) (reduce #'max boxes :key #'sixth)))

(defun find-strays (&optional (ents (remove-if-not #'entity-visible-p (live-entities))))
  "The drawing's objects in groups that lie far apart: (values groups giants gap), each group a
list of entities, the one with most objects first. GIANTS are objects so big that they would
hide the rest (over 1000 times the usual object's size)."
  (let* ((items (loop for e in ents for b = (entity-box3 e) when b collect (cons e b)))
         (sizes (sort (mapcar (lambda (it) (box3-size (cdr it))) items) #'<))
         (median (if sizes (max 1d-6 (nth (floor (length sizes) 2) sizes)) 1d0))
         (giant-size (* 1000 median))
         (giants (remove-if-not (lambda (it) (> (box3-size (cdr it)) giant-size)) items))
         (rest (set-difference items giants))
         (gap (* 20 median))
         (groups (and rest (gap-split rest gap))))
    (values (mapcar (lambda (g) (mapcar #'car g)) (sort groups #'> :key #'length))
            (mapcar #'car giants)
            gap)))

(defun zoom-to-entities (ents)
  "Zoom so that ENTS fill the view (in 3D views too)."
  (let ((b (view-extents ents)))
    (when b
      (destructuring-bind (x0 y0 x1 y1) b
        (let ((pad (* 0.05 (max 1d-6 (- x1 x0) (- y1 y0)))))
          (push-view)
          (zoom-to-box (- x0 pad) (- y0 pad) (+ x1 pad) (+ y1 pad) 0.96d0))))))

(defun describe-group (ents)
  (let* ((b (box3-union (mapcar #'entity-box3 ents)))
         (types (remove-duplicates (mapcar #'ent-type ents) :test #'equal))
         (layers (remove-duplicates (mapcar #'ent-layer ents) :test #'string-equal)))
    (format nil "~D object~:P (~{~A~^, ~}), ~A x ~A x ~A, around X ~A  Y ~A  Z ~A, layer~P ~{~A~^, ~}"
            (length ents) (mapcar #'string-downcase (subseq types 0 (min 3 (length types))))
            (fmt-len (- (fourth b) (first b))) (fmt-len (- (fifth b) (second b))) (fmt-len (- (sixth b) (third b)))
            (fmt-len (/ (+ (first b) (fourth b)) 2)) (fmt-len (/ (+ (second b) (fifth b)) 2)) (fmt-len (/ (+ (third b) (sixth b)) 2))
            (length layers) (subseq layers 0 (min 3 (length layers))))))

(defcmd "STRAYS" ("FINDSTRAYS") "Finds objects lying far from the rest of the drawing, or far too big for it, to move back or erase."
  (multiple-value-bind (groups giants gap) (find-strays)
    (cond
      ((and (null (cdr groups)) (null giants))
       (out (format nil "No strays: the drawing's ~D object~:P lie together.~%" (length (first groups)))))
      (t
       (let ((shown (subseq groups 0 (min 9 (length groups)))))
         (out (format nil "The drawing lies in ~D group~:P (more than ~A apart):~%" (length groups) (fmt-len gap)))
         (loop for g in shown for i from 1
               do (out (format nil "  ~D: ~A~%" i (describe-group g))))
         (when (> (length groups) 9)
           (out (format nil "  ... and ~D smaller group~:P.~%" (- (length groups) 9))))
         (when giants
           (out (format nil "  Large: ~D object~:P far bigger than the rest, up to ~A across (~{~A~^, ~})~%"
                        (length giants) (fmt-len (reduce #'max giants :key (lambda (e) (box3-size (entity-box3 e)))))
                        (remove-duplicates (mapcar (lambda (e) (string-downcase (ent-type e))) giants) :test #'equal))))
         (out (format nil "Pick a group to select it and zoom to it; then MOVE or ERASE uses it (Previous).~%"))
         (let ((kws (append (loop for i from 1 to (length shown) collect (princ-to-string i))
                            (and giants '("Large")) (list "All" "eXit"))))
           (loop
             (let ((r (gkw (format nil "Select group [~{~A~^/~}] <eXit>: " kws) kws "eXit")))
               (when (or (null r) (string-equal r "eXit")) (return))
               (let ((ents (cond ((string-equal r "Large") giants)
                                 ((string-equal r "All") (remove-if-not #'entity-visible-p (live-entities)))
                                 (t (nth (1- (parse-integer r)) shown)))))
                 (setf ents (remove-if-not #'ent-live-p ents))
                 (setf *previous-selection* ents)
                 (set-pickfirst ents)
                 (when (gui-p) (zoom-to-entities ents)))))))))))
