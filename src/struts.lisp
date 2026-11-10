;;;; struts.lisp — solid bars along the lines of a wireframe: each straight edge becomes a solid
;;;; the length of the edge, with a Rectangle, Polygon or Circle cross-section, and (if you like) a
;;;; ball at every corner where edges meet.
;;;;
;;;; STRUTS (WIRESOLID) works on lines, polylines (each straight segment), 3D polylines and blocks
;;;; of them - a polyhedron wireframe, for instance. POLYHEDRON and the shape commands have a
;;;; Frame mode that makes the struts straight away.
;;;;
;;;; Rectangles are turned so their Depth points away from the middle of the frame (outward on a
;;;; polyhedron) and their Width runs along the frame's surface; on a flat frame the Depth stands
;;;; up out of its plane.

(in-package :fancy-pencil)

(defvar *strut-section* '(:circle 0.1d0) "Last section used: (:circle d) (:polygon n d) (:rectangle w h).")
(defvar *strut-balls* t "Put a ball at each corner (last answer).")

(defun selection-segments (ents)
  "Straight segments ((a . b) ...) of the lines, polylines and blocks in ENTS; and the count of
objects that had none (curves)."
  (let ((segs nil) (skipped 0))
    (dolist (e ents)
      (let* ((data (ent-data e)) (ty (dxf 0 data))
             (pieces (and (member ty '("LINE" "LWPOLYLINE" "POLYLINE" "INSERT") :test #'equal)
                          (remove-if-not (lambda (pc) (eq (first pc) :seg)) (ignore-errors (snap-pieces e)))))
             (curved (and (member ty '("LWPOLYLINE" "POLYLINE") :test #'equal)
                          (some (lambda (x) (and (consp x) (eql (car x) 42) (> (abs (cdr x)) 1d-9))) data))))
        (if (or (null pieces) curved)
            (incf skipped)
            (dolist (pc pieces)
              (let ((a (pt3 (second pc))) (b (pt3 (third pc))))
                (when (> (dist a b) 1d-9) (push (cons a b) segs)))))))
    ;; the same edge twice (shared by two faces, or drawn twice) only once
    (values (remove-duplicates (nreverse segs)
                               :test (lambda (s q) (or (and (< (dist (car s) (car q)) 1d-7) (< (dist (cdr s) (cdr q)) 1d-7))
                                                       (and (< (dist (car s) (cdr q)) 1d-7) (< (dist (cdr s) (car q)) 1d-7)))))
            skipped)))

(defun segments-centre (segs)
  (let ((n 0) (sum (pt 0 0 0)))
    (dolist (s segs) (setf sum (v+ sum (v+ (car s) (cdr s)))) (incf n 2))
    (if (plusp n) (v* sum (/ 1d0 n)) sum)))

(defun segments-flat-normal (segs)
  "The normal of the plane all SEGS lie in, or NIL when they are not flat."
  (let* ((pts (loop for s in segs append (list (car s) (cdr s))))
         (o (first pts)) (n nil))
    (dolist (p (rest pts))
      (dolist (q (rest pts))
        (unless n
          (let ((c (cross3 (v- p o) (v- q o))))
            (when (> (vdot3 c c) 1d-12) (setf n (unit3 c)))))))
    (and n (every (lambda (p) (< (abs (vdot3 (v- p o) n)) 1d-7)) pts) n)))

(defun section-outline (section u v)
  "The cross-section's corners around (0,0) in the plane spanned by unit vectors U (width) and V (depth)."
  (flet ((at (x y) (v+ (v* u x) (v* v y))))
    (ecase (first section)
      (:circle (let ((r (/ (second section) 2)) (n (max 12 *solid-segments*)))
                 (loop for i below n for a = (* 2 pi (/ i n)) collect (at (* r (cos a)) (* r (sin a))))))
      (:polygon (destructuring-bind (n dia) (rest section)
                  (let ((r (/ dia 2)))
                    ;; a flat side outward (on the depth side)
                    (loop for i below n for a = (+ (/ pi 2) (/ pi n) (* 2 pi (/ i n)))
                          collect (at (* r (cos a)) (* r (sin a)))))))
      (:rectangle (destructuring-bind (w h) (rest section)
                    (let ((x (/ w 2)) (y (/ h 2)))
                      (list (at (- x) (- y)) (at x (- y)) (at x y) (at (- x) y))))))))

(defun strut-mesh (a b section centre flat-normal)
  "A prism from A to B with SECTION, its depth side turned away from CENTRE (or along FLAT-NORMAL)."
  (let* ((axis (unit3 (v- b a)))
         (radial (if flat-normal flat-normal
                     (let* ((m (v* (v+ a b) 0.5d0)) (r (v- m centre)) (r (v- r (v* axis (vdot3 r axis)))))
                       (if (> (vdot3 r r) 1d-12) (unit3 r) (any-perpendicular axis)))))
         (radial (let ((r (v- radial (v* axis (vdot3 radial axis))))) (if (> (vdot3 r r) 1d-12) (unit3 r) (any-perpendicular axis))))
         (width (unit3 (cross3 radial axis)))
         (ring (section-outline section width radial))
         (n (length ring))
         (verts (append (mapcar (lambda (p) (v+ a p)) ring) (mapcar (lambda (p) (v+ b p)) ring)))
         (faces (append (loop for i below n for j = (mod (1+ i) n)
                              collect (list (+ i 1) (+ j 1) (+ j n 1) (+ i n 1)))
                        (list (loop for i from (1- n) downto 0 collect (+ i 1))
                              (loop for i below n collect (+ i n 1))))))
    (make-mesh-data verts faces)))

(defun ask-strut-section ()
  "Ask for the section: [Rectangle/Polygon/Circle] and its sizes. Returns the section or NIL."
  (let* ((last (first *strut-section*))
         (k (gkw (format nil "Section for the struts [Rectangle/Polygon/Circle] <~A>: " (string-capitalize (symbol-name last)))
                 '("Rectangle" "Polygon" "Circle") (string-capitalize (symbol-name last)))))
    (flet ((size (prompt default)
             (let ((x (gdist (format nil "~A <~A>: " prompt (fmt-num default)))))
               (cond ((null x) default) ((realp x) (d x)) (t nil)))))
      (let ((sec
              (cond
                ((string-equal k "Circle")
                 (let ((dd (size "Diameter of the wire" (if (eq last :circle) (second *strut-section*) 0.1d0))))
                   (and dd (plusp dd) (list :circle dd))))
                ((string-equal k "Polygon")
                 (let* ((n0 (if (eq last :polygon) (second *strut-section*) 6))
                        (n (let ((x (gint (format nil "Number of sides <~D>: " n0)))) (if (integerp x) x n0)))
                        (dd (size "Diameter (corner to corner)" (if (eq last :polygon) (third *strut-section*) 0.1d0))))
                   (and (>= n 3) dd (plusp dd) (list :polygon n dd))))
                (t
                 (let* ((w (size "Width" (if (eq last :rectangle) (second *strut-section*) 0.1d0)))
                        (h (and w (size "Depth (pointing outward)" (if (eq last :rectangle) (third *strut-section*) w)))))
                   (and w h (plusp w) (plusp h) (list :rectangle w h)))))))
        (when sec (setf *strut-section* sec))
        sec))))

(defun section-size (section)
  (ecase (first section)
    (:circle (second section))
    (:polygon (third section))
    (:rectangle (max (second section) (third section)))))

(defun build-struts (segs section balls &key centre)
  "Solids along SEGS with SECTION, and balls at their corners when BALLS. Returns the count of solids."
  (let* ((centre (or centre (segments-centre segs)))
         (flat (segments-flat-normal segs))
         (count 0))
    (dolist (s segs)
      (add (strut-mesh (car s) (cdr s) section centre flat))
      (incf count))
    (when balls
      (let ((corners nil) (r (/ (section-size section) 2)))
        (dolist (s segs)
          (dolist (p (list (car s) (cdr s)))
            (unless (find p corners :test (lambda (a b) (< (dist a b) 1d-7))) (push p corners))))
        (dolist (c corners)
          (add (sphere-mesh c (* r (if (eq (first section) :rectangle) 1.15d0 1d0))))
          (incf count))))
    (when (gui-p) (clear-geom-cache) (gui-regen))
    count))

(defun describe-section (section)
  (ecase (first section)
    (:circle (format nil "round, ~A across" (fmt-num (second section))))
    (:polygon (format nil "~D-sided, ~A across the corners" (second section) (fmt-num (third section))))
    (:rectangle (format nil "~A wide by ~A deep" (fmt-num (second section)) (fmt-num (third section))))))

(defun struts-for (segs &key centre)
  (let ((section (ask-strut-section)))
    (when section
      (let ((balls (string-equal (gkw (format nil "A ball at each corner? [Yes/No] <~:[No~;Yes~]>: " *strut-balls*)
                                      '("Yes" "No") (if *strut-balls* "Yes" "No"))
                                 "Yes")))
        (setf *strut-balls* balls)
        (let ((n (build-struts segs section balls :centre centre)))
          (out (format nil "~D strut~:P (~A)~:[~;, with corner balls~]: ~D solids.~%"
                       (length segs) (describe-section section) balls n)))))))

(defcmd "STRUTS" ("WIRESOLID" "STRUT") "Turns the lines of a wireframe into solid bars the length of each line, with a rectangle, polygon or circle section."
  (let ((ents (get-selection "Select the wireframe (lines, polylines, or a block of them): ")))
    (when ents
      (multiple-value-bind (segs skipped) (selection-segments ents)
        (when (plusp skipped)
          (out (format nil "~D object~:P skipped (no straight edges - for curves use SWEEP with a circle).~%" skipped)))
        (if (null segs)
            (out (format nil "No straight edges selected.~%"))
            (progn
              (out (format nil "~D straight edge~:P.~%" (length segs)))
              (struts-for segs)))))))
