;; TOPOMODEL and ALTXT: terrain from survey elevation text (after the Autocad_Topomaker add-in).
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-topo.lisp
(in-package :fancy-pencil)
(defvar *said* nil)
(setf *ui* :tty *output-fn* (lambda (s) (push s *said*) s))
(load-aliases) (current-doc)
(defvar *pass* 0) (defvar *fail* 0)
(defmacro chk (name form expect)
  `(let ((v (handler-case ,form (error (c) (format nil "ERROR ~A" c)))))
     (if (equalp v ,expect) (incf *pass*) (incf *fail*))
     (format t "~:[FAIL~;ok  ~] ~A: ~S~%" (equalp v ,expect) ,name v)))
(defun al (s) (alisp-eval-string s))
(defun said (part) (and (search part (format nil "~{~A~}" (reverse *said*))) t))
(defun meshes () (remove "POLYFACE" (live-entities) :key #'ent-type :test-not #'equal))
(defun text (x y s &optional (layer "ELEV") (type "TEXT"))
  (al (format nil "(entmake '((0 . ~S) (8 . ~S) (10 ~F ~F 0.0) (40 . 0.2) (1 . ~S)))" type layer x y s)))

;; the numbers in the words
(chk "plain number" (first-number "12.5") 12.5d0)
(chk "comma as the decimal point" (first-number "EL 12,5 m") 12.5d0)
(chk "negative" (first-number "h=-3.25") -3.25d0)
(chk "no number" (first-number "BM") nil)
(chk "first of several" (first-number "7 (was 9)") 7d0)

;; Delaunay: a square's four corners make two triangles; a grid of 5x5 makes 32
(chk "square: 2 triangles" (length (delaunay (vector (pt 0 0) (pt 1 0) (pt 1 1) (pt 0 1)))) 2)
(chk "5x5 grid: 32 triangles"
     (length (delaunay (coerce (loop for i below 5 append (loop for j below 5 collect (pt i j))) 'vector))) 32)
(chk "no point inside any circumcircle"
     (let* ((pts (coerce (loop repeat 60 collect (pt (random 100d0) (random 100d0))) 'vector)))
       (loop for tr in (delaunay pts)
             always (let ((c (make-tri pts (first tr) (second tr) (third tr))))
                      (loop for i below (length pts)
                            never (and (not (member i tr)) (in-circumcircle-p c (aref pts i)))))))
     t)

;; a survey: z = 10 + x + 2y over a 5x5 grid of texts on layer ELEV (green)
(ensure-layer "ELEV")
(text 0 0 "10.00") (al "(setq s (entlast))")
(loop for i below 5 do (loop for j below 5 unless (and (= i 0) (= j 0))
                             do (text i j (if (= i 2) (format nil "EL ~D,0" (+ 10 i (* 2 j))) (format nil "~D" (+ 10 i (* 2 j)))))))
(text 1 1 "5")                                   ; stacked under another: the higher one (13) is kept
(text 9 9 "999" "NOTES")                          ; another layer: not a point
(text 8 8 "BM" "ELEV")                            ; no number
(text 3 0.5 "{\\fArial|b0;14.0}" "ELEV" "MTEXT")  ; an MTEXT point, formatting codes and all (z 10+3+1 = 14)
(setf *said* nil)
(al "(command \"topomodel\" s \"\")")
(chk "points reported" (said "26 points, 1 stacked") t)
(chk "one without a number" (said "1 without a number") t)
(chk "triangles" (said "Delaunay triangulation:") t)
(chk "two meshes: surface and solid" (length (meshes)) 2)
(let* ((surf (find "Topo_Surface_Visualization" (meshes) :key (lambda (e) (dxf 8 (ent-data e))) :test #'equal))
       (solid (find-if-not (lambda (e) (eq e surf)) (meshes))))
  (chk "surface on its layer" (and surf t) t)
  (chk "surface has one vertex per point" (length (mesh-verts (ent-data surf))) 26)
  (chk "surface heights" (sort (remove-duplicates (mapcar #'pz (mesh-verts (ent-data surf)))) #'<)
       '(10d0 11d0 12d0 13d0 14d0 15d0 16d0 17d0 18d0 19d0 20d0 21d0 22d0))
  ;; ground under z = 10 + x + 2y over [0,4]^2 down to 0: 16 * (10 + 2 + 4) = 256
  (chk "solid volume" (round (* 1000 (mesh-signed-volume (ent-data solid)))) 256000)
  (chk "solid closed: every edge twice" 
       (let ((h (make-hash-table :test #'equal)))
         (dolist (f (mesh-faces (ent-data solid)))
           (let ((f (mapcar #'abs f)))
             (loop for (a b) on (append f (list (first f))) while b
                   do (incf (gethash (cons (min a b) (max a b)) h 0)))))
         (loop for v being the hash-values of h always (= v 2)))
       t))

;; Surface only; a base above the ground refused
(al "(command \"erase\" (ssget \"x\" '((0 . \"POLYFACE\"))) \"\")")
(al "(command \"topomodel\" s \"s\")")
(chk "Surface only: one mesh" (length (meshes)) 1)
(setf *said* nil) (al "(command \"topomodel\" s 12)")
(chk "base above the lowest point: no solid" (said "no solid") t)
(chk "  ...surface still made" (length (meshes)) 2)

;; not enough points
(al "(command \"erase\" \"all\" \"\")")
(text 0 0 "1" "FEW") (al "(setq s (entlast))") (text 1 0 "2" "FEW")
(setf *said* nil) (al "(command \"topomodel\" s)")
(chk "two points: too few" (said "At least three points") t)
(chk "  ...nothing made" (length (meshes)) 0)

;; ALTXT: texts beside their circles move onto the centres; one too far away stays
(al "(command \"erase\" \"all\" \"\")")
(al "(command \"circle\" '(10 10) 0.5)") (al "(setq c (entlast))")
(al "(command \"circle\" '(20 10) 0.5)") (al "(command \"circle\" '(30 10) 0.5)")
(al "(command \"circle\" '(40 10) 2)")             ; another size: not a survey mark
(text 10.8 10.3 "101.2") (al "(setq tx (entlast))")
(text 20.5 9.2 "101.9") (text 33 13 "100.4") (text 60 60 "99.0") (text 41.2 10 "77")
(setf *said* nil)
(al "(command \"altxt\" c tx)")
(chk "report" (said "3 survey marks, 5 texts like the sample: moved 2 text objects") t)
(flet ((at (s) (let ((e (find s (live-entities) :key (lambda (e) (dxf 1 (ent-data e))) :test #'equal)))
                 (let ((p (dxf 10 (ent-data e)))) (list (round (px p)) (round (py p)))))))
  (chk "onto its circle" (at "101.2") '(10 10))
  (chk "onto the nearest" (at "101.9") '(20 10))
  (chk "too far away: stays" (at "100.4") '(33 13))
  (chk "far away: stays" (at "99.0") '(60 60))
  (chk "near only a circle of another size: stays" (at "77") '(41 10)))
(format t "~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
