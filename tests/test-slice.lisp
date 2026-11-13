;; SLICE: solids cut by a plane come out as closed pieces with the right volumes; meshes with
;; gaps are cut cleanly too.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-slice.lisp
(in-package :fancy-pencil)
(setf *ui* :tty *output-fn* (lambda (s) s))
(load-aliases)
(current-doc)
(defvar *pass* 0) (defvar *fail* 0)
(defmacro chk (name form expect)
  `(let ((v (handler-case ,form (error (c) (format nil "ERROR ~A" c)))))
     (if (equalp v ,expect) (incf *pass*) (incf *fail*))
     (format t "~:[FAIL~;ok  ~] ~A: ~S~%" (equalp v ,expect) ,name v)))
(defun al (s) (alisp-eval-string s))
(defun pieces ()
  (sort (mapcar (lambda (e) (let ((d (ent-data e)))
                              (list (round (mesh-volume d) 0.01) (nth-value 3 (mesh-orientation d)))))
                (live-entities))
        #'> :key #'first))
(defun clear () (al "(command \"erase\" \"all\" \"\")"))

(al "(command \"box\" '(0 0) '(10 10) 10)")
(al "(command \"slice\" \"l\" \"\" \"yz\" '(3 0 0) \"b\")")
(chk "box cut at x=3: 700 + 300, both closed" (mapcar (lambda (p) (list (* 0.01 (first p)) (second p))) (pieces)) '((700.0 t) (300.0 t)))
(clear)

(al "(command \"box\" '(0 0) '(10 10) 10)") (al "(setq b (entlast))")
(al "(command \"cylinder\" '(5 5) 3 10)") (al "(command \"subtract\" b \"\" \"l\" \"\")")
(let ((v0 (mesh-volume (ent-data (first (live-entities))))))
  (al "(command \"slice\" \"l\" \"\" \"xy\" '(0 0 4) \"b\")")
  (let ((ps (pieces)))
    (chk "box with a hole cut across: closed pieces (the cut face has the hole)" (mapcar #'second ps) '(t t))
    (chk "volumes 60% / 40%" (list (round (* 100 (/ (* 0.01 (first (first ps))) v0))) (round (* 100 (/ (* 0.01 (first (second ps))) v0)))) '(60 40))))
(clear)

(al "(command \"box\" '(0 0) '(10 10) 10)")
(al "(command \"slice\" \"l\" \"\" '(0 0) '(10 10) '(9 1))")
(chk "keep one side: a closed half" (mapcar (lambda (p) (list (* 0.01 (first p)) (second p))) (pieces)) '((500.0 t)))
(clear)

;; an open box (no lid) still cuts cleanly: the open part stays open
(al "(command \"box\" '(0 0) '(10 10) 10)")
(let* ((e (first (live-entities))) (d (ent-data e))
       (top (remove-if-not (lambda (f) (every (lambda (i) (> (pz (nth (1- (abs i)) (mesh-verts d))) 9.99)) f)) (mesh-faces d))))
  (set-entity-data e (make-mesh-data (mesh-verts d) (set-difference (mesh-faces d) top :test #'equal))))
(multiple-value-bind (plus minus loose) (mesh-plane-split (ent-data (first (live-entities))) (pt 3 0 0) (pt 1 0 0))
  (chk "open box cut: both parts made, the cut outline is open" (list (and plus t) (and minus t) (plusp loose)) '(t t t)))

(format t "~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
