;; Grip STRETCH in 3D: only the grip that was picked moves, even when another sits right
;; above or below it (a line standing straight up).
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-grips3d.lisp
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

(al "(command \"line\" '(1 0 -8) '(1 0 4) \"\")")
(let* ((e (last-entity)) (hits (grips-on-point (list e) (pt 1 0 4))))
  (chk "only the top end is on the top grip" (cdr (first hits)) '(:end))
  (let ((nd (reduce (lambda (dd id) (grip-stretch dd id (pt 1 0 4) (pt 3 0 6))) (cdr (first hits)) :initial-value (ent-data e))))
    (chk "stretched: the bottom stays" (list (pt3 (dxf 10 nd)) (pt3 (dxf 11 nd))) (list (pt 1 0 -8) (pt 3 0 6)))))
(al "(command \"line\" '(5 5 0) '(5 5 10) \"\")")
(al "(command \"line\" '(5 5 0) '(9 5 0) \"\")")
(chk "two lines meeting at a corner both stretch from it"
     (length (grips-on-point (last (live-entities) 2) (pt 5 5 0))) 2)

(format t "~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
