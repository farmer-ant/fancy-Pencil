;; Object snaps on solids: faces (CENter of a flat face, NEArest onto a face), INTersection where
;; edges meet in space or a line goes through a face, PERpendicular to an edge, in plan and 3D views.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-solid-snaps.lisp
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
(defun snap (p bits &optional base)
  (multiple-value-bind (q b) (find-osnap p base bits 0.5d0)
    (and q (list (mapcar (lambda (x) (/ (round x 1d-6) 1d6)) (list (px q) (py q) (pz q))) b))))

(al "(command \"box\" '(0 0) '(10 10) 10)")
(al "(command \"line\" '(5 -5 3) '(5 15 3) \"\")")
(al "(command \"line\" '(20 0 10) '(20 20 10) \"\")")

;; plan (Top) view
(chk "top: centre of the top face" (snap (pt 4 6 0) 4) '((5 5 10) 4))
(chk "top: nearest puts you on the top face" (snap (pt 4 6 0) 512) '((4 6 10) 512))
(chk "top: an edge snap still wins near an edge" (snap (pt 5 0.2 0) (logior 2 4)) '((5 0 10) 2))
(chk "top: perpendicular anywhere along the edge" (snap (pt 10 8 0) 128 (pt 20 3 0)) '((10 3 10) 128))

;; SW isometric
(al "(command \"-view\" \"swiso\")")
(chk "iso: centre of the top face" (snap (pt 3 7 10) 4) '((5 5 10) 4))
(chk "iso: nearest onto the face under the cursor" (snap (pt 3 7 10) 512) '((3 7 10) 512))
(chk "iso: a line through a face" (snap (pt 5 0 3) 32) '((5 0 3) 32))
(chk "iso: edges meeting at a corner" (snap (pt 10 10 10) 32) '((10 10 10) 32))
(chk "iso: perpendicular from a point to an edge" (snap (pt 10 6 10) 128 (pt 20 3 10)) '((10 3 10) 128))
(chk "iso: endpoint beats face centre" (snap (pt 10 10 10) (logior 1 4)) '((10 10 10) 1))

;; a cylinder: the round top's centre (from its edge), and its flat face's centre agree
(al "(command \"erase\" \"all\" \"\")")
(al "(command \"cylinder\" '(30 30) 4 6)")
(chk "iso: centre of a cylinder's top" (snap (pt 31 31 6) 4) '((30 30 6) 4))

(format t "~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
