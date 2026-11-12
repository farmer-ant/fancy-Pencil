;; TXTEXP / EXPLODE on text: letters become closed outlines, capitals as tall as the text height.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-txtexp.lisp
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
(defun cnt (ty) (count ty (live-entities) :key #'ent-type :test #'equal))
(defun all-pts () (loop for e in (live-entities) append (loop for x in (ename-data e) when (eql (car x) 10) collect (cdr x))))
(defun near (a b tol) (< (abs (- a b)) tol))

;; the font reader, on a font every Fancy Pencil system has
(let ((f (ignore-errors (ttf-open (font-file-for-family "DejaVu Sans" nil nil)))))
  (when f
    (chk "DejaVu Sans: capitals 0.729 em" (near (ttf-cap-height f) 0.729d0 0.002) t)
    (chk "DejaVu Sans: H is 0.752 em wide" (near (first (ttf-glyph f (char-code #\H))) 0.752d0 0.002) t)
    (chk "B has 3 outlines, o 2" (list (length (second (ttf-glyph f 66))) (length (second (ttf-glyph f 111)))) '(3 2))
    (chk "a space has no outlines" (second (ttf-glyph f 32)) nil)))

;; capitals only: they stand on the baseline and are exactly the text height
(al "(command \"text\" '(5 5) 2 0 \"HEX\" \"\")")
(al "(command \"txtexp\" (entlast) \"\")")
(chk "the text is gone" (cnt "TEXT") 0)
(chk "H, E, X: one outline each" (cnt "LWPOLYLINE") 3)
(chk "outlines are closed" (every (lambda (e) (eql (logand (or (dxf 70 (ename-data e)) 0) 1) 1)) (live-entities)) t)
(let ((ys (mapcar #'py (all-pts))) (xs (mapcar #'px (all-pts))))
  (chk "bottom on the baseline" (near (reduce #'min ys) 5d0 0.01) t)
  (chk "top at the text height" (near (reduce #'max ys) 7d0 0.01) t)
  (chk "starts at the insertion point" (near (reduce #'min xs) 5d0 0.5) t))
(al "(command \"u\")")
(chk "undo brings the text back" (list (cnt "TEXT") (cnt "LWPOLYLINE")) '(1 0))
(al "(command \"erase\" \"all\" \"\")")

;; middle-centre, rotated: the outlines gather round the point
(al "(command \"text\" \"j\" \"mc\" '(10 10) 1 45 \"NOON\" \"\")")
(al "(command \"explode\" (entlast) \"\")")
(let* ((pts (all-pts)) (xs (mapcar #'px pts)) (ys (mapcar #'py pts)))
  (chk "middle-centre: centred on the point" (list (near (/ (+ (reduce #'min xs) (reduce #'max xs)) 2) 10d0 0.1)
                                                    (near (/ (+ (reduce #'min ys) (reduce #'max ys)) 2) 10d0 0.1))
       '(t t)))
(chk "EXPLODE on text makes outlines" (> (cnt "LWPOLYLINE") 3) t)
(al "(command \"erase\" \"all\" \"\")")

;; MTEXT explodes into one TEXT per line, and those into outlines
(al "(command \"mtext\" '(0 10) '(30 0) \"Line one\\\\PLine two\" \"\")")
(al "(command \"explode\" (entlast) \"\")")
(chk "MTEXT -> a TEXT per line" (mapcar (lambda (e) (dxf 1 (ename-data e))) (live-entities)) '("Line one" "Line two"))
(al "(command \"explode\" \"all\" \"\")")
(chk "then outlines" (list (cnt "TEXT") (> (cnt "LWPOLYLINE") 10)) '(0 t))

(format t "~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
