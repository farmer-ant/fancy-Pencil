;; Gaps in hatch boundaries: found, measured, and overshoots not mistaken for gaps.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-hatchgaps.lisp
(in-package :fancy-pencil)
(setf *ui* :tty *output-fn* (lambda (s) s))
(load-aliases) (current-doc)
(defvar *pass* 0) (defvar *fail* 0)
(defmacro chk (name form expect)
  `(let ((v (handler-case ,form (error (c) (format nil "ERROR ~A" c)))))
     (if (equalp v ,expect) (incf *pass*) (incf *fail*))
     (format t "~:[FAIL~;ok  ~] ~A: ~S~%" (equalp v ,expect) ,name v)))
(defun al (s) (alisp-eval-string s))
(defun clear () (al "(command \"erase\" \"all\" \"\")"))
(defun gaps () (boundary-gaps (boundary-segments (hatch-boundary-ents))))
(defun r3 (x) (/ (round (* 1000 x)) 1000))
(defun hatches () (count "HATCH" (live-entities) :key #'ent-type :test #'equal))
(defvar *said* "")
(setf *output-fn* (lambda (s) (setf *said* (concatenate 'string *said* s)) s))

;; a closed square: no gaps
(al "(command \"line\" '(0 0) '(10 0) '(10 10) '(0 10) \"c\")")
(chk "closed square: no gaps" (gaps) nil)
(clear)

;; the last side stops 0.3 short of the corner
(al "(command \"line\" '(0 0) '(10 0) '(10 10) '(0 10) '(0 0.3) \"\")")
(chk "one gap, 0.3 wide, at the corner"
     (mapcar (lambda (g) (list (r3 (px (first g))) (r3 (py (first g))) (r3 (second g)))) (gaps)) '((0 3/20 3/10)))
(setf *said* "")
(al "(command \"-hatch\" '(5 5) \"\")")
(chk "HATCH there: no hatch, and it says where the gap is"
     (list (hatches) (and (search "1 gap" *said*) t) (and (search "0.3000 wide" *said*) t)) '(0 t t))
(chk "the gap is circled (until the drawing changes)" (length (cdr *gap-marks*)) 1)
(clear)

;; lines that cross and run past each other (overshoots) are closed, not gaps
(al "(command \"line\" '(-1 0) '(11 0) \"\")") (al "(command \"line\" '(10 -1) '(10 11) \"\")")
(al "(command \"line\" '(11 10) '(-1 10) \"\")") (al "(command \"line\" '(0 11) '(0 -1) \"\")")
(chk "overshooting corners: no gaps" (gaps) nil)
(al "(command \"-hatch\" '(5 5) \"\")")
(chk "and the hatch fills it" (hatches) 1)
(clear)

;; two loose ends near each other, missing: a gap between them
(al "(command \"line\" '(0 0) '(10 0) '(10 10) '(0 10) '(0 5) \"\")")
(al "(command \"line\" '(0.4 0) '(0.4 4.8) \"\")")
(chk "a line ending near another loose end: the gap between them"
     (let ((g (find-if (lambda (g) (< (second g) 0.5)) (gaps)))) (and g (r3 (second g)))) (r3 (sqrt (+ (* 0.4 0.4) (* 0.2 0.2)))))
(clear)

;; a room inside a bigger frame, with a gap in its wall: the hatch escapes into the frame
(al "(command \"rectang\" '(-20 -20) '(30 30))")
(al "(command \"line\" '(0 0) '(10 0) '(10 10) '(0 10) '(0 0.3) \"\")")
(setf *said* "")
(al "(command \"-hatch\" '(5 5) \"\")")
(chk "the hatch escaped, and it says so, with the gap circled"
     (list (hatches) (and (search "may have escaped" *said*) t) (length (cdr *gap-marks*))) '(1 t 1))
(clear)

;; HATCHGAPS on its own
(al "(command \"line\" '(0 0) '(10 0) '(10 10) '(0 10) '(0 0.3) \"\")")
(setf *gap-marks* nil *said* "")
(al "(command \"hatchgaps\" \"\")")
(chk "HATCHGAPS: everything in view" (list (length (cdr *gap-marks*)) (and (search "1 gap found" *said*) t)) '(1 t))
(al "(command \"line\" '(0 0.3) '(0 0) \"\")")
(chk "the drawing changed (gap closed): the circles go" (gap-mark-cmds) nil)
(al "(command \"hatchgaps\" \"\")")
(chk "and HATCHGAPS finds none now" (and (search "No gaps found" *said*) t) t)

(format t "~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
