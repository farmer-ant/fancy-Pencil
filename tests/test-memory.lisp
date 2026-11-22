;; Options > Memory and speed (0.5.29): the memory limit (used by the launcher at start-up) and speed.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-memory.lisp
(require :sb-posix)
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

(defparameter *home* "/tmp/fp-mem/")
(run-shell (format nil "rm -rf ~A; mkdir -p ~A" *home* *home*))
(defvar *real-home* (getenv "HOME"))
(sb-posix:setenv "HOME" (string-right-trim "/" *home*) 1)

;; the computer, and what Automatic gives
(chk "this computer's memory is known" (and (integerp (ram-mb)) (> (ram-mb) 500)) t)
(chk "Automatic: half the memory, at least 2 GB" (list (auto-heap-mb 64267) (auto-heap-mb 16000) (auto-heap-mb 2000))
     '(32133 8000 2048))
(chk "sizes offered on a 64 GB computer" (mapcar (lambda (mb) (floor mb 1024)) (heap-choices 64267)) '(2 4 8 16 24 32 48))
(chk "  ...on an 8 GB one" (mapcar (lambda (mb) (floor mb 1024)) (heap-choices 8024)) '(2 4))
(chk "automatic speed by memory" (mapcar #'auto-speed '(64267 16000 8024 4000)) '(4 3 2 1))

;; the settings file
(chk "no file: Automatic" (multiple-value-list (read-memory-conf)) '(:auto 0))
(write-memory-conf 16384 3)
(chk "saved and read back" (multiple-value-list (read-memory-conf)) '(16384 3))
(write-memory-conf :auto 0)
(chk "Automatic saved" (multiple-value-list (read-memory-conf)) '(:auto 0))
(with-open-file (o (memory-conf-path) :direction :output :if-exists :supersede) (format o "heap=lots~%speed=9~%"))
(chk "nonsense in the file: Automatic" (multiple-value-list (read-memory-conf)) '(:auto 0))

;; speed: applied at once, never more than a tenth of the memory limit
(chk "Light" (progn (apply-speed 1) (floor (sb-ext:bytes-consed-between-gcs) 1048576)) 50)
(chk "Normal (or a tenth of a small limit)" (progn (apply-speed 2) (floor (sb-ext:bytes-consed-between-gcs) 1048576))
     (min 200 (max 50 (floor (current-heap-mb) 10))))
(chk "Fastest, held to a tenth of the limit"
     (progn (apply-speed 4) (floor (sb-ext:bytes-consed-between-gcs) 1048576))
     (min 2048 (max 50 (floor (current-heap-mb) 10))))
(chk "Automatic follows the computer" (apply-speed 0) (auto-speed (ram-mb)))

;; set-memory: true when the limit changes (it takes effect at the next start)
(write-memory-conf :auto 0)
(chk "limit changed" (set-memory 4096 2) t)
(chk "  ...only the speed changed" (set-memory 4096 1) nil)
(chk "  ...a size above four fifths of the memory is lowered"
     (progn (set-memory 999999 1) (nth-value 0 (read-memory-conf))) (heap-limit-mb (ram-mb)))

;; MEMORY on the command line
(setf *said* nil)
(al "(command \"memory\" \"2\" \"Fast\")")
(chk "report" (said "This computer:") t)
(chk "  ...saved" (multiple-value-list (read-memory-conf)) '(2048 3))
(chk "  ...told it's for the next start" (said "next time Fancy Pencil starts") t)
(al "(command \"memory\" \"Auto\" \"Auto\")")
(chk "back to Automatic" (multiple-value-list (read-memory-conf)) '(:auto 0))

;; the Options window gets everything it shows
(chk "what the Options window gets" (let ((s (memory-dialog-arg))) (and (char= (char s 0) #\{) (search " auto 0 " s) t)) t)

;; the launcher: what it asks SBCL for
(defun launcher-heap (conf)
  (with-open-file (o (memory-conf-path) :direction :output :if-exists :supersede) (write-string conf o))
  (multiple-value-bind (code text)
      (run-shell (format nil "HOME=~A sh -c \"$(sed -n '/^RAM=/,/^fi$/p' ~A)\"'; echo $HEAP'"
                         (string-right-trim "/" *home*) (namestring (merge-pathnames "../fancypencil" *gui-dir*))))
    (declare (ignore code))
    (ignore-errors (parse-integer (string-trim '(#\Newline #\Space) text)))))
(let ((ram (ram-mb)))
  (chk "launcher: Automatic" (launcher-heap (format nil "heap=auto~%")) (max 2048 (floor ram 2)))
  (chk "launcher: a size" (launcher-heap (format nil "heap=2048~%speed=3~%")) 2048)
  (chk "launcher: too big is lowered" (launcher-heap (format nil "heap=999999~%")) (max 2048 (floor (* ram 4) 5))))

(sb-posix:setenv "HOME" *real-home* 1)
(format t "~%Memory tests: ~D passed, ~D failed.~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
