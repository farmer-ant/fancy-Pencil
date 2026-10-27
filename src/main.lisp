;;;; main.lisp — the Command: loop, dispatch, undo grouping and program entry.

(in-package :fancy-pencil)

(defvar *command-running* nil)

(defvar *in-undo-group* nil)
(defun with-undo-group (thunk)
  (let ((snap (take-snapshot)) (dwg *dwg*) (mc (drawing-modcount *dwg*)) (undos *undo-ran*))
    (unwind-protect (let ((*in-undo-group* t)) (funcall thunk))
      ;; undoing is not itself a step to undo (*undo-ran* counts U/REDO, from any thread);
      ;; the step belongs to the drawing it started in, even if another tab is current now
      (when (and (= undos *undo-ran*) (/= mc (drawing-modcount dwg)))
        (push snap (drawing-undo dwg))
        (setf (drawing-redo dwg) nil)
        (when (> (length (drawing-undo dwg)) 200)
          (setf (drawing-undo dwg) (subseq (drawing-undo dwg) 0 200)))))))

(defparameter *no-repeat-commands*
  '("NEW" "OPEN" "QSAVE" "SAVE" "SAVEAS" "SAVEASR12" "DXFIN" "DXFOUT" "QUIT" "APPLOAD" "SCRIPT"
    "PURGE" "ALIASEDIT" "REINIT" "HELP" "ABOUT" "U" "UNDO" "REDO" "OOPS" "LAYER" "LINETYPE" "LWEIGHT"
    "DIMSTYLE" "UNITS" "SETVAR" "OSNAP" "PROPERTIES" "SELECTALL" "COPYCLIP" "REGEN" "HPSET"
    "CLOSE" "CLOSEALL" "DOCSWITCH" "DOCCLOSE" "DOCNEXT" "DOCPREV" "SYSWINDOWS" "STYLE" "TEXTEDIT"
    "DESELECT" "-OSNAP" "DDOSNAP" "CUTCLIP" "PLOT" "-PLOT")
  "Commands that Enter at an empty prompt does not repeat (file, setup and undo commands).")

(defun remember-command (word name)
  (unless (member name *no-repeat-commands* :test #'string-equal)
    (setf *last-command* word)
    (note-recent-command word)))

(defparameter *no-undo-commands* '("U" "UNDO" "REDO" "MREDO" "ZOOM" "PAN" "REGEN" "HELP" "ABOUT" "LIST" "DIST" "ID" "AREA"
                                    "QSAVE" "SAVE" "SAVEAS" "DXFOUT" "NEW" "OPEN" "QUIT" "COPYCLIP" "SELECTALL"
                                    "CLOSE" "CLOSEALL" "DOCSWITCH" "DOCCLOSE" "DOCNEXT" "DOCPREV" "SYSWINDOWS" "SAVEASR12"
                                    "DESELECT" "OSNAP" "-OSNAP"))

(defun print-lisp-result (v)
  (unless (eq v *null-sym*) (out (format nil "~A~%" (lisp-repr v t)))))

(defun run-lisp-toplevel (thunk &key (print t))
  "Evaluate AutoLISP at the Command: prompt with *error* support; finish any open COMMAND."
  (multiple-value-bind (v failed)
      (with-undo-group (lambda () (call-with-alisp-errors thunk)))
    (if failed
        (co-cancel)
        (progn (co-finish-interactively) (when print (print-lisp-result v))))
    (setf *initget-bits* 0 *initget-keywords* nil)))

(defun log-backtrace (c)
  "Write an internal error and where it happened to stderr (the terminal, or ~/.cache/fancypencil.log)."
  (unless (typep c '(or alisp-cancel alisp-exit alisp-error))
    (ignore-errors
     (format *error-output* "~&Fancy Pencil internal error: ~A~%" c)
     #+sbcl (sb-debug:print-backtrace :count 25 :stream *error-output*)
     (force-output *error-output*))))

(defun run-builtin (name fn)
  (let ((*command-running* name))
    (handler-bind ((error #'log-backtrace))
     (handler-case
        (unwind-protect
             (if (member name *no-undo-commands* :test #'string-equal)
                 (funcall fn)
                 (with-undo-group fn))
          (clear-command-highlight))
      (alisp-cancel () nil)
      (alisp-exit () nil)
      (alisp-error (c) (out (format nil "~A~%" (alisp-error-msg c))))
      (error (c) (out (format nil "; error: ~A~%" (cl-error-text c))))))))

(defun execute-command (text)
  "Run what was typed at the Command: prompt."
  (let* ((raw (string-trim " " text)))
    (cond
      ((zerop (length raw))
       (when *last-command* (execute-command *last-command*)))
      ((char= (char raw 0) #\!)
       (run-lisp-toplevel (lambda () (sym-value (asym (subseq raw 1))))))
      ((member (char raw 0) '(#\( #\'))
       (if (and (char= (char raw 0) #\') (> (length raw) 1) (not (char= (char raw 1) #\()))
           (execute-command (subseq raw 1))
           (run-lisp-toplevel (lambda () (alisp-eval-string raw)))))
      (t
       (let* ((parts (split-first-word raw))
              (word (first parts))
              (name (canonical-command-name word))
              (fn (and name (gethash name *commands*)))
              ;; an alias may point at an AutoLISP command (C:XXX)
              (target (or (gethash (strip-command-prefix word) *aliases*) (strip-command-prefix word)))
              (lisp-fn (or (sym-value (asym (concatenate 'string "C:" (strip-command-prefix word))))
                           (sym-value (asym (concatenate 'string "C:" target))))))
         ;; the rest of the line (e.g. from a menu macro "ZOOM E") is fed as input; whatever of it
         ;; the command didn't use (it stopped early) is dropped, not run as further commands
         (let ((tail *script-queue*) (added (rest parts)))
          (setf *script-queue* (append added *script-queue*))
          (unwind-protect
         (progn
           (cond
             (fn (remember-command word name) (run-builtin name fn))
             ((or (ufunc-p lisp-fn) (subr-p lisp-fn) (and (consp lisp-fn) (listp (car lisp-fn))))
              (setf *last-command* word)
              (run-lisp-toplevel (lambda () (apply-a lisp-fn nil))))
             ((gethash (strip-command-prefix word) *pgp-external*)
              (run-external-command (strip-command-prefix word)))
             ((multiple-value-bind (v found) (gethash (string-upcase word) *sysvars*) (declare (ignore v)) found)
              (handler-case (setvar-prompt word)
                (alisp-cancel () nil)
                (alisp-error (c) (out (format nil "~A~%" (alisp-error-msg c))))))
             (t (out (format nil "Unknown command \"~A\".  Press F1 for help.~%" (string-upcase word))))))
           (when added (drop-unused-macro-input tail added)))))))))

(defun drop-unused-macro-input (tail added)
  "After a command run from a line like \"MVIEW 4 Standard\": if it left words of that line unread,
drop them (only those: anything that was queued before stays)."
  (when (and (tailp tail *script-queue*) (not (eq tail *script-queue*)))
    (let* ((left (ldiff *script-queue* tail))
           (joined (string-trim " " (format nil "~{~A~^ ~}" left)))
           (whole (string-trim " " (format nil "~{~A~^ ~}" added))))
      (when (and (every #'stringp left) (<= (length joined) (length whole))
                 (string-equal joined (subseq whole (- (length whole) (length joined)))))
        (setf *script-queue* tail)))))

(defun split-first-word (s)
  "The command word, then each further word of the line as its own input (a space is Enter, as in
AutoCAD menu macros: \"MVIEW 4 Standard\" answers two prompts)."
  (let ((p (position #\Space s)))
    (if p
        (cons (subseq s 0 p) (remove "" (split-string (string-trim " " (subseq s (1+ p))) #\Space) :test #'string=))
        (list s))))

(defun command-loop-step ()
  "Read one Command: line and run it."
  (if *menu-queue*
      (execute-command (pop *menu-queue*))
      (let ((text (handler-case
                      (get-user-input :command :prompt (if (plusp (length *lisp-buffer*)) "(_> " "Command: "))
                    (alisp-cancel () (setf *lisp-buffer* "") nil))))
        (when text
          (if (or (plusp (length *lisp-buffer*)) (and (plusp (length text)) (char= (char text 0) #\()))
              (let ((all (concatenate 'string *lisp-buffer* (if (plusp (length *lisp-buffer*)) (string #\Newline) "") text)))
                (if (alisp-complete-p all)
                    (progn (setf *lisp-buffer* "") (execute-command all))
                    (setf *lisp-buffer* all)))
              (execute-command text))))))

(defun install-startup-functions ()
  ;; S::STARTUP and acaddoc.lsp style autoload
  (let ((home (getenv "HOME")))
    (dolist (f (list "acaddoc.lsp" "fancypencil.lsp"
                         (and home (concatenate 'string home "/.fancypencil.lsp"))
                         ;; startup file from when the program was called LispDraft
                         (and home (not (probe-file (concatenate 'string home "/.fancypencil.lsp")))
                              (concatenate 'string home "/.lispdraft.lsp"))))
      (when (and f (probe-file f))
        (out (format nil "Loading ~A~%" f))
        (run-lisp-file f))))
  (let ((s (sym-value (sym "S::STARTUP"))))
    (when (or (ufunc-p s) (consp s)) (call-with-alisp-errors (lambda () (apply-a s nil))))))

(defun quit-program ()
  (when (eq *ui* :gui) (ignore-errors (save-prefs)))     ; settings changed with SETVAR are kept too
  (ignore-errors (when *wish-in* (tk "exit")))
  #+sbcl (sb-ext:exit :code 0 :abort t)
  #+ecl (ext:quit 0)
  #+ccl (ccl:quit 0)
  #-(or sbcl ecl ccl) (cl-user::quit))

(defun banner ()
  (format nil "~A ~A — AutoLISP-compatible 2D drafting. Type HELP for commands, or an AutoLISP expression.~%"
          *program-name* *version*))

(defun start (&key (gui t) file files lisp-files)
  "Start Fancy Pencil. With GUI NIL, run on the terminal."
  (setf *gui-dir* (or *gui-dir* (script-directory)))
  (if gui
      (handler-case (gui-init)
        (error (c)
          (format *error-output* "~&Fancy Pencil could not open its window: ~A~%~
                                  (Is a graphical display available? Use --tty for terminal mode.)~%" c)
          (quit-program)))
      (setf *ui* :tty *output-fn* (lambda (s) (write-string s) (force-output) s)))
  (load-aliases)
  (when gui (load-prefs) (apply-background) (load-recent) (refresh-recent-menu) (send-view-picker))
  (out (banner))
  (current-doc)
  (when (and gui (not (or file files))) (zoom-to-box 0 0 12 9))
  ;; every drawing named on the command line opens in its own tab
  (dolist (f (append (and file (list file)) files)) (open-in-tab f))
  (when (gui-p) (refresh-tabs t))
  (install-startup-functions)
  (dolist (f lisp-files) (run-lisp-file f))
  (setf *quit-requested* nil)
  (loop until *quit-requested*
        do (handler-case (handler-bind ((error #'log-backtrace)) (command-loop-step))
             (alisp-exit () (when (eq *ui* :tty) (setf *quit-requested* t)))
             (error (c) (out (format nil "; error: ~A~%" (cl-error-text c))))))
  (quit-program))

(defun run-tty (&rest args) (apply #'start :gui nil args))

(defun main ()
  "Command-line entry: fancypencil [--tty] [--load file.lsp]... [drawing.dxf|dwg ...]"
  (let* ((args #+sbcl (rest sb-ext:*posix-argv*)   ; SBCL removes its own options already
               #+ecl (let* ((all (ext:command-args)) (p (position "--" all :test #'string=)))
                       (and p (subseq all (1+ p))))
               #+ccl (let* ((all ccl:*command-line-argument-list*) (p (position "--" all :test #'string=)))
                       (and p (subseq all (1+ p))))
               #-(or sbcl ecl ccl) nil)
         (gui t) (files nil) (lisp nil))
    (loop while args do
      (let ((a (pop args)))
        (cond ((string= a "--tty") (setf gui nil))
              ((string= a "--load") (push (pop args) lisp))
              ((and (> (length a) 4) (string-equal (subseq a (- (length a) 4)) ".lsp")) (push a lisp))
              ((and (plusp (length a)) (char/= (char a 0) #\-)) (push a files)))))
    (start :gui gui :files (nreverse files) :lisp-files (nreverse lisp))))
