;;;; history.lisp — the record of the session: every command, prompt, answer and message shown in
;;;; the command line. The command line's own pane keeps only its last couple of thousand lines;
;;;; the whole session is kept here, shown by HISTORY (also TEXTSCR and F2), and written to a log
;;;; file as it happens when LOGFILEON is in effect (LOGFILEMODE 1), as AutoCAD's command log does.
;;;;
;;;; Log files go in ~/.local/share/fancypencil/logs (LOGFILEPATH), one per day.

(in-package :fancy-pencil)

(defparameter +record-limit+ 4000000 "Characters kept in memory (the oldest part is dropped beyond this).")

(defvar *session-record* (make-array 4096 :element-type 'character :adjustable t :fill-pointer 0))
(defvar *session-started* (get-universal-time))
(defvar *log-stream* nil)
(defvar *log-stream-path* nil)

(defun default-log-dir ()
  (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/.local/share/fancypencil/logs/"))

(defun log-dir ()
  (let ((p (sysvar "LOGFILEPATH")))
    (if (and (stringp p) (plusp (length p)))
        (if (char= (char p (1- (length p))) #\/) p (concatenate 'string p "/"))
        (default-log-dir))))

(defun date-string (&optional (ut (get-universal-time)) time)
  (multiple-value-bind (s mi h dd mm yy) (decode-universal-time (or ut (get-universal-time)))
    (if time
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" yy mm dd h mi s)
        (format nil "~4,'0D-~2,'0D-~2,'0D" yy mm dd))))

(defun log-file-path () (format nil "~Afancypencil-~A.log" (log-dir) (date-string)))

(defun close-log ()
  (when *log-stream* (ignore-errors (close *log-stream*)))
  (setf *log-stream* nil *log-stream-path* nil))

(defun log-stream ()
  "The open log file (a new one each day), or NIL when logging is off or the file can't be written."
  (when (eql (sysvar "LOGFILEMODE") 1)
    (let ((path (log-file-path)))
      (unless (and *log-stream* (equal path *log-stream-path*))
        (close-log)
        (setf *log-stream*
              (ignore-errors
               (ensure-directories-exist path)
               (let ((s (open path :direction :output :if-exists :append :if-does-not-exist :create
                                   :external-format :utf-8)))
                 (format s "~&~%===== Fancy Pencil ~A, ~A, ~A =====~%" *version* (date-string nil t)
                         (ignore-errors (drawing-display-name)))
                 s))
              *log-stream-path* (and *log-stream* path)))
      *log-stream*)))

(defun record-output (s)
  "Keep S in the session record (and the log file when it's on)."
  (when (and (stringp s) (plusp (length s)))
    (let ((r *session-record*))
      (when (> (+ (fill-pointer r) (length s)) +record-limit+)
        ;; drop the oldest half, at a line break
        (let* ((cut (or (position #\Newline r :start (floor (fill-pointer r) 2)) (floor (fill-pointer r) 2)))
               (keep (subseq r (1+ cut))))
          (setf (fill-pointer r) 0)
          (loop for c across "[... earlier lines dropped ...]" do (vector-push-extend c r))
          (vector-push-extend #\Newline r)
          (loop for c across keep do (vector-push-extend c r))))
      (loop for c across s do (vector-push-extend c r (max 4096 (floor (length r) 2)))))
    (let ((ls (log-stream)))
      (when ls (ignore-errors (write-string s ls) (force-output ls))))))

(defun session-text () (coerce *session-record* 'simple-string))

;;; ---------- commands ----------

(defcmd "LOGFILEON" () "Starts writing everything shown in the command line to a log file."
  (setf (sysvar "LOGFILEMODE") 1)
  (ignore-errors (save-prefs))
  (if (log-stream)
      (progn
        ;; the session so far goes in too, so the file holds the whole session
        (ignore-errors (write-string (session-text) *log-stream*) (force-output *log-stream*))
        (out (format nil "Log file: ~A~%" *log-stream-path*)))
      (out (format nil "Can't write a log file in ~A~%" (log-dir)))))

(defcmd "LOGFILEOFF" () "Stops writing the command line to the log file."
  (setf (sysvar "LOGFILEMODE") 0)
  (ignore-errors (save-prefs))
  (close-log)
  (out (format nil "Log file off.~%")))

(defun history-dialog ()
  (tk "histdlg ~A ~A ~A ~A" (tq (session-text)) (if (eql (sysvar "LOGFILEMODE") 1) 1 0)
      (tq (log-dir)) (tq (date-string *session-started* t))))

(defcmd "HISTORY" ("TEXTSCR" "HIS") "Shows the whole record of this session: every command, answer and message."
  (if (gui-p)
      (history-dialog)
      (out (session-text))))

(defun history-action (what arg)
  "From the History window: LOG 1/0, CLEAR, SAVE <path>, FOLDER."
  (cond
    ((string= what "LOG")
     (funcall (find-command-function (if (equal arg "1") "LOGFILEON" "LOGFILEOFF"))))
    ((string= what "CLEAR")
     (setf (fill-pointer *session-record*) 0)
     (when (gui-p) (tk ".h configure -state normal; .h delete 1.0 end; .h configure -state disabled")))
    ((string= what "SAVE")
     (let ((f (ask-file t "Save Command History" (format nil "fancypencil-history-~A.txt" (date-string)) "txt log")))
       (when f
         (with-open-file (o (ensure-ext f "txt") :direction :output :if-exists :supersede :external-format :utf-8)
           (write-string (session-text) o))
         (out (format nil "History saved to ~A~%" (ensure-ext f "txt"))))))
    ((string= what "FOLDER")
     (ignore-errors (ensure-directories-exist (log-dir)))
     (run-shell (format nil "(xdg-open ~A >/dev/null 2>&1 &)" (shell-quote (log-dir))))))
  (when (gui-p)
    (tk "if {[winfo exists .hd2]} {histdlg_set ~A ~A}" (tq (session-text)) (if (eql (sysvar "LOGFILEMODE") 1) 1 0))))
