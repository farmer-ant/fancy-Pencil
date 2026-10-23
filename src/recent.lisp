;;;; recent.lisp — File > Recent Files: the last drawings you opened or saved, kept in
;;;; ~/.config/fancypencil/recent.lisp so the list is there the next time Fancy Pencil starts.

(in-package :fancy-pencil)

(defparameter *recent-max* 12)
(defvar *recent-files* nil "Full paths, most recent first.")

(defun recent-path ()
  (merge-pathnames ".config/fancypencil/recent.lisp"
                   (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/")))

(defun load-recent ()
  (setf *recent-files*
        (ignore-errors
         (with-open-file (i (recent-path) :external-format :utf-8)
           (with-standard-io-syntax
             (let ((*read-eval* nil) (x (read i nil nil)))
               (and (listp x) (remove-if-not #'stringp x))))))))

(defun save-recent ()
  (ignore-errors
   (let ((p (recent-path)))
     (ensure-directories-exist p)
     (with-open-file (o p :direction :output :if-exists :supersede :external-format :utf-8)
       (with-standard-io-syntax
         (format o ";; Fancy Pencil recent files (written by the program)~%")
         (let ((*print-readably* nil)) (prin1 (mapcar (lambda (x) (coerce x '(simple-array character (*)))) *recent-files*) o)) (terpri o))))))

(defun note-recent (f)
  "Put drawing F at the top of the recent list (window sessions only)."
  (when (and f (gui-p))
    (let ((full (or (ignore-errors (namestring (truename f))) (namestring f))))
      (setf *recent-files* (cons full (remove full *recent-files* :test #'string=)))
      (when (> (length *recent-files*) *recent-max*)
        (setf *recent-files* (subseq *recent-files* 0 *recent-max*)))
      (save-recent)
      (refresh-recent-menu))))

(defun refresh-recent-menu ()
  (when (gui-p)
    (tk "setrecent {~{{~A ~A}~^ ~}}"
        (loop for f in *recent-files* for i from 1
              append (list (tq (file-namestring f)) (tq (directory-namestring f)))))))

(defcmd "OPENRECENT" () "Opens a drawing from the recent files list (File > Recent Files)."
  (let* ((s (or (pop *script-queue*) (gstring "Recent file number: ")))
         (n (ignore-errors (parse-integer (string-trim " " s))))
         (f (and n (nth (1- n) *recent-files*))))
    (cond ((null f) (out (format nil "No such recent file.~%")))
          ((not (probe-file f))
           (tell-user (format nil "~A can't be found any more (it may have been moved, renamed or on a drive that isn't connected).~%~%It has been removed from the recent files list." f))
           (setf *recent-files* (remove f *recent-files* :test #'string=))
           (save-recent) (refresh-recent-menu))
          (t (open-in-tab f)))))

(defcmd "CLEARRECENT" () "Empties the recent files list."
  (setf *recent-files* nil)
  (save-recent) (refresh-recent-menu)
  (out (format nil "Recent files list cleared.~%")))
