;;;; autosave.lisp — safety copies against crashes, power cuts and other calamities.
;;;; Every SAVETIME minutes (10 to start with; 0 turns it off) each open drawing with changes not
;;;; yet saved is copied, as a DXF, to ~/.local/share/fancypencil/autosave/ — the drawing's own
;;;; file is not touched. A copy is deleted once its drawing is saved or closed normally, so any
;;;; left behind mean Fancy Pencil stopped unexpectedly: the next start offers to open them
;;;; (and DRAWINGRECOVERY, File > Recover Safety Copies, opens them any time).

(in-package :fancy-pencil)

(defvar *autosaves* nil "(doc dxf-path modcount time) for each open drawing with a safety copy.")
(defvar *changed-at* (make-hash-table :test #'eq) "Open drawing -> when its first change since the last copy was noticed.")

(defun autosave-dir ()
  (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/.local/share/fancypencil/autosave/"))

(defun my-pid ()
  #+sbcl (sb-unix:unix-getpid)
  #-sbcl 0)

(defun pid-alive-p (pid)
  (and (integerp pid) (plusp pid) (probe-file (format nil "/proc/~D/" pid)) t))

(defun safe-file-part (s)
  (let ((s (map 'string (lambda (c) (if (or (alphanumericp c) (find c "-_. ")) c #\_)) s)))
    (subseq s 0 (min 60 (length s)))))

(defun autosave-paths (d)
  "The safety copy and its note for open drawing D: (values dxf info)."
  (let* ((base (format nil "~A~A-~D-~D" (autosave-dir) (safe-file-part (doc-name d)) (my-pid) (doc-id d))))
    (values (concatenate 'string base ".dxf") (concatenate 'string base ".info"))))

(defun delete-safety-copy (dxf)
  (ignore-errors (delete-file dxf))
  (ignore-errors (delete-file (make-pathname :type "info" :defaults (pathname dxf)))))

(defun autosave-forget (d)
  "Delete D's safety copy (it was saved or closed normally) - where it was written, even if the
drawing has been renamed since (Save As) - and the one its present name would give."
  (let ((e (assoc d *autosaves*)))
    (when e
      (delete-safety-copy (second e))
      (setf *autosaves* (remove e *autosaves*))))
  (multiple-value-bind (dxf info) (autosave-paths d)
    (ignore-errors (delete-file dxf))
    (ignore-errors (delete-file info))))

(defun autosave-forget-all ()
  "Closing normally: delete every safety copy this Fancy Pencil made."
  (dolist (d (copy-list (symbol-value '*docs*))) (autosave-forget d))
  (dolist (info (ignore-errors (directory (concatenate 'string (autosave-dir) "*.info"))))
    (let ((pl (read-info info)))
      (when (and (listp pl) (eql (getf pl :pid) (my-pid)))
        (delete-safety-copy (namestring (make-pathname :type "dxf" :defaults info))))))
  (dolist (part (ignore-errors (directory (concatenate 'string (autosave-dir) (format nil "*-~D-*.part" (my-pid))))))
    (ignore-errors (delete-file part))))

(defun write-safety-copy (d)
  "Copy open drawing D to its safety file, leaving D's own name and saved state as they were."
  (multiple-value-bind (dxf info) (autosave-paths d)
    (ensure-directories-exist dxf)
    (flet ((doit ()
             (let ((name (drawing-filename *dwg*)) (saved (drawing-saved-modcount *dwg*))
                   (tmp (concatenate 'string dxf ".part")))
               (unwind-protect
                    (progn (dxf-export-2000 tmp)
                           ;; only a finished copy replaces the last one
                           (rename-file tmp dxf))
                 (setf (drawing-filename *dwg*) name (drawing-saved-modcount *dwg*) saved))
               (with-open-file (o info :direction :output :if-exists :supersede :external-format :utf-8)
                 (with-standard-io-syntax
                   (let ((*print-readably* nil))
                     (prin1 (list :original (and name (coerce (namestring name) '(simple-array character (*))))
                                  :name (coerce (doc-name d) '(simple-array character (*)))
                                  :time (get-universal-time) :pid (my-pid))
                            o)))))))
      (if (eq d (current-doc))
          (doit)
          (let ((*dwg* (doc-dwg d)) (*sysvars* (doc-sysvars d))
                (*vx* (doc-vx d)) (*vy* (doc-vy d)) (*vscale* (doc-vscale d)) (*view* (doc-view d)))
            (doit))))
    (let ((dw (if (eq d (current-doc)) *dwg* (doc-dwg d))))
      (setf *autosaves* (cons (list d dxf (drawing-modcount dw) (get-universal-time))
                              (remove d *autosaves* :key #'first))))))

(defun autosave-tick ()
  "Called about once a minute (the window's timer): copy drawings whose changes are SAVETIME minutes old."
  (let ((mins (sysvar "SAVETIME")))
    (when (and (realp mins) (plusp mins) (not (block-editor-p)))
      (let ((now (get-universal-time)))
        (dolist (d (copy-list (symbol-value '*docs*)))
          (unless (doc-block-edit d)
            (let* ((dw (if (eq d (current-doc)) *dwg* (doc-dwg d)))
                   (e (assoc d *autosaves*)))
              (cond
                ;; saved since: its safety copy is no longer needed
                ((not (doc-modified-p d)) (when e (autosave-forget d)))
                ;; changed since the last copy (or never copied), and that was long enough ago
                ((or (null e) (/= (third e) (drawing-modcount dw)))
                 (let ((since (if e (fourth e) (or (gethash d *changed-at*) (setf (gethash d *changed-at*) now)))))
                   (when (>= (- now since) (* 60 mins))
                     (handler-case
                         (progn (write-safety-copy d)
                                (remhash d *changed-at*)
                                (out (format nil "(Safety copy of ~A made.)~%" (doc-name d))))
                       (error (c) (out (format nil "; safety copy of ~A failed: ~A~%" (doc-name d) (cl-error-text c))))))))))))))))

;;; ---------- recovery ----------

(defun read-info (path)
  (ignore-errors
   (with-open-file (i path :external-format :utf-8)
     (with-standard-io-syntax (let ((*read-eval* nil)) (read i nil nil))))))

(defun leftover-safety-copies ()
  "Safety copies left by a Fancy Pencil that is no longer running: ((dxf . info-plist) ...), newest first."
  (let ((out nil))
    (dolist (info (directory (concatenate 'string (autosave-dir) "*.info")))
      (let* ((pl (read-info info))
             (dxf (namestring (make-pathname :type "dxf" :defaults info))))
        (when (and (listp pl) (probe-file dxf) (not (eql (getf pl :pid) (my-pid))) (not (pid-alive-p (getf pl :pid))))
          (push (cons dxf pl) out))))
    (sort out #'> :key (lambda (x) (or (getf (cdr x) :time) 0)))))

(defun describe-copy (c)
  (multiple-value-bind (s mi h dd mo y) (decode-universal-time (or (getf (cdr c) :time) 0))
    (declare (ignore s))
    (format nil "~A  (~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D)" (or (getf (cdr c) :name) "drawing") y mo dd h mi)))

(defun open-safety-copy (c)
  "Open safety copy C in its own tab, named as its drawing (so Save puts it back there), marked changed."
  (let ((orig (getf (cdr c) :original)))
    (open-in-tab (car c))
    ;; not a drawing of yours: keep it off the recent files list
    (setf *recent-files* (remove-if (lambda (f) (search "/fancypencil/autosave/" f)) *recent-files*))
    (save-recent) (refresh-recent-menu)
    (when (and (drawing-filename *dwg*) (search "/fancypencil/autosave/" (drawing-filename *dwg*)))
      (setf (drawing-filename *dwg*) orig (drawing-saved-modcount *dwg*) -1)
      ;; it is now an open drawing again: its copy is looked after as usual
      (ignore-errors (rename-file (car c) (autosave-paths (current-doc))))
      (ignore-errors (delete-file (make-pathname :type "info" :defaults (pathname (car c)))))
      (refresh-tabs t)
      (out (format nil "Recovered ~A. Save it to keep it~:[~; (Save puts it back in ~:*~A)~].~%"
                   (describe-copy c) orig)))))

(defun mark-offered (c)
  "Note in safety copy C's info that it has been offered once (it is not offered again at startup)."
  (let ((info (make-pathname :type "info" :defaults (pathname (car c)))))
    (ignore-errors
     (with-open-file (o info :direction :output :if-exists :supersede :external-format :utf-8)
       (with-standard-io-syntax
         (let ((*print-readably* nil))
           (prin1 (list* :offered t (cdr c)) o)))))))

(defun offer-recovery ()
  "At startup: safety copies left by a crash? Offer them once. Copies offered before stay
available (File > Recover Safety Copies) without being asked about again; after 30 days they go."
  (let* ((cs (leftover-safety-copies))
         (now (get-universal-time))
         (old (remove-if-not (lambda (c) (and (getf (cdr c) :offered)
                                              (> (- now (or (getf (cdr c) :time) now)) (* 30 24 3600))))
                             cs))
         (new (remove-if (lambda (c) (getf (cdr c) :offered)) cs)))
    (dolist (c old) (delete-safety-copy (car c)))
    (when (and new (gui-p))
      (let ((r (gui-confirm (format nil "Fancy Pencil did not close normally last time. It had made safety copies of:~%~%~{    ~A~%~}~%~
                                         Open them now?~%~%Yes = open them.~%No = keep them, without asking again: open them any time with~%      File > Recover Safety Copies (they are deleted after 30 days)."
                                    (mapcar #'describe-copy new))
                            "yesno")))
        (if (string= r "yes")
            (dolist (c new) (open-safety-copy c))
            (dolist (c new) (mark-offered c)))))))

(defcmd "DRAWINGRECOVERY" ("RECOVERCOPIES") "Opens safety copies left by Fancy Pencil stopping unexpectedly."
  (let ((cs (leftover-safety-copies)))
    (if (null cs)
        (out (format nil "There are no safety copies to recover.~%"))
        (if (and (gui-p) (not (from-lisp-p)))
            (let ((r (gui-confirm (format nil "Safety copies:~%~%~{    ~A~%~}~%Open them? (No = delete them)"
                                          (mapcar #'describe-copy cs))
                                  "yesnocancel")))
              (cond ((string= r "yes") (dolist (c cs) (open-safety-copy c)))
                    ((string= r "no")
                     (dolist (c cs)
                       (ignore-errors (delete-file (car c)))
                       (ignore-errors (delete-file (make-pathname :type "info" :defaults (pathname (car c))))))
                     (out (format nil "Safety copies deleted.~%")))))
            (dolist (c cs) (open-safety-copy c))))))
