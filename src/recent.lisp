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

;;; ---------- each drawing's last view ----------
;;; When a drawing is closed (its tab, or Fancy Pencil itself), where it was zoomed and panned,
;;; which tab (Model or a layout) showed and any 3D view are kept in ~/.config/fancypencil/views.lisp,
;;; and put back the next time that file opens. Kept outside the drawing, so it works for DWG files,
;;; read-only folders and drawings closed without saving.

(defparameter *views-max* 300)

(defun views-path ()
  (merge-pathnames ".config/fancypencil/views.lisp"
                   (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/")))

(defun load-views ()
  (ignore-errors
   (with-open-file (i (views-path) :external-format :utf-8)
     (with-standard-io-syntax
       (let ((*read-eval* nil) (*read-default-float-format* 'double-float) (x (read i nil nil)))
         (and (listp x) (remove-if-not (lambda (e) (and (consp e) (stringp (car e)))) x)))))))

(defun save-views (views)
  (ignore-errors
   (let ((p (views-path)))
     (ensure-directories-exist p)
     (with-open-file (o p :direction :output :if-exists :supersede :external-format :utf-8)
       (with-standard-io-syntax
         (let ((*print-readably* nil) (*read-default-float-format* 'double-float))
           (format o ";; Fancy Pencil: each drawing's last view (written by the program; safe to delete)~%")
           (prin1 (mapcar (lambda (e) (cons (coerce (car e) '(simple-array character (*))) (cdr e))) views) o)
           (terpri o)))))))

(defun view-key (f) (or (ignore-errors (namestring (truename f))) (and f (namestring f))))

(defun remember-drawing-view ()
  "Keep the current drawing's view for the next time its file is opened."
  (let* ((f (drawing-filename *dwg*)) (key (and f (probe-file f) (view-key f))))
    (when key
      (ignore-errors (funcall 'leave-space))          ; the tab showing records its view
      (let* ((ctab (drawing-ctab *dwg*))
             (l (and ctab (ignore-errors (funcall 'find-layout ctab))))
             (entry (list key :ctab ctab :model (drawing-model-view *dwg*)
                          :pview (and l (layout-pview l)))))
        (let ((others (remove key (load-views) :key #'car :test #'string=)))
          (save-views (cons entry (subseq others 0 (min (1- *views-max*) (length others))))))))))

(defun remember-doc-view (d)
  "REMEMBER-DRAWING-VIEW for open drawing D, current or not."
  (if (eq d (current-doc))
      (remember-drawing-view)
      (let ((*dwg* (doc-dwg d)) (*sysvars* (doc-sysvars d))
            (*vx* (doc-vx d)) (*vy* (doc-vy d)) (*vscale* (doc-vscale d)) (*view* (doc-view d)))
        (remember-drawing-view))))

(defun restore-drawing-view (f)
  "Put back the view F had when it was last closed. Returns T if there was one."
  (let ((e (cdr (assoc (view-key f) (load-views) :test #'equal))))
    (when e
      (ignore-errors
       (let* ((model (getf e :model)) (ctab (getf e :ctab))
              (l (and ctab (funcall 'find-layout ctab))))
         (when (and model (= (length model) 5) (every #'realp (subseq model 1 4)) (plusp (fourth model)))
           (setf (drawing-model-view *dwg*) model))
         (cond (l
                (let ((pv (getf e :pview)))
                  (when (and pv (= (length pv) 3) (every #'realp pv) (plusp (third pv)))
                    (setf (layout-pview l) pv)))
                (funcall 'set-ctab ctab))
               ((drawing-model-view *dwg*)
                (destructuring-bind (view vx vy vs style) (drawing-model-view *dwg*)
                  (setf *view* view *vx* (d vx) *vy* (d vy) *vscale* (d vs)
                        (sysvar "VSCURRENT") (or style "Wireframe"))))
               (t (return-from restore-drawing-view nil)))
         (gui-regen)
         t)))))
