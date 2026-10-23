;;;; prefs.lisp — the cursor and selection settings (OPTIONS, Tools > Cursor & Selection) and the
;;;; user preferences file ~/.config/fancypencil/settings.lisp, so the settings stay the way you
;;;; left them the next time Fancy Pencil starts (and in every drawing tab).

(in-package :fancy-pencil)

(defparameter *pref-sysvars*
  '("CURSORSIZE" "CURSORCOLOR" "PICKBOX" "APERTURE" "AUTOSNAPSIZE" "GRIPSIZE" "ROLLOVERTIPS"
    "OSMODE")
  "Settings kept in the preferences file.")

;; they belong to the program, not to one drawing: carry them between tabs
(setf *app-sysvars* (union *app-sysvars* *pref-sysvars* :test #'string-equal))

(defun prefs-path ()
  (merge-pathnames ".config/fancypencil/settings.lisp"
                   (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/")))

(defun save-prefs ()
  (ignore-errors
   (let ((p (prefs-path)))
     (ensure-directories-exist p)
     (with-open-file (o p :direction :output :if-exists :supersede :external-format :utf-8)
       (with-standard-io-syntax
         (format o ";; Fancy Pencil settings (written by the program; safe to delete)~%")
         (prin1 (loop for n in *pref-sysvars*
                      for (v found) = (multiple-value-list (gethash n *sysvars*))
                      when (and found (or (integerp v) (stringp v))) collect (cons n v))
                o)
         (terpri o)))
     t)))

(defun load-prefs ()
  (ignore-errors
   (with-open-file (i (prefs-path) :external-format :utf-8)
     (with-standard-io-syntax
       (let ((*read-eval* nil))
         (dolist (pair (read i nil nil))
           (when (and (consp pair) (member (car pair) *pref-sysvars* :test #'string-equal)
                      (or (integerp (cdr pair)) (stringp (cdr pair))))
             (setf (sysvar (string-upcase (car pair))) (cdr pair)))))))))

(defun cursor-color ()
  (let ((c (sysvar "CURSORCOLOR")))
    (if (and (stringp c) (= (length c) 7) (char= (char c 0) #\#)) c "#c9ced6")))

(defparameter *cursor-colors*
  '(("Light gray" "#c9ced6") ("White" "#ffffff") ("Yellow" "#ffe14d") ("Green" "#5fd35f")
    ("Cyan" "#5fd3e0") ("Red" "#ff5f5f") ("Magenta" "#ff6fd8") ("Black" "#000000")))

(defun cursor-dialog ()
  (let ((r (tk-ask (format nil "cursordlg ~D ~A ~D ~D ~D ~D ~D ~A"
                           (or (sysvar "CURSORSIZE") 100) (tq (cursor-color))
                           (or (sysvar "PICKBOX") 5) (or (sysvar "APERTURE") 10)
                           (or (sysvar "AUTOSNAPSIZE") 6) (or (sysvar "GRIPSIZE") 5)
                           (if (eql (sysvar "ROLLOVERTIPS") 0) 0 1)
                           (format nil "{~{{~A ~A}~^ ~}}" (loop for (n c) in *cursor-colors* append (list (tq n) c)))))))
    (when (and (> (length r) 3) (string= (subseq r 0 3) "OK "))
      (destructuring-bind (cs col pb ap sn gs rt) (split-string (subseq r 3) #\Space)
        (flet ((int (s lo hi) (let ((v (ignore-errors (parse-integer s)))) (and v (max lo (min hi v))))))
          (setf (sysvar "CURSORSIZE") (or (int cs 1 100) 100)
                (sysvar "CURSORCOLOR") (if (and (= (length col) 7) (char= (char col 0) #\#)) col "#c9ced6")
                (sysvar "PICKBOX") (or (int pb 0 50) 5)
                (sysvar "APERTURE") (or (int ap 1 50) 10)
                (sysvar "AUTOSNAPSIZE") (or (int sn 1 20) 6)
                (sysvar "GRIPSIZE") (or (int gs 1 20) 5)
                (sysvar "ROLLOVERTIPS") (or (int rt 0 1) 1))))
      (save-prefs)
      (refresh-grips)
      (draw-temp)
      (out (format nil "Cursor settings saved.~%")))))

(defcmd "OPTIONS" ("OP" "CURSOR" "DDSELECT") "Cursor and selection settings: crosshair size and color, pickbox, snap marker, grips, rollover tips."
  (if (and (gui-p) (not (from-lisp-p)))
      (cursor-dialog)
      (progn
        (let ((v (gint (format nil "Crosshair size, percent of the screen (1-100, 100 = full screen) <~D>: " (or (sysvar "CURSORSIZE") 100)))))
          (when (and v (<= 1 v 100)) (setf (sysvar "CURSORSIZE") v)))
        (let ((v (gint (format nil "Pickbox size in pixels (0-50) <~D>: " (or (sysvar "PICKBOX") 5)))))
          (when (and v (<= 0 v 50)) (setf (sysvar "PICKBOX") v)))
        (let ((v (gint (format nil "Snap target (aperture) size in pixels (1-50) <~D>: " (or (sysvar "APERTURE") 10)))))
          (when (and v (<= 1 v 50)) (setf (sysvar "APERTURE") v)))
        (save-prefs))))
