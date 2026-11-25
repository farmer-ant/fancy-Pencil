;;;; mysettings.lisp — File > Save My Settings... and File > Load My Settings...
;;;;
;;;; Your own Fancy Pencil - the options, colors, command aliases (.pgp), toolbars and ribbon
;;;; layout, plot settings, named views, spelling words, fonts you added and your start-up
;;;; Lisp file (~/.fancypencil.lsp) - packed into one file, to carry to another computer (or keep
;;;; as a backup) and load there. The file is an ordinary .tar.gz of those files with their paths
;;;; under the home folder, so `tar xzf FILE -C ~` puts it in as well, and a Fancy Pencil
;;;; Workstation set up with one beside setup-workstation.sh starts with them.
;;;;
;;;; Not carried: memory.conf (each computer works out its own memory), the recent-files list
;;;; (its paths are the other computer's) and the safety copies and logs in ~/.local/share.

(in-package :fancy-pencil)

(defparameter *settings-skip* '("memory.conf" "recent.lisp")
  "Files in ~/.config/fancypencil that belong to one computer, so aren't carried.")

(defun home-dir ()
  (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/"))

(defun settings-dir () (concatenate 'string (home-dir) ".config/fancypencil/"))

(defun host-name ()
  (let ((h (or (ignore-errors (machine-instance)) "")))
    (if (plusp (length h)) (substitute #\- #\Space h) "computer")))

(defun today-string ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time))
    (declare (ignore s mi h))
    (format nil "~4,'0D-~2,'0D-~2,'0D" y mo d)))

(defun settings-file-name ()
  (format nil "Fancy Pencil Settings ~A ~A.tar.gz" (host-name) (today-string)))

(defun settings-members ()
  "The files to carry, relative to the home folder (\".config/fancypencil/x\", \".fancypencil.lsp\")."
  (let ((dir (settings-dir)) (out nil))
    (when (probe-file dir)
      (multiple-value-bind (code text)
          (run-shell (format nil "cd ~A && find . -type f | LC_ALL=C sort" (shell-quote dir)))
        (when (eql code 0)
          (dolist (line (split-string text #\Newline))
            (let ((rel (string-left-trim "./" (string-right-trim '(#\Return) line))))
              (when (and (plusp (length rel))
                         (not (member rel *settings-skip* :test #'string=))
                         (not (search ".fasl" rel)))
                (push (concatenate 'string ".config/fancypencil/" rel) out)))))))
    (when (probe-file (concatenate 'string (home-dir) ".fancypencil.lsp"))
      (push ".fancypencil.lsp" out))
    (sort out #'string<)))

(defun describe-member (m)
  (let ((f (file-namestring m)))
    (cond ((string= m ".fancypencil.lsp") "start-up Lisp file (~/.fancypencil.lsp)")
          ((search "/fonts/" m) nil)
          ((string= f "settings.lisp") "options and colors")
          ((string= f "fancypencil.pgp") "command aliases")
          ((string= f "toolbars.tcl") "toolbars")
          ((string= f "screen.tcl") "ribbon and window layout")
          ((string= f "plot.lisp") "plot settings")
          ((string= f "views.lisp") "named views")
          ((string= f "custom.dic") "your spelling words")
          (t f))))

(defun settings-summary (members)
  (let ((fonts (count-if (lambda (m) (search "/fonts/" m)) members))
        (names (remove-duplicates (remove nil (mapcar #'describe-member members)) :test #'string= :from-end t)))
    (format nil "~{~A~^, ~}~[~:;, ~:*~D font file~:P~]" names fonts)))

(defun write-settings-archive (out)
  "Packs the settings into OUT (.tar.gz). Returns (values t members) or (values nil message)."
  (save-prefs)
  (when (gui-p) (ignore-errors (tk-ask "catch tbx_save; catch scr_save; return {}")))
  (let ((members (settings-members)))
    (if (null members)
        (values nil "There are no settings of your own to save yet: Fancy Pencil is as it came.")
        (let ((list (format nil "/tmp/fancypencil-settings-~D.txt" (random 100000000 (make-random-state t)))))
          (with-open-file (o list :direction :output :if-exists :supersede :external-format :utf-8)
            (dolist (m members) (write-line m o)))
          (ensure-directories-exist out)
          (multiple-value-bind (code text)
              (run-shell (format nil "tar czf ~A -C ~A -T ~A" (shell-quote out) (shell-quote (home-dir)) (shell-quote list)))
            (ignore-errors (delete-file list))
            (if (eql code 0)
                (values t members)
                (values nil (format nil "Couldn't write ~A~%~A" out (last-lines text 3)))))))))

(defun settings-member-ok-p (m)
  "Only these may come out of a settings file: nothing outside ~/.config/fancypencil but the start-up file."
  (and (plusp (length m))
       (not (search ".." m))
       (char/= (char m 0) #\/)
       (or (string= m ".fancypencil.lsp")
           (and (> (length m) 20) (string= ".config/fancypencil/" m :end2 20)
                (not (member (subseq m 20) *settings-skip* :test #'string=))))))

(defun read-settings-archive (file)
  "Checks FILE and unpacks it into a fresh folder. Returns (values dir members) or (values nil message)."
  (multiple-value-bind (code text) (run-shell (format nil "tar tzf ~A" (shell-quote file)))
    (unless (eql code 0)
      (return-from read-settings-archive
        (values nil (format nil "~A isn't a Fancy Pencil settings file (it can't be read as a .tar.gz)." (file-namestring file)))))
    ;; RAW: the names as stored (to ask tar for), GOOD: the same without a leading ./ (to check)
    (let ((raw nil) (good nil))
      (dolist (l (split-string text #\Newline))
        (let* ((r (string-right-trim '(#\Return) l))
               (n (if (and (> (length r) 2) (string= "./" r :end2 2)) (subseq r 2) r)))
          (when (and (plusp (length n)) (char/= (char n (1- (length n))) #\/)   ; folders come by themselves
                     (settings-member-ok-p n))
            (push r raw) (push n good))))
      (when (null good)
        (return-from read-settings-archive
          (values nil (format nil "~A has no Fancy Pencil settings in it." (file-namestring file)))))
      (let* ((dir (format nil "/tmp/fancypencil-settings-in-~D/" (random 100000000 (make-random-state t))))
             (list (concatenate 'string (string-right-trim "/" dir) ".txt")))
        (ensure-directories-exist dir)
        (with-open-file (o list :direction :output :if-exists :supersede :external-format :utf-8)
          (dolist (m raw) (write-line m o)))
        ;; only those names come out, whatever else is in the file
        (multiple-value-bind (c2 t2)
            (run-shell (format nil "tar xzf ~A -C ~A --no-same-owner --no-same-permissions -T ~A 2>&1"
                               (shell-quote file) (shell-quote dir) (shell-quote list)))
          (ignore-errors (delete-file list))
          (if (eql c2 0)
              (values dir (sort good #'string<))
              (values nil (format nil "Couldn't unpack ~A~%~A" (file-namestring file) (last-lines t2 3)))))))))

(defun backup-settings ()
  "Saves the settings there are now in ~/.cache/fancypencil/backups/. Returns its name, or NIL if none."
  (let ((members (settings-members)))
    (when members
      (let ((out (format nil "~A.cache/fancypencil/backups/settings-before-load-~A-~D.tar.gz"
                         (home-dir) (today-string) (get-universal-time))))
        (and (write-settings-archive out) out)))))

(defvar *settings-loaded* nil "True once settings were loaded from a file in this run.")

(defun install-settings (dir members)
  "Copies the unpacked MEMBERS from DIR into the home folder, and applies what can be applied now."
  (dolist (m members)
    (let ((from (concatenate 'string dir m)) (to (concatenate 'string (home-dir) m)))
      (when (probe-file from)
        (ensure-directories-exist to)
        (run-shell (format nil "cp -f ~A ~A" (shell-quote from) (shell-quote to))))))
  (run-shell (format nil "rm -rf ~A" (shell-quote (string-right-trim "/" dir))))
  (setf *settings-loaded* t)
  ;; the running program mustn't write its old layout over the new files
  (when (gui-p) (ignore-errors (tk-ask "set ::settings_frozen 1; return {}")))
  (ignore-errors (load-prefs))
  (ignore-errors (load-aliases))
  (ignore-errors (when (gui-p) (apply-background))))

(defun default-settings-folder ()
  (let ((dl (concatenate 'string (home-dir) "Downloads/")))
    (if (probe-file dl) dl (home-dir))))

(defcmd "EXPORTSETTINGS" ("SAVESETTINGS" "SETTINGSOUT")
    "Saves your own settings (options, aliases, toolbars, ribbon, plot settings, fonts, ~/.fancypencil.lsp) in one file, to take to another computer (File > Save My Settings...)."
  (let ((f (ask-file t "Save My Settings As" (concatenate 'string (default-settings-folder) (settings-file-name)) "gz")))
    (when (or (null f) (string= f "")) (return-from command-body nil))
    (unless (search ".tar.gz" f) (setf f (concatenate 'string (string-right-trim "." (subseq f 0 (or (search ".gz" f) (length f)))) ".tar.gz")))
    (multiple-value-bind (ok info) (write-settings-archive f)
      (if ok
          (tell-user (format nil "Your settings are saved in~%~A~%~%They are: ~A.~%~%~
On another computer: File > Load My Settings... and choose this file.~%~
For a new Fancy Pencil Workstation: put it beside setup-workstation.sh." f (settings-summary info)))
          (tell-user info)))))

(defcmd "IMPORTSETTINGS" ("LOADSETTINGS" "SETTINGSIN")
    "Loads settings saved with Save My Settings (from this or another computer) (File > Load My Settings...)."
  (let ((f (ask-file nil "Load My Settings From" (default-settings-folder) "gz")))
    (cond ((or (null f) (string= f "")) nil)
          ((not (probe-file f)) (tell-user (format nil "There is no file ~A" f)))
          (t
           (multiple-value-bind (dir members) (read-settings-archive (namestring (probe-file f)))
             (if (null dir)
                 (tell-user members)
                 (if (not (ask-yes (format nil "~A has: ~A.~%~%They replace the same settings here ~
(yours are saved first, in ~~/.cache/fancypencil/backups).~%~%Load them?"
                                           (file-namestring f) (settings-summary members))
                                   "Load them?"))
                     (progn (run-shell (format nil "rm -rf ~A" (shell-quote (string-right-trim "/" dir))))
                            (out (format nil "Nothing was changed.~%")))
                     (let ((backup (backup-settings)))
                       (install-settings dir members)
                       (tell-user (format nil "Your settings are loaded.~%~%Close Fancy Pencil and start it again to see all of them ~
(toolbars, ribbon and fonts are set up at start).~@[~%~%The settings you had are saved in~%~A~]" backup))))))))))
