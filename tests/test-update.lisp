;; File > Check for Updates / Update from File (0.5.27), with a local folder standing in for GitHub.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-update.lisp
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
(defun sh (fmt &rest args) (run-shell (apply #'format nil fmt args)))
(defun file-text (f) (and (probe-file f) (read-text-file f)))

;; versions
(chk "newer" (list (version-newer-p "0.5.27" "0.5.26") (version-newer-p "0.6" "0.5.99") (version-newer-p "v1.0.0" "0.9"))
     '(t t t))
(chk "not newer" (list (version-newer-p "0.5.26" "0.5.26") (version-newer-p "0.5.9" "0.5.10") (version-newer-p "junk" "0.1"))
     '(nil nil nil))
(chk "the version in fancypencil.asd" (asd-version "(defsystem \"fancypencil\" :version \"0.5.27\" :depends-on ())") "0.5.27")

;; a pretend copy of Fancy Pencil, and a pretend GitHub with a newer one
(defparameter *t* "/tmp/fp-upd/")
(sh "rm -rf ~A; mkdir -p ~Aapp/src ~Aapp/build ~Asrc ~Anew/fancy-Pencil-main/src ~Acache" *t* *t* *t* *t* *t* *t*)
(defun make-copy (dir version &rest extra)
  (with-open-file (o (format nil "~Afancypencil.asd" dir) :direction :output :if-exists :supersede)
    (format o "(defsystem \"fancypencil\" :version ~S)~%" version))
  (dolist (f (list* "load.lisp" "src/main.lisp" extra))
    (with-open-file (o (format nil "~A~A" dir f) :direction :output :if-exists :supersede) (format o "~A ~A~%" f version))))
(make-copy (format nil "~Aapp/" *t*) *version* "old-only.txt" "build/x-SBCL.fasl")
(make-copy (format nil "~Anew/fancy-Pencil-main/" *t*) "9.9.9" "new-file.txt")
(sh "tar czf ~Asrc/main.tar.gz -C ~Anew fancy-Pencil-main" *t* *t*)
(sh "cp ~Anew/fancy-Pencil-main/fancypencil.asd ~Asrc/" *t* *t*)
(sb-posix:setenv "FANCYPENCIL_UPDATE_SOURCE" (format nil "~Asrc/" *t*) 1)
(sb-posix:setenv "XDG_CACHE_HOME" (format nil "~Acache" *t*) 1)
(setf *gui-dir* (pathname (format nil "~Aapp/src/" *t*)))

(chk "this copy: a folder of its own" (install-kind) :folder)
(chk "  ...where it is" (namestring (program-root)) (format nil "~Aapp/" *t*))

;; Check for Updates: say no at the first question - nothing changes
(setf *said* nil)
(al "(command \"update\" \"no\")")
(chk "found the newer version" (said "Fancy Pencil 9.9.9 is available on GitHub") t)
(chk "  ...no: nothing changed" (asd-version (file-text (format nil "~Aapp/fancypencil.asd" *t*))) *version*)

;; yes, yes: downloaded, checked, the old one saved, the new one in place
(setf *said* nil)
(al "(command \"update\" \"yes\" \"yes\")")
(chk "installed" (said "Fancy Pencil 9.9.9 is installed") t)
(chk "  ...its version" (asd-version (file-text (format nil "~Aapp/fancypencil.asd" *t*))) "9.9.9")
(chk "  ...a new file came" (and (probe-file (format nil "~Aapp/new-file.txt" *t*)) t) t)
(chk "  ...a file it doesn't have stays" (and (probe-file (format nil "~Aapp/old-only.txt" *t*)) t) t)
(chk "  ...build/ left for the next start" (and (probe-file (format nil "~Aapp/build/x-SBCL.fasl" *t*)) t) t)
(let ((b (directory (format nil "~Acache/fancypencil/backups/*.tar.gz" *t*))))
  (chk "  ...the old version saved" (length b) 1)
  (multiple-value-bind (code text) (run-shell (format nil "tar tzf ~A" (shell-quote (namestring (first b)))))
    (declare (ignore code))
    (chk "  ...with its files, without build/" (list (and (search "./fancypencil.asd" text) t) (and (search "build/" text) t)) '(t nil))))

;; already the newest
(sh "cp ~Aapp/fancypencil.asd ~Asrc/fancypencil.asd" *t* *t*)
(defvar *real-version* *version*)
(setf *version* "9.9.9")                  ; (commands run in a thread of their own: no LET)
(setf *said* nil)
(al "(command \"update\")")
(chk "the newest already" (said "You have the newest Fancy Pencil: 9.9.9") t)

;; Update from File: our own release tarball (top folder fancypencil/), older than this copy
(sh "mkdir -p ~Aold/fancypencil/src" *t*)
(make-copy (format nil "~Aold/fancypencil/" *t*) "0.0.1")
(sh "tar czf ~Afancypencil-0.0.1.tar.gz -C ~Aold fancypencil" *t* *t*)
(setf *said* nil)
(al (format nil "(command \"updatefile\" \"~Afancypencil-0.0.1.tar.gz\" \"no\")" *t*))
(chk "older: said so" (said "which is OLDER than yours") t)
(chk "  ...no: unchanged" (asd-version (file-text (format nil "~Aapp/fancypencil.asd" *t*))) "9.9.9")
(setf *version* *real-version*)
;; not a Fancy Pencil tarball
(sh "mkdir -p ~Ajunk/stuff && echo hi > ~Ajunk/stuff/a.txt && tar czf ~Ajunk.tar.gz -C ~Ajunk stuff" *t* *t* *t* *t*)
(setf *said* nil)
(al (format nil "(command \"updatefile\" \"~Ajunk.tar.gz\")" *t*))
(chk "not Fancy Pencil: refused" (said "isn't a Fancy Pencil tarball") t)
(chk "  ...unchanged" (asd-version (file-text (format nil "~Aapp/fancypencil.asd" *t*))) "9.9.9")

;; a git clone is left to git
(sh "mkdir -p ~Aapp/.git" *t*)
(chk "a git clone" (install-kind) :git)
(setf *said* nil) (al "(command \"update\")")
(chk "  ...told to use git pull" (said "git pull") t)
(sh "rmdir ~Aapp/.git" *t*)

;; the installed edition: a newer installer from the GitHub release
(with-open-file (o (format nil "~Asrc/release.json" *t*) :direction :output :if-exists :supersede)
  (format o "{\"tag_name\": \"v0.6.0\", \"assets\": [{\"name\": \"fancypencil-0.6.0.tar.gz\", ~
\"browser_download_url\": \"https://github.com/x/releases/download/v0.6.0/fancypencil-0.6.0.tar.gz\"}, ~
{\"name\": \"Install.Fancy.Pencil.deb\", \"browser_download_url\": \"https://github.com/x/releases/download/v0.6.0/Install%20Fancy%20Pencil.deb\"}]}"))
(chk "latest release and its installer" (multiple-value-list (release-deb))
     '("v0.6.0" "https://github.com/x/releases/download/v0.6.0/Install%20Fancy%20Pencil.deb" "Install Fancy Pencil.deb"))

;; the Snap Store: its page, and the note for a copy from it
(setf *said* nil) (al "(command \"snapstore\")")
(chk "Snap Store page" (said "Opening https://snapcraft.io/fancypencil") t)
(chk "  ...how to install and update it" (list (said "sudo snap install fancypencil") (said "snap refresh fancypencil")) '(t t))
(chk "the Snap copy is pointed at its page" (and (search "https://snapcraft.io/fancypencil" (kind-note :snap)) t) t)

(sb-posix:unsetenv "FANCYPENCIL_UPDATE_SOURCE")
(chk "GitHub by default" (list (update-url :version) (update-url :tarball))
     '("https://raw.githubusercontent.com/farmer-ant/fancy-Pencil/main/fancypencil.asd"
       "https://github.com/farmer-ant/fancy-Pencil/archive/refs/heads/main.tar.gz"))
(format t "~%Update tests: ~D passed, ~D failed.~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
