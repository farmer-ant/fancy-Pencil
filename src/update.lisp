;;;; update.lisp — File > Check for Updates... and File > Update from File...
;;;;
;;;; A copy of Fancy Pencil in a folder you can write to (like ~/Fancy Pencil/fancypencil) is updated
;;;; in place: the new version is downloaded from GitHub (or taken from a fancypencil-*.tar.gz you
;;;; have), checked, the old one is saved in ~/.cache/fancypencil/backups/, and the new files are
;;;; copied over it. The next start compiles what changed.
;;;; The installed edition (/opt/fancypencil, from Install Fancy Pencil.deb) can't be written to:
;;;; there a newer installer is downloaded from the GitHub releases and opened in the system's
;;;; software installer. The Snap Store edition (https://snapcraft.io/fancypencil) is kept up to date
;;;; by the Snap Store; File > Snap Store Page... opens that page.

(in-package :fancy-pencil)

(defparameter *update-repo* "farmer-ant/fancy-Pencil")
(defparameter *snap-store-url* "https://snapcraft.io/fancypencil"
  "Fancy Pencil's page in the Snap Store: the third way to get it (File > Snap Store Page...).")

(defun update-source ()
  "FANCYPENCIL_UPDATE_SOURCE: another place to update from (a folder, or a URL ending in /) holding
fancypencil.asd, main.tar.gz and release.json - for testing. Otherwise GitHub."
  (let ((s (getenv "FANCYPENCIL_UPDATE_SOURCE")))
    (and s (plusp (length s))
         (if (or (search "://" s)) s (format nil "file://~A" (namestring (truename s)))))))

(defun update-url (what)
  (let ((src (update-source)))
    (if src
        (format nil "~A~A" src (ecase what (:version "fancypencil.asd") (:tarball "main.tar.gz") (:release "release.json")))
        (ecase what
          (:version (format nil "https://raw.githubusercontent.com/~A/main/fancypencil.asd" *update-repo*))
          (:tarball (format nil "https://github.com/~A/archive/refs/heads/main.tar.gz" *update-repo*))
          (:release (format nil "https://api.github.com/repos/~A/releases/latest" *update-repo*))))))

;;; versions

(defun version-parts (s)
  "\"0.5.26\" (or \"v0.5.26\") -> (0 5 26); NIL if it isn't a version."
  (let ((s (string-left-trim "vV" (string-trim " " (or s "")))))
    (and (plusp (length s))
         (every (lambda (c) (or (digit-char-p c) (char= c #\.))) s)
         (mapcar #'parse-integer (remove "" (split-string s #\.) :test #'string=)))))

(defun version-newer-p (a b)
  "True when version string A is newer than B."
  (let ((x (version-parts a)) (y (version-parts b)))
    (loop for i from 0 below (max (length x) (length y))
          for p = (or (nth i x) 0) and q = (or (nth i y) 0)
          when (> p q) return t
          when (< p q) return nil)))

(defun asd-version (text)
  "The :version \"x.y.z\" in a fancypencil.asd."
  (let ((i (search ":version \"" text)))
    (and i (let* ((a (+ i 10)) (b (position #\" text :start a))) (and b (subseq text a b))))))

;;; where this copy lives

(defun program-root ()
  (let ((src (or *gui-dir* (script-directory))))
    (and src (probe-file (merge-pathnames "../" (pathname src))))))

(defun folder-writable-p (dir)
  (ignore-errors
   (let ((f (merge-pathnames ".fancypencil-write-test" dir)))
     (with-open-file (o f :direction :output :if-exists :supersede) (write-line "x" o))
     (delete-file f)
     t)))

(defun install-kind ()
  ":snap, :git (a git clone: use git pull), :folder (writable: updated in place) or :system."
  (let ((root (program-root)))
    (cond ((in-snap-p) :snap)
          ((null root) :system)
          ((probe-file (merge-pathnames ".git/" root)) :git)
          ((folder-writable-p root) :folder)
          (t :system))))

;;; downloading

(defun downloader ()
  (flet ((which (name)
           (multiple-value-bind (code text) (run-shell (format nil "command -v ~A" name))
             (and (eql code 0) (plusp (length (string-trim '(#\Newline #\Space) text)))))))
    (cond ((which "curl") :curl) ((which "wget") :wget))))

(defun download (url out)
  "Downloads URL to the file OUT. T, or (values NIL reason)."
  (let ((tool (downloader)))
    (unless tool
      (return-from download (values nil "Downloading needs curl or wget (sudo apt install curl).")))
    (ignore-errors (delete-file out))
    (multiple-value-bind (code text)
        (run-shell (if (eq tool :curl)
                       (format nil "curl -fsSL --connect-timeout 20 --max-time 600 -H 'User-Agent: FancyPencil' -o ~A ~A"
                               (shell-quote out) (shell-quote url))
                       (format nil "wget -q --timeout=20 -U FancyPencil -O ~A ~A" (shell-quote out) (shell-quote url))))
      (if (and (eql code 0) (probe-file out) (plusp (with-open-file (s out) (file-length s))))
          t
          (values nil (format nil "Couldn't download ~A~@[~%~A~]" url
                              (let ((l (last-lines text 3))) (and (plusp (length l)) l))))))))

(defun download-text (url)
  (let ((f (concatenate 'string (temp-dir "upd") "page")))
    (multiple-value-bind (ok why) (download url f)
      (if ok (read-text-file f) (values nil why)))))

;;; unpacking and installing

(defun unpack-update (tarball)
  "Unpacks a Fancy Pencil tarball into a new temporary folder and checks that it is one.
Returns (values FOLDER VERSION), or (values NIL reason)."
  (let ((dir (temp-dir "upd")))
    (multiple-value-bind (code text) (run-shell (format nil "tar xzf ~A -C ~A" (shell-quote tarball) (shell-quote dir)))
      (unless (eql code 0)
        (return-from unpack-update
          (values nil (format nil "~A isn't a .tar.gz file that can be unpacked.~%~A" (file-namestring tarball) (last-lines text 3))))))
    ;; one top folder (fancypencil/ in our tarballs, fancy-Pencil-main/ from GitHub)
    (let* ((tops (directory (concatenate 'string dir "*/")))
           (top (and (= (length tops) 1) (first tops))))
      (if (and top
               (probe-file (merge-pathnames "load.lisp" top))
               (probe-file (merge-pathnames "fancypencil.asd" top))
               (probe-file (merge-pathnames "src/main.lisp" top)))
          (values (namestring top) (asd-version (read-text-file (merge-pathnames "fancypencil.asd" top))))
          (values nil (format nil "~A isn't a Fancy Pencil tarball (no load.lisp, fancypencil.asd and src/ inside one folder)."
                              (file-namestring tarball)))))))

(defun backup-folder ()
  (let ((home (or (getenv "XDG_CACHE_HOME")
                  (concatenate 'string (or (getenv "HOME") "/tmp") "/.cache"))))
    (ensure-directories-exist (format nil "~A/fancypencil/backups/" home))))

(defun install-update (from root)
  "Saves ROOT (without its build/ folder) as a backup tarball, then copies the folder FROM over it.
T and the backup's name, or (values NIL reason)."
  (let ((backup (format nil "~Afancypencil-~A-~D.tar.gz" (namestring (backup-folder)) *version* (get-universal-time))))
    (multiple-value-bind (code text)
        (run-shell (format nil "tar czf ~A --exclude=./build -C ~A ." (shell-quote backup) (shell-quote (namestring root))))
      (unless (eql code 0)
        (return-from install-update (values nil (format nil "Couldn't save a backup of this copy first, so nothing was changed.~%~A"
                                                        (last-lines text 3))))))
    (multiple-value-bind (code text)
        (run-shell (format nil "cp -a ~A. ~A" (shell-quote from) (shell-quote (namestring root))))
      (if (eql code 0)
          (values t backup)
          (values nil (format nil "Copying the new files failed part way.~%~A~%The old version is saved in ~A~%~
To put it back: tar xzf ~A -C ~A" (last-lines text 3) backup (shell-quote backup) (shell-quote (namestring root))))))))

(defun ask-yes (msg &optional (prompt "Install it?"))
  "Yes/No question: a message box in the window, a prompt on the command line."
  (if (and (gui-p) (not (from-lisp-p)))
      (equal (gui-confirm msg) "yes")
      (progn (out (format nil "~A~%" msg))
             (equal (gkw (format nil "~A [Yes/No] <Yes>: " prompt) '("Yes" "No") "Yes") "Yes"))))

(defun update-with-tarball (tarball &key (where "this file"))
  "Unpacks and checks TARBALL, asks, and installs it over this copy."
  (multiple-value-bind (dir ver) (unpack-update tarball)
    (unless dir (tell-user ver) (return-from update-with-tarball nil))
    (let ((root (program-root))
          (q (cond ((null ver) (format nil "~A has Fancy Pencil of an unknown version." where))
                   ((version-newer-p ver *version*) (format nil "~A has Fancy Pencil ~A (you have ~A)." where ver *version*))
                   ((string= ver *version*) (format nil "~A has Fancy Pencil ~A, the version you have. Install it again?" where ver))
                   (t (format nil "~A has Fancy Pencil ~A, which is OLDER than yours (~A)." where ver *version*)))))
      (if (not (ask-yes (format nil "~A~%~%It goes into ~A (the old version is saved first).~%~%Install it?"
                                q (namestring root))))
          (progn (out (format nil "Nothing was changed.~%")) nil)
          (multiple-value-bind (ok info)
              (progn (out (format nil "Saving the old version and putting in the new one (a few seconds)...~%"))
                     (when (gui-p) (ignore-errors (tk-ask "update idletasks; return {}")))
                     (install-update dir root))
            (if ok
                (progn
                  (tell-user (format nil "Fancy Pencil ~A is installed.~%~%Close Fancy Pencil and start it again to use it ~
(the first start takes a little longer while it gets ready).~%~%The old version is saved in~%~A" (or ver "") info))
                  t)
                (progn (tell-user info) nil)))))))

(defun open-snap-store ()
  "Opens Fancy Pencil's Snap Store page in the web browser."
  (run-shell (format nil "(xdg-open ~A >/dev/null 2>&1 &)" (shell-quote *snap-store-url*))))

(defun snap-store-line ()
  (format nil "Fancy Pencil is also in the Snap Store: ~A~%(File > Snap Store Page...)" *snap-store-url*))

(defun kind-note (kind)
  (case kind
    (:snap (format nil "This copy comes from the Snap Store (~A),~%which updates it by itself.~%~
To update now, in a terminal: snap refresh fancypencil" *snap-store-url*))
    (:git (format nil "This copy is a git clone (~A): update it with~%    git pull~%and start Fancy Pencil again."
                  (namestring (program-root))))
    (t nil)))

;;; the system-wide edition: a newer installer from the GitHub releases

(defun json-string-after (text key start)
  "The string value of \"KEY\": \"...\" at or after START in a JSON TEXT, and where it ends."
  (let ((i (search (format nil "\"~A\"" key) text :start2 start)))
    (when i
      (let* ((a (position #\" text :start (1+ (position #\: text :start i))))
             (b (and a (position #\" text :start (1+ a)))))
        (and b (values (subseq text (1+ a) b) b))))))

(defun release-deb ()
  "(values VERSION DEB-URL DEB-NAME) of the latest GitHub release, or (values NIL reason)."
  (multiple-value-bind (json why) (download-text (update-url :release))
    (unless json (return-from release-deb (values nil why)))
    (let ((tag (json-string-after json "tag_name" 0)))
      (loop with pos = 0
            do (multiple-value-bind (url end) (json-string-after json "browser_download_url" pos)
                 (unless url (return (values tag nil nil)))
                 (when (string-equal ".deb" (subseq url (max 0 (- (length url) 4))))
                   (return (values tag url (let ((s (subseq url (1+ (position #\/ url :from-end t)))))
                                             (substitute #\Space #\+ (replace-all s "%20" " "))))))
                 (setf pos end))))))

(defun update-system-copy ()
  (multiple-value-bind (tag url name) (release-deb)
    (cond ((null tag)
           (tell-user (format nil "Couldn't reach GitHub to look for a newer installer.~%~A" (or url ""))))
          ((or (null url) (not (version-newer-p tag *version*)))
           (tell-user (format nil "You have Fancy Pencil ~A. The newest installer on GitHub is ~A~:[~; (with no .deb)~].~%~%~
This copy is installed for the whole computer (~A), so it is updated with a new~%~
Install Fancy Pencil.deb. A copy in a folder of your own (like ~~/Fancy Pencil/fancypencil)~%~
can update itself from GitHub with File > Check for Updates.~%~%~A"
                              *version* tag (null url) (namestring (program-root)) (snap-store-line))))
          ((ask-yes (format nil "Fancy Pencil ~A is available (you have ~A).~%~%Download the installer and open it?" tag *version*))
           (let ((out (format nil "~A/Downloads/~A" (or (getenv "HOME") "/tmp") (or name "Install Fancy Pencil.deb"))))
             (ensure-directories-exist out)
             (multiple-value-bind (ok why) (download url out)
               (if (not ok)
                   (tell-user why)
                   (progn
                     (run-shell (format nil "(xdg-open ~A >/dev/null 2>&1 &)" (shell-quote out)))
                     (tell-user (format nil "The installer is in ~A and is opening in your software installer.~%~
Install it there (or in a terminal: sudo apt install ~A),~%then close Fancy Pencil and start it again."
                                        out (shell-quote out)))))))))))

;;; the commands

(defcmd "UPDATE" ("CHECKUPDATES" "UPDATECHECK")
    "Checks GitHub for a newer Fancy Pencil and installs it (File > Check for Updates...)."
  (let ((kind (install-kind)))
    (cond ((eq kind :snap)
           (when (ask-yes (format nil "~A~%~%Open its Snap Store page?" (kind-note kind)))
             (open-snap-store)))
          ((kind-note kind) (tell-user (kind-note kind)))
          ((eq kind :system) (update-system-copy))
          (t
           (out (format nil "Looking for a newer Fancy Pencil on GitHub...~%"))
           (multiple-value-bind (asd why) (download-text (update-url :version))
             (let ((ver (and asd (asd-version asd))))
               (cond ((null ver)
                      (tell-user (format nil "Couldn't find out the newest version on GitHub.~%~A~%~%~
Check the internet connection and try again, or use File > Update from File~%with a fancypencil-*.tar.gz you have.~%~%~A"
                                         (or why "(fancypencil.asd had no version)") (snap-store-line))))
                     ((not (version-newer-p ver *version*))
                      (tell-user (format nil "You have the newest Fancy Pencil: ~A.~@[~%(GitHub has ~A.)~]"
                                         *version* (and (string/= ver *version*) ver))))
                     ((ask-yes (format nil "Fancy Pencil ~A is available on GitHub (you have ~A).~%~%Download it?" ver *version*))
                      (let ((tgz (concatenate 'string (temp-dir "upd") "main.tar.gz")))
                        (out (format nil "Downloading Fancy Pencil ~A...~%" ver))
                        (multiple-value-bind (ok why) (download (update-url :tarball) tgz)
                          (if ok
                              (update-with-tarball tgz :where "The download")
                              (tell-user why)))))
                     (t (out (format nil "Nothing was changed.~%"))))))))))

(defcmd "SNAPSTORE" ("SNAPPAGE")
    "Opens Fancy Pencil's page in the Snap Store (https://snapcraft.io/fancypencil) in the web browser (File > Snap Store Page...)."
  (open-snap-store)
  (out (format nil "Opening ~A~%~
To install the Snap Store edition:  sudo snap install fancypencil~%~
To update it:                       snap refresh fancypencil~%~
(The Snap Store edition opens and saves DXF; for .dwg files use the Install Fancy Pencil.deb edition.)~%"
               *snap-store-url*)))

(defcmd "UPDATEFILE" ("UPDATEFROMFILE")
    "Updates Fancy Pencil from a fancypencil-*.tar.gz (or an Install Fancy Pencil.deb) you have (File > Update from File...)."
  (let ((kind (install-kind)))
    (when (kind-note kind) (tell-user (kind-note kind)) (return-from command-body nil))
    (let* ((home (or (getenv "HOME") "/tmp"))
           (dl (format nil "~A/Downloads/fancy pencil project/" home))
           (f (ask-file nil "Update Fancy Pencil from" (if (probe-file dl) dl (format nil "~A/Downloads/" home))
                        (if (eq kind :system) "deb gz" "gz deb"))))
      (cond ((or (null f) (string= f "")) nil)
            ((not (probe-file f)) (tell-user (format nil "There is no file ~A" f)))
            ((string-equal (or (pathname-type (pathname f)) "") "deb")
             (run-shell (format nil "(xdg-open ~A >/dev/null 2>&1 &)" (shell-quote f)))
             (tell-user (format nil "~A is opening in your software installer.~%Install it there, then start Fancy Pencil again."
                                (file-namestring f))))
            ((eq kind :system)
             (tell-user (format nil "This copy is installed for the whole computer (~A), so a tarball can't go into it.~%~
Use an Install Fancy Pencil.deb instead, or unpack the tarball into a folder of your own." (namestring (program-root)))))
            (t (update-with-tarball (namestring (probe-file f)) :where (file-namestring f)))))))
