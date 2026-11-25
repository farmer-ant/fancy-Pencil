;;;; load.lisp — compile (SBCL) and load Fancy Pencil without needing ASDF/Quicklisp.
;;;;   sbcl --load load.lisp --eval '(fancy-pencil:main)'

(defpackage :fancy-pencil-loader (:use :cl))
(in-package :fancy-pencil-loader)

(defparameter *root* (make-pathname :name nil :type nil :defaults *load-truename*))
(defparameter *files* '("package" "geom" "db" "plane" "mesh" "vector-font" "ttf" "text3d" "textstyle" "ltype" "hatch" "dxf" "dxf2000" "alisp" "input" "gui" "grips"
                         "commands" "dims" "leader" "mleader" "modify" "modify2" "cmds2" "utilcmds" "dwg" "props" "express" "textcmds" "docs" "uicmds" "clip" "matchprop" "hover" "prefs" "memory" "recent" "blockedit" "views" "brep" "solids" "csg" "visual" "spell" "plot" "layouts" "massprop" "draworder" "tiles" "snap3d" "raster" "history" "helix" "moreobjs" "topo" "usgs" "print3d" "imgview" "surfaces" "acis" "zip" "dwf" "dgn" "underlay" "calc" "spline" "richtext" "navigate" "polyhedra" "struts" "strays" "brepcmds" "autosave" "update" "mysettings" "main"))

;; A compiled file is used only if it was built from exactly this source file: each one has a
;; stamp recording its source's date and size. (Comparing dates alone fails after an update,
;; because unpacked files carry their release date, which can be older than the old build.)
;; Once one file is recompiled, every file after it is too: they may use its structures and macros.
(defun source-stamp (src)
  (format nil "~D ~D" (file-write-date src) (with-open-file (i src :element-type '(unsigned-byte 8)) (file-length i))))

(defun stamp-matches-p (stamp-file src)
  (and (probe-file stamp-file)
       (equal (with-open-file (i stamp-file) (read-line i nil "")) (source-stamp src))))

;; Compiled files go in build/ beside the source - or, when that folder can't be written (a
;; system-wide install in /opt), in ~/.cache/fancypencil/build/.
(defun writable-dir-p (dir)
  (ignore-errors
   (ensure-directories-exist dir)
   (let ((f (merge-pathnames ".write-test" dir)))
     (with-open-file (o f :direction :output :if-exists :supersede) (write-line "x" o))
     (delete-file f)
     t)))
(defun env (name) #+sbcl (sb-ext:posix-getenv name) #+ccl (ccl:getenv name) #-(or sbcl ccl) nil)
(defparameter *build-dir*
  (let ((local (merge-pathnames "build/" *root*)))
    (if (or (writable-dir-p local)
            ;; a system-wide install compiled when it was installed: use that while it is current
            (let ((last (car (last *files*))))
              (stamp-matches-p (merge-pathnames (format nil "~A-~A.stamp" last (lisp-implementation-type)) local)
                               (merge-pathnames (format nil "src/~A.lisp" last) *root*))))
        local
        (let ((cache (or (env "XDG_CACHE_HOME") (concatenate 'string (or (env "HOME") "/tmp") "/.cache"))))
          (pathname (concatenate 'string cache "/fancypencil/build/"))))))

;; Two Fancy Pencils started at once (a drawing double-clicked twice) must not compile into
;; build/ together: one wrote over the other's file and left a damaged one that every later
;; start died on. So build/ has a lock: the second start waits until the first has finished
;; compiling and loading. A lock left by a Fancy Pencil that is no longer running is taken over.
(defun process-id ()
  #+sbcl (sb-unix:unix-getpid) #+ccl (ccl::getpid) #-(or sbcl ccl) 0)
(defun process-alive-p (pid)
  ;; on Linux a running process has a /proc/<pid> folder; elsewhere assume it is still running
  (if (probe-file "/proc/self/") (probe-file (format nil "/proc/~D/" pid)) t))
(defun lock-holder (lock)
  (ignore-errors (with-open-file (i lock) (parse-integer (read-line i nil "") :junk-allowed t))))
(defun take-build-lock (lock)
  "Creates LOCK holding this process's id: T. NIL if it can't be created (a read-only build/
that needs no compiling). Waits while another running Fancy Pencil holds it (up to 10 minutes)."
  (loop with start = (get-universal-time) and told = nil
        ;; (the stream is closed normally before returning: leaving WITH-OPEN-FILE by RETURN
        ;; would close it with :ABORT, which deletes the lock it has just made)
        for made = (handler-case
                       (with-open-file (o lock :direction :output :if-exists nil :if-does-not-exist :create)
                         (when o (format o "~D~%" (process-id)) :made))
                     (error () :cannot))
        do (case made (:made (return t)) (:cannot (return nil)))
           (let ((pid (lock-holder lock)) (age (ignore-errors (- (get-universal-time) (file-write-date lock)))))
             ;; taken over when its Fancy Pencil isn't running any more (an empty lock only once
             ;; it is a few seconds old: it may be one just being made), or after 10 minutes
             (when (or (if pid (not (process-alive-p pid)) (and age (> age 5)))
                       (> (- (get-universal-time) start) 600))
               (ignore-errors (delete-file lock))))
           (unless told
             (format t "~&Another Fancy Pencil is getting ready; waiting for it...~%") (finish-output)
             (setf told t))
           (sleep 1)))

(defun replace-file (from to)
  #+ccl (rename-file from to :if-exists :supersede)
  #-ccl (rename-file from to))

(defun compile-into (src fasl stamp)
  "Compiles SRC to FASL by way of a temporary file, so FASL is never left half-written, then
writes the STAMP that says FASL is current."
  (let ((tmp (make-pathname :type (format nil "fasl-~D" (process-id)) :defaults fasl)))
    (ignore-errors (delete-file stamp))
    (multiple-value-bind (out warnings-p failure-p) (compile-file src :output-file tmp)
      (declare (ignore warnings-p failure-p))
      (unless out (error "Fancy Pencil: ~A could not be compiled." src))
      (replace-file out fasl))
    (with-open-file (o stamp :direction :output :if-exists :supersede)
      (write-line (source-stamp src) o))))

(let ((*compile-verbose* nil) (*compile-print* nil) (*load-verbose* nil)
      (lock (merge-pathnames ".lock" *build-dir*)))
  (ensure-directories-exist lock)
  (let ((locked (take-build-lock lock)))
   ;; with the lock held nobody else is compiling: half-made files left by one that was
   ;; stopped part way are rubbish
   (when locked
     (dolist (f (directory (merge-pathnames "*.*" *build-dir*)))
       (let ((type (pathname-type f)))
         (when (and type (> (length type) 5) (string= "fasl-" type :end2 5))
           (ignore-errors (delete-file f))))))
   (unwind-protect
    (handler-bind ((style-warning #'muffle-warning)
                   #+sbcl (sb-ext:compiler-note #'muffle-warning))
     (let ((rebuild nil))
      (dolist (f *files*)
       (let ((src (merge-pathnames (format nil "src/~A.lisp" f) *root*)))
         #+(or sbcl ccl)
         (let* ((fasl (merge-pathnames (format nil "~A-~A.fasl" f (lisp-implementation-type)) *build-dir*))
                (stamp (make-pathname :type "stamp" :defaults fasl)))
           (when (or rebuild (not (probe-file fasl)) (not (stamp-matches-p stamp src)))
             (setf rebuild t)
             (compile-into src fasl stamp))
           ;; a compiled file that won't load (damaged, or from another SBCL) is compiled again,
           ;; once; so is everything after it
           (handler-case (load fasl)
             (#+sbcl sb-ext:invalid-fasl #-sbcl file-error (c)
               (format t "~&~A: compiling ~A again.~%" c f) (finish-output)
               (setf rebuild t)
               (compile-into src fasl stamp)
               (load fasl))))
         #-(or sbcl ccl)
         (load src)))))
    (when locked (ignore-errors (delete-file lock))))))

(setf (symbol-value (find-symbol "*GUI-DIR*" :fancy-pencil)) (merge-pathnames "src/" *root*))
