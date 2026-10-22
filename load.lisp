;;;; load.lisp — compile (SBCL) and load Fancy Pencil without needing ASDF/Quicklisp.
;;;;   sbcl --load load.lisp --eval '(fancy-pencil:main)'

(defpackage :fancy-pencil-loader (:use :cl))
(in-package :fancy-pencil-loader)

(defparameter *root* (make-pathname :name nil :type nil :defaults *load-truename*))
(defparameter *files* '("package" "geom" "db" "textstyle" "ltype" "hatch" "dxf" "dxf2000" "alisp" "input" "gui" "grips"
                         "commands" "dims" "leader" "modify" "modify2" "cmds2" "utilcmds" "dwg" "props" "express" "textcmds" "docs" "uicmds" "clip" "matchprop" "spell" "plot" "main"))

(let ((*compile-verbose* nil) (*compile-print* nil) (*load-verbose* nil))
  (handler-bind ((style-warning #'muffle-warning)
                 #+sbcl (sb-ext:compiler-note #'muffle-warning))
    ;; once one file is recompiled, every file after it is too: they may depend on its
    ;; structures and macros (an update otherwise could load stale compiled code and fail)
    (let ((rebuild nil) (newest 0))
     (dolist (f *files*)
      (let ((src (merge-pathnames (format nil "src/~A.lisp" f) *root*)))
        #+(or sbcl ccl)
        (let ((fasl (merge-pathnames (format nil "build/~A-~A.fasl" f (lisp-implementation-type)) *root*)))
          (ensure-directories-exist fasl)
          (when (or rebuild (not (probe-file fasl))
                    (> (file-write-date src) (file-write-date fasl))
                    (< (file-write-date fasl) newest))     ; older than a file it builds on
            (setf rebuild t)
            (compile-file src :output-file fasl))
          (setf newest (max newest (file-write-date fasl)))
          (load fasl))
        #-(or sbcl ccl)
        (load src))))))

(setf (symbol-value (find-symbol "*GUI-DIR*" :fancy-pencil)) (merge-pathnames "src/" *root*))
