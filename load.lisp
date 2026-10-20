;;;; load.lisp — compile (SBCL) and load LispDraft without needing ASDF/Quicklisp.
;;;;   sbcl --load load.lisp --eval '(lispdraft:main)'

(defpackage :lispdraft-loader (:use :cl))
(in-package :lispdraft-loader)

(defparameter *root* (make-pathname :name nil :type nil :defaults *load-truename*))
(defparameter *files* '("package" "geom" "db" "dxf" "alisp" "input" "gui" "commands" "modify" "utilcmds" "main"))

(let ((*compile-verbose* nil) (*compile-print* nil) (*load-verbose* nil))
  (handler-bind ((style-warning #'muffle-warning)
                 #+sbcl (sb-ext:compiler-note #'muffle-warning))
    (dolist (f *files*)
      (let ((src (merge-pathnames (format nil "src/~A.lisp" f) *root*)))
        #+(or sbcl ccl)
        (let ((fasl (merge-pathnames (format nil "build/~A-~A.fasl" f (lisp-implementation-type)) *root*)))
          (ensure-directories-exist fasl)
          (when (or (not (probe-file fasl)) (> (file-write-date src) (file-write-date fasl)))
            (compile-file src :output-file fasl))
          (load fasl))
        #-(or sbcl ccl)
        (load src)))))

(setf (symbol-value (find-symbol "*GUI-DIR*" :lispdraft)) (merge-pathnames "src/" *root*))
