;;;; load.lisp — compile (SBCL) and load Fancy Pencil without needing ASDF/Quicklisp.
;;;;   sbcl --load load.lisp --eval '(fancy-pencil:main)'

(defpackage :fancy-pencil-loader (:use :cl))
(in-package :fancy-pencil-loader)

(defparameter *root* (make-pathname :name nil :type nil :defaults *load-truename*))
(defparameter *files* '("package" "geom" "db" "plane" "mesh" "vector-font" "text3d" "textstyle" "ltype" "hatch" "dxf" "dxf2000" "alisp" "input" "gui" "grips"
                         "commands" "dims" "leader" "modify" "modify2" "cmds2" "utilcmds" "dwg" "props" "express" "textcmds" "docs" "uicmds" "clip" "matchprop" "hover" "prefs" "recent" "blockedit" "views" "solids" "csg" "spell" "plot" "main"))

;; A compiled file is used only if it was built from exactly this source file: each one has a
;; stamp recording its source's date and size. (Comparing dates alone fails after an update,
;; because unpacked files carry their release date, which can be older than the old build.)
;; Once one file is recompiled, every file after it is too: they may use its structures and macros.
(defun source-stamp (src)
  (format nil "~D ~D" (file-write-date src) (with-open-file (i src :element-type '(unsigned-byte 8)) (file-length i))))

(defun stamp-matches-p (stamp-file src)
  (and (probe-file stamp-file)
       (equal (with-open-file (i stamp-file) (read-line i nil "")) (source-stamp src))))

(let ((*compile-verbose* nil) (*compile-print* nil) (*load-verbose* nil))
  (handler-bind ((style-warning #'muffle-warning)
                 #+sbcl (sb-ext:compiler-note #'muffle-warning))
    (let ((rebuild nil))
     (dolist (f *files*)
      (let ((src (merge-pathnames (format nil "src/~A.lisp" f) *root*)))
        #+(or sbcl ccl)
        (let* ((fasl (merge-pathnames (format nil "build/~A-~A.fasl" f (lisp-implementation-type)) *root*))
               (stamp (make-pathname :type "stamp" :defaults fasl)))
          (ensure-directories-exist fasl)
          (when (or rebuild (not (probe-file fasl)) (not (stamp-matches-p stamp src)))
            (setf rebuild t)
            (ignore-errors (delete-file stamp))
            (compile-file src :output-file fasl)
            (with-open-file (o stamp :direction :output :if-exists :supersede)
              (write-line (source-stamp src) o)))
          (load fasl))
        #-(or sbcl ccl)
        (load src))))))

(setf (symbol-value (find-symbol "*GUI-DIR*" :fancy-pencil)) (merge-pathnames "src/" *root*))
