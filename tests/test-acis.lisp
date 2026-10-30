;; AutoCAD 3D solids (ACIS, in a DXF's ACDSDATA section) read as meshes.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-acis.lisp
(in-package :fancy-pencil)
(setf *ui* :tty *output-fn* (lambda (s) s))
(load-aliases)
(current-doc)
(defvar *pass* 0) (defvar *fail* 0)
(defmacro chk (name form expect)
  `(let ((v (handler-case ,form (error (c) (format nil "ERROR ~A" c)))))
     (if (equalp v ,expect) (incf *pass*) (incf *fail*))
     (format t "~:[FAIL~;ok  ~] ~A: ~S~%" (equalp v ,expect) ,name v)))
(defun r3 (x) (/ (round (* 1000 x)) 1000))
(defun check (d) (multiple-value-list (mesh-check (mapcar (lambda (f) (mapcar (lambda (i) (1- (abs i))) f)) (mesh-faces d)))))
(defun solid (h) (find h (live-entities) :key (lambda (e) (dxf :src-handle (ename-data e))) :test #'equal))

;; numbers from binary
(chk "double from bits" (bits-double #x3FF8000000000000) 1.5d0)
(chk "negative double" (bits-double #xC004000000000000) -2.5d0)

(dxf-import (namestring (merge-pathnames "tests/data/acis-solids.dxf" (truename "."))))
(chk "five solids read" (count "POLYFACE" (live-entities) :key #'ent-type :test #'equal) 5)
(chk "nothing left out" *import-skipped* nil)
(chk "counted" *acis-converted* 5)
(let ((meshes (remove-if-not (lambda (e) (equal (ent-type e) "POLYFACE")) (live-entities))))
  (chk "every solid closed and turned outward"
       (mapcar (lambda (e) (check (ename-data e))) meshes)
       (make-list 5 :initial-element '(0 0 0)))
  (chk "every volume positive" (every (lambda (e) (> (mesh-signed-volume (ename-data e)) 0)) meshes) t)
  ;; a box 2 x 2 x ... : 12 triangles, its 12 edges showing
  (let ((box (find-if (lambda (e) (= (length (mesh-faces (ename-data e))) 12)) meshes)))
    (chk "box: 12 triangles" (and box t) t)
    (when box (chk "box: 12 edges show" (length (mesh-edges (ename-data box))) 12)))
  (chk "layers kept" (sort (remove-duplicates (mapcar (lambda (e) (dxf 8 (ename-data e))) meshes) :test #'equal) #'string<)
       '("Elec Sym" "Grade" "Heavy Line" "Studs")))
;; curved solids (a cylinder, a cone, a hex nut's round hole) are divided to follow their surfaces
(chk "curved solids divided" (count-if (lambda (e) (> (length (mesh-faces (ename-data e))) 100))
                                       (remove-if-not (lambda (e) (equal (ent-type e) "POLYFACE")) (live-entities)))
     4)
;; saving: meshes go out as AutoCAD polyface meshes
(dxf-export-2000 "/tmp/fp-acis.dxf")
(dxf-import "/tmp/fp-acis.dxf")
(chk "saved and read back" (count "POLYFACE" (live-entities) :key #'ent-type :test #'equal) 5)
(format t "~%ACIS tests: ~D passed, ~D failed.~%" *pass* *fail*)
