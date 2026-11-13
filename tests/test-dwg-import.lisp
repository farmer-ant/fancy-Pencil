;; What opening a DWG brings in (ODA File Converter turns it into a DXF first): tables, solids and
;; regions in both of AutoCAD's ACIS formats, and pictures found beside the DWG, not beside the
;; converted copy.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-dwg-import.lisp
(in-package :fancy-pencil)
(setf *ui* :tty *output-fn* (lambda (s) s))
(load-aliases)
(current-doc)
(defvar *pass* 0) (defvar *fail* 0)
(defmacro chk (name form expect)
  `(let ((v (handler-case ,form (error (c) (format nil "ERROR ~A" c)))))
     (if (equalp v ,expect) (incf *pass*) (incf *fail*))
     (format t "~:[FAIL~;ok  ~] ~A: ~S~%" (equalp v ,expect) ,name v)))
(defun al (s) (alisp-eval-string s))
(defun data (f) (namestring (merge-pathnames (concatenate 'string "tests/data/" f) (truename "."))))
(defun types () (sort (mapcar #'ent-type (live-entities)) #'string<))
(defun tmp (f) (concatenate 'string "/tmp/" f))
(defun slurp (f) (with-open-file (in f) (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in)))))
(defun test-replace-all (text old new)
  (with-output-to-string (s)
    (loop with i = 0
          for j = (search old text :start2 i)
          do (write-string text s :start i :end (or j (length text)))
             (if j (progn (write-string new s) (setf i (+ j (length old)))) (return)))))

;; a table: AutoCAD's ACAD_TABLE comes in as an insert of its picture block
(dxf-import (data "table.dxf"))
(chk "table + line, nothing skipped" (list (types) *import-skipped*) '(("INSERT" "LINE") nil))
(let ((ins (find "INSERT" (live-entities) :key #'ent-type :test #'equal)))
  (chk "the table is in place: 60 x 24 at (100,50)" (mapcar #'round (entity-bbox ins)) '(100 26 160 50))
  (chk "its cells' text came along" (count "MTEXT" (block-def-entities (find-block (dxf 2 (ent-data ins)))) :key (lambda (d) (dxf 0 d)) :test #'equal) 6))

;; solids: AutoCAD 2018+ writes "ASM BinaryFile4" where 2013 wrote "ACIS BinaryFile"
(dxf-import (data "acis-solids.dxf"))
(let ((n13 (length (live-entities))) (v13 (loop for e in (live-entities) sum (or (ignore-errors (mesh-volume (ent-data e))) 0))))
  (let* ((text (slurp (data "acis-solids.dxf")))
         (asm (test-replace-all text "414349532042696E61727946696C65" "41534D2042696E61727946696C6534")))
    (with-open-file (o (tmp "asm-solids.dxf") :direction :output :if-exists :supersede) (write-string asm o))
    (chk "the file was rewritten with the newer header" (and (search "41534D2042696E61727946696C6534" asm) t) t)
    (dxf-import (tmp "asm-solids.dxf"))
    (chk "the same solids from the newer format" (list (length (live-entities)) (round (loop for e in (live-entities) sum (or (ignore-errors (mesh-volume (ent-data e))) 0))))
         (list n13 (round v13)))))

;; pictures: looked for beside the DWG
(al "(command \"erase\" \"all\" \"\")")
(al (format nil "(command \"imageattach\" ~S '(0 0) 8)" (data "pic.jpg")))
(al (format nil "(command \"saveas\" ~S)" (tmp "img-abs.dxf")))
(let ((text (slurp (tmp "img-abs.dxf"))))
  ;; as AutoCAD keeps it: just the name, relative to the drawing
  (with-open-file (o (tmp "img-rel.dxf") :direction :output :if-exists :supersede)
    (write-string (test-replace-all text (data "pic.jpg") "pic.jpg") o)))
(let ((*attached-base-dir* (directory-namestring (data "pic.jpg"))))
  (dxf-import (tmp "img-rel.dxf")))
(let ((im (find "IMAGE" (live-entities) :key #'ent-type :test #'equal)))
  (chk "the picture is found beside the drawing" (and im (equal (dxf :file (ent-data im)) (data "pic.jpg"))) t))

(format t "~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
