;;;; print3d.lisp — files for 3D printing: STL (binary or text), 3MF and OBJ out (3DPRINT, STLOUT),
;;;; and STL / OBJ in (STLIN).
;;;;
;;;; Every solid in Fancy Pencil is a mesh of flat faces, so it goes to a printer's slicer (Cura,
;;;; PrusaSlicer, Bambu Studio, OrcaSlicer ...) as triangles. Before writing, each solid is checked:
;;;; a solid that will print must be closed ("watertight": every edge shared by exactly two faces)
;;;; with its faces turned outward. Faces turned inward are put right; open edges are reported.
;;;;
;;;; STL has no units and slicers read it as millimetres, so a drawing in inches is written ×25.4
;;;; (unless you choose drawing units). 3MF says its units, so it is written in the drawing's own.
;;;; Solids are also moved, by default, to sit on the build plate: their lowest corner at 0,0,0
;;;; (AutoCAD's STLOUT wants them in the positive X, Y, Z too).

(in-package :fancy-pencil)

;;; ---------- binary helpers ----------

(defun float32-bits (x)
  (let ((f (coerce x 'single-float)))
    (if (zerop f) 0
        (multiple-value-bind (m e s) (integer-decode-float f)
          (logior (if (minusp s) #x80000000 0)
                  (if (< m #x800000)
                      m                                               ; denormal
                      (logior (ash (+ e 150) 23) (logand m #x7FFFFF))))))))

(defun bits-float32 (b)
  (let* ((s (if (logbitp 31 b) -1d0 1d0)) (e (ldb (byte 8 23) b)) (f (ldb (byte 23 0) b)))
    (cond ((= e 0) (* s (scale-float (d f) -149)))
          ((= e 255) 0d0)                                              ; inf / NaN: ignore
          (t (* s (scale-float (d (logior f #x800000)) (- e 150)))))))

(defun write-u32 (n s) (dotimes (i 4) (write-byte (ldb (byte 8 (* 8 i)) n) s)))
(defun write-u16 (n s) (dotimes (i 2) (write-byte (ldb (byte 8 (* 8 i)) n) s)))
(defun write-f32 (x s) (write-u32 (float32-bits x) s))
(defun u32-at (v i) (logior (aref v i) (ash (aref v (+ i 1)) 8) (ash (aref v (+ i 2)) 16) (ash (aref v (+ i 3)) 24)))
(defun f32-at (v i) (bits-float32 (u32-at v i)))

(defun utf8-bytes (s)
  (let ((out (make-array (length s) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for ch across s
          for c = (char-code ch)
          do (cond ((< c #x80) (vector-push-extend c out))
                   ((< c #x800) (vector-push-extend (logior #xC0 (ash c -6)) out)
                    (vector-push-extend (logior #x80 (logand c #x3F)) out))
                   ((< c #x10000) (vector-push-extend (logior #xE0 (ash c -12)) out)
                    (vector-push-extend (logior #x80 (logand (ash c -6) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand c #x3F)) out))
                   (t (vector-push-extend (logior #xF0 (ash c -18)) out)
                      (vector-push-extend (logior #x80 (logand (ash c -12) #x3F)) out)
                      (vector-push-extend (logior #x80 (logand (ash c -6) #x3F)) out)
                      (vector-push-extend (logior #x80 (logand c #x3F)) out))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; ---------- a small zip writer (3MF files are zip packages) ----------

(defvar *crc-table*
  (let ((v (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 v)
      (let ((c n)) (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref v n) c)))))

(defun crc32 (bytes)
  (let ((c #xFFFFFFFF))
    (loop for b across bytes do (setf c (logxor (aref *crc-table* (logand (logxor c b) #xFF)) (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun write-zip (path entries)
  "Write ENTRIES ((name . bytes) ...) to PATH as a zip file (stored, no compression)."
  (multiple-value-bind (s mi h dd mm yy) (decode-universal-time (get-universal-time))
    (let ((dtime (logior (ash h 11) (ash mi 5) (floor s 2)))
          (ddate (logior (ash (max 0 (- yy 1980)) 9) (ash mm 5) dd))
          (offsets nil) (pos 0))
      (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (flet ((header (sig name data crc offset central)
                 (let ((nb (utf8-bytes name)))
                   (write-u32 sig o)
                   (when central (write-u16 20 o))
                   (write-u16 20 o) (write-u16 #x800 o) (write-u16 0 o)        ; version, UTF-8 names, stored
                   (write-u16 dtime o) (write-u16 ddate o)
                   (write-u32 crc o) (write-u32 (length data) o) (write-u32 (length data) o)
                   (write-u16 (length nb) o) (write-u16 0 o)
                   (when central (write-u16 0 o) (write-u16 0 o) (write-u16 0 o) (write-u32 0 o) (write-u32 offset o))
                   (write-sequence nb o)
                   (+ (if central 46 30) (length nb)))))
          (dolist (e entries)
            (let ((crc (crc32 (cdr e))))
              (push (list (car e) (cdr e) crc pos) offsets)
              (incf pos (header #x04034b50 (car e) (cdr e) crc 0 nil))
              (write-sequence (cdr e) o)
              (incf pos (length (cdr e)))))
          (let ((cd-start pos) (cd-size 0))
            (dolist (x (reverse offsets))
              (destructuring-bind (name data crc offset) x
                (incf cd-size (header #x02014b50 name data crc offset t))))
            (write-u32 #x06054b50 o) (write-u16 0 o) (write-u16 0 o)
            (write-u16 (length entries) o) (write-u16 (length entries) o)
            (write-u32 cd-size o) (write-u32 cd-start o) (write-u16 0 o)))))
    path))

;;; ---------- solids as triangles ----------

(defun mesh-signed-volume (data)
  (loop for f in (mesh-faces data)
        for ps = (mesh-face-points data f)
        sum (loop for i from 1 below (1- (length ps))
                  sum (/ (vdot3 (first ps) (cross3 (nth i ps) (nth (1+ i) ps))) 6d0))))

(defun mesh-triangles (data)
  "The faces of mesh DATA as triangles (point triples), turned outward."
  (let* ((vs (coerce (mesh-verts data) 'vector))
         (flip (minusp (mesh-signed-volume data)))
         (out nil))
    (dolist (f (mesh-faces data) (nreverse out))
      (let ((ps (mapcar (lambda (i) (aref vs (1- (abs i)))) f)))
        (loop for i from 1 below (1- (length ps))
              do (let ((a (first ps)) (b (nth i ps)) (c (nth (1+ i) ps)))
                   (when (> (vlen3 (cross3 (v- b a) (v- c a))) 1d-18)   ; skip faces with no area
                     (push (if flip (list a c b) (list a b c)) out))))))))

(defun index-triangles (tris tol)
  "Shared corners: (values vertex-vector index-triples). Corners closer than TOL are one."
  (let ((h (make-hash-table :test #'equal)) (verts (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((idx (p)
             (let ((key (list (round (px p) tol) (round (py p) tol) (round (pz p) tol))))
               (or (gethash key h)
                   (setf (gethash key h) (vector-push-extend p verts))))))
      (let ((faces (loop for (a b c) in tris
                         for ia = (idx a) for ib = (idx b) for ic = (idx c)
                         unless (or (= ia ib) (= ib ic) (= ia ic)) collect (list ia ib ic))))
        (values verts faces)))))

(defun mesh-check (faces)
  "How a set of indexed triangles would print: (values open-edges bad-edges flipped-edges)."
  (let ((und (make-hash-table :test #'equal)) (dir (make-hash-table :test #'equal)))
    (dolist (f faces)
      (loop for (a b) on (append f (list (first f))) while b
            do (incf (gethash (if (< a b) (cons a b) (cons b a)) und 0))
               (incf (gethash (cons a b) dir 0))))
    (let ((open 0) (bad 0) (flipped 0))
      (maphash (lambda (k n) (declare (ignore k)) (cond ((= n 1) (incf open)) ((> n 2) (incf bad)))) und)
      (maphash (lambda (k n) (declare (ignore k)) (when (> n 1) (incf flipped))) dir)
      (values open bad flipped))))

(defun tris-bbox (tris)
  (let ((lo (list most-positive-double-float most-positive-double-float most-positive-double-float))
        (hi (list most-negative-double-float most-negative-double-float most-negative-double-float)))
    (dolist (tr tris)
      (dolist (p tr)
        (setf lo (mapcar #'min lo (list (px p) (py p) (pz p))) hi (mapcar #'max hi (list (px p) (py p) (pz p))))))
    (values lo hi)))

(defun file-unit-scale (to)
  "Drawing units -> TO (:mm or :drawing)."
  (if (eq to :drawing) 1d0
      (case (sysvar "INSUNITS") (1 25.4d0) (2 304.8d0) (4 1d0) (5 10d0) (6 1000d0) (13 0.001d0) (14 100d0) (t 1d0))))

(defun units-name (&optional (u (sysvar "INSUNITS")))
  (case u (1 "inches") (2 "feet") (4 "millimeters") (5 "centimeters") (6 "meters") (13 "microns") (0 "unitless") (t "drawing units")))

(defun close-gaps (verts faces tol)
  "Mend the cracks boolean operations leave: where a corner of one face lies along the edge of
its neighbour (a T-junction), the neighbour is split there so the edges meet exactly.
Returns (values faces splits)."
  (let ((splits 0))
    (loop repeat 8
          do (let ((und (make-hash-table :test #'equal)))
               (dolist (f faces)
                 (loop for (a b) on (append f (list (first f))) while b
                       do (incf (gethash (if (< a b) (cons a b) (cons b a)) und 0))))
               (let ((cands (let ((h (make-hash-table)))
                              (maphash (lambda (k n) (when (= n 1) (setf (gethash (car k) h) t (gethash (cdr k) h) t))) und)
                              (loop for k being the hash-keys of h collect k)))
                     (before splits))
                 (when (null cands) (return))
                 (setf faces
                       (loop for f in faces
                             append (let ((done nil))
                                      (loop for k below 3 until done
                                            do (let* ((a (nth k f)) (b (nth (mod (1+ k) 3) f)) (c (nth (mod (+ k 2) 3) f)))
                                                 (when (= 1 (gethash (if (< a b) (cons a b) (cons b a)) und 0))
                                                   (let* ((pa (aref verts a)) (ab (v- (aref verts b) pa)) (l2 (vdot3 ab ab))
                                                          (on (and (> l2 0)
                                                                   (sort (loop for v in cands
                                                                               for q = (v- (aref verts v) pa)
                                                                               for tt = (/ (vdot3 q ab) l2)
                                                                               when (and (/= v a) (/= v b) (< 1d-9 tt (- 1 1d-9))
                                                                                         (< (vlen3 (v- q (v* ab tt))) tol))
                                                                                 collect (cons tt v))
                                                                         #'< :key #'car))))
                                                     (when on
                                                       (let ((chain (append (list a) (mapcar #'cdr on) (list b))))
                                                         (incf splits (length on))
                                                         (setf done (loop for (x y) on chain while y collect (list x y c)))))))))
                                      (or done (list f)))))
                 (when (= splits before) (return)))))
    (values faces splits)))

(defun mend-triangles (tris tol)
  "TRIS with T-junction cracks mended: (values triangles splits)."
  (multiple-value-bind (verts faces) (index-triangles tris tol)
    (multiple-value-bind (faces splits) (close-gaps verts faces (* 10 tol))
      (values (if (zerop splits) tris
                  (mapcar (lambda (f) (mapcar (lambda (i) (aref verts i)) f)) faces))
              splits))))

(defun print-parts (ents &key (scale 1d0) (place t))
  "Each solid as (name triangles cracks-mended), scaled, mended and (PLACE) moved to sit at 0,0,0."
  (let* ((parts (loop for e in ents for i from 1
                      collect (let* ((tris (mapcar (lambda (tr) (mapcar (lambda (p) (v* (pt3 p) scale)) tr))
                                                   (mesh-triangles (ename-data e))))
                                     (size (multiple-value-bind (lo hi) (tris-bbox tris) (reduce #'max (mapcar #'- hi lo))))
                                     (tol (* 1d-7 (max 1d0 size))))
                                (multiple-value-bind (tris splits) (mend-triangles tris tol)
                                  (list (format nil "Solid ~D" i) tris splits)))))
         (all (loop for p in parts append (second p))))
    (if (and place all)
        (let ((lo (tris-bbox all)))
          (let ((off (pt (- (first lo)) (- (second lo)) (- (third lo)))))
            (mapcar (lambda (p) (list (first p) (mapcar (lambda (tr) (mapcar (lambda (q) (v+ q off)) tr)) (second p)) (third p)))
                    parts)))
        parts)))

;;; ---------- writers ----------

(defun tri-normal (tr)
  (let ((n (cross3 (v- (second tr) (first tr)) (v- (third tr) (first tr)))))
    (if (> (vlen3 n) 1d-30) (unit3 n) (pt 0 0 0))))

(defun write-stl-binary (path parts)
  (let ((tris (loop for p in parts append (second p))))
    (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (let ((hdr (make-array 80 :element-type '(unsigned-byte 8) :initial-element 32))
            (title (utf8-bytes (format nil "Fancy Pencil ~A binary STL" *version*))))
        (replace hdr title)
        (write-sequence hdr o))
      (write-u32 (length tris) o)
      (dolist (tr tris)
        (let ((n (tri-normal tr)))
          (write-f32 (px n) o) (write-f32 (py n) o) (write-f32 (pz n) o)
          (dolist (p tr) (write-f32 (px p) o) (write-f32 (py p) o) (write-f32 (pz p) o))
          (write-u16 0 o))))
    (length tris)))

(defun write-stl-ascii (path parts)
  (let ((n 0))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :latin-1)
      (dolist (part parts)
        (format o "solid ~A~%" (substitute #\_ #\Space (first part)))
        (dolist (tr (second part))
          (incf n)
          (let ((nv (tri-normal tr)))
            (format o "  facet normal ~,6E ~,6E ~,6E~%    outer loop~%" (px nv) (py nv) (pz nv))
            (dolist (p tr) (format o "      vertex ~,6E ~,6E ~,6E~%" (px p) (py p) (pz p)))
            (format o "    endloop~%  endfacet~%")))
        (format o "endsolid ~A~%" (substitute #\_ #\Space (first part)))))
    n))

(defun fnum (x) (let ((s (format nil "~,6F" x))) (string-right-trim "." (string-right-trim "0" s))))

(defun write-obj (path parts tol)
  (let ((n 0) (base 0))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
      (format o "# Fancy Pencil ~A~%" *version*)
      (dolist (part parts)
        (multiple-value-bind (verts faces) (index-triangles (second part) tol)
          (format o "o ~A~%" (substitute #\_ #\Space (first part)))
          (loop for p across verts do (format o "v ~A ~A ~A~%" (fnum (px p)) (fnum (py p)) (fnum (pz p))))
          (dolist (f faces) (incf n) (format o "f ~D ~D ~D~%" (+ base 1 (first f)) (+ base 1 (second f)) (+ base 1 (third f))))
          (incf base (length verts)))))
    n))

(defun xml-escape (s)
  (with-output-to-string (o)
    (loop for c across s do (case c (#\& (write-string "&amp;" o)) (#\< (write-string "&lt;" o))
                              (#\> (write-string "&gt;" o)) (#\" (write-string "&quot;" o)) (t (write-char c o))))))

(defun write-3mf (path parts unit tol)
  (let* ((n 0)
         (model
           (with-output-to-string (o)
             (format o "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<model unit=\"~A\" xml:lang=\"en-US\" xmlns=\"http://schemas.microsoft.com/3dmanufacturing/core/2015/02\">~%" unit)
             (format o " <metadata name=\"Application\">Fancy Pencil ~A</metadata>~% <resources>~%" *version*)
             (loop for part in parts for id from 1
                   do (multiple-value-bind (verts faces) (index-triangles (second part) tol)
                        (format o "  <object id=\"~D\" type=\"model\" name=\"~A\">~%   <mesh>~%    <vertices>~%" id (xml-escape (first part)))
                        (loop for p across verts do (format o "     <vertex x=\"~A\" y=\"~A\" z=\"~A\"/>~%" (fnum (px p)) (fnum (py p)) (fnum (pz p))))
                        (format o "    </vertices>~%    <triangles>~%")
                        (dolist (f faces) (incf n) (format o "     <triangle v1=\"~D\" v2=\"~D\" v3=\"~D\"/>~%" (first f) (second f) (third f)))
                        (format o "    </triangles>~%   </mesh>~%  </object>~%")))
             (format o " </resources>~% <build>~%")
             (loop for id from 1 to (length parts) do (format o "  <item objectid=\"~D\"/>~%" id))
             (format o " </build>~%</model>~%"))))
    (write-zip path
               (list (cons "[Content_Types].xml"
                           (utf8-bytes "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"model\" ContentType=\"application/vnd.ms-package.3dmanufacturing-3dmodel+xml\"/></Types>
"))
                     (cons "_rels/.rels"
                           (utf8-bytes "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Target=\"/3D/3dmodel.model\" Id=\"rel0\" Type=\"http://schemas.microsoft.com/3dmanufacturing/2013/01/3dmodel\"/></Relationships>
"))
                     (cons "3D/3dmodel.model" (utf8-bytes model))))
    n))

(defun units-3mf ()
  (case (sysvar "INSUNITS") (1 "inch") (2 "foot") (5 "centimeter") (6 "meter") (13 "micron") (t "millimeter")))

;;; ---------- exporting ----------

(defun print-solids (ents)
  "The solids among ENTS (with a note about the rest)."
  (let ((ok (remove-if-not (lambda (e) (equal (ent-type e) "POLYFACE")) ents)))
    (when (< (length ok) (length ents))
      (out (format nil "~D object~:P skipped: only solids (and 3D meshes) can be printed.~%" (- (length ents) (length ok)))))
    ok))

(defun export-3d-print (ents path format &key (to :mm) (place t))
  "Write solids ENTS to PATH as FORMAT (:stl :stl-ascii :3mf :obj). Returns a report string."
  (let* ((scale (if (eq format :3mf) 1d0 (file-unit-scale to)))
         (parts (print-parts ents :scale scale :place place))
         (all (loop for p in parts append (second p)))
         (size (multiple-value-bind (lo hi) (tris-bbox all) (mapcar #'- hi lo)))
         (tol (* 1d-7 (max 1d0 (reduce #'max size))))
         (n (ecase format
              (:stl (write-stl-binary path parts))
              (:stl-ascii (write-stl-ascii path parts))
              (:obj (write-obj path parts tol))
              (:3mf (write-3mf path parts (units-3mf) tol))))
         (units (cond ((eq format :3mf) (units-3mf))
                      ((/= scale 1) "mm")
                      ((and (eq to :mm) (member (sysvar "INSUNITS") '(0 4))) "mm")
                      (t (units-name))))
         (problems (loop for p in parts
                         for (open bad) = (multiple-value-list
                                           (multiple-value-bind (v f) (index-triangles (second p) tol)
                                             (declare (ignore v)) (mesh-check f)))
                         when (or (plusp open) (plusp bad))
                           collect (format nil "~A has ~A" (first p)
                                           (format nil "~{~A~^ and ~}"
                                                   (remove nil (list (and (plusp open) (format nil "~D open edge~:P" open))
                                                                     (and (plusp bad) (format nil "~D edge~:P shared by more than two faces" bad)))))))))
    (values
     (format nil "~A: ~D solid~:P, ~D triangle~:P, ~{~A~^ x ~} ~A.~%~A~%"
             (file-namestring path) (length parts) n (mapcar (lambda (x) (fmt-plain (/ (round (* x 1000)) 1000))) size) units
             (format nil "~@[~A~%~]~A"
                     (let ((m (reduce #'+ (mapcar #'third parts))))
                       (and (plusp m) (format nil "~D small crack~:P left by booleans mended." m)))
                     (if problems
                         (format nil "Check before printing - not closed: ~{~A~^; ~}. The slicer may repair small gaps." problems)
                         "All solids are closed (watertight) - ready for the slicer.")))
     n)))

(defparameter *print-formats*
  '(("STL (binary) - every slicer" :stl "stl") ("STL (text)" :stl-ascii "stl") ("3MF - with units" :3mf "3mf") ("OBJ" :obj "obj")))

(defun default-print-path (ext)
  (let ((f (drawing-filename *dwg*)))
    (if f (namestring (make-pathname :type ext :defaults (pathname f))) (format nil "Drawing1.~A" ext))))

(defun do-3d-print (ents format to place &optional path)
  (let* ((ext (third (find format *print-formats* :key #'second)))
         (path (or path (ask-file t "Save for 3D Printing" (default-print-path ext) ext))))
    (when path
      (let ((path (ensure-ext path ext)))
        (handler-case (out (export-3d-print ents path format :to to :place place))
          (error (c) (out (format nil "Could not write ~A: ~A~%" path c))))))))

(defcmd "3DPRINT" ("PRINT3D" "EXPORT3D") "Saves solids as a file for a 3D printer's slicer: STL, 3MF or OBJ."
  (let ((ents (print-solids (get-selection "Select solids to print: "))))
    (when ents
      (if (use-inplace-editor-p)
          (let* ((mm-choice (if (member (sysvar "INSUNITS") '(0 4)) "Millimeters" (format nil "Millimeters (~A x 25.4)" (units-name))))
                 (r (gui-form "3D Print"
                              (list (list "F" "File type" (first (first *print-formats*)) :combo (mapcar #'first *print-formats*))
                                    (list "U" "STL / OBJ units" mm-choice :combo (list mm-choice (format nil "Drawing units (~A)" (units-name))))
                                    (list "P" "Place on the build plate (0,0,0)" "Yes" :combo '("Yes" "No")))
                              "Save...")))
            (when r
              (let ((fmt (second (find (cdr (assoc "F" r :test #'string=)) *print-formats* :key #'first :test #'string=)))
                    (to (if (search "Drawing" (or (cdr (assoc "U" r :test #'string=)) "")) :drawing :mm))
                    (place (not (equal (cdr (assoc "P" r :test #'string=)) "No"))))
                (do-3d-print ents (or fmt :stl) to place))))
          (let* ((k (gkw "File type [STL/Text-stl/3MF/OBJ] <STL>: " '("STL" "Text" "3MF" "OBJ") "STL"))
                 (fmt (cond ((equal k "Text") :stl-ascii) ((equal k "3MF") :3mf) ((equal k "OBJ") :obj) (t :stl))))
            (do-3d-print ents fmt :mm t))))))

(defcmd "STLOUT" () "Saves solids as an STL file for 3D printing."
  (let ((ents (print-solids (get-selection "Select solids: "))))
    (when ents
      (let ((bin (gkw "Create a binary STL file? [Yes/No] <Yes>: " '("Yes" "No") "Yes")))
        (do-3d-print ents (if (equal bin "No") :stl-ascii :stl) :mm t)))))

;;; ---------- importing ----------

(defun read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun read-stl (path)
  "Triangles (point triples) of an STL file, binary or text."
  (let* ((v (read-file-bytes path)) (n (length v)))
    (if (and (>= n 84) (= n (+ 84 (* 50 (u32-at v 80)))))
        (loop for i below (u32-at v 80)
              for o = (+ 84 (* 50 i) 12)
              collect (loop for k below 3 for q = (+ o (* 12 k))
                            collect (pt (f32-at v q) (f32-at v (+ q 4)) (f32-at v (+ q 8)))))
        ;; text: every "vertex x y z", three to a facet
        (let ((pts nil) (*read-default-float-format* 'double-float))
          (with-input-from-string (in (map 'string #'code-char v))
            (loop for line = (read-line in nil) while line
                  do (let* ((l (string-trim '(#\Space #\Tab #\Return) line)))
                       (when (and (> (length l) 7) (string-equal (subseq l 0 7) "vertex "))
                         (let ((nums (mapcar (lambda (w) (d (or (parse-number-text w) 0))) (split-words (subseq l 7)))))
                           (push (pt (or (first nums) 0d0) (or (second nums) 0d0) (or (third nums) 0d0)) pts))))))
          (loop for (a b c) on (nreverse pts) by #'cdddr when c collect (list a b c))))))

(defun split-words (s)
  (let ((out nil) (cur nil))
    (loop for c across s
          do (if (member c '(#\Space #\Tab #\Return))
                 (when cur (push (coerce (nreverse cur) 'string) out) (setf cur nil))
                 (push c cur)))
    (when cur (push (coerce (nreverse cur) 'string) out))
    (nreverse out)))

(defun read-obj (path)
  "Triangles of an OBJ file (polygons split into fans)."
  (let ((verts (make-array 0 :adjustable t :fill-pointer 0)) (tris nil))
    (with-open-file (in path :external-format :latin-1)
      (loop for line = (read-line in nil) while line
            do (let ((w (split-words line)))
                 (cond ((equal (first w) "v")
                        (let ((n (mapcar (lambda (x) (d (or (parse-number-text x) 0))) (subseq (rest w) 0 (min 3 (length (rest w)))))))
                          (vector-push-extend (pt (or (first n) 0d0) (or (second n) 0d0) (or (third n) 0d0)) verts)))
                       ((equal (first w) "f")
                        (let ((ids (loop for x in (rest w)
                                         for i = (ignore-errors (parse-integer x :end (position #\/ x)))
                                         when i collect (if (minusp i) (+ (length verts) i) (1- i)))))
                          (when (every (lambda (i) (< -1 i (length verts))) ids)
                            (loop for k from 1 below (1- (length ids))
                                  do (push (list (aref verts (first ids)) (aref verts (nth k ids)) (aref verts (nth (1+ k) ids))) tris)))))))))
    (nreverse tris)))

(defun triangles-to-mesh (tris &optional (smooth-angle 20d0))
  "A Fancy Pencil mesh from triangles: shared corners joined, and the edges between faces that
meet almost flat hidden (so a box shows its 12 edges, not its triangles)."
  (multiple-value-bind (lo hi) (tris-bbox tris)
    (let ((tol (* 1d-7 (max 1d0 (reduce #'max (mapcar #'- hi lo))))))
      (multiple-value-bind (verts faces) (index-triangles tris tol)
        (let* ((fv (coerce faces 'vector))
               (normals (map 'vector (lambda (f) (or (newell-normal (mapcar (lambda (i) (aref verts i)) f)) (pt 0 0 1))) fv))
               (edge-faces (make-hash-table :test #'equal))
               (cosmax (cos (* smooth-angle (/ +pi+ 180)))))
          (loop for f across fv for k from 0
                do (loop for (a b) on (append f (list (first f))) while b
                         do (push k (gethash (if (< a b) (cons a b) (cons b a)) edge-faces))))
          (make-mesh-data
           (coerce verts 'list)
           (loop for f across fv for k from 0
                 collect (loop for (a b) on (append f (list (first f))) while b
                               collect (let* ((adj (remove k (gethash (if (< a b) (cons a b) (cons b a)) edge-faces)))
                                              (hide (and (= (length adj) 1)
                                                         (> (vdot3 (aref normals k) (aref normals (first adj))) cosmax))))
                                         (if hide (- (1+ a)) (1+ a)))))))))))

(defcmd "STLIN" ("IMPORT3D" "OBJIN") "Brings an STL or OBJ file (from a 3D model or a printing site) in as a solid."
  (let ((f (ask-file nil "Import STL or OBJ" nil "stl obj")))
    (when f
      (cond
        ((not (probe-file f)) (out (format nil "Cannot find ~A~%" f)))
        (t
         (let* ((tris (if (string-equal (pathname-type (pathname f)) "obj") (read-obj f) (read-stl f))))
           (if (null tris)
               (out (format nil "No triangles found in ~A.~%" (file-namestring f)))
               (let* ((k (gkw "Units in the file [Millimeters/Inches/Drawing] <Millimeters>: "
                              '("Millimeters" "Inches" "Drawing") "Millimeters"))
                      (s (cond ((equal k "Drawing") 1d0)
                               ((equal k "Inches") (/ 25.4d0 (file-unit-scale :mm)))
                               (t (/ 1d0 (file-unit-scale :mm)))))
                      (tris (if (= s 1) tris (mapcar (lambda (tr) (mapcar (lambda (p) (v* p s)) tr)) tris)))
                      (data (triangles-to-mesh tris))
                      (e (add-entity data)))
                 (multiple-value-bind (open bad) (mesh-check (mapcar (lambda (f) (mapcar (lambda (i) (1- (abs i))) f)) (mesh-faces data)))
                   (multiple-value-bind (lo hi) (tris-bbox tris)
                     (out (format nil "~A: ~D triangle~:P, ~{~A~^ x ~} ~A, ~A.~%"
                                  (file-namestring f) (length tris)
                                  (mapcar (lambda (x) (fmt-plain (/ (round (* x 1000)) 1000))) (mapcar #'- hi lo))
                                  (units-name)
                                  (if (and (zerop open) (zerop bad)) "closed (watertight)"
                                      (format nil "not closed: ~D open edge~:P~[~:;, ~:*~D shared by more than two faces~]" open bad))))))
                 (when (gui-p) (funcall 'zoom-extents))
                 e))))))))
