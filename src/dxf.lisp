;;;; dxf.lisp — DXF import (R12 through current ASCII DXF) and export (R12/AC1009).

(in-package :fancy-pencil)

(defun dxf-value (code str)
  (let ((s (string-trim '(#\Space #\Tab #\Return) str)))
    (flet ((num () (let ((*read-default-float-format* 'double-float))
                     (handler-case (d (let ((v (read-from-string s)))
                                        (if (realp v) v 0)))
                       (error () 0d0))))
           (int () (handler-case (parse-integer s :junk-allowed t) (error () 0))))
      (cond ((<= 10 code 59) (num))
            ((<= 60 code 99) (or (int) 0))
            ((<= 110 code 149) (num))
            ((<= 160 code 179) (or (int) 0))
            ((<= 210 code 239) (num))
            ((<= 270 code 299) (or (int) 0))
            ((<= 370 code 389) (or (int) 0))
            ((<= 400 code 409) (or (int) 0))
            ((<= 420 code 429) (or (int) 0))
            ((<= 440 code 459) (or (int) 0))
            ((<= 460 code 469) (num))
            ((<= 1010 code 1059) (num))
            ((<= 1060 code 1071) (or (int) 0))
            (t (decode-dxf-unicode (string-right-trim '(#\Return) str)))))))

(defun decode-dxf-unicode (s)
  "Replace \\U+XXXX escapes (used by AutoCAD in ANSI DXF files) with characters."
  (if (not (search "\\U+" s))
      s
      (with-output-to-string (o)
        (let ((i 0) (n (length s)))
          (loop while (< i n) do
            (if (and (<= (+ i 7) n) (string= "\\U+" s :start2 i :end2 (+ i 3))
                     (every (lambda (c) (digit-char-p c 16)) (subseq s (+ i 3) (+ i 7))))
                (progn (write-char (code-char (parse-integer s :start (+ i 3) :end (+ i 7) :radix 16)) o)
                       (incf i 7))
                (progn (write-char (char s i) o) (incf i))))))))

(defun read-file-lines (path)
  (flet ((try (fmt)
           (with-open-file (in path :external-format fmt)
             (loop for l = (read-line in nil) while l collect l))))
    (handler-case (try :utf-8)
      (error () (try :latin-1)))))

(defun dxf-pairs (lines)
  (loop for (c v) on lines by #'cddr
        while v
        for code = (handler-case (parse-integer (string-trim '(#\Space #\Tab #\Return) c))
                     (error () nil))
        when code collect (cons code (dxf-value code v))))

(defun group-coords (pairs &optional two-d-codes)
  "Merge X/Y/Z groups (10/20/30 etc.) into point lists (10 x y z)."
  (let ((out nil))
    (loop while pairs do
      (let* ((pr (pop pairs)) (code (car pr)))
        (if (or (<= 10 code 18) (= code 210) (<= 1010 code 1013))
            (let ((x (cdr pr)) (y 0d0) (z nil))
              (when (and pairs (= (car (first pairs)) (+ code 10))) (setf y (cdr (pop pairs))))
              (when (and pairs (= (car (first pairs)) (+ code 20))) (setf z (cdr (pop pairs))))
              (push (if (and (member code two-d-codes) (null z))
                        (list code x y)
                        (list code x y (or z 0d0)))
                    out))
            (push pr out))))
    (nreverse out)))

(defun split-entities (pairs)
  "Split a flat pair list into lists each starting with a (0 . type) pair."
  (let ((ents nil) (cur nil))
    (dolist (p pairs)
      (when (= (car p) 0)
        (when cur (push (nreverse cur) ents))
        (setf cur nil))
      (push p cur))
    (when cur (push (nreverse cur) ents))
    (nreverse ents)))

(defun section-pairs (pairs name)
  (let ((start (loop for (a b) on pairs
                     for i from 0
                     when (and (equal a '(0 . "SECTION")) (equal b (cons 2 name))) return (+ i 2))))
    (when start
      (let ((rest (nthcdr start pairs)))
        (subseq rest 0 (position '(0 . "ENDSEC") rest :test #'equal))))))

(defun clean-group (g)
  (remove-if (lambda (p) (and (consp p) (member (car p) '(5 330 360 102 -1 67 410 1001 1000 1002 1070 1071 1040 1041 1005))))
             g))

(defun ocs-fix (data)
  "Flatten entities whose extrusion is (0 0 -1) into world coordinates."
  (let ((n (dxf 210 data)))
    (if (and n (< (pz n) -0.5d0) (< (abs (px n)) 1d-6) (< (abs (py n)) 1d-6))
        (let* ((d1 (rem-dxf 210 (map-points data '(10 11) (lambda (p) (pt (- (px p)) (py p) (pz p))))))
               (type (dxf 0 data)))
          (cond ((equal type "ARC")
                 (let ((a1 (dxf 50 data)) (a2 (dxf 51 data)))
                   (set-dxf 51 (norm-ang (- +pi+ a1)) (set-dxf 50 (norm-ang (- +pi+ a2)) d1))))
                ((equal type "LWPOLYLINE") (map-codes d1 '(42) #'-))
                ((member type '("TEXT" "INSERT") :test #'equal)
                 (set-dxf 50 (norm-ang (- +pi+ (or (dxf 50 data) 0d0))) d1))
                (t d1)))
        (let ((nv (and n (consp n) (pt3 n))))
          (if (or (null nv) (up-normal-p (unit3 nv)) (wcs-type-p (dxf 0 data)))
              (rem-dxf 210 data)
              ;; an object in another plane: keep its plane; the types DXF stores in world
              ;; coordinates are kept in their plane's coordinates like the others
              (wcs-codes-to-ocs (set-dxf 210 (unit3 nv) data)))))))

(defparameter *wcs-coded*
  ;; type -> (point codes . vector codes) that DXF stores in world coordinates
  '(("ELLIPSE" (10) 11) ("MTEXT" (10)) ("SPLINE" (10 11) 12 13) ("LEADER" (10))))

(defun map-vectors (data codes fn)
  (mapcar (lambda (p) (if (and (consp p) (member (car p) codes) (consp (cdr p))) (cons (car p) (funcall fn (pt3 (cdr p)))) p)) data))

(defun wcs-codes-convert (data to-ocs)
  (let ((spec (assoc (dxf 0 data) *wcs-coded* :test #'equal)))
    (if (or (null spec) (not (offplane-p data)))
        data
        (multiple-value-bind (ax ay az) (ocs-axes (entity-normal data))
          (let ((f (if to-ocs (lambda (p) (w->ocs p ax ay az)) (lambda (p) (ocs->w p ax ay az)))))
            (map-vectors (map-vectors data (second spec) f) (cddr spec) f))))))
(defun wcs-codes-to-ocs (data) (wcs-codes-convert data t))
(defun wcs-codes-to-world (data) (wcs-codes-convert data nil))

(defun convert-in (g)
  "Convert one raw DXF entity group to internal alist (radians, world coords)."
  (when (equal (cdr (first g)) "HATCH")
    (return-from convert-in (hatch-from-dxf (clean-group g))))
  (let* ((type (cdr (first g)))
         (data (group-coords (clean-group g) (if (equal type "LWPOLYLINE") '(10) nil))))
    (cond
      ((equal type "ARC")
       (setf data (map-codes data '(50 51) #'rad)))
      ((member type '("TEXT" "INSERT" "ATTDEF" "ATTRIB") :test #'equal)
       (setf data (map-codes data '(50 51) #'rad)))
      ((equal type "DIMENSION")
       ;; rotation (50), oblique extension lines (52) and text rotation (53) are in degrees
       (setf data (map-codes data '(50 52 53) #'rad)))
      ((equal type "LEADER") (setf data (leader-from-dxf g data)))
      ((equal type "MTEXT")
       ;; rotation: the direction vector (11) wins; group 50 is in degrees despite the DXF reference
       (let ((dir (let ((v (dxf 11 data)) (n (dxf 210 data)))
                    (if (and v n (consp n) (not (up-normal-p (unit3 (pt3 n)))) (not (down-normal-p (unit3 (pt3 n)))))
                        (multiple-value-bind (ax ay az) (ocs-axes (unit3 (pt3 n))) (w->ocs (pt3 v) ax ay az))
                        v))))
         (setf data (if dir
                        (append (rem-dxf 50 (rem-dxf 11 data)) (list (cons 50 (ang (pt 0 0) dir))))
                        (map-codes data '(50) #'rad))))
       ;; MTEXT text may be split across 3 groups followed by 1
       (let ((parts (loop for p in data when (and (consp p) (member (car p) '(3 1))) collect (cdr p))))
         (setf data (append (remove-if (lambda (p) (and (consp p) (member (car p) '(3 1)))) data)
                            (list (cons 1 (apply #'concatenate 'string parts))))))))
    (ocs-fix data)))

(defun polyline-from-group (head verts)
  (let* ((flags (or (dxf 70 head) 0))
         (w0 (dxf 40 head)) (w1 (dxf 41 head)))
    (cond ((logtest flags 64) (return-from polyline-from-group (mesh-from-polyface head verts)))
          ((logtest flags 16) (return-from polyline-from-group (mesh-from-polygon-mesh head verts))))
    (when (zerop (logand flags (logior 16 64)))
      (let ((vlist (loop for v in verts
                         for vd = (group-coords (clean-group v))
                         unless (logtest 16 (or (dxf 70 vd) 0)) ; skip spline frame points
                           collect (list (pt3 (dxf 10 vd)) (or (dxf 42 vd) 0d0)))))
        (ocs-fix
         (append (make-lwpoly-data vlist (logtest 1 flags)
                                   :layer (dxf 8 head)
                                   :width (and w0 w1 (= w0 w1) w0))
                 (let ((c (assoc 62 head))) (and c (list c)))
                 (let ((n (assoc 210 (group-coords head)))) (and n (list n)))))))))

(defun convert-entity-list (groups)
  "Turn split entity groups (with POLYLINE/VERTEX/SEQEND and ATTRIBs) into alists."
  (let ((out nil))
    (loop while groups do
      (let* ((g (pop groups)) (type (cdr (first g)))
             (start out)
             ;; paper space objects remember which layout (block record) owns them
             (paper (and (eql (dxf 67 g) 1) (let ((o (dxf 330 g))) (if o (princ-to-string o) "")))))
        (cond
          ((equal type "POLYLINE")
           (let ((verts (loop while (and groups (equal (cdr (first (first groups))) "VERTEX"))
                              collect (pop groups))))
             (when (and groups (equal (cdr (first (first groups))) "SEQEND")) (pop groups))
             (let ((pl (polyline-from-group g verts))) (when pl (push (with-src-handle pl g) out)))))
          ((member type '("SEQEND" "VERTEX") :test #'equal) nil)
          ((equal type "INSERT")
           (push (with-src-handle (rem-dxf 66 (convert-in g)) g) out)
           (loop while (and groups (equal (cdr (first (first groups))) "ATTRIB"))
                 do (let* ((a (convert-in (pop groups))))
                      (unless (logtest 1 (or (dxf 70 a) 0))
                        (push (cons '(0 . "TEXT")
                                    (remove-if (lambda (p) (and (consp p) (member (car p) '(0 2 70 73 74 280 100))))
                                               a))
                              out))))
           (when (and groups (equal (cdr (first (first groups))) "SEQEND")) (pop groups)))
          ((equal type "3DFACE") (push (with-src-handle (mesh-from-3dface (convert-in g)) g) out))
          (t (push (with-src-handle (convert-in g) g) out)))
        (when paper
          (loop for cell on out until (eq cell start)
                do (setf (car cell) (append (car cell) (list (cons :paper paper))))))))
    (nreverse out)))

(defun with-src-handle (data g)
  "Remember the entity's handle in the file (for AutoCAD's draw order table)."
  (let ((h (dxf 5 g)))
    (if (and data h) (append data (list (cons :src-handle (princ-to-string h)))) data)))

(defun handle-number (h)
  (or (ignore-errors (parse-integer (princ-to-string h) :radix 16)) 0))

(defun sortents-map (objects tables)
  "AutoCAD's model-space draw order (SORTENTSTABLE): entity handle -> sort key, or NIL."
  (let* ((ms (find-if (lambda (r) (and (equal (cdr (first r)) "BLOCK_RECORD")
                                       (equalp (dxf 2 r) "*Model_Space")))
                      tables))
         (ms-handle (and ms (princ-to-string (dxf 5 ms))))
         ;; model space's extension dictionary, which owns its draw order table
         (ms-dict (and ms (dxf 360 ms) (princ-to-string (dxf 360 ms)))))
    (dolist (g objects)
      (when (equal (cdr (first g)) "SORTENTSTABLE")
        ;; it belongs to the block record named by the 330 after the AcDbSortentsTable marker; some
        ;; writers (LibreDWG) leave that out, so also accept the table owned by model space's dictionary
        (let* ((after (member-if (lambda (p) (and (consp p) (eql (car p) 100) (equal (cdr p) "AcDbSortentsTable"))) g))
               (block (let ((p (second after))) (and (consp p) (eql (car p) 330) (princ-to-string (cdr p)))))
               (dict (let ((p (find 330 g :key (lambda (x) (and (consp x) (car x)))))) (and p (princ-to-string (cdr p))))))
          (when (or (and block ms-handle (equalp block ms-handle))
                    (and (null block) dict ms-dict (equalp dict ms-dict)))
            (let ((map (make-hash-table :test #'equalp)))
              (loop for (p q) on (rest after)
                    when (and (consp p) (eql (car p) 331) (consp q) (eql (car q) 5))
                      do (setf (gethash (princ-to-string (cdr p)) map) (handle-number (cdr q))))
              (return-from sortents-map map))))))
    nil))

(defun apply-draw-order (datas map)
  "Order converted entities as AutoCAD draws them; objects without a handle stay with the one before."
  (if (null map)
      datas
      (let ((key 0) (i 0))
        (mapcar #'third
                (sort
                 (mapcar (lambda (d)
                           (let ((h (dxf :src-handle d)))
                             (when h (setf key (or (gethash h map) (handle-number h))))
                             (list key (incf i) d)))
                         datas)
                 (lambda (a b) (or (< (first a) (first b))
                                   (and (= (first a) (first b)) (< (second a) (second b))))))))))

(defvar *import-skipped* nil "Object types the last DXF import could not keep: ((type . count) ...).")

(defun block-record-name (tables handle)
  (dxf 2 (find-if (lambda (r) (and (equal (cdr (first r)) "BLOCK_RECORD")
                                   (equalp (princ-to-string (dxf 5 r)) (princ-to-string handle))))
                  tables)))

(defun %dxf-import (path &key (replace t))
  "Load PATH into the current drawing (clearing it first when REPLACE)."
  (let ((*keep-dimension-blocks* t))
    (dxf-import-1 path replace)))

(defun dxf-import-1 (path replace)
  (let* ((pairs (dxf-pairs (read-file-lines path)))
         (tables (split-entities (section-pairs pairs "TABLES")))
         (blocks (split-entities (section-pairs pairs "BLOCKS")))
         (ents (split-entities (section-pairs pairs "ENTITIES")))
         (objects (split-entities (section-pairs pairs "OBJECTS")))
         (paper-blocks nil)
         (count 0))
    (when replace
      (new-drawing)
      (setf (drawing-filename *dwg*) (namestring path)))
    ;; dimension styles
    (dolist (g tables)
      (when (and (equal (cdr (first g)) "DIMSTYLE") (dxf 2 g))
        (let ((vars (standard-dimvars)))
          (loop for (code key) in '((3 :dimpost) (40 :dimscale) (41 :dimasz) (42 :dimexo) (43 :dimdli) (44 :dimexe)
                                    (140 :dimtxt) (141 :dimcen) (142 :dimtsz) (144 :dimlfac) (147 :dimgap)
                                    (73 :dimtih) (77 :dimtad) (271 :dimdec) (176 :dimclrd) (177 :dimclre) (178 :dimclrt))
                for v = (dxf code g)
                when v do (setf (getf vars key) (if (floatp v) (d v) v)))
          ;; arrowhead: 342 points at a BLOCK_RECORD (AutoCAD 2000+); R12 names the block in group 5
          (let* ((h (dxf 342 g))
                 (name (or (and h (dxf 2 (find-if (lambda (r) (and (equal (cdr (first r)) "BLOCK_RECORD")
                                                                   (equalp (princ-to-string (dxf 5 r)) (princ-to-string h))))
                                                  tables)))
                           (let ((n (dxf 5 g))) (and (stringp n) (not (dxf 105 g)) n)))))
            (when (and name (known-arrow-name-p name))
              (setf (getf vars :dimblk) (arrow-code name))))
          (let ((lname (and (dxf 341 g) (block-record-name tables (dxf 341 g)))))
            (when (and lname (known-arrow-name-p lname))
              (setf (getf vars :dimldrblk) (arrow-code lname))))
          (setf (drawing-dimstyles *dwg*)
                (append (remove (dxf 2 g) (drawing-dimstyles *dwg*) :key #'dimstyle-name :test #'string-equal)
                        (list (make-dimstyle :name (dxf 2 g) :vars vars)))))))
    ;; drawing settings kept in the header (units, linetype scale ...)
    (import-header-vars (section-pairs pairs "HEADER"))
    ;; the current dimension style
    (let* ((hdr (section-pairs pairs "HEADER"))
           (pos (position "$DIMSTYLE" hdr :key #'cdr :test #'equal))
           (ds (and pos (find-dimstyle (cdr (nth (1+ pos) hdr))))))
      (when ds (apply-dimvars (dimstyle-vars ds)) (setf (sysvar "DIMSTYLE") (dimstyle-name ds))))
    ;; linetypes
    (dolist (g tables)
      (when (equal (cdr (first g)) "LTYPE")
        (let ((name (dxf 2 g)))
          (unless (or (null name) (continuous-name-p name))
            (add-ltype (make-ltype :name name :desc (or (dxf 3 g) "")
                                   :pattern (loop for p in g when (eql (car p) 49) collect (d (cdr p)))))))))
    ;; text styles
    (dolist (g tables)
      (when (equal (cdr (first g)) "STYLE") (import-style-record g)))
    ;; layers
    (dolist (g tables)
      (when (equal (cdr (first g)) "LAYER")
        (let* ((name (dxf 2 g)) (c (or (dxf 62 g) 7)) (flags (or (dxf 70 g) 0)))
          (when name
            (let ((l (ensure-layer name)))
              (setf (layer-color l) (max 1 (abs c)) (layer-on l) (>= c 0)
                    (layer-frozen l) (logtest 1 flags) (layer-locked l) (logtest 4 flags)
                    (layer-ltype l) (or (dxf 6 g) "CONTINUOUS")
                    (layer-lineweight l) (or (dxf 370 g) -3)
                    (layer-plot l) (not (eql (dxf 290 g) 0))))))))
    ;; the layer that was current when the drawing was saved
    (let* ((hdr (section-pairs pairs "HEADER"))
           (pos (position "$CLAYER" hdr :key #'cdr :test #'equal))
           (l (and pos (stringp (cdr (nth (1+ pos) hdr))) (find-layer (cdr (nth (1+ pos) hdr))))))
      (when (and l (not (layer-frozen l))) (setf (sysvar "CLAYER") (layer-name l))))
    ;; blocks (a layout's objects, other than the active layout's, are kept in its *Paper_Space block)
    (let ((cur nil) (body nil) (cur-br nil))
      (dolist (g blocks)
        (let ((type (cdr (first g))))
          (cond ((equal type "BLOCK")
                 (let ((gd (group-coords g)))
                   (setf cur (make-block-def :name (dxf 2 gd) :base (pt3 (or (dxf 10 gd) (pt 0 0))))
                         cur-br (and (dxf 330 g) (princ-to-string (dxf 330 g)))
                         body nil)))
                ((equal type "ENDBLK")
                 (when (and cur (stringp (block-def-name cur)) (search "*Paper_Space" (block-def-name cur) :test #'char-equal)
                            (not (string-equal (block-def-name cur) "*Paper_Space")))
                   (setf paper-blocks
                         (append paper-blocks
                                 (mapcar (lambda (x) (append (rem-dxf :paper (rem-dxf :src-handle x)) (list (cons :paper (or cur-br "")))))
                                         (convert-entity-list (nreverse body))))))
                 (when (and cur (not (member (block-def-name cur) '("*Model_Space" "*Paper_Space" "$MODEL_SPACE" "$PAPER_SPACE")
                                             :test (lambda (a b) (and (stringp a) (search b a :test #'char-equal))))))
                   (setf (block-def-entities cur) (mapcar (lambda (x) (rem-dxf :src-handle x))
                                                          (convert-entity-list (nreverse body))))
                   (push cur (drawing-blocks *dwg*)))
                 (setf cur nil body nil))
                (t (push g body))))))
    ;; entities (anything Fancy Pencil can't keep is counted, so opening can say so)
    (setf *import-skipped* nil)
    ;; the Model tab's viewports (VPORTS)
    (when replace (funcall 'import-tiles tables))
    ;; layouts (paper space): their names, sheets and the block records that own their objects
    (let ((space-of (funcall 'import-layouts objects tables replace)))
     (dolist (data (append (mapcar (lambda (x) (rem-dxf :src-handle x))
                                   (apply-draw-order (convert-entity-list ents) (sortents-map objects tables)))
                           paper-blocks))
      (when (equal (dxf 0 data) "3DFACE") (setf data (mesh-from-3dface data)))
      (let ((owner (dxf :paper data)))
        (setf data (rem-dxf :paper data))
        (when (and owner (equal (dxf 0 data) "VIEWPORT")) (setf data (funcall 'vp-from-dxf data)))
        (setf data (and data (append data (list (cons :space (and owner (funcall space-of owner))))))))
      (if (and data
               (member (dxf 0 data) '("LINE" "CIRCLE" "ARC" "POINT" "LWPOLYLINE" "TEXT" "MTEXT" "LEADER"
                                      "INSERT" "DIMENSION" "ATTDEF" "ELLIPSE" "SPLINE" "SOLID" "HATCH" "POLYFACE" "VIEWPORT")
                       :test #'equal))
          (progn
            (when (and (equal (dxf 0 data) "LEADER") (dxf :ldr-blk-handle data))
              (let ((name (block-record-name tables (dxf :ldr-blk-handle data))))
                (setf data (rem-dxf :ldr-blk-handle data))
                (when (and name (known-arrow-name-p name))
                  (setf data (append data (list (cons :ldr-blk (arrow-code name))))))))
            (let ((*space-override* (dxf :space data))) (add-entity (rem-dxf :space data)))
            (incf count))
          (when data
            (let ((c (assoc (dxf 0 data) *import-skipped* :test #'equal)))
              (if c (incf (cdr c)) (push (cons (dxf 0 data) 1) *import-skipped*)))))))
    (setf (drawing-saved-modcount *dwg*) (drawing-modcount *dwg*))
    count))

;;; ---------- writer ----------

(defun fmt-real (x)
  (let* ((x (d x))
         (s (format nil "~,10F" (if (< (abs x) 1d-11) 0d0 x))))
    (let ((dot (position #\. s)))
      (if dot
          (let ((end (length s)))
            (loop while (and (> end (+ dot 2)) (char= (char s (1- end)) #\0)) do (decf end))
            (subseq s 0 end))
          s))))

(defun wr (out code val)
  (format out "~3D~%" code)
  (cond ((floatp val) (format out "~A~%" (fmt-real val)))
        ((integerp val) (format out "~6D~%" val))
        (t (format out "~A~%" val)))
  ;; paper space objects (R12): every entity record says so
  (when (and *write-paper* (eql code 0)) (format out " 67~%     1~%")))
(defun wr-pt (out code p)
  (wr out code (px p)) (wr out (+ code 10) (py p)) (wr out (+ code 20) (pz p)))

(defun angle-codes-for (type)
  (cond ((equal type "ARC") '(50 51))
        ((member type '("TEXT" "INSERT" "ATTDEF" "ATTRIB" "MTEXT") :test #'equal) '(50 51))
        ((equal type "DIMENSION") '(50 52 53))
        (t nil)))

(defun write-entity (out data)
  (let ((type (dxf 0 data)))
    (cond
      ((equal type "POLYFACE") (write-mesh-r12 out data))
      ((offplane-p data)                    ; R12: an object in another plane goes out as its edges
       (dolist (pc (offplane-pieces data))
         (when (member (first pc) '(:seg :gseg))
           (write-entity out (append (list (cons 0 "LINE") (cons 8 (or (dxf 8 data) "0")) (cons 10 (second pc)) (cons 11 (third pc)))
                                     (let ((c (assoc 62 data))) (and c (list c))))))))
      ((equal type "LWPOLYLINE")
       (wr out 0 "POLYLINE") (wr out 8 (dxf 8 data))
       (when (dxf 62 data) (wr out 62 (dxf 62 data)))
       (wr out 66 1) (wr-pt out 10 (pt 0 0)) (wr out 70 (if (lwpoly-closed-p data) 1 0))
       (when (dxf 43 data) (wr out 40 (dxf 43 data)) (wr out 41 (dxf 43 data)))
       (loop for (p b) in (lwpoly-vertices data)
             do (wr out 0 "VERTEX") (wr out 8 (dxf 8 data)) (wr-pt out 10 p)
                (unless (zerop b) (wr out 42 b)))
       (wr out 0 "SEQEND") (wr out 8 (dxf 8 data)))
      ((equal type "MTEXT")
       (let* ((s (text-string-display (or (dxf 1 data) "")))
              (h (or (dxf 40 data) 0.2d0)) (rot (or (dxf 50 data) 0d0))
              (p (pt3 (dxf 10 data)))
              (lines (loop with start = 0 for pos = (position #\Newline s :start start)
                           collect (subseq s start pos) while pos do (setf start (1+ pos)))))
         (loop for line in lines for i from 1
               do (write-entity out (list (cons 0 "TEXT") (cons 8 (dxf 8 data))
                                          (cons 10 (polar p (- rot (/ +pi+ 2)) (* i h 1.6d0)))
                                          (cons 40 h) (cons 1 line) (cons 50 rot))))))
      ((equal type "LEADER")                                ; no R12 equivalent: its lines and arrowhead
       (dolist (sub (leader-display-entities data))
         (write-entity out (append (rem-dxf 62 (rem-dxf 8 sub)) (list (cons 8 (or (dxf 8 data) "0")))
                                   (let ((c (assoc 62 data))) (and c (list c)))))))
      ((member type '("ELLIPSE" "SPLINE") :test #'equal)    ; no R12 equivalent: write a polyline
       (let ((pts (if (equal type "ELLIPSE") (ellipse-points data) (spline-points data))))
         (when (> (length pts) 1)
           (write-entity out (append (make-lwpoly-data pts nil :layer (dxf 8 data))
                                     (let ((c (assoc 62 data))) (and c (list c))))))))
      ((not (member type '("LINE" "CIRCLE" "ARC" "POINT" "TEXT" "INSERT" "SOLID" "DIMENSION" "ATTDEF" "3DFACE" "TRACE")
                    :test #'equal))
       nil)                                ; HATCH, WIPEOUT, IMAGE ... have no R12 form
      (t
       (let ((acodes (angle-codes-for type)))
         (wr out 0 type)
         (dolist (pair (rest data))
           (when (consp pair)
             (let ((code (car pair)) (v (cdr pair)))
               (cond ((not (integerp code)) nil)          ; internal keys (:dimvars, :loops ...)
                     ((member code '(-1 0 5 100 102 330 360 410 67 210)) nil)
                     ((and (consp v) (realp (car v))) (wr-pt out code v))
                     ((member code acodes) (wr out code (deg v)))
                     ((and (equal type "TEXT") (= code 73) (eql v 0)) nil)
                     ((and (equal type "DIMENSION") (= code 3)) nil)
                     ((or (stringp v) (realp v)) (wr out code v)))))))))))

(defun %dxf-export (path)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (let ((ext (or (drawing-extents) (list 0d0 0d0 12d0 9d0))))
      (wr out 0 "SECTION") (wr out 2 "HEADER")
      (wr out 9 "$ACADVER") (wr out 1 "AC1009")
      (wr out 9 "$INSBASE") (wr-pt out 10 (pt 0 0))
      (wr out 9 "$EXTMIN") (wr-pt out 10 (pt (first ext) (second ext)))
      (wr out 9 "$EXTMAX") (wr-pt out 10 (pt (third ext) (fourth ext)))
      (wr out 9 "$CLAYER") (wr out 8 (sysvar "CLAYER"))
      (wr out 9 "$TEXTSIZE") (wr out 40 (d (sysvar "TEXTSIZE")))
      (dolist (h (header-var-pairs))     ; R12 knows all of these except the insertion units
        (unless (member (first h) '("$TEXTSIZE" "$INSUNITS" "$MEASUREMENT" "$CELTSCALE") :test #'string-equal)
          (wr out 9 (first h)) (wr out (second h) (third h))))
      (wr out 0 "ENDSEC")
      ;; tables
      (wr out 0 "SECTION") (wr out 2 "TABLES")
      (let ((ltypes (remove-duplicates
                     (remove-if (lambda (n) (or (not (stringp n)) (member n '("CONTINUOUS" "BYLAYER" "BYBLOCK") :test #'string-equal)))
                                (append (mapcar #'layer-ltype (drawing-layers *dwg*))
                                        (mapcar #'ltype-name (drawing-ltypes *dwg*))
                                        (mapcar (lambda (e) (dxf 6 (ename-data e))) (all-live-entities))
                                        (loop for b in (drawing-blocks *dwg*) append (mapcar (lambda (dd) (dxf 6 dd)) (block-def-entities b)))))
                     :test #'string-equal)))
        (wr out 0 "TABLE") (wr out 2 "LTYPE") (wr out 70 (1+ (length ltypes)))
        (dolist (n (cons "CONTINUOUS" ltypes))
          (let* ((lt (or (find-ltype n) (library-linetype n)))
                 (pat (and lt (ltype-pattern lt))))
            (wr out 0 "LTYPE") (wr out 2 n) (wr out 70 0)
            (wr out 3 (cond ((string= n "CONTINUOUS") "Solid line") (lt (ltype-desc lt)) (t n)))
            (wr out 72 65) (wr out 73 (length pat)) (wr out 40 (d (reduce #'+ (mapcar #'abs pat))))
            (dolist (x pat) (wr out 49 (d x)))))
        (wr out 0 "ENDTAB"))
      (wr out 0 "TABLE") (wr out 2 "LAYER") (wr out 70 (length (drawing-layers *dwg*)))
      (dolist (l (drawing-layers *dwg*))
        (wr out 0 "LAYER") (wr out 2 (layer-name l))
        (wr out 70 (logior (if (layer-frozen l) 1 0) (if (layer-locked l) 4 0)))
        (wr out 62 (* (if (layer-on l) 1 -1) (abs (layer-color l))))
        (wr out 6 (or (layer-ltype l) "CONTINUOUS")))
      (wr out 0 "ENDTAB")
      (let ((styles (styles-for-export)))
        (wr out 0 "TABLE") (wr out 2 "STYLE") (wr out 70 (length styles))
        (dolist (st styles)
          (wr out 0 "STYLE") (wr out 2 (textstyle-name st)) (wr out 70 0) (wr out 40 (textstyle-height st))
          (wr out 41 (textstyle-width st)) (wr out 50 (* (textstyle-oblique st) (/ 180 pi)))
          (wr out 71 (style-gen-flags st)) (wr out 42 0.2d0) (wr out 3 (style-font-file st))
          (wr out 4 (textstyle-bigfont st)))
        (wr out 0 "ENDTAB"))
      (let ((dstyles (all-dimstyles)))
        (wr out 0 "TABLE") (wr out 2 "DIMSTYLE") (wr out 70 (length dstyles))
        (dolist (ds dstyles)
          (wr out 0 "DIMSTYLE") (wr out 2 (dimstyle-name ds)) (wr out 70 0)
          (loop for (code . val) in (dimstyle-vars-for-dxf (dimstyle-vars ds)) do (wr out code val))
          (let ((name (arrow-block-name (dv (dimstyle-vars ds) :dimblk)))) (when name (wr out 5 name))))
        (wr out 0 "ENDTAB"))
      (wr out 0 "ENDSEC")
      ;; blocks
      (wr out 0 "SECTION") (wr out 2 "BLOCKS")
      ;; arrowhead blocks the dimension styles name
      (dolist (code (arrow-codes-in-use))
        (let ((name (arrow-block-name code)))
          (when (and name (not (find-block name)))
            (wr out 0 "BLOCK") (wr out 8 "0") (wr out 2 name) (wr out 70 0) (wr-pt out 10 (pt 0 0)) (wr out 3 name)
            (dolist (dd (arrow-block-entities code)) (write-entity out (append dd (list (cons 8 "0")))))
            (wr out 0 "ENDBLK") (wr out 8 "0"))))
      (dolist (b (reverse (drawing-blocks *dwg*)))
        (wr out 0 "BLOCK") (wr out 8 "0") (wr out 2 (block-def-name b))
        (wr out 70 (if (char= (char (block-def-name b) 0) #\*) 1 0))
        (wr-pt out 10 (block-def-base b)) (wr out 3 (block-def-name b))
        (dolist (e (block-def-entities b)) (write-entity out e))
        (wr out 0 "ENDBLK") (wr out 8 "0"))
      (wr out 0 "ENDSEC")
      ;; entities
      (wr out 0 "SECTION") (wr out 2 "ENTITIES")
      (dolist (e (live-entities)) (write-entity out (ename-data e)))
      ;; R12 has one paper space: the layout showing (or the first), without its viewports
      (let ((*write-paper* t))
        (dolist (e (funcall 'layout-entities (or (drawing-ctab *dwg*) (layout-name (first (drawing-layouts *dwg*))))))
          (unless (equal (ent-type e) "VIEWPORT") (write-entity out (ename-data e)))))
      (wr out 0 "ENDSEC")
      (wr out 0 "EOF")))
  (setf (drawing-filename *dwg*) (namestring path)
        (drawing-saved-modcount *dwg*) (drawing-modcount *dwg*))
  path)

;;; ---------- header variables ----------

(defparameter *header-vars*
  '(("LUNITS" :int) ("LUPREC" :int) ("AUNITS" :int) ("AUPREC" :int) ("INSUNITS" :int)
    ("MEASUREMENT" :int) ("LTSCALE" :real) ("CELTSCALE" :real) ("TEXTSIZE" :real)
    ("PDMODE" :int) ("PDSIZE" :real) ("FILLETRAD" :real) ("ANGBASE" :angle) ("ANGDIR" :int)
    ("PLINEWID" :real) ("LIMCHECK" :int) ("LIMMIN" :point) ("LIMMAX" :point))
  "Drawing settings read from and written to the DXF HEADER section.")

(defun import-header-vars (hdr)
  "Set the sysvars in *HEADER-VARS* from the HEADER pairs ((9 . \"$NAME\") (code . value) ...)."
  (loop for (p . rest) on hdr
        when (and (eql (car p) 9) (stringp (cdr p)) rest)
          do (let* ((name (string-left-trim "$" (cdr p)))
                    (spec (assoc name *header-vars* :test #'string-equal))
                    (v (cdr (first rest))))
               (when spec
                 (ignore-errors
                  (let ((val (ecase (second spec)
                               (:int (if (realp v) (round v) (parse-integer (string-trim " " v))))
                               (:real (d (if (realp v) v (parse-number-text v))))
                               (:angle (* (d (if (realp v) v (parse-number-text v))) (/ pi 180)))
                               (:point (let ((y (cdr (second rest)))) (list (d v) (d y)))))))
                    (when (and (eq (second spec) :real) (string-equal name "LTSCALE") (<= val 0)) (setf val 1d0))
                    (setf (sysvar (string-upcase name)) val)))))))

(defun header-var-pairs ()
  "(name code value) for writing the header."
  (loop for (name kind) in *header-vars*
        for v = (sysvar name)
        when (and (realp v) (not (eq kind :point)))
          collect (list (concatenate 'string "$" name)
                        (if (eq kind :int) 70 (if (string-equal name "LWDISPLAY") 290 40))
                        (case kind (:int (round v)) (:angle (* (d v) (/ 180 pi))) (t (d v))))))

;; files always hold world coordinates, whatever view is showing
(defun dxf-import (path &key (replace t)) (let ((*ucs-off* t)) (%dxf-import path :replace replace)))
(defun dxf-export (path)
  (let ((*ucs-off* t) (*space-override* nil)) (ignore-errors (prune-unused-dimension-blocks)) (%dxf-export path)))
