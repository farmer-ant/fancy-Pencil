;;;; underlay.lisp — DWF and DGN underlays: another file's drawing shown in this one, as AutoCAD's
;;;; DWFATTACH / DGNATTACH make (DWFUNDERLAY / DGNUNDERLAY with DWFDEFINITION / DGNDEFINITION).
;;;; Kept as (0 . "UNDERLAY") with (:kind . :dwf / :dgn), (:file . path), (:sheet . name), its
;;;; insertion point (10), scale (41), rotation (50, radians), fade (:fade) and the AutoCAD fields
;;;; it came with (:clip, :flags, :contrast) so they go back out unchanged. What it shows is read
;;;; from the file (dwf.lisp, dgn.lisp) and drawn faded, inside the underlay's frame; when the file
;;;; can't be read (missing, a classic DWF, a DGN V8) the frame and the file's name show instead.

(in-package :fancy-pencil)

(defvar *underlay-cache* (make-hash-table :test #'equal)
  "(file sheet) -> (entities width height note), the file's drawing in underlay units.")

(defun underlay-content (file sheet kind)
  "What FILE (sheet SHEET) shows: (values entities lower-left upper-right note), in underlay units."
  (let* ((key (list file sheet))
         (c (or (gethash key *underlay-cache*)
                (setf (gethash key *underlay-cache*)
                      (handler-case
                          (if (not (probe-file file))
                              (list nil nil nil "file not found")
                              (ecase kind
                                (:dwf (let* ((sheets (dwf-sheets file))
                                             (s (or (find sheet sheets :key #'first :test #'equalp) (first sheets))))
                                        (cond ((null s) (list nil nil nil "not a DWF file"))
                                              ((null (second s))
                                               (list nil (pt 0 0) (pt (third s) (fourth s)) "classic DWF: frame only"))
                                              (t (list (second s) (pt 0 0) (pt (third s) (fourth s)) nil)))))
                                (:dgn (multiple-value-bind (ents k) (read-dgn file)
                                        (case k
                                          (:v8 (list nil nil nil "DGN V8: frame only"))
                                          (:v7 (let ((b (entities-data-bbox ents)))
                                                 (list ents (and b (pt (first b) (second b))) (and b (pt (third b) (fourth b)))
                                                       (and (null ents) "empty"))))
                                          (t (list nil nil nil "not a DGN file")))))))
                        (error (e) (list nil nil nil (format nil "can't be read: ~A" (cl-error-text e)))))))))
    (values (first c) (second c) (third c) (fourth c))))

(defun entities-data-bbox (ents)
  (let (x0 y0 x1 y1)
    (dolist (e ents)
      (let ((b (ignore-errors (pieces-bbox (data-pieces e)))))
        (when b
          (setf x0 (if x0 (min x0 (first b)) (first b)) y0 (if y0 (min y0 (second b)) (second b))
                x1 (if x1 (max x1 (third b)) (third b)) y1 (if y1 (max y1 (fourth b)) (fourth b))))))
    (and x0 (list x0 y0 x1 y1))))

(defun underlay-xf (data)
  "The transform from underlay units to the drawing: scale, then rotate, then move to the insertion point."
  (let* ((ins (pt3 (or (dxf 10 data) (pt 0 0)))) (s (d (or (dxf 41 data) 1))) (a (d (or (dxf 50 data) 0)))
         (c (cos a)) (sn (sin a)))
    (make-xf :fn (lambda (p) (let ((p (pt3 p)))
                               (pt (+ (px ins) (* s (- (* c (px p)) (* sn (py p)))))
                                   (+ (py ins) (* s (+ (* sn (px p)) (* c (py p)))))
                                   (+ (pz ins) (* s (pz p))))))
             :ang (lambda (r) (+ r a)) :scale s)))

(defun underlay-frame (data)
  "The underlay's frame corners in the drawing, or NIL."
  (multiple-value-bind (ents lo hi) (underlay-content (dxf :file data) (dxf :sheet data) (dxf :kind data))
    (declare (ignore ents))
    (let ((lo (or lo (pt 0 0))) (hi (or hi (pt 1 1))) (fp (xf-fn (underlay-xf data))))
      (mapcar fp (list lo (pt (px hi) (py lo)) hi (pt (px lo) (py hi)))))))

(defun underlay-display-entities (data)
  "The underlay's drawing (faded) and frame, as entities in the drawing."
  (multiple-value-bind (ents lo hi note) (underlay-content (dxf :file data) (dxf :sheet data) (dxf :kind data))
    (declare (ignore lo hi))
    (let* ((xf (underlay-xf data)) (layer (or (dxf 8 data) "0"))
           (fade-color (let ((c (dxf 62 data))) (if (and c (plusp c) (< c 256)) c nil)))
           (frame (underlay-frame data))
           (out (list (list (cons 0 "LWPOLYLINE") (cons 70 1) (cons 8 layer) (cons 62 8)
                            (cons 10 (first frame)) (cons 10 (second frame)) (cons 10 (third frame)) (cons 10 (fourth frame))))))
      (unless (dxf :hidden data)
        (dolist (e ents)
          (let ((tx (xform-data e xf)))
            (push (append (rem-dxf 62 (rem-dxf 8 tx)) (list (cons 8 layer) (cons 62 (or fade-color (dxf 62 e) 8)))) out))))
      (when note
        (let* ((f (first frame)) (h (max 1d-6 (* 0.04d0 (dist2d (first frame) (second frame))))))
          (push (list (cons 0 "TEXT") (cons 8 layer) (cons 62 8) (cons 10 (v+ f (pt h h)))
                      (cons 40 h) (cons 1 (format nil "~A (~A)" (file-namestring (dxf :file data)) note))
                      (cons 50 (d (or (dxf 50 data) 0))) (cons 7 "Standard"))
                out)))
      (nreverse out))))

(defun underlay-pieces (data)
  "Picked and boxed by its frame, as AutoCAD's underlays are."
  (let ((f (underlay-frame data)))
    (loop for (a b) on (append f (list (first f))) while b collect (list :seg a b))))

(defun xform-underlay (data xf)
  (let* ((fp (xf-fn xf)) (s (d (or (xf-scale xf) 1d0)))
         (ins (funcall fp (pt3 (or (dxf 10 data) (pt 0 0)))))
         (a (funcall (xf-ang xf) (d (or (dxf 50 data) 0)))))
    (set-dxf 50 a (set-dxf 41 (* s (d (or (dxf 41 data) 1))) (set-dxf 10 ins data)))))

;;; ---------- reading from DXF ----------

(defun underlay-defs-from-objects (objects)
  "DWFDEFINITION and DGNDEFINITION objects: handle -> (file sheet kind)."
  (let ((h (make-hash-table :test #'equalp)))
    (dolist (g objects h)
      (let ((type (cdr (first g))))
        (when (member type '("DWFDEFINITION" "DGNDEFINITION") :test #'equal)
          (setf (gethash (princ-to-string (dxf 5 g)) h)
                (list (dxf 1 g) (and (dxf 2 g) (princ-to-string (dxf 2 g)))
                      (if (equal type "DWFDEFINITION") :dwf :dgn))))))))

(defun underlay-from-dxf (data defs base-dir)
  (let* ((def (gethash (princ-to-string (or (dxf 340 data) "")) defs)))
    (when def
      (let ((clip (loop for p in data when (and (consp p) (eql (car p) 11)) collect (cdr p))))
        (append (list (cons 0 "UNDERLAY") (cons :kind (third def))
                      (cons :file (resolve-attached-path (first def) base-dir)) (cons :sheet (second def))
                      (cons 10 (pt3 (or (dxf 10 data) (pt 0 0)))) (cons 41 (d (or (dxf 41 data) 1)))
                      (cons 50 (rad (d (or (dxf 50 data) 0)))))
                (let ((f (dxf 282 data))) (and f (list (cons :fade f))))
                (list (cons :flags (or (dxf 280 data) 2)) (cons :contrast (or (dxf 281 data) 100)))
                (and (eql (logand (or (dxf 280 data) 2) 2) 0) (list (cons :hidden t)))
                (and clip (list (cons :clip clip)))
                (let ((l (dxf 8 data))) (and l (list (cons 8 l))))
                (let ((c (assoc 62 data))) (and c (list c))))))))

;;; ---------- writing DXF 2000 ----------


(defun underlay-def-2000 (kind file sheet)
  (or (find-if (lambda (x) (and (eq (first x) kind) (equal (second x) file) (equal (third x) sheet))) *underlay-defs-2000*)
      (let ((x (list kind file sheet (h2))))
        (setf *underlay-defs-2000* (append *underlay-defs-2000* (list x)))
        x)))

(defun drawing-has-underlays-p ()
  (some (lambda (e) (equal (ent-type e) "UNDERLAY")) (all-live-entities)))

(defun write-underlay-2000 (out data owner handle)
  (let* ((kind (dxf :kind data)) (def (underlay-def-2000 kind (dxf :file data) (dxf :sheet data))))
    (w out 0 (if (eq kind :dwf) "DWFUNDERLAY" "DGNUNDERLAY")) (w out 5 handle) (w out 330 owner) (w out 100 "AcDbEntity")
    (when *write-paper* (w out 67 1))
    (w out 8 (or (dxf 8 data) "0"))
    (let ((c (dxf 62 data))) (when c (w out 62 c)))
    (w out 100 "AcDbUnderlayReference") (w out 340 (fourth def))
    (wp out 10 (pt3 (dxf 10 data)))
    (let ((s (d (or (dxf 41 data) 1)))) (w out 41 s) (w out 42 s) (w out 43 s))
    (w out 50 (deg (d (or (dxf 50 data) 0))))
    (wp out 210 (pt 0 0 1))
    (w out 280 (if (dxf :hidden data) (logandc2 (or (dxf :flags data) 2) 2) (logior (or (dxf :flags data) 2) 2)))
    (w out 281 (or (dxf :contrast data) 100)) (w out 282 (min 80 (or (dxf :fade data) 0)))
    (let ((clip (dxf :clip data)))
      (when clip
        (w out 91 (length clip))
        (dolist (p clip) (wp out 11 (pt (px p) (py p)) t))))))

(defun write-underlay-classes-2000 (out)
  (dolist (c '(("DWFDEFINITION" "AcDbDwfDefinition") ("DWFUNDERLAY" "AcDbDwfReference")
               ("DGNDEFINITION" "AcDbDgnDefinition") ("DGNUNDERLAY" "AcDbDgnReference")))
    (destructuring-bind (name cls) c
      (let ((ent (search "UNDERLAY" name)))
        (w out 0 "CLASS") (w out 1 name) (w out 2 cls) (w out 3 "ObjectDBX Classes")
        (w out 90 (if ent 4095 1153)) (w out 280 0) (w out 281 (if ent 1 0))))))

(defun underlay-dict-entries-2000 ()
  "Root dictionary handles for the DWF and DGN definition dictionaries: (values dwf-dict dgn-dict)."
  (values (and (some (lambda (x) (eq (first x) :dwf)) *underlay-defs-2000*) (h2))
          (and (some (lambda (x) (eq (first x) :dgn)) *underlay-defs-2000*) (h2))))

(defun write-underlay-objects-2000 (out root dwf-dict dgn-dict)
  (let ((used nil))
    (loop for (kind dict) in (list (list :dwf dwf-dict) (list :dgn dgn-dict))
          when dict
            do (w out 0 "DICTIONARY") (w out 5 dict) (w out 330 root) (w out 100 "AcDbDictionary") (w out 281 1)
               (dolist (x *underlay-defs-2000*)
                 (when (eq (first x) kind)
                   (let ((n (def-name (second x) nil used))) (push n used)
                     (w out 3 (format nil "~A - ~A" n (or (third x) "Sheet"))) (w out 350 (fourth x)))))
               (dolist (x *underlay-defs-2000*)
                 (when (eq (first x) kind)
                   (w out 0 (if (eq kind :dwf) "DWFDEFINITION" "DGNDEFINITION")) (w out 5 (fourth x))
                   (w out 102 "{ACAD_REACTORS") (w out 330 dict) (w out 102 "}")
                   (w out 330 dict) (w out 100 "AcDbUnderlayDefinition")
                   (w out 1 (dxf-escape (second x))) (w out 2 (or (third x) (if (eq kind :dwf) "Sheet 1" "Default"))))))))

;;; ---------- attaching ----------

(defun attach-underlay (kind)
  (let* ((f (ask-file nil (if (eq kind :dwf) "Select DWF File" "Select DGN File") nil
                      (if (eq kind :dwf) "dwf dwfx" "dgn"))))
    (when (and f (probe-file f))
      (let* ((f (namestring (truename f)))
             (sheet (if (eq kind :dwf)
                        (let ((sheets (ignore-errors (dwf-sheets f))))
                          (if (> (length sheets) 1)
                              (let ((s (gstring (format nil "Sheet [~{~A~^/~}] <~A>: " (mapcar #'first sheets) (first (first sheets))) t)))
                                (if (plusp (length s)) s (first (first sheets))))
                              (first (first sheets))))
                        "Default")))
        (multiple-value-bind (ents lo hi note) (underlay-content f sheet kind)
          (declare (ignore lo hi))
          (when note (out (format nil "~A: ~A.~%" (file-namestring f) note)))
          (out (format nil "~D object~:P to show.~%" (length ents))))
        (let ((p (get-user-input :point :prompt "Specify insertion point: ")))
          (when (pointp p)
            (let* ((s (or (get-user-input :distance :base p :prompt "Specify scale factor <1>: ") 1d0))
                   (r (or (get-user-input :angle :base p :prompt "Specify rotation <0>: ") 0d0)))
              (add-entity (list (cons 0 "UNDERLAY") (cons :kind kind) (cons :file f) (cons :sheet sheet)
                                (cons 10 (pt3 p)) (cons 41 (d s)) (cons 50 (d r)) (cons :fade 25)
                                (cons :flags 2) (cons :contrast 100)))
              (out (format nil "~A attached.~%" (file-namestring f))))))))))

(defcmd "DWFATTACH" ("-DWFATTACH") "Attaches a DWF or DWFx file as an underlay." (attach-underlay :dwf))
(defcmd "DGNATTACH" ("-DGNATTACH") "Attaches a MicroStation DGN file as an underlay." (attach-underlay :dgn))
