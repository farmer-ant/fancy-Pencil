;;;; db.lisp — the drawing database.
;;;; Every entity is stored as a DXF association list exactly like the one
;;;; AutoLISP's ENTGET returns (minus the (-1 . ename) pair), so ENTGET/ENTMOD
;;;; are nearly free. Angles inside the database are radians (as in AutoLISP);
;;;; the DXF reader/writer converts to and from degrees.

(in-package :fancy-pencil)

(defstruct (ename (:constructor %make-ename (handle data)))
  handle data (deleted nil))

(defstruct layer
  name (color 7) (ltype "CONTINUOUS") (on t) (frozen nil) (locked nil) (lineweight -3)
  (plot t))              ; NIL: a no-plot layer (shown on screen, left out of plots)

(defstruct ltype name (desc "") (pattern nil))

(defstruct block-def name (base (pt 0 0)) (entities nil))

(defstruct layout
  name
  (paper "Tabloid / ANSI B (11 x 17 in)") (landscape t)
  (units nil)         ; :in or :mm (NIL = from the drawing's units, the first time it is needed)
  pview)              ; the paper view last used on this tab: (vx vy vscale)

(defstruct tile
  rect                ; (x0 y0 x1 y1) as fractions of the drawing area, y0 at the top
  view (vx 6d0) (vy 4.5d0) (vscale 60d0) (vstyle "Wireframe"))

(defstruct drawing
  (entities (make-array 64 :adjustable t :fill-pointer 0))
  (layouts (list (make-layout :name "Layout1")))   ; paper space layouts, in tab order
  (ctab nil)             ; the layout tab showing (NIL = Model)
  (active-vp nil)        ; the viewport being worked in from a layout (MSPACE), or NIL
  (model-view nil)       ; the Model tab's view while a layout is showing
  (tiles nil)            ; the Model tab split into viewports (VPORTS): tile structs, or NIL for one
  (active-tile 0)
  (reordered nil)        ; true once DRAWORDER has changed the order of the list
  (next-handle #x30)
  (layers (list (make-layer :name "0")))
  (blocks nil)
  (ltypes nil)
  (dimstyles nil)
  (styles nil)
  (modcount 0)
  (saved-modcount 0)
  (filename nil)
  (skipped nil)          ; object types the file had that Fancy Pencil couldn't keep: ((type . n) ...)
  (undo nil) (redo nil))

(defvar *dwg* (make-drawing))
(defvar *sysvars* (make-hash-table :test #'equalp))

(defun dxf (code data) (cdr (assoc code data)))
(defun set-dxf (code val data)
  (if (assoc code data)
      (let ((done nil))
        (mapcar (lambda (pair)
                  (if (and (not done) (consp pair) (eql (car pair) code))
                      (progn (setf done t) (cons code val))
                      pair))
                data))
      (append data (list (cons code val)))))
(defun rem-dxf (code data) (remove code data :key (lambda (x) (and (consp x) (car x)))))

(defun touch () (incf (drawing-modcount *dwg*)))

(defun sysvar (name) (gethash name *sysvars*))
(defun (setf sysvar) (val name) (setf (gethash name *sysvars*) val))

(defun reset-sysvars ()
  (clrhash *sysvars*)
  (loop for (k v) on
        (list "CLAYER" "0" "CECOLOR" "BYLAYER" "OSMODE" 4133 "ORTHOMODE" 0
              "SNAPMODE" 0 "GRIDMODE" 0 "SNAPUNIT" (list 0.5d0 0.5d0) "GRIDUNIT" (list 0.5d0 0.5d0)
              "TEXTSIZE" 0.2d0 "ROLLOVERTIPS" 1 "SNAPSTYL" 0 "SNAPISOPAIR" 1 "THICKNESS" 0d0 "ELEVATION" 0d0 "CURSORCOLOR" "#c9ced6" "BKGCOLOR" "#212830" "AUTOSNAPSIZE" 6 "LUPREC" 4 "AUPREC" 0 "LUNITS" 2 "AUNITS" 0
              "CMDECHO" 1 "PICKBOX" 5 "APERTURE" 10 "PDMODE" 0 "PDSIZE" 0d0
              "FILLETRAD" 0d0 "OFFSETDIST" -1d0 "ANGBASE" 0d0 "ANGDIR" 0
              "LASTPOINT" (pt 0 0) "HIGHLIGHT" 1 "BLIPMODE" 0 "ERRNO" 0
              "PICKFIRST" 1 "MIRRTEXT" 0 "TEXTSTYLE" "Standard" "CTAB" "Model"
              "INSUNITS" 1 "MEASUREMENT" 0 "PLINEWID" 0d0 "LIMMIN" (list 0d0 0d0)
              "LIMMAX" (list 12d0 9d0) "LIMCHECK" 0 "GRIDDISPLAY" 1 "OSNAPZ" 0 "EXPERT" 0 "FILEDIA" 1
              "CELTYPE" "BYLAYER" "CELWEIGHT" -1 "LTSCALE" 1d0 "CELTSCALE" 1d0 "LWDISPLAY" 0
              "PLINEGEN" 0 "HPNAME" "ANSI31" "HPSCALE" 1d0 "HPANG" 0d0 "GRIPS" 1 "GRIPOBJLIMIT" 100 "GRIPSIZE" 5
              "SAVEFORMAT" "2000" "PEDITFUZZ" 0d0 "REVCLOUDARC" 0.5d0 "BRKLSIZE" 1d0
              "DIMSTYLE" "Standard" "DIMSCALE" 1d0 "DIMASZ" 0.18d0 "DIMTXT" 0.18d0 "DIMEXO" 0.0625d0
              "DIMEXE" 0.18d0 "DIMGAP" 0.09d0 "DIMDEC" 4 "DIMTAD" 1 "DIMTSZ" 0d0 "DIMCLRD" 0 "DIMCLRE" 0
              "DIMCLRT" 0 "DIMLFAC" 1d0 "DIMPOST" "" "DIMCEN" 0.09d0 "DIMDLI" 0.38d0 "DIMTIH" 0 "DIMADEC" 0
              "DIMBLK" "" "LOGFILEMODE" 0 "LOGFILEPATH" "" "DYNPICOORDS" 0)
        by #'cddr do (setf (gethash k *sysvars*) v)))
(reset-sysvars)

;;; ---------- layers ----------

(defun find-layer (name)
  (find name (drawing-layers *dwg*) :key #'layer-name :test #'string-equal))
(defun ensure-layer (name)
  (or (find-layer name)
      (let ((l (make-layer :name name)))
        (setf (drawing-layers *dwg*) (append (drawing-layers *dwg*) (list l)))
        l)))
(defun layer-visible-p (l) (and l (layer-on l) (not (layer-frozen l))))

(defun find-block (name)
  (find name (drawing-blocks *dwg*) :key #'block-def-name :test #'string-equal))

;;; ---------- entities ----------

(defun new-handle ()
  (format nil "~X" (incf (drawing-next-handle *dwg*))))

(defun normalize-entity-data (data)
  "Fill in defaults (layer, handle) and coerce numbers so stored data is clean."
  (let ((data (copy-tree data)))
    (unless (assoc 8 data) (setf data (append data (list (cons 8 (sysvar "CLAYER"))))))
    (let ((cecolor (sysvar "CECOLOR")))
      (when (and (not (assoc 62 data)) (stringp cecolor) (every #'digit-char-p cecolor)
                 (plusp (length cecolor)))
        (setf data (append data (list (cons 62 (parse-integer cecolor)))))))
    (let ((lt (sysvar "CELTYPE")) (lw (sysvar "CELWEIGHT")) (ls (sysvar "CELTSCALE")))
      (when (and (not (assoc 6 data)) (stringp lt) (not (string-equal lt "BYLAYER")))
        (setf data (append data (list (cons 6 lt)))))
      (when (and (not (assoc 370 data)) (integerp lw) (/= lw -1))
        (setf data (append data (list (cons 370 lw)))))
      (when (and (not (assoc 48 data)) (realp ls) (/= ls 1))
        (setf data (append data (list (cons 48 (d ls)))))))
    (ensure-layer (dxf 8 data))
    data))

(defvar *keep-dimension-blocks* nil
  "True while opening a file: a dimension keeps the picture (block) saved with it, as in AutoCAD,
and is only redrawn by Fancy Pencil once it is edited.")

(defun add-entity (data)
  (let* ((u (ucs))
         ;; made in a side view: from the working plane's coordinates into world coordinates
         (data (if u (data-from-ucs (normalize-entity-data data) u) data))
         (data (normalize-entity-data data))
         ;; it goes into the space being worked in
         (data (let ((d (rem-dxf :layout data)) (sp (current-space)))
                 (if sp (append d (list (cons :layout sp))) d)))
         (h (new-handle))
         (data (cons (assoc 0 data) (cons (cons 5 h) (rem-dxf 0 (rem-dxf 5 data)))))
         (data (if (and (equal (dxf 0 data) "DIMENSION")
                        (not (and *keep-dimension-blocks* (dxf 2 data) (find-block (dxf 2 data)))))
                   (maybe-rebuild-dimension data)
                   data))
         (e (%make-ename h data)))
    (vector-push-extend e (drawing-entities *dwg*))
    (touch)
    e))

(defun ent-data (e)
  "E's data as the current working plane sees it (world coordinates in Top and the isometric views)."
  (let ((u (ucs))) (if u (local-data e u) (ename-data e))))
(defun ent-type (e) (dxf 0 (ename-data e)))
(defun ent-layer (e) (dxf 8 (ename-data e)))
(defun ent-live-p (e) (and (ename-p e) (not (ename-deleted e))))

(defvar *space-override* :none "Bound to NIL (model space) or a layout name to list that space's objects.")
(defun current-space ()
  "The space being drawn in: NIL for model space (the Model tab, or inside a viewport), else the layout's name."
  (if (eq *space-override* :none)
      (and (drawing-ctab *dwg*) (not (drawing-active-vp *dwg*)) (drawing-ctab *dwg*))
      *space-override*))
(defun paper-space-p () (and (current-space) t))
(defun entity-space (data) (cdr (assoc :layout data)))
(defun live-entities ()
  "The objects in the space being worked in."
  (let ((sp (current-space)))
    (loop for e across (drawing-entities *dwg*)
          unless (or (ename-deleted e)
                     (let ((l (entity-space (ename-data e)))) (if sp (not (and l (string-equal l sp))) l)))
            collect e)))
(defun all-live-entities ()
  "Every object in the drawing: model space and every layout."
  (loop for e across (drawing-entities *dwg*) unless (ename-deleted e) collect e))
(defun last-entity ()
  "The object made last (ENTLAST). Usually the end of the list; after a change of draw order
(a hatch sent behind its boundary) the one with the newest handle."
  (let* ((v (drawing-entities *dwg*))
         (tail (loop for i from (1- (length v)) downto 0
                     for e = (aref v i) unless (ename-deleted e) return e))
         (best tail) (bh (and tail (parse-integer (ename-handle tail) :radix 16 :junk-allowed t))))
    (when (and tail (drawing-reordered *dwg*))
      (loop for e across v
            unless (ename-deleted e)
              do (let ((h (parse-integer (ename-handle e) :radix 16 :junk-allowed t)))
                   (when (and h bh (> h bh)) (setf best e bh h)))))
    best))
(defun next-entity (e)
  (let ((v (drawing-entities *dwg*)))
    (if (null e)
        (find-if-not #'ename-deleted v)
        (let ((i (position e v)))
          (and i (find-if-not #'ename-deleted v :start (1+ i)))))))
(defun entity-by-handle (h)
  (find h (drawing-entities *dwg*) :key #'ename-handle :test #'string-equal))

(defun set-entity-data (e data)
  (let ((u (ucs)))
    (if u
        ;; DATA is in the working plane's coordinates: store it in world coordinates, and keep
        ;; this exact data as what the working plane sees
        (let ((local (copy-tree data)))
          (when (equal (dxf 0 local) "DIMENSION") (setf local (maybe-rebuild-dimension local)))
          (setf (ename-data e) (data-from-ucs local u))
          (setf (gethash e *local-cache*) (list u (ename-data e) local)))
        (progn
          (setf (ename-data e) (copy-tree data))
          (when (equal (dxf 0 data) "DIMENSION")
            (setf (ename-data e) (maybe-rebuild-dimension (ename-data e)))))))
  (ensure-layer (dxf 8 data))
  (touch))
(defun delete-entity (e) (setf (ename-deleted e) t) (touch))
(defun undelete-entity (e) (setf (ename-deleted e) nil) (touch))

(defun entity-editable-p (e)
  (let ((l (find-layer (ent-layer e)))) (not (and l (layer-locked l)))))
(defun entity-visible-p (e)
  (and (ent-live-p e) (layer-visible-p (find-layer (ent-layer e)))))

(defun entity-aci (data)
  "Resolved AutoCAD Color Index for drawing."
  (let ((c (dxf 62 data)))
    (cond ((and c (/= c 256) (/= c 0)) (abs c))
          (t (let ((l (find-layer (dxf 8 data)))) (if l (abs (layer-color l)) 7))))))

;;; ---------- undo ----------

(defun take-snapshot ()
  (list (copy-seq (drawing-entities *dwg*))
        (loop for e across (drawing-entities *dwg*)
              collect (list e (ename-data e) (ename-deleted e)))
        (mapcar #'copy-layer (drawing-layers *dwg*))
        (copy-list (drawing-blocks *dwg*))
        (drawing-next-handle *dwg*)
        (sysvar "CLAYER")
        (copy-list (drawing-ltypes *dwg*))))

(defun restore-snapshot (snap)
  (destructuring-bind (ents states layers blocks nh clayer ltypes) snap
    (setf (drawing-ltypes *dwg*) ltypes)
    (let ((v (make-array (max 64 (length ents)) :adjustable t :fill-pointer 0)))
      (loop for e across ents do (vector-push-extend e v))
      (setf (drawing-entities *dwg*) v))
    (loop for (e data del) in states do (setf (ename-data e) data (ename-deleted e) del))
    (setf (drawing-layers *dwg*) (mapcar #'copy-layer layers)
          (drawing-blocks *dwg*) blocks
          (drawing-next-handle *dwg*) nh
          (sysvar "CLAYER") clayer)
    (touch)))

;;; ---------- polylines ----------

(defun lwpoly-vertices (data)
  "List of (point bulge) for an LWPOLYLINE alist."
  (let ((verts nil))
    (dolist (pair data)
      (when (consp pair)
        (case (car pair)
          (10 (push (list (pt (second pair) (third pair)) 0d0) verts))
          (42 (when verts (setf (second (first verts)) (d (cdr pair))))))))
    (nreverse verts)))

(defun lwpoly-closed-p (data) (logtest 1 (or (dxf 70 data) 0)))

(defun make-lwpoly-data (verts closed &key (layer nil) (width nil))
  "VERTS is a list of points or (point bulge) entries."
  (append (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity"))
          (when layer (list (cons 8 layer)))
          (list (cons 100 "AcDbPolyline")
                (cons 90 (length verts)) (cons 70 (if closed 1 0)))
          (when (and width (/= width 0)) (list (cons 43 (d width))))
          (loop for v in verts
                for p = (if (pointp v) v (first v))
                for b = (if (pointp v) 0d0 (second v))
                collect (list 10 (px p) (py p))
                unless (zerop b) collect (cons 42 (d b)))))

(defun lwpoly-pieces (data)
  (let* ((verts (lwpoly-vertices data))
         (closed (lwpoly-closed-p data))
         (n (length verts)))
    (loop for i from 0 below (if closed n (1- n))
          for (p b) = (nth i verts)
          for q = (first (nth (mod (1+ i) n) verts))
          unless (< (dist2d p q) 1d-12)
            collect (if (< (abs b) 1d-12)
                        (list :seg p q)
                        (multiple-value-bind (c r a1 a2) (bulge->arc p q b)
                          (list :arc c r a1 a2))))))

;;; ---------- text extents ----------

(defun text-string-display (s)
  "Strip MTEXT formatting codes for display."
  (let ((out (make-string-output-stream)) (i 0) (n (length s)))
    (loop while (< i n) do
      (let ((ch (char s i)))
        (cond ((and (char= ch #\\) (< (1+ i) n))
               (let ((nx (char-upcase (char s (1+ i)))))
                 (case nx
                   (#\P (write-char #\Newline out) (incf i 2))
                   ((#\\ #\{ #\}) (write-char (char s (1+ i)) out) (incf i 2))
                   (t (let ((semi (position #\; s :start i)))
                        (if (and semi (member nx '(#\F #\H #\C #\W #\Q #\T #\A)))
                            (setf i (1+ semi))
                            (incf i 2)))))))
              ((member ch '(#\{ #\})) (incf i))
              (t (write-char ch out) (incf i)))))
    (let ((r (get-output-stream-string out)))
      ;; %%c %%d %%p control codes
      (loop for (code rep) in '(("%%c" "Ø") ("%%C" "Ø") ("%%d" "°") ("%%D" "°") ("%%p" "±") ("%%P" "±"))
            do (loop for pos = (search code r) while pos
                     do (setf r (concatenate 'string (subseq r 0 pos) rep (subseq r (+ pos 3))))))
      r)))

(defun mtext-wrapped-lines (lines h width)
  "Approximate word-wrap of LINES at WIDTH drawing units (text height H)."
  (if (or (null width) (<= width 0)) lines
      (let ((maxc (max 1 (floor width (* 0.62d0 h)))))
        (loop for line in lines
              append (let ((out nil) (cur ""))
                       (dolist (w (split-on-spaces line))
                         (cond ((string= cur "") (setf cur w))
                               ((<= (+ (length cur) 1 (length w)) maxc) (setf cur (concatenate 'string cur " " w)))
                               (t (push cur out) (setf cur w))))
                       (push cur out)
                       (nreverse out))))))

(defun split-on-spaces (s)
  (loop with start = 0 for pos = (position #\Space s :start start)
        collect (subseq s start pos) while pos do (setf start (1+ pos))))

(defun text-lines (data)
  "The lines of TEXT/MTEXT DATA as they are shown (MTEXT wrapped to its width)."
  (let* ((h (text-effective-height data))
         (mtext (equal (dxf 0 data) "MTEXT"))
         (width (and mtext (dxf 41 data) (plusp (dxf 41 data)) (dxf 41 data)))
         (lines (let ((s (text-string-display (or (dxf 1 data) ""))))
                  (loop with start = 0 for pos = (position #\Newline s :start start)
                        collect (subseq s start pos) while pos do (setf start (1+ pos))))))
    (if mtext (mtext-wrapped-lines lines h width) lines)))

(defun text-box (data)
  "Approximate four corners of a TEXT/MTEXT entity (for selection & extents)."
  (let* ((p (or (dxf 10 data) (pt 0 0)))
         (h (text-effective-height data))
         (rot (or (dxf 50 data) 0d0))
         (mtext (equal (dxf 0 data) "MTEXT"))
         (width (and mtext (dxf 41 data) (plusp (dxf 41 data)) (dxf 41 data)))
         (lines (text-lines data))
         (w (or width (* h 0.85d0 (max 1 (reduce #'max lines :key #'length :initial-value 0)))))
         (hh (* h 1.6d0 (length lines)))
         (attach (or (and mtext (dxf 71 data)) 1))
         (o (if mtext
                (pt (- (px p) (* w (case (mod (1- attach) 3) (0 0) (1 0.5d0) (t 1))))
                    (- (py p) (* hh (case (floor (1- attach) 3) (0 1) (1 0.5d0) (t 0)))))
                (pt (px p) (- (py p) (* 0.25 h)))))
         (corners (list o (pt (+ (px o) w) (py o)) (pt (+ (px o) w) (+ (py o) (if mtext hh (* 1.25 h))))
                        (pt (px o) (+ (py o) (if mtext hh (* 1.25 h)))))))
    ;; at the text's height in its plane (Z of the insertion point)
    (let ((z (pz (pt3 p))))
      (mapcar (lambda (c) (let ((q (rot-pt c p rot))) (pt (px q) (py q) z))) corners))))

(defun text-insertion (data)
  (let ((h72 (or (dxf 72 data) 0)) (h73 (or (dxf 73 data) 0)))
    (if (and (equal (dxf 0 data) "TEXT") (or (/= h72 0) (/= h73 0)) (dxf 11 data))
        (dxf 11 data) (dxf 10 data))))

;;; ---------- pieces for any entity ----------

(defvar *insert-depth* 0)

(defun data-pieces (data)
  (let ((type (dxf 0 data)))
    (cond
      ((offplane-p data) (offplane-pieces data))
      ((equal type "LINE") (list (list :seg (pt3 (dxf 10 data)) (pt3 (dxf 11 data)))))
      ((equal type "POLYFACE") (mesh-pieces data))
      ((equal type "CIRCLE") (list (list :circle (pt3 (dxf 10 data)) (d (dxf 40 data)))))
      ((equal type "ARC") (list (list :arc (pt3 (dxf 10 data)) (d (dxf 40 data))
                                      (d (dxf 50 data)) (d (dxf 51 data)))))
      ((equal type "POINT") (list (list :pt (pt3 (dxf 10 data)))))
      ((equal type "LWPOLYLINE") (lwpoly-pieces data))
      ((member type '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal)
       (let ((c (text-box data)))
         (loop for (a b) on (append c (list (first c))) while b collect (list :gseg a b))))
      ((equal type "INSERT") (insert-pieces data))
      ((equal type "DIMENSION")
       (let ((b (find-block (dxf 2 data))))
         (when (and b (< *insert-depth* 8))
           (let ((*insert-depth* (1+ *insert-depth*)))
             (loop for sub in (block-def-entities b) append (data-pieces sub))))))
      ((equal type "ELLIPSE") (ellipse-pieces data))
      ((equal type "HATCH") (hatch-pieces data))
      ((equal type "SPLINE") (spline-pieces data))
      ((equal type "LEADER") (leader-pieces data))
      ((equal type "SOLID")
       (let ((c (remove nil (list (dxf 10 data) (dxf 11 data) (dxf 13 data) (dxf 12 data)))))
         (loop for (a b) on (append c (list (first c))) while b collect (list :seg (pt3 a) (pt3 b)))))
      ((equal type "IMAGE") (funcall 'image-pieces data))
      ((equal type "HELIX") (funcall 'helix-pieces data))   ; a picture: its frame
      ((equal type "VIEWPORT")                ; a layout's window onto model space: its border
       (let* ((c (pt3 (dxf 10 data))) (hw (/ (d (or (dxf 40 data) 1)) 2)) (hh (/ (d (or (dxf 41 data) 1)) 2))
              (q (list (pt (- (px c) hw) (- (py c) hh)) (pt (+ (px c) hw) (- (py c) hh))
                       (pt (+ (px c) hw) (+ (py c) hh)) (pt (- (px c) hw) (+ (py c) hh)))))
         (loop for (a b) on (append q (list (first q))) while b collect (list :seg a b))))
      (t nil))))

(defun insert-xf (data base)
  (xf-insert (pt3 (dxf 10 data)) (or (dxf 41 data) 1d0) (or (dxf 42 data) 1d0)
             (or (dxf 50 data) 0d0) (pt3 base) (d (or (dxf 43 data) 1d0))))

(defun insert-pieces (data)
  (let ((b (find-block (dxf 2 data))))
    (when (and b (< *insert-depth* 8))
      (let ((*insert-depth* (1+ *insert-depth*))
            (xf (insert-xf data (block-def-base b))))
        (loop for sub in (block-def-entities b)
              append (loop for pc in (data-pieces sub) append (transform-piece pc xf)))))))

(defvar *geom-cache* (make-hash-table :test #'eq))
(defun clear-geom-cache () (clrhash *geom-cache*))
(defun cached-geom (e)
  "(pieces . bbox) for E, recomputed when its data object changes."
  (let ((c (gethash e *geom-cache*)) (data (ent-data e)))
    (if (and c (eq (car c) data))
        (cdr c)
        (let* ((pcs (data-pieces data))
               ;; a solid's box is its corners' (its drawn edges may leave out curved sides)
               (v (cons pcs (if (equal (dxf 0 data) "POLYFACE")
                                (points-bbox (cdr (assoc :verts data)))
                                (pieces-bbox pcs)))))
          (setf (gethash e *geom-cache*) (cons data v))
          v))))
(defun entity-pieces (e) (car (cached-geom e)))
(defun snap-pieces (e) (remove :gseg (entity-pieces e) :key #'first))

(defun entity-bbox (e) (cdr (cached-geom e)))

(defun ellipse-points (data &optional (n 96))
  (let* ((c (pt3 (dxf 10 data))) (m (pt3 (dxf 11 data))) (k (or (dxf 40 data) 1d0))
         (t0 (or (dxf 41 data) 0d0)) (t1 (or (dxf 42 data) +2pi+))
         (sw (let ((s (norm-ang (- t1 t0)))) (if (< s 1d-9) +2pi+ s)))
         (minor (pt (* k (- (py m))) (* k (px m)))))
    (loop for i from 0 to n
          for tt = (+ t0 (* sw (/ i n)))
          collect (v+ c (v+ (v* m (cos tt)) (v* minor (sin tt)))))))
(defun ellipse-pieces (data)
  (loop for (a b) on (ellipse-points data) while b collect (list :seg a b)))

(defun spline-points (data)
  (let* ((deg (or (dxf 71 data) 3))
         (knots (loop for p in data when (and (consp p) (eql (car p) 40)) collect (d (cdr p))))
         (ctrl (loop for p in data when (and (consp p) (eql (car p) 10)) collect (pt3 (cdr p))))
         (fit (loop for p in data when (and (consp p) (eql (car p) 11)) collect (pt3 (cdr p))))
         (n (length ctrl)))
    (cond
      ((and (> n deg) (= (length knots) (+ n deg 1)))
       (let* ((kv (coerce knots 'vector)) (cv (coerce ctrl 'vector))
              (u0 (aref kv deg)) (u1 (aref kv n)) (steps (max 16 (* 8 n))))
         (loop for i from 0 to steps
               for u = (if (= i steps) (- u1 1d-12) (+ u0 (* (- u1 u0) (/ i steps))))
               collect (de-boor deg kv cv u))))
      (fit fit)
      (t ctrl))))
(defun de-boor (p kv cv u)
  (let* ((n (length cv))
         (k (loop for i from p below n when (and (<= (aref kv i) u) (< u (aref kv (1+ i)))) return i
                  finally (return (1- n))))
         (dd (make-array (1+ p))))
    (loop for j from 0 to p do (setf (aref dd j) (aref cv (+ j (- k p)))))
    (loop for r from 1 to p do
      (loop for j from p downto r
            for i = (+ j (- k p))
            for den = (- (aref kv (+ i (- p r) 1)) (aref kv i))
            for alpha = (if (zerop den) 0d0 (/ (- u (aref kv i)) den))
            do (setf (aref dd j) (v+ (v* (aref dd (1- j)) (- 1 alpha)) (v* (aref dd j) alpha)))))
    (aref dd p)))
(defun spline-pieces (data)
  (loop for (a b) on (spline-points data) while b collect (list :seg a b)))

(defun drawing-extents (&optional (ents (live-entities)))
  (let ((boxes (loop for e in ents
                     when (layer-visible-p (find-layer (ent-layer e)))
                       append (let ((b (entity-bbox e))) (and b (list b))))))
    (when boxes
      (list (reduce #'min boxes :key #'first) (reduce #'min boxes :key #'second)
            (reduce #'max boxes :key #'third) (reduce #'max boxes :key #'fourth)))))

;;; ---------- transforming entity data ----------

(defun map-points (data codes fn)
  (mapcar (lambda (pair)
            (if (and (consp pair) (member (car pair) codes) (consp (cdr pair)))
                (let ((q (funcall fn (pt3 (cdr pair)))))
                  (if (= (length (cdr pair)) 2)
                      (list (car pair) (px q) (py q))
                      (cons (car pair) q)))
                pair))
          data))

(defun map-codes (data codes fn)
  (mapcar (lambda (pair)
            (if (and (consp pair) (member (car pair) codes) (realp (cdr pair)))
                (cons (car pair) (funcall fn (cdr pair)))
                pair))
          data))

(defun xform-data (data xf)
  (when (offplane-p data) (return-from xform-data (xform-planar data xf)))
  (when (equal (dxf 0 data) "POLYFACE") (return-from xform-data (xform-mesh data xf)))
  (when (equal (dxf 0 data) "IMAGE") (return-from xform-data (funcall 'xform-image data xf)))
  (when (equal (dxf 0 data) "HELIX") (return-from xform-data (funcall 'xform-helix data xf)))
  (let* ((type (dxf 0 data))
         (s (xf-scale xf))
         (fa (xf-ang xf))
         (fp (xf-fn xf)))
    (cond
      ((member type '("LINE" "POINT") :test #'equal) (map-points data '(10 11) fp))
      ((equal type "CIRCLE") (map-codes (map-points data '(10) fp) '(40) (lambda (r) (* r s))))
      ((equal type "ARC")
       (let* ((d1 (map-codes (map-points data '(10) fp) '(40) (lambda (r) (* r s))))
              (a1 (funcall fa (dxf 50 data))) (a2 (funcall fa (dxf 51 data))))
         (if (xf-mirror xf)
             (set-dxf 51 a1 (set-dxf 50 a2 d1))
             (set-dxf 51 a2 (set-dxf 50 a1 d1)))))
      ((equal type "LWPOLYLINE")
       ;; the vertices stay flat; a move up or down changes the polyline's elevation (38)
       (let* ((el (d (or (dxf 38 data) 0)))
              (nel (pz (pt3 (funcall fp (pt 0 0 el)))))
              (d1 (map-codes (map-points data '(10) (lambda (p) (let ((q (funcall fp (pt (px p) (py p) el)))) (pt (px q) (py q)))))
                             '(40 41 43) (lambda (w) (* w s))))
              (d1 (cond ((< (abs (- nel el)) 1d-12) d1)
                        ((< (abs nel) 1d-12) (rem-dxf 38 d1))
                        (t (set-dxf 38 nel d1)))))
         (if (xf-mirror xf) (map-codes d1 '(42) #'-) d1)))
      ((equal type "MTEXT")
       ;; 10 is the insertion point; 11 (when present) is a direction vector, not a point.
       ;; Height, box width (41) and box height (46) all scale, so the text wraps the same way.
       (let* ((d1 (map-codes (map-points data '(10) fp) '(40 41 46) (lambda (h) (* h s))))
              (a (funcall fa (or (dxf 50 data)
                                 (let ((v (dxf 11 data))) (and (consp v) (atan (second v) (first v))))
                                 0d0))))
         (when (and (xf-mirror xf) (< (* 0.5 +pi+) a (* 1.5 +pi+)))
           (setf a (norm-ang (+ a +pi+))))
         (setf d1 (set-dxf 50 a d1))
         (if (assoc 11 d1) (set-dxf 11 (pt (cos a) (sin a)) d1) d1)))
      ((member type '("TEXT" "ATTDEF" "ATTRIB") :test #'equal)
       (let* ((d1 (map-codes (map-points data '(10 11) fp) '(40) (lambda (h) (* h s))))
              (a (funcall fa (or (dxf 50 data) 0d0))))
         (when (and (xf-mirror xf) (< (* 0.5 +pi+) a (* 1.5 +pi+)))
           (setf a (norm-ang (+ a +pi+))))
         (set-dxf 50 a d1)))
      ((equal type "INSERT")
       (let* ((d1 (map-points data '(10) fp))
              (sx (* (or (dxf 41 data) 1d0) s))
              (sy (* (or (dxf 42 data) 1d0) s))
              (a (funcall fa (or (dxf 50 data) 0d0))))
         (when (xf-mirror xf) (setf sx (- sx) a (norm-ang (+ a +pi+))))
         (set-dxf 50 a (set-dxf 42 sy (set-dxf 41 sx d1)))))
      ((equal type "ELLIPSE")
       (let* ((c (pt3 (dxf 10 data))) (m (pt3 (dxf 11 data)))
              (nc (funcall fp c)) (nm (v- (funcall fp (v+ c m)) nc))
              (d1 (set-dxf 11 nm (set-dxf 10 nc data))))
         (if (xf-mirror xf)
             (let ((t0 (or (dxf 41 data) 0d0)) (t1 (or (dxf 42 data) +2pi+)))
               (if (< (abs (- (norm-ang (- t1 t0)) 0)) 1d-9) d1
                   (set-dxf 42 (norm-ang (- t0)) (set-dxf 41 (norm-ang (- t1)) d1))))
             d1)))
      ((equal type "DIMENSION")
       (let ((d1 (map-points data '(10 11 13 14 15 16) fp)))
         (if (dxf 50 data) (set-dxf 50 (funcall fa (dxf 50 data)) d1) d1)))
      ((equal type "SOLID") (map-points data '(10 11 12 13) fp))
      ;; a viewport moves and grows on the sheet; it keeps its scale, so it shows more of the model
      ((equal type "VIEWPORT") (map-codes (map-points data '(10) fp) '(40 41 45) (lambda (x) (* (abs x) s))))
      ((equal type "HATCH") (xform-hatch data xf))
      ((equal type "SPLINE") (map-points data '(10 11 12 13) fp))
      ((equal type "LEADER") (xform-leader data xf))
      (t (map-points data '(10 11) fp)))))

;;; ---------- explode ----------

(defun explode-data (data)
  "Return a list of simpler entity alists, or NIL if DATA can't be exploded."
  (let ((type (dxf 0 data)) (layer (dxf 8 data)) (color (assoc 62 data)))
    (flet ((common (d) (append d (list (cons 8 layer)) (and color (list color)))))
      (cond
        ((equal type "LWPOLYLINE")
         (loop for pc in (lwpoly-pieces data)
               collect (common (piece->data pc))))
        ((equal type "INSERT")
         (let ((b (find-block (dxf 2 data))))
           (when b
             (let ((xf (insert-xf data (block-def-base b))))
               (loop for sub in (block-def-entities b)
                     collect (let ((nd (if (or (xf-uniform xf) (not (member (dxf 0 sub) '("CIRCLE" "ARC") :test #'equal)))
                                           (xform-data sub xf)
                                           sub)))
                               (if (equal (dxf 8 nd) "0") (set-dxf 8 layer nd) nd)))))))
        ((equal type "LEADER")
         (loop for sub in (leader-display-entities data)
               collect (let ((nd (set-dxf 8 layer (rem-dxf 62 sub)))) (if color (append nd (list color)) nd))))
        ((equal type "DIMENSION")
         ;; a dimension's block is already in drawing coordinates: its lines, arrows and text
         (let ((b (find-block (dxf 2 data))))
           (when b
             (loop for sub in (block-def-entities b)
                   collect (let ((nd (rem-dxf 5 (copy-tree sub))))
                             (when (or (null (dxf 8 nd)) (equal (dxf 8 nd) "0")) (setf nd (set-dxf 8 layer nd)))
                             (when (eql (dxf 62 nd) 0)    ; ByBlock -> the dimension's colour
                               (setf nd (if color (set-dxf 62 (cdr color) nd) (rem-dxf 62 nd))))
                             nd)))))
        (t nil)))))

(defun piece->data (pc)
  (ecase (first pc)
    (:seg (list (cons 0 "LINE") (cons 10 (second pc)) (cons 11 (third pc))))
    (:arc (destructuring-bind (c r a1 a2) (rest pc)
            (list (cons 0 "ARC") (cons 10 c) (cons 40 r) (cons 50 a1) (cons 51 a2))))
    (:circle (list (cons 0 "CIRCLE") (cons 10 (second pc)) (cons 40 (third pc))))
    (:pt (list (cons 0 "POINT") (cons 10 (second pc))))))

(defun new-drawing ()
  (setf *dwg* (make-drawing))
  (reset-sysvars))
