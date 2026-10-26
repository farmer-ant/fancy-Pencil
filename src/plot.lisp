;;;; plot.lisp — PLOT: the drawing to a PDF file or a printer, on a chosen paper size, area and scale.
;;;; Entities are turned into paper-space primitives (polylines, fills, arcs, text) and written as a
;;;; small self-contained PDF (vector lines, standard fonts, no embedded data). Printing sends that PDF
;;;; to CUPS with `lp`.

(in-package :fancy-pencil)


;;; ---------- paper sizes (inches) and scales ----------

(defparameter *paper-sizes*
  '(("Letter (8.5 x 11 in)" 8.5 11) ("Legal (8.5 x 14 in)" 8.5 14) ("Tabloid / ANSI B (11 x 17 in)" 11 17)
    ("ANSI C (17 x 22 in)" 17 22) ("ANSI D (22 x 34 in)" 22 34) ("ANSI E (34 x 44 in)" 34 44)
    ("ARCH A (9 x 12 in)" 9 12) ("ARCH B (12 x 18 in)" 12 18) ("ARCH C (18 x 24 in)" 18 24)
    ("ARCH D (24 x 36 in)" 24 36) ("ARCH E1 (30 x 42 in)" 30 42) ("ARCH E (36 x 48 in)" 36 48)
    ("ISO A4 (210 x 297 mm)" 8.2677 11.6929) ("ISO A3 (297 x 420 mm)" 11.6929 16.5354)
    ("ISO A2 (420 x 594 mm)" 16.5354 23.3858) ("ISO A1 (594 x 841 mm)" 23.3858 33.1102)
    ("ISO A0 (841 x 1189 mm)" 33.1102 46.8110)))

(defparameter *paper-cups-names*
  '(("Letter" . "Letter") ("Legal" . "Legal") ("Tabloid" . "Tabloid") ("ARCH C" . "ARCHC") ("ARCH D" . "ARCHD")
    ("ARCH E (" . "ARCHE") ("ISO A4" . "A4") ("ISO A3" . "A3") ("ISO A2" . "A2") ("ISO A1" . "A1") ("ISO A0" . "A0")))

(defparameter *plot-scales*
  '("Fit to paper" "1:1" "1:2" "1:4" "1:5" "1:10" "1:20" "1:25" "1:50" "1:100" "1:200" "1:500" "1:1000"
    "1/16\" = 1'-0\"" "3/32\" = 1'-0\"" "1/8\" = 1'-0\"" "3/16\" = 1'-0\"" "1/4\" = 1'-0\"" "3/8\" = 1'-0\""
    "1/2\" = 1'-0\"" "3/4\" = 1'-0\"" "1\" = 1'-0\"" "1-1/2\" = 1'-0\"" "3\" = 1'-0\"" "6\" = 1'-0\"" "1'-0\" = 1'-0\""))

(defun plot-parse-fraction (s)
  "\"1-1/2\" -> 1.5, \"3/16\" -> 0.1875, \"6\" -> 6."
  (let* ((s (string-trim " " s)) (dash (position #\- s)) (slash (position #\/ s)))
    (cond ((and dash slash (> slash dash))
           (+ (plot-parse-fraction (subseq s 0 dash)) (plot-parse-fraction (subseq s (1+ dash)))))
          (slash (/ (d (parse-number-text (subseq s 0 slash))) (d (parse-number-text (subseq s (1+ slash))))))
          (t (d (or (parse-number-text s) 1))))))

(defun scale-paper-inches-per-unit (scale units)
  "Paper inches per drawing unit for SCALE text (NIL for Fit). UNITS is \"Inches\", \"Feet\" or \"Millimeters\"."
  (let ((unit-in (cond ((string-equal units "Millimeters") (/ 1 25.4d0))
                       ((string-equal units "Feet") 12d0)
                       ((string-equal units "Meters") (/ 1000 25.4d0))
                       (t 1d0))))
    (cond
      ((or (null scale) (search "Fit" scale)) nil)
      ;; architectural: X" = 1'-0"  (X inches of paper per 12 inches of real size)
      ((search "= 1'-0\"" scale)
       (let* ((lhs (string-trim " " (subseq scale 0 (position #\= scale))))
              (paper-in (if (search "'" lhs) (* 12 (plot-parse-fraction (string-right-trim "'-0\"" lhs)))
                            (plot-parse-fraction (string-right-trim "\"" lhs)))))
         (* (/ paper-in 12d0) unit-in)))
      ;; ratio 1:N (paper : real, same units)
      ((position #\: scale)
       (let ((a (d (or (parse-number-text (subseq scale 0 (position #\: scale))) 1)))
             (b (d (or (parse-number-text (subseq scale (1+ (position #\: scale)))) 1))))
         (* (/ a b) unit-in)))
      (t nil))))

(defun default-plot-units ()
  (case (sysvar "INSUNITS") (4 "Millimeters") (6 "Meters") (2 "Feet") (t "Inches")))

;;; ---------- Helvetica metrics (for text width and alignment) ----------

(defparameter *helv-widths*
  (let ((v (make-array 256 :initial-element 556)))
    (loop for (c w) on '(32 278 33 278 34 355 35 556 36 556 37 889 38 667 39 191 40 333 41 333 42 389 43 584 44 278
                         45 333 46 278 47 278 58 278 59 278 60 584 61 584 62 584 63 556 64 1015 65 667 66 667
                         67 722 68 722 69 667 70 611 71 778 72 722 73 278 74 500 75 667 76 556 77 833 78 722 79 778
                         80 667 81 778 82 722 83 667 84 611 85 722 86 667 87 944 88 667 89 667 90 611 91 278 92 278
                         93 278 94 469 95 556 96 333 97 556 98 556 99 500 100 556 101 556 102 278 103 556 104 556
                         105 222 106 222 107 500 108 222 109 833 110 556 111 556 112 556 113 556 114 333 115 500
                         116 278 117 556 118 500 119 722 120 500 121 500 122 500 123 334 124 260 125 334 126 584
                         176 400 177 584 216 778 248 611) by #'cddr
          do (setf (aref v c) w))
    v))

(defun text-em-width (s)
  "Width of S in Helvetica, in ems."
  (/ (loop for ch across s sum (aref *helv-widths* (min 255 (char-code ch)))) 1000d0))

(defparameter +cap+ 0.718d0 "Helvetica cap height in ems: AutoCAD text height is the capital height.")

;;; ---------- primitives ----------
;;; (:poly pts color lw) open polyline      (:fill polys color)   even-odd filled area
;;; (:arc c r a1 a2 color lw)                (:text s p size rot halign valign color font)

(defvar *plot-prims* nil)
(defvar *plot-mono* nil)
(defvar *plot-lw* t)
(defvar *plot-ppu* nil "PDF points per drawing unit for the plot being made (NIL outside a plot).")

(defun plot-color (aci)
  "RGB (0-1) for ACI on white paper: white/black print black, everything black when monochrome."
  (if (or *plot-mono* (member aci '(7 0 255 256)) (null aci))
      '(0 0 0)
      (let ((hex (aci->hex aci)))
        (list (/ (parse-integer hex :start 1 :end 3 :radix 16) 255d0)
              (/ (parse-integer hex :start 3 :end 5 :radix 16) 255d0)
              (/ (parse-integer hex :start 5 :end 7 :radix 16) 255d0)))))

(defun plot-lw-mm (lw)
  (cond ((not *plot-lw*) 0.18d0)
        ((or (null lw) (minusp lw)) 0.25d0)
        ((zerop lw) 0.1d0)
        (t (/ lw 100d0))))

(defun emit-prim (p) (push p *plot-prims*))

(defun pattern-too-fine-p (pattern scale pc)
  "True when a linetype's dashes would be too small to see on paper (or absurdly many):
AutoCAD prints such lines solid, and generating them can take all the memory."
  (let ((period (* (abs scale) (reduce #'+ (mapcar #'abs pattern)))))
    (or (< period 1d-9)
        (and *plot-ppu* (< (* period *plot-ppu*) 2d0))          ; under ~0.7 mm per repeat
        (let ((len (ignore-errors (polyline-length (piece-polyline pc)))))
          (and len (> (/ len period) 5000))))))

(defun plot-point-symbol (p color lw)
  "A point as its PDMODE symbol: PDSIZE in drawing units, or (relative sizes) 1/8 inch on paper."
  (let* ((mode (or (sysvar "PDMODE") 0)) (ps (d (or (sysvar "PDSIZE") 0)))
         (s (if (plusp ps) (/ ps 2) (if *plot-ppu* (/ 4.5d0 *plot-ppu*) 0.05d0)))
         (x (px p)) (y (py p)) (lw (if (and (realp lw) (plusp lw)) lw 0.18d0)))
    (flet ((ln (x1 y1 x2 y2) (emit-prim (list :poly (list (pt x1 y1) (pt x2 y2)) color lw))))
      (case (mod mode 32)
        (0 (emit-prim (list :fill (list (arc-points p (if *plot-ppu* (/ 1.2d0 *plot-ppu*) 0.02d0) 0 +2pi+ 8)) color)))
        (2 (ln (- x s) y (+ x s) y) (ln x (- y s) x (+ y s)))
        (3 (ln (- x s) (- y s) (+ x s) (+ y s)) (ln (- x s) (+ y s) (+ x s) (- y s)))
        (4 (ln x y x (+ y s))))
      (when (logtest mode 32) (emit-prim (list :arc p s 0d0 +2pi+ color lw)))
      (when (logtest mode 64)
        (emit-prim (list :poly (list (pt (- x s) (- y s)) (pt (+ x s) (- y s)) (pt (+ x s) (+ y s)) (pt (- x s) (+ y s)) (pt (- x s) (- y s)))
                         color lw))))))

(defun plot-pieces (pieces color lw pattern scale)
  (dolist (pc pieces)
    (when (and pattern (not (member (first pc) '(:gseg :pt))) (pattern-too-fine-p pattern scale pc))
      (setf pc (cons :solid pc)))
    (case (first pc)
      (:gseg nil)
      (:pt (plot-point-symbol (second pc) color lw))
      (:solid (plot-pieces (list (rest pc)) color lw nil 1d0))
      (t
       (if pattern
           (dolist (run (dash-runs (piece-polyline pc) pattern scale))
             (when (>= (length run) 2) (emit-prim (list :poly run color lw))))
           (ecase (first pc)
             (:seg (emit-prim (list :poly (list (second pc) (third pc)) color lw)))
             (:circle (emit-prim (list :arc (second pc) (third pc) 0d0 +2pi+ color lw)))
             (:arc (destructuring-bind (c r a1 a2) (rest pc) (emit-prim (list :arc c r a1 a2 color lw))))))))))

(defun join-poly-prims ()
  "Merge consecutive connected 2-point polylines of the same colour/weight (smaller PDF)."
  (let ((out nil))
    (dolist (p (reverse *plot-prims*))
      (let ((last (first out)))
        (if (and (eq (first p) :poly) last (eq (first last) :poly)
                 (equal (third p) (third last)) (eql (fourth p) (fourth last))
                 (< (dist2d (car (last (second last))) (first (second p))) 1d-9))
            (setf (second last) (append (second last) (rest (second p))))
            (push (copy-list p) out))))
    (setf *plot-prims* out)))

(defun pdf-font-for (data)
  (let* ((st (or (find-textstyle (dxf 7 data)) (find-textstyle "Standard")))
         (fam (string-downcase (font-family-for (if st (textstyle-font st) "Arial"))))
         (fmt (and (equal (dxf 0 data) "MTEXT") (mtext-overall-format (or (dxf 1 data) ""))))
         (bold (or (getf fmt :bold) (and st (textstyle-bold st))))
         (italic (or (getf fmt :italic) (and st (textstyle-italic st))))
         (base (cond ((or (search "mono" fam) (search "cour" fam)) "Courier")
                     ((or (search "times" fam) (search "serif" fam) (search "roman" fam)) "Times")
                     (t "Helvetica"))))
    (cond ((string= base "Times") (cond ((and bold italic) "Times-BoldItalic") (bold "Times-Bold") (italic "Times-Italic") (t "Times-Roman")))
          (t (format nil "~A~@[-~A~]" base (cond ((and bold italic) (if (string= base "Courier") "BoldOblique" "BoldOblique"))
                                                 (bold "Bold") (italic "Oblique")))))))

(defun plot-text (data color)
  (let* ((type (dxf 0 data))
         (h (text-effective-height data))
         (size (/ h +cap+))
         (rot (or (dxf 50 data) 0d0))
         (font (pdf-font-for data)))
    (if (equal type "MTEXT")
        (let* ((p (pt3 (dxf 10 data)))
               (attach (or (dxf 71 data) 1))
               (col (mod (1- attach) 3)) (row (floor (1- attach) 3))
               (width (and (dxf 41 data) (plusp (dxf 41 data)) (dxf 41 data)))
               (lines (plot-wrap-lines (text-string-display (or (dxf 1 data) "")) size width))
               (pitch (* h 5/3))
               (total (+ h (* pitch (1- (length lines)))))
               ;; first baseline below the top of the block
               (top-off (case row (0 0d0) (1 (/ total 2)) (t total)))
               (halign (case col (0 0) (1 1) (t 2))))
          (loop for line in lines for i from 0
                for dy = (- top-off h (* i pitch))
                for q = (v+ p (rot-pt (pt 0 dy) (pt 0 0) rot))
                do (emit-prim (list :text line q size rot halign 0 color font))))
        (let* ((s (text-string-display (or (dxf 1 data) "")))
               (h72 (or (dxf 72 data) 0)) (v73 (or (dxf 73 data) 0))
               (p (pt3 (text-insertion data))))
          (emit-prim (list :text s p size rot
                           (case h72 ((1 4) 1) (2 2) (t 0))
                           (if (= h72 4) 2 v73)
                           color font))))))

(defun plot-wrap-lines (s size width)
  "Split on newlines, and word-wrap at WIDTH (drawing units) using Helvetica widths."
  (let ((paras (split-string s #\Newline)))
    (if (null width) paras
        (loop for para in paras
              append (let ((out nil) (cur ""))
                       (dolist (w (split-string para #\Space))
                         (let ((try (if (string= cur "") w (concatenate 'string cur " " w))))
                           (if (or (string= cur "") (<= (* size (text-em-width try)) width))
                               (setf cur try)
                               (progn (push cur out) (setf cur w)))))
                       (push cur out)
                       (nreverse out))))))

(defun plot-data (data aci &optional xf (depth 0) parent-lt parent-lw (parent-lts 1d0))
  "Turn one entity (in world or block coordinates, transformed by XF) into plot primitives."
  (let ((type (dxf 0 data)))
    (when (> depth 8) (return-from plot-data nil))
    (let ((layer (find-layer (dxf 8 data))))
      (when (and layer (or (not (layer-visible-p layer)) (not (layer-plot layer)) (string-equal (layer-name layer) "Defpoints")))
        (return-from plot-data nil)))
    (let ((color (plot-color aci))
          (lw (plot-lw-mm (entity-lineweight data parent-lw))))
      ;; an isometric view, or an object lying in another plane: plotted as it is seen, in 3D
      (when (or (projected-view-p) (offplane-p data))
        (return-from plot-data (plot-data-3d data aci color lw xf depth parent-lt parent-lw parent-lts)))
      (cond
        ((member type '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal)
         (unless (and (equal type "ATTDEF") (null xf))     ; attribute definitions show only inside blocks
           (plot-text (if xf (xform-data data xf) data) color)))
        ((member type '("INSERT" "DIMENSION") :test #'equal)
         (let ((b (find-block (dxf 2 data))))
           (when b
             (let ((cxf (if (equal type "INSERT")
                            (let ((ixf (insert-xf data (block-def-base b)))) (if xf (xf-compose ixf xf) ixf))
                            xf))
                   (lt (entity-ltype-name data parent-lt)) (lw2 (entity-lineweight data parent-lw))
                   (lts (* parent-lts (d (or (dxf 48 data) 1)))))
               (dolist (sub (block-def-entities b))
                 (plot-data sub (sub-color sub aci) cxf (1+ depth) lt lw2 lts))))))
        ((equal type "LEADER")
         (dolist (sub (leader-display-entities (if xf (xform-data data xf) data)))
           (plot-data sub (sub-color sub aci) nil (1+ depth) (entity-ltype-name data parent-lt)
                      (entity-lineweight data parent-lw) parent-lts)))
        ((equal type "SOLID")
         (let ((pts (remove nil (list (dxf 10 data) (dxf 11 data) (dxf 13 data) (dxf 12 data)))))
           (emit-prim (list :fill (list (mapcar (lambda (p) (if xf (xf-point xf (pt3 p)) (pt3 p))) pts)) color))))
        ((equal type "HATCH")
         (let* ((polys (remove-if (lambda (p) (< (length p) 3)) (hatch-world-polys data xf)))
                (bbox (points-bbox (apply #'append polys))))
           (when (and polys bbox)
             (if (or (eql (dxf 70 data) 1) (solid-pattern-p (or (dxf 2 data) "")))
                 (emit-prim (list :fill polys color))
                 (dolist (fam (if xf
                                  (mapcar (lambda (f) (destructuring-bind (a base off dashes) f
                                                        (list (funcall (xf-ang xf) a) (xf-point xf base)
                                                              (v- (xf-point xf (v+ (pt 0 0) off)) (xf-point xf (pt 0 0)))
                                                              (mapcar (lambda (x) (* x (xf-scale xf))) dashes))))
                                          (hatch-pattern-lines data))
                                  (hatch-pattern-lines data)))
                   (let ((segs (hatch-family-segments fam polys bbox 20000)))
                     (if (eq segs :too-dense)
                         (emit-prim (list :fill polys (mapcar (lambda (c) (+ c (* 0.6 (- 1 c)))) color)))
                         (dolist (s segs) (emit-prim (list :poly s color (* 0.7 lw)))))))))))
        (t
         (let* ((pattern (ltype-pattern-for (entity-ltype-name data parent-lt)))
                (scale (* (ltype-scale-for data parent-lts) (if xf (xf-scale xf) 1)))
                (wide (and (equal type "LWPOLYLINE") (dxf 43 data) (plusp (dxf 43 data))
                           (* (dxf 43 data) (if xf (xf-scale xf) 1))))
                (pieces (loop for pc in (data-pieces data) append (if xf (transform-piece pc xf) (list pc)))))
           ;; a polyline with width: its width is in drawing units (marked with a negative weight)
           (plot-pieces pieces color (if wide (- wide) lw) pattern scale)))))))

(defun collect-plot-prims (x0 y0 x1 y1)
  "Primitives for every visible entity touching the window."
  (when (and (fboundp 'visual-style) (not (equal (funcall 'visual-style) "Wireframe")))
    (let ((*plotting* t))
      ;; (joining reverses the list once more: the result is far to near, as painted)
      (let ((*plot-prims* (funcall 'styled-plot-prims x0 y0 x1 y1)))
        (join-poly-prims)
        (return-from collect-plot-prims *plot-prims*))))
  (let ((*plot-prims* nil) (*plotting* t))
    (dolist (e (live-entities))
      (let ((b (ignore-errors (if (projected-view-p) (entity-view-bbox e) (entity-bbox e)))))
        (when (or (null b) (not (or (> (first b) x1) (< (third b) x0) (> (second b) y1) (< (fourth b) y0))))
          (ignore-errors (plot-data (ent-data e) (entity-aci (ent-data e)))))))
    (join-poly-prims)
    *plot-prims*))

;;; ---------- PDF output ----------

(defun pdf-num (x)
  (let ((v (d x)))
    (if (< (abs (- v (round v))) 1d-6) (format nil "~D" (round v))
        (string-right-trim "0" (format nil "~,3F" v)))))

(defun pdf-string (s)
  (with-output-to-string (o)
    (write-char #\( o)
    (loop for ch across s
          for c = (char-code ch)
          do (cond ((member ch '(#\( #\) #\\)) (write-char #\\ o) (write-char ch o))
                   ((> c 255) (write-char #\? o))
                   ((< c 32) (write-char #\Space o))
                   (t (write-char ch o))))
    (write-char #\) o)))

(defun arc-beziers (c r a1 a2)
  "Cubic Bezier segments (p0 p1 p2 p3) approximating the CCW arc a1->a2."
  (let* ((sweep (let ((s (- a2 a1))) (if (>= (abs s) (- +2pi+ 1d-9)) +2pi+ (norm-ang s))))
         (n (max 1 (ceiling sweep (/ +pi+ 2))))
         (da (/ sweep n))
         (k (* 4/3 (tan (/ da 4)))))
    (loop for i below n
          for t0 = (+ a1 (* i da)) for t1 = (+ t0 da)
          for p0 = (polar c t0 r) for p3 = (polar c t1 r)
          collect (list p0 (v+ p0 (v* (pt (- (sin t0)) (cos t0)) (* k r)))
                        (v- p3 (v* (pt (- (sin t1)) (cos t1)) (* k r))) p3))))

(defun write-plot-pdf (path prims &key paper-w paper-h window scale-ppu offset
                                    (title "Fancy Pencil plot"))
  "PRIMS in world coordinates; WINDOW (x0 y0 x1 y1) is mapped to the page at SCALE-PPU points per
drawing unit with its lower-left corner at OFFSET (points). Page is PAPER-W x PAPER-H points."
  (destructuring-bind (wx0 wy0 wx1 wy1) window
    (let* ((ox (first offset)) (oy (second offset))
           (fonts nil)
           (content
             (with-output-to-string (o)
               (labels ((tx (p) (+ ox (* (- (px p) wx0) scale-ppu)))
                        (ty (p) (+ oy (* (- (py p) wy0) scale-ppu)))
                        (xy (p) (format nil "~A ~A" (pdf-num (tx p)) (pdf-num (ty p))))
                        (rgb (c op) (format o "~{~A~^ ~} ~A~%" (mapcar #'pdf-num c) op))
                        (width (lw) (if (minusp lw) (* (- lw) scale-ppu) (* lw 72 (/ 25.4d0)))))
                 (format o "q 1 J 1 j~%")
                 ;; clip to the plotted window
                 (format o "~A ~A ~A ~A re W n~%" (pdf-num ox) (pdf-num oy)
                         (pdf-num (* (- wx1 wx0) scale-ppu)) (pdf-num (* (- wy1 wy0) scale-ppu)))
                 (let ((cur-stroke nil) (cur-fill nil) (cur-w nil))
                   (dolist (p prims)
                     (ecase (first p)
                       (:poly
                        (destructuring-bind (pts color lw) (rest p)
                          (unless (equal color cur-stroke) (rgb color "RG") (setf cur-stroke color))
                          (let ((wv (max 0.1d0 (width lw)))) (unless (eql wv cur-w) (format o "~A w~%" (pdf-num wv)) (setf cur-w wv)))
                          (format o "~A m" (xy (first pts)))
                          (dolist (q (rest pts)) (format o " ~A l" (xy q)))
                          (format o " S~%")))
                       (:arc
                        (destructuring-bind (c r a1 a2 color lw) (rest p)
                          (unless (equal color cur-stroke) (rgb color "RG") (setf cur-stroke color))
                          (let ((wv (max 0.1d0 (width lw)))) (unless (eql wv cur-w) (format o "~A w~%" (pdf-num wv)) (setf cur-w wv)))
                          (let ((bz (arc-beziers c r a1 a2)))
                            (format o "~A m" (xy (first (first bz))))
                            (dolist (b bz) (format o " ~A ~A ~A c" (xy (second b)) (xy (third b)) (xy (fourth b))))
                            (format o " S~%"))))
                       (:fill
                        (destructuring-bind (polys color) (rest p)
                          (unless (equal color cur-fill) (rgb color "rg") (setf cur-fill color))
                          (dolist (poly polys)
                            (format o "~A m" (xy (first poly)))
                            (dolist (q (rest poly)) (format o " ~A l" (xy q)))
                            (format o " h~%"))
                          (format o "f*~%")))
                       (:text
                        (destructuring-bind (s pos size rot halign valign color font) (rest p)
                          (when (plusp (length (string-trim " " s)))
                            (pushnew font fonts :test #'string=)
                            (let* ((fs (* size scale-ppu))
                                   (w (* fs (text-em-width s)))
                                   (dx (case halign (1 (- (/ w 2))) (2 (- w)) (t 0)))
                                   (dy (case valign (1 (* 0.207 fs)) (2 (- (* 0.359 fs))) (3 (- (* +cap+ fs))) (t 0)))
                                   (c (cos rot)) (sn (sin rot))
                                   (x (+ (tx pos) (* c dx) (- (* sn dy))))
                                   (y (+ (ty pos) (* sn dx) (* c dy))))
                              (unless (equal color cur-fill) (rgb color "rg") (setf cur-fill color))
                              (format o "BT /F~D ~A Tf ~A ~A ~A ~A ~A ~A Tm ~A Tj ET~%"
                                      (1+ (position font (reverse fonts) :test #'string=)) (pdf-num fs)
                                      (pdf-num c) (pdf-num sn) (pdf-num (- sn)) (pdf-num c) (pdf-num x) (pdf-num y)
                                      (pdf-string s)))))))))
                 (format o "Q~%"))))
           (font-list (reverse fonts))
           (objects nil))
      ;; objects: 1 catalog, 2 pages, 3 page, 4 content, 5 info, 6.. fonts
      (flet ((obj (n body) (push (cons n body) objects)))
        (obj 1 "<< /Type /Catalog /Pages 2 0 R >>")
        (obj 2 "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
        (obj 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~A ~A] /Contents 4 0 R /Resources << /Font << ~{~A~^ ~} >> >> >>"
                       (pdf-num paper-w) (pdf-num paper-h)
                       (loop for f in font-list for i from 1 collect (format nil "/F~D ~D 0 R" i (+ 5 i)))))
        (obj 4 (format nil "<< /Length ~D >>~%stream~%~Aendstream" (length content) content))
        (obj 5 (format nil "<< /Producer (Fancy Pencil ~A) /Title ~A >>" *version* (pdf-string title)))
        (loop for f in font-list for i from 1
              do (obj (+ 5 i) (format nil "<< /Type /Font /Subtype /Type1 /BaseFont /~A /Encoding /WinAnsiEncoding >>" f))))
      (setf objects (sort objects #'< :key #'car))
      (with-open-file (out path :direction :output :if-exists :supersede :external-format :latin-1)
        (let ((pos 0) (offsets nil))
          (flet ((emit (s) (write-string s out) (incf pos (length s))))
            (emit (format nil "%PDF-1.4~%%~C~C~C~C~%" (code-char 226) (code-char 227) (code-char 207) (code-char 211)))
            (dolist (ob objects)
              (push pos offsets)
              (emit (format nil "~D 0 obj~%~A~%endobj~%" (car ob) (cdr ob))))
            (let ((xref pos))
              (emit (format nil "xref~%0 ~D~%0000000000 65535 f ~%" (1+ (length objects))))
              (dolist (off (reverse offsets)) (emit (format nil "~10,'0D 00000 n ~%" off)))
              (emit (format nil "trailer~%<< /Size ~D /Root 1 0 R /Info 5 0 R >>~%startxref~%~D~%%%EOF~%"
                            (1+ (length objects)) xref))))))
      path)))

;;; ---------- the plot itself ----------

(defstruct plot-settings
  (device "PDF file") (paper "Tabloid / ANSI B (11 x 17 in)") (landscape t) (area "Extents")
  (window nil) (scale "Fit to paper") (units nil) (center t) (mono nil) (lineweights t) (copies 1))

(defvar *plot-settings* nil)

(defun plot-window-for (s)
  "World rectangle (x0 y0 x1 y1) for the settings' plot area."
  (let ((area (plot-settings-area s)))
    (cond ((and (string-equal area "Window") (plot-settings-window s)) (plot-settings-window s))
          ((string-equal area "Display")
           (multiple-value-bind (x0 y0 x1 y1) (view-rect 0) (list x0 y0 x1 y1)))
          ((string-equal area "Limits")
           (let ((a (pt3 (sysvar "LIMMIN"))) (b (pt3 (sysvar "LIMMAX")))) (list (px a) (py a) (px b) (py b))))
          (t (or (if (projected-view-p) (view-extents) (drawing-extents)) (list 0d0 0d0 12d0 9d0))))))

(defun make-plot (s path)
  "Write the plot described by settings S to PDF PATH. Returns a short description."
  (let* ((paper (or (assoc (plot-settings-paper s) *paper-sizes* :test #'string-equal) (third *paper-sizes*)))
         (pw (* 72 (if (plot-settings-landscape s) (max (second paper) (third paper)) (min (second paper) (third paper)))))
         (ph (* 72 (if (plot-settings-landscape s) (min (second paper) (third paper)) (max (second paper) (third paper)))))
         (margin 18d0)                                   ; 1/4 inch printable margin
         (win (plot-window-for s))
         (ww (max 1d-9 (- (third win) (first win)))) (wh (max 1d-9 (- (fourth win) (second win))))
         (avail-w (- pw (* 2 margin))) (avail-h (- ph (* 2 margin)))
         (in-per-unit (scale-paper-inches-per-unit (plot-settings-scale s) (or (plot-settings-units s) (default-plot-units))))
         (ppu (if in-per-unit (* 72 in-per-unit) (min (/ avail-w ww) (/ avail-h wh))))
         ;; when the window is bigger than the paper at a fixed scale, plot its middle part
         (cw (min ww (/ avail-w ppu))) (ch (min wh (/ avail-h ppu)))
         (cx (/ (+ (first win) (third win)) 2)) (cy (/ (+ (second win) (fourth win)) 2))
         (clip (list (- cx (/ cw 2)) (- cy (/ ch 2)) (+ cx (/ cw 2)) (+ cy (/ ch 2))))
         (offset (if (plot-settings-center s)
                     (list (/ (- pw (* cw ppu)) 2) (/ (- ph (* ch ppu)) 2))
                     (list margin margin)))
         (prims (let ((*plot-mono* (plot-settings-mono s)) (*plot-lw* (plot-settings-lineweights s)) (*plot-ppu* ppu))
                  (apply #'collect-plot-prims clip))))
    (write-plot-pdf path prims :paper-w pw :paper-h ph :window clip :scale-ppu ppu :offset offset
                               :title (drawing-display-name))
    (format nil "~A, ~A, ~:[portrait~;landscape~], ~A~A"
            (string-trim " " (first (split-string (first paper) #\()))
            (plot-settings-area s) (plot-settings-landscape s)
            (if in-per-unit (plot-settings-scale s) "scaled to fit")
            (if (or (< cw (* ww (- 1 1d-6))) (< ch (* wh (- 1 1d-6)))) " (area larger than the paper: middle part plotted)" ""))))

(defun system-printers ()
  (multiple-value-bind (code text) (run-shell "lpstat -e 2>/dev/null || lpstat -a 2>/dev/null | cut -d' ' -f1")
    (and (eql code 0) (remove "" (mapcar (lambda (l) (string-trim '(#\Space #\Return) l)) (split-string text #\Newline))
                              :test #'string=))))

(defun send-to-printer (pdf printer s)
  (let* ((paper (plot-settings-paper s))
         (media (cdr (find-if (lambda (c) (search (car c) paper)) *paper-cups-names*))))
    (run-shell (format nil "lp -d ~A -n ~D~@[ -o media=~A~] -o ~:[portrait~;landscape~] -o print-scaling=none ~A"
                       (shell-quote printer) (max 1 (plot-settings-copies s)) media (plot-settings-landscape s)
                       (shell-quote pdf)))))

(defun open-pdf-viewer (path)
  (run-shell (format nil "(xdg-open ~A >/dev/null 2>&1 &)" (shell-quote path))))

(defun plot-dialog (s printers)
  "Show the Plot dialog. Returns (values action settings), action \"OK\" \"PREVIEW\" \"PICK\" or NIL."
  (let* ((r (tk-ask (format nil "plotdlg ~A ~A ~A ~A ~D ~A ~A ~A ~A ~D ~D ~D ~D ~A"
                            (tcl-list (cons "PDF file" printers)) (tq (plot-settings-device s))
                            (tcl-list (mapcar #'first *paper-sizes*)) (tq (plot-settings-paper s))
                            (if (plot-settings-landscape s) 1 0)
                            (tq (plot-settings-area s)) (tcl-list *plot-scales*) (tq (plot-settings-scale s))
                            (tq (or (plot-settings-units s) (default-plot-units)))
                            (if (plot-settings-center s) 1 0) (if (plot-settings-mono s) 1 0)
                            (if (plot-settings-lineweights s) 1 0) (plot-settings-copies s)
                            (tq (if (plot-settings-window s)
                                    (format nil "~{~,2F~^, ~}" (plot-settings-window s)) "")))))
         (f (split-string r (code-char 31))))
    (if (or (string= r "") (string= r "CANCEL"))
        nil
        (destructuring-bind (action device paper land area scale units center mono lw copies &rest ignore) f
          (declare (ignore ignore))
          (values action
                  (make-plot-settings :device device :paper paper :landscape (string= land "1") :area area
                                      :window (plot-settings-window s) :scale scale :units units
                                      :center (string= center "1") :mono (string= mono "1")
                                      :lineweights (string= lw "1")
                                      :copies (max 1 (or (ignore-errors (parse-integer copies)) 1))))))))

(defun pick-plot-window ()
  (let ((a (gpoint "Specify first corner of plot window: ")))
    (when (pointp a)
      (let ((b (get-user-input :corner :base a :prompt "Specify opposite corner: ")))
        (when (pointp b)
          ;; in an isometric view the window is on the screen: keep it in view coordinates
          (when (projected-view-p) (setf a (w->uv a) b (w->uv b)))
          (list (min (px a) (px b)) (min (py a) (py b)) (max (px a) (px b)) (max (py a) (py b))))))))

(defun default-pdf-name ()
  (let ((f (drawing-filename *dwg*)))
    (if f (namestring (make-pathname :type "pdf" :defaults (pathname f)))
        (namestring (merge-pathnames (make-pathname :name (pathname-name (untitled-name)) :type "pdf")
                                     (let ((h (or (getenv "HOME") "/tmp"))) (if (char= (char h (1- (length h))) #\/) h (concatenate (quote string) h "/"))))))))

(defun finish-plot (s)
  (if (string-equal (plot-settings-device s) "PDF file")
      (let ((f (ask-file t "Save Plot As" (default-pdf-name) "pdf")))
        (when f
          (let ((f (ensure-ext f "pdf")))
            (out (format nil "Plotting ~A ...~%" (file-namestring f)))
            (let ((desc (make-plot s f)))
              (out (format nil "Plot written: ~A~%  ~A~%" f desc))))))
      (let ((tmp (concatenate 'string (temp-dir "plot") "plot.pdf")))
        (let ((desc (make-plot s tmp)))
          (multiple-value-bind (code text) (send-to-printer tmp (plot-settings-device s) s)
            (if (eql code 0)
                (out (format nil "Sent to ~A: ~A~%  ~A~%" (plot-settings-device s) (string-trim '(#\Newline) text) desc))
                (tell-user (format nil "The printer did not accept the job:~%~A" text))))))))

(defcmd "PLOT" ("PRINT" "PLT") "Plots the drawing to a PDF file or a printer."
  (if (not (use-inplace-editor-p))
      (plot-command-line)
      (let ((s (or *plot-settings* (make-plot-settings :units (default-plot-units))))
            (printers (system-printers)))
        (loop
          (multiple-value-bind (action new) (plot-dialog s printers)
            (unless action (return))
            (setf s new *plot-settings* new)
            (cond
              ((string= action "PICK")
               (let ((w (pick-plot-window)))
                 (when w (setf (plot-settings-window s) w (plot-settings-area s) "Window"))))
              ((string= action "PREVIEW")
               (when (and (string-equal (plot-settings-area s) "Window") (null (plot-settings-window s)))
                 (let ((w (pick-plot-window))) (when w (setf (plot-settings-window s) w))))
               (let ((tmp (concatenate 'string (temp-dir "preview") "preview.pdf")))
                 (out (format nil "Preview: ~A~%" (make-plot s tmp)))
                 (open-pdf-viewer tmp)))
              (t
               (when (and (string-equal (plot-settings-area s) "Window") (null (plot-settings-window s)))
                 (let ((w (pick-plot-window))) (if w (setf (plot-settings-window s) w) (return))))
               (setf *plot-settings* s)
               (finish-plot s)
               (return))))))))

(defun plot-command-line ()
  "-PLOT: the same choices at the command line (for scripts and AutoLISP)."
  (let* ((s (copy-plot-settings (or *plot-settings* (make-plot-settings :units (default-plot-units)))))
         (dev (gstring "Enter an output device name or [PDF] <PDF>: " t))
         (paper (gstring (format nil "Enter paper size <~A>: " (plot-settings-paper s)) t))
         (orient (gkw "Enter drawing orientation [Portrait/Landscape] <Landscape>: " '("Portrait" "Landscape") "Landscape"))
         (area (gkw "Enter plot area [Display/Extents/Limits/Window] <Extents>: " '("Display" "Extents" "Limits" "Window") "Extents")))
    (unless (or (string= dev "") (string-equal dev "PDF")) (setf (plot-settings-device s) dev))
    (when (string= dev "") (setf (plot-settings-device s) "PDF file"))
    (when (plusp (length paper))
      (let ((m (find-if (lambda (p) (search (string-downcase paper) (string-downcase (first p)))) *paper-sizes*)))
        (when m (setf (plot-settings-paper s) (first m)))))
    (setf (plot-settings-landscape s) (string-equal orient "Landscape") (plot-settings-area s) area)
    (when (string-equal area "Window")
      (setf (plot-settings-window s) (pick-plot-window)))
    (let ((sc (gstring "Enter plot scale (e.g. 1:100, 1/4\"=1'-0\") or [Fit] <Fit>: " t)))
      (setf (plot-settings-scale s)
            (cond ((or (string= sc "") (string-equal sc "Fit") (string-equal sc "F")) "Fit to paper")
                  ((and (search "=" sc) (not (search " = " sc)))
                   (let ((p (position #\= sc))) (format nil "~A = ~A" (subseq sc 0 p) (subseq sc (1+ p)))))
                  (t sc))))
    (setf (plot-settings-mono s) (string-equal (gkw "Plot in black only? [Yes/No] <No>: " '("Yes" "No") "No") "Yes"))
    (if (string-equal (plot-settings-device s) "PDF file")
        (let ((f (gstring (format nil "Enter file name <~A>: " (default-pdf-name)) t)))
          (let ((f (ensure-ext (if (string= f "") (default-pdf-name) f) "pdf")))
            (out (format nil "Plot written: ~A~%  ~A~%" f (make-plot s f)))))
        (finish-plot s))
    (setf *plot-settings* s)))

(defcmd "-PLOT" () "Plots at the command line (for scripts)." (plot-command-line))

;;; ---------- drawing limits (LIMITS, Format > Drawing Limits) ----------

(defun limits-for-sheet (paper scale landscape &optional (origin (pt 0 0)))
  "Upper-right limits corner for sheet PAPER at plot SCALE (the area the sheet covers), from ORIGIN."
  (let* ((p (assoc paper *paper-sizes* :test #'string-equal))
         (w (and p (if landscape (max (second p) (third p)) (min (second p) (third p)))))
         (h (and p (if landscape (min (second p) (third p)) (max (second p) (third p)))))
         (ipu (scale-paper-inches-per-unit scale (default-plot-units))))
    (when (and p ipu (plusp ipu))
      (pt (+ (px origin) (/ w ipu)) (+ (py origin) (/ h ipu))))))

(defun limits-dialog ()
  (let* ((lo (pt3 (sysvar "LIMMIN"))) (hi (pt3 (sysvar "LIMMAX")))
         (vals (list (fmt-len (px lo)) (fmt-len (py lo)) (fmt-len (px hi)) (fmt-len (py hi))))
         (paper "ARCH D (24 x 36 in)") (scale "1/4\" = 1'-0\"") (land "1")
         (gridin (if (eql (sysvar "GRIDDISPLAY") 0) "1" "0")) (check (if (eql (sysvar "LIMCHECK") 1) "1" "0"))
         (r nil))
    (unless (assoc paper *paper-sizes* :test #'string-equal) (setf paper (first (nth 4 *paper-sizes*))))
    (loop
      (setf r (tk-ask (format nil "limitsdlg ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A"
                              (tq (first vals)) (tq (second vals)) (tq (third vals)) (tq (fourth vals))
                              (tcl-list (mapcar #'first *paper-sizes*))
                              (tcl-list (remove "Fit to paper" *plot-scales* :test #'string=))
                              gridin check (tq paper) (tq scale) land)))
      (if (and (> (length r) 6) (string= (subseq r 0 6) "SHEET "))
          ;; "Use sheet": the area that sheet covers at that scale, then the dialog again
          (destructuring-bind (x0 y0 x1 y1 p sc ld gi ck) (split-string (subseq r 6) (code-char 31))
            (setf paper p scale sc land ld gridin gi check ck)
            (let* ((o (pt (or (parse-number-text x0) 0) (or (parse-number-text y0) 0)))
                   (c (limits-for-sheet p sc (string= ld "1") o)))
              (setf vals (list x0 y0 (if c (fmt-len (px c)) x1) (if c (fmt-len (py c)) y1)))))
          (return)))
    (when (and (> (length r) 3) (string= (subseq r 0 3) "OK "))
      (destructuring-bind (x0 y0 x1 y1 gridin check) (split-string (subseq r 3) (code-char 31))
        (let ((a (mapcar #'parse-number-text (list x0 y0 x1 y1))))
          (if (and (every #'realp a) (< (first a) (third a)) (< (second a) (fourth a)))
              (progn
                (setf (sysvar "LIMMIN") (list (d (first a)) (d (second a)))
                      (sysvar "LIMMAX") (list (d (third a)) (d (fourth a)))
                      (sysvar "GRIDDISPLAY") (if (string= gridin "1") 0 1)
                      (sysvar "LIMCHECK") (if (string= check "1") 1 0))
                (touch)
                (zoom-to-box (d (first a)) (d (second a)) (d (third a)) (d (fourth a)))
                (out (format nil "Drawing limits: ~A,~A to ~A,~A (~A x ~A).~%"
                             (fmt-len (first a)) (fmt-len (second a)) (fmt-len (third a)) (fmt-len (fourth a))
                             (fmt-len (- (third a) (first a))) (fmt-len (- (fourth a) (second a))))))
              (tell-user "The upper-right corner must be above and to the right of the lower-left corner.")))))))

(defcmd "LIMITS" ("DRAWINGLIMITS") "Sets the drawing's size (its limits): the area the grid, ZOOM All and Plot Limits use."
  (if (and (gui-p) (not (from-lisp-p)))
      (limits-dialog)
      (let ((r (gpoint (format nil "Specify lower left corner or [ON/OFF] <~A,~A>: "
                               (fmt-len (px (sysvar "LIMMIN"))) (fmt-len (py (sysvar "LIMMIN"))))
                       :keywords '("ON" "OFF"))))
        (cond ((equal r "ON") (setf (sysvar "LIMCHECK") 1))
              ((equal r "OFF") (setf (sysvar "LIMCHECK") 0))
              (t (let* ((lo (if (pointp r) r (pt3 (sysvar "LIMMIN"))))
                        (hi (gpoint (format nil "Specify upper right corner <~A,~A>: "
                                            (fmt-len (px (sysvar "LIMMAX"))) (fmt-len (py (sysvar "LIMMAX")))))))
                   (setf hi (if (pointp hi) hi (pt3 (sysvar "LIMMAX"))))
                   (setf (sysvar "LIMMIN") (list (px lo) (py lo)) (sysvar "LIMMAX") (list (px hi) (py hi)))
                   (touch)
                   (when (gui-p) (gui-regen))))))))
;;; ---------- plotting what a 3D view shows ----------
;;; In the isometric views (and for objects lying in other planes) everything is plotted as the
;;; screen shows it: 3D outlines at their heights, with thickness, linetypes (dashes laid out along
;;; the object) and text drawn letter by letter in its plane, then seen from the view's direction.

(defun plot-uv (p) (if (projected-view-p) (w->uv (pt3 p)) (pt3 p)))

(defun plot-3d-poly (pts color lw pattern scale)
  "Plot the 3D polyline PTS, dashed when PATTERN says so and the dashes are big enough."
  (when (cdr pts)
    (let ((period (and pattern (* (abs scale) (reduce #'+ (mapcar #'abs pattern))))))
      (if (or (null pattern) (< period 1d-9)
              (and *plot-ppu* (< (* period *plot-ppu*) 2d0))
              (> (/ (polyline-length pts) period) 5000))
          (emit-prim (list :poly (mapcar #'plot-uv pts) color lw))
          (dolist (run (dash-runs pts pattern scale))
            (when (>= (length run) 2) (emit-prim (list :poly (mapcar #'plot-uv run) color lw))))))))

(defun piece-points-3d (pc)
  "Points along piece PC (arcs and circles finely, at the height of their centre)."
  (case (first pc)
    ((:seg :gseg) (list (pt3 (second pc)) (pt3 (third pc))))
    (:circle (let ((c (pt3 (second pc)))) (mapcar (lambda (q) (pt (px q) (py q) (pz c))) (arc-points c (third pc) 0d0 +2pi+ 120))))
    (:arc (destructuring-bind (c r a1 a2) (rest pc)
            (let ((c (pt3 c))) (mapcar (lambda (q) (pt (px q) (py q) (pz c))) (arc-points c r a1 a2 (max 12 (ceiling (* 60 (/ (arc-sweep a1 a2) pi)))))))))
    (t nil)))

(defun plot-data-3d (data aci color lw xf depth parent-lt parent-lw parent-lts)
  (let* ((type (dxf 0 data))
         (pattern (ltype-pattern-for (entity-ltype-name data parent-lt)))
         (scale (* (ltype-scale-for data parent-lts) (if xf (xf-scale xf) 1)))
         (xp (lambda (p) (if xf (xf-point xf (pt3 p)) (pt3 p)))))
    (cond
      ;; text: its letters, in its plane
      ((member type '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal)
       (unless (and (equal type "ATTDEF") (null xf))
         (let ((strokes (if (offplane-p data)
                            (offplane-pieces data (ignore-errors (text-stroke-pieces (rem-dxf 210 data))))
                            (ignore-errors (text-stroke-pieces data)))))
           (dolist (pc strokes)
             (emit-prim (list :poly (list (plot-uv (funcall xp (second pc))) (plot-uv (funcall xp (third pc)))) color (* 0.7 lw)))))))
      ;; outlines in another plane, with their linetype
      ((and (offplane-p data) (member type '("CIRCLE" "ARC" "LWPOLYLINE" "ELLIPSE" "SPLINE") :test #'equal))
       (dolist (poly (offplane-polylines data))
         (plot-3d-poly (mapcar xp poly) color lw pattern scale)))
      ((offplane-p data)                     ; blocks, hatches ... in another plane: their edges
       (dolist (pc (data-pieces data))
         (if (eq (first pc) :pt)
             (plot-point-symbol (plot-uv (funcall xp (second pc))) color lw)
             (emit-prim (list :poly (list (plot-uv (funcall xp (second pc))) (plot-uv (funcall xp (third pc)))) color lw)))))
      ((member type '("INSERT" "DIMENSION") :test #'equal)
       (let ((b (find-block (dxf 2 data))))
         (when b
           (let ((cxf (if (equal type "INSERT")
                          (let ((ixf (insert-xf data (block-def-base b)))) (if xf (xf-compose ixf xf) ixf))
                          xf))
                 (lt (entity-ltype-name data parent-lt)) (lw2 (entity-lineweight data parent-lw))
                 (lts (* parent-lts (d (or (dxf 48 data) 1)))))
             (dolist (sub (block-def-entities b))
               (plot-data sub (sub-color sub aci) cxf (1+ depth) lt lw2 lts))))))
      ((equal type "LEADER")
       (dolist (sub (leader-display-entities (if xf (xform-data data xf) data)))
         (plot-data sub (sub-color sub aci) nil (1+ depth) (entity-ltype-name data parent-lt)
                    (entity-lineweight data parent-lw) parent-lts)))
      ((equal type "SOLID")
       (let ((pts (remove nil (list (dxf 10 data) (dxf 11 data) (dxf 13 data) (dxf 12 data)))))
         (emit-prim (list :fill (list (mapcar (lambda (p) (plot-uv (funcall xp p))) pts)) color))))
      ((equal type "HATCH")
       (let ((polys (remove-if (lambda (p) (< (length p) 3)) (hatch-world-polys data xf))))
         (if (or (eql (dxf 70 data) 1) (solid-pattern-p (or (dxf 2 data) "")))
             (emit-prim (list :fill (mapcar (lambda (poly) (mapcar #'plot-uv poly)) polys) color))
             (let ((bbox (points-bbox (apply #'append polys))))
               (when bbox
                 (dolist (fam (hatch-pattern-lines data))
                   (let ((segs (hatch-family-segments fam polys bbox 20000)))
                     (unless (eq segs :too-dense)
                       (dolist (s segs) (emit-prim (list :poly (mapcar #'plot-uv s) color (* 0.7 lw))))))))))))
      (t
       ;; lines, arcs, circles, polylines, ellipses, splines: at their height, with thickness
       (let* ((elev (if (equal type "LWPOLYLINE") (d (or (dxf 38 data) 0)) 0d0))
              (th (if (member type '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POINT") :test #'equal) (d (or (dxf 39 data) 0)) 0d0)))
         (dolist (pc (data-pieces data))
           (if (eq (first pc) :pt)
               (plot-point-symbol (plot-uv (funcall xp (v+ (pt3 (second pc)) (pt 0 0 elev)))) color lw)
               (let ((pts (mapcar (lambda (p) (funcall xp (pt (px p) (py p) (+ (pz p) elev)))) (piece-points-3d pc))))
                 (plot-3d-poly pts color lw pattern scale)
                 (unless (zerop th)
                   (let ((top (mapcar (lambda (p) (v+ p (pt 0 0 th))) pts)))
                     (plot-3d-poly top color lw pattern scale)
                     (dolist (p (list (first pts) (car (last pts))))
                       (plot-3d-poly (list p (v+ p (pt 0 0 th))) color lw pattern scale))))))))))))
