;;;; text3d.lisp - text drawn with line strokes (from vector-font.lisp), so it shows in its own
;;;; plane in the isometric views and when it stands in a plane other than the working plane.
;;;; In plan and in the working plane of a side view text is drawn with the screen fonts as usual.

(in-package :fancy-pencil)

(defvar *vector-font* nil)

(defun vector-glyph (code)
  "(advance contours) for character CODE, contours as lists of points (capital height 1)."
  (unless *vector-font*
    (setf *vector-font* (make-hash-table))
    (dolist (g *vector-font-data*)
      (destructuring-bind (c adv contours) g
        (setf (gethash c *vector-font*)
              (list (d adv) (mapcar (lambda (ct) (loop for (x y) on ct by #'cddr collect (cons (d x) (d y)))) contours))))))
  (or (gethash code *vector-font*) (gethash (char-code #\?) *vector-font*)))

(defun vector-string-width (s)
  (loop for ch across s sum (first (vector-glyph (char-code ch)))))

(defun string-strokes (s ox oy h wf obl)
  "Segments ((x y) (x y)) of string S starting at baseline point OX,OY, height H, width factor
WF and oblique angle OBL, in the text's own unrotated coordinates."
  (let ((x ox) (out nil) (sk (tan obl)))
    (loop for ch across s
          do (destructuring-bind (adv contours) (vector-glyph (char-code ch))
               (dolist (ct contours)
                 (loop for (a b) on ct while b
                       do (push (list (cons (+ x (* h wf (car a)) (* h (cdr a) sk)) (+ oy (* h (cdr a))))
                                      (cons (+ x (* h wf (car b)) (* h (cdr b) sk)) (+ oy (* h (cdr b)))))
                                out)))
               (incf x (* h wf adv))))
    out))

(defun text-stroke-pieces (data)
  "TEXT / MTEXT / ATTDEF / ATTRIB drawn as line strokes: (:gseg a b) pieces in the text's own plane
coordinates (as its insertion point)."
  (let* ((mtext (equal (dxf 0 data) "MTEXT"))
         (p (pt3 (or (dxf 10 data) (pt 0 0))))
         (z (pz p))
         (h (d (text-effective-height data)))
         (rot (d (or (dxf 50 data) 0d0)))
         (wf (if mtext 1d0 (d (or (dxf 41 data) 1d0))))
         (obl (if mtext 0d0 (d (or (dxf 51 data) 0d0))))
         (s (text-string-display (or (dxf 1 data) "")))
         (segs nil))
    (if mtext
        (let* ((width (and (dxf 41 data) (plusp (dxf 41 data)) (dxf 41 data)))
               (lines (mtext-wrapped-lines (loop with start = 0 for pos = (position #\Newline s :start start)
                                                 collect (subseq s start pos) while pos do (setf start (1+ pos)))
                                           h width))
               (n (length lines))
               (ws (mapcar (lambda (l) (* h (vector-string-width l))) lines))
               (w (or width (reduce #'max ws :initial-value 0d0)))
               (hh (* h 1.6d0 n))
               (attach (or (dxf 71 data) 1))
               (col (mod (1- attach) 3)) (row (floor (1- attach) 3))
               (x0 (- (px p) (* w (case col (0 0) (1 0.5d0) (t 1)))))
               (top (+ (py p) (* hh (case row (0 0) (1 0.5d0) (t 1))))))
          (loop for line in lines for lw in ws for i from 0
                for lx = (+ x0 (case col (0 0) (1 (/ (- w lw) 2)) (t (- w lw))))
                for by = (- top (* h (+ 1 (* 1.6d0 i))))
                do (setf segs (nconc (string-strokes line lx by h 1d0 0d0) segs))))
        (let* ((sw (* h wf (vector-string-width s)))
               (h72 (or (dxf 72 data) 0)) (h73 (or (dxf 73 data) 0))
               (ap (if (and (or (/= h72 0) (/= h73 0)) (dxf 11 data)) (pt3 (dxf 11 data)) p))
               ;; start of the baseline before rotation, from the justification
               (dx (case h72 ((1 4) (- (/ sw 2))) (2 (- sw)) (t 0d0)))
               (dy (cond ((= h72 4) (- (/ h 2))) (t (case h73 (1 (* 0.2d0 h)) (2 (- (/ h 2))) (3 (- h)) (t 0d0))))))
          (setf p ap)
          (setf segs (string-strokes s (+ (px ap) dx) (+ (py ap) dy) h wf obl))))
    ;; rotate about the insertion point
    (let ((c (cos rot)) (sn (sin rot)))
      (flet ((place (q) (let ((dx (- (car q) (px p))) (dy (- (cdr q) (py p))))
                          (pt (+ (px p) (- (* dx c) (* dy sn))) (+ (py p) (* dx sn) (* dy c)) z))))
        (mapcar (lambda (sg) (list :gseg (place (first sg)) (place (second sg)))) segs)))))

;;; ---------------------------------------------------------------- exploding text
;;; TXTEXP (and EXPLODE on text): letters become closed polylines tracing their outlines. The
;;; outlines come from the font file the text is shown in on the screen (ttf.lisp), so they match
;;; it; the built-in letter shapes are used when that font can't be read. Either way the capital
;;; letters are made the text's height, as AutoCAD does. MTEXT explodes into one TEXT per line first.

(defun text-glyphs (data)
  "Letter shapes for text DATA: (values GLYPH WIDTH). (GLYPH code) gives (advance contours) and
(WIDTH string) the string's width, in units of the capital-letter height."
  (let ((f (and (not (equal (dxf 0 data) "MTEXT")) (ignore-errors (text-font data)))))
    (if f
        (let ((k (/ 1d0 (ttf-cap-height f))))
          (values (lambda (code) (destructuring-bind (adv cts) (ttf-glyph f code)
                                   (list (* k adv) (mapcar (lambda (ct) (mapcar (lambda (q) (cons (* k (car q)) (* k (cdr q)))) ct)) cts))))
                  (lambda (s) (* k (ttf-string-width f s)))))
        (values #'vector-glyph #'vector-string-width))))

(defun text-line-layout (data)
  "How TEXT / MTEXT / ATTDEF / ATTRIB DATA is laid out: (values lines p rot h wf obl z), lines being
(string x y) - each line's baseline start in the text's own unrotated plane coordinates; P the point
it turns about."
  (let* ((mtext (equal (dxf 0 data) "MTEXT"))
         (p (pt3 (or (dxf 10 data) (pt 0 0))))
         (z (pz p))
         (h (d (text-effective-height data)))
         (rot (d (or (dxf 50 data) 0d0)))
         (wf (if mtext 1d0 (d (or (dxf 41 data) 1d0))))
         (obl (if mtext 0d0 (d (or (dxf 51 data) 0d0))))
         (s (text-string-display (or (dxf 1 data) ""))))
    (if mtext
        (let* ((width (and (dxf 41 data) (plusp (dxf 41 data)) (dxf 41 data)))
               (lines (mtext-wrapped-lines (loop with start = 0 for pos = (position #\Newline s :start start)
                                                 collect (subseq s start pos) while pos do (setf start (1+ pos)))
                                           h width))
               (n (length lines))
               (ws (mapcar (lambda (l) (* h (vector-string-width l))) lines))
               (w (or width (reduce #'max ws :initial-value 0d0)))
               (hh (* h 1.6d0 n))
               (attach (or (dxf 71 data) 1))
               (col (mod (1- attach) 3)) (row (floor (1- attach) 3))
               (x0 (- (px p) (* w (case col (0 0) (1 0.5d0) (t 1)))))
               (top (+ (py p) (* hh (case row (0 0) (1 0.5d0) (t 1))))))
          (values (loop for line in lines for lw in ws for i from 0
                        collect (list line
                                      (+ x0 (case col (0 0) (1 (/ (- w lw) 2)) (t (- w lw))))
                                      (- top (* h (+ 1 (* 1.6d0 i))))))
                  p rot h wf obl z))
        (let* ((sw (* h wf (funcall (nth-value 1 (text-glyphs data)) s)))
               (h72 (or (dxf 72 data) 0)) (h73 (or (dxf 73 data) 0))
               (ap (if (and (or (/= h72 0) (/= h73 0)) (dxf 11 data)) (pt3 (dxf 11 data)) p))
               (dx (case h72 ((1 4) (- (/ sw 2))) (2 (- sw)) (t 0d0)))
               (dy (cond ((= h72 4) (- (/ h 2))) (t (case h73 (1 (* 0.2d0 h)) (2 (- (/ h 2))) (3 (- h)) (t 0d0))))))
          (values (list (list s (+ (px ap) dx) (+ (py ap) dy))) ap rot h wf obl z)))))

(defun text-outline-contours (data)
  "The letters of text DATA as closed outlines: a list of point lists (each closed, last = first
dropped), in the text's own plane coordinates."
  (multiple-value-bind (lines p rot h wf obl z) (text-line-layout data)
    (let ((c (cos rot)) (sn (sin rot)) (sk (tan obl)) (out nil) (glyph (text-glyphs data)))
      (flet ((place (x y) (let ((dx (- x (px p))) (dy (- y (py p))))
                            (pt (+ (px p) (- (* dx c) (* dy sn))) (+ (py p) (* dx sn) (* dy c)) z))))
        (dolist (ln lines)
          (destructuring-bind (s ox oy) ln
            (let ((x ox))
              (loop for ch across s
                    do (destructuring-bind (adv contours) (funcall glyph (char-code ch))
                         (dolist (ct contours)
                           (let ((pts (mapcar (lambda (a) (place (+ x (* h wf (car a)) (* h (cdr a) sk)) (+ oy (* h (cdr a))))) ct)))
                             ;; the font's contours repeat the first point at the end
                             (when (and (cdr pts) (< (dist2d (first pts) (car (last pts))) 1d-9)) (setf pts (butlast pts)))
                             (when (cddr pts) (push pts out))))
                         (incf x (* h wf adv))))))))
      (nreverse out))))

(defparameter *text-props-kept* '(8 6 62 420 370 48 60 67 410)
  "Group codes copied from exploded text: layer, linetype, color, true color, lineweight, linetype
scale, visibility, paper space, layout.")

(defun text-kept-props (data)
  (remove-if-not (lambda (x) (and (consp x) (member (car x) *text-props-kept*))) data))

(defun text-outline-entities (data)
  "Closed LWPOLYLINEs tracing the letters of text DATA, in its plane."
  (let ((props (text-kept-props data)) (n (dxf 210 data)))
    (loop for ct in (text-outline-contours data)
          collect (append (list (cons 0 "LWPOLYLINE")) props
                          (list (cons 90 (length ct)) (cons 70 1) (cons 38 (pz (first ct))))
                          (loop for q in ct collect (cons 10 (pt (px q) (py q))))
                          (when n (list (cons 210 n)))))))

(defun mtext-to-text-entities (data)
  "MTEXT DATA as one TEXT per line, where the lines fall (AutoCAD's EXPLODE of MTEXT)."
  (multiple-value-bind (lines p rot h) (text-line-layout data)
    (let ((c (cos rot)) (sn (sin rot)) (props (text-kept-props data)) (n (dxf 210 data))
          (style (or (dxf 7 data) "Standard")))
      (loop for (s x y) in lines
            unless (string= (string-trim " " s) "")
              collect (let ((dx (- x (px p))) (dy (- y (py p))))
                        (append (list (cons 0 "TEXT")) props
                                (list (cons 10 (pt (+ (px p) (- (* dx c) (* dy sn))) (+ (py p) (* dx sn) (* dy c)) (pz p)))
                                      (cons 40 h) (cons 1 s) (cons 50 rot) (cons 7 style))
                                (when n (list (cons 210 n)))))))))

(defun text-entity-p (data) (member (dxf 0 data) '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal))

