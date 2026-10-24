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
