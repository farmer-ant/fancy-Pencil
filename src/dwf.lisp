;;;; dwf.lisp — reading DWF files, for DWF underlays.
;;;; DWFx (AutoCAD 2009 and later) is a zip of XPS pages: each sheet a FixedPage of Path elements
;;;; (outlines in a small path language) and Glyphs (text), sized in 1/96 inch. Those are read into
;;;; lines and text, in inches with the sheet's lower-left corner at 0,0. A classic DWF (version 6)
;;;; is a zip too, but its sheets are W2D streams (Autodesk's binary format): for those only the
;;;; sheet names and paper sizes are read, so the underlay shows as its sheet's frame.

(in-package :fancy-pencil)

;;; ---------- a small XML reader: tags with attributes, in order ----------

(defun xml-unescape (s)
  (if (not (find #\& s)) s
      (with-output-to-string (o)
        (loop with i = 0 while (< i (length s))
              do (let ((c (char s i)))
                   (if (char= c #\&)
                       (let ((semi (position #\; s :start i)))
                         (if (null semi) (progn (write-char c o) (incf i))
                             (let ((ent (subseq s (1+ i) semi)))
                               (write-string
                                (cond ((string= ent "lt") "<") ((string= ent "gt") ">") ((string= ent "amp") "&")
                                      ((string= ent "quot") "\"") ((string= ent "apos") "'")
                                      ((and (> (length ent) 1) (char= (char ent 0) #\#))
                                       (string (code-char (or (ignore-errors
                                                               (if (char-equal (char ent 1) #\x)
                                                                   (parse-integer ent :start 2 :radix 16)
                                                                   (parse-integer ent :start 1)))
                                                              63))))
                                      (t ""))
                                o)
                               (setf i (1+ semi)))))
                       (progn (write-char c o) (incf i))))))))

(defun xml-tags (s)
  "Tags of XML text S in order: (name attrs close-p self-closing-p), attrs an alist of strings."
  (let ((out nil) (i 0) (n (length s)))
    (loop
      (let ((lt (position #\< s :start i)))
        (unless lt (return))
        (cond ((and (< (+ lt 3) n) (string= "<!--" s :start2 lt :end2 (min n (+ lt 4))))
               (setf i (let ((e (search "-->" s :start2 lt))) (if e (+ e 3) n))))
              ((and (< (1+ lt) n) (member (char s (1+ lt)) '(#\? #\!)))
               (setf i (1+ (or (position #\> s :start lt) (1- n)))))
              (t
               (let* ((gt (or (position #\> s :start lt) n))
                      (body (subseq s (1+ lt) gt))
                      (close (and (plusp (length body)) (char= (char body 0) #\/)))
                      (self (and (plusp (length body)) (char= (char body (1- (length body))) #\/)))
                      (body (string-trim "/ " body))
                      (sp (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) body))
                      (name (subseq body 0 (or sp (length body))))
                      (attrs nil))
                 (when sp
                   (let ((j sp) (m (length body)))
                     (loop
                       (let ((eq (position #\= body :start j)))
                         (unless eq (return))
                         (let* ((an (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq body j eq)))
                                (q (position-if (lambda (c) (member c '(#\" #\'))) body :start eq))
                                (qe (and q (position (char body q) body :start (1+ q)))))
                           (unless qe (return))
                           (push (cons an (xml-unescape (subseq body (1+ q) qe))) attrs)
                           (setf j (1+ qe))
                           (when (>= j m) (return)))))))
                 (push (list name (nreverse attrs) close self) out)
                 (setf i (1+ gt)))))))
    (nreverse out)))

(defun attr (attrs name) (cdr (assoc name attrs :test #'string-equal)))

;;; ---------- XPS geometry ----------

(defun xps-numbers (s)
  (let ((out nil) (i 0) (n (length s)))
    (loop while (< i n)
          do (let ((st (position-if (lambda (c) (or (digit-char-p c) (find c "-+."))) s :start i)))
               (unless st (return))
               (let ((en (or (position-if-not (lambda (c) (or (digit-char-p c) (find c ".eE"))) s :start (1+ st)) n)))
                 ;; an exponent's sign belongs to the number
                 (loop while (and (< en n) (find (char s en) "+-") (find (char s (1- en)) "eE"))
                       do (setf en (or (position-if-not (lambda (c) (or (digit-char-p c) (find c ".eE"))) s :start (1+ en)) n)))
                 (let ((v (ignore-errors (let ((*read-default-float-format* 'double-float))
                                           (parse-xps-real (subseq s st en))))))
                   (when v (push v out)))
                 (setf i en))))
    (nreverse out)))

(defun parse-xps-real (s)
  "A decimal number such as -12.5 or 3e-4."
  (let* ((epos (position-if (lambda (c) (char-equal c #\e)) s))
         (mant (subseq s 0 (or epos (length s)))) (ex (if epos (parse-integer s :start (1+ epos)) 0))
         (neg (and (plusp (length mant)) (char= (char mant 0) #\-)))
         (mant (string-left-trim "+-" mant))
         (dot (position #\. mant))
         (ip (if (and dot (zerop dot)) 0 (parse-integer mant :end (or dot (length mant)))))
         (fp (if (and dot (< (1+ dot) (length mant))) (/ (parse-integer mant :start (1+ dot)) (expt 10d0 (- (length mant) dot 1))) 0d0)))
    (* (if neg -1 1) (+ ip fp) (expt 10d0 ex))))

(defun xps-matrix (s)
  "An XPS RenderTransform \"m11,m12,m21,m22,dx,dy\" as a list, or NIL."
  (let ((v (and s (xps-numbers s)))) (and (= (length v) 6) v)))

(defun xps-mat* (a b)
  "A then B (row-vector convention, as XPS): the combined matrix."
  (destructuring-bind (a11 a12 a21 a22 adx ady) a
    (destructuring-bind (b11 b12 b21 b22 bdx bdy) b
      (list (+ (* a11 b11) (* a12 b21)) (+ (* a11 b12) (* a12 b22))
            (+ (* a21 b11) (* a22 b21)) (+ (* a21 b12) (* a22 b22))
            (+ (* adx b11) (* ady b21) bdx) (+ (* adx b12) (* ady b22) bdy)))))

(defun xps-mat-apply (m x y)
  (destructuring-bind (m11 m12 m21 m22 dx dy) m
    (values (+ (* x m11) (* y m21) dx) (+ (* x m12) (* y m22) dy))))

(defun xps-path-figures (data)
  "The figures of an XPS path's Data: list of (points . closed-p) in path coordinates."
  (let ((figs nil) (cur nil) (x 0d0) (y 0d0) (sx 0d0) (sy 0d0) (lcx 0d0) (lcy 0d0)
        (i 0) (n (length data)) (cmd #\M))
    (labels ((finish (closed) (when (>= (length cur) 2) (push (cons (nreverse cur) closed) figs)) (setf cur nil))
             (to (nx ny) (setf x nx y ny) (push (cons x y) cur))
             (nums (k)                ; read K numbers after position i
               (let ((out nil))
                 (dotimes (j k (nreverse out))
                   (let ((st (position-if (lambda (c) (or (digit-char-p c) (find c "-+."))) data :start i)))
                     (when (or (null st) (let ((l (position-if #'alpha-char-p data :start i))) (and l (< l st) (not (char-equal (char data l) #\e)))))
                       (return-from nums nil))
                     (let ((en (or (position-if-not (lambda (c) (or (digit-char-p c) (find c ".eE"))) data :start (1+ st)) n)))
                       (loop while (and (< en n) (find (char data en) "+-") (find (char data (1- en)) "eE"))
                             do (setf en (or (position-if-not (lambda (c) (or (digit-char-p c) (find c ".eE"))) data :start (1+ en)) n)))
                       (push (parse-xps-real (subseq data st en)) out)
                       (setf i en))))))
             (bez (x1 y1 x2 y2 x3 y3)
               (let ((x0 x) (y0 y))
                 (loop for k from 1 to 8
                       for tt = (/ k 8d0) for u = (- 1 tt)
                       do (to (+ (* u u u x0) (* 3 u u tt x1) (* 3 u tt tt x2) (* tt tt tt x3))
                              (+ (* u u u y0) (* 3 u u tt y1) (* 3 u tt tt y2) (* tt tt tt y3)))))))
      (loop
        (let ((c (position-if (lambda (ch) (or (alpha-char-p ch) (digit-char-p ch) (find ch "-+."))) data :start i)))
          (unless c (return))
          (setf i c)
          (let ((ch (char data i)))
            (cond ((and (char-equal ch #\F) (< (1+ i) n) (find (char data (1+ i)) "01")) (incf i 2))
                  ((alpha-char-p ch) (setf cmd ch) (incf i)
                   (when (char-equal ch #\Z) (when cur (push (car (last cur)) cur)) (finish t) (setf x sx y sy)))
                  (t
                   (let ((rel (lower-case-p cmd)) (ox x) (oy y))
                     (case (char-upcase cmd)
                       (#\M (let ((v (nums 2))) (unless v (return))
                              (finish nil)
                              (to (+ (first v) (if rel ox 0)) (+ (second v) (if rel oy 0)))
                              (setf sx x sy y cmd (if rel #\l #\L))))
                       (#\L (let ((v (nums 2))) (unless v (return)) (to (+ (first v) (if rel ox 0)) (+ (second v) (if rel oy 0)))))
                       (#\H (let ((v (nums 1))) (unless v (return)) (to (+ (first v) (if rel ox 0)) y)))
                       (#\V (let ((v (nums 1))) (unless v (return)) (to x (+ (first v) (if rel oy 0)))))
                       (#\C (let ((v (nums 6))) (unless v (return))
                              (flet ((ax (k) (+ (nth k v) (if rel ox 0))) (ay (k) (+ (nth k v) (if rel oy 0))))
                                (setf lcx (ax 2) lcy (ay 3))
                                (bez (ax 0) (ay 1) (ax 2) (ay 3) (ax 4) (ay 5)))))
                       (#\S (let ((v (nums 4))) (unless v (return))
                              (flet ((ax (k) (+ (nth k v) (if rel ox 0))) (ay (k) (+ (nth k v) (if rel oy 0))))
                                (let ((x1 (- (* 2 x) lcx)) (y1 (- (* 2 y) lcy)))
                                  (setf lcx (ax 0) lcy (ay 1))
                                  (bez x1 y1 (ax 0) (ay 1) (ax 2) (ay 3))))))
                       (#\Q (let ((v (nums 4))) (unless v (return))
                              (flet ((ax (k) (+ (nth k v) (if rel ox 0))) (ay (k) (+ (nth k v) (if rel oy 0))))
                                (bez (+ x (* 2/3 (- (ax 0) x))) (+ y (* 2/3 (- (ay 1) y)))
                                     (+ (ax 2) (* 2/3 (- (ax 0) (ax 2)))) (+ (ay 3) (* 2/3 (- (ay 1) (ay 3))))
                                     (ax 2) (ay 3)))))
                       (#\A (let ((v (nums 7))) (unless v (return))
                              ;; an elliptical arc: drawn through points along its chord's bulge
                              (let* ((ex (+ (nth 5 v) (if rel ox 0))) (ey (+ (nth 6 v) (if rel oy 0)))
                                     (r (max 1d-9 (abs (first v)))) (chord (sqrt (+ (expt (- ex x) 2) (expt (- ey y) 2)))))
                                (if (or (< chord 1d-9) (> chord (* 2 r)))
                                    (to ex ey)
                                    (let* ((large (/= (nth 3 v) 0)) (sweep (/= (nth 4 v) 0))
                                           (mx (/ (+ x ex) 2)) (my (/ (+ y ey) 2))
                                           (h (sqrt (max 0d0 (- (* r r) (expt (/ chord 2) 2)))))
                                           (ux (/ (- ex x) chord)) (uy (/ (- ey y) chord))
                                           (sgn (if (eq large sweep) -1 1))
                                           (cx (- mx (* sgn h uy))) (cy (+ my (* sgn h ux)))
                                           (a0 (atan (- y cy) (- x cx))) (a1 (atan (- ey cy) (- ex cx)))
                                           (da (- a1 a0)))
                                      (if sweep (when (< da 0) (incf da (* 2 +pi+))) (when (> da 0) (decf da (* 2 +pi+))))
                                      (loop for k from 1 to 16
                                            do (let ((a (+ a0 (* da (/ k 16d0))))) (to (+ cx (* r (cos a))) (+ cy (* r (sin a)))))))))))
                       (t (incf i)))))))))
      (finish nil)
      (nreverse figs))))

(defun xps-page-entities (xml)
  "Lines and text of one XPS FixedPage, in inches, y up from the page's lower-left corner:
(values entities width-inches height-inches)."
  (let* ((tags (xml-tags xml))
         (page (find "FixedPage" tags :key #'first :test #'string-equal))
         (w (or (ignore-errors (parse-xps-real (attr (second page) "Width"))) 816d0))
         (h (or (ignore-errors (parse-xps-real (attr (second page) "Height"))) 1056d0))
         (stack (list (list 1d0 0d0 0d0 1d0 0d0 0d0)))
         (out nil))
    (flet ((pt-of (m x y) (multiple-value-bind (a b) (xps-mat-apply m x y) (pt (/ a 96d0) (/ (- h b) 96d0)))))
      (dolist (tg tags)
        (destructuring-bind (name attrs close self) tg
          (cond
            ((and close (member name '("Canvas") :test #'string-equal)) (when (cdr stack) (pop stack)))
            ((string-equal name "Canvas")
             (let ((m (xps-matrix (attr attrs "RenderTransform"))))
               (unless self (push (if m (xps-mat* m (first stack)) (first stack)) stack))))
            ((and (string-equal name "Path") (not close))
             (let* ((m (let ((r (xps-matrix (attr attrs "RenderTransform")))) (if r (xps-mat* r (first stack)) (first stack))))
                    (data (attr attrs "Data")))
               (when data
                 (dolist (fig (xps-path-figures data))
                   (let ((pts (mapcar (lambda (q) (pt-of m (car q) (cdr q))) (car fig))))
                     (when (>= (length pts) 2) (push (make-lwpoly-data pts (cdr fig)) out)))))))
            ((and (string-equal name "Glyphs") (not close))
             (let* ((m (let ((r (xps-matrix (attr attrs "RenderTransform")))) (if r (xps-mat* r (first stack)) (first stack))))
                    (s (attr attrs "UnicodeString"))
                    (x (or (ignore-errors (parse-xps-real (attr attrs "OriginX"))) 0d0))
                    (y (or (ignore-errors (parse-xps-real (attr attrs "OriginY"))) 0d0))
                    (em (or (ignore-errors (parse-xps-real (attr attrs "FontRenderingEmSize"))) 12d0)))
               (when (and s (plusp (length (string-trim " " s))))
                 (let* ((o (pt-of m x y)) (o2 (pt-of m (+ x 10) y))
                        (scale (/ (dist2d o o2) (/ 10 96d0))))
                   (push (list (cons 0 "TEXT") (cons 10 o) (cons 40 (* 0.7d0 scale (/ em 96d0)))
                               (cons 1 (string-left-trim "{}" s)) (cons 50 (atan (- (py o2) (py o)) (- (px o2) (px o))))
                               (cons 7 "Standard"))
                         out))))))))
      (values (nreverse out) (/ w 96d0) (/ h 96d0)))))

(defun dwf-sheets (path)
  "The sheets of DWF or DWFx file PATH: list of (name entities width height), entities NIL when the
sheet's drawing can't be read (a classic DWF's W2D stream)."
  (let* ((bytes (read-file-bytes path))
         (entries (zip-entries bytes)))
    (unless entries (return-from dwf-sheets nil))
    (let ((pages (remove-if-not (lambda (e) (search ".fpage" (first e) :test #'char-equal)) entries)))
      (if pages
          (loop for e in pages for i from 1
                collect (multiple-value-bind (ents w h) (xps-page-entities (octets-string (zip-entry-bytes bytes e)))
                          (list (format nil "Sheet ~D" i) ents w h)))
          ;; a classic DWF: sheet names and paper sizes from its descriptors
          (let ((out nil))
            (dolist (e entries)
              (when (search "descriptor.xml" (first e) :test #'char-equal)
                (let* ((tags (xml-tags (octets-string (zip-entry-bytes bytes e))))
                       (sec (find-if (lambda (tg) (and (search "Section" (first tg)) (attr (second tg) "title"))) tags))
                       (paper (find-if (lambda (tg) (search "Paper" (first tg))) tags)))
                  (when paper
                    (let* ((units (or (attr (second paper) "units") "in"))
                           (k (if (string-equal units "mm") (/ 1 25.4d0) 1d0))
                           (w (ignore-errors (* k (parse-xps-real (attr (second paper) "width")))))
                           (h (ignore-errors (* k (parse-xps-real (attr (second paper) "height"))))))
                      (when (and w h)
                        (push (list (or (and sec (attr (second sec) "title")) (format nil "Sheet ~D" (1+ (length out))))
                                    nil w h)
                              out)))))))
            (nreverse out))))))
