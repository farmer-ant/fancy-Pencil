;;;; textstyle.lisp — text styles (the STYLE table), font lookup for display, and the
;;;; whole-object MTEXT formatting codes ({\fArial|b1|i0;\L...}) that AutoCAD writes.

(in-package :fancy-pencil)

(defstruct textstyle
  (name "Standard")
  (font "Arial")        ; a font family ("Arial", "DejaVu Serif") or a font file ("txt.shx", "romans.shx")
  (bigfont "")
  (height 0d0)          ; 0 = asked for when text is placed
  (width 1d0)           ; width factor
  (oblique 0d0)         ; radians
  (bold nil) (italic nil)
  (backwards nil) (upside-down nil))

(defun all-textstyles ()
  (unless (find "Standard" (drawing-styles *dwg*) :key #'textstyle-name :test #'string-equal)
    (setf (drawing-styles *dwg*) (cons (make-textstyle) (drawing-styles *dwg*))))
  (drawing-styles *dwg*))

(defun find-textstyle (name)
  (and name (find name (all-textstyles) :key #'textstyle-name :test #'string-equal)))

(defun current-textstyle ()
  (or (find-textstyle (sysvar "TEXTSTYLE")) (find-textstyle "Standard")))

(defun define-textstyle (style)
  "Add STYLE, replacing one of the same name."
  (all-textstyles)
  (setf (drawing-styles *dwg*)
        (append (remove (textstyle-name style) (drawing-styles *dwg*) :key #'textstyle-name :test #'string-equal)
                (list style)))
  style)

(defparameter *mtext-justify*
  '(("TL" . 1) ("TC" . 2) ("TR" . 3) ("ML" . 4) ("MC" . 5) ("MR" . 6) ("BL" . 7) ("BC" . 8) ("BR" . 9)))

;;; ---------- fonts ----------

(defparameter *shx-fonts* '("txt.shx" "simplex.shx" "romans.shx" "romand.shx" "romanc.shx" "romant.shx"
                            "isocp.shx" "isoct.shx" "italic.shx" "monotxt.shx" "complex.shx" "gothice.shx" "scripts.shx")
  "AutoCAD shape fonts offered in the font lists. They are shown with a similar system font.")

(defun font-family-for (font)
  "Tk / fontconfig family to draw FONT with. Font files map to look-alike families; fontconfig
substitutes metric-compatible fonts (Arial -> Liberation Sans) when the named one isn't installed."
  (let* ((f (string-trim " " (or font "")))
         (base (string-downcase (pathname-name (pathname (substitute #\/ #\\ f)))))
         (ext (string-downcase (or (pathname-type (pathname (substitute #\/ #\\ f))) ""))))
    (cond
      ((string= f "") "Helvetica")
      ((member ext '("shx") :test #'string=)
       (cond ((member base '("romanc" "romant" "romand" "complex" "italic" "isoct") :test #'string=) "Times")
             ((member base '("monotxt") :test #'string=) "Courier")
             (t "Helvetica")))
      ((member ext '("ttf" "otf" "ttc") :test #'string=)
       (cond ((search "arial" base) "Arial")
             ((or (search "times" base) (string= base "tnr")) "Times New Roman")
             ((search "cour" base) "Courier New")
             ((search "calibri" base) "Calibri")
             ((search "verdana" base) "Verdana")
             ((search "tahoma" base) "Tahoma")
             ((search "isocpeur" base) "Helvetica")
             (t (pathname-name (pathname (substitute #\/ #\\ f))))))
      (t f))))

(defun style-display-font (style)
  (if style (textstyle-font style) "Arial"))

(defvar *tk-families* nil "Font families Tk can use (filled when the window opens).")

(defparameter *family-fallbacks*
  '(("Arial" "Liberation Sans" "Arimo" "DejaVu Sans" "Helvetica")
    ("Helvetica" "Liberation Sans" "Arimo" "DejaVu Sans" "Helvetica")
    ("Times New Roman" "Liberation Serif" "Tinos" "DejaVu Serif" "Times")
    ("Times" "Liberation Serif" "Tinos" "DejaVu Serif" "Times")
    ("Courier New" "Liberation Mono" "Cousine" "DejaVu Sans Mono" "Courier")
    ("Courier" "Liberation Mono" "Cousine" "DejaVu Sans Mono" "Courier")
    ("Calibri" "Carlito" "Liberation Sans" "DejaVu Sans" "Helvetica")
    ("Cambria" "Caladea" "Liberation Serif" "DejaVu Serif" "Times")))

(defun installed-family (family)
  "FAMILY if it is installed, else a metric-compatible look-alike, else Helvetica (Tk always has it)."
  (flet ((have (f) (find f *tk-families* :test #'string-equal)))
    (cond ((null *tk-families*) family)
          ((have family))
          (t (or (some #'have (rest (assoc family *family-fallbacks* :test #'string-equal)))
                 (let ((lf (string-downcase family)))
                   (cond ((or (search "mono" lf) (search "cour" lf)) (or (have "DejaVu Sans Mono") "Courier"))
                         ((or (search "serif" lf) (search "roman" lf) (search "times" lf)) (or (have "DejaVu Serif") "Times"))
                         (t "Helvetica"))))))))

;;; ---------- whole-object MTEXT formatting ----------

(defun mtext-overall-format (content)
  "Formatting codes at the very start of CONTENT that apply to the whole MTEXT.
Returns a plist (:font F :bold B :italic I :underline U :height H :relative-height R)."
  (let ((i 0) (n (length content)) (res nil))
    (loop
      (cond ((>= i n) (return))
            ((char= (char content i) #\{) (incf i))
            ((and (char= (char content i) #\\) (< (1+ i) n))
             (let ((c (char content (1+ i))))
               (case c
                 ((#\f #\F)
                  (let* ((semi (or (position #\; content :start i) n))
                         (parts (split-string (subseq content (+ i 2) semi) #\|)))
                    (setf (getf res :font) (first parts))
                    (dolist (p (rest parts))
                      (when (> (length p) 1)
                        (case (char p 0)
                          (#\b (setf (getf res :bold) (string= (subseq p 1) "1")))
                          (#\i (setf (getf res :italic) (string= (subseq p 1) "1"))))))
                    (setf i (1+ semi))))
                 ((#\H)
                  (let* ((semi (or (position #\; content :start i) n))
                         (txt (subseq content (+ i 2) semi))
                         (rel (and (plusp (length txt)) (char-equal (char txt (1- (length txt))) #\x)))
                         (v (ignore-errors (parse-number-text (string-right-trim "xX" txt)))))
                    (when (and v (plusp v))
                      (if rel (setf (getf res :relative-height) (d v)) (setf (getf res :height) (d v))))
                    (setf i (1+ semi))))
                 ((#\L) (setf (getf res :underline) t) (incf i 2))
                 ((#\C #\c #\A #\Q #\W #\T)
                  (setf i (1+ (or (position #\; content :start i) (1- n)))))
                 (t (return)))))
            (t (return))))
    res))

(defun split-string (s ch)
  (loop with start = 0 for pos = (position ch s :start start)
        collect (subseq s start pos) while pos do (setf start (1+ pos))))

(defun mtext-make-content (body &key font bold italic underline)
  "MTEXT content: BODY (already MTEXT-encoded) wrapped with whole-object formatting codes."
  (let ((prefix (with-output-to-string (o)
                  (when (or font bold italic)
                    (format o "\\f~A|b~D|i~D|c0|p0;" (or font "Arial") (if bold 1 0) (if italic 1 0)))
                  (when underline (write-string "\\L" o)))))
    (if (string= prefix "") body (concatenate 'string "{" prefix body "}"))))

(defun text-font-spec (data px-size)
  "Tk font description for a TEXT/MTEXT entity drawn PX-SIZE pixels high."
  (let* ((style (or (find-textstyle (dxf 7 data)) (find-textstyle "Standard")))
         (fmt (and (equal (dxf 0 data) "MTEXT") (mtext-overall-format (or (dxf 1 data) ""))))
         (family (installed-family (font-family-for (or (getf fmt :font) (style-display-font style)))))
         (bold (if (getf fmt :font) (getf fmt :bold) (or (getf fmt :bold) (and style (textstyle-bold style)))))
         (italic (if (getf fmt :font) (getf fmt :italic) (or (getf fmt :italic) (and style (textstyle-italic style))))))
    (format nil "{~A} -~D~:[~; bold~]~:[~; italic~]~:[~; underline~]"
            family px-size bold italic (getf fmt :underline))))

(defun text-effective-height (data)
  "Drawn height of DATA, honouring an MTEXT whole-object \\H override."
  (let* ((h (or (dxf 40 data) 0.2d0))
         (fmt (and (equal (dxf 0 data) "MTEXT") (mtext-overall-format (or (dxf 1 data) "")))))
    (cond ((getf fmt :height) (getf fmt :height))
          ((getf fmt :relative-height) (* h (getf fmt :relative-height)))
          (t h))))

;;; ---------- DXF ----------

(defun styles-for-export ()
  "Every text style to write: the defined ones plus any name an entity uses without a definition."
  (let ((defined (all-textstyles)))
    (append defined
            (loop for n in (remove-duplicates
                            (remove-if-not #'stringp
                                           (append (mapcar (lambda (e) (dxf 7 (ename-data e))) (live-entities))
                                                   (loop for b in (drawing-blocks *dwg*)
                                                         append (mapcar (lambda (dd) (dxf 7 dd)) (block-def-entities b)))))
                            :test #'string-equal)
                  unless (or (find n defined :key #'textstyle-name :test #'string-equal) (string= n ""))
                    collect (make-textstyle :name n)))))

(defun style-font-file (style)
  "Value for group 3: the font file name AutoCAD looks for."
  (let ((f (textstyle-font style)))
    (if (pathname-type (pathname f)) f
        (let ((key (string-downcase (remove #\Space f))))
          (cond ((string= key "arial") "arial.ttf")
                ((string= key "timesnewroman") "times.ttf")
                ((string= key "couriernew") "cour.ttf")
                (t (concatenate 'string key ".ttf")))))))

(defun style-truetype-p (style)
  (not (string-equal (or (pathname-type (pathname (textstyle-font style))) "") "shx")))

(defun style-xdata-flags (style)
  (logior (if (textstyle-bold style) #x2000000 0) (if (textstyle-italic style) #x1000000 0) 34))

(defun style-gen-flags (style)
  (logior (if (textstyle-backwards style) 2 0) (if (textstyle-upside-down style) 4 0)))

(defun import-style-record (g)
  "Define a text style from a DXF STYLE table record (an alist of groups)."
  (let ((name (dxf 2 g)) (flags (or (dxf 70 g) 0)))
    (when (and name (plusp (length name)) (not (logtest 1 flags)))   ; flag 1 = shape file, not a text style
      (let* ((family (let ((x (member 1001 g :key (lambda (p) (and (consp p) (car p))))))
                       (and x (equal (cdr (first x)) "ACAD") (dxf 1000 (rest x)))))
             (xflags (or (dxf 1071 g) 0))
             (gen (or (dxf 71 g) 0)))
        (define-textstyle
            (make-textstyle :name name
                            :font (if (and family (plusp (length family))) family (or (dxf 3 g) "txt.shx"))
                            :bigfont (or (dxf 4 g) "")
                            :height (d (or (dxf 40 g) 0)) :width (d (or (dxf 41 g) 1))
                            :oblique (* (d (or (dxf 50 g) 0)) (/ pi 180))
                            :bold (logtest #x2000000 xflags) :italic (logtest #x1000000 xflags)
                            :backwards (logtest 2 gen) :upside-down (logtest 4 gen)))))))
