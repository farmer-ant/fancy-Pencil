;;;; ttf.lisp - letter outlines read from TrueType font files (the "glyf" kind: .ttf, .ttc, and
;;;; .otf files that carry TrueType outlines), so TXTEXP and EXPLODE trace exactly the letters
;;;; that are drawn on the screen. Fonts with PostScript (CFF) outlines aren't read; for those,
;;;; and when no font file is found, the built-in letter shapes (vector-font.lisp) are used.
;;;;
;;;; Units: glyphs come back in em units (1 = the font's em size), y up, baseline at 0.

(in-package :fancy-pencil)

(defstruct ttf
  path bytes (units 2048) (loc-format 0) (nglyphs 0) (nhmetrics 0)
  tables cmap-offset cmap-format (ascent 0.9d0) (descent 0.2d0) cap
  (glyphs (make-hash-table)))

(defvar *ttf-cache* (make-hash-table :test 'equal) "Fonts read, by file path (NIL: not readable).")
(defvar *font-file-cache* (make-hash-table :test 'equal) "fc-match answers, by family/bold/italic.")

(defun ttf-u8 (f o) (aref (ttf-bytes f) o))
(defun ttf-u16 (f o) (logior (ash (ttf-u8 f o) 8) (ttf-u8 f (1+ o))))
(defun ttf-s16 (f o) (let ((v (ttf-u16 f o))) (if (>= v #x8000) (- v #x10000) v)))
(defun ttf-u32 (f o) (logior (ash (ttf-u16 f o) 16) (ttf-u16 f (+ o 2))))
(defun ttf-table (f tag) (cdr (assoc tag (ttf-tables f) :test #'string=)))

(defun read-file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence b in) b)))

(defun ttf-open (path)
  "The font in PATH, or NIL when it can't be read (or has no TrueType outlines)."
  (multiple-value-bind (f found) (gethash path *ttf-cache*)
    (when found (return-from ttf-open f)))
  (setf (gethash path *ttf-cache*)
        (ignore-errors
         (let* ((f (make-ttf :path path :bytes (read-file-bytes path)))
                (base (if (= (ttf-u32 f 0) #x74746366) (ttf-u32 f 12) 0)) ; 'ttcf': the first font
                (n (ttf-u16 f (+ base 4))))
           (setf (ttf-tables f)
                 (loop for i below n for r = (+ base 12 (* 16 i))
                       collect (cons (map 'string #'code-char (subseq (ttf-bytes f) r (+ r 4))) (ttf-u32 f (+ r 8)))))
           (unless (and (ttf-table f "glyf") (ttf-table f "loca") (ttf-table f "cmap")) (return-from ttf-open nil))
           (let ((head (ttf-table f "head")) (hhea (ttf-table f "hhea")) (maxp (ttf-table f "maxp")))
             (setf (ttf-units f) (ttf-u16 f (+ head 18))
                   (ttf-loc-format f) (ttf-s16 f (+ head 50))
                   (ttf-nglyphs f) (ttf-u16 f (+ maxp 4))
                   (ttf-nhmetrics f) (ttf-u16 f (+ hhea 34))
                   (ttf-ascent f) (/ (d (ttf-s16 f (+ hhea 4))) (ttf-units f))
                   (ttf-descent f) (/ (d (abs (ttf-s16 f (+ hhea 6)))) (ttf-units f))))
           ;; the Unicode character map: format 12 (full Unicode) or 4 (basic)
           (let* ((cm (ttf-table f "cmap")) (n (ttf-u16 f (+ cm 2))) best)
             (loop for i below n for r = (+ cm 4 (* 8 i))
                   for plat = (ttf-u16 f r) for enc = (ttf-u16 f (+ r 2)) for off = (+ cm (ttf-u32 f (+ r 4)))
                   for fmt = (ttf-u16 f off)
                   do (when (and (member fmt '(4 12)) (or (= plat 0) (and (= plat 3) (member enc '(1 10)))))
                        (when (or (null best) (= fmt 12)) (setf best (cons off fmt)))))
             (unless best (return-from ttf-open nil))
             (setf (ttf-cmap-offset f) (car best) (ttf-cmap-format f) (cdr best)))
           f))))

(defun ttf-glyph-index (f code)
  (let ((o (ttf-cmap-offset f)))
    (if (= (ttf-cmap-format f) 12)
        (loop with n = (ttf-u32 f (+ o 12))
              for i below n for r = (+ o 16 (* 12 i))
              for start = (ttf-u32 f r) for end = (ttf-u32 f (+ r 4))
              when (<= start code end) return (+ (ttf-u32 f (+ r 8)) (- code start))
              finally (return 0))
        (let ((segs (floor (ttf-u16 f (+ o 6)) 2)))
          (when (> code #xFFFF) (return-from ttf-glyph-index 0))
          (loop for i below segs
                for end = (ttf-u16 f (+ o 14 (* 2 i)))
                when (>= end code)
                  do (let* ((start (ttf-u16 f (+ o 16 (* 2 segs) (* 2 i))))
                            (delta (ttf-s16 f (+ o 16 (* 4 segs) (* 2 i))))
                            (ro-pos (+ o 16 (* 6 segs) (* 2 i)))
                            (ro (ttf-u16 f ro-pos)))
                       (return
                         (cond ((< code start) 0)
                               ((zerop ro) (logand (+ code delta) #xFFFF))
                               (t (let ((g (ttf-u16 f (+ ro-pos ro (* 2 (- code start))))))
                                    (if (zerop g) 0 (logand (+ g delta) #xFFFF)))))))
                finally (return 0))))))

(defun ttf-advance (f gi)
  (let ((hmtx (ttf-table f "hmtx")) (k (min gi (1- (ttf-nhmetrics f)))))
    (ttf-u16 f (+ hmtx (* 4 k)))))

(defun ttf-glyph-offset (f gi)
  "Where glyph GI's data starts in the file, or NIL for an empty glyph (a space)."
  (let* ((loca (ttf-table f "loca")) (glyf (ttf-table f "glyf"))
         (a (if (zerop (ttf-loc-format f)) (* 2 (ttf-u16 f (+ loca (* 2 gi)))) (ttf-u32 f (+ loca (* 4 gi)))))
         (b (if (zerop (ttf-loc-format f)) (* 2 (ttf-u16 f (+ loca (* 2 (1+ gi))))) (ttf-u32 f (+ loca (* 4 (1+ gi)))))))
    (and (< a b) (+ glyf a))))

(defun ttf-contours-raw (f gi &optional (depth 0))
  "Glyph GI's contours as lists of (x y on-curve-p), in font units."
  (let ((o (ttf-glyph-offset f gi)))
    (when (or (null o) (> depth 8)) (return-from ttf-contours-raw nil))
    (let ((nc (ttf-s16 f o)))
      (if (>= nc 0)
          ;; a simple glyph
          (let* ((ends (loop for i below nc collect (ttf-u16 f (+ o 10 (* 2 i)))))
                 (npts (if ends (1+ (car (last ends))) 0))
                 (p (+ o 10 (* 2 nc)))
                 (p (+ p 2 (ttf-u16 f p)))          ; skip the hinting instructions
                 (flags (make-array npts)) (xs (make-array npts)) (ys (make-array npts)))
            (let ((i 0))
              (loop while (< i npts)
                    do (let ((fl (ttf-u8 f p)))
                         (incf p)
                         (setf (aref flags i) fl) (incf i)
                         (when (logtest fl 8)        ; repeated
                           (let ((r (ttf-u8 f p)))
                             (incf p)
                             (dotimes (k r) (when (< i npts) (setf (aref flags i) fl) (incf i))))))))
            (flet ((coords (arr short same)
                     (let ((v 0))
                       (dotimes (i npts)
                         (let ((fl (aref flags i)))
                           (cond ((logtest fl short)
                                  (let ((dv (ttf-u8 f p))) (incf p) (incf v (if (logtest fl same) dv (- dv)))))
                                 ((not (logtest fl same))
                                  (incf v (ttf-s16 f p)) (incf p 2))))
                         (setf (aref arr i) v)))))
              (coords xs 2 16)
              (coords ys 4 32))
            (let ((start 0))
              (loop for e in ends
                    collect (prog1 (loop for i from start to e
                                         collect (list (aref xs i) (aref ys i) (logtest (aref flags i) 1)))
                              (setf start (1+ e))))))
          ;; a composite glyph: other glyphs, moved and scaled
          (let ((p (+ o 10)) (out nil))
            (loop
              (let* ((fl (ttf-u16 f p)) (sub (ttf-u16 f (+ p 2))) dx dy (a 1d0) (b 0d0) (c 0d0) (dd 1d0))
                (incf p 4)
                (if (logtest fl 1)
                    (setf dx (ttf-s16 f p) dy (ttf-s16 f (+ p 2)) p (+ p 4))
                    (setf dx (let ((v (ttf-u8 f p))) (if (> v 127) (- v 256) v))
                          dy (let ((v (ttf-u8 f (1+ p)))) (if (> v 127) (- v 256) v)) p (+ p 2)))
                (unless (logtest fl 2) (setf dx 0 dy 0)) ; matched points: rare, not followed
                (flet ((f2dot14 (q) (/ (d (ttf-s16 f q)) 16384)))
                  (cond ((logtest fl 8) (setf a (f2dot14 p) dd a p (+ p 2)))
                        ((logtest fl #x40) (setf a (f2dot14 p) dd (f2dot14 (+ p 2)) p (+ p 4)))
                        ((logtest fl #x80) (setf a (f2dot14 p) b (f2dot14 (+ p 2)) c (f2dot14 (+ p 4)) dd (f2dot14 (+ p 6)) p (+ p 8)))))
                (dolist (ct (ttf-contours-raw f sub (1+ depth)))
                  (push (mapcar (lambda (q) (destructuring-bind (x y on) q
                                              (list (+ (* a x) (* c y) dx) (+ (* b x) (* dd y) dy) on)))
                                ct)
                        out))
                (unless (logtest fl #x20) (return))))
            (nreverse out))))))

(defun quad-contour-points (ct &optional (steps 6))
  "A TrueType contour (points with on/off-curve flags) as a closed list of points along it,
the curves split into STEPS straight pieces."
  (let* ((n (length ct)) (v (coerce ct 'vector)) (out nil))
    (when (< n 2) (return-from quad-contour-points nil))
    (flet ((on (i) (third (aref v (mod i n))))
           (xy (i) (let ((q (aref v (mod i n)))) (cons (d (first q)) (d (second q)))))
           (mid (a b) (cons (/ (+ (car a) (car b)) 2) (/ (+ (cdr a) (cdr b)) 2))))
      ;; start at an on-curve point (or the midpoint of two off-curve ones)
      (let* ((s (or (position-if #'third ct) -1))
             (start (if (>= s 0) (xy s) (mid (xy 0) (xy 1))))
             (i0 (if (>= s 0) s 0))
             (cur start))
        (push start out)
        (let ((k 1))
          (loop while (<= k n)
                do (let ((i (+ i0 k)))
                     (if (on i)
                         (progn (setf cur (xy i)) (push cur out))
                         (let* ((ctrl (xy i))
                                (end (if (on (1+ i)) (xy (1+ i)) (mid ctrl (xy (1+ i))))))
                           (loop for j from 1 to steps
                                 for tt = (/ (d j) steps)
                                 do (push (cons (+ (* (expt (- 1 tt) 2) (car cur)) (* 2 (- 1 tt) tt (car ctrl)) (* tt tt (car end)))
                                                (+ (* (expt (- 1 tt) 2) (cdr cur)) (* 2 (- 1 tt) tt (cdr ctrl)) (* tt tt (cdr end))))
                                          out))
                           (setf cur end)
                           (when (on (1+ i)) (incf k))))
                     (incf k))))
        ;; drop the closing repeat and any doubles
        (let ((pts (nreverse out)) (clean nil))
          (dolist (q pts) (unless (and clean (< (+ (abs (- (car q) (caar clean))) (abs (- (cdr q) (cdar clean)))) 1d-9)) (push q clean)))
          (setf clean (nreverse clean))
          (when (and (cdr clean) (< (+ (abs (- (caar clean) (car (car (last clean))))) (abs (- (cdar clean) (cdr (car (last clean)))))) 1d-9))
            (setf clean (butlast clean)))
          clean)))))

(defun ttf-glyph (f code)
  "(advance contours) for character CODE in em units: contours as lists of (x . y)."
  (or (gethash code (ttf-glyphs f))
      (setf (gethash code (ttf-glyphs f))
            (let* ((gi (ttf-glyph-index f code)) (u (d (ttf-units f))))
              (list (/ (ttf-advance f gi) u)
                    (loop for ct in (ttf-contours-raw f gi)
                          for pts = (quad-contour-points ct)
                          when (cddr pts) collect (mapcar (lambda (q) (cons (/ (car q) u) (/ (cdr q) u))) pts)))))))

(defun ttf-string-width (f s) (loop for ch across s sum (first (ttf-glyph f (char-code ch)))))

(defun ttf-cap-height (f)
  "Height of the capital letters, in em units (the top of H)."
  (or (ttf-cap f)
      (setf (ttf-cap f)
            (let ((ys (loop for ct in (second (ttf-glyph f (char-code #\H))) append (mapcar #'cdr ct))))
              (if ys (reduce #'max ys) 0.72d0)))))

;;; ---------------------------------------------------------------- which file

(defun font-file-for-family (family bold italic)
  "The font file fontconfig would use for FAMILY (with BOLD / ITALIC), or NIL."
  (let ((key (format nil "~A|~A|~A" family bold italic)))
    (multiple-value-bind (v found) (gethash key *font-file-cache*)
      (if found v
          (setf (gethash key *font-file-cache*)
                (ignore-errors
                 (let* ((pattern (format nil "~A:weight=~:[regular~;bold~]:slant=~:[roman~;italic~]" family bold italic))
                        (out (with-output-to-string (s)
                               (sb-ext:run-program "fc-match" (list "-f" "%{file}" pattern) :search t :output s :error nil))))
                   (and (plusp (length out)) (probe-file out) out))))))))

(defun text-font (data)
  "The TrueType font text DATA is shown in on the screen, or NIL."
  (let* ((style (or (find-textstyle (dxf 7 data)) (find-textstyle "Standard")))
         (fmt (and (equal (dxf 0 data) "MTEXT") (mtext-overall-format (or (dxf 1 data) ""))))
         (family (installed-family (font-family-for (or (getf fmt :font) (style-display-font style)))))
         (bold (if (getf fmt :font) (getf fmt :bold) (or (getf fmt :bold) (and style (textstyle-bold style)))))
         (italic (if (getf fmt :font) (getf fmt :italic) (or (getf fmt :italic) (and style (textstyle-italic style)))))
         (file (font-file-for-family family bold italic)))
    (and file (ttf-open file))))
