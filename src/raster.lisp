;;;; raster.lisp — images and PDF pages in the drawing (IMAGEATTACH, PDFATTACH, ATTACH), and the
;;;; Image Manager (IMAGE): attached files listed with their status; attach, detach, reload,
;;;; show/hide, fade and re-path them.
;;;;
;;;; An attached picture is an IMAGE object: its lower-left corner (10), the size of one pixel along
;;;; the picture's width (11) and height (12), its size in pixels (13), and the file it shows
;;;; (:file, and :page for a PDF). The drawing keeps only the file's name, as AutoCAD does.
;;;;
;;;; Tk shows PNG and GIF itself. JPEG, TIFF, BMP and WebP are turned into PNG with whichever of
;;;; ImageMagick, GraphicsMagick, vips, ffmpeg or Python's PIL is installed, and PDF pages with
;;;; poppler (pdftoppm / pdftocairo), mutool or ImageMagick. The PNGs are kept in
;;;; ~/.cache/fancypencil/images so each file is converted once.
;;;;
;;;; Pictures show in the plan views, square to the screen (not turned); in the 3D views and when
;;;; turned only their frame shows. They plot (embedded in the PDF), and are saved in DXF files as
;;;; IMAGE / IMAGEDEF and PDFUNDERLAY / PDFDEFINITION, as AutoCAD keeps them.

(in-package :fancy-pencil)

;;; ---------- tools ----------

(defvar *program-paths* (make-hash-table :test #'equal))

(defun program-path (name)
  (multiple-value-bind (v found) (gethash name *program-paths*)
    (if found v
        (setf (gethash name *program-paths*)
              (multiple-value-bind (code text) (run-shell (format nil "command -v ~A 2>/dev/null" name))
                (and (eql code 0) (plusp (length (string-trim '(#\Newline #\Space) text)))
                     (string-trim '(#\Newline #\Space) text)))))))

(defun python-pil-p ()
  (multiple-value-bind (v found) (gethash "PIL" *program-paths*)
    (if found v
        (setf (gethash "PIL" *program-paths*)
              (and (program-path "python3")
                   (eql 0 (run-shell "python3 -c 'import PIL' 2>/dev/null")))))))

(defun raster-kind (path)
  (let ((e (string-downcase (or (pathname-type (pathname path)) ""))))
    (cond ((equal e "pdf") :pdf)
          ((member e '("png" "gif" "ppm" "pgm") :test #'equal) :native)
          ((member e '("jpg" "jpeg" "jpe" "jfif" "tif" "tiff" "bmp" "webp") :test #'equal) :raster)
          (t nil))))

(defun raster-type-name (path page)
  (let ((e (string-upcase (or (pathname-type (pathname path)) ""))))
    (cond ((equal e "PDF") (format nil "PDF page ~D" (or page 1)))
          ((member e '("JPG" "JPEG" "JPE" "JFIF") :test #'equal) "JPEG")
          ((member e '("TIF" "TIFF") :test #'equal) "TIFF")
          (t e))))

(defun image-cache-dir ()
  (let ((d (merge-pathnames ".cache/fancypencil/images/"
                            (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/"))))
    (ensure-directories-exist d)
    (namestring d)))

(defun cache-name (path page dpi)
  (let ((date (ignore-errors (file-write-date path))))
    (format nil "~Aimg-~36R-~D.png" (image-cache-dir)
            (logand (sxhash (format nil "~A|~A|~A|~A" (namestring (truename path)) date page dpi)) #xFFFFFFFFFF) (or page 0))))

(defun try-commands (cmds out)
  "Run shell commands in turn until one makes file OUT."
  (dolist (c cmds nil)
    (when c
      (ignore-errors (delete-file out))
      (run-shell c)
      (when (and (probe-file out) (plusp (or (ignore-errors (with-open-file (s out) (file-length s))) 0)))
        (return t)))))

(defun convert-raster (src out)
  (let ((q (shell-quote (namestring src))) (o (shell-quote out)))
    (try-commands
     (list (and (program-path "magick") (format nil "magick ~A[0] ~A 2>/dev/null" q o))
           (and (program-path "convert") (format nil "convert ~A[0] ~A 2>/dev/null" q o))
           (and (program-path "gm") (format nil "gm convert ~A[0] ~A 2>/dev/null" q o))
           (and (program-path "vips") (format nil "vips copy ~A ~A 2>/dev/null" q o))
           (and (program-path "ffmpeg") (format nil "ffmpeg -loglevel error -y -i ~A -frames:v 1 ~A 2>/dev/null" q o))
           (and (python-pil-p)
                (format nil "python3 -c 'import sys;from PIL import Image;Image.open(sys.argv[1]).convert(\"RGB\").save(sys.argv[2])' ~A ~A 2>/dev/null" q o)))
     out)))

(defun convert-pdf-page (src page dpi out)
  (let* ((q (shell-quote (namestring src))) (o (shell-quote out))
         (base (subseq out 0 (- (length out) 4))))
    (try-commands
     (list (and (program-path "pdftoppm") (format nil "pdftoppm -png -r ~D -f ~D -l ~D -singlefile ~A ~A 2>/dev/null" dpi page page q (shell-quote base)))
           (and (program-path "pdftocairo") (format nil "pdftocairo -png -r ~D -f ~D -l ~D -singlefile ~A ~A 2>/dev/null" dpi page page q (shell-quote base)))
           (and (program-path "mutool") (format nil "mutool draw -q -r ~D -o ~A ~A ~D 2>/dev/null" dpi o q page))
           (and (program-path "magick") (format nil "magick -density ~D ~A[~D] ~A 2>/dev/null" dpi q (1- page) o))
           (and (program-path "convert") (format nil "convert -density ~D ~A[~D] ~A 2>/dev/null" dpi q (1- page) o)))
     out)))

(defparameter +pdf-dpi+ 150)

(defvar *png-memo* (make-hash-table :test #'equal))

(defun raster-png (path &optional page)
  "A PNG Tk can show for PATH (a PDF's PAGE): (values png nil) or (values nil reason)."
  (let* ((path (namestring path)) (key (list path page (ignore-errors (file-write-date path)))))
    (or (gethash key *png-memo*)
        (let ((kind (raster-kind path)))
          (cond
            ((not (probe-file path)) (values nil "file not found"))
            ((null kind) (values nil "not an image or PDF file"))
            ((eq kind :native) (setf (gethash key *png-memo*) path))
            (t (let ((out (cache-name path page (and (eq kind :pdf) +pdf-dpi+))))
                 (cond ((probe-file out) (setf (gethash key *png-memo*) out))
                       ((if (eq kind :pdf) (convert-pdf-page path (or page 1) +pdf-dpi+ out) (convert-raster path out))
                        (setf (gethash key *png-memo*) out))
                       (t (values nil (missing-tool-message kind)))))))))))

(defun missing-tool-message (kind)
  (if (eq kind :pdf)
      "no PDF reader found: install poppler-utils (sudo dnf install poppler-utils)"
      "no image converter found for this file type: install ImageMagick (sudo dnf install ImageMagick)"))

(defun read-bytes (path n)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
      (read-sequence v s) v)))

(defun image-pixel-size (png)
  "(width height) of a PNG, GIF or PPM file, or NIL."
  (ignore-errors
   (let ((b (read-bytes png 32)))
     (cond ((and (= (aref b 1) 80) (= (aref b 2) 78) (= (aref b 3) 71))  ; \x89PNG
            (flet ((u32 (i) (+ (ash (aref b i) 24) (ash (aref b (+ i 1)) 16) (ash (aref b (+ i 2)) 8) (aref b (+ i 3)))))
              (list (u32 16) (u32 20))))
           ((and (= (aref b 0) 71) (= (aref b 1) 73) (= (aref b 2) 70))   ; GIF
            (list (+ (aref b 6) (ash (aref b 7) 8)) (+ (aref b 8) (ash (aref b 9) 8))))
           ((= (aref b 0) 80)                                               ; P6 / P5
            (let ((nums (ppm-header png))) (list (first nums) (second nums))))))))

(defun ppm-header (path)
  "(width height maxval header-length) of a binary PPM/PGM file."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((nums nil) (pos 0))
      (read-byte s) (read-byte s) (setf pos 2)
      (loop while (< (length nums) 3)
            do (let ((c (read-byte s))) (incf pos)
                 (cond ((= c 35) (loop for d = (read-byte s) do (incf pos) until (= d 10)))   ; comment
                       ((<= 48 c 57)
                        (let ((n (- c 48)))
                          (loop for d = (read-byte s) do (incf pos)
                                while (<= 48 d 57) do (setf n (+ (* n 10) (- d 48))))
                          (push n nums))))))
      (append (reverse nums) (list pos)))))

(defun pdf-page-info (path page)
  "(values pages width-inches height-inches) of a PDF page, from pdfinfo (NIL when unknown)."
  (when (program-path "pdfinfo")
    (multiple-value-bind (code text) (run-shell (format nil "pdfinfo -f ~D -l ~D ~A 2>/dev/null" page page (shell-quote (namestring path))))
      (when (eql code 0)
        (let ((pages nil) (w nil) (h nil))
          (dolist (line (split-string text #\Newline))
            (let ((l (string-trim " " line)))
              (cond ((and (> (length l) 6) (string= (subseq l 0 6) "Pages:"))
                     (setf pages (ignore-errors (parse-integer (string-trim " " (subseq l 6))))))
                    ((and (search "size:" l) (search " x " l) (search "pts" l))
                     (let* ((s (subseq l (+ 5 (search "size:" l)))) (x (search " x " s)))
                       (setf w (ignore-errors (/ (d (read-from-string (subseq s 0 x))) 72))
                             h (ignore-errors (/ (d (read-from-string (subseq s (+ 3 x) (search "pts" s)))) 72))))))))
          (values pages w h))))))

;;; ---------- IMAGE objects ----------

(defun image-file (d) (dxf :file d))
(defun image-page (d) (dxf :page d))
(defun image-px (d) (let ((s (dxf 13 d))) (if (consp s) (list (round (px s)) (round (py s))) (list 1 1))))
(defun image-u (d) (pt3 (or (dxf 11 d) (pt 1 0 0))))
(defun image-v (d) (pt3 (or (dxf 12 d) (pt 0 1 0))))
(defun image-corners (d)
  "Lower-left, lower-right, upper-right and upper-left corners."
  (destructuring-bind (w h) (image-px d)
    (let* ((p (pt3 (dxf 10 d))) (u (v* (image-u d) w)) (v (v* (image-v d) h)))
      (list p (v+ p u) (v+ p (v+ u v)) (v+ p v)))))
(defun image-width (d) (* (dist (pt 0 0 0) (image-u d)) (first (image-px d))))
(defun image-height (d) (* (dist (pt 0 0 0) (image-v d)) (second (image-px d))))
(defun image-fade (d) (or (dxf :fade d) 0))
(defun image-hidden-p (d) (and (dxf :hidden d) t))

(defun make-image-data (file page ll width pw ph)
  (let ((k (/ (d width) (max 1 pw))))
    (append (list (cons 0 "IMAGE") (cons 10 (pt3 ll)) (cons 11 (pt k 0 0)) (cons 12 (pt 0 k 0)) (cons 13 (pt pw ph 0))
                  (cons :file (namestring file)))
            (and page (list (cons :page page))))))

(defun image-pieces (d)
  (let ((c (image-corners d)))
    (loop for (a b) on (append c (list (first c))) while b collect (list :seg a b))))

(defun xform-image (d xf)
  (let* ((fp (xf-fn xf)) (p (pt3 (dxf 10 d))) (np (funcall fp p))
         (nu (v- (funcall fp (v+ p (image-u d))) np)) (nv (v- (funcall fp (v+ p (image-v d))) np)))
    (set-dxf 12 nv (set-dxf 11 nu (set-dxf 10 np d)))))

(defun image-upright-p (d)
  "Shown square to the screen in a plan view: width along +X, height along +Y."
  (let ((u (image-u d)) (v (image-v d)))
    (and (null *view*) (> (px u) 0) (< (abs (py u)) (* 1d-9 (px u))) (< (abs (pz u)) 1d-9)
         (> (py v) 0) (< (abs (px v)) (* 1d-9 (py v))))))

(defun natural-width (pw ph kind page path)
  "The picture's width at its real size, in drawing units (PDF page size; 96 dpi for images)."
  (declare (ignore ph))
  (let ((in-units (if (eql (sysvar "INSUNITS") 4) 25.4d0 1d0)))
    (* in-units
       (if (eq kind :pdf)
           (or (nth-value 1 (pdf-page-info path (or page 1))) (/ pw (d +pdf-dpi+)))
           (/ pw 96d0)))))

;;; ---------- on the screen ----------

(defun best-ratio (k)
  "Integers (z s) with z/s close to K (zoom then subsample), z and s up to 48."
  (let ((best nil) (be nil))
    (loop for s from 1 to 48
          do (let ((z (max 1 (min 48 (round (* k s))))))
               (let ((e (abs (- (/ z s) k))))
                 (when (or (null be) (< e (- be 1d-12))) (setf best (list z s) be e)))))
    best))

(defun image-render-cmds (d color tags hl)
  "Canvas commands for image D: the picture (when it can be shown) and its frame."
  (let* ((corners (image-corners d)) (frame (append corners (list (first corners))))
         (frame-cmds (screen-line frame (if hl "#8fc4ff" color) tags :dash hl))
         (out nil))
    (when (and (not (image-hidden-p d)) (image-upright-p d) (not (projected-view-p)))
      (let ((png (raster-png (image-file d) (image-page d))))
        (if (null png)
            ;; not found or can't be read: a cross and the file's name
            (multiple-value-bind (x0 y0) (w->s (first corners))
              (multiple-value-bind (x1 y1) (w->s (third corners))
                (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}" (fmtn x0) (fmtn y0) (fmtn x1) (fmtn y1) color tags) out)
                (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}" (fmtn x0) (fmtn y1) (fmtn x1) (fmtn y0) color tags) out)
                (push (format nil ".c create text ~A ~A -text ~A -fill ~A -font {Helvetica 9} -tags {~A}"
                              (fmtn (/ (+ x0 x1) 2)) (fmtn (/ (+ y0 y1) 2))
                              (tq (format nil "~A (not found)" (file-namestring (image-file d)))) color tags)
                      out)))
            (destructuring-bind (pw ph) (image-px d)
              (multiple-value-bind (sx0 sy1) (w->s (first corners))        ; lower left
                (multiple-value-bind (sx1 sy0) (w->s (third corners))      ; upper right
                  (let* ((k (/ (- sx1 sx0) pw))                            ; pixels on screen per picture pixel
                         (cx0 (max sx0 *tx*)) (cy0 (max sy0 *ty*))
                         (cx1 (min sx1 (+ *tx* *cw*))) (cy1 (min sy1 (+ *ty* *ch*))))
                    (when (and (> k 1d-9) (< cx0 cx1) (< cy0 cy1))
                      (destructuring-bind (z s) (best-ratio k)
                        ;; the part of the picture on the screen, whole pixels at the scale used
                        (let* ((kk (/ z s))
                               (fx0 (max 0 (floor (/ (- cx0 sx0) k)))) (fy0 (max 0 (floor (/ (- cy0 sy0) k))))
                               (fx1 (min pw (ceiling (/ (- cx1 sx0) k)))) (fy1 (min ph (ceiling (/ (- cy1 sy0) k)))))
                          (declare (ignorable kk))
                          (when (and (< fx0 fx1) (< fy0 fy1) (< (* (- fx1 fx0) (/ z s) (- fy1 fy0) (/ z s)) 2.5d7))
                            (push (format nil "fpimg_show ~A ~A ~A ~A ~D ~D ~D ~D ~D ~D ~A ~D {~A}"
                                          (tq (or (dxf 5 d) "x")) (tq png)
                                          (fmtn (+ sx0 (* fx0 k))) (fmtn (+ sy0 (* fy0 k)))
                                          fx0 fy0 fx1 fy1 z s (tq (canvas-bg)) (image-fade d) tags)
                                  out))))))))))))
    (append (nreverse out) frame-cmds)))

;;; ---------- plotting ----------

(defun image-ppm (d max-px)
  "An RGB PPM of image D at most MAX-PX across, for a plot: (values path width height) or NIL."
  (let ((png (raster-png (image-file d) (image-page d))))
    (when png
      (destructuring-bind (pw ph) (image-px d)
        (let* ((s (max 1 (ceiling (max pw ph) max-px)))
               (out (format nil "~Aplot-~36R-~D.ppm" (image-cache-dir) (logand (sxhash (list png s)) #xFFFFFFFF) s)))
          (unless (probe-file out)
            (if (eq *ui* :gui)
                (tk-ask (format nil "fpimg_ppm ~A ~A ~D" (tq png) (tq out) s))
                (let ((q (shell-quote png)) (o (shell-quote out)) (pct (format nil "~,4F%" (/ 100d0 s))))
                  (try-commands
                   (list (and (program-path "magick") (format nil "magick ~A -resize ~A ~A" q pct o))
                         (and (program-path "convert") (format nil "convert ~A -resize ~A ~A" q pct o))
                         (and (program-path "ffmpeg") (format nil "ffmpeg -loglevel error -y -i ~A -vf scale=iw/~D:ih/~D -pix_fmt rgb24 ~A" q s s o))
                         (and (python-pil-p) (format nil "python3 -c 'import sys;from PIL import Image;i=Image.open(sys.argv[1]).convert(\"RGB\");i.resize((max(1,i.width//~D),max(1,i.height//~D))).save(sys.argv[2])' ~A ~A" s s q o)))
                   out))))
          (when (probe-file out)
            (let ((h (ignore-errors (ppm-header out))))
              (when h (values out (first h) (second h))))))))))

(defun image-plot-prims (d)
  "(:image ppm width height lower-left upper-right) for an upright image."
  (when (and (not (image-hidden-p d)) (image-upright-p d) (not (projected-view-p)))
    (multiple-value-bind (ppm w h) (image-ppm d 2400)
      (when ppm
        (let ((c (image-corners d)))
          (list (list :image ppm w h (first c) (third c) (image-fade d))))))))

(defun ppm-pixel-string (path &optional (fade 0))
  "The RGB bytes of a binary PPM as a string of 8-bit characters (for the PDF stream), faded
FADE percent toward white. A gray PGM is spread to RGB."
  (destructuring-bind (w h maxv hlen) (ppm-header path)
    (let ((gray (= 53 (aref (read-bytes path 2) 1))))     ; P5
      (with-open-file (s path :element-type '(unsigned-byte 8))
        (file-position s hlen)
        (let* ((n (* w h (if gray 1 3))) (v (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
               (f (/ (max 0 (min 100 (or fade 0))) 100d0)) (k (/ 255d0 (max 1 maxv))))
          (read-sequence v s)
          (let ((out (make-string (* w h 3))))
            (dotimes (i (* w h 3) out)
              (let* ((c (* k (aref v (if gray (floor i 3) i))))
                     (c (if (zerop f) c (+ c (* f (- 255 c))))))
                (setf (char out i) (code-char (min 255 (max 0 (round c)))))))))))))

;;; ---------- attaching ----------

(defun ask-page (path)
  "The page of a PDF to attach (asked when it has more than one)."
  (let ((pages (pdf-page-info path 1)))
    (if (or (null pages) (<= pages 1))
        1
        (let ((r (if (use-inplace-editor-p)
                     (cdr (assoc "P" (gui-form (format nil "~A" (file-namestring path))
                                               (list (list "P" (format nil "Page (1 to ~D)" pages) "1" :combo
                                                           (loop for i from 1 to (min pages 200) collect (princ-to-string i))))
                                               "Attach")
                                 :test #'string=))
                     (gstring (format nil "Enter page number (1 to ~D) <1>: " pages)))))
          (max 1 (min pages (or (ignore-errors (parse-integer (string-trim " " (or r "1")))) 1)))))))

(defun attach-image (path &optional page)
  "Attach the picture at PATH: asks for the page (PDF), place and size. Returns the new object or NIL."
  (let* ((path (namestring (or (probe-file path) path)))
         (kind (raster-kind path)))
    (cond
      ((not (probe-file path)) (tell-user (format nil "Cannot find ~A" path)) nil)
      ((null kind) (tell-user (format nil "~A is not an image (PNG, JPEG, TIFF, GIF, BMP) or a PDF." (file-namestring path))) nil)
      (t
       (let* ((page (and (eq kind :pdf) (or page (ask-page path)))))
         (multiple-value-bind (png why) (raster-png path page)
           (if (null png)
               (progn (tell-user (format nil "~A can't be read: ~A." (file-namestring path) why)) nil)
               (destructuring-bind (pw ph) (or (image-pixel-size png) (list 100 100))
                 (let* ((natural (natural-width pw ph kind page path))
                        (frame (lambda (ll w) (let ((k (/ w pw)))
                                                (image-pieces (list (cons 0 "IMAGE") (cons 10 (pt3 ll)) (cons 11 (pt k 0 0)) (cons 12 (pt 0 k 0)) (cons 13 (pt pw ph 0)))))))
                         (ll (gpoint "Specify insertion point <0,0>: " :drag (lambda (cur) (funcall frame cur natural))))
                         (ll (if (pointp ll) ll (pt 0 0 0)))
                         (r (get-user-input :distance :base ll
                                            :prompt (format nil "Specify image width or [Scale] <~A>: " (fmt-len natural))
                                            :keywords '("Scale")
                                            :drag (lambda (cur) (funcall frame ll (max 1d-6 (abs (- (px cur) (px ll))))))))
                         (width (cond ((null r) natural)
                                      ((equal r "Scale")
                                       (let ((s (greal "Specify scale factor <1>: " :flags 6))) (* natural (or s 1d0))))
                                      ((realp r) (abs (d r)))
                                      (t natural))))
                   (let ((e (add-entity (make-image-data path page ll width pw ph))))
                     ;; pictures go behind the drawing (DRAWORDER Front brings one forward)
                     (reorder-entities (list e) :back)
                     (out (format nil "~A attached (~D x ~D pixels, ~A wide).~%"
                                  (raster-type-name path page) pw ph (fmt-len width)))
                     (when (gui-p) (gui-regen))
                     e))))))))))

(defparameter *image-exts* "png jpg jpeg tif tiff gif bmp webp")

(defcmd "IMAGEATTACH" ("IAT") "Attaches a picture (PNG, JPEG, TIFF, GIF, BMP) to the drawing."
  (let ((f (ask-file nil "Attach Image" nil *image-exts*))) (when f (attach-image f))))

(defcmd "PDFATTACH" () "Attaches a page of a PDF file to the drawing."
  (let ((f (ask-file nil "Attach PDF" nil "pdf"))) (when f (attach-image f))))

(defcmd "ATTACH" () "Attaches a picture or a PDF page to the drawing (choose the file)."
  (let ((f (ask-file nil "Attach Image or PDF" nil (concatenate 'string "pdf " *image-exts*)))) (when f (attach-image f))))

;;; ---------- the Image Manager ----------

(defun attached-files ()
  "One entry per attached file (and PDF page): (file page refs)."
  (let ((out nil))
    (dolist (e (all-live-entities) (nreverse out))
      (let ((d (ename-data e)))
        (when (equal (dxf 0 d) "IMAGE")
          (let ((x (find-if (lambda (o) (and (equal (first o) (image-file d)) (eql (second o) (image-page d)))) out)))
            (if x (push e (third x)) (push (list (image-file d) (image-page d) (list e)) out))))))))

(defun attached-status (file page)
  (cond ((not (probe-file file)) "Not found")
        ((raster-png file page) "Loaded")
        (t "Can't read")))

(defun image-manager-rows ()
  (mapcar (lambda (a)
            (destructuring-bind (file page refs) a
              (let ((d (ename-data (first refs))))
                (list (file-namestring file) (raster-type-name file page) (attached-status file page)
                      (destructuring-bind (w h) (image-px d) (format nil "~D x ~D" w h))
                      (princ-to-string (length refs))
                      (if (every (lambda (e) (image-hidden-p (ename-data e))) refs) "Hidden" "Shown")
                      (format nil "~D%" (image-fade d))
                      file))))
          (attached-files)))

(defun set-images (refs fn)
  (with-undo-group (lambda () (dolist (e refs) (set-entity-data e (funcall fn (ename-data e)))))))

(defun forget-raster (file page)
  (remhash (list file page (ignore-errors (file-write-date file))) *png-memo*)
  (maphash (lambda (k v) (declare (ignore v)) (when (equal (first k) file) (remhash k *png-memo*))) *png-memo*)
  (when (gui-p) (tk "fpimg_forget_all")))

(defun image-manager-action (action idx extra)
  (let* ((files (attached-files)) (a (and idx (nth idx files))))
    (cond
      ((string= action "ATTACH") (let ((f (ask-file nil "Attach Image or PDF" nil (concatenate 'string "pdf " *image-exts*)))) (when f (attach-image f))))
      ((null a) nil)
      ((string= action "DETACH")
       (with-undo-group (lambda () (dolist (e (third a)) (delete-entity e))))
       (out (format nil "~A detached.~%" (file-namestring (first a)))))
      ((string= action "RELOAD")
       (forget-raster (first a) (second a))
       (out (format nil "~A reloaded: ~A.~%" (file-namestring (first a)) (attached-status (first a) (second a)))))
      ((string= action "TOGGLE")
       (let ((hide (notany (lambda (e) (image-hidden-p (ename-data e))) (third a))))
         (set-images (third a) (lambda (d) (if hide (set-vp-key :hidden t d) (rem-dxf :hidden d))))))
      ((string= action "FADE")
       (let ((f (ignore-errors (parse-integer (string-right-trim "%" extra)))))
         (when f (set-images (third a) (lambda (d) (if (zerop f) (rem-dxf :fade d) (set-vp-key :fade (max 0 (min 90 f)) d)))))))
      ((string= action "PATH")
       (let ((f (ask-file nil (format nil "Find ~A" (file-namestring (first a))) (first a)
                          (concatenate 'string "pdf " *image-exts*))))
         (when f
           (let ((f (namestring (or (probe-file f) f))))
             (set-images (third a) (lambda (d) (set-dxf :file f d)))
             (forget-raster f (second a))
             (out (format nil "Now showing ~A.~%" f))))))
      ((string= action "SELECT")
       (setf *pickfirst* (remove-if-not #'ent-live-p (third a)))
       (when (gui-p) (ui-highlight *pickfirst* t) (refresh-grips) (refresh-palette))))
    (when (gui-p) (gui-regen))))

(defcmd "IMAGE" ("IM" "IMAGEMANAGER" "EXTERNALREFERENCES" "XREF" "XR") "The Image Manager: the pictures and PDF pages attached to the drawing."
  (if (use-inplace-editor-p)
      (loop
        (let* ((rows (image-manager-rows))
               (r (tk-ask (format nil "imagemgr {~{{~{~A~^ ~}}~^ ~}}"
                                  (mapcar (lambda (row) (mapcar #'tq row)) rows))))
               (f (split-string r (code-char 31))))
          (when (or (string= r "") (string= (first f) "CLOSE")) (return))
          (image-manager-action (first f) (ignore-errors (parse-integer (or (second f) ""))) (third f))
          (when (string= (first f) "SELECT") (return))))
      (let ((k (gkw "Enter image option [?/Attach/Detach/Reload/Hide/Show] <Attach>: "
                    '("?" "Attach" "Detach" "Reload" "Hide" "Show") "Attach")))
        (cond
          ((equal k "Attach") (let ((f (gstring "Enter image file name: " t))) (when (plusp (length f)) (attach-image f))))
          ((equal k "?")
           (dolist (row (image-manager-rows))
             (out (format nil "  ~A  ~A  ~A  ~A  ~A ref(s)  ~A~%" (first row) (second row) (third row) (fourth row) (fifth row) (eighth row)))))
          (t
           (let* ((name (gstring "Enter image name: " t))
                  (idx (position-if (lambda (a) (string-equal (file-namestring (first a)) name)) (attached-files))))
             (if (null idx)
                 (out (format nil "No image named ~A.~%" name))
                 (image-manager-action (cond ((equal k "Detach") "DETACH") ((equal k "Reload") "RELOAD") (t "TOGGLE")) idx nil))))))))

;;; ---------- Properties and LIST ----------

(defun image-palette-rows (sel)
  (flet ((v (fn) (common-value sel fn)))
    (list '("" "Image" "" :head)
          (list "" "File" (v (lambda (d) (file-namestring (image-file d)))) :ro)
          (list "" "Type" (v (lambda (d) (raster-type-name (image-file d) (image-page d)))) :ro)
          (list "IMGW" "Width" (v (lambda (d) (fmt-num (image-width d)))) :entry)
          (list "IMGH" "Height" (v (lambda (d) (fmt-num (image-height d)))) :entry)
          (list "PX" "Position X" (v (lambda (d) (fmt-num (px (dxf 10 d))))) :entry)
          (list "PY" "Position Y" (v (lambda (d) (fmt-num (py (dxf 10 d))))) :entry)
          (list "IMGFADE" "Fade" (v (lambda (d) (format nil "~D" (image-fade d)))) :combo '("0" "25" "50" "75"))
          (list "IMGSHOW" "Show image" (v (lambda (d) (if (image-hidden-p d) "No" "Yes"))) :combo '("Yes" "No")))))

(defun image-palette-edit (d key val num)
  (cond
    ((and (member key '("IMGW" "IMGH") :test #'string=) num (plusp num))
     (let* ((cur (if (string= key "IMGW") (image-width d) (image-height d))) (k (/ num cur)))
       (set-dxf 12 (v* (image-v d) k) (set-dxf 11 (v* (image-u d) k) d))))
    ((and (string= key "IMGFADE") num) (if (zerop num) (rem-dxf :fade d) (set-vp-key :fade (max 0 (min 90 (round num))) d)))
    ((string= key "IMGSHOW") (if (string-equal val "No") (set-vp-key :hidden t d) (rem-dxf :hidden d)))
    (t d)))

(defun describe-image (d o)
  (format o "~10TFile: ~A~%~10TType: ~A~%~10TStatus: ~A~%" (image-file d) (raster-type-name (image-file d) (image-page d))
          (attached-status (image-file d) (image-page d)))
  (destructuring-bind (w h) (image-px d) (format o "~10TSize: ~D x ~D pixels~%" w h))
  (let ((p (pt3 (dxf 10 d)))) (format o "~10TInsertion point: X=~A  Y=~A~%" (fmt-len (px p)) (fmt-len (py p))))
  (format o "~10TWidth: ~A,  height: ~A~%~10TFade: ~D~:[~;  (hidden)~]~%" (fmt-len (image-width d)) (fmt-len (image-height d))
          (image-fade d) (image-hidden-p d)))

;;; ---------- DXF ----------

(defun image-from-dxf (data defs base-dir)
  "An IMAGE or PDFUNDERLAY read from a file (DEFS: definition handle -> (file page))."
  (let* ((def (gethash (princ-to-string (or (dxf 340 data) "")) defs))
         (file (and def (first def)))
         (file (and file (resolve-attached-path file base-dir)))
         (layer (dxf 8 data)) (color (assoc 62 data)))
    (when file
      (let ((d (if (equal (dxf 0 data) "IMAGE")
                   (append (list (cons 0 "IMAGE") (cons 10 (pt3 (or (dxf 10 data) (pt 0 0)))) (cons 11 (pt3 (or (dxf 11 data) (pt 1 0))))
                                 (cons 12 (pt3 (or (dxf 12 data) (pt 0 1)))) (cons 13 (pt3 (or (dxf 13 data) (pt 1 1))))
                                 (cons :file file))
                           (let ((f (dxf 283 data))) (and f (plusp f) (list (cons :fade f))))
                           (and (eql (logand (or (dxf 70 data) 1) 1) 0) (list (cons :hidden t))))
                   ;; a PDF underlay: scale is drawing units per inch of the page
                   (let* ((page (or (ignore-errors (parse-integer (second def))) 1))
                          (png (raster-png file page))
                          (px (or (and png (image-pixel-size png)) (list (round (* 8.5 +pdf-dpi+)) (round (* 11 +pdf-dpi+)))))
                          (sc (d (or (dxf 41 data) 1)))
                          (k (/ sc +pdf-dpi+))
                          (a (rad (d (or (dxf 50 data) 0)))))
                     (append (list (cons 0 "IMAGE") (cons 10 (pt3 (or (dxf 10 data) (pt 0 0))))
                                   (cons 11 (pt (* k (cos a)) (* k (sin a)) 0)) (cons 12 (pt (- (* k (sin a))) (* k (cos a)) 0))
                                   (cons 13 (pt (first px) (second px) 0)) (cons :file file) (cons :page page))
                             (let ((f (dxf 282 data))) (and f (plusp f) (list (cons :fade f))))
                             (and (eql (logand (or (dxf 280 data) 2) 2) 0) (list (cons :hidden t))))))))
        (append d (and layer (list (cons 8 layer))) (and color (list color)))))))

(defun resolve-attached-path (file base-dir)
  "FILE as saved (maybe relative, maybe with backslashes): the file if it can be found, else as given."
  (let* ((f (substitute #\/ #\\ file))
         (cands (remove nil (list f (and base-dir (namestring (merge-pathnames f base-dir)))
                                  (and base-dir (namestring (merge-pathnames (file-namestring f) base-dir)))))))
    (or (find-if #'probe-file cands) f)))

(defun image-defs-from-objects (objects)
  "IMAGEDEF and PDFDEFINITION objects: handle -> (file page)."
  (let ((h (make-hash-table :test #'equalp)))
    (dolist (g objects h)
      (let ((type (cdr (first g))))
        (when (member type '("IMAGEDEF" "PDFDEFINITION") :test #'equal)
          (setf (gethash (princ-to-string (dxf 5 g)) h) (list (dxf 1 g) (and (dxf 2 g) (princ-to-string (dxf 2 g))))))))))

(defvar *image-defs-2000* nil
  "While writing a DXF 2000 file: one entry per picture file, (file page def-handle reactors), reactors
being (reactor-handle . image-handle).")

(defun image-def-2000 (file page)
  (or (find-if (lambda (x) (and (equal (first x) file) (eql (second x) page))) *image-defs-2000*)
      (let ((x (list file page (h2) nil)))
        (setf *image-defs-2000* (append *image-defs-2000* (list x)))
        x)))

(defun write-image-entity-2000 (out data owner handle)
  "IMAGE (a picture) or PDFUNDERLAY (a PDF page), with its definition kept for the OBJECTS section."
  (let* ((def (image-def-2000 (image-file data) (image-page data))))
    (if (image-page data)
        (write-pdf-underlay-2000 out data handle owner (third def))
        (let ((rh (h2)))
          (push (cons rh handle) (fourth def))
          (write-image-2000 out data handle owner (third def) rh)))))

(defun drawing-has-images-p ()
  (or (some (lambda (e) (equal (ent-type e) "IMAGE")) (all-live-entities))
      (some (lambda (b) (some (lambda (d) (equal (dxf 0 d) "IMAGE")) (block-def-entities b))) (drawing-blocks *dwg*))))

(defun write-image-classes-2000 (out)
  (dolist (c '(("IMAGE" "AcDbRasterImage" "ISM" 2175 1) ("IMAGEDEF" "AcDbRasterImageDef" "ISM" 0 0)
               ("IMAGEDEF_REACTOR" "AcDbRasterImageDefReactor" "ISM" 1 0)
               ("RASTERVARIABLES" "AcDbRasterVariables" "ISM" 0 0)
               ("PDFDEFINITION" "AcDbPdfDefinition" "ObjectDBX Classes" 1153 0)
               ("PDFUNDERLAY" "AcDbPdfReference" "ObjectDBX Classes" 4095 1)))
    (destructuring-bind (name cls app flags ent) c
      (w out 0 "CLASS") (w out 1 name) (w out 2 cls) (w out 3 app) (w out 90 flags) (w out 280 0) (w out 281 ent))))

(defun image-dict-entries-2000 ()
  "Handles for the root dictionary: (values image-dict pdf-dict raster-vars), NIL when not needed."
  (values (and (some (lambda (x) (null (second x))) *image-defs-2000*) (h2))
          (and (some #'second *image-defs-2000*) (h2))
          (and (some (lambda (x) (null (second x))) *image-defs-2000*) (h2))))

(defun def-name (file page used)
  (let* ((base (pathname-name (pathname file))) (n (if page (format nil "~A-~D" base page) base)))
    (loop for k from 1 for name = n then (format nil "~A~D" n k)
          unless (member name used :test #'string-equal) return name)))

(defun write-image-objects-2000 (out root image-dict pdf-dict raster-vars)
  (let ((used nil))
    (when image-dict
      (w out 0 "DICTIONARY") (w out 5 image-dict) (w out 330 root) (w out 100 "AcDbDictionary") (w out 281 1)
      (dolist (x *image-defs-2000*)
        (unless (second x)
          (let ((n (def-name (first x) nil used))) (push n used) (w out 3 n) (w out 350 (third x)))))
      (w out 0 "RASTERVARIABLES") (w out 5 raster-vars) (w out 330 root) (w out 100 "AcDbRasterVariables")
      (w out 90 0) (w out 70 1) (w out 71 1) (w out 72 (if (eql (sysvar "INSUNITS") 4) 1 5))
      (dolist (x *image-defs-2000*)
        (unless (second x)
          (destructuring-bind (file page h reactors) x
            (declare (ignore page))
            (let* ((png (raster-png file nil)) (sz (or (and png (image-pixel-size png)) (list 1 1))))
              (w out 0 "IMAGEDEF") (w out 5 h)
              (w out 102 "{ACAD_REACTORS") (w out 330 image-dict) (dolist (r reactors) (w out 330 (car r))) (w out 102 "}")
              (w out 330 image-dict) (w out 100 "AcDbRasterImageDef") (w out 90 0) (w out 1 (dxf-escape file))
              (wp out 10 (pt (first sz) (second sz)) t) (wp out 11 (pt 1 1) t) (w out 280 (if png 1 0)) (w out 281 0)
              (dolist (r reactors)
                (w out 0 "IMAGEDEF_REACTOR") (w out 5 (car r)) (w out 330 (cdr r))
                (w out 100 "AcDbRasterImageDefReactor") (w out 90 2) (w out 330 (cdr r))))))))
    (when pdf-dict
      (w out 0 "DICTIONARY") (w out 5 pdf-dict) (w out 330 root) (w out 100 "AcDbDictionary") (w out 281 1)
      (dolist (x *image-defs-2000*)
        (when (second x)
          (let ((n (def-name (first x) (second x) used))) (push n used) (w out 3 n) (w out 350 (third x)))))
      (dolist (x *image-defs-2000*)
        (when (second x)
          (w out 0 "PDFDEFINITION") (w out 5 (third x))
          (w out 102 "{ACAD_REACTORS") (w out 330 pdf-dict) (w out 102 "}")
          (w out 330 pdf-dict) (w out 100 "AcDbUnderlayDefinition")
          (w out 1 (dxf-escape (first x))) (w out 2 (princ-to-string (second x))))))))

(defun write-image-2000 (out data handle owner def-handle reactor-handle)
  (destructuring-bind (w h) (image-px data)
    (w out 0 "IMAGE") (w out 5 handle) (w out 330 owner) (w out 100 "AcDbEntity")
    (when *write-paper* (w out 67 1))
    (w out 8 (or (dxf 8 data) "0"))
    (let ((c (dxf 62 data))) (when c (w out 62 c)))
    (w out 100 "AcDbRasterImage") (w out 90 0)
    (wp out 10 (pt3 (dxf 10 data))) (wp out 11 (image-u data)) (wp out 12 (image-v data)) (wp out 13 (pt w h) t)
    (w out 340 def-handle) (w out 70 (if (image-hidden-p data) 2 3)) (w out 280 0) (w out 281 50) (w out 282 50)
    (w out 283 (image-fade data)) (w out 360 reactor-handle)
    (w out 71 1) (w out 91 2) (wp out 14 (pt -0.5 -0.5) t) (wp out 14 (pt (- w 0.5) (- h 0.5)) t)))

(defun write-pdf-underlay-2000 (out data handle owner def-handle)
  (w out 0 "PDFUNDERLAY") (w out 5 handle) (w out 330 owner) (w out 100 "AcDbEntity")
  (when *write-paper* (w out 67 1))
  (w out 8 (or (dxf 8 data) "0"))
  (let ((c (dxf 62 data))) (when c (w out 62 c)))
  (w out 100 "AcDbUnderlayReference") (w out 340 def-handle)
  (wp out 10 (pt3 (dxf 10 data)))
  (let ((u (image-u data)))
    (let ((sc (* (dist (pt 0 0 0) u) +pdf-dpi+)))
      (w out 41 sc) (w out 42 sc) (w out 43 sc))
    (w out 50 (deg (atan (py u) (px u)))))
  (w out 280 (if (image-hidden-p data) 0 2)) (w out 281 100) (w out 282 (min 80 (image-fade data))))
