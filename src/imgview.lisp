;;;; imgview.lisp — drawing attached pictures on the screen.
;;;;
;;;; Tk can only enlarge or shrink a picture by whole steps, which turns a photo into coarse blocks
;;;; and drops the thin lines of a scanned drawing. So Fancy Pencil keeps each picture's pixels
;;;; itself, with smaller copies made by averaging (each half the size of the one before), and
;;;; for every view works out exactly the part on the screen, pixel by pixel, from the copy
;;;; nearest the screen's scale (blending the four nearest pixels). The result goes to Tk as a PNG
;;;; whose pixels outside the picture are clear, so a picture can be turned at any angle and seen
;;;; in the 3D views too. Work grows with the screen, not with the picture.

(in-package :fancy-pencil)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defstruct (pixmap (:constructor %make-pixmap)) (w 0 :type fixnum) (h 0 :type fixnum) (rgb nil))

(defun make-pixmap (w h &optional rgb)
  (%make-pixmap :w w :h h :rgb (or rgb (make-array (* w h 3) :element-type '(unsigned-byte 8) :initial-element 0))))

;;; ---------- reading pixels ----------

(defun read-ppm-pixmap (path)
  "A PPM (P6) or PGM (P5) file as a pixmap."
  (destructuring-bind (w h maxv hlen) (ppm-header path)
    (let* ((gray (= 53 (aref (read-bytes path 2) 1)))
           (n (* w h (if gray 1 3)))
           (raw (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
      (with-open-file (s path :element-type '(unsigned-byte 8))
        (file-position s hlen)
        (read-sequence raw s))
      (let ((rgb (if gray
                     (let ((v (make-array (* w h 3) :element-type '(unsigned-byte 8))))
                       (dotimes (i (* w h) v)
                         (let ((g (aref raw i))) (setf (aref v (* 3 i)) g (aref v (+ 1 (* 3 i))) g (aref v (+ 2 (* 3 i))) g))))
                     raw)))
        (unless (= maxv 255)
          (dotimes (i (length rgb)) (setf (aref rgb i) (min 255 (round (* 255 (aref rgb i)) (max 1 maxv))))))
        (make-pixmap w h rgb)))))

(defun picture-ppm (png out)
  "Write PNG (or GIF/PPM) as a PPM file OUT: Tk reads it in the window; otherwise a converter."
  (if (eq *ui* :gui)
      (string= (tk-ask (format nil "fpimg_ppm ~A ~A 1" (tq png) (tq out))) "ok")
      (let ((q (shell-quote png)) (o (shell-quote out)))
        (try-commands
         (list (and (program-path "magick") (format nil "magick ~A -alpha off -depth 8 ~A 2>/dev/null" q o))
               (and (program-path "convert") (format nil "convert ~A -alpha off -depth 8 ~A 2>/dev/null" q o))
               (and (program-path "ffmpeg") (format nil "ffmpeg -loglevel error -y -i ~A -pix_fmt rgb24 ~A" q o))
               (and (python-pil-p) (format nil "python3 -c 'import sys;from PIL import Image;Image.open(sys.argv[1]).convert(\"RGB\").save(sys.argv[2])' ~A ~A" q o)))
         out))))

(defvar *pictures* nil "Recently used pictures: ((png . levels) ...), newest first.")
(defparameter +pictures-kept+ 4)

(defun picture-levels (png)
  "The pixmaps of PNG: a vector whose first is full size and each next half the size (made as
needed). NIL when the picture can't be read."
  (let ((hit (assoc png *pictures* :test #'equal)))
    (if hit
        (progn (setf *pictures* (cons hit (remove hit *pictures*))) (cdr hit))
        (let* ((tmp (format nil "~Apix-~D.ppm" (image-cache-dir) (random 1000000000)))
               (pm (ignore-errors (and (picture-ppm png tmp) (read-ppm-pixmap tmp)))))
          (ignore-errors (delete-file tmp))
          (when pm
            (let ((levels (make-array 1 :adjustable t :fill-pointer 1 :initial-element pm)))
              (push (cons png levels) *pictures*)
              (when (> (length *pictures*) +pictures-kept+) (setf *pictures* (subseq *pictures* 0 +pictures-kept+)))
              levels))))))

(defun forget-picture-pixels (&optional png)
  (setf *pictures* (if png (remove png *pictures* :key #'car :test #'equal) nil)))

(defun half-pixmap (pm)
  "PM at half size, each pixel the average of four."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((w (pixmap-w pm)) (h (pixmap-h pm)) (src (pixmap-rgb pm))
         (w2 (max 1 (floor w 2))) (h2 (max 1 (floor h 2)))
         (dst (make-array (* w2 h2 3) :element-type '(unsigned-byte 8))))
    (declare (fixnum w h w2 h2) (type octets src dst))
    (dotimes (y h2)
      (let* ((y0 (min (1- h) (* 2 y))) (y1 (min (1- h) (1+ (* 2 y)))))
        (declare (fixnum y0 y1))
        (dotimes (x w2)
          (let* ((x0 (min (1- w) (* 2 x))) (x1 (min (1- w) (1+ (* 2 x))))
                 (a (* 3 (+ (* y0 w) x0))) (b (* 3 (+ (* y0 w) x1)))
                 (c (* 3 (+ (* y1 w) x0))) (e (* 3 (+ (* y1 w) x1)))
                 (o (* 3 (+ (* y w2) x))))
            (declare (fixnum x0 x1 a b c e o))
            (dotimes (k 3)
              (setf (aref dst (+ o k))
                    (ash (+ (aref src (+ a k)) (aref src (+ b k)) (aref src (+ c k)) (aref src (+ e k)) 2) -2)))))))
    (make-pixmap w2 h2 dst)))

(defun picture-level (levels k)
  "Level K of LEVELS (making the smaller copies up to it)."
  (loop while (and (< (1- (length levels)) k)
                   (let ((top (aref levels (1- (length levels))))) (or (> (pixmap-w top) 1) (> (pixmap-h top) 1))))
        do (vector-push-extend (half-pixmap (aref levels (1- (length levels)))) levels))
  (aref levels (min k (1- (length levels)))))

;;; ---------- drawing a view of it ----------

(defun render-picture (pm level-scale ox oy ax ay bx by x0 y0 w h fade bg)
  "An RGBA buffer W x H for screen pixels from X0,Y0: screen point = O + i*A + j*B for picture
pixel coordinates i (across) and j (down) of the full-size picture; PM is the copy LEVEL-SCALE
times smaller. Outside the picture: clear. FADE (0-90) blends toward BG (r g b)."
  (render-picture-1 pm (d level-scale) (d ox) (d oy) (d ax) (d ay) (d bx) (d by)
                    (round x0) (round y0) (round w) (round h) (round (or fade 0)) (mapcar #'d bg)))

(defun render-picture-1 (pm level-scale ox oy ax ay bx by x0 y0 w h fade bg)
  (declare (optimize (speed 3) (safety 0)) (double-float level-scale ox oy ax ay bx by) (fixnum x0 y0 w h fade))
  (let* ((out (make-array (* w h 4) :element-type '(unsigned-byte 8) :initial-element 0))
         (pw (pixmap-w pm)) (ph (pixmap-h pm)) (src (pixmap-rgb pm))
         (det (- (* ax by) (* ay bx)))
         ;; inverse of [A B]
         (ia (/ by det)) (ib (/ (- bx) det)) (ic (/ (- ay) det)) (id (/ ax det))
         (ls level-scale)
         (fw (d (* pw ls))) (fh (d (* ph ls)))
         (f (/ (coerce (max 0 (min 90 fade)) 'double-float) 100d0))
         (br (* f (first bg))) (bgg (* f (second bg))) (bb (* f (third bg))) (keep (- 1d0 f)))
    (declare (type octets out src) (fixnum pw ph w h x0 y0) (double-float det ia ib ic id ls fw fh f br bgg bb keep))
    (dotimes (y h)
      (let ((sy (- (+ (d (+ y0 y)) 0.5d0) oy)))
        (declare (double-float sy))
        (dotimes (x w)
          (let* ((sx (- (+ (d (+ x0 x)) 0.5d0) ox))
                 (ci (+ (* ia sx) (* ib sy)))          ; full-size picture coordinates
                 (cj (+ (* ic sx) (* id sy))))
            (declare (double-float sx ci cj))
            (when (and (>= ci 0d0) (< ci fw) (>= cj 0d0) (< cj fh))
              (let* ((u (- (/ ci ls) 0.5d0)) (v (- (/ cj ls) 0.5d0))
                     (iu (floor u)) (iv (floor v))
                     (fu (- u iu)) (fv (- v iv))
                     (u0 (max 0 (min (1- pw) iu))) (u1 (max 0 (min (1- pw) (1+ iu))))
                     (v0 (max 0 (min (1- ph) iv))) (v1 (max 0 (min (1- ph) (1+ iv))))
                     (p00 (* 3 (+ (* v0 pw) u0))) (p10 (* 3 (+ (* v0 pw) u1)))
                     (p01 (* 3 (+ (* v1 pw) u0))) (p11 (* 3 (+ (* v1 pw) u1)))
                     (o (* 4 (+ (* y w) x))))
                (declare (double-float u v fu fv) (fixnum iu iv u0 u1 v0 v1 p00 p10 p01 p11 o))
                (flet ((ch (k)
                         (let ((c (+ (* (- 1d0 fv) (+ (* (- 1d0 fu) (aref src (+ p00 k))) (* fu (aref src (+ p10 k)))))
                                     (* fv (+ (* (- 1d0 fu) (aref src (+ p01 k))) (* fu (aref src (+ p11 k))))))))
                           (declare (double-float c))
                           c)))
                  (setf (aref out o) (min 255 (max 0 (round (+ (* keep (ch 0)) br))))
                        (aref out (+ o 1)) (min 255 (max 0 (round (+ (* keep (ch 1)) bgg))))
                        (aref out (+ o 2)) (min 255 (max 0 (round (+ (* keep (ch 2)) bb))))
                        (aref out (+ o 3)) 255))))))))
    out))

;;; ---------- a plain PNG writer (stored, not compressed: quick to write and to read) ----------

(defun crc32-update (crc bytes start end)
  (declare (optimize (speed 3) (safety 0)) (type octets bytes) (fixnum start end) (type (unsigned-byte 32) crc))
  (let ((c (logxor crc #xFFFFFFFF)) (tab *crc-table*))
    (declare (type (unsigned-byte 32) c) (type (simple-array (unsigned-byte 32) (256)) tab))
    (loop for i fixnum from start below end
          do (setf c (logxor (aref tab (logand (logxor c (aref bytes i)) #xFF)) (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun write-png-rgba (path w h rgba)
  (declare (optimize (speed 3) (safety 0)) (fixnum w h) (type octets rgba))
  (let* ((row (1+ (* 4 w))) (rawlen (* h row))
         (raw (make-array rawlen :element-type '(unsigned-byte 8) :initial-element 0)))
    (declare (fixnum row rawlen) (type octets raw))
    (dotimes (y h) (replace raw rgba :start1 (1+ (* y row)) :start2 (* y 4 w) :end2 (* (1+ y) 4 w)))
    (let* ((nblocks (max 1 (ceiling rawlen 65535)))
           (zlen (+ 2 (* 5 nblocks) rawlen 4))
           (z (make-array zlen :element-type '(unsigned-byte 8)))
           (pos 2) (a 1) (b 0))
      (declare (fixnum nblocks zlen pos) (type (unsigned-byte 32) a b) (type octets z))
      (setf (aref z 0) #x78 (aref z 1) #x01)
      (dotimes (k nblocks)
        (let* ((start (* k 65535)) (len (min 65535 (- rawlen start))))
          (declare (fixnum start len))
          (setf (aref z pos) (if (= k (1- nblocks)) 1 0)
                (aref z (+ pos 1)) (logand len #xFF) (aref z (+ pos 2)) (ash len -8)
                (aref z (+ pos 3)) (logand (lognot len) #xFF) (aref z (+ pos 4)) (logand (ash (lognot len) -8) #xFF))
          (replace z raw :start1 (+ pos 5) :start2 start :end2 (+ start len))
          (incf pos (+ 5 len))))
      ;; Adler-32 of the raw rows
      (loop for i fixnum from 0 below rawlen
            do (setf a (mod (+ a (aref raw i)) 65521) b (mod (+ b a) 65521)))
      (let ((adl (logior (ash b 16) a)))
        (setf (aref z pos) (ldb (byte 8 24) adl) (aref z (+ pos 1)) (ldb (byte 8 16) adl)
              (aref z (+ pos 2)) (ldb (byte 8 8) adl) (aref z (+ pos 3)) (ldb (byte 8 0) adl)))
      (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (labels ((u32be (n) (dotimes (i 4) (write-byte (ldb (byte 8 (* 8 (- 3 i))) n) o)))
               (chunk (type data)
                 (let ((tb (map '(simple-array (unsigned-byte 8) (*)) #'char-code type)))
                   (u32be (length data))
                   (write-sequence tb o) (write-sequence data o)
                   (u32be (crc32-update (crc32-update 0 tb 0 4) data 0 (length data))))))
          (write-sequence (coerce '(137 80 78 71 13 10 26 10) '(simple-array (unsigned-byte 8) (*))) o)
          (let ((ihdr (make-array 13 :element-type '(unsigned-byte 8) :initial-element 0)))
            (dotimes (i 4) (setf (aref ihdr i) (ldb (byte 8 (* 8 (- 3 i))) w) (aref ihdr (+ 4 i)) (ldb (byte 8 (* 8 (- 3 i))) h)))
            (setf (aref ihdr 8) 8 (aref ihdr 9) 6)       ; 8 bits, RGBA
            (chunk "IHDR" ihdr))
          (chunk "IDAT" z)
          (chunk "IEND" (make-array 0 :element-type '(unsigned-byte 8))))))
    path))

;;; ---------- the canvas commands ----------

(defvar *view-pictures* (make-hash-table :test #'equal)
  "Per picture object: the key of the view last drawn (so an unchanged view isn't drawn again).")

(defun view-picture-cmds (d tags)
  "Canvas commands showing picture D as the current view sees it, or NIL (cannot be shown here:
no picture, edge-on, off the screen). Second value: the reason when it can't be read."
  (let ((png (raster-png (image-file d) (image-page d))))
    (if (null png)
        (values nil :unreadable)
        (destructuring-bind (pw ph) (image-px d)
          (let* ((ll (pt3 (dxf 10 d))) (u (image-u d)) (v (image-v d))
                 (top-left (v+ ll (v* v ph))))
            (multiple-value-bind (ox oy) (w->s top-left)
              (multiple-value-bind (x1 y1) (w->s (v+ top-left u))
                (multiple-value-bind (x2 y2) (w->s (v- top-left v))
                  (let* ((ax (- x1 ox)) (ay (- y1 oy)) (bx (- x2 ox)) (by (- y2 oy))
                         (det (- (* ax by) (* ay bx))))
                    (when (> (abs det) 1d-6)
                      (let* ((xs (list ox (+ ox (* pw ax)) (+ ox (* ph bx)) (+ ox (* pw ax) (* ph bx))))
                             (ys (list oy (+ oy (* pw ay)) (+ oy (* ph by)) (+ oy (* pw ay) (* ph by))))
                             (sx0 (max (floor (reduce #'min xs)) (floor *tx*)))
                             (sy0 (max (floor (reduce #'min ys)) (floor *ty*)))
                             (sx1 (min (ceiling (reduce #'max xs)) (ceiling (+ *tx* *cw*))))
                             (sy1 (min (ceiling (reduce #'max ys)) (ceiling (+ *ty* *ch*))))
                             (w (- sx1 sx0)) (h (- sy1 sy0)))
                        (when (and (> w 0) (> h 0) (< (* w h) 16000000))
                          (let* ((name (format nil "fpv_~A" (or (dxf 5 d) "x")))
                                 (bg (canvas-bg))
                                 (key (list png (image-fade d) bg sx0 sy0 w h
                                            (mapcar (lambda (x) (/ (round (* x 1d4)) 1d4)) (list ox oy ax ay bx by))))
                                 (file (format nil "~Aview-~A.png" (image-cache-dir) (or (dxf 5 d) "x")))
                                 (fresh (not (equal (gethash name *view-pictures*) key))))
                            (when fresh
                              (let ((levels (picture-levels png)))
                                (if (null levels)
                                    (return-from view-picture-cmds (values nil :unreadable))
                                    (let* ((per-px (max (sqrt (+ (* ax ax) (* ay ay))) (sqrt (+ (* bx bx) (* by by)))))
                                           ;; the copy whose pixels are about one screen pixel (or bigger)
                                           (k (max 0 (floor (log (/ 1d0 (max per-px 1d-9)) 2))))
                                           (pm (picture-level levels k))
                                           (ls (/ (d pw) (pixmap-w pm)))
                                           (rgba (render-picture pm ls ox oy ax ay bx by sx0 sy0 w h (image-fade d)
                                                                 (mapcar (lambda (c) (d c)) (hex-rgb bg)))))
                                      (write-png-rgba file w h rgba)
                                      (setf (gethash name *view-pictures*) key)))))
                            (list (format nil "fpimg_place ~A ~A ~D ~D ~D {~A}" name (tq file) (if fresh 1 0) sx0 sy0 tags)))))))))))))))

(defun image-render-cmds (d color tags hl)
  "Canvas commands for image D: the picture (when it can be shown) and its frame."
  (let* ((corners (image-corners d)) (frame (append corners (list (first corners))))
         (frame-cmds (screen-line frame (if hl "#8fc4ff" color) tags :dash hl))
         (out nil))
    (unless (image-hidden-p d)
      (multiple-value-bind (cmds why) (view-picture-cmds d tags)
        (if (eq why :unreadable)
            ;; not found or can't be read: a cross and the file's name
            (multiple-value-bind (x0 y0) (w->s (first corners))
              (multiple-value-bind (x1 y1) (w->s (third corners))
                (multiple-value-bind (x2 y2) (w->s (second corners))
                  (multiple-value-bind (x3 y3) (w->s (fourth corners))
                    (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}" (fmtn x0) (fmtn y0) (fmtn x1) (fmtn y1) color tags) out)
                    (push (format nil ".c create line ~A ~A ~A ~A -fill ~A -tags {~A}" (fmtn x2) (fmtn y2) (fmtn x3) (fmtn y3) color tags) out)
                    (push (format nil ".c create text ~A ~A -text ~A -fill ~A -font {Helvetica 9} -tags {~A}"
                                  (fmtn (/ (+ x0 x1) 2)) (fmtn (/ (+ y0 y1) 2))
                                  (tq (format nil "~A (~:[not found~;can't be read~])" (file-namestring (image-file d)) (probe-file (image-file d))))
                                  color tags)
                          out)))))
            (setf out (reverse cmds)))))
    (append (nreverse out) frame-cmds)))
