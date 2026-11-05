;;;; zip.lisp — reading zip archives (DWFx and DWF packages are zip files): the central directory,
;;;; and DEFLATE decompression (RFC 1951) written out here so no outside program is needed.

(in-package :fancy-pencil)

(defun u16le (v i) (logior (aref v i) (ash (aref v (+ i 1)) 8)))
(defun u32le (v i) (logior (aref v i) (ash (aref v (+ i 1)) 8) (ash (aref v (+ i 2)) 16) (ash (aref v (+ i 3)) 24)))

;;; ---------- inflate ----------

(defstruct (bitin (:constructor make-bitin (buf pos))) buf pos (bit 0) (val 0))

(declaim (inline bitin-bit))
(defun bitin-bits (b n)
  "N bits, least significant first."
  (let ((v 0))
    (dotimes (i n v)
      (when (zerop (bitin-bit b))
        (setf (bitin-val b) (if (< (bitin-pos b) (length (bitin-buf b))) (aref (bitin-buf b) (bitin-pos b)) 0))
        (incf (bitin-pos b)))
      (setf v (logior v (ash (logand (ash (bitin-val b) (- (bitin-bit b))) 1) i)))
      (setf (bitin-bit b) (mod (1+ (bitin-bit b)) 8)))))

(defun huffman-table (lengths)
  "Canonical Huffman decoding table from code LENGTHS: (counts . symbols)."
  (let* ((maxlen 16) (counts (make-array (1+ maxlen) :initial-element 0))
         (offs (make-array (1+ maxlen) :initial-element 0))
         (syms (make-array (length lengths) :initial-element 0)))
    (loop for l across lengths do (incf (aref counts l)))
    (setf (aref counts 0) 0)
    (loop for i from 1 below maxlen do (setf (aref offs (1+ i)) (+ (aref offs i) (aref counts i))))
    (loop for s from 0 below (length lengths)
          for l = (aref lengths s)
          when (plusp l) do (setf (aref syms (aref offs l)) s) (incf (aref offs l)))
    (cons counts syms)))

(defun huffman-decode (b table)
  (let ((counts (car table)) (syms (cdr table)) (code 0) (first 0) (index 0))
    (loop for len from 1 to 15
          do (setf code (logior code (bitin-bits b 1)))
             (let ((count (aref counts len)))
               (when (< (- code first) count)
                 (return-from huffman-decode (aref syms (+ index (- code first)))))
               (incf index count) (incf first count)
               (setf first (ash first 1) code (ash code 1))))
    (error "Bad compressed data.")))

(defparameter +len-base+ #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter +len-extra+ #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter +dist-base+ #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073
                            4097 6145 8193 12289 16385 24577))
(defparameter +dist-extra+ #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(defun fixed-tables ()
  (let ((l (make-array 288)) (dd (make-array 30 :initial-element 5)))
    (loop for i from 0 below 288 do (setf (aref l i) (cond ((< i 144) 8) ((< i 256) 9) ((< i 280) 7) (t 8))))
    (values (huffman-table l) (huffman-table dd))))

(defun inflate (buf &optional (start 0) expected-size)
  "Decompress raw DEFLATE data in BUF (octets) from START: a fresh octet vector."
  (let ((b (make-bitin buf start))
        (out (make-array (or expected-size 4096) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
      (let ((final (bitin-bits b 1)) (type (bitin-bits b 2)))
        (case type
          (0 (setf (bitin-bit b) 0)              ; stored: byte aligned
           (let* ((p (bitin-pos b)) (len (u16le buf p)))
             (loop for i from (+ p 4) below (+ p 4 len) do (vector-push-extend (aref buf i) out))
             (setf (bitin-pos b) (+ p 4 len))))
          ((1 2)
           (multiple-value-bind (lit dist)
               (if (= type 1) (fixed-tables)
                   (let* ((hlit (+ (bitin-bits b 5) 257)) (hdist (+ (bitin-bits b 5) 1)) (hclen (+ (bitin-bits b 4) 4))
                          (order #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
                          (cl (make-array 19 :initial-element 0)))
                     (dotimes (i hclen) (setf (aref cl (aref order i)) (bitin-bits b 3)))
                     (let ((clt (huffman-table cl)) (lens (make-array (+ hlit hdist) :initial-element 0)) (i 0))
                       (loop while (< i (+ hlit hdist))
                             do (let ((sym (huffman-decode b clt)))
                                  (cond ((< sym 16) (setf (aref lens i) sym) (incf i))
                                        ((= sym 16) (let ((prev (aref lens (1- i))))
                                                      (dotimes (k (+ 3 (bitin-bits b 2))) (setf (aref lens i) prev) (incf i))))
                                        ((= sym 17) (incf i (+ 3 (bitin-bits b 3))))
                                        (t (incf i (+ 11 (bitin-bits b 7)))))))
                       (values (huffman-table (subseq lens 0 hlit)) (huffman-table (subseq lens hlit))))))
             (loop
               (let ((sym (huffman-decode b lit)))
                 (cond ((< sym 256) (vector-push-extend sym out))
                       ((= sym 256) (return))
                       (t (let* ((k (- sym 257))
                                 (len (+ (aref +len-base+ k) (bitin-bits b (aref +len-extra+ k))))
                                 (dk (huffman-decode b dist))
                                 (back (+ (aref +dist-base+ dk) (bitin-bits b (aref +dist-extra+ dk))))
                                 (from (- (fill-pointer out) back)))
                            (dotimes (j len) (vector-push-extend (aref out (+ from j)) out)))))))))
          (t (error "Bad compressed data.")))
        (when (= final 1) (return))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; ---------- zip archives ----------

(defun zip-entries (bytes)
  "The files in zip archive BYTES: list of (name method compressed-size size local-offset)."
  (let ((eocd (loop for i from (- (length bytes) 22) downto (max 0 (- (length bytes) 65600))
                    when (and (= (aref bytes i) #x50) (= (aref bytes (+ i 1)) #x4B)
                              (= (aref bytes (+ i 2)) 5) (= (aref bytes (+ i 3)) 6))
                      return i)))
    (when eocd
      (let* ((n (u16le bytes (+ eocd 10))) (cd-size (u32le bytes (+ eocd 12))) (cd-off (u32le bytes (+ eocd 16)))
             ;; data in front of the archive (a classic DWF's "(DWF V06.00)" header) shifts every offset
             (delta (max 0 (- eocd (+ cd-off cd-size))))
             (p (+ cd-off delta)) (out nil))
        (dotimes (k n (nreverse out))
          (unless (= (u32le bytes p) #x02014B50) (return (nreverse out)))
          (let* ((method (u16le bytes (+ p 10))) (csize (u32le bytes (+ p 20))) (size (u32le bytes (+ p 24)))
                 (nlen (u16le bytes (+ p 28))) (xlen (u16le bytes (+ p 30))) (clen (u16le bytes (+ p 32)))
                 (off (u32le bytes (+ p 42)))
                 (name (map 'string #'code-char (subseq bytes (+ p 46) (+ p 46 nlen)))))
            (push (list name method csize size (+ off delta)) out)
            (setf p (+ p 46 nlen xlen clen))))))))

(defun zip-entry-bytes (bytes entry)
  (destructuring-bind (name method csize size off) entry
    (declare (ignore name))
    (let ((data (+ off 30 (u16le bytes (+ off 26)) (u16le bytes (+ off 28)))))
      (case method
        (0 (subseq bytes data (+ data csize)))
        (8 (inflate bytes data size))
        (t (error "Unsupported compression in the package."))))))

(defun zip-read (bytes name)
  "The contents of NAME (matched without case) inside zip BYTES, or NIL."
  (let ((e (find name (zip-entries bytes) :key #'first :test #'string-equal)))
    (and e (zip-entry-bytes bytes e))))

(defun octets-string (v)
  "UTF-8 (or UTF-16 with a byte-order mark) octets as a string."
  (cond ((and (>= (length v) 2) (= (aref v 0) #xFF) (= (aref v 1) #xFE))
         (let ((s (make-string (floor (- (length v) 2) 2))))
           (dotimes (i (length s) s) (setf (char s i) (code-char (u16le v (+ 2 (* 2 i))))))))
        (t #+sbcl (sb-ext:octets-to-string v :external-format :utf-8)
           #-sbcl (map 'string #'code-char v))))
