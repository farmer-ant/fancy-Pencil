;;;; dgn.lisp — reading MicroStation DGN V7 files (ISFF), for DGN underlays.
;;;; A V7 file is a run of elements, each with a 36-byte header (level, type, size, range,
;;;; symbology) and its data; coordinates are whole "units of resolution" (UORs), turned into master
;;;; units with the file's TCB (first element). Lines, line strings, shapes, curves, arcs, ellipses
;;;; and text are read (cells and complex chains by their parts). V8 files (an OLE container,
;;;; MicroStation's closed format since 2000) are recognised but not read.

(in-package :fancy-pencil)

(defun dgn-int32 (v i)
  "A DGN 32-bit integer: two 16-bit little-endian words, the high word first."
  (let ((u (logior (aref v (+ i 2)) (ash (aref v (+ i 3)) 8) (ash (aref v i) 16) (ash (aref v (+ i 1)) 24))))
    (if (>= u #x80000000) (- u #x100000000) u)))

(defun dgn-double (v i)
  "A VAX D-float (as DGN stores reals) as a double."
  (let* ((w0 (u16le v i)) (w1 (u16le v (+ i 2))) (w2 (u16le v (+ i 4))) (w3 (u16le v (+ i 6)))
         (m (logior (ash w0 48) (ash w1 32) (ash w2 16) w3))
         (s (ldb (byte 1 63) m)) (e (ldb (byte 8 55) m)) (f (ldb (byte 55 0) m)))
    (if (zerop e) 0d0
        (* (if (= s 1) -1 1) (scale-float (+ 1d0 (/ (d f) (expt 2d0 55))) (- e 129))))))

(defun dgn-kind (bytes)
  (cond ((and (> (length bytes) 4) (= (aref bytes 0) #xD0) (= (aref bytes 1) #xCF) (= (aref bytes 2) #x11) (= (aref bytes 3) #xE0))
         :v8)
        ((and (> (length bytes) 1536) (= (logand (aref bytes 1) #x7F) 9)) :v7)
        (t nil)))

(defparameter +dgn-colors+ #(7 5 3 1 2 6 30 4)
  "MicroStation's first eight standard colours as AutoCAD colours (others shown in colour 8).")

(defun dgn-color (i) (if (< i (length +dgn-colors+)) (aref +dgn-colors+ i) 8))

(defun read-dgn (path)
  "The elements of DGN V7 file PATH as display entities in master units: (values entities kind),
KIND being :v7, :v8 (not readable) or NIL (not a DGN file)."
  (let* ((bytes (read-file-bytes path)) (kind (dgn-kind bytes)))
    (unless (eq kind :v7) (return-from read-dgn (values nil kind)))
    (let* ((three-d (logtest (aref bytes 1214) #x40))
           (subs (max 1 (dgn-int32 bytes 1112))) (uor (max 1 (dgn-int32 bytes 1116)))
           (k (/ 1d0 (* subs uor)))
           (ox (dgn-double bytes 1240)) (oy (dgn-double bytes 1248))
           (out nil) (p 0) (n (length bytes)))
      (labels ((xy (i) (pt (* k (- (dgn-int32 bytes i) ox)) (* k (- (dgn-int32 bytes (+ i 4)) oy))))
               (dxy (i) (pt (* k (- (dgn-double bytes i) ox)) (* k (- (dgn-double bytes (+ i 8)) oy))))
               (ang (i) (rad (/ (d (dgn-int32 bytes i)) 360000d0)))
               (sweep (i) (let ((u (logior (aref bytes (+ i 2)) (ash (aref bytes (+ i 3)) 8)
                                           (ash (aref bytes i) 16) (ash (aref bytes (+ i 1)) 24))))
                            (rad (/ (d (if (logtest u #x80000000) (- (logand u #x7FFFFFFF)) u)) 360000d0))))
               (add (e color) (push (append e (list (cons 62 (dgn-color color)))) out))
               (verts (i count) (loop for j below count collect (xy (+ i (* j (if three-d 12 8)))))))
        (loop while (< (+ p 36) n)
              do (let* ((b0 (aref bytes p)) (b1 (aref bytes (+ p 1))))
                   (when (and (= b0 #xFF) (= b1 #xFF)) (return))
                   (let* ((type (logand b1 #x7F)) (deleted (logtest b1 #x80))
                          (size (+ 4 (* 2 (u16le bytes (+ p 2)))))
                          (color (aref bytes (+ p 35))))
                     (when (or (< size 4) (> (+ p size) n)) (return))
                     (unless (or deleted (member type '(0 1 5 8 9 10 66)))
                       (ignore-errors
                        (let ((e p))
                          (case type
                            (3 (let ((a (xy (+ e 36))) (b2 (xy (+ e (if three-d 48 44)))))
                                 (add (line-data a b2) color)))
                            ((4 6 11)
                             (let* ((cnt (u16le bytes (+ e 36))) (vs (verts (+ e 38) (min cnt 5000))))
                               (when (>= (length vs) 2)
                                 (add (make-lwpoly-data vs (= type 6)) color))))
                            ((15 16)
                             (let* ((arc (= type 16))
                                    (o (+ e 36 (if arc 8 0)))
                                    (a1 (if arc (ang (+ e 36)) 0d0))
                                    (sw (if arc (sweep (+ e 40)) (* 2 +pi+)))
                                    (r1 (* k (dgn-double bytes o))) (r2 (* k (dgn-double bytes (+ o 8))))
                                    (rot (if three-d 0d0 (ang (+ o 16))))
                                    (c (dxy (+ o (if three-d 32 20)))))
                               (let ((pts (loop for i from 0 to 72
                                                for tt = (+ a1 (* sw (/ i 72d0)))
                                                collect (let ((x (* r1 (cos tt))) (y (* r2 (sin tt))))
                                                          (pt (+ (px c) (- (* x (cos rot)) (* y (sin rot))))
                                                              (+ (py c) (+ (* x (sin rot)) (* y (cos rot)))))))))
                                 (add (make-lwpoly-data pts (not arc)) color))))
                            (17 (let* ((h (* k (/ (* 6d0 (dgn-int32 bytes (+ e 42))) 1000d0)))
                                       (rot (if three-d 0d0 (ang (+ e 46))))
                                       (o (xy (+ e (if three-d 62 50))))
                                       (cnt (aref bytes (+ e (if three-d 74 58))))
                                       (s (map 'string #'code-char (subseq bytes (+ e (if three-d 76 60)) (+ e (if three-d 76 60) cnt)))))
                                  (when (plusp (length (string-trim " " s)))
                                    (add (list (cons 0 "TEXT") (cons 10 o) (cons 40 (max h 1d-6)) (cons 1 s)
                                               (cons 50 rot) (cons 7 "Standard"))
                                         color))))))))
                     (incf p size))))
        (values (nreverse out) kind)))))
