;;;; usgs.lisp — ground models and contour lines from USGS maps (The National Map, 3DEP)
;;;;   DEMIMPORT      a height map (GeoTIFF DEM: 1 arc-second, 1/3 arc-second, 1 meter ...) becomes a
;;;;                  surface mesh and a 3D solid of the ground, for an area you choose
;;;;   CONTOURIMPORT  contour lines (shapefile, or the .zip as downloaded) become polylines at their heights
;;;; Both put a place's latitude and longitude at 0,0 (a note on layer GEO_ORIGIN says which), x east
;;;; and y north in the drawing's units, so a second map of the same place lines up with the first.

(in-package :fancy-pencil)

;;; ---------------------------------------------------------------- bytes

(defun read-octets (stream offset n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (file-position stream offset)
    (let ((got (read-sequence v stream)))
      (if (< got n) (subseq v 0 got) v))))

(defun octets-uint (v i n le)
  (let ((x 0))
    (if le
        (loop for k from (1- n) downto 0 do (setf x (logior (ash x 8) (aref v (+ i k)))))
        (loop for k below n do (setf x (logior (ash x 8) (aref v (+ i k))))))
    x))

(defun octets-sint (v i n le)
  (let ((x (octets-uint v i n le)))
    (if (logbitp (1- (* 8 n)) x) (- x (ash 1 (* 8 n))) x)))

(defun bits-float (x bytes)
  "The IEEE float with bit pattern X (4 or 8 bytes) as a double."
  (if (= bytes 4)
      (d (sb-kernel:make-single-float (if (logbitp 31 x) (- x (ash 1 32)) x)))
      (let ((hi (ash x -32)) (lo (ldb (byte 32 0) x)))
        (sb-kernel:make-double-float (if (logbitp 31 hi) (- hi (ash 1 32)) hi) lo))))

;;; ---------------------------------------------------------------- TIFF

(defstruct tiff path stream le big width height bps fmt spp compression predictor
  tile-w tile-h offsets counts tags (cache (make-hash-table)) nodata)

(defparameter +tiff-type-size+ '((1 . 1) (2 . 1) (3 . 2) (4 . 4) (5 . 8) (6 . 1) (7 . 1) (8 . 2) (9 . 4)
                                 (10 . 8) (11 . 4) (12 . 8) (16 . 8) (17 . 8) (18 . 8)))

(defun tiff-values (stream le type count v pos)
  "COUNT values of TIFF TYPE in octets V from POS."
  (let ((size (or (cdr (assoc type +tiff-type-size+)) 1)))
    (if (= type 2)
        (string-right-trim '(#\Nul) (map 'string #'code-char (subseq v pos (+ pos count))))
        (loop for k below count
              for i = (+ pos (* k size))
              collect (case type
                        ((1 7) (aref v i))
                        (6 (octets-sint v i 1 le))
                        (3 (octets-uint v i 2 le)) (8 (octets-sint v i 2 le))
                        (4 (octets-uint v i 4 le)) (9 (octets-sint v i 4 le))
                        ((16 18) (octets-uint v i 8 le)) (17 (octets-sint v i 8 le))
                        (5 (let ((dn (octets-uint v (+ i 4) 4 le))) (if (zerop dn) 0 (/ (octets-uint v i 4 le) dn))))
                        (10 (let ((dn (octets-sint v (+ i 4) 4 le))) (if (zerop dn) 0 (/ (octets-sint v i 4 le) dn))))
                        (11 (bits-float (octets-uint v i 4 le) 4))
                        (12 (bits-float (octets-uint v i 8 le) 8))
                        (t (aref v i)))))))

(defun open-tiff (path)
  "The first image of the TIFF (or BigTIFF) at PATH; its tags are read, its pixels when asked for."
  (let* ((s (open path :element-type '(unsigned-byte 8)))
         (h (read-octets s 0 16))
         (le (cond ((and (= (aref h 0) 73) (= (aref h 1) 73)) t)
                   ((and (= (aref h 0) 77) (= (aref h 1) 77)) nil)
                   (t (close s) (error "~A is not a TIFF file." (file-namestring path)))))
         (magic (octets-uint h 2 2 le))
         (big (case magic (42 nil) (43 t) (t (close s) (error "~A is not a TIFF file." (file-namestring path)))))
         (ifd (if big (octets-uint h 8 8 le) (octets-uint h 4 4 le)))
         (n (octets-uint (read-octets s ifd 8) 0 (if big 8 2) le))
         (esize (if big 20 12))
         (ents (read-octets s (+ ifd (if big 8 2)) (* n esize)))
         (tags (make-hash-table)))
    (dotimes (k n)
      (let* ((i (* k esize))
             (tag (octets-uint ents i 2 le)) (type (octets-uint ents (+ i 2) 2 le))
             (count (if big (octets-uint ents (+ i 4) 8 le) (octets-uint ents (+ i 4) 4 le)))
             (bytes (* count (or (cdr (assoc type +tiff-type-size+)) 1)))
             (inline (<= bytes (if big 8 4)))
             (vpos (+ i (if big 12 8))))
        (setf (gethash tag tags)
              (if inline
                  (tiff-values s le type count ents vpos)
                  (tiff-values s le type count
                               (read-octets s (if big (octets-uint ents vpos 8 le) (octets-uint ents vpos 4 le)) bytes) 0)))))
    (flet ((one (tag &optional dflt) (let ((v (gethash tag tags))) (if (consp v) (first v) (or v dflt)))))
      (let* ((w (one 256)) (hgt (one 257))
             (tiled (gethash 322 tags)))
        (make-tiff :path path :stream s :le le :big big :width w :height hgt
                   :bps (one 258 1) :fmt (one 339 1) :spp (one 277 1) :compression (one 259 1) :predictor (one 317 1)
                   :tile-w (if tiled (one 322) w) :tile-h (if tiled (one 323) (min hgt (one 278 hgt)))
                   :offsets (coerce (gethash (if tiled 324 273) tags) 'vector)
                   :counts (coerce (gethash (if tiled 325 279) tags) 'vector)
                   :tags tags
                   :nodata (let ((nd (gethash 42113 tags))) (and (stringp nd) (ignore-errors (d (read-from-string nd))))))))))

(defun close-tiff (tf) (ignore-errors (close (tiff-stream tf))))

(defun lzw-decode (in &optional (expected 0))
  "TIFF LZW: codes of 9 to 12 bits, most significant bit first."
  (let ((out (make-array (max 1024 expected) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (table (make-array 4096)) (next 258) (width 9) (old nil) (bitpos 0) (nbits (* 8 (length in))))
    (dotimes (i 256) (setf (aref table i) (make-array 1 :element-type '(unsigned-byte 8) :initial-element i)))
    (flet ((code ()
             (when (> (+ bitpos width) nbits) (return-from lzw-decode out))
             (let ((c 0))
               (dotimes (k width c)
                 (let ((b (+ bitpos k)))
                   (setf c (logior (ash c 1) (ldb (byte 1 (- 7 (mod b 8))) (aref in (floor b 8)))))))
               (incf bitpos width)
               c))
           (emit (v) (loop for x across v do (vector-push-extend x out)))
           (join (a b) (let ((r (make-array (1+ (length a)) :element-type '(unsigned-byte 8))))
                         (replace r a) (setf (aref r (length a)) b) r)))
      (loop
        (let ((c (code)))
          (cond ((= c 257) (return out))
                ((= c 256) (setf next 258 width 9 old nil))
                ((null old) (when (< c 256) (emit (aref table c))) (setf old c))
                (t (let ((entry (if (< c next) (aref table c)
                                    (join (aref table old) (aref (aref table old) 0)))))
                     (emit entry)
                     (when (< next 4096)
                       (setf (aref table next) (join (aref table old) (aref entry 0)))
                       (incf next))
                     (when (and (>= (1+ next) (ash 1 width)) (< width 12)) (incf width))
                     (setf old c)))))))))

(defun tiff-chunk (tf idx rows)
  "Chunk (tile or strip) IDX of TF, ROWS rows high, decoded: a vector of doubles, tile-w per row."
  (or (gethash idx (tiff-cache tf))
      (let* ((tw (tiff-tile-w tf)) (bps (tiff-bps tf)) (spp (tiff-spp tf)) (le (tiff-le tf))
             (nb (floor bps 8)) (rowbytes (* tw spp nb))
             (raw (read-octets (tiff-stream tf) (aref (tiff-offsets tf) idx) (aref (tiff-counts tf) idx)))
             (bytes (case (tiff-compression tf)
                      (1 raw)
                      (5 (lzw-decode raw (* rows rowbytes)))
                      ((8 32946) (inflate raw 2 (* rows rowbytes)))   ; (a zlib stream: 2 header bytes)
                      (t (error "This TIFF is compressed in a way Fancy Pencil can't read (method ~A)." (tiff-compression tf)))))
             (rows (min rows (floor (length bytes) rowbytes)))
             (out (make-array (* tw rows) :element-type 'double-float :initial-element 0d0)))
        (when (> (hash-table-count (tiff-cache tf)) 256) (clrhash (tiff-cache tf)))
        (case (tiff-predictor tf)
          (3                            ; floating point: bytes differenced, then split by significance
           (let ((tmp (make-array rowbytes :element-type '(unsigned-byte 8))) (n (* tw spp)))
             (dotimes (r rows)
               (let ((o (* r rowbytes)))
                 (loop for i from (+ o spp) below (+ o rowbytes)
                       do (setf (aref bytes i) (ldb (byte 8 0) (+ (aref bytes i) (aref bytes (- i spp))))))
                 (dotimes (k n)
                   (dotimes (j nb) (setf (aref tmp (+ (* k nb) j)) (aref bytes (+ o (* j n) k)))))
                 (dotimes (c tw)
                   (setf (aref out (+ (* r tw) c)) (bits-float (octets-uint tmp (* c spp nb) nb nil) nb)))))))
          (t
           (dotimes (r rows)
             (let ((prev 0))
               (dotimes (c tw)
                 (let* ((i (+ (* r rowbytes) (* c spp nb)))
                        (v (case (tiff-fmt tf)
                             (3 (bits-float (octets-uint bytes i nb le) nb))
                             (2 (octets-sint bytes i nb le))
                             (t (octets-uint bytes i nb le)))))
                   (when (and (eql (tiff-predictor tf) 2) (/= (tiff-fmt tf) 3))
                     ;; integers differenced along the row
                     (let ((m (ash 1 bps)))
                       (setf v (mod (+ (mod v m) prev) m) prev v)
                       (when (and (= (tiff-fmt tf) 2) (logbitp (1- bps) v)) (setf v (- v m)))))
                   (setf (aref out (+ (* r tw) c)) (d v))))))))
        (setf (gethash idx (tiff-cache tf)) out))))

(defun tiff-value (tf col row)
  (let* ((tw (tiff-tile-w tf)) (th (tiff-tile-h tf))
         (across (ceiling (tiff-width tf) tw))
         (idx (+ (* (floor row th) across) (floor col tw)))
         (chunk (tiff-chunk tf idx th))
         (i (+ (* (mod row th) tw) (mod col tw))))
    (if (< i (length chunk)) (aref chunk i) nil)))

;;; ---------------------------------------------------------------- where on Earth

;; GRS 80 (NAD83), close enough to WGS 84 for this
(defparameter +earth-a+ 6378137d0)
(defparameter +earth-f+ (/ 1d0 298.257222101d0))

(defun meridian-arc (lat)
  "Distance in metres along a meridian from the equator to latitude LAT (degrees)."
  (let* ((f +earth-f+) (n (/ f (- 2 f))) (n2 (* n n)) (phi (rad lat)))
    (* (/ +earth-a+ (+ 1 n)) (+ (* (+ 1 (/ n2 4) (/ (* n2 n2) 64)) phi)
                                (* -1.5d0 (- n (/ (* n2 n) 8)) (sin (* 2 phi)))
                                (* (/ 15 16d0) (- n2 (/ (* n2 n2) 4)) (sin (* 4 phi)))
                                (* (/ -35 48d0) n2 n (sin (* 6 phi)))))))

(defun prime-radius (lat)
  (let ((e2 (* +earth-f+ (- 2 +earth-f+))) (s (sin (rad lat))))
    (/ +earth-a+ (sqrt (- 1 (* e2 s s))))))

(defun utm->latlon (e n zone &optional south)
  "Latitude and longitude (degrees) of UTM easting E, northing N (metres) in ZONE."
  (let* ((k0 0.9996d0) (a +earth-a+) (e2 (* +earth-f+ (- 2 +earth-f+))) (ep2 (/ e2 (- 1 e2)))
         (x (- e 500000d0)) (y (if south (- n 10000000d0) n))
         (lon0 (rad (- (* 6 zone) 183)))
         (m (/ y k0))
         (mu (/ m (* a (- 1 (/ e2 4) (/ (* 3 e2 e2) 64) (/ (* 5 e2 e2 e2) 256)))))
         (e1 (/ (- 1 (sqrt (- 1 e2))) (+ 1 (sqrt (- 1 e2)))))
         (phi1 (+ mu (* (- (* 1.5d0 e1) (* 27/32 e1 e1 e1)) (sin (* 2 mu)))
                  (* (- (* 21/16 e1 e1) (* 55/32 e1 e1 e1 e1)) (sin (* 4 mu)))
                  (* 151/96 e1 e1 e1 (sin (* 6 mu)))
                  (* 1097/512 e1 e1 e1 e1 (sin (* 8 mu)))))
         (c1 (* ep2 (expt (cos phi1) 2))) (t1 (expt (tan phi1) 2))
         (n1 (/ a (sqrt (- 1 (* e2 (expt (sin phi1) 2))))))
         (r1 (/ (* a (- 1 e2)) (expt (- 1 (* e2 (expt (sin phi1) 2))) 1.5d0)))
         (dd (/ x (* n1 k0))))
    (values (deg (- phi1 (* (/ (* n1 (tan phi1)) r1)
                            (+ (/ (* dd dd) 2)
                               (* (- (+ 5 (* 3 t1) (* 10 c1)) (* 4 c1 c1) (* 9 ep2)) (expt dd 4) 1/24)
                               (* (- (+ 61 (* 90 t1) (* 298 c1) (* 45 t1 t1)) (* 252 ep2) (* 3 c1 c1)) (expt dd 6) 1/720)))))
            (deg (+ lon0 (/ (+ dd (* -1 (+ 1 (* 2 t1) c1) (expt dd 3) 1/6)
                               (* (+ (- 5 (* 2 c1)) (* 28 t1) (* -3 c1 c1) (* 8 ep2) (* 24 t1 t1)) (expt dd 5) 1/120))
                            (cos phi1)))))))

(defun latlon->local (lat lon lat0 lon0)
  "Metres east and north of LAT0,LON0 (degrees)."
  (values (* (rad (- lon lon0)) (prime-radius lat) (cos (rad lat)))
          (- (meridian-arc lat) (meridian-arc lat0))))

(defun local->latlon (x y lat0 lon0)
  "The latitude and longitude X metres east and Y north of LAT0,LON0."
  (let ((lat (+ lat0 (deg (/ y +earth-a+)))))
    ;; a few steps put LAT where the meridian distance is right
    (loop repeat 6
          do (let* ((target (+ (meridian-arc lat0) y)) (err (- target (meridian-arc lat))))
               (incf lat (deg (/ err +earth-a+)))))
    (values lat (+ lon0 (deg (/ x (* (prime-radius lat) (cos (rad lat)))))))))

;;; How a map's coordinates turn into latitude and longitude: (:geo), (:utm zone south), or
;;; (:plane metres-per-unit) for other projections (kept as they are, in metres).

(defun map->latlon (crs x y)
  (ecase (first crs)
    (:geo (values y x))
    (:utm (utm->latlon x y (second crs) (third crs)))))

(defun crs-label (crs)
  (case (first crs)
    (:geo "latitude and longitude")
    (:utm (format nil "UTM zone ~D~:[N~;S~]" (second crs) (third crs)))
    (t "a map projection")))

;;; ---------------------------------------------------------------- the origin note

(defparameter +geo-layer+ "GEO_ORIGIN")

(defun fmt-latlon (lat lon)
  (format nil "~,6F~A ~,6F~A" (abs lat) (if (minusp lat) "S" "N") (abs lon) (if (minusp lon) "W" "E")))

(defun parse-latlon (s)
  "Latitude and longitude typed as \"37.75, -122.45\", \"37.75N 122.45W\" or \"37 45 0 N, 122 27 W\"
(degrees, minutes, seconds; S and W south and west), or NIL."
  (let* ((s (substitute #\Space #\% (string-upcase s)))
         (s (substitute-if #\Space (lambda (c) (member c '(#\° #\' #\" #\D))) s))
         (halves (let ((comma (position #\, s)))
                   (if comma
                       (list (subseq s 0 comma) (subseq s (1+ comma)))
                       (let ((ns (position-if (lambda (c) (member c '(#\N #\S #\E #\W))) s)))
                         (if (and ns (< (1+ ns) (length (string-right-trim " " s))))
                             (list (subseq s 0 (1+ ns)) (subseq s (1+ ns)))
                             ;; two numbers apart
                             (let ((nums (number-runs s)))
                               (and (= (length nums) 2) (list (first nums) (second nums))))))))))
    (when (= (length halves) 2)
      (flet ((angle (h)
               (let ((nums (mapcar (lambda (x) (ignore-errors (let ((*read-default-float-format* 'double-float))
                                                                  (read-from-string x))))
                                   (number-runs h))))
                 (when (and nums (every #'realp nums) (<= (length nums) 3))
                   (let ((v (+ (abs (first nums)) (/ (or (second nums) 0) 60d0) (/ (or (third nums) 0) 3600d0))))
                     (values (d (if (or (minusp (first nums)) (find #\S h) (find #\W h)) (- v) v))
                             (cond ((or (find #\N h) (find #\S h)) :lat) ((or (find #\E h) (find #\W h)) :lon))))))))
        (multiple-value-bind (a ka) (angle (first halves))
          (multiple-value-bind (b kb) (angle (second halves))
            (when (and a b)
              (if (or (eq ka :lon) (eq kb :lat)) (values b a) (values a b)))))))))

(defun number-runs (s)
  "The numbers written in S, as strings."
  (let ((out nil) (i 0) (n (length s)))
    (loop while (< i n)
          do (if (or (digit-char-p (char s i))
                     (and (member (char s i) '(#\- #\+ #\.)) (< (1+ i) n) (or (digit-char-p (char s (1+ i))) (char= (char s (1+ i)) #\.))))
                 (let ((j (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.))) s :start (1+ i)) n)))
                   (push (subseq s i j) out) (setf i j))
                 (incf i)))
    (nreverse out)))

(defun geo-origin ()
  "The latitude and longitude at 0,0 from the drawing's GEO_ORIGIN note: (values lat lon), or NIL."
  (dolist (e (live-entities))
    (let ((dd (ent-data e)))
      (when (and (member (dxf 0 dd) '("TEXT" "MTEXT") :test #'equal) (string-equal (dxf 8 dd) +geo-layer+))
        (let* ((s (or (dxf 1 dd) "")) (at (search " is " s)))
          (when at
            (multiple-value-bind (lat lon) (parse-latlon (subseq s (+ at 4)))
              (when lat (return-from geo-origin (values lat lon))))))))))

(defun add-geo-origin (lat lon size source)
  (ensure-layer +geo-layer+)
  (add (list (cons 0 "TEXT") (cons 8 +geo-layer+) (cons 10 (pt 0 (* -2.5 size))) (cons 40 size)
             (cons 1 (format nil "0,0 is ~A" (fmt-latlon lat lon)))))
  (add (list (cons 0 "TEXT") (cons 8 +geo-layer+) (cons 10 (pt 0 (* -4 size))) (cons 40 (* 0.6 size))
             (cons 1 (format nil "From ~A. X east, Y north, in ~A." source (geo-units-name))))))

;;; ---------------------------------------------------------------- the drawing's units

(defun units-per-metre ()
  (case (sysvar "INSUNITS") (1 (/ 1 0.0254d0)) (2 (/ 1 0.3048d0)) (3 (/ 1 1609.344d0)) (4 1000d0) (5 100d0)
    (7 0.001d0) (10 (/ 1 0.9144d0)) (t 1d0)))

(defun geo-units-name ()
  (case (sysvar "INSUNITS") (1 "inches") (2 "feet") (3 "miles") (4 "millimetres") (5 "centimetres")
    (7 "kilometres") (10 "yards") (t "metres")))

(defun ask-drawing-units ()
  "Asks for metres or feet when the drawing has no units yet."
  (unless (member (sysvar "INSUNITS") '(1 2 3 4 5 6 7 10))
    (let ((u (gkw "Specify drawing units [Meters/Feet] <Meters>: " '("Meters" "Feet") "Meters")))
      (setf (sysvar "INSUNITS") (if (equal u "Feet") 2 6)))))

(defun parse-length (s)
  "A length typed as \"2km\", \"1 mi\", \"500 m\", \"3000ft\" or a plain number (drawing units), in metres."
  (let* ((s (string-trim " " (string-downcase s)))
         (j (or (position-if-not (lambda (c) (or (digit-char-p c) (member c '(#\. #\- #\+)))) s) (length s)))
         (v (ignore-errors (let ((*read-default-float-format* 'double-float)) (read-from-string (subseq s 0 j)))))
         (u (string-trim " ." (subseq s j))))
    (when (and (realp v) (plusp v))
      (let ((f (cond ((member u '("km" "kilometer" "kilometers" "kilometre" "kilometres") :test #'equal) 1000d0)
                     ((member u '("m" "meter" "meters" "metre" "metres") :test #'equal) 1d0)
                     ((member u '("mi" "mile" "miles") :test #'equal) 1609.344d0)
                     ((member u '("ft" "feet" "foot" "'") :test #'equal) 0.3048d0)
                     ((member u '("yd" "yard" "yards") :test #'equal) 0.9144d0)
                     ((equal u "") (/ 1 (units-per-metre)))
                     (t nil))))
        (and f (* v f))))))

(defun trim-number (x)
  "X with no more decimals than it needs (20, 2.5, 0.125)."
  (let ((s (string-right-trim "." (string-right-trim "0" (format nil "~,4F" x))))) (if (equal s "") "0" s)))

(defun fmt-metres (m)
  (if (>= m 1000) (format nil "~,2F km (~,2F mi)" (/ m 1000) (/ m 1609.344))
      (format nil "~,1F m (~:D ft)" m (round m 0.3048))))

;;; ---------------------------------------------------------------- choosing the area

(defun ask-map-area (lat-lo lat-hi lon-lo lon-hi &key (default "Center") (lat-c nil) (lon-c nil))
  "Asks which part of a map to use. The map covers LAT-LO..LAT-HI, LON-LO..LON-HI (degrees).
Returns (values lat-lo lat-hi lon-lo lon-hi), or NIL."
  (let ((how (gkw (format nil "Specify area [Whole/Center/Corners] <~A>: " default) '("Whole" "Center" "Corners") default)))
    (cond
      ((equal how "Whole") (values lat-lo lat-hi lon-lo lon-hi))
      ((equal how "Center")
       (let* ((c0 (fmt-latlon (or lat-c (/ (+ lat-lo lat-hi) 2)) (or lon-c (/ (+ lon-lo lon-hi) 2)))))
         (multiple-value-bind (lat lon)
             (let ((s (gstring (format nil "Specify centre (latitude, longitude) <~A>: " c0) t)))
               (parse-latlon (if (plusp (length s)) s c0)))
           (if (null lat)
               (progn (out (format nil "Type a latitude and longitude, such as 37.75, -122.45 or 37.75N 122.45W.~%")) nil)
               (let* ((s (gstring "Specify width of the area <1 km>: " t))
                      (w (if (plusp (length s)) (parse-length s) 1000d0)))
                 (if (null w)
                     (progn (out (format nil "Type a width such as 2 km, 1 mi, 800 m or 3000 ft.~%")) nil)
                     (multiple-value-bind (la0 lo0) (local->latlon (- (/ w 2)) (- (/ w 2)) lat lon)
                       (multiple-value-bind (la1 lo1) (local->latlon (/ w 2) (/ w 2) lat lon)
                         (values la0 la1 lo0 lo1)))))))))
      ((equal how "Corners")
       (multiple-value-bind (a1 o1) (parse-latlon (gstring "Specify first corner (latitude, longitude): " t))
         (multiple-value-bind (a2 o2) (and a1 (parse-latlon (gstring "Specify opposite corner (latitude, longitude): " t)))
           (if (null a2)
               (progn (out (format nil "Type a latitude and longitude, such as 37.75, -122.45 or 37.75N 122.45W.~%")) nil)
               (values (min a1 a2) (max a1 a2) (min o1 o2) (max o1 o2)))))))))

;;; ---------------------------------------------------------------- DEMIMPORT

(defun tiff-geo (tf)
  "How TF's pixels map onto the Earth: (values crs map-x-of map-y-of), the two functions taking a
pixel's column and row (its centre) to map coordinates; or NIL without GeoTIFF tags."
  (let* ((tags (tiff-tags tf))
         (scale (gethash 33550 tags)) (tie (gethash 33922 tags)) (mat (gethash 34264 tags))
         (keys (let ((k (gethash 34735 tags)) (h (make-hash-table)))
                 (when (and (consp k) (> (length k) 4))
                   (loop for (id loc count val) on (nthcdr 4 k) by (lambda (l) (nthcdr 4 l))
                         when (and id (eql loc 0)) do (setf (gethash id h) val)))
                 h))
         (ascii (let ((a (gethash 34737 tags))) (if (stringp a) a "")))
         (area (not (eql (gethash 1025 keys) 2)))   ; PixelIsArea: corners at whole pixels
         (half (if area 0.5d0 0d0)))
    (multiple-value-bind (fx fy)
        (cond (mat (destructuring-bind (a b c dd e f g h &rest r) mat
                     (declare (ignore c g r))
                     (values (lambda (col row) (+ (* a (+ col half)) (* b (+ row half)) dd))
                             (lambda (col row) (+ (* e (+ col half)) (* f (+ row half)) h)))))
              ((and scale tie)
               (destructuring-bind (i j k x y &rest r) tie
                 (declare (ignore k r))
                 (let ((sx (first scale)) (sy (second scale)))
                   (values (lambda (col row) (declare (ignore row)) (+ x (* (- (+ col half) i) sx)))
                           (lambda (col row) (declare (ignore col)) (- y (* (- (+ row half) j) sy)))))))
              (t (return-from tiff-geo nil)))
      (let* ((model (gethash 1024 keys))
             (pcs (gethash 3072 keys)) (proj (gethash 3074 keys))
             (zone-from-text (let ((p (search "ZONE" (string-upcase ascii))))
                               (and p (let* ((rest (string-trim " _" (subseq ascii (+ p 4))))
                                             (j (or (position-if-not #'digit-char-p rest) (length rest)))
                                             (z (ignore-errors (parse-integer rest :end j))))
                                        (and z (list z (and (< j (length rest)) (char-equal (char rest j) #\S))))))))
             (crs (cond ((or (eql model 2) (and (null model) (< (abs (funcall fx 1 0)) 361) (< (abs (- (funcall fx 1 0) (funcall fx 0 0))) 0.01)))
                         (list :geo))
                        ((and pcs (<= 26901 pcs 26923)) (list :utm (- pcs 26900) nil))
                        ((and pcs (<= 32601 pcs 32660)) (list :utm (- pcs 32600) nil))
                        ((and pcs (<= 32701 pcs 32760)) (list :utm (- pcs 32700) t))
                        ((and pcs (<= 6330 pcs 6348)) (list :utm (- pcs 6329) nil))      ; NAD83(2011)
                        ((and pcs (<= 6328 pcs 6329)) (list :utm (+ 59 (- pcs 6328)) nil))
                        ((and proj (<= 16001 proj 16060)) (list :utm (- proj 16000) nil))
                        ((and proj (<= 16101 proj 16160)) (list :utm (- proj 16100) t))
                        (zone-from-text (list :utm (first zone-from-text) (second zone-from-text)))
                        (t (list :plane (case (gethash 3076 keys) (9002 0.3048d0) (9003 (/ 1200 3937d0)) (t 1d0)))))))
        (values crs fx fy (case (gethash 4099 keys) (9002 0.3048d0) (9003 (/ 1200 3937d0)) (t 1d0)))))))

(defun grid-mesh-faces (index nr nc)
  "Faces for a grid of NR x NC heights (INDEX: row-major vertex numbers from 1, or NIL where there is
no height): two triangles a square, the diagonal hidden. Also the triangles as (i j k) from 0."
  (let ((faces nil) (tris nil))
    (dotimes (r (1- nr))
      (dotimes (c (1- nc))
        (let ((a (aref index r c)) (b (aref index r (1+ c))) (cc (aref index (1+ r) (1+ c))) (dd (aref index (1+ r) c)))
          ;; rows run south, columns east: a b on top, d c below, so a d c and a c b go counterclockwise
          (when (and a dd cc) (push (list a dd (- cc)) faces) (push (list (1- a) (1- dd) (1- cc)) tris))
          (when (and a cc b) (push (list (- a) cc b) faces) (push (list (1- a) (1- cc) (1- b)) tris))
          ;; a square with one corner missing still gets its one triangle
          (when (and (not a) b cc dd) (push (list b dd cc) faces) (push (list (1- b) (1- dd) (1- cc)) tris))
          (when (and (not cc) a b dd) (push (list a dd b) faces) (push (list (1- a) (1- dd) (1- b)) tris)))))
    (values (nreverse faces) (nreverse tris))))

(defun dem-read (tf c0 r0 c1 r1 step)
  "Heights from columns C0..C1 and rows R0..R1 of TF, every STEP: a 2D array (NIL where none)."
  (let* ((nc (1+ (floor (- c1 c0) step))) (nr (1+ (floor (- r1 r0) step)))
         (a (make-array (list nr nc) :initial-element nil))
         (nd (tiff-nodata tf)))
    (dotimes (i nr a)
      (dotimes (j nc)
        (let ((v (tiff-value tf (+ c0 (* j step)) (+ r0 (* i step)))))
          (when (and v (not (sb-ext:float-nan-p v)) (> v -1d5) (< v 1d6) (not (and nd (= v nd))))
            (setf (aref a i j) v)))))))

(defcmd "DEMIMPORT" ("DEM") "Builds a ground model from a USGS elevation map (GeoTIFF DEM from The National Map)."
  (let ((f (ask-file nil "Open USGS elevation map (GeoTIFF)" nil "tif tiff")))
    (when (and f (plusp (length f)))
      (let ((tf (handler-case (open-tiff f)
                  (error (c) (out (format nil "~A~%" c)) nil))))
        (when tf
          (unwind-protect (dem-import tf f)
            (close-tiff tf)))))))

(defun dem-import (tf f)
  (multiple-value-bind (crs fx fy zunit) (tiff-geo tf)
    (when (null crs)
      (out (format nil "~A has no map position (GeoTIFF tags): it is a picture, not a height map.~%" (file-namestring f)))
      (return-from dem-import))
    (when (eq (first crs) :plane)
      (out (format nil "~A uses a map projection Fancy Pencil doesn't know; USGS maps in latitude and longitude or UTM work.~%" (file-namestring f)))
      (return-from dem-import))
    (let* ((w (tiff-width tf)) (h (tiff-height tf))
           (corners (loop for (c r) in (list (list 0 0) (list (1- w) 0) (list 0 (1- h)) (list (1- w) (1- h)))
                          collect (multiple-value-list (map->latlon crs (funcall fx c r) (funcall fy c r)))))
           (lat-lo (reduce #'min corners :key #'first)) (lat-hi (reduce #'max corners :key #'first))
           (lon-lo (reduce #'min corners :key #'second)) (lon-hi (reduce #'max corners :key #'second))
           ;; metres between neighbouring heights
           (spacing (multiple-value-bind (la lo) (map->latlon crs (funcall fx 0 0) (funcall fy 0 0))
                      (multiple-value-bind (la2 lo2) (map->latlon crs (funcall fx 1 0) (funcall fy 1 0))
                        (multiple-value-bind (x y) (latlon->local la2 lo2 la lo) (sqrt (+ (* x x) (* y y)))))))
           (map-w (* spacing w)))
      (out (format nil "~A: ~:D x ~:D heights about ~A apart (~A), from ~A to ~A.~%"
                   (file-namestring f) w h (fmt-metres spacing) (crs-label crs)
                   (fmt-latlon lat-lo lon-lo) (fmt-latlon lat-hi lon-hi)))
      (multiple-value-bind (olat olon) (geo-origin)
        (multiple-value-bind (a0 a1 o0 o1)
            (ask-map-area lat-lo lat-hi lon-lo lon-hi :default (if (< map-w 2500) "Whole" "Center")
                      :lat-c (and olat (<= lat-lo olat lat-hi) olat) :lon-c (and olon (<= lon-lo olon lon-hi) olon))
          (unless a0 (return-from dem-import))
          (let ((a0 (max a0 lat-lo)) (a1 (min a1 lat-hi)) (o0 (max o0 lon-lo)) (o1 (min o1 lon-hi)))
            (when (or (>= a0 a1) (>= o0 o1))
              (out (format nil "That area is not on this map.~%"))
              (return-from dem-import))
            ;; the pixels inside the area: found from their map positions
            (let* ((cols nil) (rows nil))
              (flet ((inside (c r) (multiple-value-bind (la lo) (map->latlon crs (funcall fx c r) (funcall fy c r))
                                     (and (<= a0 la a1) (<= o0 lo o1)))))
                ;; (the grid is near enough north-up that a pixel's row and column bounds come from the edges)
                (let ((cstep (max 1 (floor w 400))) (rstep (max 1 (floor h 400))))
                  (loop for r from 0 below h by rstep
                        do (loop for c from 0 below w by cstep
                                 when (inside c r) do (push c cols) (push r rows))))
                (when (null cols)
                  (out (format nil "That area is too small for this map; make it wider.~%"))
                  (return-from dem-import)))
              (let* ((pad (max (floor w 400) (floor h 400)))
                     (c0 (max 0 (- (reduce #'min cols) pad))) (c1 (min (1- w) (+ (reduce #'max cols) pad)))
                     (r0 (max 0 (- (reduce #'min rows) pad))) (r1 (min (1- h) (+ (reduce #'max rows) pad)))
                     (cells (max (- c1 c0) (- r1 r0)))
                     (auto (max 1 (ceiling cells 150))))
                (ask-drawing-units)
                (let* ((upm (units-per-metre))
                       (s (gstring (format nil "Specify spacing between heights or [Finest] <~A>: "
                                           (fmt-plain (* auto spacing upm))) t))
                       (step (cond ((zerop (length s)) auto)
                                   ((string-equal s "F") 1) ((string-equal s "Finest") 1)
                                   (t (let ((m (parse-length s))) (if m (max 1 (round m spacing)) auto)))))
                       (step (if (> (ceiling cells step) 500)
                                 (progn (out (format nil "That is more than 500 heights across; using every ~:R height.~%" (ceiling cells 500)))
                                        (ceiling cells 500))
                                 step))
                       (ex (let ((v (get-user-input :real :prompt "Specify vertical exaggeration <1>: ")))
                             (if (and (realp v) (plusp v)) (d v) 1d0))))
                  ;; a new drawing's 0,0: the area's south-west corner
                  (unless olat (setf olat a0 olon o0))
                  (let* ((grid (dem-read tf c0 r0 c1 r1 step))
                         (nr (array-dimension grid 0)) (nc (array-dimension grid 1))
                         (index (make-array (list nr nc) :initial-element nil))
                         (verts nil) (count 0))
                    ;; each height where it is, kept only inside the area
                    (dotimes (i nr)
                      (dotimes (j nc)
                        (let ((z (aref grid i j)) (c (+ c0 (* j step))) (r (+ r0 (* i step))))
                          (when z
                            (multiple-value-bind (la lo) (map->latlon crs (funcall fx c r) (funcall fy c r))
                              (when (and (<= (- a0 1d-9) la (+ a1 1d-9)) (<= (- o0 1d-9) lo (+ o1 1d-9)))
                                (multiple-value-bind (x y) (latlon->local la lo olat olon)
                                  (push (pt (* x upm) (* y upm) (* z zunit upm ex)) verts)
                                  (setf (aref index i j) (incf count)))))))))
                    (setf verts (nreverse verts))
                    (if (< count 4)
                        (out (format nil "Too few heights in that area (~D).~%" count))
                        (multiple-value-bind (faces tris) (grid-mesh-faces index nr nc)
                          (let* ((zs (mapcar #'pz verts)) (lo (reduce #'min zs)) (hi (reduce #'max zs))
                                 (layer "Topo_Surface_Visualization"))
                            (out (format nil "~:D heights (~D x ~D), ~A apart; ground from ~A to ~A ~A~:[~; (exaggerated ~Ax)~].~%"
                                         count nc nr (fmt-plain (* step spacing upm)) (fmt-plain lo) (fmt-plain hi) (geo-units-name)
                                         (/= ex 1) (fmt-plain ex)))
                            (let* ((dflt (default-base lo hi))
                                   (b (get-user-input :real :keywords '("Surface")
                                                            :prompt (format nil "Specify base elevation for the solid or [Surface only] <~A>: " (fmt-plain dflt))))
                                   (base (cond ((equal b "Surface") nil) ((realp b) (d b)) (t dflt))))
                              (unless (geo-origin)
                                (add-geo-origin olat olon (max 1d-3 (* 0.02 (- (reduce #'max verts :key #'px) (reduce #'min verts :key #'px))))
                                                (format nil "USGS ~A" (file-namestring f))))
                              (ensure-layer layer)
                              (add (append (make-mesh-data verts faces) (list (cons 8 layer) (cons 62 256))))
                              (cond ((null base) (out (format nil "Surface made (layer ~A).~%" layer)))
                                    ((> base lo) (out (format nil "Surface made; no solid: the base (~A) must be below the lowest point (~A).~%"
                                                              (fmt-plain base) (fmt-plain lo))))
                                    (t (let ((solid (topo-solid (coerce verts 'vector) tris base faces)))
                                         (add solid)
                                         (out (format nil "Surface made (layer ~A), and a 3D solid down to ~A, volume ~A.~%"
                                                      layer (fmt-plain base) (fmt-plain (abs (mesh-signed-volume solid))))))))
                              (when (gui-p) (zoom-extents) (gui-regen))
                              (out (format nil "0,0 is ~A. For a 3D look: View > SW Isometric, and the Shaded visual style.~%"
                                           (fmt-latlon olat olon)))))))))))))))))

;;; ---------------------------------------------------------------- shapefiles

(defun read-file-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))

(defun shp-records (v)
  "The shapefile records' line parts: a list, one entry per record, of its parts."
  (let ((type (octets-uint v 32 4 t)) (pos 100) (out nil) (n (length v)))
    (unless (member type '(3 13 23 5 15 25))
      (error "This shapefile holds ~A, not lines." (case type ((1 8 11 21 18 28) "points") (t (format nil "shapes of type ~D" type)))))
    (loop while (<= (+ pos 8) n)
          do (let* ((len (* 2 (octets-uint v (+ pos 4) 4 nil))) (c (+ pos 8)) (st (octets-uint v c 4 t)))
               (push (when (member st '(3 13 23 5 15 25))
                       (let* ((np (octets-uint v (+ c 36) 4 t)) (npts (octets-uint v (+ c 40) 4 t))
                              (parts (loop for k below np collect (octets-uint v (+ c 44 (* 4 k)) 4 t)))
                              (p0 (+ c 44 (* 4 np)))
                              (z0 (and (member st '(13 15)) (+ p0 (* 16 npts) 16))))
                         (loop for (a b) on (append parts (list npts)) while b
                               collect (loop for k from a below b
                                             collect (list (bits-float (octets-uint v (+ p0 (* 16 k)) 8 t) 8)
                                                           (bits-float (octets-uint v (+ p0 (* 16 k) 8) 8 t) 8)
                                                           (and z0 (bits-float (octets-uint v (+ z0 (* 8 k)) 8 t) 8)))))))
                     out)
               (setf pos (+ c len))))
    (nreverse out)))

(defun dbf-table (v)
  "The fields and rows of dBASE octets V: (values field-names rows), each row a list of strings."
  (let* ((nrec (octets-uint v 4 4 t)) (hlen (octets-uint v 8 2 t)) (rlen (octets-uint v 10 2 t))
         (fields (loop for p from 32 by 32
                       while (and (< p (- hlen 1)) (/= (aref v p) 13))
                       collect (list (string-right-trim '(#\Nul #\Space) (map 'string #'code-char (subseq v p (+ p 11))))
                                     (aref v (+ p 16))))))
    (values (mapcar #'first fields)
            (loop for r below nrec
                  for p = (+ hlen (* r rlen))
                  while (<= (+ p rlen) (length v))
                  collect (let ((q (1+ p)))
                            (loop for (nil len) in fields
                                  collect (prog1 (string-trim " " (map 'string #'code-char (subseq v q (+ q len))))
                                            (incf q len))))))))

(defun contour-source (path)
  "The .shp, .dbf and .prj octets for PATH (a .shp, or a .zip holding one): (values shp dbf prj name)."
  (if (string-equal (pathname-type path) "zip")
      (let* ((z (read-file-octets path))
             (shps (remove-if-not (lambda (e) (let ((n (first e))) (and (> (length n) 4) (string-equal ".shp" (subseq n (- (length n) 4))))))
                                  (zip-entries z)))
             (shp (or (find-if (lambda (e) (search "CONTOUR" (string-upcase (first e)))) shps) (first shps))))
        (unless shp (error "~A has no shapefile (.shp) in it." (file-namestring path)))
        (let ((base (subseq (first shp) 0 (- (length (first shp)) 4))))
          (flet ((part (ext) (let ((e (find (concatenate 'string base ext) (zip-entries z) :key #'first :test #'string-equal)))
                               (and e (zip-entry-bytes z e)))))
            (values (zip-entry-bytes z shp) (part ".dbf") (part ".prj") (file-namestring (first shp))))))
      (flet ((side (ext) (let ((p (make-pathname :type ext :defaults path)))
                           (or (and (probe-file p) (read-file-octets p))
                               (let ((p2 (make-pathname :type (string-upcase ext) :defaults path)))
                                 (and (probe-file p2) (read-file-octets p2)))))))
        (values (read-file-octets path) (side "dbf") (side "prj") (file-namestring path)))))

(defun prj-crs (prj bbox)
  "The coordinate system a .prj describes (or guessed from the coordinates' range)."
  (let ((s (and prj (string-upcase (map 'string #'code-char prj)))))
    (cond ((and s (search "UTM" s) (search "ZONE" s))
           (let* ((p (+ (search "ZONE" s) 4)) (rest (string-left-trim " _" (subseq s p)))
                  (j (or (position-if-not #'digit-char-p rest) (length rest))))
             (list :utm (parse-integer rest :end j) (and (< j (length rest)) (char= (char rest j) #\S)))))
          ((and s (search "PROJCS" s)) (list :plane 1d0))
          ((and s (search "GEOGCS" s)) (list :geo))
          ((and (<= -180 (first bbox) (third bbox) 360) (<= -90 (second bbox) (fourth bbox) 90)) (list :geo))
          (t (list :plane 1d0)))))

(defun clip-to-rect (pts x0 y0 x1 y1)
  "The pieces of polyline PTS (local points) inside the rectangle: a list of point lists."
  (let ((runs nil) (cur nil))
    (flet ((in (p) (and (<= x0 (px p) x1) (<= y0 (py p) y1)))
           (cut (p q)                    ; the stretch of p->q inside, by Liang-Barsky: (values t0 t1) or nil
             (let ((t0 0d0) (t1 1d0) (dx (- (px q) (px p))) (dy (- (py q) (py p))))
               (loop for (pp qq) in (list (list (- dx) (- (px p) x0)) (list dx (- x1 (px p)))
                                          (list (- dy) (- (py p) y0)) (list dy (- y1 (py p))))
                     do (if (zerop pp)
                            (when (minusp qq) (return-from cut nil))
                            (let ((r (/ qq pp)))
                              (if (minusp pp) (setf t0 (max t0 r)) (setf t1 (min t1 r))))))
               (and (<= t0 t1) (values t0 t1))))
           (at (p q u) (pt (+ (px p) (* u (- (px q) (px p)))) (+ (py p) (* u (- (py q) (py p)))) (pz p))))
      (when (and pts (in (first pts))) (push (first pts) cur))
      (loop for (p q) on pts while q
            do (multiple-value-bind (t0 t1) (cut p q)
                 (cond ((null t0) (when cur (push (nreverse cur) runs) (setf cur nil)))
                       (t (unless cur (push (at p q t0) cur))
                          (push (at p q t1) cur)
                          (when (< t1 1) (push (nreverse cur) runs) (setf cur nil))))))
      (when cur (push (nreverse cur) runs))
      (remove-if (lambda (r) (< (length r) 2)) (nreverse runs)))))

(defun thin-run (pts tol)
  "PTS without points closer than TOL to the last one kept (the ends always kept)."
  (if (< (length pts) 3) pts
      (let ((out (list (first pts))))
        (dolist (p (butlast (rest pts)))
          (when (>= (dist2d p (first out)) tol) (push p out)))
        (push (car (last pts)) out)
        (nreverse out))))

(defun contour-interval (zs)
  "The commonest gap between neighbouring contour heights."
  (let* ((u (sort (remove-duplicates (mapcar (lambda (z) (/ (round (* z 1000)) 1000d0)) zs)) #'<))
         (gaps (loop for (a b) on u while b collect (/ (round (* 1000 (- b a))) 1000d0)))
         (h (make-hash-table)))
    (dolist (g gaps) (incf (gethash g h 0)))
    (let ((best nil) (bn 0))
      (maphash (lambda (g n) (when (> n bn) (setf best g bn n))) h)
      best)))

(defcmd "CONTOURIMPORT" ("CONTOURS") "Reads contour lines from a USGS contour shapefile (or the .zip as downloaded) as polylines at their heights."
  (let ((f (ask-file nil "Open contour lines (shapefile .shp or .zip)" nil "shp zip")))
    (when (and f (plusp (length f)))
      (handler-case (contour-import f)
        (error (c) (out (format nil "~A~%" c)))))))

(defun contour-import (f)
  (multiple-value-bind (shp dbf prj name) (contour-source f)
    (let* ((records (shp-records shp))
           (bbox (list (bits-float (octets-uint shp 36 8 t) 8) (bits-float (octets-uint shp 44 8 t) 8)
                       (bits-float (octets-uint shp 52 8 t) 8) (bits-float (octets-uint shp 60 8 t) 8)))
           (crs (prj-crs prj bbox)))
      (multiple-value-bind (fields rows) (if dbf (dbf-table dbf) (values nil nil))
        (let* ((ufields (mapcar #'string-upcase fields))
               (zf (or (position "CONTOURELE" ufields :test #'string=)
                       (position-if (lambda (n) (search "ELEV" n)) ufields)
                       (position-if (lambda (n) (member n '("CONTOUR" "Z" "HEIGHT" "ALT" "ALTITUDE") :test #'string=)) ufields)))
               (uf (position-if (lambda (n) (search "CONTOURUN" n)) ufields))
               (heights (loop for i below (length records)
                              collect (let ((row (nth i rows)))
                                        (or (and zf row (ignore-errors (let ((*read-default-float-format* 'double-float))
                                                                        (d (read-from-string (nth zf row))))))
                                            (let ((p (first (first (nth i records))))) (and p (third p))))))))
          (when (eq (first crs) :plane)
            (out (format nil "~A uses a map projection Fancy Pencil doesn't know; USGS contours (latitude and longitude, or UTM) work.~%" name))
            (return-from contour-import))
          (when (notany #'identity heights)
            (out (format nil "~A has no heights for its lines (no elevation field).~%" name))
            (return-from contour-import))
          (out (format nil "~A: ~:D contour line~:P (~A~@[, heights from field ~A~]).~%"
                       name (count-if #'identity records) (crs-label crs) (and zf (nth zf fields))))
          (let* ((zunit (cond ((and uf rows) (if (equal (nth uf (first rows)) "2") 1d0 0.3048d0))
                              ((not zf) 1d0)
                              (t (if (equal (gkw "Specify the contours' height units [Feet/Meters] <Feet>: " '("Feet" "Meters") "Feet") "Meters")
                                     1d0 0.3048d0))))
                 (corners (list (multiple-value-list (map->latlon crs (first bbox) (second bbox)))
                                (multiple-value-list (map->latlon crs (third bbox) (fourth bbox)))
                                (multiple-value-list (map->latlon crs (first bbox) (fourth bbox)))
                                (multiple-value-list (map->latlon crs (third bbox) (second bbox)))))
                 (lat-lo (reduce #'min corners :key #'first)) (lat-hi (reduce #'max corners :key #'first))
                 (lon-lo (reduce #'min corners :key #'second)) (lon-hi (reduce #'max corners :key #'second))
                 (map-w (multiple-value-bind (x) (latlon->local lat-lo lon-hi lat-lo lon-lo) x)))
            (out (format nil "They cover ~A to ~A (about ~A across).~%" (fmt-latlon lat-lo lon-lo) (fmt-latlon lat-hi lon-hi) (fmt-metres map-w)))
            (multiple-value-bind (olat olon) (geo-origin)
              (multiple-value-bind (a0 a1 o0 o1)
                  (ask-map-area lat-lo lat-hi lon-lo lon-hi :default (if (< map-w 3000) "Whole" "Center")
                            :lat-c (and olat (<= lat-lo olat lat-hi) olat) :lon-c (and olon (<= lon-lo olon lon-hi) olon))
                (unless a0 (return-from contour-import))
                (ask-drawing-units)
                (let* ((upm (units-per-metre))
                       (ex (let ((v (get-user-input :real :prompt "Specify vertical exaggeration <1>: ")))
                             (if (and (realp v) (plusp v)) (d v) 1d0)))
                       (new-origin (not olat)))
                  (when new-origin (setf olat a0 olon o0))
                  (multiple-value-bind (x0 y0) (latlon->local a0 o0 olat olon)
                    (multiple-value-bind (x1 y1) (latlon->local a1 o1 olat olon)
                      (let* ((x0 (* x0 upm)) (y0 (* y0 upm)) (x1 (* x1 upm)) (y1 (* y1 upm))
                             (tol (/ (max (- x1 x0) (- y1 y0)) 3000))
                             (zs (remove nil heights))
                             (interval (contour-interval zs))
                             (made 0) (zmin nil) (zmax nil))
                        (ensure-layer "Contours")
                        (loop for parts in records
                              for z in heights
                              when (and parts z)
                                do (let* ((zd (* z zunit upm ex))
                                          (index (and interval (plusp interval)
                                                      (< (abs (- (/ z (* 5 interval)) (round z (* 5 interval)))) 1d-6))))
                                     (dolist (part parts)
                                       (let ((local (mapcar (lambda (p)
                                                              (multiple-value-bind (la lo) (map->latlon crs (first p) (second p))
                                                                (multiple-value-bind (x y) (latlon->local la lo olat olon)
                                                                  (pt (* x upm) (* y upm) zd))))
                                                            part)))
                                         (dolist (run (clip-to-rect local x0 y0 x1 y1))
                                           (let* ((closed (and (= (length run) (length local))
                                                               (< (dist2d (first run) (car (last run))) 1d-9)))
                                                  (run (thin-run (if closed (butlast run) run) tol)))
                                             (when (>= (length run) 2)
                                               (add (append (make-lwpoly-data run closed :layer "Contours")
                                                            (list (cons 38 zd))
                                                            (when index (list (cons 370 35) (cons 62 30)))))
                                               (incf made)
                                               (setf zmin (if zmin (min zmin zd) zd) zmax (if zmax (max zmax zd) zd)))))))))
                        (if (zerop made)
                            (out (format nil "No contour lines in that area.~%"))
                            (progn
                              (when new-origin
                                (add-geo-origin olat olon (* 0.02 (- x1 x0)) (format nil "USGS ~A" name)))
                              (out (format nil "~:D contour line~:P on layer Contours, ~A to ~A ~A~@[ (every ~A)~]~:[~;, index lines orange~].~%"
                                           made (fmt-plain zmin) (fmt-plain zmax) (geo-units-name)
                                           (and interval (format nil "~A ~A" (trim-number interval) (if (= zunit 1d0) "m" "ft")))
                                           interval))
                              (out (format nil "TOPOMODEL, picking a contour line, makes a ground model from them.~%"))
                              (when (gui-p) (zoom-extents) (gui-regen))))))))))))))))
