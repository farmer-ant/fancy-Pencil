;; DEMIMPORT and CONTOURIMPORT: USGS height maps (GeoTIFF) and contour shapefiles.
;; The test maps (tests/data/usgs) are made like USGS's: a hill at 37.05N 122.05W.
;; Run: sbcl --non-interactive --load load.lisp --load tests/test-usgs.lisp
(in-package :fancy-pencil)
(defvar *said* nil)
(setf *ui* :tty *output-fn* (lambda (s) (push s *said*) s))
(load-aliases) (current-doc)
(defvar *pass* 0) (defvar *fail* 0)
(defmacro chk (name form expect)
  `(let ((v (handler-case ,form (error (c) (format nil "ERROR ~A" c)))))
     (if (equalp v ,expect) (incf *pass*) (incf *fail*))
     (format t "~:[FAIL~;ok  ~] ~A: ~S~%" (equalp v ,expect) ,name v)))
(defun al (s) (alisp-eval-string s))
(defun said (part) (and (search part (format nil "~{~A~}" (reverse *said*))) t))
(defun meshes () (remove "POLYFACE" (live-entities) :key #'ent-type :test-not #'equal))
(defun data (f) (namestring (merge-pathnames (concatenate 'string "tests/data/usgs/" f))))
(defun hill (lat lon) (+ 120 (* 80 (exp (- (+ (expt (/ (- lat 37.05) 0.02) 2) (expt (/ (+ lon 122.05) 0.025) 2))))) (* 300 (- lat 37.0))))
(defun fresh () (al "(command \"erase\" \"all\" \"\")") (setf *said* nil))

;; reading the numbers
(chk "lat, lon" (multiple-value-list (parse-latlon "37.75, -122.45")) '(37.75d0 -122.45d0))
(chk "N and W" (multiple-value-list (parse-latlon "37.75N 122.45W")) '(37.75d0 -122.45d0))
(chk "degrees minutes seconds" (multiple-value-list (parse-latlon "37 45 0 N, 122 27 0 W")) '(37.75d0 -122.45d0))
(chk "longitude first, with letters" (multiple-value-list (parse-latlon "122.45W 37.75N")) '(37.75d0 -122.45d0))
(chk "2 km" (parse-length "2 km") 2000d0)
(chk "1 mi" (parse-length "1mi") 1609.344d0)
(chk "3000 ft" (round (parse-length "3000 ft")) 914)
(chk "UTM back to latitude/longitude (a survey mark)"
     (multiple-value-bind (la lo) (utm->latlon 550999.5665d0 4179999.4872d0 10) (list (round la 1d-6) (round lo 1d-6)))
     '(37765900 -122420960))
(chk "a thousandth of a degree east at 37.5N: 88.425 m" (round (* 1000 (latlon->local 37.5d0 -121.999d0 37.5d0 -122d0))) 88425)

;; the same map, compressed three ways, reads the same
(defun corner (f) (let ((tf (open-tiff (data f)))) (unwind-protect (list (tiff-value tf 0 0) (tiff-value tf 50 20) (tiff-value tf 199 199)) (close-tiff tf))))
(chk "LZW, floating point predictor, tiled = plain" (corner "geo_lzw_pred3_tiled.tif") (corner "geo_plain.tif"))
(chk "Deflate, strips, big-endian = plain" (corner "geo_deflate_strips_be.tif") (corner "geo_plain.tif"))
(chk "the heights are the hill's" (round (* 100 (first (corner "geo_plain.tif")))) (round (* 100 (hill (- 37.1 0.00025) (+ -122.1 0.00025)))))
(chk "integers, LZW, horizontal predictor" (let ((tf (open-tiff (data "utm_int16_lzw_pred2.tif"))))
                                           (unwind-protect (list (tiff-value tf 0 0) (tiff-value tf 59 39) (tiff-value tf 40 20)) (close-tiff tf)))
     '(100d0 107d0 104d0))

;; DEMIMPORT: the whole map, every height, in metres
(setf (sysvar "INSUNITS") 0)
(al (format nil "(command \"demimport\" ~S \"w\" \"m\" \"f\" 1 \"\")" (data "geo_lzw_pred3_tiled.tif")))
(chk "map described" (said "200 x 200 heights") t)
(chk "drawing set to metres" (sysvar "INSUNITS") 6)
(chk "surface and solid" (length (meshes)) 2)
(chk "origin note: the south-west height" (multiple-value-list (geo-origin)) '(37.00025d0 -122.09975d0))
(let* ((surf (find "Topo_Surface_Visualization" (meshes) :key (lambda (e) (dxf 8 (ent-data e))) :test #'equal))
       (vs (mesh-verts (ent-data surf))))
  (chk "every height but the missing one" (length vs) (1- (* 200 200)))
  (chk "each height where it belongs"
       (loop for p in vs by (lambda (l) (nthcdr 997 l))
             always (multiple-value-bind (la lo) (local->latlon (px p) (py p) 37.00025d0 -122.09975d0)
                      (< (abs (- (pz p) (hill la lo))) 0.05)))
       t)
  (chk "about 8.9 km wide (0.1 degree of longitude)" (round (- (reduce #'max vs :key #'px) (reduce #'min vs :key #'px)) 100) 89)
  (chk "the hill top near 37.05N 122.05W"
       (let ((top (reduce (lambda (a b) (if (> (pz a) (pz b)) a b)) vs)))
         (multiple-value-bind (la lo) (local->latlon (px top) (py top) 37.00025d0 -122.09975d0) (list (round la 0.001) (round lo 0.001))))
       '(37051 -122050)))
(let ((solid (find-if-not (lambda (e) (equal (dxf 8 (ent-data e)) "Topo_Surface_Visualization")) (meshes))))
  (chk "solid closed: every edge twice"
       (let ((h (make-hash-table :test #'equal)))
         (dolist (f (mesh-faces (ent-data solid)))
           (let ((f (mapcar #'abs f)))
             (loop for (a b) on (append f (list (first f))) while b do (incf (gethash (cons (min a b) (max a b)) h 0)))))
         (loop for v being the hash-values of h always (= v 2)))
       t)
  (chk "solid volume positive" (plusp (mesh-signed-volume (ent-data solid))) t))

;; a 1 km square round the hill, in feet, picked by its centre; it lines up with the first
(fresh) (setf (sysvar "INSUNITS") 0)
(al (format nil "(command \"demimport\" ~S \"c\" \"37.05N 122.05W\" \"1 km\" \"f\" \"\" 2 \"s\")" (data "geo_deflate_strips_be.tif")))
(chk "drawing set to feet" (sysvar "INSUNITS") 2)
(chk "surface only" (length (meshes)) 1)
(let ((vs (mesh-verts (ent-data (first (meshes))))))
  (chk "a kilometre across, in feet" (< 2900 (- (reduce #'max vs :key #'px) (reduce #'min vs :key #'px)) 3281) t)
  (chk "the top, twice as high, in feet" (round (* 2 (/ (hill 37.05 -122.05) 0.3048)) 10) (round (reduce #'max vs :key #'pz) 10)))
(chk "exaggeration said" (said "exaggerated 2") t)
(chk "origin is the area's corner" (let ((la (geo-origin))) (< 37.04 la 37.05)) t)

;; spacing typed: every 10th height
(fresh) (setf (sysvar "INSUNITS") 6)
(al "(command \"erase\" (ssget \"x\" '((8 . \"GEO_ORIGIN\"))) \"\")")
(al (format nil "(command \"demimport\" ~S \"w\" \"500 m\" 1 \"s\")" (data "geo_plain.tif")))
(chk "every 11th height (500 m): 19 x 19" (length (mesh-verts (ent-data (first (meshes))))) 361)

;; UTM 1-metre-style map
(fresh) (al "(command \"erase\" (ssget \"x\" '((8 . \"GEO_ORIGIN\"))) \"\")")
(al (format nil "(command \"demimport\" ~S \"w\" \"\" 1 \"\")" (data "utm_int16_lzw_pred2.tif")))
(chk "UTM map described" (said "UTM zone 10N") t)
(let ((vs (mesh-verts (ent-data (find "Topo_Surface_Visualization" (meshes) :key (lambda (e) (dxf 8 (ent-data e))) :test #'equal)))))
  (chk "600 m across" (round (- (reduce #'max vs :key #'px) (reduce #'min vs :key #'px)) 10) 59)
  (chk "heights 100 to 107" (list (round (reduce #'min vs :key #'pz)) (round (reduce #'max vs :key #'pz))) '(100 107)))

;; not a map
(fresh)
(al (format nil "(command \"demimport\" ~S)" (namestring (merge-pathnames "tests/data/more-objects.dxf"))))
(chk "not a TIFF: said so" (said "not a TIFF") t)

;; CONTOURIMPORT from the .zip as downloaded, lined up with a height map's origin
(fresh) (al "(command \"erase\" (ssget \"x\" '((8 . \"GEO_ORIGIN\"))) \"\")")
(setf (sysvar "INSUNITS") 6)
(al (format nil "(command \"contourimport\" ~S \"w\" \"\")" (data "ELEV_Contour_test.zip")))
(defun contours () (remove-if-not (lambda (e) (and (equal (ent-type e) "LWPOLYLINE") (equal (dxf 8 (ent-data e)) "Contours"))) (live-entities)))
(chk "field found" (said "heights from field CONTOURELE") t)
(chk "11 lines" (length (contours)) 11)
(chk "every 20 ft" (said "every 20 ft") t)
(chk "rings closed" (count-if (lambda (e) (lwpoly-closed-p (ent-data e))) (contours)) 8)
(chk "heights in metres" (sort (mapcar (lambda (e) (round (dxf 38 (ent-data e)) 0.01)) (contours)) #'<)
     (sort (mapcar (lambda (ft) (round (* ft 0.3048) 0.01)) '(680 660 640 620 600 580 560 540 300 320 340)) #'<))
(chk "index lines (every 100 ft: 600 and 300) orange" (count-if (lambda (e) (eql (dxf 62 (ent-data e)) 30)) (contours)) 2)
(chk "a ring centred on the hill top"
     (let* ((e (find-if (lambda (e) (lwpoly-closed-p (ent-data e))) (contours)))
            (vs (mapcar #'first (lwpoly-vertices (ent-data e))))
            (cx (/ (reduce #'+ vs :key #'px) (length vs))) (cy (/ (reduce #'+ vs :key #'py) (length vs))))
       (multiple-value-bind (olat olon) (geo-origin)
         (multiple-value-bind (la lo) (local->latlon cx cy olat olon) (list (round la 0.001) (round lo 0.001)))))
     '(37050 -122050))
(chk "contours shown at their height in 3D" (let ((e (first (contours)))) (= (view-object-height (ent-data e)) (dxf 38 (ent-data e)))) t)

;; TOPOMODEL from the contour lines
(setf *said* nil)
(al "(command \"topomodel\" (cdr (assoc -1 (entget (ssname (ssget \"x\" '((8 . \"Contours\"))) 0)))) \"s\")")
(chk "TOPOMODEL from contours" (said "11 contour lines on layer \"Contours\"") t)
(chk "  ...a surface" (said "Surface made") t)

;; the loose .shp beside its .dbf and .prj, a part of it
(fresh) (al "(command \"erase\" (ssget \"x\" '((8 . \"GEO_ORIGIN\"))) \"\")")
(al (format nil "(command \"contourimport\" ~S \"c\" \"37.05, -122.05\" \"1600 m\" \"\")" (data "Elev_Contour.shp")))
(chk "two whole rings, and four pieces of the third" (length (contours)) 6)
(chk "cut at the area's edge (open pieces)" (plusp (count-if-not (lambda (e) (lwpoly-closed-p (ent-data e))) (contours))) t)
(format t "~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
