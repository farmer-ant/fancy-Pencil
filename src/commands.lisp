;;;; commands.lisp — command registry and drawing commands.

(in-package :lispdraft)

(defvar *commands* (make-hash-table :test #'equalp))
(defvar *aliases* (make-hash-table :test #'equalp))
(defvar *command-help* (make-hash-table :test #'equalp))

(defmacro defcmd (name (&rest aliases) doc &body body)
  `(progn
     (setf (gethash ,name *commands*) (lambda () (block command-body ,@body))
           (gethash ,name *command-help*) ,doc)
     (dolist (a ',aliases) (setf (gethash a *aliases*) ,name))
     ,name))

(defmacro return-from-cmd () `(return-from command-body nil))

(defun strip-command-prefix (s)
  (string-left-trim "_.'+" (string-trim " " s)))

(defun canonical-command-name (name)
  (let* ((n (strip-command-prefix name)))
    (cond ((gethash n *commands*) (string-upcase n))
          ((gethash n *aliases*) (gethash n *aliases*))
          ((and (> (length n) 1) (char= (char n 0) #\-))
           (let ((m (subseq n 1)))
             (cond ((gethash (concatenate 'string "-" m) *commands*) (string-upcase n))
                   ((gethash m *commands*) (string-upcase m))
                   ((gethash m *aliases*) (gethash m *aliases*))))))))

(defun find-command-function (name)
  (let ((c (canonical-command-name name)))
    (and c (gethash c *commands*))))

;;; ---------- helpers ----------

(defun gpoint (prompt &rest keys) (apply #'get-user-input :point :prompt prompt keys))
(defun gdist (prompt &rest keys) (apply #'get-user-input :distance :prompt prompt keys))
(defun gangle (prompt &rest keys) (apply #'get-user-input :angle :prompt prompt keys))
(defun gkw (prompt keywords &optional default)
  (let ((r (get-user-input :keyword :prompt prompt :keywords keywords)))
    (or r default)))
(defun greal (prompt &rest keys) (apply #'get-user-input :real :prompt prompt keys))
(defun gint (prompt &rest keys) (apply #'get-user-input :int :prompt prompt keys))
(defun gstring (prompt &optional spaces) (or (get-user-input :string :prompt prompt :spaces spaces) ""))

(defun fmt-len (x) (rtos x 2 (or (sysvar "LUPREC") 4)))
(defun fmt-ang (a) (angtos a 0 (max 0 (or (sysvar "AUPREC") 0))))

(defun add (data) (add-entity data))
(defun line-data (a b) (list (cons 0 "LINE") (cons 10 (pt3 a)) (cons 11 (pt3 b))))
(defun circle-data (c r) (list (cons 0 "CIRCLE") (cons 10 (pt3 c)) (cons 40 (d r))))
(defun arc-data (c r a1 a2) (list (cons 0 "ARC") (cons 10 (pt3 c)) (cons 40 (d r)) (cons 50 (norm-ang a1)) (cons 51 (norm-ang a2))))
(defun common-props (data)
  (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370 48)))) data))
(defun with-props (new old) (append new (common-props old)))

(defun circumcenter (a b c)
  (let* ((ax (px a)) (ay (py a)) (bx (px b)) (by (py b)) (cx (px c)) (cy (py c))
         (dd (* 2 (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by))))))
    (when (> (abs dd) 1d-12)
      (let ((ux (/ (+ (* (+ (* ax ax) (* ay ay)) (- by cy)) (* (+ (* bx bx) (* by by)) (- cy ay)) (* (+ (* cx cx) (* cy cy)) (- ay by))) dd))
            (uy (/ (+ (* (+ (* ax ax) (* ay ay)) (- cx bx)) (* (+ (* bx bx) (* by by)) (- ax cx)) (* (+ (* cx cx) (* cy cy)) (- bx ax))) dd)))
        (pt ux uy (pz a))))))

(defun cross2 (o a b) (- (* (- (px a) (px o)) (- (py b) (py o))) (* (- (py a) (py o)) (- (px b) (px o)))))

(defun arc-3p (p1 p2 p3)
  "ARC data through three points, or NIL if collinear."
  (let ((c (circumcenter p1 p2 p3)))
    (when c
      (let ((r (dist2d c p1)))
        (if (> (cross2 p1 p2 p3) 0)
            (arc-data c r (ang c p1) (ang c p3))
            (arc-data c r (ang c p3) (ang c p1)))))))

;;; ---------- LINE ----------

(defcmd "LINE" ("L") "Creates straight line segments."
  (let ((p0 (gpoint "Specify first point: ")) (pts nil) (segs nil))
    (when p0
      (push p0 pts)
      (loop
        (let* ((n (length segs))
               (r (gpoint (if (>= n 2) "Specify next point or [Close/Undo]: " "Specify next point or [Undo]: ")
                          :base (first pts) :keywords (if (>= n 2) '("Close" "Undo") '("Undo")))))
          (cond ((null r) (return))
                ((equal r "Undo")
                 (if segs
                     (progn (delete-entity (pop segs)) (pop pts) (setf (sysvar "LASTPOINT") (first pts)))
                     (out (format nil "All segments already undone.~%"))))
                ((equal r "Close") (push (add (line-data (first pts) p0)) segs) (return))
                (t (push (add (line-data (first pts) r)) segs) (push r pts))))))))

;;; ---------- PLINE ----------

(defun tangent-bulge (from dir to)
  "Bulge of an arc leaving FROM with direction DIR and ending at TO."
  (let* ((chord-ang (ang from to))
         (delta (- chord-ang dir)))
    (setf delta (- (mod (+ delta +pi+) +2pi+) +pi+))
    (tan (/ (* 2 delta) 4))))

(defun pline-end-direction (verts)
  "Direction of travel at the end of VERTS (list of (pt bulge), oldest first)."
  (let* ((n (length verts)))
    (if (< n 2) 0d0
        (destructuring-bind (p b) (nth (- n 2) verts)
          (let ((q (first (nth (1- n) verts))))
            (if (zerop b) (ang p q)
                (+ (ang p q) (* 2 (atan b)))))))))

(defcmd "PLINE" ("PL") "Creates a 2D polyline of line and arc segments."
  (let ((start (gpoint "Specify start point: ")) (verts nil) (arc-mode nil) (closed nil)
        (width (d (or (sysvar "PLINEWID") 0d0))))
    (when start
      (out (format nil "Current line-width is ~A~%" (fmt-len width)))
      (setf verts (list (list start 0d0)))
      (labels ((last-pt () (first (car (last verts))))
               (preview (cur)
                 (let ((vs (append (mapcar #'copy-list verts)
                                   (list (list cur 0d0)))))
                   (when arc-mode
                     (setf (second (nth (- (length vs) 2) vs))
                           (if (> (length verts) 1)
                               (tangent-bulge (last-pt) (pline-end-direction verts) cur)
                               (tangent-bulge (last-pt) 0d0 cur))))
                   (mapcar (lambda (pc) pc) (lwpoly-pieces (make-lwpoly-data vs nil))))))
        (loop
          (let* ((kws (if arc-mode '("Line" "Close" "Width" "Undo") '("Arc" "Close" "Width" "Undo")))
                 (r (gpoint (if arc-mode
                                "Specify endpoint of arc or [Line/Close/Width/Undo]: "
                                "Specify next point or [Arc/Close/Width/Undo]: ")
                            :base (last-pt) :keywords kws :drag #'preview)))
            (cond ((null r) (return))
                  ((equal r "Arc") (setf arc-mode t))
                  ((equal r "Line") (setf arc-mode nil))
                  ((equal r "Width")
                   (let ((w (gdist (format nil "Specify width <~A>: " (fmt-len width)) :base (last-pt))))
                     (when w (setf width w (sysvar "PLINEWID") w))))
                  ((equal r "Undo") (when (> (length verts) 1) (setf verts (butlast verts))
                                          (setf (second (car (last verts))) 0d0)))
                  ((equal r "Close")
                   (when (> (length verts) 1)
                     (when arc-mode
                       (setf (second (car (last verts))) (tangent-bulge (last-pt) (pline-end-direction verts) start)))
                     (setf closed t))
                   (return))
                  (t (when arc-mode
                       (setf (second (car (last verts)))
                             (tangent-bulge (last-pt) (pline-end-direction verts) r)))
                     (setf verts (append verts (list (list r 0d0))))))))
        (when (> (length verts) 1)
          (add (make-lwpoly-data verts closed :width width)))))))

;;; ---------- RECTANG / POLYGON ----------

(defcmd "RECTANG" ("REC" "RECTANGLE") "Creates a rectangular polyline."
  (let ((p1 (gpoint "Specify first corner point: ")))
    (when p1
      (let* ((rect (lambda (p2) (list (pt3 p1) (pt (px p2) (py p1)) (pt3 p2) (pt (px p1) (py p2)))))
             (r (gpoint "Specify other corner point or [Dimensions]: " :keywords '("Dimensions")
                        :drag (lambda (cur) (lwpoly-pieces (make-lwpoly-data (funcall rect cur) t))))))
        (when (equal r "Dimensions")
          (let ((w (gdist "Specify length for rectangles: " :flags 3))
                (h (gdist "Specify width for rectangles: " :flags 3)))
            (let ((q (gpoint "Specify other corner point: " :base p1)))
              (setf r (pt (+ (px p1) (if (and q (< (px q) (px p1))) (- w) w))
                          (+ (py p1) (if (and q (< (py q) (py p1))) (- h) h)))))))
        (when (pointp r) (add (make-lwpoly-data (funcall rect r) t)))))))

(defcmd "POLYGON" ("POL") "Creates a regular polygon."
  (let ((n (or (gint "Enter number of sides <4>: " :flags 6) 4)))
    (when (< n 3) (aerr "Requires an integer between 3 and 1024."))
    (let ((c (gpoint "Specify center of polygon or [Edge]: " :keywords '("Edge"))))
      (if (equal c "Edge")
          (let* ((a (gpoint "Specify first endpoint of edge: "))
                 (b (gpoint "Specify second endpoint of edge: " :base a))
                 (verts (loop with ang0 = (ang a b) with len = (dist2d a b)
                              for i from 0 below n
                              for p = a then (polar p (+ ang0 (* (1- i) (/ +2pi+ n))) len)
                              collect p)))
            (add (make-lwpoly-data verts t)))
          (when c
            (let* ((mode (gkw "Enter an option [Inscribed in circle/Circumscribed about circle] <I>: " '("Inscribed" "Circumscribed") "Inscribed"))
                   (mk (lambda (cur)
                         (let* ((r (dist2d c cur)) (a0 (ang c cur))
                                (rr (if (equal mode "Circumscribed") (/ r (cos (/ +pi+ n))) r))
                                (a0 (if (equal mode "Circumscribed") (+ a0 (/ +pi+ n)) a0)))
                           (loop for i from 0 below n collect (polar c (+ a0 (* i (/ +2pi+ n))) rr)))))
                   (r (gpoint "Specify radius of circle: " :base c
                              :drag (lambda (cur) (lwpoly-pieces (make-lwpoly-data (funcall mk cur) t))))))
              (when (realp r) (setf r (polar c (/ +pi+ 2) r)))
              (when (pointp r) (add (make-lwpoly-data (funcall mk r) t)))))))))

;;; ---------- CIRCLE ----------

(defcmd "CIRCLE" ("C") "Creates a circle."
  (let ((r (gpoint "Specify center point for circle or [3P/2P/Ttr]: " :keywords '("3P" "2P" "Ttr"))))
    (cond
      ((equal r "2P")
       (let* ((a (gpoint "Specify first end point of circle's diameter: "))
              (b (gpoint "Specify second end point of circle's diameter: " :base a
                         :drag (lambda (cur) (list (list :circle (midpt a cur) (/ (dist2d a cur) 2)))))))
         (when (pointp b) (add (circle-data (midpt a b) (/ (dist2d a b) 2))))))
      ((equal r "3P")
       (let* ((a (gpoint "Specify first point on circle: "))
              (b (gpoint "Specify second point on circle: " :base a))
              (c (gpoint "Specify third point on circle: "
                         :drag (lambda (cur) (let ((cc (circumcenter a b cur)))
                                               (and cc (list (list :circle cc (dist2d cc a)))))))))
         (let ((cc (and (pointp c) (circumcenter a b c))))
           (if cc (add (circle-data cc (dist2d cc a))) (out (format nil "Circle does not exist.~%"))))))
      ((equal r "Ttr") (out (format nil "TTR is not supported yet; use 3P with tangent object snaps.~%")))
      ((pointp r)
       (let* ((last (or (sysvar "CIRCLERAD") 1d0))
              (rad (gdist (format nil "Specify radius of circle or [Diameter] <~A>: " (fmt-len last))
                          :base r :keywords '("Diameter")
                          :drag (lambda (cur) (list (list :circle r (dist2d r cur)) (list :seg r cur))))))
         (when (equal rad "Diameter")
           (setf rad (gdist (format nil "Specify diameter of circle <~A>: " (fmt-len (* 2 last))) :base r
                            :drag (lambda (cur) (list (list :circle r (/ (dist2d r cur) 2))))))
           (when (realp rad) (setf rad (/ rad 2))))
         (let ((rad (if (realp rad) rad last)))
           (when (<= rad 0) (aerr "Value must be positive and nonzero."))
           (setf (sysvar "CIRCLERAD") rad)
           (add (circle-data r rad))))))))

;;; ---------- ARC ----------

(defcmd "ARC" ("A") "Creates an arc."
  (let ((r (gpoint "Specify start point of arc or [Center]: " :keywords '("Center"))))
    (cond
      ((equal r "Center")
       (let* ((c (gpoint "Specify center point of arc: "))
              (s (gpoint "Specify start point of arc: " :base c)))
         (arc-center-start c s)))
      ((pointp r)
       (let ((p2 (gpoint "Specify second point of arc or [Center/End]: " :base r :keywords '("Center" "End"))))
         (cond
           ((equal p2 "Center")
            (arc-center-start (gpoint "Specify center point of arc: " :base r) r))
           ((equal p2 "End")
            (let* ((e (gpoint "Specify end point of arc: " :base r))
                   (c (gpoint "Specify center point of arc or [Radius]: " :base e :keywords '("Radius")
                              :drag (lambda (cur) (list (cons :arc (cdr (data-pieces-arc cur r e))))))))
              (cond ((equal c "Radius")
                     (let* ((rad (gdist "Specify radius of arc: " :base e :flags 3))
                            (m (midpt r e)) (half (/ (dist2d r e) 2)))
                       (if (< rad half) (out (format nil "Invalid radius.~%"))
                           (let ((cc (polar m (+ (ang r e) (/ +pi+ 2)) (sqrt (- (* rad rad) (* half half))))))
                             (add (arc-data cc rad (ang cc r) (ang cc e)))))))
                    ((pointp c) (add (arc-data c (dist2d c r) (ang c r) (ang c e)))))))
           ((pointp p2)
            (let ((p3 (gpoint "Specify end point of arc: " :base p2
                              :drag (lambda (cur) (let ((a (arc-3p r p2 cur))) (and a (data-pieces a)))))))
              (when (pointp p3)
                (let ((a (arc-3p r p2 p3)))
                  (if a (add a) (out (format nil "Points are collinear.~%")))))))))))))

(defun data-pieces-arc (c s e) (list :arc c (dist2d c s) (ang c s) (ang c e)))

(defun arc-center-start (c s)
  (let* ((rad (dist2d c s)) (a1 (ang c s))
         (e (gpoint "Specify end point of arc or [Angle/chord Length]: " :base c :keywords '("Angle" "Length")
                    :drag (lambda (cur) (list (list :arc c rad a1 (ang c cur)))))))
    (cond ((equal e "Angle")
           (let ((inc (gangle "Specify included angle: " :base c)))
             (when inc (if (minusp inc) (add (arc-data c rad (+ a1 inc) a1)) (add (arc-data c rad a1 (+ a1 inc)))))))
          ((equal e "Length")
           (let ((len (gdist "Specify length of chord: " :base s)))
             (when (and len (< len (* 2 rad)))
               (add (arc-data c rad a1 (+ a1 (* 2 (asin (/ len (* 2 rad))))))))))
          ((pointp e) (add (arc-data c rad a1 (ang c e)))))))

;;; ---------- ELLIPSE ----------

(defcmd "ELLIPSE" ("EL") "Creates an ellipse."
  (let ((a (gpoint "Specify axis endpoint of ellipse or [Center]: " :keywords '("Center"))))
    (multiple-value-bind (c major)
        (if (equal a "Center")
            (let* ((c (gpoint "Specify center of ellipse: "))
                   (e (gpoint "Specify endpoint of axis: " :base c)))
              (values c (v- e c)))
            (let ((b (gpoint "Specify other endpoint of axis: " :base a)))
              (values (midpt a b) (v* (v- b a) 0.5d0))))
      (let* ((mlen (dist2d (pt 0 0) major))
             (mk (lambda (len) (list (cons 0 "ELLIPSE") (cons 10 (pt3 c)) (cons 11 (pt3 major))
                                     (cons 40 (max 1d-6 (min 1d0 (/ len mlen)))) (cons 41 0d0) (cons 42 +2pi+))))
             (r (gdist "Specify distance to other axis: " :base c
                       :drag (lambda (cur) (ellipse-pieces (funcall mk (dist2d c cur)))))))
        (when (realp r)
          (if (> r mlen)
              ;; the second axis is the major one
              (let ((nm (polar (pt 0 0) (+ (ang (pt 0 0) major) (/ +pi+ 2)) r)))
                (add (list (cons 0 "ELLIPSE") (cons 10 (pt3 c)) (cons 11 nm)
                           (cons 40 (/ mlen r)) (cons 41 0d0) (cons 42 +2pi+))))
              (add (funcall mk r))))))))

;;; ---------- POINT / TEXT ----------

(defcmd "POINT" ("PO") "Creates a point object."
  (out (format nil "Current point modes:  PDMODE=~D  PDSIZE=~A~%" (sysvar "PDMODE") (fmt-len (sysvar "PDSIZE"))))
  (let ((p (gpoint "Specify a point: ")))
    (when p (add (list (cons 0 "POINT") (cons 10 p))))))

(defcmd "MULTIPLE-POINT" () "Creates points until Enter."
  (loop for p = (gpoint "Specify a point: ") while p do (add (list (cons 0 "POINT") (cons 10 p)))))

(defparameter *text-justify*
  '(("Left" 0 0) ("Center" 1 0) ("Right" 2 0) ("Middle" 4 0) ("TL" 0 3) ("TC" 1 3) ("TR" 2 3)
    ("ML" 0 2) ("MC" 1 2) ("MR" 2 2) ("BL" 0 1) ("BC" 1 1) ("BR" 2 1)))

(defcmd "TEXT" ("DT" "DTEXT") "Creates single-line text."
  (let ((just '(0 0)))
    (out (format nil "Current text style:  \"Standard\"  Text height:  ~A~%" (fmt-len (sysvar "TEXTSIZE"))))
    (let ((p (gpoint "Specify start point of text or [Justify/Style]: " :keywords '("Justify" "Style"))))
      (when (equal p "Style") (out (format nil "Only the Standard style is available.~%"))
            (setf p (gpoint "Specify start point of text or [Justify]: " :keywords '("Justify"))))
      (when (equal p "Justify")
        (let ((j (gkw "Enter an option [Left/Center/Right/Middle/TL/TC/TR/ML/MC/MR/BL/BC/BR]: "
                      (mapcar #'first *text-justify*) "Left")))
          (setf just (rest (assoc j *text-justify* :test #'string-equal)))
          (setf p (gpoint (format nil "Specify ~A point of text: " (string-downcase j))))))
      (when (pointp p)
        (let* ((h (or (gdist (format nil "Specify height <~A>: " (fmt-len (sysvar "TEXTSIZE"))) :base p :flags 6)
                      (sysvar "TEXTSIZE")))
               (rot (or (gangle "Specify rotation angle of text <0>: " :base p) 0d0)))
          (setf (sysvar "TEXTSIZE") h)
          (loop for i from 0
                for s = (gstring "Enter text: " t)
                until (string= s "")
                do (let ((ip (polar p (- rot (/ +pi+ 2)) (* i h 1.667d0))))
                     (add (append (list (cons 0 "TEXT") (cons 10 ip) (cons 40 (d h)) (cons 1 s) (cons 50 (d rot)))
                                  (unless (equal just '(0 0))
                                    (list (cons 72 (first just)) (cons 73 (second just)) (cons 11 ip))))))))))))

(defcmd "MTEXT" ("MT" "T") "Creates multiline text (enter lines; empty line finishes)."
  (let ((p (gpoint "Specify first corner: ")))
    (when p
      (get-user-input :corner :base p :prompt "Specify opposite corner: ")
      (let ((lines (loop for s = (gstring "MText: " t) until (string= s "") collect s)))
        (when lines
          (add (list (cons 0 "MTEXT") (cons 10 p) (cons 40 (d (sysvar "TEXTSIZE"))) (cons 71 1)
                     (cons 1 (format nil "~{~A~^\\P~}" lines)) (cons 50 0d0))))))))

;;; ---------- DIMENSIONS ----------

(defvar *dim-counter* 0)
(defun dimvar (name default) (or (sysvar name) default))

(defun unique-dim-block-name ()
  (loop for n = (format nil "*D~D" (incf *dim-counter*)) unless (find-block n) return n))

(defun dim-geometry (p1 p2 loc theta text-override)
  "Entities (alists) for a linear dimension measured along THETA."
  (let* ((sc (d (dimvar "DIMSCALE" 1d0)))
         (asz (* sc (d (dimvar "DIMASZ" 0.18d0)))) (txt (* sc (d (dimvar "DIMTXT" 0.18d0))))
         (exo (* sc (d (dimvar "DIMEXO" 0.0625d0)))) (exe (* sc (d (dimvar "DIMEXE" 0.18d0))))
         (gap (* sc (d (dimvar "DIMGAP" 0.09d0))))
         (u (pt (cos theta) (sin theta))) (n (pt (- (sin theta)) (cos theta)))
         (dot (lambda (a b) (+ (* (px a) (px b)) (* (py a) (py b)))))
         (d1 (v+ p1 (v* n (funcall dot (v- loc p1) n))))
         (d2 (v+ p2 (v* n (funcall dot (v- loc p2) n))))
         (meas (abs (funcall dot (v- p2 p1) u)))
         (ents nil))
    (flet ((ext (p dd)
             (let ((len (dist2d p dd)))
               (when (> len exo)
                 (let ((dir (ang p dd)))
                   (push (line-data (polar p dir exo) (polar dd dir exe)) ents)))))
           (arrow (tip toward)
             (let* ((a (ang tip toward)) (b (polar tip a asz))
                    (w (/ asz 6)) (c1 (polar b (+ a (/ +pi+ 2)) w)) (c2 (polar b (- a (/ +pi+ 2)) w)))
               (push (list (cons 0 "SOLID") (cons 10 (pt3 tip)) (cons 11 (pt3 c1)) (cons 12 (pt3 c2)) (cons 13 (pt3 c2))) ents))))
      (ext p1 d1) (ext p2 d2)
      (push (line-data d1 d2) ents)
      (when (> (dist2d d1 d2) 1d-9) (arrow d1 d2) (arrow d2 d1))
      (let* ((tr (norm-ang theta))
             (tr (if (and (> tr (/ +pi+ 2)) (<= tr (* 1.5 +pi+))) (norm-ang (+ tr +pi+)) tr))
             (up (polar (pt 0 0) (+ tr (/ +pi+ 2)) 1))
             (mid (midpt d1 d2))
             (tp (v+ mid (v* up gap)))
             (s (if (and text-override (plusp (length text-override)) (not (equal text-override "<>")))
                    text-override (fmt-len meas))))
        (push (list (cons 0 "MTEXT") (cons 10 tp) (cons 40 txt) (cons 71 8) (cons 1 s) (cons 50 tr)) ents)
        (values (nreverse ents) meas d2 tp)))))

(defun maybe-rebuild-dimension (data)
  "Regenerate the anonymous block of a linear/aligned dimension from its definition points."
  (let ((type (logand (or (dxf 70 data) 0) 7)))
    (if (and (member type '(0 1)) (dxf 13 data) (dxf 14 data) (dxf 10 data))
        (let* ((p1 (pt3 (dxf 13 data))) (p2 (pt3 (dxf 14 data))) (loc (pt3 (dxf 10 data)))
               (theta (if (= type 1) (ang p1 p2) (or (dxf 50 data) 0d0))))
          (multiple-value-bind (ents meas) (dim-geometry p1 p2 loc theta (dxf 1 data))
            (let ((name (unique-dim-block-name)))
              (setf (drawing-blocks *dwg*)
                    (cons (make-block-def :name name :base (pt 0 0) :entities ents) (drawing-blocks *dwg*)))
              (set-dxf 42 meas (set-dxf 2 name data)))))
        data)))

(defvar *dim-p2* nil)
(defun dim-command (aligned)
  (let* ((p1 (gpoint "Specify first extension line origin or <select object>: ")))
    (when (null p1)
      (let ((sel (get-user-input :entity :prompt "Select object to dimension: ")))
        (let ((e (sel-entity sel)))
          (when (and e (equal (ent-type e) "LINE"))
            (setf p1 (pt3 (dxf 10 (ent-data e)))
                  *dim-p2* (pt3 (dxf 11 (ent-data e))))))))
    (when p1
      (let* ((p2 (or (shiftf *dim-p2* nil) (gpoint "Specify second extension line origin: " :base p1)))
             (theta-fn (lambda (loc)
                         (if aligned (ang p1 p2)
                             (let ((m (midpt p1 p2)))
                               (if (or (> (py loc) (max (py p1) (py p2))) (< (py loc) (min (py p1) (py p2)))
                                       (< (abs (- (py loc) (py m))) (abs (- (px loc) (px m)))))
                                   (if (or (> (px loc) (max (px p1) (px p2))) (< (px loc) (min (px p1) (px p2))))
                                       (if (> (abs (- (px loc) (px m))) (abs (- (py loc) (py m)))) (/ +pi+ 2) 0d0)
                                       0d0)
                                   (/ +pi+ 2))))))
             (text nil)
             (loc nil))
        (loop
          (let ((r (gpoint (if aligned "Specify dimension line location or [Text]: "
                               "Specify dimension line location or [Text/Horizontal/Vertical]: ")
                           :keywords (if aligned '("Text") '("Text" "Horizontal" "Vertical"))
                           :drag (lambda (cur) (mapcar (lambda (e) (list :data e))
                                                       (dim-geometry p1 p2 cur (funcall theta-fn cur) text))))))
            (cond ((equal r "Text") (setf text (gstring "Enter dimension text <>: " t)))
                  ((equal r "Horizontal") (setf theta-fn (constantly 0d0)))
                  ((equal r "Vertical") (setf theta-fn (constantly (/ +pi+ 2))))
                  ((pointp r) (setf loc r) (return))
                  (t (return)))))
        (when loc
          (let ((theta (funcall theta-fn loc)))
            (multiple-value-bind (ents meas d2 tp) (dim-geometry p1 p2 loc theta text)
              (declare (ignore ents))
              (add (list (cons 0 "DIMENSION") (cons 2 "*D") (cons 10 d2) (cons 11 tp)
                         (cons 70 (if aligned 33 32)) (cons 1 (or text "")) (cons 3 "Standard")
                         (cons 13 (pt3 p1)) (cons 14 (pt3 p2)) (cons 50 (d theta)) (cons 42 meas)))
              (out (format nil "Dimension text = ~A~%" (fmt-len meas))))))))))

(defcmd "DIMLINEAR" ("DLI" "DIMLIN") "Creates a horizontal or vertical linear dimension." (dim-command nil))
(defcmd "DIMALIGNED" ("DAL" "DIMALI") "Creates an aligned linear dimension." (dim-command t))
