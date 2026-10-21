;;;; commands.lisp — command registry and drawing commands.

(in-package :fancy-pencil)

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

