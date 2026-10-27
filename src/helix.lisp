;;;; helix.lisp — HELIX: a 2D spiral or a 3D spring/thread path, as in AutoCAD.
;;;;
;;;; A helix is kept by its axis and shape, so it can be changed afterwards (Properties, grips):
;;;;   10  centre of the base          11  start point (sets the base radius and start angle)
;;;;   12  centre of the top           13  a point one unit up the axis from the base (its direction)
;;;;   40  top radius                  41  number of turns          290  1 = counterclockwise (right-handed)
;;;; With a height of 0 it is a flat spiral. SWEEP a circle along it to make a spring or a thread.
;;;; Saved in DXF 2000 files as AutoCAD's HELIX object (a spline with the helix's own data).

(in-package :fancy-pencil)

(defvar *helix-turns* 3d0)
(defvar *helix-ccw* t)
(defvar *helix-height* 1d0)
(defparameter +helix-segments-per-turn+ 36)

;;; ---------- geometry ----------

(defun helix-frame (data)
  "(values base axis-unit height u-unit v-unit base-radius top-radius turns ccw)."
  (let* ((b (pt3 (dxf 10 data)))
         (w (let ((d (v- (pt3 (or (dxf 13 data) (v+ b (pt 0 0 1)))) b)))
              (if (< (vlen3 d) 1d-12) (pt 0 0 1) (unit3 d))))
         (h (vdot3 (v- (pt3 (or (dxf 12 data) b)) b) w))
         (s (v- (pt3 (or (dxf 11 data) (v+ b (pt 1 0 0)))) b))
         (u0 (v- s (v* w (vdot3 s w))))
         (r0 (vlen3 u0))
         (u (if (< r0 1d-12) (any-perpendicular w) (v* u0 (/ 1 r0))))
         (v (cross3 w u)))
    (values b w h u v r0 (d (or (dxf 40 data) r0)) (d (or (dxf 41 data) 1)) (not (eql (dxf 290 data) 0)))))

(defun vlen3 (v) (sqrt (vdot3 v v)))

(defun helix-point-at (data tt)
  "The point a fraction TT (0 to 1) of the way along the helix."
  (multiple-value-bind (b w h u v r0 r1 n ccw) (helix-frame data)
    (let* ((th (* (if ccw 1 -1) 2 +pi+ n tt)) (r (+ r0 (* (- r1 r0) tt))))
      (v+ (v+ b (v* w (* h tt))) (v+ (v* u (* r (cos th))) (v* v (* r (sin th))))))))

(defun helix-points (data &optional per-turn)
  (multiple-value-bind (b w h u v r0 r1 n) (helix-frame data)
    (declare (ignore b w h u v r0 r1))
    (let ((m (max 8 (min 6000 (ceiling (* (abs n) (or per-turn +helix-segments-per-turn+)))))))
      (loop for i from 0 to m collect (helix-point-at data (/ i (d m)))))))

(defun helix-pieces (data)
  (loop for (a b) on (helix-points data) while b collect (list :seg a b)))

(defun helix-length (data) (polyline-length (helix-points data 360)))

(defun make-helix-data (base start top dir rtop turns ccw)
  (list (cons 0 "HELIX") (cons 10 (pt3 base)) (cons 11 (pt3 start)) (cons 12 (pt3 top)) (cons 13 (pt3 dir))
        (cons 40 (d rtop)) (cons 41 (d turns)) (cons 290 (if ccw 1 0))))

(defun simple-helix (c r0 r1 h turns ccw &optional (start-angle 0d0))
  "A helix standing on the working plane at C (up its Z)."
  (let ((c (pt3 c)))
    (make-helix-data c (v+ c (pt (* r0 (cos start-angle)) (* r0 (sin start-angle)) 0))
                     (v+ c (pt 0 0 h)) (v+ c (pt 0 0 1)) r1 turns ccw)))

(defun xform-helix (data xf)
  (let* ((s (abs (xf-scale xf)))
         (d1 (map-codes (map-points data '(10 11 12 13) (xf-fn xf)) '(40) (lambda (r) (* r s))))
         ;; the direction point moves with the scale: put it back one unit from the base
         (b (pt3 (dxf 10 d1))) (dd (v- (pt3 (dxf 13 d1)) b))
         (d1 (if (> (vlen3 dd) 1d-12) (set-dxf 13 (v+ b (unit3 dd)) d1) d1)))
    ;; a mirror image turns the other way
    (if (xf-mirror xf) (set-dxf 290 (if (eql (dxf 290 data) 0) 1 0) d1) d1)))

(defun set-helix (data &key rbase rtop height turns turn-height ccw)
  "DATA with some of its shape changed (the base, axis and start direction stay)."
  (multiple-value-bind (b w h u v r0 r1 n dir) (helix-frame data)
    (declare (ignore v))
    (let* ((n (cond (turns (max 1d-3 (d turns)))
                    ((and turn-height (> (abs turn-height) 1d-12)) (max 1d-3 (abs (/ h turn-height))))
                    (t n)))
           (h (or height h))
           (r0 (if rbase (max 0d0 (d rbase)) r0))
           (r1 (if rtop (max 0d0 (d rtop)) r1))
           (dir (cond ((eq ccw :cw) nil) ((eq ccw t) t) (t dir))))
      (set-dxf 290 (if dir 1 0)
               (set-dxf 41 n (set-dxf 40 r1 (set-dxf 12 (v+ b (v* w h)) (set-dxf 11 (v+ b (v* u r0)) data))))))))

;;; ---------- the command ----------

(defun helix-drag (c r0 r1 h)
  (lambda (cur)
    (declare (ignore cur))
    (helix-pieces (simple-helix c r0 r1 h *helix-turns* *helix-ccw*))))

(defun helix-settings-line ()
  (format nil "Number of turns = ~A     Twist=~:[CW~;CCW~]~%" (fmt-plain *helix-turns*) *helix-ccw*))

(defun ask-helix-radius (prompt c default other)
  "A radius picked or typed (or [Diameter]): (values radius point). A picked point (or a typed
distance, along the cursor's direction) also gives where the helix starts. OTHER, when given, is
the base radius (for the drag)."
  (let ((r (get-user-input :point :prompt (format nil "~A or [Diameter] <~A>: " prompt (fmt-len default))
                                  :base c :keywords '("Diameter")
                                  :drag (lambda (cur) (let ((rr (dist2d c cur)))
                                                        (helix-pieces (simple-helix c (or other rr) rr 0d0 1 *helix-ccw* (ang c cur))))))))
    (cond ((equal r "Diameter")
           (let ((dd (get-user-input :distance :prompt (format nil "Specify diameter <~A>: " (fmt-len (* 2 default))) :base c)))
             (cond ((realp dd) (/ (abs (d dd)) 2)) ((pointp dd) (/ (dist2d c dd) 2)) (t default))))
          ((pointp r) (values (dist2d c r) r))
          (t default))))

(defcmd "HELIX" () "Creates a helix (a spring or thread path) or a flat spiral."
  (out (helix-settings-line))
  (let ((c (gpoint "Specify center point of base: ")))
    (when (pointp c)
      (let ((c (pt3 c)))
        (multiple-value-bind (r0 picked) (ask-helix-radius "Specify base radius" c 1d0 nil)
          (when (and r0 (plusp r0))
            (let* ((start-angle (if (pointp picked) (ang c picked) 0d0))
                   (r1 (or (ask-helix-radius "Specify top radius" c r0 r0) r0))
                   (h nil) (top nil))
              (loop
                (let ((x (get-user-input :distance :base c
                                                   :prompt (format nil "Specify helix height or [Axis endpoint/Turns/turn Height/tWist] <~A>: "
                                                                   (fmt-len *helix-height*))
                                                   :keywords '("Axis" "Turns" "Height" "tWist")
                                                   :drag (lambda (cur) (funcall (helix-drag c r0 r1 (dist2d c cur)) cur)))))
                  (cond
                    ((equal x "Turns")
                     (let ((n (greal (format nil "Enter number of turns <~A>: " (fmt-plain *helix-turns*)) :flags 6)))
                       (when n (setf *helix-turns* (min 500d0 (max 1d-3 (d n)))))))
                    ((equal x "Height")
                     (let ((th (gdist (format nil "Specify distance between turns <~A>: " (fmt-len (/ *helix-height* *helix-turns*))) :base c)))
                       (when (and (realp th) (/= th 0))
                         ;; turn height first: then the height sets the number of turns
                         (let ((hh (ask-height "Specify helix height" c (* *helix-turns* (abs th)))))
                           (when hh
                             (setf h hh *helix-turns* (min 500d0 (max 1d-3 (abs (/ hh th)))))
                             (return))))))
                    ((equal x "tWist")
                     (let ((k (gkw (format nil "Enter twist direction of helix [CW/CCW] <~:[CW~;CCW~]>: " *helix-ccw*)
                                   '("CW" "CCW") (if *helix-ccw* "CCW" "CW"))))
                       (setf *helix-ccw* (equal k "CCW"))))
                    ((equal x "Axis")
                     (let ((p (get-user-input :point :base c :prompt "Specify axis endpoint: ")))
                       (when (pointp p)
                         (setf top (pt3 p)) (return))))
                    ((realp x) (setf h (d x)) (return))
                    ((pointp x) (setf h (dist2d c x)) (return))
                    (t (setf h *helix-height*) (return)))))
              (let* ((data (if top
                               ;; along a picked axis: the start direction square to it
                               (let* ((ax (v- top c)) (len (vlen3 ax)))
                                 (if (< len 1d-9)
                                     (simple-helix c r0 r1 0d0 *helix-turns* *helix-ccw* start-angle)
                                     (let* ((w (unit3 ax))
                                            (want (pt (cos start-angle) (sin start-angle) 0))
                                            (u0 (v- want (v* w (vdot3 want w))))
                                            (u (if (< (vlen3 u0) 1d-6) (any-perpendicular w) (unit3 u0))))
                                       (setf *helix-height* len)
                                       (make-helix-data c (v+ c (v* u r0)) top (v+ c w) r1 *helix-turns* *helix-ccw*))))
                               (progn (setf *helix-height* (if (zerop h) *helix-height* h))
                                      (simple-helix c r0 r1 h *helix-turns* *helix-ccw* start-angle))))
                     (e (add data)))
                (out (format nil "Helix: ~A turn~:P, ~:[flat spiral~;height ~:*~A~], length ~A.~%"
                             (fmt-plain (dxf 41 data))
                             (let ((hh (nth-value 2 (helix-frame data)))) (and (> (abs hh) 1d-12) (fmt-len hh)))
                             (fmt-len (helix-length data))))
                (when (gui-p) (gui-regen))
                e))))))))

;;; ---------- grips, Properties, LIST ----------

(defun helix-grips (data)
  (multiple-value-bind (b w h u v r0 r1) (helix-frame data)
    (declare (ignore v r0))
    (let ((top (v+ b (v* w h))))
      (list (cons b :move)
            (cons (pt3 (dxf 11 data)) :hstart)
            (cons top :htop)
            (cons (v+ top (v* u r1)) :htopr)))))

(defun helix-grip-stretch (data id to)
  (multiple-value-bind (b w h u v r0 r1) (helix-frame data)
    (declare (ignore h u v r0 r1))
    (let ((d (v- (pt3 to) b)))
      (case id
        ;; the start point: the base radius (and where it starts), square to the axis
        (:hstart (let ((u0 (v- d (v* w (vdot3 d w))))) (if (< (vlen3 u0) 1d-9) data (set-dxf 11 (v+ b u0) data))))
        ;; the top centre: the height, along the axis
        (:htop (set-helix data :height (vdot3 d w)))
        ;; a point on the top: the top radius
        (:htopr (let* ((top (pt3 (dxf 12 data))) (dt (v- (pt3 to) top)))
                  (set-helix data :rtop (vlen3 (v- dt (v* w (vdot3 dt w)))))))
        (t data)))))

(defun helix-palette-rows (sel)
  (flet ((v (fn) (common-value sel fn))
         (f (data k) (nth k (multiple-value-list (helix-frame data)))))
    (list '("" "Helix" "" :head)
          (list "HXBR" "Base radius" (v (lambda (d) (fmt-num (f d 5)))) :entry)
          (list "HXTR" "Top radius" (v (lambda (d) (fmt-num (f d 6)))) :entry)
          (list "HXH" "Height" (v (lambda (d) (fmt-num (f d 2)))) :entry)
          (list "HXN" "Turns" (v (lambda (d) (fmt-plain (f d 7)))) :entry)
          (list "HXTH" "Turn height" (v (lambda (d) (fmt-num (/ (f d 2) (max 1d-12 (f d 7)))))) :entry)
          (list "HXTW" "Twist" (v (lambda (d) (if (f d 8) "CCW" "CW"))) :combo '("CCW" "CW"))
          (list "" "Length" (v (lambda (d) (fmt-num (helix-length d)))) :ro))))

(defun helix-palette-edit (d key val num)
  (cond ((and (string= key "HXBR") num (>= num 0)) (set-helix d :rbase num))
        ((and (string= key "HXTR") num (>= num 0)) (set-helix d :rtop num))
        ((and (string= key "HXH") num) (set-helix d :height num))
        ((and (string= key "HXN") num (> num 0)) (set-helix d :turns (min 500 num)))
        ((and (string= key "HXTH") num (/= num 0)) (set-helix d :turn-height num))
        ((string= key "HXTW") (set-helix d :ccw (if (string-equal val "CW") :cw t)))
        (t d)))

(defun describe-helix (data o)
  (multiple-value-bind (b w h u v r0 r1 n ccw) (helix-frame data)
    (declare (ignore u v))
    (format o "~10TBase center: X=~A  Y=~A  Z=~A~%" (fmt-len (px b)) (fmt-len (py b)) (fmt-len (pz b)))
    (format o "~10TAxis direction: ~A, ~A, ~A~%" (fmt-plain (px w)) (fmt-plain (py w)) (fmt-plain (pz w)))
    (format o "~10TBase radius: ~A,  top radius: ~A~%" (fmt-len r0) (fmt-len r1))
    (format o "~10THeight: ~A,  turns: ~A,  turn height: ~A~%" (fmt-len h) (fmt-plain n) (fmt-len (/ h (max 1d-12 n))))
    (format o "~10TTwist: ~:[CW~;CCW~],  length: ~A~%" ccw (fmt-len (helix-length data)))))

;;; ---------- DXF ----------

(defun helix-spline (data)
  "A cubic B-spline close to the helix: (values control-points knots). Its ends are exact; the
control points between are pushed out so the curve keeps the helix's radius."
  (multiple-value-bind (b w h u v r0 r1 n ccw) (helix-frame data)
    (declare (ignore h u v r0 r1 ccw))
    (let* ((per 32) (m (max 4 (ceiling (* (abs n) per))))
           (dth (/ (* 2 +pi+ (abs n)) m))
           (f (/ (+ 4 (* 2 (cos dth))) 6))
           (ctrl (loop for i from 0 to m
                       collect (let ((p (helix-point-at data (/ i (d m)))))
                                 (if (or (= i 0) (= i m)) p
                                     (let* ((d (v- p b)) (along (v* w (vdot3 d w))) (out (v- d along)))
                                       (v+ (v+ b along) (v* out (/ 1 f))))))))
           (nc (length ctrl))
           (knots (append (list 0 0 0 0) (loop for k from 1 to (- nc 4) collect k) (make-list 4 :initial-element (- nc 3)))))
      (values ctrl (mapcar #'d knots)))))

(defun write-helix-2000 (out data handle owner)
  (multiple-value-bind (ctrl knots) (helix-spline data)
    (multiple-value-bind (b w h u v r0 r1 n ccw) (helix-frame data)
      (declare (ignore u v r1))
      (write-common out data handle owner)
      (w out 100 "AcDbSpline") (w out 70 0) (w out 71 3) (w out 72 (length knots)) (w out 73 (length ctrl)) (w out 74 0)
      (w out 42 1d-10) (w out 43 1d-10)
      (dolist (k knots) (w out 40 k))
      (dolist (p ctrl) (wp out 10 p))
      (w out 100 "AcDbHelix") (w out 90 29) (w out 91 63)
      (wp out 10 b) (wp out 11 (pt3 (dxf 11 data))) (wp out 12 w)
      (w out 40 r0) (w out 41 n) (w out 42 (/ h (max 1d-12 n))) (w out 290 (if ccw 1 0)) (w out 280 1))))

(defun helix-from-dxf (g)
  "A HELIX read from a file (its raw group G)."
  (let* ((pos (position-if (lambda (p) (and (consp p) (eql (car p) 100) (equal (cdr p) "AcDbHelix"))) g))
         (spl (group-coords (clean-group (subseq g 0 (or pos (length g))))))
         (hel (and pos (group-coords (subseq g pos)))))
    (when hel
      (let* ((b (pt3 (or (dxf 10 hel) (pt 0 0))))
             (w (let ((a (pt3 (or (dxf 12 hel) (pt 0 0 1))))) (if (< (vlen3 a) 1d-12) (pt 0 0 1) (unit3 a))))
             (s (pt3 (or (dxf 11 hel) (v+ b (pt 1 0 0)))))
             (n (d (or (dxf 41 hel) 1))) (th (d (or (dxf 42 hel) 0)))
             (h (* n th))
             (ctrl (loop for p in spl when (and (consp p) (eql (car p) 10)) collect (pt3 (cdr p))))
             (lastp (car (last ctrl)))
             (top (v+ b (v* w h)))
             (r1 (if lastp (let ((d (v- lastp b))) (vlen3 (v- d (v* w (vdot3 d w)))))
                     (d (or (dxf 40 hel) 1)))))
        (append (make-helix-data b s top (v+ b w) r1 n (not (eql (dxf 290 hel) 0)))
                (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370 48)))) spl))))))
