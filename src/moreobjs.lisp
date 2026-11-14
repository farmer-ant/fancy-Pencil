;;;; moreobjs.lisp — more kinds of AutoCAD object:
;;;;   XLINE, RAY   construction lines (no end either way) and rays (one end)
;;;;   TRACE        wide filled lines (the four corners, like SOLID)
;;;;   WIPEOUT      a blank area that hides what is under it
;;;;   MLINE        multilines: parallel lines drawn together, from a multiline style (MLINESTYLE)
;;;;   TOLERANCE    geometric tolerance frames (feature control frames)
;;;;   DIMARC       arc length dimensions (kept as a DIMENSION marked :arclen; ARC_DIMENSION in files)
;;;;   OLE2FRAME    embedded Office (OLE) objects: shown, moved, kept on saving, opened by OLEOPEN
;;;;
;;;; Each type registers its functions in *KINDS* (package.lisp); the general code (display,
;;;; plotting, grips, LIST, Properties, EXPLODE, DXF reading and writing) calls them.

(in-package :fancy-pencil)

(defmacro defkind (type &rest fns)
  `(setf (gethash ,type *kinds*) (list ,@fns)))

;;; ---------------------------------------------------------------- shared helpers

(defun picture-sub (data &optional color ltype)
  "A piece of an object's picture: on layer 0 and without a colour of its own, so it takes the
object's colour (COLOR, an ACI, and LTYPE when the piece has its own)."
  (append data (list (cons 8 "0")) (and color (list (cons 62 color))) (and ltype (list (cons 6 ltype)))))

(defun explode-picture (data subs)
  "SUBS (an object's picture) as objects of their own, on the object's layer, in its colour."
  (let ((layer (or (dxf 8 data) "0")) (c (assoc 62 data)) (lt (assoc 6 data)))
    (loop for s in subs
          collect (let* ((nd (set-dxf 8 layer s)) (sc (dxf 62 s)))
                    (when (or (null sc) (eql sc 0)) (setf nd (rem-dxf 62 nd)) (when c (setf nd (append nd (list c)))))
                    (if (and lt (not (dxf 6 s))) (append nd (list lt)) nd)))))

(defun picture-pieces (fn)
  (lambda (data) (loop for s in (funcall fn data) append (data-pieces s))))

(defun dxf-props (g)
  "The layer, colour, linetype ... of raw DXF group G."
  (remove-if-not (lambda (p) (and (consp p) (member (car p) '(8 62 6 370 48 420 440)))) (clean-group g)))

(defun raw-subclass (g name)
  "The groups of raw DXF group G after its subclass marker (100) NAME, or NIL."
  (let ((pos (position-if (lambda (p) (and (consp p) (eql (car p) 100) (equal (cdr p) name))) g)))
    (and pos (nthcdr (1+ pos) g))))

(defun all-codes (code data) (loop for p in data when (and (consp p) (eql (car p) code)) collect (cdr p)))

(defun left-normal (u) (pt (- (py u)) (px u) 0))

(defun grip-reach ()
  "How far from its point a direction grip sits: about 80 pixels on the screen."
  (if (gui-p) (/ 80d0 *vscale*) 1d0))

(defun every-drawing-object ()
  "Every object's data: model space, the layouts and inside blocks."
  (append (mapcar #'ename-data (all-live-entities))
          (loop for b in (drawing-blocks *dwg*) append (block-def-entities b))))

(defun drawing-uses-p (pred) (some pred (every-drawing-object)))

;;; ---------------------------------------------------------------- XLINE and RAY

(defparameter +xline-reach+ 1d7 "How far a construction line runs, for selecting and snapping.")

(defun xline-base (d) (pt3 (or (dxf 10 d) (pt 0 0))))
(defun xline-dir (d)
  (let ((v (pt3 (or (dxf 11 d) (pt 1 0 0))))) (if (< (vlen3 v) 1d-12) (pt 1 0 0) (unit3 v))))
(defun ray-p (d) (equal (dxf 0 d) "RAY"))

(defun xline-pieces (d)
  (let* ((b (xline-base d)) (u (xline-dir d)) (r (max +xline-reach+ (* 1000 (vlen3 b)))))
    (list (list :seg (if (ray-p d) b (v- b (v* u r))) (v+ b (v* u r))))))

(defun xline-clip (d x0 y0 x1 y1 &optional (uv #'identity))
  "The part of construction line or ray D inside the rectangle X0 Y0 X1 Y1 (in the coordinates UV
maps points to): (start end), or NIL."
  (let* ((b (xline-base d)) (u (xline-dir d))
         (p0 (funcall uv b)) (p1 (funcall uv (v+ b u)))
         (dx (- (px p1) (px p0))) (dy (- (py p1) (py p0)))
         (t0 (if (ray-p d) 0d0 -1d30)) (t1 1d30))
    (flet ((edge (p q)
             (cond ((< (abs p) 1d-15) (when (< q 0) (return-from xline-clip nil)))
                   ((< p 0) (setf t0 (max t0 (/ q p))))
                   (t (setf t1 (min t1 (/ q p)))))))
      (edge (- dx) (- (px p0) x0)) (edge dx (- x1 (px p0)))
      (edge (- dy) (- (py p0) y0)) (edge dy (- y1 (py p0))))
    (when (< t0 t1) (list (v+ b (v* u t0)) (v+ b (v* u t1))))))

(defun xline-picture (d)
  "The part that shows: cut to the plot's window or the view (else a long line)."
  (multiple-value-bind (x0 y0 x1 y1)
      (cond (*plot-clip* (values-list *plot-clip*))
            ((gui-p) (view-rect 400))
            (t nil))
    (let ((seg (if x0
                   (xline-clip d x0 y0 x1 y1 (if (projected-view-p) #'w->uv #'identity))
                   (rest (first (xline-pieces d))))))
      (and seg (list (picture-sub (line-data (first seg) (second seg))))))))

(defun xform-xline (d xf)
  (let* ((fp (xf-fn xf)) (b (xline-base d)) (nb (funcall fp b))
         (nu (unit3 (v- (funcall fp (v+ b (xline-dir d))) nb))))
    (set-dxf 11 (if (< (vlen3 nu) 1d-12) (xline-dir d) nu) (set-dxf 10 nb d))))

(defun xline-grips (d)
  (let ((b (xline-base d)) (u (v* (xline-dir d) (grip-reach))))
    (if (ray-p d)
        (list (cons b :xbase) (cons (v+ b u) :xdir+))
        (list (cons b :move) (cons (v+ b u) :xdir+) (cons (v- b u) :xdir-)))))

(defun xline-stretch (d id from to)
  (declare (ignore from))
  (let ((b (xline-base d)))
    (case id
      (:xbase (set-dxf 10 (pt3 to) d))
      ((:xdir+ :xdir-) (let ((v (v- (pt3 to) b)))
                         (if (< (vlen3 v) 1d-12) d (set-dxf 11 (unit3 (if (eq id :xdir-) (v* v -1) v)) d))))
      (t d))))

(defun describe-xline (d o)
  (let ((b (xline-base d)) (u (xline-dir d)))
    (format o "~10T~:[Root~;Base~] point: X=~A  Y=~A  Z=~A~%" (ray-p d) (fmt-len (px b)) (fmt-len (py b)) (fmt-len (pz b)))
    (format o "~10TSecond point: X=~A  Y=~A  Z=~A~%" (fmt-len (+ (px b) (px u))) (fmt-len (+ (py b) (py u))) (fmt-len (+ (pz b) (pz u))))
    (format o "~10TDirection: ~A~@[ (rising ~A)~]~%" (fmt-ang (atan (py u) (px u)))
            (and (> (abs (pz u)) 1d-9) (fmt-plain (pz u))))))

(defun write-xline-2000 (out d handle owner)
  (write-common out d handle owner)
  (w out 100 (if (ray-p d) "AcDbRay" "AcDbXline"))
  (wp out 10 (xline-base d)) (wp out 11 (xline-dir d)))

(defun xline-r12 (d)
  "R12 has neither: a line across the drawing (half as far again beyond its extents)."
  (let* ((e (or (drawing-extents) (list 0d0 0d0 12d0 9d0)))
         (mx (* 0.5 (max 1d0 (- (third e) (first e)) (- (fourth e) (second e)))))
         (seg (xline-clip d (- (first e) mx) (- (second e) mx) (+ (third e) mx) (+ (fourth e) mx))))
    (and seg (explode-picture d (list (line-data (first seg) (second seg)))))))

(defun xline-from-dxf (g)
  (let ((data (group-coords (clean-group g))))
    (append (list (cons 0 (cdr (first g))) (cons 10 (pt3 (or (dxf 10 data) (pt 0 0))))
                  (cons 11 (let ((v (pt3 (or (dxf 11 data) (pt 1 0 0))))) (if (< (vlen3 v) 1d-12) (pt 1 0 0) (unit3 v)))))
            (dxf-props g))))

(dolist (type '("XLINE" "RAY"))
  (setf (gethash type *kinds*)
        (list :name (if (equal type "RAY") "Ray" "Xline") :wcs t :infinite t
              :pieces 'xline-pieces :picture 'xline-picture :xform 'xform-xline
              :grips 'xline-grips :stretch 'xline-stretch :describe 'describe-xline
              :hover (lambda (d) (list (if (ray-p d) "Ray" "Construction line")
                                       (format nil "Angle  ~A" (fmt-ang (let ((u (xline-dir d))) (atan (py u) (px u)))))))
              :explode nil :write2000 'write-xline-2000 :write12 'xline-r12 :from-dxf 'xline-from-dxf
              :valid (lambda (d) (and (consp (dxf 10 d)) (consp (dxf 11 d)))))))

(defun make-xline (type b dir)
  (let ((u (unit3 (pt3 dir))))
    (when (> (vlen3 u) 0.5d0)
      (add (list (cons 0 type) (cons 10 (pt3 b)) (cons 11 u))))))

(defun xline-drag (type b &optional dir)
  "Drag: the line from B toward the cursor (or along DIR through the cursor)."
  (lambda (cur)
    (let* ((cur (pt3 cur)) (base (if dir cur (pt3 b))) (u (if dir (pt3 dir) (v- cur (pt3 b)))))
      (when (> (vlen3 u) 1d-12)
        (xline-pieces (list (cons 0 type) (cons 10 base) (cons 11 (unit3 u))))))))

(defun xlines-through-points (type dir)
  "Construction lines along DIR, one through each point given, until Enter."
  (loop (let ((p (gpoint "Specify through point: " :drag (xline-drag type nil dir))))
          (if (pointp p) (make-xline type p dir) (return)))))

(defun line-like-direction (e)
  "(values point direction) of a line, construction line or ray E, or NIL."
  (let* ((d (and e (ent-data e))) (type (and d (dxf 0 d))))
    (cond ((equal type "LINE") (let ((a (pt3 (dxf 10 d))) (b (pt3 (dxf 11 d))))
                                 (and (> (dist a b) 1d-12) (values a (unit3 (v- b a))))))
          ((member type '("XLINE" "RAY") :test #'equal) (values (xline-base d) (xline-dir d)))
          (t nil))))

(defcmd "XLINE" ("XL") "Creates construction lines: lines with no end, for laying out a drawing."
  (let ((r (gpoint "Specify a point or [Hor/Ver/Ang/Bisect/Offset]: "
                   :keywords '("Hor" "Ver" "Ang" "Bisect" "Offset"))))
    (cond
      ((pointp r)
       (let ((b (pt3 r)))
         (loop (let ((p (gpoint "Specify through point: " :drag (xline-drag "XLINE" b))))
                 (if (and (pointp p) (> (dist (pt3 p) b) 1d-12)) (make-xline "XLINE" b (v- (pt3 p) b)) (return))))))
      ((equal r "Hor") (xlines-through-points "XLINE" (pt 1 0 0)))
      ((equal r "Ver") (xlines-through-points "XLINE" (pt 0 1 0)))
      ((equal r "Ang")
       (let ((a (get-user-input :angle :prompt "Enter angle of xline (0) or [Reference]: " :keywords '("Reference"))))
         (when (equal a "Reference")
           (let ((e (sel-entity (get-user-input :entity :prompt "Select a line object: "))))
             (multiple-value-bind (p u) (line-like-direction e)
               (declare (ignore p))
               (if (null u)
                   (progn (out (format nil "That is not a line.~%")) (setf a nil))
                   (let ((a2 (get-user-input :angle :prompt "Enter angle of xline <0>: ")))
                     (setf a (+ (atan (py u) (px u)) (if (realp a2) a2 0d0))))))))
         (when (realp a) (xlines-through-points "XLINE" (pt (cos a) (sin a) 0)))))
      ((equal r "Bisect")
       (let* ((v (gpoint "Specify angle vertex point: "))
              (s (and (pointp v) (gpoint "Specify angle start point: " :base v))))
         (when (pointp s)
           (let ((v (pt3 v)) (us (unit3 (v- (pt3 s) (pt3 v)))))
             (flet ((bisector (e) (let* ((ue (unit3 (v- (pt3 e) v))) (m (v+ us ue)))
                                    (if (< (vlen3 m) 1d-9) (left-normal us) m))))
               (loop (let ((e (gpoint "Specify angle end point: " :base v
                                      :drag (lambda (cur) (xline-pieces (list (cons 0 "XLINE") (cons 10 v) (cons 11 (unit3 (bisector cur)))))))))
                       (if (pointp e) (make-xline "XLINE" v (bisector e)) (return)))))))))
      ((equal r "Offset")
       (let* ((now (sysvar "OFFSETDIST"))
              (k (get-user-input :distance :keywords '("Through")
                                           :prompt (format nil "Specify offset distance or [Through] <~A>: "
                                                           (if (and (realp now) (>= now 0)) (fmt-len now) "Through")))))
         (cond ((equal k "Through") (setf (sysvar "OFFSETDIST") -1d0))
               ((realp k) (setf (sysvar "OFFSETDIST") (abs (d k)))))
         (let ((dd (sysvar "OFFSETDIST")))
           (loop
             (let ((e (sel-entity (get-user-input :entity :prompt "Select a line object: "))))
               (when (null e) (return))
               (multiple-value-bind (a u) (line-like-direction e)
                 (if (null u)
                     (out (format nil "That is not a line, construction line or ray.~%"))
                     (if (minusp dd)
                         (let ((p (gpoint "Specify through point: " :drag (xline-drag "XLINE" nil u))))
                           (if (pointp p) (make-xline "XLINE" p u) (return)))
                         (let ((side (gpoint "Specify side to offset: ")))
                           (if (pointp side)
                               (let* ((n (left-normal u)) (s (if (minusp (vdot3 (v- (pt3 side) a) n)) -1 1)))
                                 (make-xline "XLINE" (v+ a (v* n (* s dd))) u))
                               (return)))))))))))
      (t nil))
    (when (gui-p) (gui-regen))))

(defcmd "RAY" () "Creates rays: lines that start at a point and go on without end."
  (let ((b (gpoint "Specify start point: ")))
    (when (pointp b)
      (let ((b (pt3 b)))
        (loop (let ((p (gpoint "Specify through point: " :drag (xline-drag "RAY" b))))
                (if (and (pointp p) (> (dist (pt3 p) b) 1d-12)) (make-xline "RAY" b (v- (pt3 p) b)) (return)))))
      (when (gui-p) (gui-regen)))))

;;; ---------------------------------------------------------------- TRACE

(defun trace-quads (pts w)
  "A wide line WIDTH W through PTS as mitred quads: (left-start right-start left-end right-end) each,
the corner order of a TRACE's groups 10 11 12 13."
  (let* ((h (/ (d w) 2))
         (pts (loop for (p q) on pts when (or (null q) (> (dist2d p q) 1d-12)) collect p))
         (segs (loop for (a b) on pts while b collect (list a b))))
    (flet ((off (seg side)
             (let* ((u (unit3 (v- (second seg) (first seg)))) (o (v* (left-normal u) (* side h))))
               (list (v+ (first seg) o) (v+ (second seg) o)))))
      (loop for seg in segs for i from 0
            collect (let ((prev (and (> i 0) (nth (1- i) segs))) (next (nth (1+ i) segs)))
                      (flet ((corner (side at-start)
                               (let* ((mine (off seg side))
                                      (other (if at-start (and prev (off prev side)) (and next (off next side))))
                                      (plain (if at-start (first mine) (second mine)))
                                      (x (and other (line-line-int (first mine) (second mine) (first other) (second other) nil))))
                                 ;; very sharp turns: no long spike
                                 (if (and x (< (dist2d x plain) (* 10 (max h 1d-12)))) (pt3 x) plain))))
                        (list (corner 1 t) (corner -1 t) (corner 1 nil) (corner -1 nil))))))))

(defun trace-data (q)
  (list (cons 0 "TRACE") (cons 10 (first q)) (cons 11 (second q)) (cons 12 (third q)) (cons 13 (fourth q))))

(defun quad-outline (q)
  (destructuring-bind (a b c dd) q (list (list :seg a b) (list :seg b dd) (list :seg dd c) (list :seg c a))))

(defcmd "TRACE" () "Draws traces: solid-filled lines of a set width (TRACEWID)."
  (let ((w (get-user-input :distance :prompt (format nil "Specify trace width <~A>: " (fmt-len (sysvar "TRACEWID"))))))
    (when (and (realp w) (>= w 0)) (setf (sysvar "TRACEWID") (d w))))
  (let* ((w (d (sysvar "TRACEWID")))
         (p0 (gpoint "Specify start point: "))
         (pts (and (pointp p0) (list (pt3 p0)))))
    (when pts
      (loop (let ((p (gpoint "Specify next point: " :base (car (last pts))
                             :drag (lambda (cur) (loop for q in (trace-quads (append pts (list (pt3 cur))) w)
                                                       append (quad-outline q))))))
              (if (pointp p) (setf pts (append pts (list (pt3 p)))) (return))))
      (when (cdr pts)
        (dolist (q (trace-quads pts w)) (add (trace-data q)))
        (when (gui-p) (gui-regen))))))

;;; ---------------------------------------------------------------- WIPEOUT

(defun wipeout-points (d) (mapcar #'pt3 (all-codes 10 d)))
(defun wipeout-closed-points (d) (let ((ps (wipeout-points d))) (and ps (append ps (list (first ps))))))
(defun wipeout-pieces (d) (loop for (a b) on (wipeout-closed-points d) while b collect (list :seg a b)))
(defun wipeout-frame-mode () (or (sysvar "WIPEOUTFRAME") 1))

(defun clip-polygon-rect (pts x0 y0 x1 y1)
  "Polygon PTS (screen (x y) lists) cut to the rectangle (Sutherland-Hodgman)."
  (flet ((clip (poly coord k keep-above)
           (let ((out nil))
             (flet ((inside (p) (if keep-above (>= (funcall coord p) k) (<= (funcall coord p) k)))
                    (cross (a b) (let ((tt (/ (- k (funcall coord a)) (- (funcall coord b) (funcall coord a)))))
                                   (list (+ (first a) (* tt (- (first b) (first a))))
                                         (+ (second a) (* tt (- (second b) (second a))))))))
               (loop for (a b) on (append poly (list (first poly))) while b
                     do (let ((ia (inside a)) (ib (inside b)))
                          (when ia (push a out))
                          (unless (eq ia ib) (push (cross a b) out)))))
             (nreverse out))))
    (dolist (edge (list (list #'first x0 t) (list #'first x1 nil) (list #'second y0 t) (list #'second y1 nil)) pts)
      (when pts (setf pts (apply #'clip pts edge))))))

(defun wipeout-render (d color tags hl aci)
  (declare (ignore aci))
  (let* ((pts (wipeout-points d))
         (scr (mapcar (lambda (p) (multiple-value-list (w->s p))) pts))
         (m 2000d0)
         (poly (and (>= (length scr) 3) (clip-polygon-rect scr (- m) (- m) (+ *full-cw* m) (+ *full-ch* m))))
         (bg (background-color)))
    (append
     (when (>= (length poly) 3)
       (list (format nil ".c create polygon ~{~A~^ ~} -fill ~A -outline {} -tags {~A}"
                     (mapcar #'fmtn (apply #'append poly)) bg tags)))
     ;; its frame (WIPEOUTFRAME 1 or 2), and always while it is highlighted
     (when (or hl (/= (wipeout-frame-mode) 0))
       (screen-line (wipeout-closed-points d) color tags :dash hl)))))

(defun wipeout-plot (d color lw xp)
  (let ((pts (mapcar (lambda (p) (funcall xp p)) (wipeout-points d))))
    (when (>= (length pts) 3)
      (emit-prim (list :fill (list pts)
                       ;; the paper (or, shown on the screen, the background)
                       (if (and *prims-for-display* (not (light-background-p)))
                           (destructuring-bind (r g b) (hex-rgb (background-color)) (list (/ r 255d0) (/ g 255d0) (/ b 255d0)))
                           '(1d0 1d0 1d0))))
      (when (eql (wipeout-frame-mode) 1)
        (emit-prim (list :poly (append pts (list (first pts))) color lw))))))

(defun wipeout-grips (d) (loop for p in (wipeout-points d) for i from 0 collect (cons p (list :wvertex i))))
(defun wipeout-stretch (d id from to)
  (declare (ignore from))
  (if (and (consp id) (eq (first id) :wvertex)) (set-nth-code d 10 (second id) (pt3 to)) d))

(defun describe-wipeout (d o)
  (let ((ps (wipeout-points d)))
    (format o "~10TVertices: ~D,  area: ~A~%" (length ps) (fmt-area (abs (polygon-area ps))))
    (loop for p in ps for i from 1
          do (format o "~10Tvertex ~D: X=~A  Y=~A  Z=~A~%" i (fmt-len (px p)) (fmt-len (py p)) (fmt-len (pz p))))))

(defun write-wipeout-2000 (out d handle owner)
  "As AutoCAD writes it: an image 1 x 1 pixel stretched over the box, clipped to the outline."
  (let* ((ps (wipeout-points d)) (z (pz (first ps)))
         (x0 (reduce #'min ps :key #'px)) (y0 (reduce #'min ps :key #'py))
         (wd (max 1d-9 (- (reduce #'max ps :key #'px) x0))) (ht (max 1d-9 (- (reduce #'max ps :key #'py) y0))))
    (write-common out d handle owner)
    (w out 100 "AcDbWipeout") (w out 90 0)
    (wp out 10 (pt x0 y0 z)) (wp out 11 (pt wd 0 0)) (wp out 12 (pt 0 ht 0)) (wp out 13 (pt 1 1) t)
    (w out 340 "0") (w out 70 7) (w out 280 1) (w out 281 50) (w out 282 50) (w out 283 0) (w out 360 "0")
    (w out 71 2) (w out 91 (1+ (length ps)))
    (dolist (p (append ps (list (first ps))))
      (wp out 14 (pt (- (/ (- (px p) x0) wd) 0.5d0) (- 0.5d0 (/ (- (py p) y0) ht))) t))))

(defun wipeout-from-dxf (g)
  (let* ((data (group-coords (clean-group g)))
         (ins (pt3 (or (dxf 10 data) (pt 0 0)))) (u (pt3 (or (dxf 11 data) (pt 1 0 0))))
         (v (pt3 (or (dxf 12 data) (pt 0 1 0)))) (h (let ((s (dxf 13 data))) (if (consp s) (py s) 1d0)))
         (clip (mapcar #'pt3 (all-codes 14 data)))
         (clip (if (and (eql (dxf 71 data) 1) (= (length clip) 2))      ; a rectangle: two corners
                   (destructuring-bind (a b) clip (list a (pt (px b) (py a)) b (pt (px a) (py b))))
                   clip))
         (clip (if (and (cdr clip) (< (dist2d (first clip) (car (last clip))) 1d-9)) (butlast clip) clip)))
    (when (>= (length clip) 3)
      (append (list (cons 0 "WIPEOUT"))
              (mapcar (lambda (c) (cons 10 (v+ ins (v+ (v* u (+ (px c) 0.5d0)) (v* v (- h (py c) 0.5d0)))))) clip)
              (dxf-props g)))))

(defun write-wipeout-class (out)
  (w out 0 "CLASS") (w out 1 "WIPEOUT") (w out 2 "AcDbWipeout")
  (w out 3 "WipeOut|AutoCAD Express Tool|expresstools@autodesk.com")
  (w out 90 127) (w out 280 0) (w out 281 1))

(defkind "WIPEOUT" :name "Wipeout" :wcs t
  :pieces 'wipeout-pieces :render 'wipeout-render :plot 'wipeout-plot
  :xform (lambda (d xf) (map-points d '(10) (xf-fn xf)))
  :grips 'wipeout-grips :stretch 'wipeout-stretch :describe 'describe-wipeout
  :hover (lambda (d) (list "Wipeout" (format nil "Area  ~A" (fmt-area (abs (polygon-area (wipeout-points d)))))))
  :explode (lambda (d) (explode-picture d (list (make-lwpoly-data (wipeout-points d) t))))
  :write2000 'write-wipeout-2000 :class 'write-wipeout-class
  :write12 (lambda (d) (explode-picture d (list (make-lwpoly-data (wipeout-points d) t))))
  :from-dxf 'wipeout-from-dxf
  :valid (lambda (d) (>= (length (wipeout-points d)) 3)))

(defun polyline-outline-points (data)
  "A closed polyline's outline as points (its arcs in short straight pieces)."
  (let ((vs (lwpoly-vertices data)))
    ;; (each piece gives its far end: the first corner comes last)
    (loop for ((p b) (q)) on (append vs (list (first vs))) while q
          append (bulge-points (pt3 p) (pt3 q) b))))

(defcmd "WIPEOUT" () "Covers part of the drawing with a blank area (a mask), e.g. under a note on busy linework."
  (let ((r (gpoint "Specify first point or [Frames/Polyline] <Polyline>: " :keywords '("Frames" "Polyline"))))
    (cond
      ((equal r "Frames")
       (let ((k (gkw (format nil "Enter mode [ON/OFF/Display but not plot] <~A>: "
                             (case (wipeout-frame-mode) (0 "OFF") (2 "Display but not plot") (t "ON")))
                     '("ON" "OFF" "Display"))))
         (when k (setf (sysvar "WIPEOUTFRAME") (cond ((equal k "ON") 1) ((equal k "OFF") 0) (t 2))))
         (when (gui-p) (gui-regen))))
      ((pointp r)
       (let ((pts (list (pt3 r))))
         (loop (let* ((n (length pts))
                      (p (gpoint (cond ((= n 1) "Specify next point: ")
                                       ((= n 2) "Specify next point or [Undo]: ")
                                       (t "Specify next point or [Close/Undo]: "))
                                 :base (car (last pts)) :keywords (if (> n 2) '("Close" "Undo") (and (> n 1) '("Undo")))
                                 :drag (lambda (cur) (let ((ps (append pts (list (pt3 cur)))))
                                                       (loop for (a b) on (append ps (list (first ps))) while b collect (list :seg a b)))))))
                 (cond ((pointp p) (setf pts (append pts (list (pt3 p)))))
                       ((equal p "Undo") (setf pts (butlast pts)))
                       (t (return)))))
         (if (>= (length pts) 3)
             (add (cons (cons 0 "WIPEOUT") (mapcar (lambda (p) (cons 10 p)) pts)))
             (out (format nil "A wipeout needs at least three points.~%")))
         (when (gui-p) (gui-regen))))
      (t
       (let ((e (sel-entity (get-user-input :entity :prompt "Select a closed polyline: "))))
         (cond ((null e) nil)
               ((not (and (equal (ent-type e) "LWPOLYLINE") (lwpoly-closed-p (ent-data e))))
                (out (format nil "That is not a closed polyline.~%")))
               (t (let ((pts (polyline-outline-points (ent-data e))))
                    (add (cons (cons 0 "WIPEOUT") (mapcar (lambda (p) (cons 10 p)) pts)))
                    (when (equal (gkw "Erase polyline? [Yes/No] <No>: " '("Yes" "No") "No") "Yes")
                      (delete-entity e))
                    (when (gui-p) (gui-regen))))))))))

;;; ---------------------------------------------------------------- MLINE and its styles

(defparameter +standard-mlstyle+
  '(:name "STANDARD" :flags 0 :desc "" :fill 256 :a1 90d0 :a2 90d0
    :elements ((0.5d0 256 "BYLAYER") (-0.5d0 256 "BYLAYER")))
  "A multiline style: flags 1 fill, 2 joints (miters), 16/256 square caps at the start/end, 64/1024
round (outer arc) caps; elements are (offset colour linetype).")

(defun all-mlstyles ()
  (let ((own (drawing-mlstyles *dwg*)))
    (if (find "STANDARD" own :key (lambda (s) (getf s :name)) :test #'string-equal) own (cons +standard-mlstyle+ own))))
(defun find-mlstyle (name)
  (find name (all-mlstyles) :key (lambda (s) (getf s :name)) :test #'string-equal))
(defun set-mlstyle (st)
  (setf (drawing-mlstyles *dwg*)
        (append (remove (getf st :name) (drawing-mlstyles *dwg*) :key (lambda (s) (getf s :name)) :test #'string-equal)
                (list st))))

(defun mlstyles-from-objects (objects)
  "Keep the drawing's multiline styles (MLINESTYLE objects)."
  (dolist (g objects)
    (when (equal (cdr (first g)) "MLINESTYLE")
      (let* ((g (or (raw-subclass g "AcDbMlineStyle") g))
             (name (cdr (assoc 2 g))) (head (loop for p in g until (eql (car p) 71) collect p))
             (tail (cdr (member 71 g :key #'car)))
             (els nil))
        (loop while tail
              do (let ((p (pop tail)))
                   (case (car p)
                     (49 (push (list (d (cdr p)) 256 "BYLAYER") els))
                     (62 (when els (setf (second (first els)) (cdr p))))
                     (6 (when els (setf (third (first els)) (cdr p)))))))
        (when (and (stringp name) els)
          (set-mlstyle (list :name name :flags (or (cdr (assoc 70 head)) 0) :desc (or (cdr (assoc 3 head)) "")
                             :fill (or (cdr (assoc 62 head)) 256)
                             :a1 (d (or (cdr (assoc 51 head)) 90)) :a2 (d (or (cdr (assoc 52 head)) 90))
                             :elements (nreverse els))))))))

(defun mline-verts (d) (mapcar #'pt3 (all-codes 11 d)))
(defun mline-closed-p (d) (logtest 2 (or (dxf 71 d) 0)))
(defun mline-style (d) (or (find-mlstyle (or (dxf 2 d) "STANDARD")) +standard-mlstyle+))
(defun mline-scale (d) (d (or (dxf 40 d) 1)))

(defun mline-offsets (d)
  "Each element's distance to the left of the multiline's points (in drawing units)."
  (let* ((offs (mapcar #'first (getf (mline-style d) :elements)))
         (j (case (or (dxf 70 d) 0) (0 (reduce #'max offs)) (2 (reduce #'min offs)) (t 0d0))))
    (mapcar (lambda (o) (* (mline-scale d) (- o j))) offs)))

(defun dedupe-points (ps)
  (loop for (p q) on ps when (or (null q) (> (dist p q) 1d-12)) collect p))

(defun offset-path (verts dist closed)
  "VERTS moved DIST to the left of the path, the corners mitred: one point per vertex."
  (let* ((n (length verts))
         (lines (loop for i below (if closed n (1- n))
                      collect (let* ((a (nth i verts)) (b (nth (mod (1+ i) n) verts))
                                     (o (v* (left-normal (unit3 (v- b a))) dist)))
                                (list (v+ a o) (v+ b o))))))
    (loop for i below n
          collect (let ((in (cond ((> i 0) (nth (1- i) lines)) (closed (car (last lines)))))
                        (outl (and (< i (length lines)) (nth i lines))))
                    (cond ((and in outl)
                           (let ((x (line-line-int (first in) (second in) (first outl) (second outl) nil)))
                             (if x (pt (px x) (py x) (pz (nth i verts))) (first outl))))
                          (outl (first outl))
                          (t (second in)))))))

(defun element-color (c) (and (integerp c) (/= c 256) (/= c 0) c))
(defun element-ltype (lt) (and (stringp lt) (not (member lt '("BYLAYER" "BYBLOCK") :test #'string-equal)) lt))

(defun mline-picture (d)
  (let* ((vs (dedupe-points (mline-verts d)))
         (closed (and (mline-closed-p d) (> (length vs) 2)))
         (st (mline-style d)) (els (getf st :elements)) (flags (or (getf st :flags) 0))
         (offs (mline-offsets d)) (out nil))
    (when (or (< (length vs) 2) (null els)) (return-from mline-picture nil))
    (let* ((paths (mapcar (lambda (o) (offset-path vs o closed)) offs))
           (hi (nth (position (reduce #'max offs) offs) paths))
           (lo (nth (position (reduce #'min offs) offs) paths))
           (n (length vs)))
      ;; fill: under the lines
      (when (logtest 1 flags)
        (loop for i below (if closed n (1- n))
              do (let ((j (mod (1+ i) n)))
                   (push (picture-sub (list (cons 0 "SOLID") (cons 10 (nth i hi)) (cons 11 (nth i lo))
                                            (cons 12 (nth j hi)) (cons 13 (nth j lo)))
                                      (element-color (getf st :fill)))
                         out))))
      (loop for path in paths for el in els
            do (loop for (a b) on (if closed (append path (list (first path))) path) while b
                     do (push (picture-sub (line-data a b) (element-color (second el)) (element-ltype (third el))) out)))
      ;; joints across at the corners
      (when (logtest 2 flags)
        (loop for i from (if closed 0 1) below (if closed n (1- n))
              do (push (picture-sub (line-data (nth i hi) (nth i lo))) out)))
      (unless closed
        (flet ((cap (k line-bit arc-bit suppress-bit start)
                 (unless (logtest suppress-bit (or (dxf 71 d) 0))
                   (let ((a (nth k hi)) (b (nth k lo)))
                     (when (logtest line-bit flags) (push (picture-sub (line-data a b)) out))
                     (when (and (logtest arc-bit flags) (> (dist2d a b) 1d-12))
                       (let ((c (midpt a b)) (r (/ (dist2d a b) 2)))
                         (push (picture-sub (if start (arc-data c r (ang c a) (ang c b)) (arc-data c r (ang c b) (ang c a)))) out)))))))
          (cap 0 16 64 4 t)
          (cap (1- n) 256 1024 8 nil))))
    (nreverse out)))

(defun xform-mline (d xf)
  (let ((d1 (map-codes (map-points d '(11) (xf-fn xf)) '(40) (lambda (s) (* s (xf-scale xf))))))
    (if (xf-mirror xf)
        ;; seen in a mirror the left side is the right: the points run the other way
        (let ((f (or (dxf 71 d1) 0)))
          (append (set-dxf 71 (logior (logand f (lognot 12)) (if (logtest f 4) 8 0) (if (logtest f 8) 4 0))
                           (remove 11 d1 :key (lambda (p) (and (consp p) (car p)))))
                  (mapcar (lambda (v) (cons 11 v)) (reverse (mline-verts d1)))))
        d1)))

(defun mline-grips (d) (loop for p in (mline-verts d) for i from 0 collect (cons p (list :mlv i))))
(defun mline-stretch (d id from to)
  (declare (ignore from))
  (if (and (consp id) (eq (first id) :mlv)) (set-nth-code d 11 (second id) (pt3 to)) d))

(defun just-name (k) (case k (0 "Top") (2 "Bottom") (t "Zero")))

(defun mline-length (d)
  (let ((vs (mline-verts d))) (polyline-length (if (mline-closed-p d) (append vs (list (first vs))) vs))))

(defun describe-mline (d o)
  (format o "~10TStyle: ~A,  scale: ~A,  justification: ~A~%" (or (dxf 2 d) "STANDARD") (fmt-plain (mline-scale d))
          (just-name (or (dxf 70 d) 0)))
  (format o "~10T~:[Open~;Closed~],  length: ~A~%" (mline-closed-p d) (fmt-len (mline-length d)))
  (loop for p in (mline-verts d) for i from 1
        do (format o "~10Tvertex ~D: X=~A  Y=~A  Z=~A~%" i (fmt-len (px p)) (fmt-len (py p)) (fmt-len (pz p)))))

(defun mline-palette-rows (sel)
  (flet ((v (fn) (common-value sel fn)))
    (list '("" "Multiline" "" :head)
          (list "MLSTY" "Style" (v (lambda (d) (or (dxf 2 d) "STANDARD"))) :combo (mapcar (lambda (s) (getf s :name)) (all-mlstyles)))
          (list "MLSCALE" "Scale" (v (lambda (d) (fmt-plain (mline-scale d)))) :entry)
          (list "MLJUST" "Justification" (v (lambda (d) (just-name (or (dxf 70 d) 0)))) :combo '("Top" "Zero" "Bottom"))
          (list "MLCLOSED" "Closed" (v (lambda (d) (if (mline-closed-p d) "Yes" "No"))) :combo '("Yes" "No"))
          (list "" "Length" (v (lambda (d) (fmt-num (mline-length d)))) :ro))))

(defun mline-palette-edit (d key val num)
  (cond ((and (string= key "MLSTY") (find-mlstyle val)) (set-dxf 2 (getf (find-mlstyle val) :name) d))
        ((and (string= key "MLSCALE") num (/= num 0)) (set-dxf 40 (d num) d))
        ((string= key "MLJUST") (set-dxf 70 (cond ((string-equal val "Top") 0) ((string-equal val "Bottom") 2) (t 1)) d))
        ((string= key "MLCLOSED") (let ((f (or (dxf 71 d) 1)))
                                    (set-dxf 71 (if (string-equal val "Yes") (logior f 2) (logand f (lognot 2))) d)))
        (t d)))

(defun mline-miters (vs closed)
  "Per vertex: (direction-on miter) as AutoCAD keeps them."
  (let* ((n (length vs))
         (dirs (loop for i below n
                     collect (let ((j (if (and (not closed) (= i (1- n))) (1- i) i)))
                               (unit3 (v- (nth (mod (1+ j) n) vs) (nth j vs)))))))
    (loop for i below n
          collect (let* ((dout (nth i dirs))
                         (din (cond ((> i 0) (nth (1- i) dirs)) (closed (car (last dirs))) (t dout)))
                         (m (v+ (left-normal din) (left-normal dout))))
                    (list dout (if (< (vlen3 m) 1d-9) (left-normal dout) (unit3 m)))))))

(defun write-mline-2000 (out d handle owner)
  (let* ((vs (dedupe-points (mline-verts d))) (closed (mline-closed-p d))
         (st (mline-style d)) (offs (mline-offsets d))
         (style-h (cdr (assoc (getf st :name) (fourth (assoc "ACAD_MLINESTYLE" *kind-extras* :test #'equal)) :test #'string-equal))))
    (write-common out d handle owner)
    (w out 100 "AcDbMline") (w out 2 (getf st :name)) (w out 340 (or style-h "0"))
    (w out 40 (mline-scale d)) (w out 70 (or (dxf 70 d) 0))
    (w out 71 (logior 1 (logand (or (dxf 71 d) 0) 14))) (w out 72 (length vs)) (w out 73 (length offs))
    (wp out 10 (first vs)) (wp out 210 (pt 0 0 1))
    (loop for v in vs for (dir m) in (mline-miters vs closed)
          do (wp out 11 v) (wp out 12 dir) (wp out 13 m)
             (let ((k (vdot3 m (left-normal dir))))
               (dolist (o offs)
                 (w out 74 2) (w out 41 (/ o (if (< (abs k) 1d-9) 1d0 k))) (w out 41 0d0) (w out 75 0))))))

(defun mline-from-dxf (g)
  (let* ((raw (or (raw-subclass g "AcDbMline") g))
         (data (group-coords raw)))
    (append (list (cons 0 "MLINE") (cons 2 (or (dxf 2 data) "STANDARD")) (cons 40 (d (or (dxf 40 data) 1)))
                  (cons 70 (or (dxf 70 data) 0)) (cons 71 (logand (or (dxf 71 data) 1) 14)))
            (mapcar (lambda (p) (cons 11 (pt3 p))) (all-codes 11 data))
            (dxf-props g))))

(defun write-mlstyles-2000 (out dict root names)
  (w out 0 "DICTIONARY") (w out 5 dict) (w out 330 root) (w out 100 "AcDbDictionary") (w out 281 1)
  (loop for (n . h) in names do (w out 3 n) (w out 350 h))
  (loop for (n . h) in names
        do (let ((st (find-mlstyle n)))
             (w out 0 "MLINESTYLE") (w out 5 h) (w out 102 "{ACAD_REACTORS") (w out 330 dict) (w out 102 "}")
             (w out 330 dict) (w out 100 "AcDbMlineStyle") (w out 2 (getf st :name)) (w out 70 (or (getf st :flags) 0))
             (w out 3 (or (getf st :desc) "")) (w out 62 (or (getf st :fill) 256))
             (w out 51 (d (or (getf st :a1) 90))) (w out 52 (d (or (getf st :a2) 90)))
             (w out 71 (length (getf st :elements)))
             (dolist (el (getf st :elements))
               (w out 49 (d (first el))) (w out 62 (second el)) (w out 6 (third el))))))

(defkind "MLINE" :name "Multiline" :wcs t
  :picture 'mline-picture :pieces (picture-pieces 'mline-picture) :xform 'xform-mline
  :grips 'mline-grips :stretch 'mline-stretch :describe 'describe-mline
  :palette 'mline-palette-rows :palette-edit 'mline-palette-edit
  :palette-key-p (lambda (k) (member k '("MLSTY" "MLSCALE" "MLJUST" "MLCLOSED") :test #'string=))
  :hover (lambda (d) (list "Multiline" (format nil "Style  ~A    Scale  ~A" (or (dxf 2 d) "STANDARD") (fmt-plain (mline-scale d)))
                           (format nil "Length  ~A" (fmt-len (mline-length d)))))
  ;; (exploded: its lines and arcs; the fill is left out, as in AutoCAD)
  :explode (lambda (d) (explode-picture d (remove "SOLID" (mline-picture d) :key (lambda (s) (dxf 0 s)) :test #'equal)))
  :write2000 'write-mline-2000 :write12 (lambda (d) (explode-picture d (mline-picture d)))
  :from-dxf 'mline-from-dxf
  :valid (lambda (d) (>= (length (mline-verts d)) 2)))

(defun mline-settings-line ()
  (format nil "Current settings: Justification = ~A, Scale = ~A, Style = ~A~%"
          (just-name (sysvar "CMLJUST")) (fmt-plain (sysvar "CMLSCALE")) (sysvar "CMLSTYLE")))

(defun make-mline-data (pts closed)
  (append (list (cons 0 "MLINE") (cons 2 (getf (or (find-mlstyle (sysvar "CMLSTYLE")) +standard-mlstyle+) :name))
                (cons 40 (d (sysvar "CMLSCALE"))) (cons 70 (sysvar "CMLJUST")) (cons 71 (if closed 2 0)))
          (mapcar (lambda (p) (cons 11 (pt3 p))) pts)))

(defun list-mlstyles ()
  (dolist (s (all-mlstyles))
    (out (format nil "  ~A~@[  ~A~]: elements at ~{~A~^, ~}~%" (getf s :name)
                 (let ((x (getf s :desc))) (and (plusp (length x)) x))
                 (mapcar (lambda (e) (fmt-plain (first e))) (getf s :elements))))))

(defcmd "MLINE" ("ML") "Draws multilines: several parallel lines at once, as set by a multiline style (MLSTYLE)."
  (out (mline-settings-line))
  (let ((p0 nil))
    (loop
      (let ((r (gpoint "Specify start point or [Justification/Scale/STyle]: " :keywords '("Justification" "Scale" "STyle"))))
        (cond
          ((equal r "Justification")
           (let ((k (gkw (format nil "Enter justification type [Top/Zero/Bottom] <~A>: " (string-downcase (just-name (sysvar "CMLJUST"))))
                         '("Top" "Zero" "Bottom"))))
             (when k (setf (sysvar "CMLJUST") (cond ((equal k "Top") 0) ((equal k "Bottom") 2) (t 1))))
             (out (mline-settings-line))))
          ((equal r "Scale")
           (let ((s (greal (format nil "Enter mline scale <~A>: " (fmt-plain (sysvar "CMLSCALE"))))))
             (when (and (realp s) (/= s 0)) (setf (sysvar "CMLSCALE") (d s)))
             (out (mline-settings-line))))
          ((equal r "STyle")
           (loop (let ((n (string-trim " " (gstring "Enter mline style name or [?]: "))))
                   (cond ((string= n "") (return))
                         ((string= n "?") (list-mlstyles))
                         ((find-mlstyle n) (setf (sysvar "CMLSTYLE") (getf (find-mlstyle n) :name)) (return))
                         (t (out (format nil "Cannot find mline style \"~A\" (MLSTYLE makes one).~%" n))))))
           (out (mline-settings-line)))
          ((pointp r) (setf p0 (pt3 r)) (return))
          (t (return-from-cmd)))))
    (let ((pts (list p0)) (closed nil))
      (flet ((preview (ps) (data-pieces (make-mline-data ps nil))))
        (loop (let* ((n (length pts))
                     (p (gpoint (cond ((= n 1) "Specify next point: ")
                                      ((= n 2) "Specify next point or [Undo]: ")
                                      (t "Specify next point or [Close/Undo]: "))
                                :base (car (last pts)) :keywords (if (> n 2) '("Close" "Undo") (and (> n 1) '("Undo")))
                                :drag (lambda (cur) (preview (append pts (list (pt3 cur))))))))
                (cond ((pointp p) (setf pts (append pts (list (pt3 p)))))
                      ((equal p "Undo") (if (cdr pts) (setf pts (butlast pts)) (return)))
                      ((equal p "Close") (setf closed t) (return))
                      (t (return))))))
      (when (cdr (dedupe-points pts))
        (add (make-mline-data (dedupe-points pts) (and closed (> (length pts) 2))))
        (when (gui-p) (gui-regen))))))

(defun parse-mlstyle-offsets (s)
  (let ((parts (loop with start = 0 for pos = (position #\, s :start start)
                     collect (string-trim " " (subseq s start pos)) while pos do (setf start (1+ pos)))))
    (let ((nums (mapcar (lambda (x) (ignore-errors (let ((v (read-from-string x))) (and (realp v) (d v))))) parts)))
      (and nums (every #'identity nums) (sort nums #'>)))))

(defcmd "MLSTYLE" ("-MLSTYLE") "Makes and chooses multiline styles: the lines a multiline is made of, its caps, joints and fill."
  (loop
    (let ((k (gkw (format nil "Current multiline style: ~A~%Enter an option [New/Set/List/Delete] <exit>: " (sysvar "CMLSTYLE"))
                  '("New" "Set" "List" "Delete"))))
      (cond
        ((equal k "List") (list-mlstyles))
        ((equal k "Set")
         (let ((n (string-trim " " (gstring "Enter mline style name: "))))
           (if (find-mlstyle n) (setf (sysvar "CMLSTYLE") (getf (find-mlstyle n) :name))
               (unless (string= n "") (out (format nil "Cannot find mline style \"~A\".~%" n))))))
        ((equal k "Delete")
         (let ((n (string-trim " " (gstring "Enter mline style name to delete: "))))
           (cond ((string-equal n "STANDARD") (out (format nil "STANDARD can't be deleted.~%")))
                 ((some (lambda (dd) (and (equal (dxf 0 dd) "MLINE") (string-equal (or (dxf 2 dd) "") n))) (every-drawing-object))
                  (out (format nil "~A is in use.~%" n)))
                 ((find-mlstyle n)
                  (setf (drawing-mlstyles *dwg*) (remove n (drawing-mlstyles *dwg*) :key (lambda (s) (getf s :name)) :test #'string-equal))
                  (when (string-equal (sysvar "CMLSTYLE") n) (setf (sysvar "CMLSTYLE") "STANDARD"))))))
        ((equal k "New")
         (let ((n (string-upcase (string-trim " " (gstring "Enter new style name: ")))))
           (unless (string= n "")
             (let* ((offs (loop (let ((s (gstring "Enter element offsets, separated by commas <0.5,-0.5>: ")))
                                  (if (string= (string-trim " " s) "") (return (list 0.5d0 -0.5d0))
                                      (let ((v (parse-mlstyle-offsets s))) (if v (return v) (out (format nil "Numbers, separated by commas.~%"))))))))
                    (joints (equal (gkw "Display joints? [Yes/No] <No>: " '("Yes" "No") "No") "Yes"))
                    (fill (equal (gkw "Fill? [Yes/No] <No>: " '("Yes" "No") "No") "Yes"))
                    (sc (gkw "Start cap [None/Line/Arc] <None>: " '("None" "Line" "Arc") "None"))
                    (ec (gkw "End cap [None/Line/Arc] <None>: " '("None" "Line" "Arc") "None"))
                    (fc (if fill (let ((c (gint "Fill color (1-255) <256 = ByLayer>: "))) (if (and (integerp c) (<= 1 c 255)) c 256)) 256)))
               (set-mlstyle (list :name n :flags (logior (if fill 1 0) (if joints 2 0)
                                                         (cond ((equal sc "Line") 16) ((equal sc "Arc") 64) (t 0))
                                                         (cond ((equal ec "Line") 256) ((equal ec "Arc") 1024) (t 0)))
                                  :desc "" :fill fc :a1 90d0 :a2 90d0
                                  :elements (mapcar (lambda (o) (list o 256 "BYLAYER")) offs)))
               (setf (sysvar "CMLSTYLE") n)
               (touch)
               (out (format nil "Multiline style ~A made, and current.~%" n))))))
        (t (return))))))

;;; ---------------------------------------------------------------- TOLERANCE (feature control frames)

(defun tolerance-height (d)
  (let ((vars (dimension-vars d)))
    (* (d (dv vars :dimscale)) (d (dv vars :dimtxt)) (d (or (dxf :scale d) 1)))))

(defun split-text (s sep)
  (loop with start = 0 for pos = (search sep s :start2 start)
        collect (subseq s start pos) while pos do (setf start (+ pos (length sep)))))

(defun tolerance-tokens (field)
  "A frame box's contents: a list of strings (text) and characters (GDT symbols)."
  (let ((out nil) (i 0) (buf (make-string-output-stream)))
    (flet ((flush () (let ((s (get-output-stream-string buf))) (when (plusp (length s)) (push s out)))))
      (loop while (< i (length field))
            do (let ((open (search "{\\Fgdt;" field :start2 i :test #'char-equal)))
                 (cond ((and open (= open i))
                        (let ((close (position #\} field :start i)))
                          (flush)
                          (loop for ch across (subseq field (+ i 7) (or close (length field))) do (push ch out))
                          (setf i (if close (1+ close) (length field)))))
                       (t (write-char (char field i) buf) (incf i)))))
      (flush))
    (nreverse out)))

(defun gdt-symbol (ch x y h)
  "The GDT symbol CH drawn in a cell of width 1.2 H from X, Y (its bottom left)."
  (flet ((p (u v) (pt (+ x (* u h)) (+ y (* v h))))
         (ln (a b) (picture-sub (line-data a b))))
    (let ((c (p 0.6 0.5)))
      (flet ((circ (r) (picture-sub (circle-data c (* r h))))
             (letter (s) (picture-sub (list (cons 0 "TEXT") (cons 10 (p 0.6 0.5)) (cons 11 (p 0.6 0.5)) (cons 40 (* 0.6 h))
                                            (cons 1 s) (cons 72 1) (cons 73 2))))
             (arrow (a b) (let* ((u (unit3 (v- b a))) (n (left-normal u)) (back (v- b (v* u (* 0.3 h)))))
                            (list (ln a b) (ln b (v+ back (v* n (* 0.12 h)))) (ln b (v- back (v* n (* 0.12 h))))))))
        (case (char-downcase ch)
          (#\j (list (circ 0.35) (ln (p 0.1 0.5) (p 1.1 0.5)) (ln (p 0.6 0) (p 0.6 1))))
          (#\r (list (circ 0.45) (circ 0.22)))
          (#\i (list (ln (p 0.1 0.5) (p 1.1 0.5)) (ln (p 0.3 0.8) (p 0.9 0.8)) (ln (p 0.3 0.2) (p 0.9 0.2))))
          (#\f (list (ln (p 0.2 0) (p 0.6 1)) (ln (p 0.6 0) (p 1.0 1))))
          (#\b (list (ln (p 0.1 0) (p 1.1 0)) (ln (p 0.6 0) (p 0.6 1))))
          (#\a (list (ln (p 0.1 0) (p 1.1 0)) (ln (p 0.1 0) (p 1.0 0.8))))
          (#\g (list (circ 0.3) (ln (p 0.05 0) (p 0.55 1)) (ln (p 0.65 0) (p 1.15 1))))
          (#\c (list (ln (p 0.1 0.2) (p 0.8 0.2)) (ln (p 0.8 0.2) (p 1.1 0.8)) (ln (p 1.1 0.8) (p 0.4 0.8)) (ln (p 0.4 0.8) (p 0.1 0.2))))
          (#\e (list (circ 0.45)))
          (#\u (list (ln (p 0.1 0.5) (p 1.1 0.5))))
          (#\d (list (picture-sub (arc-data (p 0.6 0.2) (* 0.45 h) 0d0 +pi+)) (ln (p 0.15 0.2) (p 1.05 0.2))))
          (#\k (list (picture-sub (arc-data (p 0.6 0.2) (* 0.45 h) 0d0 +pi+))))
          (#\h (arrow (p 0.3 0) (p 0.9 1)))
          (#\t (append (arrow (p 0.15 0.1) (p 0.55 1)) (arrow (p 0.65 0.1) (p 1.05 1)) (list (ln (p 0.15 0.1) (p 0.65 0.1)))))
          (#\n (list (circ 0.38) (ln (p 0.15 0) (p 1.05 1))))
          ((#\m #\l #\s #\p) (list (circ 0.5) (letter (string (char-upcase ch)))))
          (t (list (letter (string ch)))))))))

(defun text-width (s h)
  (let* ((td (list (cons 0 "TEXT") (cons 10 (pt 0 0)) (cons 40 h) (cons 1 s)))
         (b (ignore-errors (text-box td))))
    (if b (- (reduce #'max b :key #'px) (reduce #'min b :key #'px)) (* 0.8 h (length s)))))

(defun tolerance-picture-flat (str h)
  "The frame's boxes, symbols and text with its insertion point (the middle of the first row's left
side) at 0,0 and its rows along +X."
  (let* ((rowh (* 2 h)) (cell (* 1.2 h)) (out nil))
    (loop for row in (split-text str "^J") for i from 0
          do (let* ((top (- (/ rowh 2) (* i rowh))) (bot (- top rowh)) (base (+ bot (/ (- rowh h) 2))) (x 0d0))
               (dolist (field (split-text row "%%v"))
                 (let ((toks (tolerance-tokens (string-trim " " field))))
                   (when toks
                     (let* ((ws (mapcar (lambda (tk) (if (characterp tk) cell (text-width tk h))) toks))
                            (wd (+ (reduce #'+ ws) h)) (cx (+ x (/ h 2))))
                       (loop for tk in toks for tw in ws
                             do (if (characterp tk)
                                    (setf out (append (reverse (gdt-symbol tk cx base h)) out))
                                    (push (picture-sub (list (cons 0 "TEXT") (cons 10 (pt cx base)) (cons 40 h) (cons 1 tk))) out))
                                (incf cx tw))
                       (let ((a (pt x bot)) (b (pt (+ x wd) bot)) (c (pt (+ x wd) top)) (e (pt x top)))
                         (dolist (s (list (list a b) (list b c) (list c e) (list e a)))
                           (push (picture-sub (line-data (first s) (second s))) out)))
                       (incf x wd)))))))
    (nreverse out)))

(defun tolerance-dir (d)
  (let ((v (pt3 (or (dxf 11 d) (pt 1 0 0))))) (if (< (vlen3 v) 1d-12) (pt 1 0 0) (unit3 v))))

(defun tolerance-picture (d)
  (let* ((u (tolerance-dir d)) (a (atan (py u) (px u))) (ins (pt3 (or (dxf 10 d) (pt 0 0))))
         (xf (xf-compose (xf-rotate (pt 0 0) a) (xf-move ins))))
    (mapcar (lambda (s) (xform-data s xf)) (tolerance-picture-flat (or (dxf 1 d) "") (tolerance-height d)))))

(defun xform-tolerance (d xf)
  (let* ((fp (xf-fn xf)) (b (pt3 (or (dxf 10 d) (pt 0 0)))) (nb (funcall fp b))
         (nu (v- (funcall fp (v+ b (tolerance-dir d))) nb))
         (s (vlen3 nu)))
    (let ((d1 (set-dxf 11 (if (< s 1d-12) (tolerance-dir d) (unit3 nu)) (set-dxf 10 nb d))))
      (if (> (abs (- s 1)) 1d-9) (set-dxf :scale (* (d (or (dxf :scale d) 1)) s) d1) d1))))

(defun tolerance-plain-text (s)
  "The frame's contents in words: symbols by name, boxes split by |."
  (let ((names '((#\j . "position") (#\r . "concentricity") (#\i . "symmetry") (#\f . "parallelism")
                 (#\b . "perpendicularity") (#\a . "angularity") (#\g . "cylindricity") (#\c . "flatness")
                 (#\e . "circularity") (#\u . "straightness") (#\d . "surface profile") (#\k . "line profile")
                 (#\h . "circular runout") (#\t . "total runout") (#\n . "%%c") (#\m . "(M)") (#\l . "(L)")
                 (#\s . "(S)") (#\p . "(P)"))))
    (format nil "~{~A~^ / ~}"
            (loop for row in (split-text s "^J")
                  collect (format nil "~{~A~^ | ~}"
                                  (loop for f in (split-text row "%%v")
                                        for toks = (tolerance-tokens (string-trim " " f))
                                        when toks collect (format nil "~{~A~}" (mapcar (lambda (tk) (if (characterp tk) (or (cdr (assoc (char-downcase tk) names)) tk) tk)) toks))))))))

(defun describe-tolerance (d o)
  (let ((p (pt3 (or (dxf 10 d) (pt 0 0)))))
    (format o "~10TInsertion point: X=~A  Y=~A  Z=~A~%" (fmt-len (px p)) (fmt-len (py p)) (fmt-len (pz p)))
    (format o "~10TDimension style: \"~A\",  text height: ~A~%" (or (dxf 3 d) "Standard") (fmt-len (tolerance-height d)))
    (format o "~10TContents: ~A~%" (tolerance-plain-text (or (dxf 1 d) "")))))

(defun write-tolerance-2000 (out d handle owner)
  (write-common out d handle owner)
  (w out 100 "AcDbFcf") (w out 3 (if (find-dimstyle (dxf 3 d)) (dimstyle-name (find-dimstyle (dxf 3 d))) "Standard"))
  (wp out 10 (pt3 (or (dxf 10 d) (pt 0 0)))) (w out 1 (or (dxf 1 d) "")) (wp out 11 (tolerance-dir d))
  ;; a size of its own: as AutoCAD keeps it, an override of the dimension scale
  (let ((s (d (or (dxf :scale d) 1))))
    (when (> (abs (- s 1)) 1d-12)
      (w out 1001 "ACAD") (w out 1000 "DSTYLE") (w out 1002 "{") (w out 1070 40)
      (w out 1040 (* s (d (dv (dimension-vars (rem-dxf :scale d)) :dimscale)))) (w out 1002 "}"))))

(defun dstyle-override (g code)
  "A dimension variable (by its DXF CODE) overridden in G's xdata, or NIL."
  (let ((x (member-if (lambda (p) (and (consp p) (eql (car p) 1000) (equal (cdr p) "DSTYLE"))) g)))
    (loop for (a b) on x
          when (and (consp a) (eql (car a) 1070) (eql (cdr a) code) (consp b)) return (cdr b))))

(defun tolerance-from-dxf (g)
  (let* ((raw (or (raw-subclass g "AcDbFcf") g))
         (data (group-coords (remove-if (lambda (p) (and (consp p) (>= (car p) 1000))) raw)))
         (d0 (append (list (cons 0 "TOLERANCE") (cons 10 (pt3 (or (dxf 10 data) (pt 0 0))))
                           (cons 11 (let ((v (pt3 (or (dxf 11 data) (pt 1 0 0))))) (if (< (vlen3 v) 1d-12) (pt 1 0 0) (unit3 v))))
                           (cons 1 (or (dxf 1 data) "")) (cons 3 (or (dxf 3 data) "Standard")))
                     (dxf-props g)))
         (ds (dstyle-override g 40)))
    (if (realp ds)
        (let ((base (d (dv (dimension-vars d0) :dimscale))))
          (if (> base 0) (append d0 (list (cons :scale (/ (d ds) base)))) d0))
        d0)))

(defkind "TOLERANCE" :name "Tolerance" :wcs t
  :picture 'tolerance-picture :pieces (picture-pieces 'tolerance-picture) :xform 'xform-tolerance
  :grips (lambda (d) (list (cons (pt3 (or (dxf 10 d) (pt 0 0))) :move)))
  :describe 'describe-tolerance
  :palette (lambda (sel) (list '("" "Tolerance" "" :head)
                               (list "TOLTEXT" "Contents" (common-value sel (lambda (d) (or (dxf 1 d) ""))) :entry)
                               (list "DIMSTY" "Dim style" (common-value sel (lambda (d) (or (dxf 3 d) "Standard")))
                                     :combo (mapcar #'dimstyle-name (all-dimstyles)))))
  :palette-key-p (lambda (k) (string= k "TOLTEXT"))
  :palette-edit (lambda (d key val num) (declare (ignore key num)) (set-dxf 1 val d))
  :hover (lambda (d) (list "Tolerance" (let ((s (tolerance-plain-text (or (dxf 1 d) ""))))
                                         (if (> (length s) 50) (concatenate 'string (subseq s 0 50) "...") s))))
  :explode (lambda (d) (explode-picture d (tolerance-picture d)))
  :write2000 'write-tolerance-2000 :write12 (lambda (d) (explode-picture d (tolerance-picture d)))
  :from-dxf 'tolerance-from-dxf
  :valid (lambda (d) (and (consp (dxf 10 d)) (stringp (dxf 1 d)))))

(defparameter +gdt-characteristics+
  '(("POsition" . #\j) ("COncentricity" . #\r) ("SYmmetry" . #\i) ("PArallelism" . #\f)
    ("PErpendicularity" . #\b) ("Angularity" . #\a) ("CYlindricity" . #\g) ("Flatness" . #\c)
    ("CIrcularity" . #\e) ("STraightness" . #\u) ("SUrfaceprofile" . #\d) ("Lineprofile" . #\k)
    ("Runout" . #\h) ("Totalrunout" . #\t)))

(defun gdt (ch) (format nil "{\\Fgdt;~C}" ch))

(defun tolerance-field (s datum)
  "Typed tolerance (\"%%c0.05 M\") or datum (\"A M\") text in frame form: a diameter sign and a
material condition (M, L, S) become symbols."
  (let* ((s (string-trim " " s)) (dia nil) (cond-ch nil))
    (when (and (not datum) (> (length s) 2) (string-equal (subseq s 0 3) "%%c")) (setf dia t s (subseq s 3)))
    (when (and (not datum) (plusp (length s)) (char= (char s 0) (code-char 216))) (setf dia t s (subseq s 1)))
    (let ((s (string-trim " " s)))
      (when (and (> (length s) 1) (member (char-upcase (char s (1- (length s)))) '(#\M #\L #\S))
                 (let ((prev (char s (- (length s) 2)))) (or (char= prev #\Space) (digit-char-p prev) (char= prev #\.) datum)))
        (setf cond-ch (char-downcase (char s (1- (length s)))) s (string-trim " " (subseq s 0 (1- (length s))))))
      (format nil "~A~A~@[~A~]" (if dia (gdt #\n) "") s (and cond-ch (gdt cond-ch))))))

(defcmd "TOLERANCE" ("TOL") "Places a geometric tolerance (feature control frame)."
  (let* ((names (mapcar #'car +gdt-characteristics+))
         (k (gkw (format nil "Enter geometric characteristic [~{~A~^/~}/None] <POsition>: " names) (append names '("None")) "POsition"))
         (sym (cdr (assoc k +gdt-characteristics+ :test #'string=)))
         (t1 (tolerance-field (gstring "Enter tolerance 1 (start with %%c for a diameter, end with M, L or S): " t) nil))
         (t2 (tolerance-field (gstring "Enter tolerance 2 <none>: " t) nil))
         (d1 (tolerance-field (gstring "Enter datum 1 <none>: " t) t))
         (d2 (if (plusp (length d1)) (tolerance-field (gstring "Enter datum 2 <none>: " t) t) ""))
         (d3 (if (plusp (length d2)) (tolerance-field (gstring "Enter datum 3 <none>: " t) t) ""))
         (str (format nil "~@[~A~]%%v~A%%v~A%%v~A%%v~A%%v~A" (and sym (gdt sym)) t1 t2 d1 d2 d3)))
    (unless (or sym (notevery (lambda (x) (string= x "")) (list t1 t2 d1)))
      (out (format nil "Nothing to put in the frame.~%")))
    (when (or sym (notevery (lambda (x) (string= x "")) (list t1 t2 d1)))
      (let* ((base (list (cons 0 "TOLERANCE") (cons 10 (pt 0 0)) (cons 11 (pt 1 0 0)) (cons 1 str)
                         (cons 3 (sysvar "DIMSTYLE")) (cons :dimvars (current-dimvars))))
             (p (gpoint "Enter tolerance location: "
                        :drag (lambda (cur) (data-pieces (set-dxf 10 (pt3 cur) base))))))
        (when (pointp p)
          (add (set-dxf 10 (pt3 p) base))
          (when (gui-p) (gui-regen)))))))

;;; ---------------------------------------------------------------- DIMARC (arc length dimensions)

(defun arcdim-geometry (c r a1 a2 loc override vars)
  "An arc length dimension of the arc of radius R about C from A1 counterclockwise to A2, its
dimension arc through LOC: (values entities length text-point)."
  (let* ((sc (d (dv vars :dimscale))) (gap (* sc (d (dv vars :dimgap))))
         (exo (* sc (d (dv vars :dimexo)))) (exe (* sc (d (dv vars :dimexe))))
         (asz (* sc (d (dv vars :dimasz)))) (h (* sc (d (dv vars :dimtxt))))
         (sw (arc-sweep a1 a2)) (meas (* r sw))
         (rr (max 1d-9 (dist2d c loc)))
         (s (dim-text-string meas vars override))
         (ents nil))
    (push (colored (arc-data c rr a1 a2) (dv vars :dimclrd)) ents)
    (setf ents (append (dim-arrow (polar c a1 rr) (polar c (+ a1 (min 0.5d0 (/ asz rr))) rr) vars)
                       (dim-arrow (polar c a2 rr) (polar c (- a2 (min 0.5d0 (/ asz rr))) rr) vars)
                       ents))
    ;; extension lines out from the arc's ends to the dimension arc
    (when (> (abs (- rr r)) exo)
      (let ((sg (if (> rr r) 1 -1)))
        (dolist (a (list a1 a2))
          (push (colored (line-data (polar c a (+ r (* sg exo))) (polar c a (+ rr (* sg exe)))) (dv vars :dimclre)) ents))))
    (let* ((mid (+ a1 (/ sw 2))) (tp (polar c mid (+ rr gap))) (tr (readable-angle (- mid (/ +pi+ 2))))
           (tw (text-width (text-string-display s) h)) (sym (or (sysvar "DIMARCSYM") 0)))
      (push (dim-text-entity tp tr s vars 8) ents)
      ;; the arc length symbol: before the number (DIMARCSYM 0) or over it (1)
      (unless (eql sym 2)
        (let* ((up (pt (cos (+ tr (/ +pi+ 2))) (sin (+ tr (/ +pi+ 2)))))
               (along (pt (cos tr) (sin tr)))
               (cc (if (eql sym 1)
                       (v+ tp (v* up (* 1.3 h)))
                       (v+ (v- tp (v* along (+ (/ tw 2) (* 0.7 h)))) (v* up (* 0.25 h))))))
          (push (colored (arc-data cc (* 0.55 h) (+ tr 0.35d0) (+ tr +pi+ -0.35d0)) (dv vars :dimclrt)) ents)))
      (values (nreverse ents) meas tp))))

(defun arcdim-entities (data vars)
  "The picture of arc length dimension DATA: (values entities length)."
  (when (and (dxf 15 data) (dxf 13 data) (dxf 14 data) (dxf 10 data))
    (let* ((c (pt3 (dxf 15 data))) (p1 (pt3 (dxf 13 data))) (p2 (pt3 (dxf 14 data))))
      (multiple-value-bind (es meas) (arcdim-geometry c (dist2d c p1) (ang c p1) (ang c p2) (pt3 (dxf 10 data)) (dxf 1 data) vars)
        (values es meas)))))

(defun write-arcdim-body (out data)
  (let* ((c (pt3 (or (dxf 15 data) (pt 0 0)))) (p1 (pt3 (or (dxf 13 data) c))) (p2 (pt3 (or (dxf 14 data) c))))
    (w out 100 "AcDbDimension") (w out 2 (or (dxf 2 data) ""))
    (wp out 10 (pt3 (or (dxf 10 data) (pt 0 0)))) (wp out 11 (pt3 (or (dxf 11 data) (pt 0 0))))
    (w out 70 (logior 32 5 (logand (or (dxf 70 data) 0) 128))) (w out 1 (or (dxf 1 data) ""))
    (when (realp (dxf 42 data)) (w out 42 (d (dxf 42 data))))
    (w out 3 (if (find-dimstyle (dxf 3 data)) (dimstyle-name (find-dimstyle (dxf 3 data))) "Standard"))
    (w out 100 "AcDbArcDimension")
    (wp out 13 p1) (wp out 14 p2) (wp out 15 c)
    (w out 70 0) (w out 40 (ang c p1)) (w out 41 (ang c p2)) (w out 71 0)
    (wp out 16 p1) (wp out 17 p2)))

(defun arcdim-from-dxf (g)
  "ARC_DIMENSION: a dimension (its picture block as saved) marked as an arc length one."
  (let* ((arc (group-coords (or (raw-subclass g "AcDbArcDimension") nil)))
         (dimpart (subseq g 0 (or (position-if (lambda (p) (and (consp p) (eql (car p) 100) (equal (cdr p) "AcDbArcDimension"))) g)
                                  (length g))))
         (data (convert-in (cons (cons 0 "DIMENSION") (rest dimpart)))))
    (append (rem-dxf 13 (rem-dxf 14 (rem-dxf 15 data)))
            (list (cons 13 (pt3 (or (dxf 13 arc) (pt 0 0)))) (cons 14 (pt3 (or (dxf 14 arc) (pt 0 0))))
                  (cons 15 (pt3 (or (dxf 15 arc) (pt 0 0)))) (cons :arclen t)))))

(defkind "ARC_DIMENSION" :from-dxf 'arcdim-from-dxf)

(defun pick-arc-piece (e at)
  "(c r a1 a2) of the arc, or polyline arc segment, of E nearest AT."
  (let ((best nil) (bd nil))
    (dolist (pc (entity-pieces e))
      (when (eq (first pc) :arc)
        (destructuring-bind (c r a1 a2) (rest pc)
          (let ((dd (abs (- (dist2d c at) r))))
            (when (or (null bd) (< dd bd)) (setf best (list (pt3 c) r a1 a2) bd dd))))))
    best))

(defcmd "DIMARC" ("DAR") "Creates an arc length dimension: the length along an arc or a polyline arc segment."
  (let* ((sel (get-user-input :entity :prompt "Select arc or polyline arc segment: "))
         (e (and sel (sel-entity sel)))
         (arc (and e (pick-arc-piece e (pt3 (sel-point sel))))))
    (cond
      ((null sel) nil)
      ((null arc) (out (format nil "Object selected is not an arc or polyline arc segment.~%")))
      (t
       (destructuring-bind (c r a1 a2) arc
         (let ((vars (current-dimvars)) (text nil))
           (loop
             (let ((loc (gpoint "Specify arc length dimension location, or [Text/Partial]: " :keywords '("Text" "Partial")
                                :drag (lambda (cur) (mapcar (lambda (x) (list :data x)) (arcdim-geometry c r a1 a2 cur text vars))))))
               (cond
                 ((equal loc "Text") (setf text (gstring "Enter dimension text <>: " t)))
                 ((equal loc "Partial")
                  (let* ((q1 (gpoint "Specify first point for arc length dimension: "))
                         (q2 (and (pointp q1) (gpoint "Specify second point for arc length dimension: "))))
                    (when (pointp q2)
                      (let ((b1 (ang c q1)) (b2 (ang c q2)))
                        (if (<= (norm-ang (- b1 a1)) (norm-ang (- b2 a1))) (setf a1 b1 a2 b2) (setf a1 b2 a2 b1))))))
                 ((pointp loc)
                  (multiple-value-bind (ents meas tp) (arcdim-geometry c r a1 a2 loc text vars)
                    (declare (ignore ents))
                    (add (append (dim-data 5 (cons 15 c) (cons 13 (polar c a1 r)) (cons 14 (polar c a2 r)) (cons 10 (pt3 loc))
                                           (cons 11 tp) (cons 42 meas) (cons :arclen t))
                                 (and text (list (cons 1 text)))))
                    (out (format nil "Dimension text = ~A~%" (dim-text-string meas vars nil))))
                  (return))
                 (t (return)))))))))
    (when (gui-p) (gui-regen))))

;;; ---------------------------------------------------------------- OLE2FRAME (embedded OLE objects)

(defun hex-octets (s)
  (let* ((n (floor (length s) 2)) (v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v) (setf (aref v i) (parse-integer s :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))
(defun octets-hex (v)
  (with-output-to-string (o) (loop for b across v do (format o "~2,'0X" b))))

(defun u16 (v i) (if (<= (+ i 2) (length v)) (logior (aref v i) (ash (aref v (1+ i)) 8)) 0))
(defun u32 (v i) (if (<= (+ i 4) (length v)) (logior (u16 v i) (ash (u16 v (+ i 2)) 16)) 0))
(defun s32 (v i) (let ((x (u32 v i))) (if (>= x #x80000000) (- x #x100000000) x)))

(defun cfb-streams (v)
  "The streams of a compound file (the container of OLE objects) in V: ((name . octets) ...)."
  (unless (and (> (length v) 512) (= (u32 v 0) #xE011CFD0) (= (u32 v 4) #xE11AB1A1))
    (return-from cfb-streams nil))
  (let* ((ss (ash 1 (u16 v 30))) (mss (ash 1 (u16 v 32)))
         (nfat (u32 v 44)) (dirstart (u32 v 48)) (cutoff (u32 v 56)) (minifat-start (u32 v 60))
         (difat (loop for i below (min nfat 109) collect (u32 v (+ 76 (* 4 i)))))
         (fat (make-array 0 :adjustable t :fill-pointer 0)))
    (labels ((sec (n) (+ 512 (* n ss)))
             (chain (s &optional (table fat))
               (loop with seen = 0 while (and (< s #xFFFFFFFA) (< s (length table)) (< (incf seen) 1000000))
                     collect s do (setf s (aref table s))))
             (read-chain (s size)
               (let ((out (make-array size :element-type '(unsigned-byte 8))) (pos 0))
                 (dolist (k (chain s) out)
                   (let ((n (min ss (- size pos) (max 0 (- (length v) (sec k))))))
                     (when (plusp n) (replace out v :start1 pos :start2 (sec k) :end2 (+ (sec k) n)) (incf pos n)))))))
      ;; the extra DIFAT sectors of big files
      (let ((x (u32 v 68)) (left (- nfat 109)))
        (loop while (and (plusp left) (< x #xFFFFFFFA))
              do (loop for i below (1- (/ ss 4)) while (plusp left)
                       do (push (u32 v (+ (sec x) (* 4 i))) (cdr (last difat))) (decf left))
                 (setf x (u32 v (+ (sec x) (- ss 4))))))
      (dolist (s difat)
        (loop for i below (/ ss 4) do (vector-push-extend (u32 v (+ (sec s) (* 4 i))) fat)))
      (let* ((dir-chain (chain dirstart))
             (entries (loop for s in dir-chain
                            append (loop for k below (/ ss 128) collect (+ (sec s) (* k 128)))))
             (root (first entries))
             (ministream (and root (read-chain (u32 v (+ root 116)) (u32 v (+ root 120)))))
             (minifat (let ((m (make-array 0 :adjustable t :fill-pointer 0)))
                        (dolist (s (chain minifat-start) m)
                          (loop for i below (/ ss 4) do (vector-push-extend (u32 v (+ (sec s) (* 4 i))) m))))))
        (loop for e in entries
              when (and (< (+ e 128) (length v)) (= (aref v (+ e 66)) 2))
                collect (let* ((nl (u16 v (+ e 64)))
                               (name (map 'string (lambda (k) (code-char (u16 v (+ e (* 2 k))))) (loop for k below (max 0 (1- (floor nl 2))) collect k)))
                               (start (u32 v (+ e 116))) (size (u32 v (+ e 120))))
                          (cons name
                                (if (< size cutoff)
                                    (let ((out (make-array size :element-type '(unsigned-byte 8))) (pos 0))
                                      (dolist (k (chain start minifat) out)
                                        (let ((n (min mss (- size pos))))
                                          (when (and (plusp n) ministream (<= (+ (* k mss) n) (length ministream)))
                                            (replace out ministream :start1 pos :start2 (* k mss) :end2 (+ (* k mss) n)))
                                          (incf pos (max n 0)))))
                                    (read-chain start size)))))))))

(defun ole-octets (d) (let ((h (dxf :ole d))) (and (stringp h) (hex-octets h))))
(defun ole-cfb (v)
  "Where the compound file starts in an OLE frame's data (after AutoCAD's own header)."
  (and v (search #(#xD0 #xCF #x11 #xE0 #xA1 #xB1 #x1A #xE1) v)))

(defun ole-contents (d)
  "((name . octets) ...) of OLE object D's streams."
  (let* ((v (ole-octets d)) (k (ole-cfb v)))
    (and k (ignore-errors (cfb-streams (subseq v k))))))

(defun ansi-at (v i)
  "A length-prefixed string at I in V: (values string next)."
  (let ((n (u32 v i)))
    (if (or (zerop n) (> n 4096) (> (+ i 4 n) (length v)))
        (values "" (+ i 4))
        (values (map 'string #'code-char (subseq v (+ i 4) (+ i 4 (max 0 (1- n))))) (+ i 4 n)))))

(defun compobj-info (streams)
  "(values user-type prog-id) from the CompObj stream."
  (let ((v (cdr (find (format nil "~CCompObj" (code-char 1)) streams :key #'car :test #'string=))))
    (if (null v) (values nil nil)
        (multiple-value-bind (user i) (ansi-at v 28)
          (let* ((m (u32 v i))
                 (j (cond ((zerop m) (+ i 4)) ((>= m #xFFFFFFFE) (+ i 8)) (t (+ i 4 m)))))
            (values (and (plusp (length user)) user)
                    (let ((p (ignore-errors (ansi-at v j)))) (and p (plusp (length p)) (< (length p) 80) p))))))))

(defun ole-kind-name (d)
  (or (ignore-errors (compobj-info (ole-contents d)))
      (let ((s (dxf 3 d))) (and (stringp s) (plusp (length s)) (not (string-equal s "OLE")) s))
      "OLE object"))

;;; a picture of it: the Paintbrush bitmap, or the DIB the object keeps for showing itself

(defun dib-rgba (v off)
  "A device-independent bitmap at OFF in V: (values width height rgba) or NIL."
  (let* ((hs (u32 v off)) (w (s32 v (+ off 4))) (h0 (s32 v (+ off 8))) (h (abs h0))
         (bpp (u16 v (+ off 14))) (comp (u32 v (+ off 16))) (used (u32 v (+ off 32))))
    (when (and (<= 12 hs 124) (< 0 w 20000) (< 0 h 20000) (member bpp '(1 4 8 24 32)) (member comp '(0 3)))
      (let* ((ncol (if (<= bpp 8) (if (plusp used) used (ash 1 bpp)) 0))
             (pal (+ off hs (if (= comp 3) 12 0)))
             (pix (+ pal (* 4 ncol)))
             (stride (* 4 (ceiling (* w bpp) 32)))
             (rgba (make-array (* w h 4) :element-type '(unsigned-byte 8) :initial-element 255)))
        (when (> (+ pix (* stride h)) (length v)) (return-from dib-rgba nil))
        (dotimes (y h)
          (let ((row (+ pix (* stride (if (minusp h0) y (- h 1 y))))))
            (dotimes (x w)
              (let ((o (* 4 (+ x (* y w)))))
                (flet ((put (b g r) (setf (aref rgba o) r (aref rgba (+ o 1)) g (aref rgba (+ o 2)) b)))
                  (case bpp
                    ((24 32) (let ((q (+ row (* x (/ bpp 8))))) (put (aref v q) (aref v (+ q 1)) (aref v (+ q 2)))))
                    (t (let* ((bit (* x bpp)) (byte (aref v (+ row (floor bit 8))))
                              (idx (ldb (byte bpp (- 8 bpp (mod bit 8))) byte))
                              (q (+ pal (* 4 (min idx (max 0 (1- ncol)))))))
                         (put (aref v q) (aref v (+ q 1)) (aref v (+ q 2)))))))))))
        (values w h rgba)))))

(defun ole-preview-png (d)
  "A PNG of OLE object D's picture (made once, in the picture cache), or NIL."
  (let* ((hex (dxf :ole d))
         (path (and hex (format nil "~Aole-~36R.png" (image-cache-dir) (logand (sxhash hex) #xFFFFFFFFFF)))))
    (cond ((null path) nil)
          ((probe-file path) path)
          (t (let* ((streams (ole-contents d))
                    (native (cdr (find (format nil "~COle10Native" (code-char 1)) streams :key #'car :test #'string=)))
                    (pres (cdr (find-if (lambda (s) (search "OlePres" (car s))) streams))))
               (destructuring-bind (&optional w h rgba)
                   (or (and native (> (length native) 20) (= (aref native 4) 66) (= (aref native 5) 77)   ; "BM": a bitmap file
                            (multiple-value-list (dib-rgba native (+ 4 14))))
                       (and pres (> (length pres) 40) (>= (u32 pres 0) #xFFFFFFFE) (= (u32 pres 4) 8)  ; CF_DIB
                            (let ((td (u32 pres 8))) (multiple-value-list (dib-rgba pres (+ 8 (max 4 td) 4 4 4 4 4 4 4))))))
                 (when (and w rgba (ignore-errors (write-png-rgba path w h rgba) t))
                   path)))))))

(defun ole-corners (d)
  "(upper-left lower-right)."
  (list (pt3 (or (dxf 10 d) (pt 0 1))) (pt3 (or (dxf 11 d) (pt 1 0)))))

(defun ole-picture (d)
  (destructuring-bind (ul lr) (ole-corners d)
    (let* ((ll (pt (px ul) (py lr) (pz ul))) (wd (- (px lr) (px ul))) (ht (- (py ul) (py lr)))
           (png (ignore-errors (ole-preview-png d)))
           (frame (let ((ps (list ll (pt (px lr) (py lr) (pz ul)) (pt (px lr) (py ul) (pz ul)) ul)))
                    (loop for (a b) on (append ps (list (first ps))) while b collect (picture-sub (line-data a b))))))
      (if (and png (> (abs wd) 1d-12) (> (abs ht) 1d-12) (fboundp 'image-pixel-size))
          (destructuring-bind (&optional pw ph) (ignore-errors (image-pixel-size png))
            (if (and pw ph)
                (list (picture-sub (append (list (cons 0 "IMAGE") (cons 10 ll) (cons 11 (pt (/ wd pw) 0 0)) (cons 12 (pt 0 (/ ht ph) 0))
                                                 (cons 13 (pt pw ph 0)) (cons :file png)))))
                frame))
          (append frame
                  (let ((h (* 0.12 (min (abs wd) (abs ht)))))
                    (when (> h 1d-9)
                      (list (picture-sub (list (cons 0 "TEXT") (cons 10 (midpt ul lr)) (cons 11 (midpt ul lr)) (cons 40 h)
                                               (cons 1 (ole-kind-name d)) (cons 72 1) (cons 73 2)))))))))))

(defun ole-pieces (d)
  (destructuring-bind (ul lr) (ole-corners d)
    (let ((ps (list (pt (px ul) (py lr) (pz ul)) lr (pt (px lr) (py ul) (pz ul)) ul)))
      (loop for (a b) on (append ps (list (first ps))) while b collect (list :seg a b)))))

(defun xform-ole (d xf)
  "An OLE object stays square to the page: it moves and is resized (by its box's corners)."
  (let* ((ps (mapcar (xf-fn xf) (mapcar #'second (ole-pieces d))))
         (x0 (reduce #'min ps :key #'px)) (x1 (reduce #'max ps :key #'px))
         (y0 (reduce #'min ps :key #'py)) (y1 (reduce #'max ps :key #'py)) (z (pz (first ps))))
    (set-dxf 11 (pt x1 y0 z) (set-dxf 10 (pt x0 y1 z) d))))

(defun ole-grips (d)
  (destructuring-bind (ul lr) (ole-corners d)
    (list (cons (midpt ul lr) :move) (cons ul :oleul) (cons lr :olelr))))
(defun ole-stretch (d id from to)
  (declare (ignore from))
  (destructuring-bind (ul lr) (ole-corners d)
    (let ((a (if (eq id :oleul) (pt3 to) ul)) (b (if (eq id :olelr) (pt3 to) lr)))
      (set-dxf 11 (pt (max (px a) (px b)) (min (py a) (py b)) (pz ul))
               (set-dxf 10 (pt (min (px a) (px b)) (max (py a) (py b)) (pz ul)) d)))))

(defun describe-ole (d o)
  (destructuring-bind (ul lr) (ole-corners d)
    (format o "~10TObject: ~A~%" (ole-kind-name d))
    (format o "~10TUpper left: X=~A  Y=~A,  lower right: X=~A  Y=~A~%" (fmt-len (px ul)) (fmt-len (py ul)) (fmt-len (px lr)) (fmt-len (py lr)))
    (format o "~10TType: ~A,  data: ~:D bytes~%" (case (dxf 71 d) (1 "linked") (3 "static") (t "embedded"))
            (floor (length (or (dxf :ole d) "")) 2))
    (format o "~10T(OLEOPEN opens it in the program it came from.)~%")))

(defun ole-data-for-saving (d)
  "The object's data with AutoCAD's header updated to where its box is now."
  (let* ((v (ole-octets d)))
    (when (and v (> (length v) 98) (= (u16 v 0) #x5580))
      (destructuring-bind (ul lr) (ole-corners d)
        (let ((corners (list ul (pt (px lr) (py ul) (pz ul)) lr (pt (px ul) (py lr) (pz ul)))))
          (loop for p in corners for k from 0
                do (loop for x in (list (px p) (py p) (pz p)) for j from 0
                         do (let ((bits (ldb (byte 64 0) (double-bits (d x)))) (at (+ 2 (* 8 (+ (* 3 k) j)))))
                              (dotimes (b 8) (setf (aref v (+ at b)) (ldb (byte 8 (* 8 b)) bits)))))))))
    v))

(defun double-bits (x)
  #+sbcl (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits x)) 32) (sb-kernel:double-float-low-bits x))
  #-sbcl (multiple-value-bind (m e s) (integer-decode-float x)
           (if (zerop m) 0 (logior (if (minusp s) (ash 1 63) 0) (ash (+ e 1075) 52) (ldb (byte 52 0) m)))))

(defun write-ole-2000 (out d handle owner)
  (let ((v (ole-data-for-saving d)))
    (destructuring-bind (ul lr) (ole-corners d)
      (write-common out d handle owner)
      (w out 100 "AcDbOle2Frame") (w out 70 (or (dxf 70 d) 2)) (w out 3 (or (dxf 3 d) "OLE"))
      (wp out 10 ul) (wp out 11 lr) (w out 71 (or (dxf 71 d) 2)) (w out 72 (or (dxf 72 d) 0))
      (w out 90 (if v (length v) 0))
      (when v (dolist (c (split-chunks (octets-hex v) 254)) (w out 310 c)))
      (w out 1 "OLE"))))

(defun ole-from-dxf (g)
  (let* ((raw (or (raw-subclass g "AcDbOle2Frame") g))
         (data (group-coords (remove-if (lambda (p) (and (consp p) (or (eql (car p) 310) (>= (car p) 1000)))) raw)))
         (hex (apply #'concatenate 'string (all-codes 310 raw))))
    (append (list (cons 0 "OLE2FRAME") (cons 70 (or (dxf 70 data) 2)) (cons 3 (or (dxf 3 data) "OLE"))
                  (cons 10 (pt3 (or (dxf 10 data) (pt 0 1)))) (cons 11 (pt3 (or (dxf 11 data) (pt 1 0))))
                  (cons 71 (or (dxf 71 data) 2)) (cons 72 (or (dxf 72 data) 0)) (cons :ole hex))
            (dxf-props g))))

(defkind "OLE2FRAME" :name "OLE Object" :wcs t
  :picture 'ole-picture :pieces 'ole-pieces :xform 'xform-ole
  :grips 'ole-grips :stretch 'ole-stretch :describe 'describe-ole
  :hover (lambda (d) (list "OLE object" (ole-kind-name d)))
  :explode nil :write2000 'write-ole-2000
  :write12 (lambda (d) (explode-picture d (remove "IMAGE" (ole-picture d) :key (lambda (s) (dxf 0 s)) :test #'equal)))
  :from-dxf 'ole-from-dxf
  :valid (lambda (d) (and (consp (dxf 10 d)) (consp (dxf 11 d)))))

(defun ole-document (d)
  "(values octets file-type) of the document embedded in OLE object D."
  (let* ((v (ole-octets d)) (k (ole-cfb v)) (streams (ole-contents d)))
    (flet ((stream (n) (cdr (find n streams :key #'car :test #'string-equal))))
      (multiple-value-bind (user prog) (compobj-info streams)
        (declare (ignore user))
        (let ((prog (or prog "")))
          (cond
            ((stream "Package")
             (values (stream "Package")
                     (cond ((search "Excel" prog) (if (search "Macro" prog) "xlsm" "xlsx"))
                           ((search "Word" prog) (if (search "Macro" prog) "docm" "docx"))
                           ((search "PowerPoint" prog) "pptx")
                           (t "zip"))))
            ((stream "WordDocument") (values (subseq v k) "doc"))
            ((or (stream "Workbook") (stream "Book")) (values (subseq v k) "xls"))
            ((stream "PowerPoint Document") (values (subseq v k) "ppt"))
            ((stream (format nil "~COle10Native" (code-char 1)))
             (let ((n (stream (format nil "~COle10Native" (code-char 1)))))
               (cond ((and (> (length n) 6) (= (aref n 4) 66) (= (aref n 5) 77)) (values (subseq n 4) "bmp"))
                     ((search "Package" prog)
                      ;; a packaged file: its name, where it came from, then the file itself
                      (let* ((i 6) (label (let ((e (position 0 n :start i))) (prog1 (map 'string #'code-char (subseq n i e)) (setf i (1+ e)))))
                             (i (1+ (position 0 n :start i)))                ; its original path
                             (i (+ i 4))                                       ; (a version)
                             (i (+ i 4 (u32 n i)))                             ; its temporary path
                             (size (u32 n i)))
                        (values (subseq n (+ i 4) (min (length n) (+ i 4 size)))
                                (let ((dot (position #\. label :from-end t))) (if dot (subseq label (1+ dot)) "bin")))))
                     (t (values (subseq n 4) "bin")))))
            (k (values (subseq v k) "ole"))
            (t nil)))))))

(defcmd "OLEOPEN" () "Opens an embedded OLE object (an Office document, a picture ...) in the program it came from, as a copy."
  (let ((e (sel-entity (get-user-input :entity :prompt "Select OLE object: "))))
    (cond ((null e) nil)
          ((not (equal (ent-type e) "OLE2FRAME")) (out (format nil "That is not an OLE object.~%")))
          (t (multiple-value-bind (bytes ext) (ole-document (ent-data e))
               (if (null bytes)
                   (out (format nil "The object's contents can't be read.~%"))
                   (let ((path (format nil "~A~A-~A.~A" (let ((dd (merge-pathnames "ole/" (image-cache-dir)))) (ensure-directories-exist dd) (namestring dd))
                                       (string-trim "./ " (remove-if-not (lambda (c) (or (alphanumericp c) (member c '(#\- #\_)))) (ole-kind-name (ent-data e))))
                                       (ename-handle e) ext)))
                     (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
                       (write-sequence bytes o))
                     (out (format nil "~A saved as ~A~%(a copy: changes to it don't come back into the drawing).~%" (ole-kind-name (ent-data e)) path))
                     (when (gui-p) (run-shell (format nil "(xdg-open ~A >/dev/null 2>&1 &)" (shell-quote path)))))))))))

;;; ---------------------------------------------------------------- DXF 2000: classes and objects

(defun kind-extras-2000 ()
  "The objects the types above need in a DXF 2000 file: multiline styles."
  (let ((used (remove-duplicates (loop for d in (every-drawing-object)
                                       when (equal (dxf 0 d) "MLINE") collect (getf (mline-style d) :name))
                                 :test #'string-equal)))
    (when (or used (drawing-mlstyles *dwg*))
      (let ((names (mapcar (lambda (s) (cons (getf s :name) (h2))) (all-mlstyles))))
        (list (list "ACAD_MLINESTYLE" (h2) (lambda (out h root names) (write-mlstyles-2000 out h root names)) names))))))

(defun write-kind-classes-2000 (out)
  (when (drawing-uses-p (lambda (d) (equal (dxf 0 d) "WIPEOUT"))) (write-wipeout-class out))
  (when (drawing-uses-p (lambda (d) (and (equal (dxf 0 d) "DIMENSION") (dxf :arclen d))))
    (w out 0 "CLASS") (w out 1 "ARC_DIMENSION") (w out 2 "AcDbArcDimension") (w out 3 "ObjectDBX Classes")
    (w out 90 1025) (w out 280 0) (w out 281 1)))
