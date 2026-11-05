;;;; grips.lisp — AutoCAD-style grips on selected objects and grip editing.

(in-package :fancy-pencil)

(defvar *grip-points* nil "Grip points currently displayed: list of (point . ename).")

(defun entity-grips (data)
  "List of (point . id) grips for DATA."
  (when (offplane-p data) (return-from entity-grips (offplane-grips data)))
  ;; a solid: one grip in its middle moves it
  (when (equal (dxf 0 data) "POLYFACE") (return-from entity-grips (list (cons (mesh-centroid data) :offplane))))
  (let ((type (dxf 0 data)))
    (flet ((g (code id) (let ((p (dxf code data))) (and p (cons (pt3 p) id)))))
      (remove nil
              (cond
                ((equal type "LINE")
                 (list (g 10 :start) (g 11 :end) (cons (midpt (pt3 (dxf 10 data)) (pt3 (dxf 11 data))) :mid)))
                ((equal type "CIRCLE")
                 (let ((c (pt3 (dxf 10 data))) (r (dxf 40 data)))
                   (cons (cons c :center) (loop for i below 4 collect (cons (polar c (* i (/ +pi+ 2)) r) (list :quad i))))))
                ((equal type "ARC")
                 (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data)) (a1 (dxf 50 data)) (a2 (dxf 51 data)))
                   (list (cons (polar c a1 r) :start) (cons (polar c a2 r) :end)
                         (cons (polar c (+ a1 (/ (arc-sweep a1 a2) 2)) r) :mid) (cons c :center))))
                ((equal type "LWPOLYLINE")
                 ;; grips sit at the polyline's elevation (where the 3D views draw it)
                 (let* ((el (d (or (dxf 38 data) 0)))
                        (vs (mapcar (lambda (v) (cons (pt (px (first v)) (py (first v)) el) (rest v))) (lwpoly-vertices data)))
                        (closed (lwpoly-closed-p data)))
                   (append (loop for (p) in vs for i from 0 collect (cons p (list :vertex i)))
                           ;; segment midpoint grips (move the segment)
                           (loop for i from 0 below (if closed (length vs) (1- (length vs)))
                                 for p = (first (nth i vs)) for q = (first (nth (mod (1+ i) (length vs)) vs))
                                 for b = (second (nth i vs))
                                 collect (cons (if (zerop b) (midpt p q)
                                                   (multiple-value-bind (c r a1 a2) (bulge->arc p q b)
                                                     (polar c (+ a1 (/ (arc-sweep a1 a2) 2)) r)))
                                               (list :segment i))))))
                ((member type '("TEXT" "ATTDEF") :test #'equal) (list (cons (pt3 (text-insertion data)) :insert)))
                ((equal type "MTEXT")
                 ;; insertion grip plus the top corners of the box, which set the wrap width
                 (let* ((c (text-box data)) (ins (pt3 (dxf 10 data))))
                   (cons (cons ins :insert)
                         (loop for (p id) in (list (list (fourth c) :mtw-left) (list (third c) :mtw-right))
                               unless (< (dist2d p ins) 1d-9) collect (cons (pt3 p) id)))))
                ((member type '("INSERT" "POINT") :test #'equal) (list (g 10 :insert)))
                ((equal type "ELLIPSE")
                 (let* ((c (pt3 (dxf 10 data))) (m (pt3 (dxf 11 data))) (k (or (dxf 40 data) 1d0))
                        (minor (pt (* k (- (py m))) (* k (px m)))))
                   (list (cons c :center) (cons (v+ c m) :major+) (cons (v- c m) :major-)
                         (cons (v+ c minor) :minor+) (cons (v- c minor) :minor-))))
                ((and (equal type "DIMENSION") (linear-dim-p data))
                 ;; extension line origins, the two ends of the dimension line (arrows), the text
                 (destructuring-bind (e1 e2) (dim-line-ends data)
                   (list (g 13 :d13) (g 14 :d14) (cons (pt3 e1) :dl1) (cons (pt3 e2) :dl2) (g 11 :d11))))
                ((equal type "DIMENSION")
                 (list (g 13 :d13) (g 14 :d14) (g 10 :d10) (g 11 :d11) (g 15 :d15) (g 16 :d16)))
                ((equal type "SOLID") (list (g 10 :d10) (g 11 :d11) (g 12 :d12) (g 13 :d13)))
                ((equal type "VIEWPORT")
                 (loop for pc in (data-pieces data) for i from 0 collect (cons (second pc) (list :vpcorner i))))
                ((equal type "HATCH")
                 (let ((b (entity-pieces-bbox data)))
                   (and b (list (cons (pt (/ (+ (first b) (third b)) 2) (/ (+ (second b) (fourth b)) 2)) :move)))))
                ((equal type "HELIX") (funcall 'helix-grips data))
                ;; a fit spline: grips on the points it passes through
                ((and (equal type "SPLINE") (funcall 'spline-fit-points data))
                 (loop for f in (funcall 'spline-fit-points data) for i from 0 collect (cons f (list :fit i))))
                ((member type '("SPLINE" "LEADER") :test #'equal)
                 (loop for p in data for i from 0
                       when (and (consp p) (eql (car p) 10)) collect (cons (pt3 (cdr p)) (list :ctrl i))))
                (t nil))))))

(defun entity-pieces-bbox (data) (pieces-bbox (data-pieces data)))

(defun set-nth-code (data code n new)
  "Replace the Nth (0-based) occurrence of CODE in DATA with NEW (a point)."
  (let ((k -1))
    (mapcar (lambda (p)
              (if (and (consp p) (eql (car p) code) (= (incf k) n))
                  (if (= (length (cdr p)) 2) (list code (px new) (py new)) (cons code (pt3 new)))
                  p))
            data)))

(defun grip-stretch (data id from to)
  "New entity data after dragging grip ID from FROM to TO."
  (let ((type (dxf 0 data)) (dv (v- to from)))
    (cond
      ((eq id :offplane) (xform-data data (xf-move dv)))
      ((member id '(:hstart :htop :htopr)) (funcall 'helix-grip-stretch data id to))
      ((member id '(:center :insert :move))
       (xform-data data (xf-move dv)))
      ((and (equal type "MTEXT") (member id '(:mtw-left :mtw-right)))
       (mtext-width-stretch data id to))
      ((equal type "LINE")
       (case id (:start (set-dxf 10 to data)) (:end (set-dxf 11 to data)) (:mid (xform-data data (xf-move dv)))))
      ((equal type "CIRCLE") (set-dxf 40 (max 1d-9 (dist2d (pt3 (dxf 10 data)) to)) data))
      ((equal type "ARC")
       (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data)) (a1 (dxf 50 data)) (a2 (dxf 51 data))
              (s (polar c a1 r)) (e (polar c a2 r)) (m (polar c (+ a1 (/ (arc-sweep a1 a2) 2)) r))
              (new (case id (:start (arc-3p to m e)) (:end (arc-3p s m to)) (:mid (arc-3p s to e)))))
         (if new (append (rem-dxf 10 (rem-dxf 40 (rem-dxf 50 (rem-dxf 51 data)))) (rest new)) data)))
      ((and (equal type "LWPOLYLINE") (consp id) (eq (first id) :vertex))
       ;; a polyline is flat: a vertex moves across, never up or down on its own
       (set-nth-code data 10 (second id) (pt (px to) (py to))))
      ((and (equal type "LWPOLYLINE") (consp id) (eq (first id) :segment))
       (let* ((i (second id)) (n (length (lwpoly-vertices data))) (dv (pt (px dv) (py dv))))
         (set-nth-code (set-nth-code data 10 i (v+ (first (nth i (lwpoly-vertices data))) dv))
                       10 (mod (1+ i) n) (v+ (first (nth (mod (1+ i) n) (lwpoly-vertices data))) dv))))
      ((equal type "ELLIPSE")
       (let* ((c (pt3 (dxf 10 data))) (m (pt3 (dxf 11 data))) (k (or (dxf 40 data) 1d0))
              (mlen (dist2d (pt 0 0) m)) (minor-len (* k mlen)))
         (case id
           ((:major+ :major-)
            (let* ((nm (if (eq id :major+) (v- to c) (v- c to))) (nl (max 1d-9 (dist2d (pt 0 0) nm))))
              (set-dxf 40 (min 1d0 (/ minor-len nl)) (set-dxf 11 nm data))))
           ((:minor+ :minor-) (set-dxf 40 (max 1d-6 (min 1d0 (/ (dist2d c to) mlen))) data)))))
      ((member id '(:dl1 :dl2)) (dimline-grip-stretch data id to))
      ((member id '(:d10 :d11 :d12 :d13 :d14 :d15 :d16))
       (let ((code (parse-integer (subseq (symbol-name id) 1))))
         (if (and (equal type "DIMENSION") (eq id :d11) (member (logand (or (dxf 70 data) 0) 7) '(0 1)))
             ;; dragging linear dimension text moves the dimension line with it
             (set-dxf 10 (v+ (pt3 (dxf 10 data)) dv) (set-dxf 11 to data))
             (set-dxf code to data))))
      ((and (consp id) (eq (first id) :vpcorner))
       ;; the opposite corner stays put; the scale stays, so more or less of the model shows
       (let* ((pcs (data-pieces data)) (opp (second (nth (mod (+ (second id) 2) 4) pcs)))
              (w (max 1d-6 (abs (- (px to) (px opp))))) (h (max 1d-6 (abs (- (py to) (py opp)))))
              (k (/ h (d (or (dxf 41 data) h)))))
         (set-dxf 45 (* (d (or (dxf 45 data) (dxf 41 data) h)) k)
                  (set-dxf 41 h (set-dxf 40 w (set-dxf 10 (pt (/ (+ (px to) (px opp)) 2) (/ (+ (py to) (py opp)) 2) 0) data))))))
      ((and (consp id) (eq (first id) :fit))
       ;; a fit point moved: the spline is fitted again through the new points
       (let ((fit (funcall 'spline-fit-points data)))
         (setf (nth (second id) fit) (pt3 to))
         (funcall 'refit-spline data fit)))
      ((and (consp id) (eq (first id) :ctrl))
       (let ((target (second id)))
         ;; a control vertex moved: the curve no longer passes through its fit points (AutoCAD drops them)
         (remove-if (lambda (p) (and (consp p) (member (car p) '(11 12 13))))
                    (loop for p in data for j from 0 collect (if (= j target) (cons 10 (pt3 to)) p)))))
      (t data))))

(defun pickfirst-grips ()
  (let ((sel (remove-if-not #'ent-live-p *pickfirst*)))
    (when (and (eql (sysvar "GRIPS") 1) sel (<= (length sel) (or (sysvar "GRIPOBJLIMIT") 100)))
      (loop for e in sel
            append (loop for (p . id) in (entity-grips (ent-data e)) collect (list p e id))))))

(defun grip-pixel-size () (or (sysvar "GRIPSIZE") 5))

(defun refresh-grips ()
  (when (eq *ui* :gui)
    (setf *grip-points* (pickfirst-grips))
    (let ((s (grip-pixel-size)) (cmds (list ".c delete grip")))
      (dolist (g (remove-duplicates *grip-points* :key #'first :test (lambda (a b) (< (dist2d a b) 1d-9))))
        (multiple-value-bind (x y) (w->s (first g))
          (when (and (< -20 x (+ *full-cw* 20)) (< -20 y (+ *full-ch* 20)))
            (push (format nil ".c create rectangle ~D ~D ~D ~D -fill #1e6fff -outline #0b2a66 -tags grip"
                          (round (- x s)) (round (- y s)) (round (+ x s)) (round (+ y s)))
                  cmds))))
      (push ".c raise tmp" cmds)
      (tk-send-string (format nil "~{~A~%~}" (nreverse cmds))))
    (refresh-layer-box)))

(defun grip-at (p)
  "The grip point within pick distance of P, if any."
  (let ((tol (gui-pixels->world (+ (grip-pixel-size) 2))) (best nil) (bd nil))
    (dolist (g *grip-points* best)
      (let ((dd (view-dist (first g) p)))
        (when (and (<= dd tol) (or (null bd) (< dd bd))) (setf best (first g) bd dd))))))

(defun grip-hover-cmds ()
  "Pink grip under the cursor at the Command: prompt."
  (when (and *grip-points* *mouse-in*)
    (let ((g (grip-at (s->w *mouse-sx* *mouse-sy*))) (s (grip-pixel-size)))
      (when g
        (multiple-value-bind (x y) (w->s g)
          (list (format nil ".c create rectangle ~D ~D ~D ~D -fill #ff6fa8 -outline #7a1f45 -tags tmp"
                        (round (- x s)) (round (- y s)) (round (+ x s)) (round (+ y s)))))))))

(defun hot-grip-item (p) (list :hotgrip p))

(defun grip-edit (gp)
  "Interactive grip editing from the hot grip GP: STRETCH -> MOVE -> ROTATE -> SCALE -> MIRROR."
  (let* ((sel (remove-if-not #'ent-live-p (filter-editable *pickfirst* nil)))
         (modes '(:stretch :move :rotate :scale :mirror))
         (mode :stretch) (base gp) (copy nil) (done nil))
    (flet ((affected ()
             ;; for STRETCH: every selected object with a grip on the hot point
             (loop for e in sel
                   for gs = (remove-if-not (lambda (g) (< (dist2d (car g) gp) 1d-9)) (entity-grips (ent-data e)))
                   when gs collect (cons e (mapcar #'cdr gs))))
           (xf-for (to)
             (case mode
               (:move (xf-move (v- to base)))
               (:rotate (xf-rotate base (ang base to)))
               (:scale (let ((k (dist2d base to))) (and (> k 1d-9) (xf-scaler base k))))
               (:mirror (and (> (dist2d base to) 1d-9) (xf-reflect base to))))))
      ;; dragging a dimension line by its end: line it up with other dimensions
      (let ((dl (loop for (e . ids) in (affected)
                      when (intersection ids '(:dl1 :dl2)) return e)))
        (setf *dimline-snap* (and dl (let ((th (dim-line-theta (ent-data dl)))) (lambda (c) (declare (ignore c)) th)))
              *dimline-exclude* dl))
      (unwind-protect
      (with-undo-group
          (lambda ()
            (loop until done do
              (let* ((label (string-upcase (symbol-name mode)))
                     (prompt (progn
                               (out (format nil "~:[~%~;~]** ~A~:[~; (multiple)~] **~%" *at-line-start* label copy))
                               (format nil "Specify ~A or [Base point/Copy/Undo/eXit]: "
                                       (case mode (:stretch "stretch point") (:move "move point") (:rotate "rotation angle")
                                         (:scale "scale factor") (:mirror "second point")))))
                     (drag (lambda (cur)
                             (cons (hot-grip-item gp)
                                   (if (eq mode :stretch)
                                       (loop for (e . ids) in (affected)
                                             append (preview-items (reduce (lambda (dd id) (grip-stretch dd id gp cur)) ids
                                                                           :initial-value (ent-data e))))
                                       (let ((xf (xf-for cur)))
                                         (and xf (loop for e in sel collect (list :data (xform-data (ent-data e) xf)))))))))
                     (r (get-user-input (if (member mode '(:rotate)) :angle :point)
                                        :prompt prompt :base base
                                        :keywords '("Base" "Copy" "Undo" "eXit") :drag drag)))
                (cond
                  ((null r) (setf mode (nth (mod (1+ (position mode modes)) (length modes)) modes)))
                  ((equal r "eXit") (setf done t))
                  ((equal r "Base") (let ((b (gpoint "Specify base point: "))) (when (pointp b) (setf base b))))
                  ((equal r "Copy") (setf copy t))
                  ((equal r "Undo") (undo-one))
                  (t
                   (let ((to (if (realp r) (polar base r 1d0) r)))
                     (if (eq mode :stretch)
                         (loop for (e . ids) in (affected)
                               for nd = (reduce (lambda (dd id) (grip-stretch dd id gp to)) ids :initial-value (ent-data e))
                               do (if copy (add-entity (rem-dxf 5 nd)) (set-entity-data e nd)))
                         (let ((xf (if (and (eq mode :rotate) (realp r)) (xf-rotate base r) (xf-for to))))
                           (when xf (transform-entities sel xf :copy copy))))
                     (unless copy (setf done t)))))))))
        (setf *dimline-snap* nil *dimline-exclude* nil *dimline-match* nil)))
    (refresh-grips)))

(defun preview-items (data)
  "Drag preview of DATA. A linear dimension is drawn from its new geometry (its saved picture
would still show the old position)."
  (if (linear-dim-p data)
      (or (ignore-errors
           (mapcar (lambda (e) (list :data e))
                   (linear-dim-geometry (pt3 (dxf 13 data)) (pt3 (dxf 14 data)) (pt3 (dxf 10 data))
                                        (dim-line-theta data) (let ((tx (dxf 1 data))) (and tx (plusp (length tx)) tx))
                                        (dimension-vars data))))
          (list (list :data data)))
      (list (list :data data))))
