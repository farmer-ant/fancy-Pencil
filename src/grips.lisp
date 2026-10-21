;;;; grips.lisp — AutoCAD-style grips on selected objects and grip editing.

(in-package :fancy-pencil)

(defvar *grip-points* nil "Grip points currently displayed: list of (point . ename).")

(defun entity-grips (data)
  "List of (point . id) grips for DATA."
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
                 (let ((vs (lwpoly-vertices data)) (closed (lwpoly-closed-p data)))
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
                ((member type '("MTEXT" "INSERT" "POINT") :test #'equal) (list (g 10 :insert)))
                ((equal type "ELLIPSE")
                 (let* ((c (pt3 (dxf 10 data))) (m (pt3 (dxf 11 data))) (k (or (dxf 40 data) 1d0))
                        (minor (pt (* k (- (py m))) (* k (px m)))))
                   (list (cons c :center) (cons (v+ c m) :major+) (cons (v- c m) :major-)
                         (cons (v+ c minor) :minor+) (cons (v- c minor) :minor-))))
                ((equal type "DIMENSION")
                 (list (g 13 :d13) (g 14 :d14) (g 10 :d10) (g 11 :d11) (g 15 :d15) (g 16 :d16)))
                ((equal type "SOLID") (list (g 10 :d10) (g 11 :d11) (g 12 :d12) (g 13 :d13)))
                ((equal type "HATCH")
                 (let ((b (entity-pieces-bbox data)))
                   (and b (list (cons (pt (/ (+ (first b) (third b)) 2) (/ (+ (second b) (fourth b)) 2)) :move)))))
                ((equal type "SPLINE")
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
      ((member id '(:center :insert :move))
       (xform-data data (xf-move dv)))
      ((equal type "LINE")
       (case id (:start (set-dxf 10 to data)) (:end (set-dxf 11 to data)) (:mid (xform-data data (xf-move dv)))))
      ((equal type "CIRCLE") (set-dxf 40 (max 1d-9 (dist2d (pt3 (dxf 10 data)) to)) data))
      ((equal type "ARC")
       (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data)) (a1 (dxf 50 data)) (a2 (dxf 51 data))
              (s (polar c a1 r)) (e (polar c a2 r)) (m (polar c (+ a1 (/ (arc-sweep a1 a2) 2)) r))
              (new (case id (:start (arc-3p to m e)) (:end (arc-3p s m to)) (:mid (arc-3p s to e)))))
         (if new (append (rem-dxf 10 (rem-dxf 40 (rem-dxf 50 (rem-dxf 51 data)))) (rest new)) data)))
      ((and (equal type "LWPOLYLINE") (consp id) (eq (first id) :vertex))
       (set-nth-code data 10 (second id) to))
      ((and (equal type "LWPOLYLINE") (consp id) (eq (first id) :segment))
       (let* ((i (second id)) (n (length (lwpoly-vertices data))))
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
      ((member id '(:d10 :d11 :d12 :d13 :d14 :d15 :d16))
       (let ((code (parse-integer (subseq (symbol-name id) 1))))
         (if (and (equal type "DIMENSION") (eq id :d11) (member (logand (or (dxf 70 data) 0) 7) '(0 1)))
             ;; dragging linear dimension text moves the dimension line with it
             (set-dxf 10 (v+ (pt3 (dxf 10 data)) dv) (set-dxf 11 to data))
             (set-dxf code to data))))
      ((and (consp id) (eq (first id) :ctrl))
       (let ((target (second id)))
         (loop for p in data for j from 0 collect (if (= j target) (cons 10 (pt3 to)) p))))
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
          (when (and (< -20 x (+ *cw* 20)) (< -20 y (+ *ch* 20)))
            (push (format nil ".c create rectangle ~D ~D ~D ~D -fill #1e6fff -outline #0b2a66 -tags grip"
                          (round (- x s)) (round (- y s)) (round (+ x s)) (round (+ y s)))
                  cmds))))
      (push ".c raise tmp" cmds)
      (tk-send-string (format nil "~{~A~%~}" (nreverse cmds))))))

(defun grip-at (p)
  "The grip point within pick distance of P, if any."
  (let ((tol (gui-pixels->world (+ (grip-pixel-size) 2))) (best nil) (bd nil))
    (dolist (g *grip-points* best)
      (let ((dd (dist2d (first g) p)))
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
                                             collect (list :data (reduce (lambda (dd id) (grip-stretch dd id gp cur)) ids
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
                     (unless copy (setf done t))))))))))
    (refresh-grips)))
