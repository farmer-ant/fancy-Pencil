;;;; modify.lisp — editing commands.

(in-package :fancy-pencil)

(defvar *last-erased* nil)
(defvar *clipboard* nil)

(defun preview-xf (ents xf-fn &optional (limit 400))
  (lambda (cur)
    (let ((xf (funcall xf-fn cur)))
      (when xf
        (loop for e in ents for i from 0 below limit
              collect (list :data (xform-data (ent-data e) xf)))))))

(defun transform-entities (ents xf &key copy)
  (dolist (e ents)
    (let ((nd (xform-data (ent-data e) xf)))
      (if copy (add-entity (rem-dxf 5 nd)) (set-entity-data e nd)))))

;;; ---------- ERASE / OOPS ----------

(defcmd "ERASE" ("E" "DELETE") "Removes objects from the drawing."
  (let ((ents (get-selection)))
    (dolist (e ents) (delete-entity e))
    (when ents (setf *last-erased* ents))))

(defcmd "OOPS" () "Restores the last erased objects."
  (dolist (e *last-erased*) (undelete-entity e))
  (setf *last-erased* nil))

(defcmd "SELECTALL" ("AI_SELALL") "Selects every visible object."
  (ui-highlight *pickfirst* nil)
  (setf *pickfirst* (remove-if-not #'entity-visible-p (live-entities)))
  (ui-highlight *pickfirst* t)
  (out (format nil "~D found~%" (length *pickfirst*))))

;;; ---------- MOVE / COPY ----------

(defun base-and-displacement (ents verb)
  "AutoCAD's 'Specify base point or [Displacement]' sequence. Returns delta vector or NIL."
  (let ((b (gpoint "Specify base point or [Displacement] <Displacement>: " :keywords '("Displacement"))))
    (cond
      ((or (null b) (equal b "Displacement"))
       (let ((dd (gpoint (format nil "Specify displacement <0.0000, 0.0000, 0.0000>: "))))
         (and (pointp dd) dd)))
      (t (let ((p2 (gpoint (format nil "Specify second point or <use first point as displacement>: ")
                           :base b :drag (preview-xf ents (lambda (cur) (xf-move (v- cur b)))))))
           (declare (ignorable verb))
           (cond ((null p2) b)
                 ((pointp p2) (v- p2 b))))))))

(defcmd "MOVE" ("M") "Moves objects."
  (let ((ents (get-selection)))
    (when ents
      (let ((dv (base-and-displacement ents "move")))
        (when dv (transform-entities ents (xf-move dv)))))))

(defcmd "COPY" ("CO" "CP") "Copies objects."
  (let ((ents (get-selection)))
    (when ents
      (out (format nil "Current settings:  Copy mode = Multiple~%"))
      (let ((b (gpoint "Specify base point or [Displacement] <Displacement>: " :keywords '("Displacement"))))
        (if (or (null b) (equal b "Displacement"))
            (let ((dd (gpoint "Specify displacement <0.0000, 0.0000, 0.0000>: ")))
              (when (pointp dd) (transform-entities ents (xf-move dd) :copy t)))
            (let ((made nil))
              (loop
                (let ((p2 (gpoint (if made "Specify second point or [Exit/Undo] <Exit>: "
                                      "Specify second point or [Array] <use first point as displacement>: ")
                                  :base b :keywords (if made '("Exit" "Undo") '("Array"))
                                  :drag (preview-xf ents (lambda (cur) (xf-move (v- cur b)))))))
                  (cond ((equal p2 "Exit") (return))
                        ((null p2) (unless made (transform-entities ents (xf-move b) :copy t)) (return))
                        ((equal p2 "Undo")
                         (let ((batch (pop made))) (dolist (e batch) (delete-entity e))))
                        ((equal p2 "Array")
                         (let* ((n (or (gint "Enter number of items to array: " :flags 7) 1))
                                (q (gpoint "Specify second point or [Fit]: " :base b :keywords '("Fit")))
                                (fit (equal q "Fit"))
                                (q (if fit (gpoint "Specify second point: " :base b) q)))
                           (when (pointp q)
                             (loop for i from 1 below n
                                   for dv = (v* (v- q b) (if fit (/ i (max 1 (1- n))) i))
                                   do (transform-entities ents (xf-move dv) :copy t)))
                           (return)))
                        ((pointp p2)
                         (let ((before (length (drawing-entities *dwg*))))
                           (transform-entities ents (xf-move (v- p2 b)) :copy t)
                           (push (coerce (subseq (drawing-entities *dwg*) before) 'list) made))))))))))))

;;; ---------- ROTATE / SCALE / MIRROR ----------

(defcmd "ROTATE" ("RO") "Rotates objects around a base point."
  (let ((ents (get-selection)))
    (when ents
      (out (format nil "Current positive angle in UCS:  ANGDIR=counterclockwise  ANGBASE=0~%"))
      (let* ((b (gpoint "Specify base point: ")) (copy nil)
             (last (or (sysvar "LASTROTATION") 0d0)))
        (when b
          (loop
            (let ((a (gangle (format nil "Specify rotation angle or [Copy/Reference] <~A>: " (fmt-ang last))
                             :base b :keywords '("Copy" "Reference")
                             :drag (preview-xf ents (lambda (cur) (xf-rotate b (ang b cur)))))))
              (cond ((equal a "Copy") (setf copy t) (out (format nil "Rotating a copy of the selected objects.~%")))
                    ((equal a "Reference")
                     (let* ((r1 (gangle "Specify the reference angle <0>: " :base b))
                            (r2 (gangle "Specify the new angle or [Points] <0>: " :base b)))
                       (transform-entities ents (xf-rotate b (- (or r2 0d0) (or r1 0d0))) :copy copy)
                       (return)))
                    (t (let ((a (if (realp a) a last)))
                         (setf (sysvar "LASTROTATION") a)
                         (transform-entities ents (xf-rotate b a) :copy copy)
                         (return)))))))))))

(defcmd "SCALE" ("SC") "Enlarges or reduces objects."
  (let ((ents (get-selection)))
    (when ents
      (let ((b (gpoint "Specify base point: ")) (copy nil))
        (when b
          (loop
            (let ((s (gdist "Specify scale factor or [Copy/Reference] <1.0000>: " :base b :keywords '("Copy" "Reference")
                            :drag (preview-xf ents (lambda (cur) (let ((k (dist2d b cur))) (and (> k 1d-9) (xf-scaler b k))))))))
              (cond ((equal s "Copy") (setf copy t) (out (format nil "Scaling a copy of the selected objects.~%")))
                    ((equal s "Reference")
                     (let* ((r (gdist "Specify reference length <1.0000>: " :flags 6))
                            (n (gdist "Specify new length or [Points] <1.0000>: " :base b :flags 6)))
                       (transform-entities ents (xf-scaler b (/ (or n 1d0) (or r 1d0))) :copy copy)
                       (return)))
                    ((and (realp s) (<= s 0)) (out (format nil "Value must be positive and nonzero.~%")))
                    (t (transform-entities ents (xf-scaler b (if (realp s) s 1d0)) :copy copy)
                       (return))))))))))

(defcmd "MIRROR" ("MI") "Creates a mirrored copy of objects."
  (let ((ents (get-selection)))
    (when ents
      (let* ((a (gpoint "Specify first point of mirror line: "))
             (b (and a (gpoint "Specify second point of mirror line: " :base a
                               :drag (preview-xf ents (lambda (cur) (and (> (dist2d a cur) 1d-9) (xf-reflect a cur))))))))
        (when (pointp b)
          (let ((erase (gkw "Erase source objects? [Yes/No] <No>: " '("Yes" "No") "No")))
            (transform-entities ents (xf-reflect a b) :copy (not (equal erase "Yes")))))))))

;;; ---------- OFFSET ----------

(defun offset-line (data side-pt dd)
  (let* ((a (pt3 (dxf 10 data))) (b (pt3 (dxf 11 data)))
         (s (if (> (cross2 a b side-pt) 0) 1 -1))
         (n (polar (pt 0 0) (+ (ang a b) (* s (/ +pi+ 2))) dd)))
    (set-dxf 11 (v+ b n) (set-dxf 10 (v+ a n) data))))

(defun offset-circular (data side-pt dd)
  (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data))
         (nr (if (> (dist2d c side-pt) r) (+ r dd) (- r dd))))
    (when (> nr 1d-9) (set-dxf 40 nr data))))

(defun offset-lwpoly (data side-pt dd)
  (let* ((verts (lwpoly-vertices data)) (closed (lwpoly-closed-p data))
         (n (length verts)) (nseg (if closed n (1- n)))
         (segs (loop for i from 0 below nseg
                     for (p b) = (nth i verts)
                     for q = (first (nth (mod (1+ i) n) verts))
                     collect (list p q b)))
         ;; which side? use nearest segment
         (best (let ((bi 0) (bd nil))
                 (loop for (p q b) in segs for i from 0
                       for pc = (if (zerop b) (list :seg p q)
                                    (multiple-value-bind (c r a1 a2) (bulge->arc p q b) (list :arc c r a1 a2)))
                       for dist = (piece-dist pc side-pt)
                       when (or (null bd) (< dist bd)) do (setf bd dist bi i))
                 bi))
         (s (destructuring-bind (p q b) (nth best segs)
              (if (zerop b)
                  (if (> (cross2 p q side-pt) 0) 1 -1)
                  (multiple-value-bind (c r) (bulge->arc p q b)
                    (let ((inside (< (dist2d c side-pt) r)))
                      (if (eq inside (> b 0)) 1 -1))))))
         (osegs (loop for (p q b) in segs
                      collect (if (zerop b)
                                  (let ((nv (polar (pt 0 0) (+ (ang p q) (* s (/ +pi+ 2))) dd)))
                                    (list (v+ p nv) (v+ q nv) b))
                                  (multiple-value-bind (c r) (bulge->arc p q b)
                                    (let ((nr (- r (* s dd (if (> b 0) 1 -1)))))
                                      (when (<= nr 1d-9) (return-from offset-lwpoly nil))
                                      (list (polar c (ang c p) nr) (polar c (ang c q) nr) b))))))
         (nv nil))
    (loop for i from 0 below (length osegs)
          for cur = (nth i osegs)
          for prev = (if (> i 0) (nth (1- i) osegs) (and closed (car (last osegs))))
          do (let ((start (if (and prev (zerop (third prev)) (zerop (third cur)))
                              (or (line-line-int (first prev) (second prev) (first cur) (second cur) nil) (first cur))
                              (first cur))))
               (push (list start (third cur)) nv)))
    (unless closed (push (list (second (car (last osegs))) 0d0) nv))
    (append (list (cons 0 "LWPOLYLINE"))
            (rest (make-lwpoly-data (nreverse nv) closed :width (dxf 43 data)))
            (common-props data))))

(defun offset-entity (data side-pt dd)
  (let ((type (dxf 0 data)))
    (cond ((equal type "LINE") (offset-line data side-pt dd))
          ((member type '("CIRCLE" "ARC") :test #'equal) (offset-circular data side-pt dd))
          ((equal type "LWPOLYLINE") (offset-lwpoly data side-pt dd))
          (t :unsupported))))

(defcmd "OFFSET" ("O") "Creates concentric circles, parallel lines, and parallel curves."
  (out (format nil "Current settings: Erase source=No  Layer=Source  OFFSETGAPTYPE=0~%"))
  (let* ((cur (sysvar "OFFSETDIST"))
         (r (gdist (format nil "Specify offset distance or [Through] <~A>: " (if (minusp cur) "Through" (fmt-len cur)))
                   :keywords '("Through")))
         (through (or (equal r "Through") (and (null r) (minusp cur))))
         (dd (cond ((realp r) r) (through -1d0) (t cur))))
    (when (and (realp dd) (not through) (<= dd 0)) (aerr "Value must be positive and nonzero."))
    (setf (sysvar "OFFSETDIST") dd)
    (loop
      (let ((sel (get-user-input :entity :prompt "Select object to offset or [Exit/Undo] <Exit>: " :keywords '("Exit" "Undo"))))
        (cond ((or (null sel) (equal sel "Exit")) (return))
              ((equal sel "Undo") (let ((e (last-entity))) (when e (delete-entity e))))
              ((consp sel)
               (let ((e (sel-entity sel)))
                 (cond ((null e) (out (format nil "No object found.~%")))
                       (t (ui-highlight (list e) t)
                          (let ((side (gpoint (if through "Specify through point or [Exit]: "
                                                  "Specify point on side to offset or [Exit]: ")
                                              :keywords '("Exit"))))
                            (ui-highlight (list e) nil)
                            (when (pointp side)
                              (let* ((data (ent-data e))
                                     (dist (if through
                                               (piece-dist (reduce (lambda (a b) (if (<= (piece-dist a side) (piece-dist b side)) a b))
                                                                   (snap-pieces e))
                                                           side)
                                               dd))
                                     (nd (offset-entity data side dist)))
                                (cond ((eq nd :unsupported) (out (format nil "Cannot offset that object.~%")))
                                      ((null nd) (out (format nil "Offset distance too large for that object.~%")))
                                      (t (add-entity (rem-dxf 5 nd))))))
                            (when (equal side "Exit") (return))))))))))))

;;; ---------- TRIM / EXTEND / BREAK ----------

(defun cutting-pieces (edges self)
  (loop for e in edges unless (eq e self) append (snap-pieces e)))

(defun line-int-params (a b cuts)
  (sort (remove-duplicates
         (loop for c in cuts
               append (loop for q in (piece-ints (list :seg a b) c)
                            for tt = (seg-param q a b)
                            when (< 1d-9 tt (- 1 1d-9)) collect tt))
         :test (lambda (x y) (< (abs (- x y)) 1d-9)))
        #'<))

(defun arc-int-params (c r a1 a2 cuts &optional full)
  "Positions (radians from A1, within the sweep) where cuts meet the arc."
  (let ((sw (if full +2pi+ (arc-sweep a1 a2))))
    (sort (remove-duplicates
           (loop for cut in cuts
                 append (loop for q in (piece-ints (if full (list :circle c r) (list :arc c r a1 a2)) cut)
                              for s = (norm-ang (- (ang c q) a1))
                              when (or full (< 1d-9 s (- sw 1d-9))) collect s))
           :test (lambda (x y) (< (abs (- x y)) 1d-9)))
          #'<)))

(defun replace-with (e datas)
  "Replace entity E by the list of entity alists DATAS (first reuses E)."
  (let ((props (common-props (ent-data e))))
    (if (null datas) (delete-entity e)
        (progn
          (if (equal (dxf 0 (first datas)) (ent-type e))
              (set-entity-data e (append (cons (assoc 0 (first datas)) (list (cons 5 (ename-handle e))))
                                         (rem-dxf 0 (rem-dxf 5 (first datas))) props))
              (progn (delete-entity e) (add-entity (append (first datas) props))))
          (dolist (dd (rest datas)) (add-entity (append dd props)))))))

(defun trim-entity (e pick cuts)
  "Returns :done, :no-intersection or :unsupported."
  (let* ((data (ent-data e)) (type (dxf 0 data)))
    (cond
      ((equal type "LINE")
       (let* ((a (pt3 (dxf 10 data))) (b (pt3 (dxf 11 data)))
              (ts (line-int-params a b cuts)) (tp (seg-param pick a b))
              (lo (find-if (lambda (x) (< x tp)) ts :from-end t))
              (hi (find-if (lambda (x) (> x tp)) ts)))
         (if (null ts) :no-intersection
             (progn (replace-with e (remove nil (list (and lo (line-data a (at-param a b lo)))
                                                      (and hi (line-data (at-param a b hi) b)))))
                    :done))))
      ((equal type "ARC")
       (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data)) (a1 (dxf 50 data)) (a2 (dxf 51 data))
              (ss (arc-int-params c r a1 a2 cuts)) (sp (norm-ang (- (ang c pick) a1)))
              (lo (find-if (lambda (x) (< x sp)) ss :from-end t))
              (hi (find-if (lambda (x) (> x sp)) ss)))
         (if (null ss) :no-intersection
             (progn (replace-with e (remove nil (list (and lo (arc-data c r a1 (+ a1 lo)))
                                                      (and hi (arc-data c r (+ a1 hi) a2)))))
                    :done))))
      ((equal type "CIRCLE")
       (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data))
              (ss (arc-int-params c r 0d0 0d0 cuts t)) (ap (ang c pick)))
         (if (< (length ss) 2) :no-intersection
             (let ((prev (or (find-if (lambda (x) (< x ap)) ss :from-end t) (car (last ss))))
                   (next (or (find-if (lambda (x) (> x ap)) ss) (first ss))))
               (replace-with e (list (arc-data c r next prev)))
               :done))))
      ((equal type "LWPOLYLINE") (trim-polyline e pick cuts))
      (t :unsupported))))

(defun extend-entity (e pick bounds)
  (let* ((data (ent-data e)) (type (dxf 0 data)))
    (flet ((hits (probe) (loop for bpc in bounds
                               append (loop for q in (piece-ints probe bpc t)
                                            when (on-piece-p bpc q 1d-7) collect q))))
      (cond
        ((equal type "LINE")
         (let* ((a (pt3 (dxf 10 data))) (b (pt3 (dxf 11 data)))
                (at-a (< (dist2d pick a) (dist2d pick b)))
                (from (if at-a b a)) (to (if at-a a b))
                (cands (loop for q in (hits (list :seg from to))
                             for s = (seg-param q from to)
                             when (> s (+ 1 1d-9)) collect s))
                (s (and cands (reduce #'min cands))))
           (if s (progn (set-entity-data e (set-dxf (if at-a 10 11) (at-param from to s) data)) :done)
               :no-intersection)))
        ((equal type "ARC")
         (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data)) (a1 (dxf 50 data)) (a2 (dxf 51 data))
                (at-start (< (dist2d pick (polar c a1 r)) (dist2d pick (polar c a2 r))))
                (qs (hits (list :circle c r)))
                (deltas (loop for q in qs
                              for aq = (ang c q)
                              for dlt = (if at-start (norm-ang (- a1 aq)) (norm-ang (- aq a2)))
                              when (> dlt 1d-9) collect dlt))
                (dlt (and deltas (reduce #'min deltas))))
           (if dlt (progn (set-entity-data e (if at-start (set-dxf 50 (norm-ang (- a1 dlt)) data)
                                                 (set-dxf 51 (norm-ang (+ a2 dlt)) data)))
                          :done)
               :no-intersection)))
        ((equal type "LWPOLYLINE") (extend-polyline e pick bounds))
        (t :unsupported)))))

(defun edge-select (what)
  (out (format nil "Current settings: Projection=UCS, Edge=None~%Select ~A edges ...~%" what))
  (let ((sel (select-objects :prompt "Select objects or <select all>: " :editable nil)))
    (or sel (remove-if-not #'entity-visible-p (live-entities)))))

(defun trim-or-extend (command-trims)
  "TRIM (COMMAND-TRIMS true) or EXTEND. As in AutoCAD, a Shift+click does the other operation
with the same edges: Shift-pick in TRIM extends, Shift-pick in EXTEND trims."
  (let ((edges (edge-select (if command-trims "cutting" "boundary"))) (undo nil))
    (loop
      (let* ((r (progn (setf *pick-shift* nil)
                       (get-user-input :entity :prompt (if command-trims
                                                           "Select object to trim or shift-select to extend or [eRase/Undo]: "
                                                           "Select object to extend or shift-select to trim or [eRase/Undo]: ")
                                               :keywords '("eRase" "Undo"))))
             (trimming (if *pick-shift* (not command-trims) command-trims)))
        (cond ((null r) (return))
              ((equal r "Undo") (let ((s (pop undo))) (if s (restore-snapshot s) (out (format nil "Nothing to undo.~%")))))
              ((equal r "eRase")
               (let ((ents (select-objects :prompt "Select objects to erase: ")))
                 (push (take-snapshot) undo)
                 (dolist (e ents) (delete-entity e))))
              ((consp r)
               (let* ((pick (sel-point r)) (e (sel-entity r)))
                 (if (null e) (out (format nil "No object found.~%"))
                     (let ((snap (take-snapshot))
                           (res (if trimming
                                    (trim-entity e pick (cutting-pieces edges e))
                                    (extend-entity e pick (cutting-pieces edges e)))))
                       (case res
                         (:done (push snap undo)
                          (setf edges (remove-if-not #'ent-live-p (append edges (live-entities-after snap)))))
                         (:no-intersection (out (format nil "Object does not intersect an edge.~%")))
                         (t (out (format nil "Cannot ~A this object.~%" (if trimming "trim" "extend"))))))))))))))

(defun live-entities-after (snap)
  (let ((n (length (first snap))))
    (loop for i from n below (length (drawing-entities *dwg*))
          for e = (aref (drawing-entities *dwg*) i)
          unless (ename-deleted e) collect e)))

(defcmd "TRIM" ("TR") "Trims objects to meet the edges of other objects." (trim-or-extend t))
(defcmd "EXTEND" ("EX") "Extends objects to meet the edges of other objects." (trim-or-extend nil))

(defcmd "BREAK" ("BR") "Breaks the selected object between two points."
  (let ((sel (get-user-input :entity :prompt "Select object: ")))
    (when (consp sel)
      (let* ((p1 (sel-point sel)) (e (sel-entity sel)))
        (when e
          (let* ((r (gpoint "Specify second break point or [First point]: " :keywords '("First")))
                 (p1 (if (equal r "First") (gpoint "Specify first break point: ") p1))
                 (p2 (if (equal r "First") (gpoint "Specify second break point: ") r))
                 (data (ent-data e)) (type (dxf 0 data)))
            (when (and (pointp p1) (pointp p2))
              (cond
                ((equal type "LINE")
                 (let* ((a (pt3 (dxf 10 data))) (b (pt3 (dxf 11 data)))
                        (t1 (max 0d0 (min 1d0 (seg-param p1 a b)))) (t2 (max 0d0 (min 1d0 (seg-param p2 a b)))))
                   (when (> t1 t2) (rotatef t1 t2))
                   (replace-with e (remove nil (list (and (> t1 1d-9) (line-data a (at-param a b t1)))
                                                     (and (< t2 (- 1 1d-9)) (line-data (at-param a b t2) b)))))))
                ((equal type "ARC")
                 (let* ((c (pt3 (dxf 10 data))) (rr (dxf 40 data)) (a1 (dxf 50 data)) (a2 (dxf 51 data))
                        (sw (arc-sweep a1 a2))
                        (s1 (min sw (norm-ang (- (ang c p1) a1)))) (s2 (min sw (norm-ang (- (ang c p2) a1)))))
                   (when (> s1 s2) (rotatef s1 s2))
                   (replace-with e (remove nil (list (and (> s1 1d-9) (arc-data c rr a1 (+ a1 s1)))
                                                     (and (< s2 (- sw 1d-9)) (arc-data c rr (+ a1 s2) a2)))))))
                ((equal type "CIRCLE")
                 (let ((c (pt3 (dxf 10 data))) (rr (dxf 40 data)))
                   (replace-with e (list (arc-data c rr (ang c p2) (ang c p1))))))
                (t (out (format nil "Cannot break that object.~%")))))))))))

;;; ---------- EXPLODE / JOIN ----------

(defcmd "EXPLODE" ("X") "Breaks a compound object into its component objects."
  (let ((ents (get-selection)) (n 0))
    (dolist (e ents)
      (let ((parts (explode-data (ent-data e))))
        (if parts
            (progn (delete-entity e) (dolist (p parts) (add-entity (rem-dxf 5 p))) (incf n))
            (out (format nil "1 object could not be exploded.~%")))))
    (when (plusp n) (out (format nil "~D exploded.~%" n)))))

(defun entity-chain-pieces (e)
  "Pieces of a LINE/ARC/open LWPOLYLINE as (start end bulge) triples."
  (let ((data (ent-data e)))
    (cond ((equal (dxf 0 data) "LINE") (list (list (pt3 (dxf 10 data)) (pt3 (dxf 11 data)) 0d0)))
          ((equal (dxf 0 data) "ARC")
           (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data)) (a1 (dxf 50 data)) (a2 (dxf 51 data)))
             (list (list (polar c a1 r) (polar c a2 r) (tan (/ (arc-sweep a1 a2) 4))))))
          ((and (equal (dxf 0 data) "LWPOLYLINE") (not (lwpoly-closed-p data)))
           (let ((v (lwpoly-vertices data)))
             (loop for ((p b) (q)) on v while q collect (list p q b)))))))

(defun reverse-chain (ch) (reverse (mapcar (lambda (s) (list (second s) (first s) (- (third s)))) ch)))

(defun join-entities (ents &key (tol 1d-6))
  "Join chainable ENTS (first one is the source) into one polyline. Returns (values new-ename count)."
  (let* ((first-e (first ents)) (chain (entity-chain-pieces first-e)) (used (list first-e)) (progress t))
    (loop while progress do
      (setf progress nil)
      (dolist (e ents)
        (unless (member e used)
          (let ((c (entity-chain-pieces e))
                (s (first (first chain))) (f (second (car (last chain)))))
            (cond ((< (dist2d f (first (first c))) tol) (setf chain (append chain c)))
                  ((< (dist2d f (second (car (last c)))) tol) (setf chain (append chain (reverse-chain c))))
                  ((< (dist2d s (second (car (last c)))) tol) (setf chain (append c chain)))
                  ((< (dist2d s (first (first c))) tol) (setf chain (append (reverse-chain c) chain)))
                  (t (setf c nil)))
            (when c (push e used) (setf progress t))))))
    (let* ((closed (< (dist2d (first (first chain)) (second (car (last chain)))) tol))
           (verts (append (mapcar (lambda (s) (list (first s) (third s))) chain)
                          (unless closed (list (list (second (car (last chain))) 0d0)))))
           (data (ent-data first-e))
           (new (add-entity (append (make-lwpoly-data verts closed :width (and (equal (dxf 0 data) "LWPOLYLINE") (dxf 43 data)))
                                    (common-props data)))))
      (dolist (e used) (delete-entity e))
      (values new (length used)))))

(defcmd "JOIN" ("J") "Joins lines, arcs and open polylines end to end into one polyline."
  (let* ((ents (remove-if-not #'entity-chain-pieces (get-selection "Select source objects or multiple objects to join at once: "))))
    (when (< (length ents) 2) (out (format nil "Select at least two connected objects.~%")) (return-from-cmd))
    (multiple-value-bind (new n) (join-entities ents)
      (declare (ignore new))
      (out (format nil "~D objects joined into 1 polyline.~%" n)))))

;;; ---------- STRETCH ----------

(defcmd "STRETCH" ("S") "Stretches objects crossed by a selection window."
  (out (format nil "Select objects to stretch by crossing-window or crossing-polygon...~%"))
  (let ((windows nil) (ents nil))
    ;; AutoCAD-style selection: C, implicit windows, or picks; Enter ends
    (loop
      (let ((r (get-user-input :select-point :prompt "Select objects: " :keywords '("Crossing" "Window"))))
        (flet ((corners (p1)
                 (let ((p2 (get-user-input :corner :base p1 :prompt "Specify opposite corner: "
                                                   :drag (lambda (cur) (list (list :rect p1 cur :crossing))) :nosnap t)))
                   (when (pointp p2)
                     (push (list (min (px p1) (px p2)) (min (py p1) (py p2)) (max (px p1) (px p2)) (max (py p1) (py p2))) windows)
                     (let ((found (filter-editable (entities-in-window p1 p2 t))))
                       (setf ents (union ents found))
                       (ui-highlight found t)
                       (out (format nil "~D found~%" (length found))))))))
          (cond ((null r) (return))
                ((member r '("Crossing" "Window") :test #'equal) (corners (gpoint "Specify first corner: ")))
                ((pickset-p r) (setf ents (union ents (pickset-items r))))
                ((ename-p r) (pushnew r ents))
                ((pointp r) (let ((e (pick-entity-at r)))
                              (if e (progn (pushnew e ents) (ui-highlight (list e) t)) (corners r))))))))
    (ui-highlight ents nil)
    (let* ((inside (lambda (p) (some (lambda (w) (destructuring-bind (x0 y0 x1 y1) w
                                                   (and (<= x0 (px p) x1) (<= y0 (py p) y1))))
                                     windows)))
           (mover (lambda (data dv)
                    (let ((type (dxf 0 data)))
                      (cond ((member type '("LINE" "LWPOLYLINE") :test #'equal)
                             (map-points data '(10 11) (lambda (p) (if (funcall inside p) (v+ p dv) p))))
                            ((or (null windows) (funcall inside (pt3 (or (dxf 10 data) (pt 0 0)))))
                             (xform-data data (xf-move dv)))
                            (t data))))))
      (when ents
        (let* ((b (gpoint "Specify base point or [Displacement] <Displacement>: " :keywords '("Displacement")))
               (dv (if (or (null b) (equal b "Displacement"))
                       (let ((dd (gpoint "Specify displacement <0.0000, 0.0000, 0.0000>: "))) (and (pointp dd) dd))
                       (let ((p2 (gpoint "Specify second point or <use first point as displacement>: " :base b
                                         :drag (lambda (cur) (mapcar (lambda (e) (list :data (funcall mover (ent-data e) (v- cur b)))) ents)))))
                         (if (pointp p2) (v- p2 b) b)))))
          (when dv (dolist (e ents) (set-entity-data e (funcall mover (ent-data e) dv)))))))))

;;; ---------- ARRAY ----------

(defun rect-array (ents)
  (let* ((rows (or (gint "Enter the number of rows (---) <1>: " :flags 6) 1))
         (cols (or (gint "Enter the number of columns (|||) <1>: " :flags 6) 1))
         (dy (if (> rows 1) (or (gdist "Enter the distance between rows or specify unit cell (---): ") 1d0) 0d0))
         (dx (if (> cols 1) (or (gdist "Specify the distance between columns (|||): ") 1d0) 0d0)))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   unless (and (zerop r) (zerop c))
                     do (transform-entities ents (xf-move (pt (* c dx) (* r dy))) :copy t)))))

(defun polar-array (ents)
  (let* ((c (gpoint "Specify center point of array: "))
         (n (or (gint "Enter the number of items in the array: " :flags 7) 1))
         (fill (or (gangle "Specify the angle to fill (+=ccw, -=cw) <360>: " :base c) +2pi+))
         (rot (not (equal (gkw "Rotate arrayed objects? [Yes/No] <Y>: " '("Yes" "No") "Yes") "No")))
         (full (< (abs (- (abs fill) +2pi+)) 1d-9))
         (step (if (> n 1) (/ fill (if full n (1- n))) 0d0)))
    (when c
      (loop for i from 1 below n
            for a = (* i step)
            do (transform-entities ents (if rot (xf-rotate c a)
                                             (let* ((ref (or (dxf 10 (ent-data (first ents))) c))
                                                    (moved (rot-pt (pt3 ref) c a)))
                                               (xf-move (v- moved (pt3 ref)))))
                                   :copy t)))))

(defcmd "ARRAY" ("AR" "-ARRAY") "Creates multiple copies in a rectangular or polar pattern."
  (let ((ents (get-selection)))
    (when ents
      (let ((k (gkw "Enter the type of array [Rectangular/Polar] <R>: " '("Rectangular" "Polar") "Rectangular")))
        (if (equal k "Polar") (polar-array ents) (rect-array ents))))))
(defcmd "ARRAYRECT" () "Rectangular array." (let ((ents (get-selection))) (when ents (rect-array ents))))
(defcmd "ARRAYPOLAR" () "Polar array." (let ((ents (get-selection))) (when ents (polar-array ents))))

;;; ---------- properties ----------

(defparameter *color-names*
  '(("RED" . 1) ("YELLOW" . 2) ("GREEN" . 3) ("CYAN" . 4) ("BLUE" . 5) ("MAGENTA" . 6) ("WHITE" . 7)
    ("BYLAYER" . 256) ("BYBLOCK" . 0)))

(defun parse-color (s)
  (let ((s (string-trim " " s)))
    (or (cdr (assoc s *color-names* :test #'string-equal))
        (let ((n (ignore-errors (parse-integer s)))) (and n (<= 0 n 256) n)))))

(defun set-entity-color (data c)
  (if (= c 256) (rem-dxf 62 data) (set-dxf 62 c data)))

(defcmd "CHPROP" ("CHANGE" "-CHPROP") "Changes the layer or color of objects."
  (let ((ents (get-selection)))
    (when ents
      (loop
        (let ((k (gkw "Enter property to change [Color/LAyer/LType/ltScale/LWeight]: " '("Color" "LAyer" "LType" "ltScale" "LWeight"))))
          (cond ((null k) (return))
                ((equal k "LType")
                 (let ((n (gstring "Enter new linetype name <ByLayer>: " t)))
                   (when (string= n "") (setf n "BYLAYER"))
                   (if (ensure-ltype n)
                       (dolist (e ents) (set-entity-data e (if (string-equal n "BYLAYER") (rem-dxf 6 (ent-data e)) (set-dxf 6 n (ent-data e)))))
                       (out (format nil "Linetype ~A not found. Use LINETYPE to load it.~%" n)))))
                ((equal k "ltScale")
                 (let ((v (greal "Specify new linetype scale <1.0000>: " :flags 6)))
                   (when v (dolist (e ents) (set-entity-data e (set-dxf 48 (d v) (ent-data e)))))))
                ((equal k "LWeight")
                 (let ((lw (parse-lweight-label (gstring "Enter new lineweight <ByLayer>: "))))
                   (dolist (e ents) (set-entity-data e (if (or (null lw) (= lw -1)) (rem-dxf 370 (ent-data e)) (set-dxf 370 lw (ent-data e)))))))
                ((equal k "Color")
                 (let ((c (parse-color (gstring "Enter new color [Truecolor/COlorbook] <BYLAYER>: "))))
                   (dolist (e ents) (set-entity-data e (set-entity-color (ent-data e) (or c 256))))))
                ((equal k "LAyer")
                 (let ((n (gstring (format nil "Enter new layer name <~A>: " (ent-layer (first ents))) t)))
                   (when (plusp (length n))
                     (let ((l (find-layer n)))
                       (if l (dolist (e ents) (set-entity-data e (set-dxf 8 (layer-name l) (ent-data e))))
                           (out (format nil "Cannot find layer \"~A\".~%" n)))))))))))))

;;; MATCHPROP: see matchprop.lisp

(defcmd "COLOR" ("COL" "-COLOR" "DDCOLOR") "Sets the color for new objects (the Select Color dialog in the window)."
  (let ((c (if (and (gui-p) (not (from-lisp-p)))
               (color-dialog (sysvar "CECOLOR"))
               (parse-color (gstring (format nil "Enter default object color [Truecolor/COlorbook] <~A>: " (sysvar "CECOLOR")))))))
    (when c (setf (sysvar "CECOLOR") (case c (256 "BYLAYER") (0 "BYBLOCK") (t (princ-to-string c)))))
    (when (and c (gui-p)) (gui-sync))))

(defcmd "DIVIDE" ("DIV") "Places evenly spaced point objects along a line, arc or circle."
  (let ((sel (get-user-input :entity :prompt "Select object to divide: ")))
    (when (consp sel)
      (let ((e (sel-entity sel)))
        (when e
          (let* ((n (or (gint "Enter the number of segments: " :flags 7) 2))
                 (pc (first (snap-pieces e))))
            (when pc
              (loop for i from 1 below n
                    for f = (/ i n)
                    for p = (ecase (first pc)
                              (:seg (at-param (second pc) (third pc) f))
                              (:arc (destructuring-bind (c r a1 a2) (rest pc) (polar c (+ a1 (* f (arc-sweep a1 a2))) r)))
                              (:circle (polar (second pc) (* f +2pi+) (third pc)))
                              (:pt (second pc)))
                    do (add (list (cons 0 "POINT") (cons 10 p)))))))))))

;;; clipboard commands: see clip.lisp

