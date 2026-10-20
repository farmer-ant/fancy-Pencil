;;;; modify2.lisp — polyline geometry helpers, TRIM/EXTEND on polylines,
;;;; FILLET and CHAMFER for lines, arcs, circles and polylines, PEDIT.

(in-package :lispdraft)

;;; ---------- polyline segments, parameterised as u = segment-index + fraction ----------

(defun pline-segs (data)
  "Vector of (p q bulge) segments of an LWPOLYLINE."
  (let* ((vs (lwpoly-vertices data)) (n (length vs)) (closed (lwpoly-closed-p data)))
    (coerce (loop for i from 0 below (if closed n (1- n))
                  collect (list (first (nth i vs)) (first (nth (mod (1+ i) n) vs)) (second (nth i vs))))
            'vector)))

(defun seg-piece (seg)
  (destructuring-bind (p q b) seg
    (if (< (abs b) 1d-12) (list :seg p q)
        (multiple-value-bind (c r a1 a2) (bulge->arc p q b) (list :arc c r a1 a2)))))

(defun seg-point-at (seg f)
  (destructuring-bind (p q b) seg
    (if (< (abs b) 1d-12) (at-param p q f)
        (multiple-value-bind (c r) (bulge->arc p q b)
          (polar c (+ (ang c p) (* f 4 (atan b))) r)))))

(defun seg-param-of (seg pnt)
  "Fraction along SEG of the point (projected onto the segment's line or circle)."
  (destructuring-bind (p q b) seg
    (if (< (abs b) 1d-12) (seg-param pnt p q)
        (multiple-value-bind (c) (bulge->arc p q b)
          (let* ((sweep (* 4 (atan b)))
                 (da (- (ang c pnt) (ang c p)))
                 (da (if (> sweep 0) (norm-ang da) (- (norm-ang (- da))))))
            ;; values just outside the arc map naturally beyond [0,1]
            (when (and (> sweep 0) (> da (+ sweep (/ (- +2pi+ sweep) 2)))) (decf da +2pi+))
            (when (and (< sweep 0) (< da (- sweep (/ (- +2pi+ (abs sweep)) 2)))) (incf da +2pi+))
            (/ da sweep))))))

(defun sub-seg-bulge (seg f1 f2)
  (let ((b (third seg))) (if (< (abs b) 1d-12) 0d0 (tan (/ (* (- f2 f1) 4 (atan b)) 4)))))

(defun pline-sub (segs u1 u2)
  "Vertices (point bulge) of the polyline portion from U1 to U2 (u1 < u2; may run past the last
segment for closed polylines, wrapping round)."
  (let ((n (length segs)) (verts nil) (u u1))
    (loop while (< u (- u2 1d-12))
          do (let* ((i (floor (+ u 1d-12)))
                    (f1 (max 0d0 (- u i)))
                    (f2 (min 1d0 (- u2 i)))
                    (seg (aref segs (mod i n))))
               (push (list (seg-point-at seg f1) (sub-seg-bulge seg f1 f2)) verts)
               (setf u (+ i f2))))
    (let* ((i (floor (- u2 1d-12))) (seg (aref segs (mod i n))))
      (push (list (seg-point-at seg (- u2 i)) 0d0) verts))
    (nreverse verts)))

(defun pline-params-of-cuts (segs cuts)
  (let ((us nil))
    (loop for i from 0 below (length segs)
          for pc = (seg-piece (aref segs i))
          do (dolist (c cuts)
               (dolist (q (piece-ints pc c))
                 (let ((f (seg-param-of (aref segs i) q)))
                   (when (<= -1d-9 f (+ 1 1d-9)) (push (+ i (max 0d0 (min 1d0 f))) us))))))
    (sort (remove-duplicates us :test (lambda (a b) (< (abs (- a b)) 1d-9))) #'<)))

(defun pline-param-near (segs pick)
  "Global parameter of the point of the polyline nearest PICK."
  (let ((best 0d0) (bd nil))
    (loop for i from 0 below (length segs)
          for pc = (seg-piece (aref segs i))
          for q = (piece-nearest pc pick)
          for dd = (dist2d q pick)
          when (or (null bd) (< dd bd))
            do (setf bd dd best (+ i (max 0d0 (min 1d0 (seg-param-of (aref segs i) q))))))
    best))

(defun pline-from-verts (verts data)
  (append (make-lwpoly-data verts nil :width (dxf 43 data)) (common-props data)))

(defun trim-polyline (e pick cuts)
  (let* ((data (ent-data e)) (segs (pline-segs data)) (n (length segs))
         (closed (lwpoly-closed-p data))
         (us (remove-if (lambda (u) (and (not closed) (or (< u 1d-9) (> u (- n 1d-9))))) (pline-params-of-cuts segs cuts)))
         (up (pline-param-near segs pick)))
    (cond
      ((null us) :no-intersection)
      (closed
       (if (< (length us) 2) :no-intersection
           (let* ((lo (or (find-if (lambda (u) (< u up)) us :from-end t) (- (car (last us)) n)))
                  (hi (or (find-if (lambda (u) (> u up)) us) (+ (first us) n))))
             ;; keep from HI round to LO (+n)
             (replace-with e (list (pline-from-verts (pline-sub segs hi (+ lo n)) data)))
             :done)))
      (t
       (let ((lo (find-if (lambda (u) (< u up)) us :from-end t))
             (hi (find-if (lambda (u) (> u up)) us)))
         (replace-with e (remove nil (list (and lo (> lo 1d-9) (pline-from-verts (pline-sub segs 0d0 lo) data))
                                           (and hi (< hi (- n 1d-9)) (pline-from-verts (pline-sub segs hi (float n 1d0)) data)))))
         :done)))))

(defun extend-polyline (e pick bounds)
  "Extend the end segment of an open polyline nearest PICK (straight end segments)."
  (let* ((data (ent-data e)))
    (when (lwpoly-closed-p data) (return-from extend-polyline :unsupported))
    (let* ((vs (lwpoly-vertices data)) (n (length vs))
           (at-start (< (dist2d pick (first (first vs))) (dist2d pick (first (car (last vs))))))
           (from (if at-start (first (second vs)) (first (nth (- n 2) vs))))
           (to (if at-start (first (first vs)) (first (car (last vs)))))
           (seg-b (if at-start (second (first vs)) (second (nth (- n 2) vs)))))
      (unless (< (abs seg-b) 1d-12) (return-from extend-polyline :unsupported))
      (let* ((cands (loop for bpc in bounds
                          append (loop for q in (piece-ints (list :seg from to) bpc t)
                                       for s = (seg-param q from to)
                                       when (and (on-piece-p bpc q 1d-7) (> s (+ 1 1d-9))) collect s)))
             (s (and cands (reduce #'min cands))))
        (if (null s) :no-intersection
            (progn (set-entity-data e (set-nth-code data 10 (if at-start 0 (1- n)) (at-param from to s))) :done))))))

;;; ---------- FILLET / CHAMFER ----------

(defun curve-of (e)
  "Infinite line or full circle for an entity: (:line a b) or (:circle c r)."
  (let ((d0 (ent-data e)))
    (cond ((equal (dxf 0 d0) "LINE") (list :line (pt3 (dxf 10 d0)) (pt3 (dxf 11 d0))))
          ((member (dxf 0 d0) '("ARC" "CIRCLE") :test #'equal) (list :circle (pt3 (dxf 10 d0)) (dxf 40 d0))))))

(defun curve-offsets (cv r)
  (ecase (first cv)
    (:line (destructuring-bind (a b) (rest cv)
             (let ((n (polar (pt 0 0) (+ (ang a b) (/ +pi+ 2)) r)))
               (list (list :line (v+ a n) (v+ b n)) (list :line (v- a n) (v- b n))))))
    (:circle (destructuring-bind (c rr) (rest cv)
               (remove nil (list (list :circle c (+ rr r)) (and (> (- rr r) 1d-9) (list :circle c (- rr r)))
                                 (and (> (- r rr) 1d-9) (list :circle c (- r rr)))))))))

(defun curve-ints (c1 c2)
  (flet ((pc (cv) (if (eq (first cv) :line) (list :seg (second cv) (third cv)) (list :circle (second cv) (third cv)))))
    (piece-ints (pc c1) (pc c2) t)))

(defun curve-foot (cv p)
  (if (eq (first cv) :line) (foot-on-line p (second cv) (third cv)) (polar (second cv) (ang (second cv) p) (third cv))))

(defun trim-curve-entity (e tpt pick)
  "Shorten or lengthen entity E so it ends at TPT, keeping the side where PICK is."
  (let* ((d0 (ent-data e)) (type (dxf 0 d0)))
    (cond
      ((equal type "LINE")
       (let* ((a (pt3 (dxf 10 d0))) (b (pt3 (dxf 11 d0))))
         (set-entity-data e (if (> (seg-param pick a b) (seg-param tpt a b)) (set-dxf 10 tpt d0) (set-dxf 11 tpt d0)))))
      ((equal type "ARC")
       (let* ((c (pt3 (dxf 10 d0))) (a1 (dxf 50 d0)) (a2 (dxf 51 d0)) (at (ang c tpt)) (ap (ang c pick))
              (opts (remove-if-not (lambda (o) (ang-in-arc-p ap (first o) (second o)))
                                   (list (list at a2) (list a1 at))))
              (best (first (sort opts #'< :key (lambda (o) (arc-sweep (first o) (second o)))))))
         (when best (set-entity-data e (set-dxf 51 (second best) (set-dxf 50 (first best) d0))))))
      (t nil))))

(defun fillet-two (e1 p1 e2 p2 r)
  "Fillet between two lines/arcs/circles. Returns T on success."
  (let ((c1 (curve-of e1)) (c2 (curve-of e2)))
    (unless (and c1 c2) (return-from fillet-two nil))
    (if (< r 1d-9)
        (let ((xs (curve-ints c1 c2)))
          (when xs
            (let ((x (first (sort (copy-list xs) #'< :key (lambda (q) (+ (dist2d q p1) (dist2d q p2)))))))
              (trim-curve-entity e1 x p1) (trim-curve-entity e2 x p2) t)))
        (let ((best nil) (score nil))
          (dolist (o1 (curve-offsets c1 r))
            (dolist (o2 (curve-offsets c2 r))
              (dolist (cen (curve-ints o1 o2))
                (let* ((t1 (curve-foot c1 cen)) (t2 (curve-foot c2 cen))
                       (sc (+ (dist2d t1 p1) (dist2d t2 p2))))
                  (when (or (null score) (< sc score)) (setf score sc best (list cen t1 t2)))))))
          (when best
            (destructuring-bind (cen t1 t2) best
              (unless (equal (ent-type e1) "CIRCLE") (trim-curve-entity e1 t1 p1))
              (unless (equal (ent-type e2) "CIRCLE") (trim-curve-entity e2 t2 p2))
              (let ((a1 (ang cen t1)) (a2 (ang cen t2)))
                (add (with-props (if (< (norm-ang (- a2 a1)) +pi+) (arc-data cen r a1 a2) (arc-data cen r a2 a1))
                                 (ent-data e1))))
              t))))))

(defun corner-vertex-fillet (verts k r &key chamfer (d2 r))
  "Replace vertex K of VERTS (both neighbouring segments straight) by a fillet arc or chamfer.
Returns new vertex list or NIL when it does not fit."
  (let* ((n (length verts))
         (a (first (nth (mod (1- k) n) verts))) (v (first (nth k verts))) (b (first (nth (mod (1+ k) n) verts)))
         (u1 (ang v a)) (u2 (ang v b))
         (theta (let ((dd (abs (- u1 u2)))) (if (> dd +pi+) (- +2pi+ dd) dd))))
    (when (or (< theta 1d-6) (> theta (- +pi+ 1d-6))
              (/= 0 (second (nth (mod (1- k) n) verts))) (/= 0 (second (nth k verts))))
      (return-from corner-vertex-fillet nil))
    (let* ((tl1 (if chamfer r (/ r (tan (/ theta 2))))) (tl2 (if chamfer d2 tl1)))
      (when (or (> tl1 (* 0.999 (dist2d v a))) (> tl2 (* 0.999 (dist2d v b)))) (return-from corner-vertex-fillet nil))
      (let* ((t1 (polar v u1 tl1)) (t2 (polar v u2 tl2))
             (left (> (cross2 a v b) 0))
             (bulge (if chamfer 0d0 (* (if left 1 -1) (tan (/ (- +pi+ theta) 4))))))
        (append (subseq verts 0 k) (list (list t1 bulge) (list t2 0d0)) (subseq verts (1+ k)))))))

(defun fillet-polyline (e r &key chamfer (d2 r))
  (let* ((data (ent-data e)) (verts (lwpoly-vertices data)) (closed (lwpoly-closed-p data)) (count 0))
    (loop with k = (if closed 0 1)
          while (< k (if closed (length verts) (1- (length verts))))
          do (let ((nv (corner-vertex-fillet verts k r :chamfer chamfer :d2 d2)))
               (if nv (progn (setf verts nv) (incf count) (incf k 2)) (incf k))))
    (when (plusp count)
      (set-entity-data e (append (make-lwpoly-data verts closed :width (dxf 43 data)) (common-props data))))
    count))

(defun pline-segment-index (data pick)
  (let ((segs (pline-segs data))) (floor (min (- (length segs) 1d-9) (pline-param-near segs pick)))))

(defun pick-fillet-object (prompt keywords)
  (loop
    (let ((r (get-user-input :entity :prompt prompt :keywords keywords)))
      (cond ((or (null r) (stringp r)) (return r))
            ((consp r)
             (let ((e (sel-entity r)))
               (cond ((null e) (out (format nil "No object found.~%")))
                     ((not (member (ent-type e) '("LINE" "ARC" "CIRCLE" "LWPOLYLINE") :test #'equal))
                      (out (format nil "Cannot fillet that object.~%")))
                     (t (return (list e (sel-point r)))))))))))

(defun fillet-or-chamfer (chamfer)
  (let ((multiple nil))
    (loop
      (if chamfer
          (out (format nil "(TRIM mode) Current chamfer Dist1 = ~A, Dist2 = ~A~%"
                       (fmt-len (or (sysvar "CHAMFERA") 0d0)) (fmt-len (or (sysvar "CHAMFERB") 0d0))))
          (out (format nil "Current settings: Mode = TRIM, Radius = ~A~%" (fmt-len (sysvar "FILLETRAD")))))
      (let ((s1 (pick-fillet-object (if chamfer "Select first line or [Polyline/Distance/Multiple]: "
                                        "Select first object or [Polyline/Radius/Multiple]: ")
                                    (if chamfer '("Polyline" "Distance" "Multiple") '("Polyline" "Radius" "Multiple")))))
        (cond
          ((null s1) (return))
          ((equal s1 "Multiple") (setf multiple t))
          ((equal s1 "Radius")
           (let ((r (gdist (format nil "Specify fillet radius <~A>: " (fmt-len (sysvar "FILLETRAD"))) :flags 4)))
             (when r (setf (sysvar "FILLETRAD") r))))
          ((equal s1 "Distance")
           (let* ((a (or (gdist (format nil "Specify first chamfer distance <~A>: " (fmt-len (or (sysvar "CHAMFERA") 0d0))) :flags 4)
                         (sysvar "CHAMFERA") 0d0))
                  (b (or (gdist (format nil "Specify second chamfer distance <~A>: " (fmt-len a)) :flags 4) a)))
             (setf (sysvar "CHAMFERA") (d a) (sysvar "CHAMFERB") (d b))))
          ((equal s1 "Polyline")
           (let ((e (sel-entity (get-user-input :entity :prompt "Select 2D polyline: "))))
             (if (and e (equal (ent-type e) "LWPOLYLINE"))
                 (let ((n (fillet-polyline e (if chamfer (d (or (sysvar "CHAMFERA") 0d0)) (sysvar "FILLETRAD"))
                                           :chamfer chamfer :d2 (d (or (sysvar "CHAMFERB") 0d0)))))
                   (out (format nil "~D line~:P ~:[were~;was~] ~A~%" n (= n 1) (if chamfer "chamfered" "filleted"))))
                 (out (format nil "Object selected is not a polyline.~%")))
             (unless multiple (return))))
          (t
           (ui-highlight (list (first s1)) t)
           (let ((s2 (unwind-protect (pick-fillet-object (if chamfer "Select second line: " "Select second object: ") nil)
                       (ui-highlight (list (first s1)) nil))))
             (when (consp s2)
               (destructuring-bind (e1 p1) s1
                 (destructuring-bind (e2 p2) s2
                   (cond
                     ;; two segments of the same polyline sharing a vertex
                     ((and (eq e1 e2) (equal (ent-type e1) "LWPOLYLINE"))
                      (let* ((data (ent-data e1)) (i (pline-segment-index data p1)) (j (pline-segment-index data p2))
                             (n (length (lwpoly-vertices data))) (closed (lwpoly-closed-p data))
                             (k (cond ((= j (1+ i)) j) ((= i (1+ j)) i)
                                      ((and closed (= i (1- n)) (= j 0)) 0) ((and closed (= j (1- n)) (= i 0)) 0))))
                        (if k
                            (let ((nv (corner-vertex-fillet (lwpoly-vertices data) k
                                                            (if chamfer (d (or (sysvar "CHAMFERA") 0d0)) (sysvar "FILLETRAD"))
                                                            :chamfer chamfer :d2 (d (or (sysvar "CHAMFERB") 0d0)))))
                              (if nv (set-entity-data e1 (append (make-lwpoly-data nv closed :width (dxf 43 data)) (common-props data)))
                                  (out (format nil "Distance is too large.~%"))))
                            (out (format nil "Select two adjacent segments.~%")))))
                     ((eq e1 e2) (aerr "Cannot ~A an object to itself." (if chamfer "chamfer" "fillet")))
                     ((or (equal (ent-type e1) "LWPOLYLINE") (equal (ent-type e2) "LWPOLYLINE"))
                      (out (format nil "Use the Polyline option, or explode the polyline, to ~A it with another object.~%"
                                   (if chamfer "chamfer" "fillet"))))
                     (chamfer
                      (if (and (equal (ent-type e1) "LINE") (equal (ent-type e2) "LINE"))
                          (chamfer-lines e1 p1 e2 p2)
                          (out (format nil "Chamfer requires two lines.~%"))))
                     ((not (fillet-two e1 p1 e2 p2 (d (sysvar "FILLETRAD"))))
                      (out (format nil "Radius is too large, or the objects do not meet.~%")))))))
             (unless multiple (return)))))))))

(defun chamfer-lines (e1 p1 e2 p2)
  (let* ((d1 (ent-data e1)) (d2 (ent-data e2))
         (x (line-line-int (pt3 (dxf 10 d1)) (pt3 (dxf 11 d1)) (pt3 (dxf 10 d2)) (pt3 (dxf 11 d2)) nil)))
    (if (null x) (out (format nil "Lines are parallel.~%"))
        (flet ((far (dd p) (let ((a (pt3 (dxf 10 dd))) (b (pt3 (dxf 11 dd))))
                             (if (>= (seg-param p a b) (seg-param x a b)) b a))))
          (let* ((f1 (far d1 p1)) (f2 (far d2 p2))
                 (da (d (or (sysvar "CHAMFERA") 0d0))) (db (d (or (sysvar "CHAMFERB") 0d0)))
                 (q1 (polar x (ang x f1) da)) (q2 (polar x (ang x f2) db)))
            (set-entity-data e1 (set-dxf 11 f1 (set-dxf 10 q1 d1)))
            (set-entity-data e2 (set-dxf 11 f2 (set-dxf 10 q2 d2)))
            (when (> (dist2d q1 q2) 1d-9) (add (with-props (line-data q1 q2) d1))))))))

(defcmd "FILLET" ("F") "Rounds and fillets the edges of objects." (fillet-or-chamfer nil))
(defcmd "CHAMFER" ("CHA") "Bevels the edges of objects." (fillet-or-chamfer t))

;;; ---------- PEDIT ----------

(defun to-polyline (e)
  "Turn a LINE or ARC into an LWPOLYLINE in place; returns the new entity."
  (let ((ch (entity-chain-pieces e)))
    (when ch
      (let ((new (add-entity (append (make-lwpoly-data (append (mapcar (lambda (s) (list (first s) (third s))) ch)
                                                               (list (list (second (car (last ch))) 0d0)))
                                                       nil)
                                     (common-props (ent-data e))))))
        (delete-entity e)
        new))))

(defun pedit-one (e)
  (loop
    (let* ((data (ent-data e))
           (k (gkw (format nil "Enter an option [~:[Close~;Open~]/Join/Width/Decurve/Ltype gen/Reverse/Undo]: " (lwpoly-closed-p data))
                   (list (if (lwpoly-closed-p data) "Open" "Close") "Join" "Width" "Decurve" "Ltype" "Reverse" "Undo"))))
      (cond
        ((null k) (return))
        ((equal k "Close") (set-entity-data e (set-dxf 70 (logior 1 (or (dxf 70 data) 0)) data)))
        ((equal k "Open") (set-entity-data e (set-dxf 70 (logand (lognot 1) (or (dxf 70 data) 0)) data)))
        ((equal k "Join")
         (let ((others (remove-if-not #'entity-chain-pieces (select-objects :prompt "Select objects to join: "))))
           (if (lwpoly-closed-p data) (out (format nil "Cannot join to a closed polyline.~%"))
               (multiple-value-bind (new n) (join-entities (cons e (remove e others)) :tol (max 1d-6 (or (sysvar "PEDITFUZZ") 0d0)))
                 (setf e new)
                 (out (format nil "~D segment~:P added to polyline~%" (1- n)))))))
        ((equal k "Width")
         (let ((wd (gdist "Specify new width for all segments: " :flags 4)))
           (when wd (set-entity-data e (if (zerop wd) (rem-dxf 43 data) (set-dxf 43 (d wd) data))))))
        ((equal k "Decurve") (set-entity-data e (rem-dxf 42 data)))
        ((equal k "Ltype")
         (let ((on (equal "ON" (gkw "Enter polyline linetype generation option [ON/OFF] <Off>: " '("ON" "OFF") "OFF"))))
           (set-entity-data e (set-dxf 70 (if on (logior 128 (or (dxf 70 data) 0)) (logand (lognot 128) (or (dxf 70 data) 0))) data))))
        ((equal k "Reverse")
         (let* ((vs (lwpoly-vertices data)) (closed (lwpoly-closed-p data))
                (n (length vs))
                (rev (loop for i from (1- n) downto 0
                           for bulge = (if (or (> i 0) closed) (- (second (nth (mod (1- i) n) vs))) 0d0)
                           collect (list (first (nth i vs)) (if (and (= i 0) (not closed)) 0d0 bulge)))))
           (set-entity-data e (append (make-lwpoly-data rev closed :width (dxf 43 data)) (common-props data)))
           (out (format nil "Polyline reversed~%"))))
        ((equal k "Undo") (undo-one))))))

(defcmd "PEDIT" ("PE") "Edits polylines."
  (let ((sel (get-user-input :entity :prompt "Select polyline or [Multiple]: " :keywords '("Multiple"))))
    (cond
      ((equal sel "Multiple") (mpedit))
      ((consp sel)
       (let ((e (sel-entity sel)))
         (cond ((null e) nil)
               ((equal (ent-type e) "LWPOLYLINE") (pedit-one e))
               ((member (ent-type e) '("LINE" "ARC") :test #'equal)
                (when (equal "Yes" (gkw "Object selected is not a polyline. Do you want to turn it into one? <Y> " '("Yes" "No") "Yes"))
                  (pedit-one (to-polyline e))))
               (t (out (format nil "Object selected is not a polyline.~%")))))))))

(defun mpedit ()
  (let* ((ents (select-objects)) (plines nil))
    (dolist (e ents)
      (cond ((equal (ent-type e) "LWPOLYLINE") (push e plines))
            ((member (ent-type e) '("LINE" "ARC") :test #'equal) (push e plines))))
    (when plines
      (when (some (lambda (e) (not (equal (ent-type e) "LWPOLYLINE"))) plines)
        (when (equal "Yes" (gkw "Convert Lines and Arcs to polylines [Yes/No]? <Y> " '("Yes" "No") "Yes"))
          (setf plines (mapcar (lambda (e) (if (equal (ent-type e) "LWPOLYLINE") e (to-polyline e))) plines))))
      (setf plines (remove-if-not (lambda (e) (and (ent-live-p e) (equal (ent-type e) "LWPOLYLINE"))) plines))
      (loop
        (let ((k (gkw "Enter an option [Close/Open/Join/Width/Decurve/Ltype gen/Reverse]: "
                      '("Close" "Open" "Join" "Width" "Decurve" "Ltype" "Reverse"))))
          (cond ((null k) (return))
                ((equal k "Join")
                 (let ((n (length plines)))
                   (when (> n 1) (join-entities plines :tol (max 1d-6 (or (sysvar "PEDITFUZZ") 0d0))))
                   (setf plines (remove-if-not #'ent-live-p plines))
                   (out (format nil "~D polylines joined~%" n))))
                ((equal k "Width")
                 (let ((wd (gdist "Specify new width for all segments: " :flags 4)))
                   (when wd (dolist (e plines) (set-entity-data e (if (zerop wd) (rem-dxf 43 (ent-data e)) (set-dxf 43 (d wd) (ent-data e))))))))
                (t (dolist (e plines)
                     (let ((data (ent-data e)))
                       (set-entity-data e
                                        (cond ((equal k "Close") (set-dxf 70 (logior 1 (or (dxf 70 data) 0)) data))
                                              ((equal k "Open") (set-dxf 70 (logand (lognot 1) (or (dxf 70 data) 0)) data))
                                              ((equal k "Decurve") (rem-dxf 42 data))
                                              (t data))))))))))))
(defcmd "MPEDIT" () "Express Tools: edits several polylines at once." (mpedit))
