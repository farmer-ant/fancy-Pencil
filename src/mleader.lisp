;;;; mleader.lisp — AutoCAD multileaders (MULTILEADER, made by MLEADER in AutoCAD 2008 and later).
;;;; A multileader is one object: one or more leaders (each with its leader lines, an arrowhead on
;;;; each line, and a landing "dogleg" by the content) and its content, a note (MTEXT) or a block.
;;;; It is kept as (0 . "MULTILEADER") with everything else in a property list under :ml:
;;;;   :leaders  list of (:conn p :dir v :len n :lines ((p ...) ...))   conn = where lines meet the landing
;;;;   :content  2 note, 1 block, 0 none
;;;;   :text :tpos :th :tw :attach :trot :tstyle :tcolor :frame          the note (MTEXT)
;;;;   :block :bpos :bscale :brot :bcolor                                 a block instead of a note
;;;;   :type (0 none, 1 straight, 2 spline) :asz :arrow :landing :gap :lcolor :lweight :ltype :scale
;;;; Colours are ACI numbers, 0 for ByBlock, NIL for ByLayer. It is drawn from pieces made on the
;;;; fly (mleader-display-entities) and written back to DXF as a MULTILEADER.

(in-package :fancy-pencil)

(defun ml (data key &optional default)
  (let ((v (getf (cdr (assoc :ml data)) key :none)))
    (if (eq v :none) default v)))

(defun ml-plist (data) (cdr (assoc :ml data)))

(defun set-ml-plist (data plist)
  (append (remove :ml data :key (lambda (p) (and (consp p) (car p)))) (list (cons :ml plist))))

;;; ---------- reading from DXF ----------


(defun table-record-by-handle (type handle)
  (and handle
       (find-if (lambda (r) (and (equal (cdr (first r)) type)
                                 (equalp (princ-to-string (dxf 5 r)) (princ-to-string handle))))
                *dxf-tables*)))

(defun ml-merge-points (pairs)
  "Merge X/Y/Z groups into points: codes 10-18 and 110-112 (with +10 and +20)."
  (let ((out nil))
    (loop while pairs do
      (let* ((pr (pop pairs)) (code (car pr)))
        (if (or (<= 10 code 18) (<= 110 code 112))
            (let ((x (cdr pr)) (y 0d0) (z 0d0))
              (when (and pairs (= (car (first pairs)) (+ code 10))) (setf y (cdr (pop pairs))))
              (when (and pairs (= (car (first pairs)) (+ code 20))) (setf z (cdr (pop pairs))))
              (push (cons code (pt (d x) (d y) (d z))) out))
            (push pr out))))
    (nreverse out)))

(defun ml-color (v)
  "An AutoCAD colour value (AcCmColor, as in groups 90-93): an ACI number, 0 for ByBlock, NIL for ByLayer."
  (if (not (integerp v)) nil
      (let* ((u (logand v #xFFFFFFFF)) (method (ash u -24)))
        (case method
          (#xC0 nil)
          (#xC1 0)
          (#xC3 (let ((i (logand u #xFF))) (if (<= 1 i 255) i nil)))
          (#xC2 (rgb-to-aci (logand (ash u -16) #xFF) (logand (ash u -8) #xFF) (logand u #xFF)))
          (t nil)))))

(defun rgb-to-aci (r g b)
  "The nearest of AutoCAD's 255 index colours."
  (or (ignore-errors
       (let (best bd)
         (loop for i from 1 to 255
               do (destructuring-bind (r2 g2 b2) (hex-rgb (aci->hex i))
                    (let ((dd (+ (expt (- r r2) 2) (expt (- g g2) 2) (expt (- b b2) 2))))
                      (when (or (null bd) (< dd bd)) (setf bd dd best i)))))
         best))
      7))

(defun ml-color-value (c)
  "Our colour (ACI, 0 ByBlock, NIL ByLayer) as an AcCmColor number for DXF."
  (cond ((null c) -1073741824)               ; #xC0000000 ByLayer
        ((eql c 0) -1056964608)              ; #xC1000000 ByBlock
        (t (- (logior #xC3000000 c) #x100000000))))

(defun mleader-from-dxf (g)
  "Read a MULTILEADER from its raw DXF groups G."
  (let* ((pairs (ml-merge-points g))
         (data (list (cons 0 "MULTILEADER")))
         (pl nil) (leaders nil) (leader nil) (line nil)
         (state :entity) (texts nil))
    (flet ((put (k v) (setf (getf pl k) v)))
      (dolist (p pairs)
        (let ((c (car p)) (v (cdr p)))
          (case state
            (:entity
             (cond ((and (eql c 300) (equal v "CONTEXT_DATA{")) (setf state :context))
                   ((member c '(8 6 62 370 48 60 420)) (push p data))
                   ((eql c 340) (put :mlstyle v))
                   ((eql c 170) (put :type v))
                   ((eql c 91) (put :lcolor (ml-color v)))
                   ((eql c 341) (let ((r (table-record-by-handle "LTYPE" v))) (when r (put :ltype (dxf 2 r)))))
                   ((eql c 171) (put :lweight v))
                   ((eql c 290) (put :landing (eql v 1)))
                   ((eql c 291) (put :dogleg (eql v 1)))
                   ((eql c 342) (let ((r (table-record-by-handle "BLOCK_RECORD" v)))
                                  (when r (put :arrow-name (dxf 2 r)))))
                   ((eql c 42) (put :asz-entity (d v)))
                   ((eql c 172) (put :content v))
                   ((eql c 343) (let ((r (table-record-by-handle "STYLE" v))) (when r (put :tstyle-entity (dxf 2 r)))))
                   ((eql c 92) (put :tcolor-entity (ml-color v)))
                   ((eql c 292) (put :frame (eql v 1)))
                   ((eql c 344) (let ((r (table-record-by-handle "BLOCK_RECORD" v))) (when r (put :block-entity (dxf 2 r)))))
                   ((eql c 93) (put :bcolor-entity (ml-color v)))
                   ((eql c 45) (put :scale (d v)))))
            (:context
             (cond ((and (eql c 301)) (setf state :entity))
                   ((and (eql c 302) (equal v "LEADER{"))
                    (setf state :leader leader (list :lines nil)))
                   ((eql c 40) (put :cscale (d v)))
                   ((eql c 41) (put :th (d v)))
                   ((eql c 140) (put :asz (d v)))
                   ((eql c 145) (put :gap (d v)))
                   ((eql c 290) (put :has-text (eql v 1)))
                   ((eql c 304) (push v texts))
                   ((eql c 340) (let ((r (table-record-by-handle "STYLE" v))) (when r (put :tstyle (dxf 2 r)))))
                   ((eql c 12) (put :tpos v))
                   ((eql c 13) (put :tdir v))
                   ((eql c 42) (put :trot-c (d v)))
                   ((eql c 43) (put :tw (d v)))
                   ((eql c 90) (put :tcolor (ml-color v)))
                   ((eql c 171) (put :attach v))
                   ((eql c 296) (put :has-block (eql v 1)))
                   ((eql c 341) (let ((r (table-record-by-handle "BLOCK_RECORD" v))) (when r (put :block (dxf 2 r)))))
                   ((eql c 15) (put :bpos v))
                   ((eql c 16) (put :bscale v))
                   ((eql c 46) (put :brot (d v)))
                   ((eql c 93) (put :bcolor (ml-color v)))))
            (:leader
             (cond ((and (eql c 303)) (push leader leaders) (setf state :context))
                   ((and (eql c 304) (equal v "LEADER_LINE{")) (setf state :line line nil))
                   ((eql c 10) (setf (getf leader :conn) v))
                   ((eql c 11) (setf (getf leader :dir) v))
                   ((eql c 40) (setf (getf leader :len) (d v)))))
            (:line
             (cond ((eql c 305)
                    (setf (getf leader :lines) (append (getf leader :lines) (list (nreverse line))))
                    (setf state :leader))
                   ((eql c 10) (push v line)))))))
      ;; the note: one text in the context data (long ones may come in several pieces)
      (let ((tx (apply #'concatenate 'string (nreverse texts))))
        (put :text tx))
      (put :leaders (nreverse leaders))
      ;; settings the context data doesn't hold come from the entity's own
      (unless (getf pl :tstyle) (put :tstyle (or (getf pl :tstyle-entity) "Standard")))
      (unless (getf pl :asz) (put :asz (or (getf pl :asz-entity) 0.18d0)))
      (unless (getf pl :block) (when (getf pl :block-entity) (put :block (getf pl :block-entity))))
      (unless (member :tcolor pl) (put :tcolor (getf pl :tcolor-entity 0)))
      (put :trot (let ((dv (getf pl :tdir)))
                   (if (and dv (> (+ (expt (px dv) 2) (expt (py dv) 2)) 1d-12))
                       (atan (py dv) (px dv))
                       (or (getf pl :trot-c) 0d0))))
      (put :arrow (let ((n (getf pl :arrow-name))) (if (and n (known-arrow-name-p n)) (arrow-code n) "")))
      (unless (getf pl :content)
        (put :content (cond ((getf pl :has-text) 2) ((getf pl :has-block) 1) (t 0))))
      (unless (member :type pl) (put :type 1))
      (unless (member :landing pl) (put :landing t))
      (unless (member :dogleg pl) (put :dogleg t))
      (unless (getf pl :th) (put :th 0.18d0))
      (unless (getf pl :attach) (put :attach 1))
      (dolist (k '(:tstyle-entity :asz-entity :block-entity :tcolor-entity :bcolor-entity :arrow-name
                   :has-text :has-block :tdir :trot-c))
        (remf pl k)))
    (append (nreverse data) (list (cons :ml pl)))))

;;; ---------- what it looks like ----------

(defun ml-sub (data sub color)
  "A display piece of multileader DATA: on its layer, in COLOR (ACI, 0 ByBlock, NIL ByLayer)."
  (let ((s (append sub (list (cons 8 (or (dxf 8 data) "0"))))))
    (if color (append s (list (cons 62 color))) s)))

(defun ml-text-data (data)
  "The multileader's note as an MTEXT entity, or NIL."
  (when (and (eql (ml data :content) 2) (plusp (length (or (ml data :text) ""))) (ml data :tpos))
    (append (list (cons 0 "MTEXT") (cons 10 (pt3 (ml data :tpos)))
                  (cons 40 (d (ml data :th 0.18d0))) (cons 41 (d (ml data :tw 0d0)))
                  (cons 71 (ml data :attach 1)) (cons 72 1) (cons 1 (ml data :text))
                  (cons 50 (d (ml data :trot 0d0))) (cons 7 (or (ml data :tstyle) "Standard"))
                  (cons 8 (or (dxf 8 data) "0")))
            (let ((c (ml data :tcolor))) (and c (list (cons 62 c)))))))

(defun ml-block-data (data)
  (when (and (eql (ml data :content) 1) (ml data :block) (find-block (ml data :block)) (ml data :bpos))
    (let ((s (or (ml data :bscale) (pt 1 1 1))))
      (append (list (cons 0 "INSERT") (cons 2 (ml data :block)) (cons 10 (pt3 (ml data :bpos)))
                    (cons 41 (d (px s))) (cons 42 (d (py s))) (cons 43 (d (pz s)))
                    (cons 50 (d (ml data :brot 0d0))) (cons 8 (or (dxf 8 data) "0")))
              (let ((c (ml data :bcolor))) (and c (list (cons 62 c))))))))

(defun mleader-display-entities (data)
  "The lines, arrowheads, landings and note (or block) that show multileader DATA."
  (let* ((type (ml data :type 1))
         (lcol (ml data :lcolor 0))
         (asz (d (ml data :asz 0.18d0)))
         (vars (list :dimscale 1d0 :dimasz asz :dimtsz 0d0 :dimclrd 0 :dimblk (or (ml data :arrow) "")))
         (out nil))
    (unless (eql type 0)
      (dolist (ldr (ml data :leaders))
        (let ((conn (getf ldr :conn)))
          (dolist (verts (getf ldr :lines))
            (let* ((pts (append (mapcar #'pt3 verts) (and conn (list (pt3 conn)))))
                   (pts (remove-duplicates pts :test (lambda (a b) (< (dist3 a b) 1d-9)) :from-end t))
                   (path (if (eql type 2) (catmull-rom pts) pts)))
              (when (>= (length path) 2)
                (push (ml-sub data (make-lwpoly-data path nil) lcol) out)
                (when (> asz 0)
                  (dolist (a (dim-arrow (first path) (second path) vars))
                    (push (ml-sub data (rem-dxf 62 a) lcol) out))))))
          ;; the landing: from where the lines meet, toward the note
          (when (and conn (ml data :landing t) (ml data :dogleg t)
                     (getf ldr :dir) (getf ldr :len) (> (getf ldr :len) 0))
            (let* ((dv (getf ldr :dir)) (n (sqrt (+ (expt (px dv) 2) (expt (py dv) 2) (expt (pz dv) 2)))))
              (when (> n 1d-12)
                (push (ml-sub data (line-data conn (v+ (pt3 conn) (pt (* (/ (px dv) n) (getf ldr :len))
                                                                      (* (/ (py dv) n) (getf ldr :len))
                                                                      (* (/ (pz dv) n) (getf ldr :len)))))
                              lcol)
                      out)))))))
    (setf out (nreverse out))
    (let ((tx (ml-text-data data)) (bk (ml-block-data data)))
      (when tx
        (setf out (append out (list tx)))
        (when (ml data :frame)
          (let ((c (text-box tx)))
            (setf out (append out (list (ml-sub data (make-lwpoly-data c t) (ml data :tcolor 0))))))))
      (when bk (setf out (append out (list bk)))))
    out))

(defun dist3 (a b)
  (let ((a (pt3 a)) (b (pt3 b)))
    (sqrt (+ (expt (- (px a) (px b)) 2) (expt (- (py a) (py b)) 2) (expt (- (pz a) (pz b)) 2)))))

(defun mleader-pieces (data)
  (loop for sub in (mleader-display-entities data) append (data-pieces sub)))

;;; ---------- moving, rotating, scaling, mirroring ----------

(defun xform-mleader (data xf)
  (let* ((fp (xf-fn xf)) (fa (xf-ang xf)) (s (d (or (xf-scale xf) 1d0)))
         (pl (copy-list (ml-plist data)))
         (o (funcall fp (pt 0 0 0))))
    (flet ((pnt (p) (and p (funcall fp (pt3 p))))
           (vec (v) (and v (v- (funcall fp (pt3 v)) o))))
      (setf (getf pl :leaders)
            (mapcar (lambda (l)
                      (let* ((dv (getf l :dir))
                             (nd (and dv (let ((w (vec dv)))
                                           (let ((n (sqrt (+ (expt (px w) 2) (expt (py w) 2) (expt (pz w) 2)))))
                                             (if (> n 1d-12) (pt (/ (px w) n) (/ (py w) n) (/ (pz w) n)) dv))))))
                        (list :conn (pnt (getf l :conn)) :dir nd :len (and (getf l :len) (* s (getf l :len)))
                              :lines (mapcar (lambda (ln) (mapcar #'pnt ln)) (getf l :lines)))))
                    (getf pl :leaders)))
      (when (getf pl :tpos) (setf (getf pl :tpos) (pnt (getf pl :tpos))))
      (when (getf pl :bpos) (setf (getf pl :bpos) (pnt (getf pl :bpos))))
      (setf (getf pl :trot) (funcall fa (d (getf pl :trot 0d0))))
      (when (getf pl :brot) (setf (getf pl :brot) (funcall fa (d (getf pl :brot)))))
      (when (/= s 1)
        (dolist (k '(:th :tw :asz :gap))
          (when (getf pl k) (setf (getf pl k) (* s (getf pl k)))))
        (when (getf pl :bscale)
          (let ((b (pt3 (getf pl :bscale)))) (setf (getf pl :bscale) (pt (* s (px b)) (* s (py b)) (* s (pz b))))))
        (setf (getf pl :scale) (* s (d (getf pl :scale 1d0)))))
      ;; mirrored: the note stays readable (it is moved, not flipped), as with MIRRTEXT 0
      (when (and (xf-mirror xf) (getf pl :attach))
        (let* ((a (getf pl :attach)) (col (mod (1- a) 3)))
          (setf (getf pl :attach) (+ (* 3 (floor (1- a) 3)) (- 2 col) 1)))
        (setf (getf pl :trot) (norm-ang (+ (getf pl :trot) +pi+)))))
    (set-ml-plist data pl)))

;;; ---------- writing DXF ----------


(defun ml-handle (key arg)
  (let ((f (getf *ml-handles* key))) (and f (funcall f arg))))

(defun write-mleader-2000 (out data)
  (let* ((leaders (ml data :leaders))
         (content (ml data :content 2))
         (text-p (eql content 2))
         (block-p (and (eql content 1) (ml data :block)))
         (style-h (or (ml-handle :style (or (ml data :tstyle) "Standard")) (ml-handle :style "Standard")))
         (block-h (and block-p (ml-handle :block (ml data :block))))
         (arrow-h (let ((a (ml data :arrow ""))) (and (plusp (length a)) (ml-handle :arrow a))))
         (ltype-h (ml-handle :ltype (or (ml data :ltype) "ByBlock")))
         (th (d (ml data :th 0.18d0))) (asz (d (ml data :asz 0.18d0))) (gap (d (ml data :gap 0.09d0)))
         (trot (d (ml data :trot 0d0)))
         (tpos (or (ml data :tpos) (let ((l (first leaders))) (and l (getf l :conn))) (pt 0 0)))
         (base (or (let ((l (first leaders))) (and l (getf l :conn))) tpos))
         (doglen (or (let ((l (first leaders))) (and l (getf l :len))) (* 2 asz))))
    (w out 100 "AcDbMLeader")
    (w out 270 2)
    (w out 300 "CONTEXT_DATA{")
    (w out 40 (d (ml data :scale 1d0)))
    (wp out 10 (pt3 base))
    (w out 41 th) (w out 140 asz) (w out 145 gap)
    (w out 174 1) (w out 175 1) (w out 176 0) (w out 177 0)
    (w out 290 (if text-p 1 0))
    (when text-p
      (let ((chunks (split-chunks (dxf-escape (ml data :text "")) 250)))
        (if (null chunks) (w out 304 "")
            (dolist (c chunks) (format out "304~%~A~%" c))))
      (wp out 11 (pt 0 0 1))
      (when style-h (w out 340 style-h))
      (wp out 12 (pt3 tpos))
      (wp out 13 (pt (cos trot) (sin trot) 0))
      (w out 42 trot)
      (w out 43 (d (ml data :tw 0d0))) (w out 44 0d0) (w out 45 1d0)
      (w out 170 1)
      (w out 90 (ml-color-value (ml data :tcolor 0)))
      (w out 171 (ml data :attach 1)) (w out 172 1)
      (w out 91 -939524096) (w out 141 1.5d0) (w out 92 0)
      (w out 291 0) (w out 292 0) (w out 173 0) (w out 293 0)
      (w out 142 0d0) (w out 143 0d0) (w out 294 0) (w out 295 0))
    (w out 296 (if block-p 1 0))
    (when block-p
      (when block-h (w out 341 block-h))
      (wp out 14 (pt 0 0 1))
      (wp out 15 (pt3 (or (ml data :bpos) tpos)))
      (wp out 16 (pt3 (or (ml data :bscale) (pt 1 1 1))))
      (w out 46 (d (ml data :brot 0d0)))
      (w out 93 (ml-color-value (ml data :bcolor 0)))
      (let* ((s (pt3 (or (ml data :bscale) (pt 1 1 1)))) (r (d (ml data :brot 0d0)))
             (c (cos r)) (sn (sin r)) (p (pt3 (or (ml data :bpos) tpos))))
        (dolist (x (list (* c (px s)) (- (* sn (py s))) 0d0 (px p)
                         (* sn (px s)) (* c (py s)) 0d0 (py p)
                         0d0 0d0 (pz s) (pz p)
                         0d0 0d0 0d0 1d0))
          (w out 47 (d x)))))
    (wp out 110 (pt3 base)) (wp out 111 (pt 1 0 0)) (wp out 112 (pt 0 1 0))
    (w out 297 0)
    (loop for l in leaders for i from 0
          do (w out 302 "LEADER{")
             (w out 290 1) (w out 291 1)
             (wp out 10 (pt3 (or (getf l :conn) base)))
             (wp out 11 (pt3 (or (getf l :dir) (pt 1 0 0))))
             (w out 90 i)
             (w out 40 (d (or (getf l :len) doglen)))
             (loop for ln in (getf l :lines) for j from 0
                   do (w out 304 "LEADER_LINE{")
                      (dolist (v ln) (wp out 10 (pt3 v)))
                      (w out 91 j)
                      (w out 305 "}"))
             (w out 271 0)
             (w out 303 "}"))
    (w out 272 9) (w out 273 9)
    (w out 301 "}")
    (let ((ms (getf *ml-handles* :mlstyle))) (when ms (w out 340 ms)))
    (w out 90 6816768)
    (w out 170 (ml data :type 1))
    (w out 91 (ml-color-value (ml data :lcolor 0)))
    (when ltype-h (w out 341 ltype-h))
    (w out 171 (or (ml data :lweight) -2))
    (w out 290 (if (ml data :landing t) 1 0))
    (w out 291 (if (ml data :dogleg t) 1 0))
    (w out 41 (d doglen))
    (when arrow-h (w out 342 arrow-h))
    (w out 42 asz)
    (w out 172 content)
    (when style-h (w out 343 style-h))
    (w out 173 1) (w out 95 1) (w out 174 1) (w out 175 0)
    (w out 92 (ml-color-value (ml data :tcolor 0)))
    (w out 292 (if (ml data :frame) 1 0))
    (when block-h (w out 344 block-h))
    (w out 93 (ml-color-value (ml data :bcolor 0)))
    (wp out 10 (pt 1 1 1))
    (w out 43 0d0)
    (w out 176 0)
    (w out 293 0) (w out 294 0) (w out 178 0) (w out 179 1)
    (w out 45 (d (ml data :scale 1d0)))
    (w out 271 0) (w out 272 9) (w out 273 9)))

(defun drawing-has-mleaders-p ()
  (or (some (lambda (e) (equal (ent-type e) "MULTILEADER")) (all-live-entities))
      (some (lambda (b) (some (lambda (dd) (equal (dxf 0 dd) "MULTILEADER")) (block-def-entities b)))
            (drawing-blocks *dwg*))))

(defun mleader-arrow-codes ()
  "Arrowheads used by multileaders (other than the default)."
  (let ((out nil))
    (flet ((note (dd) (when (equal (dxf 0 dd) "MULTILEADER")
                        (let ((a (ml dd :arrow ""))) (when (plusp (length a)) (pushnew a out :test #'string=))))))
      (dolist (e (all-live-entities)) (note (ename-data e)))
      (dolist (b (drawing-blocks *dwg*)) (mapc #'note (block-def-entities b))))
    out))

(defun write-mleader-classes-2000 (out)
  (w out 0 "CLASS") (w out 1 "MLEADERSTYLE") (w out 2 "AcDbMLeaderStyle") (w out 3 "ACDB_MLEADERSTYLE_CLASS")
  (w out 90 4095) (w out 280 0) (w out 281 0)
  (w out 0 "CLASS") (w out 1 "MULTILEADER") (w out 2 "AcDbMLeader") (w out 3 "ACDB_MLEADER_CLASS")
  (w out 90 3071) (w out 280 0) (w out 281 1))

(defun write-mleaderstyle-2000 (out h dict ltype-h style-h)
  "The Standard multileader style (sized from the current dimension style)."
  (let* ((v (current-dimvars))
         (sc (let ((s (dv v :dimscale))) (if (and (realp s) (plusp s)) (d s) 1d0)))
         (asz (* sc (d (dv v :dimasz)))) (th (* sc (d (dv v :dimtxt)))) (gap (* sc (abs (d (dv v :dimgap))))))
    (w out 0 "MLEADERSTYLE") (w out 5 h)
    (w out 102 "{ACAD_REACTORS") (w out 330 dict) (w out 102 "}")
    (w out 330 dict) (w out 100 "AcDbMLeaderStyle")
    (w out 179 2) (w out 170 2) (w out 171 1) (w out 172 0) (w out 90 2)
    (w out 40 0d0) (w out 41 0d0) (w out 173 1)
    (w out 91 -1056964608) (when ltype-h (w out 340 ltype-h)) (w out 92 -2)
    (w out 290 1) (w out 42 gap) (w out 291 1) (w out 43 (* 2 asz)) (w out 3 "Standard")
    (w out 44 asz) (w out 300 "") (when style-h (w out 342 style-h))
    (w out 174 1) (w out 178 1) (w out 175 1) (w out 176 0) (w out 93 -1056964608)
    (w out 45 th) (w out 292 0) (w out 297 0) (w out 46 4d0)
    (w out 94 -1056964608) (w out 47 1d0) (w out 49 1d0) (w out 140 1d0)
    (w out 293 1) (w out 141 0d0) (w out 294 1) (w out 177 0) (w out 142 1d0)
    (w out 295 0) (w out 296 0) (w out 143 0.125d0) (w out 271 0) (w out 272 9) (w out 273 9)))

;;; ---------- the MLEADER command ----------
;;; Pick the arrowhead, then where the landing goes, then type the note (as AutoCAD's MLEADER).
;;; Sizes come from the current dimension style: text DIMTXT, arrow DIMASZ, gap DIMGAP (x DIMSCALE).

(defun mleader-data (tip land text)
  (let* ((vars (current-dimvars))
         (sc (let ((s (dv vars :dimscale))) (if (and (realp s) (plusp s)) (d s) 1d0)))
         (asz (* sc (d (dv vars :dimasz)))) (th (* sc (d (dv vars :dimtxt))))
         (gap (* sc (abs (d (dv vars :dimgap)))))
         (doglen (* 2 asz))
         (right (>= (px land) (px tip)))
         (sign (if right 1 -1))
         (end (pt (+ (px land) (* sign doglen)) (py land) (pz (pt3 land))))
         (tpos (pt (+ (px end) (* sign gap)) (+ (py end) (* 0.5d0 th)) (pz (pt3 land)))))
    (list (cons 0 "MULTILEADER")
          (cons :ml (list :leaders (list (list :conn (pt3 land) :dir (pt sign 0 0) :len doglen
                                               :lines (list (list (pt3 tip)))))
                          :content (if text 2 0) :text (or text "") :tpos tpos :th th :tw 0d0
                          :attach (if right 1 3) :trot 0d0
                          :tstyle (textstyle-name (current-textstyle)) :tcolor 0
                          :type 1 :asz asz :arrow (arrow-code (or (dv vars :dimldrblk) "")) :gap gap
                          :landing t :dogleg t :lcolor 0 :scale 1d0)))))

(defcmd "MLEADER" ("MLD") "Creates a multileader: an arrow, a landing and a note, as one object."
  (let ((tip (get-user-input :point :prompt "Specify leader arrowhead location: ")))
    (when (pointp tip)
      (let ((land (get-user-input :point :base tip :prompt "Specify leader landing location: "
                                         :drag (lambda (p) (mapcar (lambda (sub) (list :data sub))
                                                                   (mleader-display-entities (mleader-data tip p "")))))))
        (when (pointp land)
          (let* ((probe (mleader-data tip land ""))
                 (tpos (ml probe :tpos)) (attach (ml probe :attach)) (h (ml probe :th))
                 (text (leader-note-text tpos attach h)))
            (add-entity (mleader-data tip land text))))))))
