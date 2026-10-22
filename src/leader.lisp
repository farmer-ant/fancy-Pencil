;;;; leader.lisp — AutoCAD LEADER objects (the arrow-and-line callouts that point at notes).
;;;; A leader is kept as its own entity: vertices (10 ...), arrowhead flag (71), straight/spline
;;;; path (72), dimension style (3) and per-leader overrides from the ACAD "DSTYLE" xdata
;;;; (:ldr-asz arrow size, :ldr-scale overall scale, :ldr-blk arrowhead). It is drawn as a
;;;; polyline plus an arrowhead, and written back to DXF as a LEADER (exploded for R12).

(in-package :fancy-pencil)

(defun leader-vertices (data)
  (loop for p in data when (and (consp p) (eql (car p) 10)) collect (pt3 (cdr p))))

(defun catmull-rom (pts &optional (steps 12))
  "A smooth curve through PTS (how AutoCAD's spline leaders look)."
  (if (< (length pts) 3) pts
      (let* ((v (coerce pts 'vector)) (n (length v)) (out nil))
        (flet ((at (i) (aref v (max 0 (min (1- n) i)))))
          (loop for i from 0 below (1- n)
                do (let ((p0 (at (1- i))) (p1 (at i)) (p2 (at (1+ i))) (p3 (at (+ i 2))))
                     (loop for k from 0 below steps
                           for u = (/ k (d steps))
                           do (flet ((cr (a b c dd)
                                       (* 0.5d0 (+ (* 2 b) (* (- c a) u)
                                                   (* (+ (* 2 a) (* -5 b) (* 4 c) (- dd)) u u)
                                                   (* (+ (- a) (* 3 b) (* -3 c) dd) u u u)))))
                                (push (pt (cr (px p0) (px p1) (px p2) (px p3))
                                          (cr (py p0) (py p1) (py p2) (py p3)))
                                      out))))))
        (nreverse (cons (pt3 (aref v (1- n))) out)))))

(defun leader-arrow-vars (data)
  (let* ((ds (find-dimstyle (dxf 3 data)))
         (vars (if ds (dimstyle-vars ds) (current-dimvars)))
         (scale (let ((s (or (dxf :ldr-scale data) (dv vars :dimscale)))) (if (and (realp s) (plusp s)) s 1d0)))
         (asz (or (dxf :ldr-asz data) (dv vars :dimasz))))
    (list :dimscale (d scale) :dimasz (d asz) :dimtsz 0d0 :dimclrd 0
          :dimblk (or (dxf :ldr-blk data) (getf vars :dimldrblk) ""))))

(defun leader-display-entities (data)
  "The lines and arrowhead that show a LEADER (colour ByBlock, so they take the leader's colour)."
  (let* ((verts (leader-vertices data))
         (path (if (eql (dxf 72 data) 1) (catmull-rom verts) verts))
         (out nil))
    (when (>= (length path) 2)
      (push (append (make-lwpoly-data path nil :layer (or (dxf 8 data) "0")) (list (cons 62 0))) out)
      (unless (eql (dxf 71 data) 0)
        (setf out (append out (dim-arrow (first path) (second path) (leader-arrow-vars data))))))
    out))

(defun leader-pieces (data)
  (loop for sub in (leader-display-entities data)
        when (equal (dxf 0 sub) "LWPOLYLINE") append (data-pieces sub)))

(defun xform-leader (data xf)
  "Move/rotate/mirror the vertices; SCALE also scales the arrowhead."
  (let ((d1 (map-points data '(10) (xf-fn xf)))
        (s (xf-scale xf)))
    (if (and s (/= s 1))
        (set-dxf :ldr-scale (* s (or (dxf :ldr-scale data) (dv (dimstyle-vars* (dxf 3 data)) :dimscale) 1d0)) d1)
        d1)))

(defun dimstyle-vars* (name)
  (let ((ds (find-dimstyle name))) (if ds (dimstyle-vars ds) (current-dimvars))))

(defun leader-xdata-overrides (raw)
  "Per-leader dimension overrides from the ACAD DSTYLE xdata in the raw DXF group RAW.
Returns an alist of (dimvar-code . value) — e.g. 41 arrow size, 40 overall scale, 341 arrow block handle."
  (let ((pos (position-if (lambda (p) (and (consp p) (eql (car p) 1000) (equal (cdr p) "DSTYLE"))) raw)))
    (when pos
      (loop with items = (nthcdr (1+ pos) raw)
            with out = nil
            while items
            do (let ((p (pop items)))
                 (cond ((and (eql (car p) 1002) (equal (cdr p) "}")) (return out))
                       ((and (eql (car p) 1070) items)
                        (push (cons (cdr p) (cdr (pop items))) out))))
            finally (return out)))))

(defun leader-from-dxf (raw data)
  "Attach the xdata overrides of RAW to converted leader DATA."
  (let ((ov (leader-xdata-overrides raw)))
    (when (assoc 41 ov) (setf data (append data (list (cons :ldr-asz (d (cdr (assoc 41 ov))))))))
    (when (assoc 40 ov) (setf data (append data (list (cons :ldr-scale (d (cdr (assoc 40 ov))))))))
    (when (assoc 341 ov) (setf data (append data (list (cons :ldr-blk-handle (cdr (assoc 341 ov)))))))
    data))

(defun write-leader-2000 (out data)
  (let ((verts (leader-vertices data)))
    (w out 100 "AcDbLeader")
    (w out 3 (let ((ds (find-dimstyle (dxf 3 data)))) (if ds (dimstyle-name ds) "Standard")))
    (w out 71 (if (eql (dxf 71 data) 0) 0 1))
    (w out 72 (if (eql (dxf 72 data) 1) 1 0))
    (w out 73 3)                         ; no linked annotation (the note is its own MTEXT)
    (w out 74 0) (w out 75 (if (eql (dxf 75 data) 1) 1 0))
    (w out 40 (d (or (dxf 40 data) 0))) (w out 41 (d (or (dxf 41 data) 0)))
    (w out 76 (length verts))
    (dolist (v verts) (wp out 10 v))
    (wp out 210 (pt 0 0 1))
    ;; per-leader overrides, as AutoCAD stores them
    (when (or (dxf :ldr-asz data) (dxf :ldr-scale data))
      (w out 1001 "ACAD") (w out 1000 "DSTYLE") (w out 1002 "{")
      (when (dxf :ldr-scale data) (w out 1070 40) (w out 1040 (d (dxf :ldr-scale data))))
      (when (dxf :ldr-asz data) (w out 1070 41) (w out 1040 (d (dxf :ldr-asz data))))
      (w out 1002 "}"))))
