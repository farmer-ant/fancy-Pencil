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
         (path (cond ((not (eql (dxf 72 data) 1)) verts)
                     ;; a spline leader's landing (hook) line stays straight
                     ((and (eql (dxf 75 data) 1) (> (length verts) 3))
                      (append (catmull-rom (butlast verts)) (last verts)))
                     (t (catmull-rom verts))))
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
    (wp out 210 (entity-normal data))
    ;; per-leader overrides, as AutoCAD stores them
    (when (or (dxf :ldr-asz data) (dxf :ldr-scale data))
      (w out 1001 "ACAD") (w out 1000 "DSTYLE") (w out 1002 "{")
      (when (dxf :ldr-scale data) (w out 1070 40) (w out 1040 (d (dxf :ldr-scale data))))
      (when (dxf :ldr-asz data) (w out 1070 41) (w out 1040 (d (dxf :ldr-asz data))))
      (w out 1002 "}"))))

;;; ---------- the LEADER command ----------
;;; Pick the arrow tip, then the points of the leader line; Enter ends it and asks for the note.
;;; Spline / STraight switch the line between a smooth curve and straight segments at any time,
;;; Arrow / None turn the arrowhead on and off. The note is MTEXT beside a short horizontal
;;; landing line, sized by the current dimension style (text height DIMTXT x DIMSCALE).

(defvar *leader-spline* nil "LEADER draws a spline leader (remembered for the next leader).")
(defvar *leader-arrow* t "LEADER draws an arrowhead (remembered for the next leader).")

(defun leader-data (pts &key spline (arrow t) hook)
  "A new leader. An arrow size or overall scale set at the command line (DIMASZ, DIMSCALE) that
differs from the dimension style is kept on the leader, as AutoCAD keeps dimension overrides."
  (let* ((style (or (sysvar "DIMSTYLE") "Standard"))
         (svars (dimstyle-vars* style))
         (asz (sysvar "DIMASZ")) (sc (sysvar "DIMSCALE")))
    (append (list (cons 0 "LEADER") (cons 3 style)
                  (cons 71 (if arrow 1 0)) (cons 72 (if spline 1 0)) (cons 75 (if hook 1 0)))
            (mapcar (lambda (p) (cons 10 (pt3 p))) pts)
            (when (and (realp asz) (>= asz 0) (> (abs (- asz (d (dv svars :dimasz)))) 1d-9))
              (list (cons :ldr-asz (d asz))))
            (when (and (realp sc) (plusp sc) (> (abs (- sc (d (dv svars :dimscale)))) 1d-9))
              (list (cons :ldr-scale (d sc)))))))

(defun leader-text-layout (pts)
  "Where a leader's note goes: (values landing-end insertion attachment height hook-p).
The note sits to the right of the last point when the leader runs rightward, else to the left."
  (let* ((vars (current-dimvars))
         (sc (let ((v (dv vars :dimscale))) (if (and (realp v) (plusp v)) (d v) 1d0)))
         (h (* sc (d (dv vars :dimtxt))))
         (gap (* sc (abs (d (dv vars :dimgap)))))
         (end (car (last pts))) (prev (car (last pts 2)))
         (right (>= (px end) (px prev)))
         (sign (if right 1 -1))
         ;; a sloped last segment gets a short horizontal landing, as in AutoCAD
         (hook (> (abs (- (py end) (py prev))) (* 1d-6 (max 1d0 (dist2d end prev)))))
         (land (if hook (pt (+ (px end) (* sign sc (d (dv vars :dimasz)))) (py end) (pz end)) end)))
    ;; the middle of the note's first line sits on the landing line (top-left/top-right attachment)
    (values land (pt (+ (px land) (* sign gap)) (+ (py land) (* 0.5d0 h)) (pz land)) (if right 1 3) h hook)))

(defun leader-note-text (ins attach h)
  "The note: typed in place in the GUI, else line by line. Returns MTEXT content or NIL."
  (if (use-inplace-editor-p)
      (let* ((st (current-textstyle))
             (topleft (pt (if (= attach 1) (px ins) (- (px ins) (* 12 h))) (py ins)))
             (r (inplace-edit :mtext topleft 0d0 h :justify (if (= attach 1) "left" "right")
                                                   :style (textstyle-name st))))
        (and (plusp (length (string-trim '(#\Space) (text-string-display (getf r :text)))))
             (editor-result-content r)))
      (let ((lines (loop for i from 0
                         for s = (gstring (if (= i 0) "Enter first line of annotation text <none>: "
                                              "Enter next line of annotation text: ")
                                          t)
                         until (string= s "") collect s)))
        (and lines (format nil "~{~A~^\\P~}" lines)))))

(defun leader-format-option ()
  (let ((r (gkw (format nil "Enter leader format option [Spline/STraight/Arrow/None] <Exit>: ")
                '("Spline" "STraight" "Arrow" "None" "Exit"))))
    (cond ((equal r "Spline") (setf *leader-spline* t))
          ((equal r "STraight") (setf *leader-spline* nil))
          ((equal r "Arrow") (setf *leader-arrow* t))
          ((equal r "None") (setf *leader-arrow* nil)))))

(defun leader-status ()
  (out (format nil "Leader: ~:[straight~;spline~], ~:[no arrowhead~;arrowhead~].~%" *leader-spline* *leader-arrow*)))

(defcmd "LEADER" ("LE" "LEAD" "QLEADER" "QL" "MLEADER" "MLD")
    "Creates a leader: an arrow and a straight or spline line pointing at a note."
  (leader-status)
  (let ((start (gpoint "Specify leader start point (arrow tip) or [Spline/STraight/Arrow/None]: "
                       :keywords '("Spline" "STraight" "Arrow" "None"))))
    (loop while (stringp start)
          do (cond ((equal start "Spline") (setf *leader-spline* t))
                   ((equal start "STraight") (setf *leader-spline* nil))
                   ((equal start "Arrow") (setf *leader-arrow* t))
                   ((equal start "None") (setf *leader-arrow* nil)))
             (leader-status)
             (setf start (gpoint "Specify leader start point (arrow tip) or [Spline/STraight/Arrow/None]: "
                                 :keywords '("Spline" "STraight" "Arrow" "None"))))
    (when (pointp start)
      (let ((pts (list start)) (note :ask))
        (flet ((preview (cur)
                 (list (list :data (leader-data (append pts (list cur)) :spline *leader-spline* :arrow *leader-arrow*)))))
          (loop
            (let* ((one (= (length pts) 1))
                   (r (gpoint (if one
                                  "Specify next point or [Spline/STraight/Arrow/None]: "
                                  "Specify next point or [Text/NOText/Spline/STraight/Arrow/None/Format/Undo] <Text>: ")
                              :base (car (last pts)) :drag #'preview
                              :keywords (if one '("Spline" "STraight" "Arrow" "None")
                                            '("Text" "Annotation" "NOText" "Spline" "STraight" "Arrow" "None" "Format" "Undo")))))
              (cond ((pointp r) (setf pts (append pts (list r))))
                    ((and (null r) one) (return))
                    ((or (null r) (equal r "Text") (equal r "Annotation")) (return))
                    ((equal r "NOText") (setf note nil) (return))
                    ((equal r "Spline") (setf *leader-spline* t) (leader-status))
                    ((equal r "STraight") (setf *leader-spline* nil) (leader-status))
                    ((equal r "Arrow") (setf *leader-arrow* t) (leader-status))
                    ((equal r "None") (setf *leader-arrow* nil) (leader-status))
                    ((equal r "Format") (leader-format-option) (leader-status))
                    ((equal r "Undo") (when (> (length pts) 1) (setf pts (butlast pts))))))))
        (when (>= (length pts) 2)
          (multiple-value-bind (land ins attach h hook) (leader-text-layout pts)
            ;; the leader goes in first, so it shows while the note is typed
            (let ((ldr (add (leader-data pts :spline *leader-spline* :arrow *leader-arrow*))))
              (when (gui-p) (gui-regen))
              (let ((content (and note (leader-note-text ins attach h))))
                (when content
                  (when hook
                    ;; keep the layer, color, linetype ... the new leader was given
                    (set-entity-data ldr (with-props (leader-data (append pts (list land)) :spline *leader-spline*
                                                                                          :arrow *leader-arrow* :hook t)
                                                     (ent-data ldr))))
                  (add (list (cons 0 "MTEXT") (cons 10 (pt3 ins)) (cons 40 (d h)) (cons 41 0d0)
                             (cons 71 attach) (cons 72 1) (cons 1 content) (cons 50 0d0)
                             (cons 7 (textstyle-name (current-textstyle)))))))
              (when (gui-p) (gui-regen)))))))))
