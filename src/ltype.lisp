;;;; ltype.lisp — linetypes (acad.lin-compatible), dash generation, lineweights.

(in-package :lispdraft)

;;; The standard linetypes from acad.lin (imperial). Each pattern is a list of
;;; lengths: positive = dash, negative = gap, 0 = dot.
(defparameter *standard-linetypes*
  '(("BORDER" "Border __ __ . __ __ . __ __ . __ __ . __ __ ." .5 -.25 .5 -.25 0 -.25)
    ("BORDER2" "Border (.5x) __.__.__.__.__.__.__.__.__.__.__." .25 -.125 .25 -.125 0 -.125)
    ("BORDERX2" "Border (2x) ____  ____  .  ____  ____  .  ___" 1.0 -.5 1.0 -.5 0 -.5)
    ("CENTER" "Center ____ _ ____ _ ____ _ ____ _ ____ _ ____" 1.25 -.25 .25 -.25)
    ("CENTER2" "Center (.5x) ___ _ ___ _ ___ _ ___ _ ___ _ ___" .75 -.125 .125 -.125)
    ("CENTERX2" "Center (2x) ________  __  ________  __  _____" 2.5 -.5 .5 -.5)
    ("DASHDOT" "Dash dot __ . __ . __ . __ . __ . __ . __ . __" .5 -.25 0 -.25)
    ("DASHDOT2" "Dash dot (.5x) _._._._._._._._._._._._._._._." .25 -.125 0 -.125)
    ("DASHDOTX2" "Dash dot (2x) ____  .  ____  .  ____  .  ___" 1.0 -.5 0 -.5)
    ("DASHED" "Dashed __ __ __ __ __ __ __ __ __ __ __ __ __ _" .5 -.25)
    ("DASHED2" "Dashed (.5x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _" .25 -.125)
    ("DASHEDX2" "Dashed (2x) ____  ____  ____  ____  ____  ___" 1.0 -.5)
    ("DIVIDE" "Divide ____ . . ____ . . ____ . . ____ . . ____" .5 -.25 0 -.25 0 -.25)
    ("DIVIDE2" "Divide (.5x) __..__..__..__..__..__..__..__.._" .25 -.125 0 -.125 0 -.125)
    ("DIVIDEX2" "Divide (2x) ________  .  .  ________  .  .  _" 1.0 -.5 0 -.5 0 -.5)
    ("DOT" "Dot . . . . . . . . . . . . . . . . . . . . . . . ." 0 -.25)
    ("DOT2" "Dot (.5x) ........................................" 0 -.125)
    ("DOTX2" "Dot (2x) .  .  .  .  .  .  .  .  .  .  .  .  .  ." 0 -.5)
    ("HIDDEN" "Hidden __ __ __ __ __ __ __ __ __ __ __ __ __ __" .25 -.125)
    ("HIDDEN2" "Hidden (.5x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _" .125 -.0625)
    ("HIDDENX2" "Hidden (2x) ____ ____ ____ ____ ____ ____ ____ " .5 -.25)
    ("PHANTOM" "Phantom ______  __  __  ______  __  __  ______ " 1.25 -.25 .25 -.25 .25 -.25)
    ("PHANTOM2" "Phantom (.5x) ___ _ _ ___ _ _ ___ _ _ ___ _ _ ___" .625 -.125 .125 -.125 .125 -.125)
    ("PHANTOMX2" "Phantom (2x) ____________    ____    ____    _" 2.5 -.5 .5 -.5 .5 -.5)))

(defun library-linetype (name)
  (let ((e (assoc name *standard-linetypes* :test #'string-equal)))
    (and e (make-ltype :name (first e) :desc (second e) :pattern (mapcar #'d (cddr e))))))

(defun continuous-name-p (name)
  (or (null name) (member name '("CONTINUOUS" "BYLAYER" "BYBLOCK" "") :test #'string-equal)))

(defun find-ltype (name)
  (and (stringp name)
       (find name (drawing-ltypes *dwg*) :key #'ltype-name :test #'string-equal)))

(defun add-ltype (lt)
  (setf (drawing-ltypes *dwg*)
        (append (remove (ltype-name lt) (drawing-ltypes *dwg*) :key #'ltype-name :test #'string-equal)
                (list lt)))
  (touch)
  lt)

(defun ensure-ltype (name)
  "Make NAME available in the drawing, loading it from the standard library if needed."
  (cond ((continuous-name-p name) t)
        ((find-ltype name))
        ((library-linetype name) (add-ltype (library-linetype name)))
        (t nil)))

(defun parse-lin-file (path)
  "Read an AutoCAD .lin file. Returns a list of LTYPE structs (complex elements are ignored)."
  (let ((result nil) (cur nil))
    (dolist (raw (read-file-lines path) (nreverse result))
      (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
        (cond ((or (zerop (length line)) (char= (char line 0) #\;)) nil)
              ((char= (char line 0) #\*)
               (let* ((comma (position #\, line)))
                 (setf cur (make-ltype :name (string-trim " " (subseq line 1 comma))
                                       :desc (if comma (string-trim " " (subseq line (1+ comma))) "")))
                 (push cur result)))
              ((and cur (char-equal (char line 0) #\A))
               (setf (ltype-pattern cur)
                     (loop with start = (1+ (or (position #\, line) 0))
                           with depth = 0
                           for i from start to (length line)
                           for c = (if (< i (length line)) (char line i) #\,)
                           do (case c (#\[ (incf depth)) (#\] (decf depth)))
                           when (and (char= c #\,) (zerop depth))
                             append (let ((tok (string-trim " " (subseq line start i))))
                                      (setf start (1+ i))
                                      (if (and (plusp (length tok)) (number-token-p tok))
                                          (list (d (parse-token tok)))
                                          nil))))))))))

;;; ---------- resolving an entity's linetype ----------

(defun entity-ltype-name (data &optional parent-ltype)
  "Effective linetype name for DATA (resolving BYLAYER/BYBLOCK)."
  (let ((lt (dxf 6 data)))
    (cond ((or (null lt) (string-equal lt "BYLAYER"))
           (let ((l (find-layer (dxf 8 data))))
             (if (and l (equal (dxf 8 data) "0") parent-ltype) parent-ltype (and l (layer-ltype l)))))
          ((string-equal lt "BYBLOCK") (or parent-ltype "CONTINUOUS"))
          (t lt))))

(defun ltype-pattern-for (name)
  (unless (continuous-name-p name)
    (let ((lt (or (find-ltype name) (library-linetype name))))
      (and lt (ltype-pattern lt) (some (lambda (x) (/= x 0)) (ltype-pattern lt)) (ltype-pattern lt)))))

(defun entity-lineweight (data &optional parent-lw)
  "Effective lineweight in hundredths of a millimetre (-3 = default)."
  (let ((lw (dxf 370 data)))
    (cond ((or (null lw) (= lw -1))
           (let ((l (find-layer (dxf 8 data))))
             (if (and (equal (dxf 8 data) "0") parent-lw) parent-lw (if l (layer-lineweight l) -3))))
          ((= lw -2) (or parent-lw -3))
          (t lw))))

(defparameter *lineweights* '(0 5 9 13 15 18 20 25 30 35 40 50 53 60 70 80 90 100 106 120 140 158 200 211))

(defun lineweight-pixels (lw)
  (if (and (eql (sysvar "LWDISPLAY") 1) (integerp lw))
      (let ((v (if (minusp lw) 25 lw)))
        (max 1 (round (* v 0.06d0))))
      1))

(defun lineweight-label (lw)
  (case lw (-1 "ByLayer") (-2 "ByBlock") (-3 "Default")
    (t (format nil "~,2Fmm" (/ lw 100d0)))))

;;; ---------- dash generation ----------

(defun polyline-length (pts) (loop for (a b) on pts while b sum (dist2d a b)))

(defun dash-runs (pts pattern scale)
  "Split the polyline PTS into dash runs following PATTERN (world units x SCALE).
Returns a list of point lists; dots become very short runs."
  (let* ((pat (mapcar (lambda (x) (* x scale)) pattern))
         (period (reduce #'+ (mapcar #'abs pat)))
         (runs nil) (cur nil)
         (k 0) (remaining (abs (first pat))) (on (>= (first pat) 0))
         (dotlen (/ 0.75d0 *vscale*)))
    (when (or (< period 1d-12) (null pts)) (return-from dash-runs (list pts)))
    (flet ((advance ()
             (setf k (mod (1+ k) (length pat)))
             (setf remaining (abs (nth k pat)) on (>= (nth k pat) 0))))
      ;; zero-length dot at the very start
      (loop for (a b) on pts while b
            do (let ((seglen (dist2d a b)) (pos 0d0))
                 (loop while (< pos seglen) do
                   (if (and on (zerop (nth k pat)))
                       ;; a dot
                       (let ((p (at-param a b (/ pos (max seglen 1d-15)))))
                         (when cur (push (nreverse cur) runs) (setf cur nil))
                         (push (list p (polar p (ang a b) dotlen)) runs)
                         (advance))
                       (let* ((step (min remaining (- seglen pos)))
                              (p0 (at-param a b (/ pos seglen)))
                              (p1 (at-param a b (/ (+ pos step) seglen))))
                         (when on
                           (unless cur (push p0 cur))
                           (push p1 cur))
                         (incf pos step)
                         (decf remaining step)
                         (when (<= remaining 1d-12)
                           (when (and on cur) (push (nreverse cur) runs) (setf cur nil))
                           (advance)))))))
      (when cur (push (nreverse cur) runs))
      (nreverse runs))))

(defun ltype-scale-for (data &optional (parent-scale 1d0))
  (* (d (or (sysvar "LTSCALE") 1)) (d (or (dxf 48 data) 1)) parent-scale))
