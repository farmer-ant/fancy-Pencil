;;;; brep.lisp - exact solids. Open CASCADE (the geometry engine FreeCAD uses) does the solid
;;;; geometry; Fancy Pencil talks to it through a small program, fancypencil-solids
;;;; (support/solids), one line of text per request.
;;;;
;;;; A solid made this way is still a POLYFACE - the faceted skin Fancy Pencil draws, shades,
;;;; snaps to and saves - and also carries its exact shape: (:brep . text) is the shape in Open
;;;; CASCADE's BREP form, in the coordinates it was made in, and (:brep-m . 12 numbers), when
;;;; present, the move/turn/scale since (a 3x4 matrix by rows). Moving, rotating, scaling and
;;;; mirroring a solid change only the matrix, so they are as quick as ever. When Open CASCADE
;;;; isn't installed, nothing here is used: solids are made and edited as facets, as before.

(in-package :fancy-pencil)

(define-condition brep-error (error) ((message :initarg :message :reader brep-error-message))
  (:report (lambda (c s) (write-string (brep-error-message c) s))))

(defvar *solid-segments* 36) ; (set in solids.lisp)
(defvar *brep-process* nil)
(defvar *brep-in* nil "The helper's standard input (we write requests).")
(defvar *brep-out* nil "Its standard output (we read answers).")
(defvar *brep-state* nil "NIL: not tried yet; :ok; :missing.")
(defvar *brep-next-id* 0)
(defvar *brep-loaded* (make-hash-table :test #'eq) "BREP text -> the helper's number for it.")

(defun brep-helper-path ()
  "Where fancypencil-solids is, or NIL."
  (let* ((gd (and (boundp '*gui-dir*) (symbol-value '*gui-dir*)))
         (root (and gd (merge-pathnames "../" gd))))
    (or (let ((e (sb-ext:posix-getenv "FANCYPENCIL_SOLIDS"))) (and e (plusp (length e)) (probe-file e) e))
        (and root (let ((p (probe-file (merge-pathnames "support/solids/fancypencil-solids" root)))) (and p (namestring p))))
        (let ((p (probe-file "/opt/fancypencil/support/solids/fancypencil-solids"))) (and p (namestring p)))
        (ignore-errors
         (let ((out (with-output-to-string (s)
                      (sb-ext:run-program "/bin/sh" (list "-c" "command -v fancypencil-solids") :output s :error nil))))
           (let ((p (string-trim '(#\Newline #\Space) out))) (and (plusp (length p)) p)))))))

(defun brep-stop ()
  (when *brep-process*
    (ignore-errors (close *brep-in*))
    (ignore-errors (sb-ext:process-kill *brep-process* 15))
    (setf *brep-process* nil *brep-state* nil)
    (clrhash *brep-loaded*)))

(defvar *brep-why* nil "Why the solid engine isn't running, for SOLIDENGINE.")
(defvar *brep-tried-at* 0)

(defun brep-start ()
  (setf *brep-tried-at* (get-universal-time))
  (let ((path (brep-helper-path))
        (errfile (format nil "/tmp/fancypencil-solids-~D.err" (sb-unix:unix-getpid))))
    (if (null path)
        (setf *brep-state* :missing
              *brep-why* "fancypencil-solids wasn't found (looked beside Fancy Pencil and in /opt/fancypencil/support/solids)")
        (handler-case
            (let ((p (sb-ext:run-program path nil :wait nil :input :stream :output :stream
                                                  :error errfile :if-error-exists :supersede
                                                  :external-format :utf-8)))
              (setf *brep-process* p
                    *brep-in* (sb-ext:process-input p)
                    *brep-out* (sb-ext:process-output p))
              (clrhash *brep-loaded*)
              (write-line "ping" *brep-in*) (force-output *brep-in*)
              (let ((l (ignore-errors (read-line *brep-out* nil nil))))
                (if (and l (search "ok fancypencil-solids" l))
                    (setf *brep-state* :ok *brep-why* nil)
                    (setf *brep-state* :missing
                          *brep-why* (format nil "~A was found but didn't answer~@[ (it said: ~A)~]~@[~%  ~A~]" path l
                                             (ignore-errors
                                              (let ((txt (with-open-file (i errfile) (let ((s (make-string (min 2000 (file-length i))))) (subseq s 0 (read-sequence s i))))))
                                                (and (plusp (length (string-trim '(#\Space #\Newline) txt))) (string-trim '(#\Space #\Newline) txt)))))))))
          (error (c) (setf *brep-state* :missing
                           *brep-why* (format nil "~A couldn't be started: ~A" path c)))))))

(defun brep-available-p ()
  "True when exact solids can be made (Open CASCADE and the helper are installed, and SOLIDMODE
isn't set to 0)."
  (and (not (eql (sysvar "SOLIDMODE") 0))
       (progn (when (or (null *brep-state*)
                        ;; (not found before: look again now and then - it may have been installed since)
                        (and (eq *brep-state* :missing) (> (- (get-universal-time) *brep-tried-at*) 10))
                        (and (eq *brep-state* :ok) *brep-process* (not (sb-ext:process-alive-p *brep-process*))))
                (setf *brep-process* nil)
                (brep-start))
              (eq *brep-state* :ok))))

(defun brep-send (lines)
  (dolist (l lines) (write-line l *brep-in*))
  (force-output *brep-in*))

(defun brep-request (line &rest more-lines)
  "Send a request; the answer's lines (before the last), and the last line's words after \"ok\"."
  (unless (brep-available-p) (error 'brep-error :message "the solid engine isn't running"))
  (handler-case (brep-send (cons line more-lines))
    (error () (brep-stop) (error 'brep-error :message "the solid engine stopped")))
  (let ((lines nil))
    (loop
      (let ((l (read-line *brep-out* nil nil)))
        (cond ((null l) (brep-stop) (error 'brep-error :message "the solid engine stopped"))
              ((and (>= (length l) 2) (string= l "ok" :end1 2) (or (= (length l) 2) (char= (char l 2) #\Space)))
               (return (values (nreverse lines) (string-trim " " (subseq l 2)))))
              ((and (>= (length l) 3) (string= l "err" :end1 3))
               (error 'brep-error :message (string-trim " " (subseq l 3))))
              (t (push l lines)))))))

(defun brep-num (x)
  (let ((*read-default-float-format* 'double-float))
    (format nil "~,15,,,,,'eE" (d x))))
(defun brep-nums (&rest xs) (format nil "~{~A~^ ~}" (mapcar #'brep-num xs)))
(defun brep-pt (p) (let ((p (pt3 p))) (brep-nums (px p) (py p) (pz p))))

(defun brep-new-id () (incf *brep-next-id*))

;;; ---------------------------------------------------------------- matrices (3 x 4, by rows)

(defparameter +identity-m+ '(1d0 0d0 0d0 0d0  0d0 1d0 0d0 0d0  0d0 0d0 1d0 0d0))

(defun m-apply (m p)
  (let ((p (pt3 p)))
    (destructuring-bind (a b c d e f g h i j k l) m
      (pt (+ (* a (px p)) (* b (py p)) (* c (pz p)) d)
          (+ (* e (px p)) (* f (py p)) (* g (pz p)) h)
          (+ (* i (px p)) (* j (py p)) (* k (pz p)) l)))))

(defun m-compose (outer inner)
  "The matrix doing INNER, then OUTER."
  (let ((a (coerce outer 'vector)) (b (coerce inner 'vector)) (r nil))
    (dotimes (row 3)
      (dotimes (col 4)
        (push (+ (* (aref a (* 4 row)) (aref b col))
                 (* (aref a (+ (* 4 row) 1)) (aref b (+ 4 col)))
                 (* (aref a (+ (* 4 row) 2)) (aref b (+ 8 col)))
                 (if (= col 3) (aref a (+ (* 4 row) 3)) 0d0))
              r)))
    (nreverse r)))

(defun xf-matrix (fn)
  "The 3x4 matrix of the (affine) point mapping FN."
  (let* ((o (funcall fn (pt 0 0 0)))
         (x (v- (pt3 (funcall fn (pt 1 0 0))) (pt3 o)))
         (y (v- (pt3 (funcall fn (pt 0 1 0))) (pt3 o)))
         (z (v- (pt3 (funcall fn (pt 0 0 1))) (pt3 o))))
    (list (px x) (px y) (px z) (px o)
          (py x) (py y) (py z) (py o)
          (pz x) (pz y) (pz z) (pz o))))

(defun brep-transform-data (data fn)
  "DATA's exact-shape matrix after the point mapping FN (for xform-mesh)."
  (if (assoc :brep data)
      (let ((m (m-compose (xf-matrix fn) (or (dxf :brep-m data) +identity-m+))))
        (if (assoc :brep-m data) (set-dxf :brep-m m data) (append data (list (cons :brep-m m)))))
      data))

;;; ---------------------------------------------------------------- shapes in, results out

(defun brep-data-p (data) (and (equal (dxf 0 data) "POLYFACE") (stringp (dxf :brep data))))

(defun brep-load (data)
  "The helper's number for DATA's exact shape, as DATA has it placed."
  (let* ((text (dxf :brep data))
         (id (gethash text *brep-loaded*)))
    (unless id
      (setf id (brep-new-id))
      (let ((lines (split-lines text)))
        (apply #'brep-request (format nil "load ~D ~D" id (length lines)) lines))
      (setf (gethash text *brep-loaded*) id))
    (let ((m (dxf :brep-m data)))
      (if (or (null m) (every (lambda (a b) (< (abs (- a b)) 1d-14)) m +identity-m+))
          id
          (let ((id2 (brep-new-id)))
            (brep-request (format nil "trsf ~D ~D ~A" id2 id (format nil "~{~A~^ ~}" (mapcar #'brep-num m))))
            id2)))))

(defun split-lines (text)
  (let ((out nil) (start 0))
    (loop for i = (position #\Newline text :start start)
          do (push (subseq text start (or i (length text))) out)
             (if i (setf start (1+ i)) (return)))
    (when (and out (zerop (length (first out)))) (pop out))
    (nreverse out)))

(defun brep-text (id)
  (multiple-value-bind (lines) (brep-request (format nil "save ~D" id))
    ;; the first line is "brep N"
    (format nil "~{~A~%~}" (rest lines))))

(defvar *brep-edges* (make-hash-table :test #'eq #+sbcl :weakness #+sbcl :key)
  "BREP text -> its edges as point lists, by edge number (in the text's own coordinates).")

(defun brep-mesh (id)
  "Shape ID as mesh data (corners welded, the faces' own edges visible), and its edges as lists
of points by edge number."
  (multiple-value-bind (lines box) (brep-request (format nil "bbox ~D" id))
    (declare (ignore lines))
    (let* ((b (with-input-from-string (s box) (loop repeat 6 collect (let ((*read-default-float-format* 'double-float)) (read s)))))
           (size (max 1d-6 (dist (pt (first b) (second b) (third b)) (pt (fourth b) (fifth b) (sixth b)))))
           (defl (* size 0.0015d0))
           (angle (/ 360d0 (max 8 *solid-segments*)))
           (lines (brep-request (format nil "mesh ~D ~A ~A" id (brep-num defl) (brep-num angle))))
           (weld (make-hash-table :test #'equal)) (verts nil) (nv 0) (faces nil) (edges nil)
           (eps (* size 1d-9))
           (*read-default-float-format* 'double-float) (*read-eval* nil))
      (flet ((vid (p)
               (let ((k (list (round (px p) eps) (round (py p) eps) (round (pz p) eps))))
                 (or (gethash k weld) (progn (push p verts) (setf (gethash k weld) (incf nv))))))
             (nums (l) (with-input-from-string (s l) (loop for x = (read s nil nil) while x collect x))))
        (let ((ls lines))
          (pop ls)                      ; "faces F edges E"
          (loop while ls
                do (let ((l (pop ls)))
                     (cond
                       ((and (> (length l) 5) (string= l "face " :end1 5))
                        (let* ((parts (with-input-from-string (s l) (loop for x = (read s nil nil) while x collect x)))
                               (nn (fourth parts)) (nt (fifth parts))
                               (ids (coerce (loop repeat nn collect (let ((xyz (nums (pop ls)))) (vid (pt (d (first xyz)) (d (second xyz)) (d (third xyz)))))) 'vector))
                               (tris (loop repeat nt collect (nums (pop ls))))
                               (bound (make-hash-table :test #'equal)))
                          (loop while (and ls (> (length (first ls)) 6) (string= (first ls) "bound " :end1 6))
                                do (let* ((ns (cdr (nums (subseq (pop ls) 6)))) (chain (cdr ns)))
                                     (loop for (a c) on chain while c
                                           do (setf (gethash (if (< a c) (cons a c) (cons c a)) bound) t))))
                          (dolist (tr tris)
                            (let ((ia (aref ids (first tr))) (ib (aref ids (second tr))) (ic (aref ids (third tr))))
                              (unless (or (= ia ib) (= ib ic) (= ia ic))
                                (flet ((vis (a c) (gethash (if (< a c) (cons a c) (cons c a)) bound)))
                                  (push (list (if (vis (first tr) (second tr)) ia (- ia))
                                              (if (vis (second tr) (third tr)) ib (- ib))
                                              (if (vis (third tr) (first tr)) ic (- ic)))
                                        faces)))))))
                       ((and (> (length l) 5) (string= l "edge " :end1 5))
                        (let* ((ns (nums (subseq l 5))) (k (first ns)) (n (second ns)) (xs (cddr ns)))
                          (push (cons k (loop repeat n for (x y z) on xs by #'cdddr collect (pt (d x) (d y) (d z)))) edges))))))))
      (values (make-mesh-data (nreverse verts) (nreverse faces))
              (let ((v (make-array (1+ (length edges)) :initial-element nil)))
                (dolist (e edges) (when (< (car e) (length v)) (setf (aref v (car e)) (cdr e))))
                v)))))

(defun brep-result (id &optional props kind)
  "Mesh data for shape ID, carrying the exact shape, with PROPS (layer, colour ...) and KIND."
  (multiple-value-bind (mesh edges) (brep-mesh id)
    (let ((text (brep-text id)))
      (setf (gethash text *brep-edges*) edges)
      (setf (gethash text *brep-loaded*) id)
      (append mesh props
              (and kind (list (cons :kind kind)))
              (list (cons :brep text))))))

(defun brep-edges-of (data)
  "DATA's exact edges as point lists by edge number, placed as DATA is."
  (let* ((text (dxf :brep data))
         (es (or (gethash text *brep-edges*)
                 (multiple-value-bind (m es) (brep-mesh (brep-load (rem-dxf :brep-m data))) (declare (ignore m))
                   (setf (gethash text *brep-edges*) es))))
         (m (dxf :brep-m data)))
    (if m (map 'vector (lambda (pts) (mapcar (lambda (p) (m-apply m p)) pts)) es) es)))

(defmacro with-brep ((&key (fallback nil)) &body body)
  "Do BODY with the exact engine; if it isn't there, or fails, do FALLBACK (and say why it failed)."
  `(if (brep-available-p)
       (handler-case (progn ,@body)
         (brep-error (c)
           (out (format nil "The exact solid engine couldn't do that (~A)~:[.~;; using facets.~]~%"
                        (brep-error-message c) ',fallback))
           ,fallback))
       ,fallback))

;;; ---------------------------------------------------------------- profiles from drawing objects

(defun bulge-mid (a b bulge)
  "The middle of the arc from A to B with BULGE (2D)."
  (let* ((m (midpt a b)) (chord (dist2d a b)) (s (* bulge chord 0.5d0))
         (l (max 1d-300 (dist2d a b)))
         (dir (pt (/ (- (px b) (px a)) l) (/ (- (py b) (py a)) l))))
    ;; the arc's middle lies off the chord's middle, to the right for a positive bulge
    (v+ m (v* (pt (py dir) (- (px dir))) s))))

(defun entity-profile (e)
  "Profile lines (strings) for drawing object E, in the coordinates in use, and whether it is closed."
  (let* ((data (ent-data e)) (type (dxf 0 data)))
    (flet ((w3 (p) (if (wcs-type-p type) (pt3 p)
                       (multiple-value-bind (ax ay az) (ocs-axes (entity-normal data)) (ocs->w (pt3 p) ax ay az)))))
      (cond
        ((equal type "LINE") (values (list (format nil "line ~A ~A" (brep-pt (dxf 10 data)) (brep-pt (dxf 11 data)))) nil))
        ((equal type "CIRCLE")
         (values (list (format nil "circle ~A ~A ~A" (brep-pt (w3 (dxf 10 data))) (brep-pt (entity-normal data)) (brep-num (dxf 40 data)))) t))
        ((equal type "ARC")
         (let* ((c (pt3 (dxf 10 data))) (r (d (dxf 40 data))) (a0 (d (dxf 50 data))) (sw (arc-sweep (dxf 50 data) (dxf 51 data)))
                (at (lambda (a) (pt (+ (px c) (* r (cos a))) (+ (py c) (* r (sin a))) (pz c)))))
           (values (list (format nil "arc ~A ~A ~A" (brep-pt (w3 (funcall at a0))) (brep-pt (w3 (funcall at (+ a0 (/ sw 2)))))
                                 (brep-pt (w3 (funcall at (+ a0 sw))))))
                   nil)))
        ((equal type "LWPOLYLINE")
         (let* ((el (d (or (dxf 38 data) 0)))
                (vs (mapcar (lambda (v) (list (pt (px (first v)) (py (first v)) el) (or (second v) 0d0))) (lwpoly-vertices data)))
                (closed (or (lwpoly-closed-p data)
                            (and (> (length vs) 2) (< (dist (first (first vs)) (first (car (last vs)))) 1d-9))))
                (vs (if (and closed (not (lwpoly-closed-p data))) (butlast vs) vs))
                (lines (loop for (a b) on (if closed (append vs (list (first vs))) vs) while b
                             unless (< (dist (first a) (first b)) 1d-12)
                               collect (if (< (abs (second a)) 1d-12)
                                           (format nil "line ~A ~A" (brep-pt (w3 (first a))) (brep-pt (w3 (first b))))
                                           (let ((m (bulge-mid (first a) (first b) (second a))))
                                             (format nil "arc ~A ~A ~A" (brep-pt (w3 (first a)))
                                                     (brep-pt (w3 (pt (px m) (py m) el))) (brep-pt (w3 (first b)))))))))
           (values lines closed)))
        (t
         ;; ellipses, splines, helixes: a smooth curve through points along them
         (multiple-value-bind (pts closed) (shape-of e)
           (when (cdr pts)
             (let ((pts (if closed (append pts (list (first pts))) pts)))
               (values (list (format nil "spline ~D ~{~A~^ ~}" (length pts) (mapcar #'brep-pt pts))) closed)))))))))

(defun brep-profile-lines (e &key outline)
  (multiple-value-bind (lines closed) (entity-profile e)
    (values (append (and outline (list "outline")) (list "loop") lines (list "end")) closed)))
