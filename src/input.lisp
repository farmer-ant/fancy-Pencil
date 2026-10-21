;;;; input.lisp — prompts, coordinate entry, keywords, object snaps, selection,
;;;; and the AutoLISP (COMMAND ...) bridge.

(in-package :fancy-pencil)

(defvar *initget-bits* 0)
(defvar *initget-keywords* nil)
(defvar *osnap-override* nil "One-shot osnap bits typed at a point prompt (END, MID ...).")
(defvar *pickfirst* nil "Entities selected at the Command: prompt (noun/verb selection).")
(defvar *previous-selection* nil)
(defvar *cursor-world* (pt 0 0) "Last cursor position, world coordinates (GUI).")

(defparameter +pause+ "\\")

(defvar *sync-queue* :none "Queue used when threads are unavailable.")
(defvar *script-queue* nil "Pending inputs from SCRIPT files and menu macros.")
(defvar *from-script* nil)

(defstruct inreq
  kind prompt base keywords (flags 0) drag spaces nosnap)

;;; ---------- thread shim (for the COMMAND coroutine) ----------

(defun spawn-thread (name fn)
  #+sbcl (sb-thread:make-thread fn :name name)
  #+ecl (mp:process-run-function name fn)
  #+ccl (ccl:process-run-function name fn)
  #-(or sbcl ecl ccl) (progn name (funcall fn) nil))
(defun current-thread ()
  #+sbcl sb-thread:*current-thread*
  #+ecl mp:*current-process*
  #+ccl ccl:*current-process*
  #-(or sbcl ecl ccl) :main)
(defun make-lock* (name)
  #+sbcl (sb-thread:make-mutex :name name)
  #+ecl (mp:make-lock :name name)
  #+ccl (ccl:make-lock name)
  #-(or sbcl ecl ccl) name)
(defmacro with-lock* ((lock) &body body)
  #+sbcl `(sb-thread:with-recursive-lock (,lock) ,@body)
  #+ecl `(mp:with-lock (,lock) ,@body)
  #+ccl `(ccl:with-lock-grabbed (,lock) ,@body)
  #-(or sbcl ecl ccl) `(progn ,lock ,@body))
(defparameter *threads-available*
  #+(or (and sbcl sb-thread) (and ecl threads) ccl) t
  #-(or (and sbcl sb-thread) (and ecl threads) ccl) nil)

;;; ---------- the COMMAND coroutine ----------
;;; A built-in command started from AutoLISP runs in its own thread so that it can
;;; stay "active" between successive (command ...) calls, exactly like AutoCAD:
;;;   (command "_.line" p1 p2)  (command p3 "")

(defstruct cmdco
  thread (queue nil) (state :running) (interactive nil) (cancel nil) (result :ok) (lock (make-lock* "cmdco")))

(defvar *co* nil "The active command coroutine, if any.")

(defvar *in-worker* nil "Bound to T (thread-locally) inside a command worker thread.")
(defun in-worker-p () *in-worker*)
(defun co-active-p () (and *co* (not (eq (cmdco-state *co*) :done))))

(defun wait-until (pred)
  (loop until (funcall pred) do (sleep 0.001)))

(defun co-next-item ()
  "Called in the worker: next queued argument, or :INTERACTIVE."
  (let ((co *co*))
    (loop
      (with-lock* ((cmdco-lock co))
        (when (cmdco-cancel co) (error 'alisp-cancel))
        (cond ((cmdco-queue co) (setf (cmdco-state co) :running) (return (pop (cmdco-queue co))))
              ((cmdco-interactive co) (setf (cmdco-state co) :running) (return :interactive))
              (t (setf (cmdco-state co) :waiting))))
      (sleep 0.001))))

(defun co-start (fn items)
  (let ((co (make-cmdco :queue items)))
    (setf *co* co)
    (setf (cmdco-thread co)
          (spawn-thread "command"
                        (lambda ()
                          (let* ((*in-worker* t)
                                 (parent-out *output-fn*)
                                 (*output-fn* (lambda (s)
                                                (if (and (eql (sysvar "CMDECHO") 0) (not (cmdco-interactive co)))
                                                    s
                                                    (funcall parent-out s))))
                                 (r (handler-case (progn (funcall fn) :ok)
                                     (alisp-cancel () :cancel)
                                     (alisp-error (c) (list :error (alisp-error-msg c)))
                                     (error (c) (list :error (cl-error-text c))))))
                            (with-lock* ((cmdco-lock co))
                              (setf (cmdco-result co) r (cmdco-state co) :done))))))
    (unless (cmdco-thread co)            ; no threads: ran synchronously
      (setf (cmdco-state co) :done))
    co))

(defun co-wait-idle ()
  (let ((co *co*))
    (wait-until (lambda () (member (cmdco-state co) '(:waiting :done))))))

(defun co-raise-result (co)
  (let ((r (cmdco-result co)))
    (cond ((eq r :cancel) (error 'alisp-cancel))
          ((consp r) (aerr "~A" (second r))))))

(defun co-cancel ()
  (when (co-active-p)
    (let ((co *co*))
      (with-lock* ((cmdco-lock co)) (setf (cmdco-cancel co) t))
      (wait-until (lambda () (eq (cmdco-state co) :done)))
      (setf *co* nil))))

(defun co-finish-interactively ()
  "At the end of a top-level Lisp evaluation, let a still-active command continue with the user."
  (when (co-active-p)
    (let ((co *co*))
      (with-lock* ((cmdco-lock co)) (setf (cmdco-interactive co) t))
      (wait-until (lambda () (eq (cmdco-state co) :done)))
      (setf *co* nil)
      (handler-case (co-raise-result co)
        (alisp-cancel () (out (format nil "*Cancel*~%")))
        (alisp-error (c) (out (format nil "~A~%" (alisp-error-msg c))))))))

(defun flatten-command-args (args)
  (loop for a in args
        collect (cond ((null a) "")
                      ((or (stringp a) (realp a) (ename-p a) (pickset-p a)) a)
                      ((pointp a) (pt3 a))
                      ((and (consp a) (ename-p (first a))) (list :entsel (first a) (pt3 (second a)))) ; (ename pt)
                      (t (bad-arg "command argument" a)))))

(defvar *in-undo-group*)
(defun alisp-command (args)
  (if (or *in-undo-group* (co-active-p) (in-worker-p))
      (alisp-command-1 args)
      (with-undo-group (lambda () (alisp-command-1 args)))))

(defun alisp-command-1 (args)
  (let ((items (flatten-command-args args)))
    (when (null items) (co-cancel) (return-from alisp-command-1 nil))
    (loop
      (cond
        ((and (co-active-p) (null items)) (return nil))
        ((co-active-p)
         (let ((co *co*))
           (with-lock* ((cmdco-lock co))
             (setf (cmdco-queue co) (append (cmdco-queue co) items) (cmdco-state co) :running))
           (setf items nil)
           (co-wait-idle)
           (when (eq (cmdco-state co) :done)
             (setf items (cmdco-queue co) *co* nil)
             (co-raise-result co))))
        ((null items) (return nil))
        (t (let ((name (pop items)))
             (cond ((equal name "") nil)
                   ((not (stringp name)) (bad-arg "command name" name))
                   (t (let ((fn (find-command-function name)))
                        (unless fn (aerr "Unknown command \"~A\"" (string-upcase (strip-command-prefix name))))
                        (if *threads-available*
                            (progn (co-start fn items) (setf items nil) (co-wait-idle)
                                   (let ((co *co*))
                                     (when (eq (cmdco-state co) :done)
                                       (setf items (cmdco-queue co) *co* nil)
                                       (co-raise-result co))))
                            (let ((*sync-queue* items))
                              (funcall fn)
                              (setf items *sync-queue*))))))))))
    nil))


(defun next-queued-input ()
  "Returns a queued COMMAND argument, :INTERACTIVE, or NIL when not inside (command)."
  (setf *from-script* nil)
  (cond ((in-worker-p) (co-next-item))
        ((listp *sync-queue*) (if *sync-queue* (pop *sync-queue*) :interactive))
        (*script-queue* (setf *from-script* t) (pop *script-queue*))
        (t nil)))

;;; ---------- keywords ----------

(defun parse-keywords (s)
  (when (stringp s)
    (let ((toks (loop with start = 0
                      for pos = (position #\Space s :start start)
                      for tok = (subseq s start pos)
                      when (plusp (length tok)) collect tok
                      while pos do (setf start (1+ pos)))))
      (loop for tk in toks until (char= (char tk 0) #\_) collect tk))))

(defun kw-abbrev (kw)
  (let ((caps (remove-if-not #'upper-case-p kw)))
    (if (or (zerop (length caps)) (= (length caps) (length (remove-if-not #'alpha-char-p kw))))
        kw caps)))

(defun match-keyword (text keywords)
  (let ((s (string-left-trim "_" text)))
    (when (plusp (length s))
      (or (find s keywords :test #'string-equal)
          (find s keywords :test (lambda (s kw) (string-equal s (kw-abbrev kw))))
          (find-if (lambda (kw) (and (<= (length (kw-abbrev kw)) (length s) (length kw))
                                     (string-equal s kw :end2 (length s))
                                     (let ((ab (kw-abbrev kw)))
                                       (string-equal ab kw :end2 (min (length kw) (length ab))))))
                   keywords)))))

;;; ---------- coordinate entry ----------

(defun parse-number-text (s) (or (distof s) nil))

(defun parse-point-text (s base)
  (let* ((s (string-trim " " s))
         (s (string-left-trim "#*" s))
         (rel (and (plusp (length s)) (char= (char s 0) #\@)))
         (body (if rel (subseq s 1) s))
         (last (or (sysvar "LASTPOINT") (pt 0 0)))
         (off
           (cond ((zerop (length body)) (and rel (pt 0 0)))
                 ((find #\< body)
                  (let* ((k (position #\< body))
                         (dd (parse-number-text (subseq body 0 k)))
                         (a (angtof (subseq body (1+ k)))))
                    (and dd a (polar (pt 0 0) a dd))))
                 ((find #\, body)
                  (let ((parts (loop with start = 0 for pos = (position #\, body :start start)
                                     collect (parse-number-text (subseq body start pos))
                                     while pos do (setf start (1+ pos)))))
                    (and (<= 2 (length parts) 3) (every #'identity parts)
                         (pt (first parts) (second parts) (or (third parts) 0)))))
                 (t nil))))
    (cond (off (if rel (v+ last off) off))
          ;; direct distance entry: a bare number along the rubber-band direction
          ((and base (not rel) (parse-number-text body))
           (polar base (direct-distance-angle base) (parse-number-text body)))
          (t nil))))

(defun direct-distance-angle (base)
  (let ((a (if (< (dist2d base *cursor-world*) 1d-12) 0d0 (ang base *cursor-world*))))
    (if (eql (sysvar "ORTHOMODE") 1)
        (* (round a (/ +pi+ 2)) (/ +pi+ 2))
        a)))

(defparameter *osnap-words*
  '(("END" . 1) ("ENDP" . 1) ("ENDPOINT" . 1) ("MID" . 2) ("MIDPOINT" . 2) ("CEN" . 4) ("CENTER" . 4)
    ("NOD" . 8) ("NODE" . 8) ("QUA" . 16) ("QUAD" . 16) ("QUADRANT" . 16) ("INT" . 32) ("INTERSECTION" . 32)
    ("APP" . 32) ("INS" . 64) ("INSERT" . 64) ("PER" . 128) ("PERP" . 128) ("TAN" . 256) ("NEA" . 512)
    ("NEAREST" . 512) ("NON" . -1) ("NONE" . -1)))

(defun osnap-bits-from-string (s)
  (reduce #'logior
          (loop with start = 0 for pos = (position #\, s :start start)
                for w = (string-trim " _" (subseq s start pos))
                for b = (cdr (assoc w *osnap-words* :test #'string-equal))
                when (and b (plusp b)) collect b
                while pos do (setf start (1+ pos)))
          :initial-value 0))

;;; ---------- object snap ----------

(defun pick-tolerance () (if (eq *ui* :gui) (gui-pixels->world (or (sysvar "PICKBOX") 5)) 1d-6))
(defun aperture-world () (if (eq *ui* :gui) (gui-pixels->world (or (sysvar "APERTURE") 10)) 1d-6))

(defun entities-near (p tol)
  (loop for e in (live-entities)
        when (and (layer-visible-p (find-layer (ent-layer e)))
                  (let ((b (entity-bbox e)))
                    (and b (<= (- (first b) tol) (px p) (+ (third b) tol))
                         (<= (- (second b) tol) (py p) (+ (fourth b) tol)))))
          collect e))

(defun osnap-candidates (p bits base tol)
  "List of (point . bit) candidates near P."
  (let* ((near-ents (entities-near p tol))
         (pieces (loop for e in near-ents
                       append (loop for pc in (snap-pieces e)
                                    when (<= (piece-dist pc p) tol) collect pc)))
         (cands nil))
    (flet ((add (q bit) (when (and q (<= (dist2d q p) tol)) (push (cons q bit) cands))))
      (dolist (pc pieces)
        (when (logtest bits 1) (dolist (q (piece-ends pc)) (add q 1)))
        (when (logtest bits 2) (dolist (q (piece-mids pc)) (add q 2)))
        (when (logtest bits 4) (let ((c (piece-center pc))) (when c (push (cons c 4) cands))))
        (when (logtest bits 8) (when (eq (first pc) :pt) (add (second pc) 8)))
        (when (logtest bits 16) (dolist (q (piece-quads pc)) (add q 16)))
        (when (and base (logtest bits 128)) (dolist (q (piece-perp pc base))
                                              (when (on-piece-p pc q 1d-6) (add q 128))))
        (when (and base (logtest bits 256)) (dolist (q (piece-tangents pc base)) (add q 256)))
        (when (logtest bits 512) (add (piece-nearest pc p) 512)))
      (when (logtest bits 32)
        (loop for (a . rest) on pieces
              do (dolist (b rest) (dolist (q (piece-ints a b)) (add q 32)))))
      (when (logtest bits 64)
        (dolist (e near-ents)
          (when (member (ent-type e) '("TEXT" "MTEXT" "INSERT") :test #'equal)
            (add (pt3 (text-insertion (ent-data e))) 64)))))
    cands))

(defun find-osnap (p &optional base (bits nil) (tol (aperture-world)))
  "Best snap near P: (values point bit) or NIL."
  (let* ((bits (or bits (osnap-active-bits)))
         (cands (and bits (plusp bits) (osnap-candidates p bits base tol))))
    (when cands
      ;; prefer specific snaps over NEArest; then closest to cursor
      (let* ((specific (remove 512 cands :key #'cdr))
             (pool (or specific cands))
             ;; coincident candidates: Endpoint beats Midpoint beats Center ... beats Intersection
             (prio '(1 2 4 8 16 64 32 128 256 512))
             (score (lambda (c) (+ (dist2d (car c) p) (* tol 1d-3 (or (position (cdr c) prio) 10)))))
             (best (reduce (lambda (a b) (if (<= (funcall score a) (funcall score b)) a b)) pool)))
        (values (pt3 (car best)) (cdr best))))))

(defun osnap-active-bits ()
  (cond ((eql *osnap-override* -1) 0)
        (*osnap-override* *osnap-override*)
        (t (let ((m (or (sysvar "OSMODE") 0))) (if (logtest m 16384) 0 (logand m 16383))))))

(defun apply-ortho (p base)
  (if (and base (eql (sysvar "ORTHOMODE") 1))
      (if (> (abs (- (px p) (px base))) (abs (- (py p) (py base))))
          (pt (px p) (py base) (pz base))
          (pt (px base) (py p) (pz base)))
      p))

(defun apply-grid-snap (p)
  (if (eql (sysvar "SNAPMODE") 1)
      (let* ((u (sysvar "SNAPUNIT")) (ux (d (first u))) (uy (d (second u))))
        (pt (* ux (fround (/ (px p) ux))) (* uy (fround (/ (py p) uy))) (pz p)))
      p))

(defun resolve-cursor-point (raw base &key (ortho t))
  "Snap/ortho processing for a clicked or hovered point: (values point snap-bit)."
  (multiple-value-bind (q bit) (find-osnap raw base)
    (if q (values q bit)
        (values (if ortho (apply-ortho (apply-grid-snap raw) base) (apply-grid-snap raw)) nil))))

;;; ---------- selection helpers ----------

(defun pick-entity-at (p &optional (tol (pick-tolerance)))
  (let ((best nil) (bd nil))
    (dolist (e (entities-near p tol) best)
      (dolist (pc (entity-pieces e))
        (let ((dd (cond ((eq (first pc) :gseg) (if (point-in-text-box-p p e) 0d0 (piece-dist pc p)))
                        ((equal (ent-type e) "HATCH") (if (point-in-hatch-p p (ent-data e)) (* 0.5 tol) (piece-dist pc p)))
                        (t (piece-dist pc p)))))
          (when (and (<= dd tol) (or (null bd) (< dd bd))) (setf best e bd dd)))))))

(defun point-in-text-box-p (p e)
  (let ((c (text-box (ent-data e))))
    (when (= (length c) 4)
      (let ((a (first c)) (b (second c)) (dd (fourth c)))
        (let ((u (seg-param p a b)) (v (seg-param p a dd)))
          (and (<= 0 u 1) (<= 0 v 1)))))))

(defun entities-in-window (p1 p2 crossing)
  (let ((x0 (min (px p1) (px p2))) (x1 (max (px p1) (px p2)))
        (y0 (min (py p1) (py p2))) (y1 (max (py p1) (py p2))))
    (loop for e in (live-entities)
          when (and (layer-visible-p (find-layer (ent-layer e)))
                    (let ((b (entity-bbox e)))
                      (and b
                           (or (and (<= x0 (first b)) (<= (third b) x1) (<= y0 (second b)) (<= (fourth b) y1))
                               (and crossing
                                    (not (or (> (first b) x1) (< (third b) x0) (> (second b) y1) (< (fourth b) y0)))
                                    (some (lambda (pc) (piece-crosses-rect-p pc x0 y0 x1 y1)) (entity-pieces e)))))))
            collect e)))

(defun filter-editable (ents &optional (report t))
  (let* ((ok (remove-if-not #'entity-editable-p ents)) (n (- (length ents) (length ok))))
    (when (and report (plusp n)) (out (format nil "~D ~A on a locked layer.~%" n (if (= n 1) "was" "were"))))
    ok))

(defun window-selection-drag (base)
  (lambda (cur)
    (list (list :rect base cur (if (< (px cur) (px base)) :crossing :window)))))

(defun select-objects (&key (prompt "Select objects: ") single (editable t))
  "AutoCAD-style object selection. Returns a list of enames."
  (let ((sel nil) (mode :add) (history nil))
    (labels ((report (found)
               (out (format nil "~D found~A~%" (length found)
                            (if (and (> (length sel) 0) (/= (length sel) (length found)))
                                (format nil ", ~D total" (length sel)) ""))))
             (take (found)
               (let ((found (if editable (filter-editable found) found)))
                 (if (eq mode :add)
                     (let ((new (remove-if (lambda (e) (member e sel)) found)))
                       (setf sel (append sel new)) (push new history)
                       (report found)
                       (ui-highlight new t))
                     (progn (setf sel (set-difference sel found))
                            (ui-highlight found nil)
                            (out (format nil "~D found, ~D removed, ~D total~%" (length found) (length found) (length sel)))))))
             (corner-select (p1 force)
               (let ((p2 (get-user-input :corner :base p1 :prompt "Specify opposite corner: "
                                                 :drag (window-selection-drag p1) :nosnap t)))
                 (when (pointp p2)
                   (take (entities-in-window p1 p2 (ecase force (:window nil) (:crossing t)
                                                     ((nil) (< (px p2) (px p1))))))))))
      (unwind-protect
           (loop
             (let ((r (get-user-input :select-point
                                      :prompt (if (eq mode :add) prompt "Remove objects: ")
                                      :keywords '("Window" "Crossing" "ALL" "Last" "Previous" "Remove" "Add" "Undo" "BOX" "SIngle"))))
               (cond
                 ((null r) (return))
                 ((pickset-p r) (take (remove-if-not #'ent-live-p (pickset-items r))))
                 ((ename-p r) (when (ent-live-p r) (take (list r))))
                 ((pointp r)
                  (let ((e (pick-entity-at r)))
                    (if e (take (list e))
                        (corner-select r nil))))
                 ((equal r "Window") (let ((p (get-user-input :point :prompt "Specify first corner: " :nosnap t))) (corner-select p :window)))
                 ((equal r "Crossing") (let ((p (get-user-input :point :prompt "Specify first corner: " :nosnap t))) (corner-select p :crossing)))
                 ((equal r "BOX") (let ((p (get-user-input :point :prompt "Specify first corner: " :nosnap t))) (corner-select p nil)))
                 ((equal r "ALL") (take (remove-if-not (lambda (e) (layer-visible-p (find-layer (ent-layer e)))) (live-entities))))
                 ((equal r "Last") (let ((e (last-entity))) (when e (take (list e)))))
                 ((equal r "Previous") (take (remove-if-not #'ent-live-p *previous-selection*)))
                 ((equal r "Remove") (setf mode :remove))
                 ((equal r "Add") (setf mode :add))
                 ((equal r "SIngle") (setf single t))
                 ((equal r "Undo") (let ((h (pop history)))
                                     (ui-highlight h nil) (setf sel (set-difference sel h)))))
               (when (and single sel) (return))))
        (ui-highlight sel nil)))
    (when sel (setf *previous-selection* sel))
    sel))

(defun entity-sample-point (e)
  (let ((pc (first (entity-pieces e))))
    (if pc
        (ecase (first pc)
          ((:seg :gseg) (midpt (second pc) (third pc)))
          (:pt (second pc))
          (:circle (polar (second pc) 0d0 (third pc)))
          (:arc (first (piece-mids pc))))
        (pt3 (or (dxf 10 (ent-data e)) (pt 0 0))))))

(defun sel-entity (sel)
  "Entity from an :entity input result (:pick point [ename])."
  (and (consp sel) (eq (first sel) :pick) (or (third sel) (pick-entity-at (second sel)))))
(defun sel-point (sel) (second sel))

(defun take-pickfirst ()
  (let ((s (remove-if-not #'ent-live-p *pickfirst*)))
    (ui-highlight *pickfirst* nil)
    (setf *pickfirst* nil)
    s))

(defvar *command-highlight* nil "Selection kept highlighted until the running command ends.")
(defun get-selection (&optional (prompt "Select objects: "))
  "Selection for editing commands: honours PICKFIRST (noun/verb) like AutoCAD."
  (let* ((pf (and (not (in-worker-p)) (not (listp *sync-queue*)) (take-pickfirst)))
         (sel (if pf
                  (progn (setf *previous-selection* pf) (filter-editable pf))
                  (select-objects :prompt prompt))))
    (when (and sel (not (in-worker-p)))
      (ui-highlight sel t)
      (setf *command-highlight* (append *command-highlight* sel)))
    sel))
(defun clear-command-highlight ()
  (when *command-highlight*
    (ui-highlight *command-highlight* nil)
    (setf *command-highlight* nil)))

;;; ---------- the central input routine ----------

(defun interpret-text (text req)
  "Turn typed TEXT into a value for REQ. Returns (values value ok-p)."
  (let* ((kind (inreq-kind req))
         (s (if (inreq-spaces req) text (string-trim " " text)))
         (kws (inreq-keywords req)))
    ;; transparent AutoLISP at any prompt: (expression) or !variable
    (when (and (member kind '(:point :corner :distance :angle :real :int :select-point))
               (plusp (length s)) (member (char s 0) '(#\( #\!)))
      (let ((v (if (char= (char s 0) #\!) (sym-value (asym (subseq s 1))) (alisp-eval-string s))))
        (return-from interpret-text (interpret-value v req))))
    (cond
      ((eq kind :string) (values text t))
      ((eq kind :command) (values s t))
      ((zerop (length s)) (values nil t))
      ((and kws (match-keyword s kws)) (values (match-keyword s kws) t))
      ((and (member kind '(:point :corner :distance :angle))
            (assoc (string-left-trim "_" s) *osnap-words* :test #'string-equal))
       (setf *osnap-override* (cdr (assoc (string-left-trim "_" s) *osnap-words* :test #'string-equal)))
       (values :retry t))
      (t
       (case kind
         ((:point :corner :select-point)
          (let ((p (parse-point-text s (and (not (eq kind :select-point)) (inreq-base req)))))
            (if p (values p t) (values nil nil))))
         (:distance
          (let ((n (parse-number-text s)))
            (cond (n (values (d n) t))
                  (t (let ((p (parse-point-text s nil)))
                       (cond ((and p (inreq-base req)) (values (dist2d (inreq-base req) p) t))
                             (p (let ((q (get-user-input :point :base p :prompt "Specify second point: ")))
                                  (values (if (pointp q) (dist2d p q) 0d0) t)))
                             (t (values nil nil))))))))
         (:angle
          (let ((a (angtof s)))
            (cond (a (values a t))
                  (t (let ((p (parse-point-text s nil)))
                       (if (and p (inreq-base req)) (values (ang (inreq-base req) p) t) (values nil nil)))))))
         (:real (let ((n (parse-number-text s))) (if n (values (d n) t) (values nil nil))))
         (:int (if (every #'digit-char-p (string-left-trim "-+" s))
                   (values (parse-integer s) t) (values nil nil)))
         (t (values nil nil)))))))

(defun interpret-value (v req)
  "Interpret a non-text value (from (command ...) or a transparent expression)."
  (let ((kind (inreq-kind req)))
    (cond
      ((and (consp v) (eq (first v) :entsel))
       (case kind
         (:entity (values (list :pick (third v) (second v)) t))
         (:select-point (values (second v) t))
         ((:point :corner) (values (third v) t))
         (t (values nil nil))))
      ((stringp v) (if (equal v +pause+) (values :pause t) (interpret-text v req)))
      ((null v) (values nil t))
      ((pointp v)
       (let ((p (pt3 v)))
         (case kind
           ((:point :corner :select-point) (values p t))
           (:entity (values (list :pick p) t))
           (:distance (if (inreq-base req) (values (dist2d (inreq-base req) p) t)
                          (let ((q (get-user-input :point :base p :prompt "Specify second point: ")))
                            (values (dist2d p q) t))))
           (:angle (if (inreq-base req) (values (ang (inreq-base req) p) t)
                       (let ((q (get-user-input :point :base p :prompt "Specify second point: ")))
                         (values (ang p q) t))))
           (t (values nil nil)))))
      ((realp v)
       (case kind
         (:angle (values (rad v) t))
         ((:distance :real) (values (d v) t))
         (:int (if (integerp v) (values v t) (values nil nil)))
         ((:point :corner) (if (inreq-base req) (values (polar (inreq-base req) (direct-distance-angle (inreq-base req)) (d v)) t)
                               (values nil nil)))
         (:string (values (lisp-repr v) t))
         (t (values nil nil))))
      ((ename-p v)
       (case kind
         (:entity (values (list :pick (entity-sample-point v) v) t))
         (:select-point (values v t))
         (t (values nil nil))))
      ((pickset-p v)
       (case kind
         (:select-point (values v t))
         (:entity (let ((e (first (pickset-items v)))) (if e (values (list :pick (entity-sample-point e) e) t) (values nil nil))))
         (t (values nil nil))))
      (t (values nil nil)))))

(defun invalid-message (req)
  (case (inreq-kind req)
    ((:point :corner) (if (inreq-keywords req) "Point or option keyword required." "Invalid point."))
    (:distance "Requires numeric distance or two points.")
    (:angle "Requires valid numeric angle or second point.")
    (:real "Requires a real number.")
    (:int "Requires an integer value.")
    (t "Invalid input.")))

(defun get-user-input (kind &key (prompt "") base keywords (flags 0) drag spaces nosnap)
  "Ask for input of KIND. Returns the value, NIL for Enter, or a matched keyword string.
NOSNAP turns off object snap, grid snap and ortho (used for selection windows)."
  (let ((req (make-inreq :kind kind :prompt prompt :base (and base (pt3 base)) :keywords keywords
                         :flags flags :drag drag :spaces spaces :nosnap nosnap))
        (*osnap-override* nil))
    (loop
      (let ((item (next-queued-input)) value ok)
        (if (and item (not (eq item :interactive)) (not (equal item +pause+)))
            (progn
              (when *from-script* (out (format nil "~A~A~%" prompt item)))
              (multiple-value-setq (value ok) (interpret-value item req))
              (when (eq value :retry) (setf ok nil value nil))
              (unless ok
                (aerr "~A (got ~A at \"~A\")" (invalid-message req) (lisp-repr item) (string-right-trim " :" prompt))))
            ;; interactive
            (let ((raw (ui-acquire req)))
              (ecase (first raw)
                (:text (multiple-value-setq (value ok) (interpret-text (second raw) req)))
                (:point (setf value (second raw) ok t))
                (:pick (setf value (if (eq kind :entity) raw (second raw)) ok t))
                (:value (multiple-value-setq (value ok) (interpret-value (second raw) req))))))
        (cond
          ((eq value :retry) nil)
          ((not ok) (out (format nil "~A~%" (invalid-message req))))
          ((and (null value) (logtest flags 1) (not (member kind '(:string :command))))
           (out (format nil "Requires a value.~%")))
          ((and (realp value) (logtest flags 2) (zerop value))
           (out (format nil "Value must be nonzero.~%")))
          ((and (realp value) (logtest flags 4) (minusp value))
           (out (format nil "Value must be positive.~%")))
          (t
           (when (and (pointp value) (member kind '(:point :corner)))
             (setf (sysvar "LASTPOINT") value))
           (return value)))))))

;;; ---------- AutoLISP get* functions ----------

(defun take-initget ()
  (multiple-value-prog1 (values *initget-bits* *initget-keywords*)
    (setf *initget-bits* 0 *initget-keywords* nil)))

(defun split-pt-msg (args)
  (let ((base nil) (msg ""))
    (dolist (a args) (cond ((pointp a) (setf base a)) ((stringp a) (setf msg a))))
    (values base msg)))

(defun lisp-get (kind args &key base-allowed)
  (multiple-value-bind (base msg) (split-pt-msg args)
    (multiple-value-bind (bits kws) (take-initget)
      (unless base-allowed (setf base nil))
      (let ((v (get-user-input kind :prompt msg :base base :keywords kws :flags bits)))
        v))))

(defsubr "initget" (&optional a b)
  (let ((bits (if (integerp a) a 0)) (kw (cond ((stringp a) a) ((stringp b) b))))
    (setf *initget-bits* bits *initget-keywords* (parse-keywords kw))
    nil))
(defsubr "getpoint" (&optional a b) (lisp-get :point (list a b) :base-allowed t))
(defsubr "getcorner" (p &optional msg) (lisp-get :corner (list p msg) :base-allowed t))
(defsubr "getdist" (&optional a b) (lisp-get :distance (list a b) :base-allowed t))
(defsubr ("getangle" "getorient") (&optional a b) (lisp-get :angle (list a b) :base-allowed t))
(defsubr "getreal" (&optional msg) (lisp-get :real (list msg)))
(defsubr "getint" (&optional msg) (lisp-get :int (list msg)))
(defsubr "getkword" (&optional msg)
  (multiple-value-bind (bits kws) (take-initget)
    (loop
      (let ((v (get-user-input :keyword :prompt (or msg "") :keywords kws :flags bits)))
        (cond ((or (null v) (member v kws :test #'equal)) (return v))
              ((logtest bits 128) (return v))
              (t (out (format nil "Invalid option keyword.~%"))))))))
(defsubr "getstring" (&optional a b)
  (let ((cr (and a (not (stringp a)))) (msg (cond ((stringp a) a) ((stringp b) b) (t ""))))
    (take-initget)
    (or (get-user-input :string :prompt msg :spaces cr) "")))
(defsubr ("entsel" "nentsel" "nentselp") (&optional msg)
  (multiple-value-bind (bits kws) (take-initget)
    (loop
      (let ((r (get-user-input :entity :prompt (or msg "Select object: ") :keywords kws :flags bits)))
        (cond ((null r) (return nil))
              ((stringp r) (return r))
              ((and (consp r) (eq (first r) :pick))
               (let ((e (sel-entity r)))
                 (if e (return (list e (second r)))
                     (progn (out (format nil "~%Nothing selected.~%"))
                            (unless (eq *ui* :gui) (return nil)))))))))))

(defsubr ("command" "command-s" "vl-cmdf") (&rest args) (alisp-command args))

;;; ---------- selection sets ----------

(defun dxf-filter-match-p (data filter)
  "Evaluate an SSGET filter list against an entity alist."
  (let ((stack (list (list :and))) (pending-op nil))
    (labels ((value-test (pair)
               (let* ((code (car pair)) (want (cdr pair)) (op (or pending-op "="))
                      (have (if (= code 8) (dxf 8 data) (dxf code data))))
                 (setf pending-op nil)
                 (cond ((stringp want)
                        (and (stringp have)
                             (if (member code '(0 8 2 6 7 1))
                                 (wcmatch (string-upcase have) (string-upcase want))
                                 (wcmatch have want))))
                       ((realp want)
                        (and (realp have) (relop op have want)))
                       ((pointp want)
                        (and (pointp have)
                             (let ((ops (loop with s = op with start = 0 for pos = (position #\, s :start start)
                                              collect (string-trim " " (subseq s start pos))
                                              while pos do (setf start (1+ pos)))))
                               (loop for i from 0 below (length want)
                                     always (relop (or (nth i ops) (car (last ops))) (nth i have) (nth i want))))))
                       (t (aequal have want))))))
      (dolist (pair filter)
        (if (eql (car pair) -4)
            (let ((s (string-upcase (cdr pair))))
              (cond ((member s '("<AND" "<OR" "<NOT" "<XOR") :test #'string=)
                     (push (list (intern (subseq s 1) :keyword)) stack))
                    ((member s '("AND>" "OR>" "NOT>" "XOR>") :test #'string=)
                     (let* ((frame (pop stack)) (kind (first frame)) (vals (reverse (rest frame)))
                            (r (ecase kind
                                 (:and (every #'identity vals)) (:or (some #'identity vals))
                                 (:not (not (first vals))) (:xor (= 1 (count t vals))))))
                       (push (bool r) (cdr (first stack)))))
                    (t (setf pending-op (cdr pair)))))
            (push (value-test pair) (cdr (first stack)))))
      (every #'identity (rest (first stack))))))

(defun relop (op a b)
  (cond ((equal op "*") t)
        ((member op '("=" "==") :test #'equal) (= a b))
        ((member op '("!=" "/=" "<>") :test #'equal) (/= a b))
        ((equal op "<") (< a b)) ((equal op "<=") (<= a b))
        ((equal op ">") (> a b)) ((equal op ">=") (>= a b))
        ((equal op "&") (logtest (truncate a) (truncate b)))
        ((equal op "&=") (= (logand (truncate a) (truncate b)) (truncate b)))
        (t (= a b))))

(defun do-ssget (args)
  (let* ((mode (and (stringp (first args)) (string-upcase (string-left-trim "_" (first args)))))
         (rest (if mode (rest args) args))
         (filter (find-if (lambda (x) (and (consp x) (consp (first x)))) rest))
         (pts (remove-if-not #'pointp rest))
         (ents
           (cond
             ((null mode)
              (if pts
                  (let ((e (pick-entity-at (first pts)))) (and e (list e)))
                  (select-objects)))
             ((string= mode "X") (live-entities))
             ((string= mode "A") (live-entities))
             ((string= mode "I") (remove-if-not #'ent-live-p *pickfirst*))
             ((string= mode "L") (let ((e (last-entity))) (and e (list e))))
             ((string= mode "P") (remove-if-not #'ent-live-p *previous-selection*))
             ((string= mode "W") (entities-in-window (first pts) (second pts) nil))
             ((string= mode "C") (entities-in-window (first pts) (second pts) t))
             ((member mode '("WP" "CP" "F") :test #'string=)
              (let* ((poly (first (remove-if-not #'listp rest))))
                (polygon-select poly mode)))
             ((or (string= mode ":S") (string= mode "+.:S") (string= mode ":E:S") (string= mode ":S:E"))
              (select-objects :single t :editable nil))
             ((string= mode ":E") (select-objects :editable nil))
             (t (select-objects :editable nil))))
         (ents (if filter (remove-if-not (lambda (e) (dxf-filter-match-p (ent-data e) filter)) ents) ents)))
    (when ents (setf *previous-selection* ents))
    (and ents (make-pickset ents))))

(defun point-in-polygon-p (p poly)
  (let ((inside nil) (n (length poly)))
    (loop for i from 0 below n
          for a = (nth i poly) for b = (nth (mod (1+ i) n) poly)
          when (and (not (eq (> (py a) (py p)) (> (py b) (py p))))
                    (< (px p) (+ (px a) (/ (* (- (px b) (px a)) (- (py p) (py a))) (- (py b) (py a))))))
            do (setf inside (not inside)))
    inside))

(defun polygon-select (poly mode)
  (let ((edges (loop for (a b) on (if (string= mode "F") poly (append poly (list (first poly)))) while b
                     collect (list :seg (pt3 a) (pt3 b)))))
    (loop for e in (live-entities)
          for pcs = (entity-pieces e)
          for pts = (mapcan #'piece-points-for-bbox pcs)
          for crosses = (some (lambda (pc) (some (lambda (ed) (piece-ints pc ed)) edges)) pcs)
          when (cond ((string= mode "F") crosses)
                     ((string= mode "WP") (and pts (every (lambda (q) (point-in-polygon-p q poly)) pts) (not crosses)))
                     (t (or crosses (and pts (some (lambda (q) (point-in-polygon-p q poly)) pts)))))
            collect e)))

(defun apick (x) (if (pickset-p x) x (bad-arg "lselsetp" x)))
(defsubr "ssget" (&rest args) (do-ssget args))
(defsubr "ssadd" (&optional e ss)
  (cond ((null e) (make-pickset))
        ((null ss) (make-pickset (list e)))
        (t (unless (member e (pickset-items (apick ss)))
             (setf (pickset-items ss) (append (pickset-items ss) (list e))))
           ss)))
(defsubr "ssdel" (e ss)
  (when (member e (pickset-items (apick ss)))
    (setf (pickset-items ss) (remove e (pickset-items ss)))
    ss))
(defsubr "sslength" (ss) (length (pickset-items (apick ss))))
(defsubr "ssname" (ss i) (nth (aint (truncate (anum i))) (pickset-items (apick ss))))
(defsubr "ssmemb" (e ss) (and (member e (pickset-items (apick ss))) e))
(defsubr "sssetfirst" (&optional gset ss)
  (declare (ignore gset))
  (ui-highlight *pickfirst* nil)
  (setf *pickfirst* (and ss (pickset-items ss)))
  (ui-highlight *pickfirst* t)
  (list nil ss))
(defsubr "ssgetfirst" () (list nil (and *pickfirst* (make-pickset *pickfirst*))))
(defsubr "ssnamex" (ss &optional i)
  (let ((items (pickset-items (apick ss))))
    (if i (list (list 0 (nth i items) 0))
        (loop for e in items for k from 0 collect (list 0 e 0)))))

;;; ---------- osnap function ----------

(defsubr "osnap" (p modes)
  (let ((q (find-osnap (pt3 (apt p)) nil (osnap-bits-from-string (astr modes)) (max (aperture-world) 1d-6))))
    q))

;;; ---------- UI hooks with TTY defaults (the GUI redefines behaviour via *ui*) ----------

(defun ui-acquire (req)
  (if (eq *ui* :gui) (gui-acquire req) (tty-acquire req)))
(defun ui-highlight (ents on) (when (and (eq *ui* :gui) ents) (gui-highlight ents on)))

(defvar *tty-in* *standard-input*)
(defun tty-acquire (req)
  (write-string (inreq-prompt req) *standard-output*)
  (force-output *standard-output*)
  (let ((line (read-line *tty-in* nil :eof)))
    (when (eq line :eof) (error 'alisp-exit))
    (let ((line (string-right-trim '(#\Return) line)))
      (case (inreq-kind req)
        ((:entity) (let ((p (parse-point-text line nil)))
                     (if p (list :pick p) (list :text line))))
        (t (list :text line))))))
