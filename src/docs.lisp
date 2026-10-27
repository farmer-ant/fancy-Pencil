;;;; docs.lisp — several drawings open at once, each in its own tab (AutoCAD's multiple-document
;;;; interface). The active drawing lives in the usual globals (*dwg*, *sysvars*, the view ...);
;;;; switching tabs saves those into the outgoing document and loads the incoming one.
;;;; The clipboard (COPYCLIP / PASTECLIP) and AutoLISP functions are shared by all drawings.

(in-package :fancy-pencil)

(defstruct doc
  id dwg sysvars
  (vx 6d0) (vy 4.5d0) (vscale 60d0) view-history
  pickfirst previous-selection grdraw-items last-dim dim-p2 last-erased layiso-saved
  untitled
  block-edit
  view)              ; the 3D view direction (*VIEW*)        ; Block Editor tabs: (parent-doc block-name)

(defvar *docs* nil "Open documents, in tab order.")
(defvar *current-doc* nil)
(defvar *doc-counter* 0)
(defvar *untitled-counter* 0)
(defvar *tabs-shown* nil "Last tab list sent to the window (to avoid redundant updates).")

(defparameter *app-sysvars*
  '("OSMODE" "ORTHOMODE" "SNAPMODE" "GRIDMODE" "PICKBOX" "APERTURE" "FILEDIA" "CMDECHO" "PICKFIRST"
    "HIGHLIGHT" "LWDISPLAY" "SAVEFORMAT" "GRIPS" "POLARMODE" "AUTOSNAP" "LOGFILEMODE" "LOGFILEPATH" "DYNPICOORDS")
  "Settings that belong to the program rather than to one drawing: they carry over between tabs.")

(defun app-sysvar-values ()
  (loop for k in *app-sysvars*
        for (v found) = (multiple-value-list (gethash k *sysvars*))
        when found collect (cons k v)))
(defun restore-app-sysvars (app)
  (loop for (k . v) in app do (setf (gethash k *sysvars*) v)))

(defun next-untitled ()
  (format nil "Drawing~D.dxf" (incf *untitled-counter*)))

(defun current-doc ()
  "The active document, wrapping the current globals the first time it is asked for."
  (or *current-doc*
      (let ((d (make-doc :id (incf *doc-counter*) :untitled (next-untitled))))
        (setf *docs* (list d) *current-doc* d)
        (save-doc-state d)
        d)))

(defun untitled-name () (doc-untitled (current-doc)))

(defun save-doc-state (d)
  (setf (doc-dwg d) *dwg* (doc-sysvars d) *sysvars*
        (doc-vx d) *vx* (doc-vy d) *vy* (doc-vscale d) *vscale* (doc-view-history d) *view-history*
        (doc-pickfirst d) *pickfirst* (doc-previous-selection d) *previous-selection*
        (doc-grdraw-items d) *grdraw-items* (doc-last-dim d) *last-dim* (doc-dim-p2 d) *dim-p2*
        (doc-last-erased d) *last-erased* (doc-layiso-saved d) *layiso-saved*
        (doc-view d) *view*))

(defun load-doc-state (d)
  (let ((app (loop for k in *app-sysvars*
                   for (v found) = (multiple-value-list (gethash k *sysvars*))
                   when found collect (cons k v))))
    (setf *dwg* (doc-dwg d) *sysvars* (doc-sysvars d)
          *vx* (doc-vx d) *vy* (doc-vy d) *vscale* (doc-vscale d) *view-history* (doc-view-history d)
          *pickfirst* (remove-if-not #'ent-live-p (doc-pickfirst d)) *previous-selection* (doc-previous-selection d)
          *grdraw-items* (doc-grdraw-items d) *last-dim* (doc-last-dim d) *dim-p2* (doc-dim-p2 d)
          *last-erased* (doc-last-erased d) *layiso-saved* (doc-layiso-saved d)
          *view* (doc-view d))
    ;; program-wide settings follow you into the other drawing
    (loop for (k . v) in app do (setf (gethash k *sysvars*) v)))
  (setf *current-doc* d *drawn-modcount* -1 *command-highlight* nil)
  (clrhash *hl*)
  (clrhash *tblnext-state*)
  (clear-geom-cache))

(defun refresh-view-after-switch ()
  (when (gui-p)
    (gui-regen) (gui-sync) (refresh-grips) (refresh-palette) (refresh-tabs t)))

(defun switch-to-doc (d)
  (let ((cur (current-doc)))
    (unless (eq d cur)
      (save-doc-state cur)
      (load-doc-state d)
      (refresh-view-after-switch))
    d))

(defun new-doc ()
  "Open a fresh, empty drawing in a new tab and make it current."
  (let ((cur (current-doc)))
    (save-doc-state cur)
    (let ((d (make-doc :id (incf *doc-counter*) :untitled (next-untitled)
                       :dwg (make-drawing) :sysvars (make-hash-table :test #'equalp))))
      (setf *docs* (append *docs* (list d)))
      ;; start from default settings, then load them as the current drawing
      (let ((*sysvars* (doc-sysvars d))) (reset-sysvars))
      (let ((*sysvars* (doc-sysvars cur)))      ; carry program settings from the drawing we came from
        (loop for k in *app-sysvars*
              do (multiple-value-bind (v found) (gethash k *sysvars*)
                   (when found (setf (gethash k (doc-sysvars d)) v)))))
      (setf (doc-view-history d) nil)
      (load-doc-state d)
      (when (gui-p) (zoom-to-box 0 0 12 9))
      (refresh-view-after-switch)
      d)))

(defun doc-pristine-p (d)
  "An untouched new drawing: no file, nothing drawn, nothing changed (OPEN reuses its tab)."
  (let ((dw (if (eq d *current-doc*) *dwg* (doc-dwg d))))
    (and (null (drawing-filename dw))
         (zerop (drawing-modcount dw))
         (zerop (count-if-not #'ename-deleted (drawing-entities dw))))))

(defun doc-name (d)
  (let ((dw (if (eq d *current-doc*) *dwg* (doc-dwg d))))
    (if (drawing-filename dw) (file-namestring (drawing-filename dw)) (doc-untitled d))))

(defun doc-modified-p (d)
  (let ((dw (if (eq d *current-doc*) *dwg* (doc-dwg d))))
    (/= (drawing-modcount dw) (drawing-saved-modcount dw))))

(defun find-doc-by-file (f)
  (let ((tn (ignore-errors (namestring (truename f)))))
    (and tn (find-if (lambda (d) (let ((fn (drawing-filename (if (eq d *current-doc*) *dwg* (doc-dwg d)))))
                                   (and fn (equal (ignore-errors (namestring (truename fn))) tn))))
                     *docs*))))

(defun open-in-tab (f)
  "OPEN: show F in its own tab (or switch to it if it is already open)."
  (let ((already (find-doc-by-file f)))
    (cond
      (already (switch-to-doc already)
               (out (format nil "~A is already open.~%" (doc-name already))))
      ((doc-pristine-p (current-doc))
       (let ((app (app-sysvar-values)))
         (open-drawing f)
         (restore-app-sysvars app))
       (refresh-tabs t))
      (t
       (let* ((from (current-doc)) (d (new-doc)) (app (app-sysvar-values)))
         (open-drawing f)
         (restore-app-sysvars app)
         ;; nothing was loaded (bad file, failed conversion): drop the new tab again
         (when (and (null (drawing-filename *dwg*)) (doc-pristine-p d))
           (remove-doc d from)))))))

(defun remove-doc (d &optional prefer)
  "Close tab D without asking; show PREFER or a neighbour, or a new empty drawing if it was the last."
  (let* ((pos (position d *docs*))
         (rest (remove d *docs*)))
    (setf *docs* rest)
    (cond ((null rest)
           (let ((app (app-sysvar-values)))
             (setf *current-doc* nil *docs* nil *dwg* (make-drawing) *sysvars* (make-hash-table :test #'equalp))
             (reset-sysvars)
             (restore-app-sysvars app))
           (let ((nd (current-doc)))
             (load-doc-state nd)
             (when (gui-p) (zoom-to-box 0 0 12 9))
             (refresh-view-after-switch)))
          (t
           (let ((target (or (and prefer (member prefer rest) prefer)
                             (nth (min pos (1- (length rest))) rest))))
             (when (eq d *current-doc*) (setf *current-doc* nil))
             (load-doc-state target)
             (refresh-view-after-switch))))))

(defun close-current-doc ()
  "CLOSE: offer to save, then close. Returns NIL if cancelled."
  (when (ask-save-changes)
    (let ((d (current-doc)))
      (out (format nil "Closed ~A.~%" (doc-name d)))
      (remove-doc d (and (doc-block-edit d) (first (doc-block-edit d)))))
    t))

(defun refresh-tabs (&optional force)
  (when (gui-p)
    (current-doc)
    (let ((items (mapcar (lambda (d) (list (doc-id d) (doc-name d) (doc-modified-p d))) *docs*)))
      (when (or force (not (equal (cons (doc-id *current-doc*) items) *tabs-shown*)))
        (setf *tabs-shown* (cons (doc-id *current-doc*) items))
        (tk "settabs {~{{~D ~A ~D}~^ ~}} ~D"
            (loop for (id name m) in items append (list id (tq name) (if m 1 0)))
            (doc-id *current-doc*))))))

(defun doc-arg ()
  "The tab number given after DOCSWITCH / DOCCLOSE (from the menu event)."
  (let ((s (or (pop *script-queue*) (gstring "Drawing: "))))
    (find (ignore-errors (parse-integer (string-trim " " s))) *docs* :key #'doc-id)))

(defcmd "DOCSWITCH" () "Makes another open drawing current (used by the file tabs)."
  (let ((d (doc-arg))) (when d (switch-to-doc d))))

(defcmd "DOCCLOSE" () "Closes an open drawing (used by the file tabs' close buttons)."
  (let ((d (doc-arg)))
    (when d
      (let ((back (current-doc)))
        (switch-to-doc d)
        (if (close-current-doc)
            (when (and (not (eq back d)) (member back *docs*)) (switch-to-doc back))
            (when (member back *docs*) (switch-to-doc back)))))))

(defcmd "DOCNEXT" () "Switches to the next open drawing (Ctrl+Tab)."
  (let* ((ds *docs*) (i (position (current-doc) ds)))
    (when (> (length ds) 1) (switch-to-doc (nth (mod (1+ i) (length ds)) ds)))))

(defcmd "DOCPREV" () "Switches to the previous open drawing (Ctrl+Shift+Tab)."
  (let* ((ds *docs*) (i (position (current-doc) ds)))
    (when (> (length ds) 1) (switch-to-doc (nth (mod (1- i) (length ds)) ds)))))

(defcmd "CLOSE" () "Closes the current drawing."
  (close-current-doc))

(defcmd "CLOSEALL" () "Closes all open drawings."
  (loop for d in (copy-list *docs*)
        do (switch-to-doc d)
           (unless (close-current-doc) (return))))

(defcmd "SYSWINDOWS" () "Lists the open drawings."
  (loop for d in *docs* for i from 1
        do (out (format nil "~:[ ~;*~]~D  ~A~:[~; (modified)~]~%" (eq d (current-doc)) i (doc-name d) (doc-modified-p d)))))

(defun quit-all-docs ()
  "QUIT: offer to save every modified drawing. Returns NIL if cancelled."
  (loop for d in (copy-list *docs*)
        always (or (not (doc-modified-p d))
                   (progn (switch-to-doc d) (ask-save-changes)))))
