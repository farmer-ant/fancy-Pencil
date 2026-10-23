;;;; utilcmds.lisp — view, file, layer, block, inquiry and undo commands;
;;;; AutoLISP database/system-variable functions.

(in-package :fancy-pencil)

(defun gui-p () (eq *ui* :gui))
(defun from-lisp-p () (or (in-worker-p) (listp *sync-queue*) *script-queue*))

;;; ---------- view ----------

(defcmd "ZOOM" ("Z") "Changes the view magnification."
  (let ((r (gpoint "Specify corner of window, enter a scale factor (nX or nXP), or [All/Center/Extents/Previous/Window] <real time>: "
                   :keywords '("All" "Center" "Extents" "Previous" "Window" "E" "A" "P" "W"))))
    (when (gui-p)
      (cond ((null r) nil)
            ((member r '("Extents" "E") :test #'string-equal) (zoom-extents))
            ((member r '("All" "A") :test #'string-equal)
             (if *view* (zoom-extents)
                 (let ((b (or (drawing-extents) (list 0 0 12 9))))
                   (zoom-to-box (min 0 (first b)) (min 0 (second b)) (max 12 (third b)) (max 9 (fourth b))))))
            ((member r '("Previous" "P") :test #'string-equal) (zoom-previous))
            ((member r '("Window" "W") :test #'string-equal)
             (let* ((a (gpoint "Specify first corner: "))
                    (b (get-user-input :corner :base a :prompt "Specify opposite corner: ")))
               (when (pointp b)
                 (let ((a (w->uv a)) (b (w->uv b)))
                   (zoom-to-box (min (px a) (px b)) (min (py a) (py b)) (max (px a) (px b)) (max (py a) (py b)) 1d0)))))
            ((equal r "Center")
             (let ((c (gpoint "Specify center point: ")))
               (when c (let ((h (or (gdist (format nil "Enter magnification or height <~A>: " (fmt-len (/ *ch* *vscale*)))) (/ *ch* *vscale*))))
                         (let ((c (w->uv c))) (push-view) (setf *vx* (px c) *vy* (py c) *vscale* (/ *ch* h))) (gui-regen)))))
            ((pointp r)
             (let ((b (get-user-input :corner :base r :prompt "Specify opposite corner: ")))
               (when (pointp b)
                 (let ((r (w->uv r)) (b (w->uv b)))
                   (zoom-to-box (min (px r) (px b)) (min (py r) (py b)) (max (px r) (px b)) (max (py r) (py b)) 1d0)))))))))

;; scale factors such as 2x are typed at the ZOOM prompt; handled in the dispatcher hook below
(defun zoom-scale-text-p (s)
  (let ((s (string-trim " " s)))
    (and (> (length s) 1) (char-equal (char s (1- (length s))) #\x)
         (parse-number-text (string-right-trim "xXpP" s)))))

(defcmd "PAN" ("P" "-PAN") "Shifts the view."
  (let* ((a (gpoint "Specify base point or displacement: "))
         (b (and a (gpoint "Specify second point: " :base a))))
    (when (and (gui-p) (pointp a))
      (let ((dv (if (pointp b) (v- (w->uv b) (w->uv a)) (w->uv a))))
        (push-view) (setf *vx* (- *vx* (px dv)) *vy* (- *vy* (py dv))) (gui-regen)))))

(defcmd "REGEN" ("RE" "REDRAW" "R" "REGENALL" "REDRAWALL") "Regenerates the display."
  (setf *grdraw-items* nil)
  (when (gui-p) (gui-regen)))

;;; ---------- inquiry ----------

(defcmd "DIST" ("DI") "Measures the distance and angle between two points."
  (let* ((a (gpoint "Specify first point: ")) (b (and a (gpoint "Specify second point: " :base a))))
    (when (pointp b)
      (out (format nil "Distance = ~A,  Angle in XY Plane = ~A,  Angle from XY Plane = 0~%Delta X = ~A,  Delta Y = ~A,   Delta Z = ~A~%"
                   (fmt-len (dist a b)) (fmt-ang (ang a b))
                   (fmt-len (- (px b) (px a))) (fmt-len (- (py b) (py a))) (fmt-len (- (pz b) (pz a))))))))

(defcmd "ID" () "Displays the coordinates of a location."
  (let ((p (gpoint "Specify point: ")))
    (when p (out (format nil "X = ~A     Y = ~A     Z = ~A~%" (fmt-len (px p)) (fmt-len (py p)) (fmt-len (pz p)))))))

(defun polygon-area (pts)
  (/ (abs (loop for (a b) on (append pts (list (first pts))) while b
                sum (- (* (px a) (py b)) (* (px b) (py a)))))
     2))

(defun entity-area-perimeter (e)
  (let* ((data (ent-data e)) (type (dxf 0 data)))
    (cond ((equal type "CIRCLE") (let ((r (dxf 40 data))) (values (* +pi+ r r) (* +2pi+ r))))
          ((equal type "LWPOLYLINE")
           (let* ((pcs (lwpoly-pieces data))
                  (pts (loop for pc in pcs append (if (eq (first pc) :seg) (list (second pc))
                                                      (butlast (apply #'arc-points (append (rest pc) '(64)))))))
                  (len (loop for pc in pcs sum (if (eq (first pc) :seg) (dist2d (second pc) (third pc))
                                                   (* (third pc) (arc-sweep (fourth pc) (fifth pc)))))))
             (values (polygon-area pts) len)))
          ((equal type "ELLIPSE") (let ((pts (ellipse-points data 256)))
                                    (values (polygon-area pts) (loop for (a b) on pts while b sum (dist2d a b)))))
          (t nil))))

(defcmd "AREA" ("AA") "Calculates the area and perimeter of objects or a polygon of points."
  (let ((r (gpoint "Specify first corner point or [Object] <Object>: " :keywords '("Object"))))
    (if (or (null r) (equal r "Object"))
        (let ((sel (get-user-input :entity :prompt "Select objects: ")))
          (when (consp sel)
            (let ((e (sel-entity sel)))
              (multiple-value-bind (a p) (and e (entity-area-perimeter e))
                (if a (out (format nil "Area = ~A, ~A = ~A~%" (fmt-area a) (if (equal (ent-type e) "CIRCLE") "Circumference" "Perimeter") (fmt-len p)))
                    (out (format nil "Selected object does not have an area.~%")))))))
        (let ((pts (list r)))
          (loop for p = (gpoint "Specify next point or press ENTER for total: " :base (first pts)
                                :drag (lambda (cur) (lwpoly-pieces (make-lwpoly-data (reverse (cons cur pts)) t))))
                while (pointp p) do (push p pts))
          (setf pts (nreverse pts))
          (out (format nil "Area = ~A, Perimeter = ~A~%" (fmt-area (polygon-area pts))
                       (fmt-len (loop for (a b) on (append pts (list (first pts))) while b sum (dist2d a b)))))))))

(defun describe-entity (e)
  (let* ((data (ent-data e)) (type (dxf 0 data)))
    (with-output-to-string (o)
      (format o "~&~20@A      Layer: \"~A\"~%~26TSpace: Model space~%~20THandle = ~(~A~)~%"
              type (dxf 8 data) (ename-handle e))
      (when (dxf 62 data) (format o "~26TColor: ~D~%" (dxf 62 data)))
      (flet ((p (label pt) (format o "~10@A point, X=~10@A  Y=~10@A  Z=~10@A~%" label
                                   (fmt-len (px pt)) (fmt-len (py pt)) (fmt-len (pz pt)))))
        (cond
          ((equal type "LINE")
           (let ((a (dxf 10 data)) (b (dxf 11 data)))
             (p "from" a) (p "to" b)
             (format o "~10TLength = ~A,  Angle in XY Plane = ~A~%~10TDelta X = ~A, Delta Y = ~A~%"
                     (fmt-len (dist a b)) (fmt-ang (ang a b)) (fmt-len (- (px b) (px a))) (fmt-len (- (py b) (py a))))))
          ((member type '("CIRCLE" "ARC") :test #'equal)
           (p "center" (dxf 10 data))
           (format o "~10Tradius ~A~%" (fmt-len (dxf 40 data)))
           (if (equal type "CIRCLE")
               (format o "~10Tcircumference ~A~%~10Tarea ~A~%" (fmt-len (* +2pi+ (dxf 40 data))) (fmt-len (* +pi+ (expt (dxf 40 data) 2))))
               (format o "~10Tstart angle ~A~%~10Tend angle ~A~%~10Tlength ~A~%" (fmt-ang (dxf 50 data)) (fmt-ang (dxf 51 data))
                       (fmt-len (* (dxf 40 data) (arc-sweep (dxf 50 data) (dxf 51 data)))))))
          ((equal type "LWPOLYLINE")
           (format o "~10T~:[Open~;Closed~]~%" (lwpoly-closed-p data))
           (multiple-value-bind (a per) (entity-area-perimeter e)
             (format o "~10Tarea ~A~%~10Tlength ~A~%" (fmt-len a) (fmt-len per)))
           (loop for (v b) in (lwpoly-vertices data)
                 do (format o "~10Tat point  X=~10@A  Y=~10@A~@[   bulge ~A~]~%" (fmt-len (px v)) (fmt-len (py v))
                            (and (/= b 0) (fmt-len b)))))
          ((member type '("TEXT" "MTEXT") :test #'equal)
           (format o "~10TContents: ~A~%" (dxf 1 data)) (p "insertion" (dxf 10 data))
           (format o "~10Theight ~A~%~10Trotation angle ~A~%" (fmt-len (dxf 40 data)) (fmt-ang (or (dxf 50 data) 0d0))))
          ((equal type "INSERT")
           (format o "~10TBlock Name: \"~A\"~%" (dxf 2 data)) (p "at" (dxf 10 data))
           (format o "~10TX scale factor: ~A~%~10TY scale factor: ~A~%~10Trotation angle: ~A~%"
                   (fmt-len (or (dxf 41 data) 1)) (fmt-len (or (dxf 42 data) 1)) (fmt-ang (or (dxf 50 data) 0d0))))
          ((equal type "POINT") (p "at" (dxf 10 data)))
          (t (format o "~10T~S~%" (lisp-repr data))))))))

(defcmd "LIST" ("LI" "LS") "Displays property data for selected objects."
  (dolist (e (get-selection)) (out (describe-entity e))))

;;; ---------- undo ----------

(defun undo-one ()
  (let ((snap (progn (incf *undo-ran*) (pop (drawing-undo *dwg*)))))
    (if snap
        (progn (push (take-snapshot) (drawing-redo *dwg*)) (restore-snapshot snap) t)
        (progn (out (format nil "Everything has been undone~%")) nil))))

(defcmd "U" () "Reverses the most recent operation."
  (when (undo-one) (out (format nil "Undo~%"))))
(defcmd "UNDO" () "Reverses the effect of commands."
  (let ((n (get-user-input :int :prompt "Enter the number of operations to undo or [All] <1>: " :keywords '("All"))))
    (if (equal n "All")
        (loop while (drawing-undo *dwg*) do (undo-one))
        (loop repeat (or n 1) while (undo-one)))))
(defcmd "REDO" ("MREDO") "Reverses the effects of the previous UNDO."
  (let ((snap (progn (incf *undo-ran*) (pop (drawing-redo *dwg*)))))
    (if snap (progn (push (take-snapshot) (drawing-undo *dwg*)) (restore-snapshot snap) (out (format nil "Redo~%")))
        (out (format nil "Nothing to redo~%")))))

;;; ---------- layers ----------

(defcmd "LAYER" ("LA" "-LAYER" "DDLMODES" "LAYERP") "Manages layers."
  (if (and (gui-p) (not (from-lisp-p)))
      (show-layer-dialog)
      (layer-command-line)))

(defun layer-command-line ()
  (loop
    (out (format nil "Current layer:  \"~A\"~%" (sysvar "CLAYER")))
    (let ((k (gkw "Enter an option [?/Make/Set/New/ON/OFF/Color/Ltype/LWeight/Plot/Freeze/Thaw/LOck/Unlock]: "
                  '("?" "Make" "Set" "New" "ON" "OFF" "Color" "Ltype" "LWeight" "Plot" "Freeze" "Thaw" "LOck" "Unlock"))))
      (flet ((names (prompt) (let ((s (let ((x (gstring prompt t))) (if (string= x "") (sysvar "CLAYER") x))))
                               (loop with start = 0 for pos = (position #\, s :start start)
                                     for nm = (string-trim " " (subseq s start pos))
                                     when (plusp (length nm)) collect nm
                                     while pos do (setf start (1+ pos)))))
             (each (nms fn) (dolist (n nms)
                              (let ((matches (remove-if-not (lambda (l) (wcmatch (string-upcase (layer-name l)) (string-upcase n)))
                                                            (drawing-layers *dwg*))))
                                (if matches (mapc fn matches) (out (format nil "No matching layer names found.~%")))))))
        (cond
          ((null k) (return))
          ((equal k "?")
           (out (format nil "~%Layer name                     State     Color   Linetype~%"))
           (dolist (l (drawing-layers *dwg*))
             (out (format nil "~30A ~:[Off~;On ~] ~:[    ~;Frz ~]~:[ ~;L~]~:[ NoPlot~;       ~]  ~5D   ~A~%" (layer-name l) (layer-on l)
                          (layer-frozen l) (layer-locked l) (layer-plot l) (abs (layer-color l)) (layer-ltype l)))))
          ((equal k "Make") (let ((n (gstring "Enter name for new layer (becomes the current layer) <0>: " t)))
                              (when (plusp (length n)) (ensure-layer n) (setf (sysvar "CLAYER") (layer-name (find-layer n))))))
          ((equal k "Set") (let* ((n (gstring "Enter layer name to make current or <select object>: " t)) (l (find-layer n)))
                             (cond ((null l) (out (format nil "Cannot find layer \"~A\".~%" n)))
                                   ((layer-frozen l) (out (format nil "Cannot set a frozen layer current.~%")))
                                   (t (setf (sysvar "CLAYER") (layer-name l))))))
          ((equal k "New") (dolist (n (names "Enter name list for new layer(s): ")) (ensure-layer n)))
          ((equal k "ON") (each (names "Enter name list of layer(s) to turn on: ") (lambda (l) (setf (layer-on l) t))))
          ((equal k "OFF") (each (names "Enter name list of layer(s) to turn off or <select objects>: ") (lambda (l) (setf (layer-on l) nil))))
          ((equal k "Freeze") (each (names "Enter name list of layer(s) to freeze or <select objects>: ")
                                    (lambda (l) (if (string-equal (layer-name l) (sysvar "CLAYER"))
                                                    (out (format nil "Cannot freeze layer \"~A\".  It is the CURRENT layer.~%" (layer-name l)))
                                                    (setf (layer-frozen l) t)))))
          ((equal k "Thaw") (each (names "Enter name list of layer(s) to thaw: ") (lambda (l) (setf (layer-frozen l) nil))))
          ((equal k "LOck") (each (names "Enter name list of layer(s) to lock or <select objects>: ") (lambda (l) (setf (layer-locked l) t))))
          ((equal k "Plot")
           (let* ((pk (gkw "Enter a plotting preference [Plot/No plot] <Plot>: " '("Plot" "No") "Plot"))
                  (on (string-equal pk "Plot")))
             (each (names (format nil "Enter layer name(s) for this plot preference <~A>: " (sysvar "CLAYER")))
                   (lambda (l) (setf (layer-plot l) on)))))
          ((equal k "Unlock") (each (names "Enter name list of layer(s) to unlock or <select objects>: ") (lambda (l) (setf (layer-locked l) nil))))
          ((equal k "Ltype")
           (let ((n (gstring "Enter a loaded linetype name or [?] <Continuous>: " t)))
             (when (string= n "") (setf n "CONTINUOUS"))
             (if (ensure-ltype n)
                 (each (names (format nil "Enter name list of layer(s) for linetype \"~A\" <~A>: " n (sysvar "CLAYER")))
                       (lambda (l) (setf (layer-ltype l) (if (continuous-name-p n) "CONTINUOUS" n))))
                 (out (format nil "Linetype ~A not found.~%" n)))))
          ((equal k "LWeight")
           (let ((lw (let ((s (gstring "Enter lineweight (0.0mm - 2.11mm): "))) (or (parse-lweight-label s)
                                                                                      (let ((v (parse-number-text s))) (and v (round (* 100 v))))))))
             (if lw
                 (each (names (format nil "Enter name list of layers(s) for lineweight ~A <~A>: " (lineweight-label lw) (sysvar "CLAYER")))
                       (lambda (l) (setf (layer-lineweight l) lw)))
                 (out (format nil "Invalid lineweight.~%")))))
          ((equal k "Color")
           (let ((c (parse-color (gstring "Enter color name or number (1-255): "))))
             (if (and c (<= 1 c 255))
                 (each (names (format nil "Enter name list of layer(s) for color ~D <~A>: " c (sysvar "CLAYER")))
                       (lambda (l) (setf (layer-color l) (if (layer-on l) c (- c)))))
                 (out (format nil "Invalid color.~%"))))))
        (touch)))))

(defcmd "LAYMCUR" ("LMC" "LAYERMAKECURRENT") "Makes the layer of a selected object current."
  (let* ((pre (remove-if-not #'ent-live-p *pickfirst*))
         (e (if (= (length pre) 1)
                (progn (take-pickfirst) (first pre))
                (let ((sel (get-user-input :entity :prompt "Select object whose layer will become current: ")))
                  (and (consp sel) (sel-entity sel))))))
    (when e
      (let ((l (find-layer (ent-layer e))))
        (cond ((null l) nil)
              ((layer-frozen l) (out (format nil "Layer ~A is frozen and can't be made current.~%" (layer-name l))))
              (t (setf (sysvar "CLAYER") (layer-name l))
                 (out (format nil "~A is now the current layer.~%" (layer-name l)))
                 (when (gui-p) (sweep-highlights) (refresh-grips) (gui-sync))))))))

;;; ---------- blocks ----------

;;; ---------- BLOCK ----------

(defun valid-block-name-p (name)
  (and (plusp (length name)) (<= (length name) 255)
       (not (find-if (lambda (c) (find c "<>/\\\":;?*|,=`")) name))
       (char/= (char name 0) #\*)))

(defun make-block (name base ents mode)
  "Define block NAME from ENTS around BASE. MODE: :convert (replace them by the block),
:delete (remove them) or :retain (leave them as they are)."
  (setf (drawing-blocks *dwg*)
        (cons (make-block-def :name name :base (pt3 base) :entities (mapcar (lambda (e) (rem-dxf 5 (ent-data e))) ents))
              (remove name (drawing-blocks *dwg*) :key #'block-def-name :test #'string-equal)))
  (clear-geom-cache)
  (unless (eq mode :retain) (dolist (e ents) (delete-entity e)))
  (when (eq mode :convert)
    (add (list (cons 0 "INSERT") (cons 2 name) (cons 10 (pt3 base)) (cons 41 1d0) (cons 42 1d0) (cons 50 0d0))))
  (touch)
  (when (gui-p) (gui-regen))
  (out (format nil "Block ~A defined with ~D object~:P.~%" name (length ents))))

(defun block-dialog ()
  "AutoCAD's Block Definition dialog: name, base point, objects, and what happens to them."
  (let ((name "") (base (pt 0 0)) (ents (take-pickfirst)) (mode "Convert"))
    (loop
      (let* ((names (sort (loop for b in (drawing-blocks *dwg*)
                                unless (char= (char (block-def-name b) 0) #\*) collect (block-def-name b))
                          #'string-lessp))
             (r (tk-ask (format nil "blockdlg ~A ~A ~A ~A ~D ~A"
                                (tcl-list names) (tq name) (tq (fmt-len (px base))) (tq (fmt-len (py base)))
                                (length (remove-if-not #'ent-live-p ents)) (tq mode))))
             (f (split-string r (code-char 31)))
             (action (first f)))
        (when (or (null action) (string= action "CANCEL") (string= action "")) (return))
        (destructuring-bind (&optional n x y m) (rest f)
          (setf name (string-trim " " (or n "")) mode (or m mode))
          (let ((bx (and x (parse-number-text (string-trim " " x)))) (by (and y (parse-number-text (string-trim " " y)))))
            (when (and bx by) (setf base (pt bx by)))))
        (cond
          ((string= action "PICK")
           (let ((p (gpoint "Specify insertion base point: ")))
             (when (pointp p) (setf base p))))
          ((string= action "SELECT")
           (let ((sel (get-selection)))
             (when sel (setf ents sel))))
          ((string= action "OK")
           (setf ents (remove-if-not #'ent-live-p ents))
           (cond ((not (valid-block-name-p name))
                  (gui-confirm (if (string= name "") "Enter a name for the block."
                                   (format nil "\"~A\" can't be used as a block name (it may not contain < > / \\ \" : ; ? * | , = `)." name))
                               "ok"))
                 ((null ents) (gui-confirm "Select the objects for the block first (Select objects)." "ok"))
                 ((and (find-block name)
                       (not (string= "yes" (gui-confirm (format nil "A block named \"~A\" already exists. Redefine it?~%Its copies in the drawing will change too." name))))))
                 (t (make-block name base ents (cond ((string-equal mode "Retain") :retain)
                                                     ((string-equal mode "Delete") :delete)
                                                     (t :convert)))
                    (return)))))))))

(defcmd "BLOCK" ("B" "BMAKE") "Creates a block definition from selected objects (Block Definition dialog)."
  (if (and (gui-p) (not (from-lisp-p)))
      (block-dialog)
      (block-command-line)))

(defcmd "-BLOCK" () "Creates a block definition at the command line."
  (block-command-line))

(defun block-command-line ()
  (let ((name (gstring "Enter block name or [?]: " t)))
    (cond
      ((string= name "") nil)
      ((string= name "?") (dolist (b (drawing-blocks *dwg*))
                            (unless (char= (char (block-def-name b) 0) #\*) (out (format nil "  ~A~%" (block-def-name b))))))
      ((not (valid-block-name-p name)) (out (format nil "Invalid block name.~%")))
      (t
       (when (and (find-block name)
                  (not (equal "Yes" (gkw (format nil "Block \"~A\" already exists. Redefine it? [Yes/No] <N>: " name) '("Yes" "No") "No"))))
         (return-from block-command-line))
       (let* ((base (gpoint "Specify insertion base point: "))
              (ents (and base (get-selection))))
         (when ents (make-block name base ents :convert)))))))

(defun load-block-from-dxf (name)
  "Make a block NAME from a DXF file on the support path."
  (let ((path (find-file-in-support (if (search ".dxf" name :test #'char-equal) name (concatenate 'string name ".dxf")))))
    (when path
      (let ((saved *dwg*) (saved-vars (let ((h (make-hash-table :test #'equalp))) (maphash (lambda (k v) (setf (gethash k h) v)) *sysvars*) h)))
        (let ((ents nil) (blocks nil) (layers nil))
          (unwind-protect
               (progn (dxf-import path)
                      (setf ents (mapcar (lambda (e) (rem-dxf 5 (ent-data e))) (live-entities))
                            blocks (drawing-blocks *dwg*) layers (drawing-layers *dwg*)))
            (setf *dwg* saved *sysvars* saved-vars))
          (dolist (l layers) (unless (find-layer (layer-name l)) (setf (drawing-layers *dwg*) (append (drawing-layers *dwg*) (list l)))))
          (dolist (b blocks) (unless (find-block (block-def-name b)) (push b (drawing-blocks *dwg*))))
          (let* ((base-name (pathname-name (pathname path)))
                 (bn (if (or (find base-name blocks :key #'block-def-name :test #'string-equal) (find-block base-name))
                         (loop for i from 1 for n = (format nil "~A-~D" base-name i)
                               unless (or (find n blocks :key #'block-def-name :test #'string-equal) (find-block n))
                                 return n)
                         base-name)))
            (push (make-block-def :name bn :base (pt 0 0) :entities ents) (drawing-blocks *dwg*))
            bn))))))

(defcmd "INSERT" ("I" "-INSERT" "DDINSERT") "Inserts a block or a DXF drawing."
  (let* ((name (gstring "Enter block name or [?]: " t)))
    (cond
      ((string= name "") nil)
      ((string= name "?") (dolist (b (drawing-blocks *dwg*))
                            (unless (char= (char (block-def-name b) 0) #\*) (out (format nil "  ~A~%" (block-def-name b))))))
      (t
       (let ((b (or (find-block name) (let ((bn (load-block-from-dxf name))) (and bn (find-block bn))))))
         (if (null b) (out (format nil "Block \"~A\" not found.~%" name))
             (let* ((mk (lambda (p sx sy rot) (list (cons 0 "INSERT") (cons 2 (block-def-name b)) (cons 10 (pt3 p))
                                                    (cons 41 (d sx)) (cons 42 (d sy)) (cons 50 (d rot)))))
                    (p (gpoint "Specify insertion point or [Scale/Rotate]: " :keywords '("Scale" "Rotate")
                               :drag (lambda (cur) (list (list :data (funcall mk cur 1 1 0))))))
                    (sx 1d0) (rot 0d0))
               (loop while (stringp p) do
                 (cond ((equal p "Scale") (setf sx (or (greal "Specify scale factor for XYZ axes <1>: ") 1d0)))
                       ((equal p "Rotate") (setf rot (or (gangle "Specify rotation angle <0>: ") 0d0))))
                 (setf p (gpoint "Specify insertion point or [Scale/Rotate]: " :keywords '("Scale" "Rotate")
                                 :drag (lambda (cur) (list (list :data (funcall mk cur sx sx rot)))))))
               (when (pointp p)
                 (let* ((x (or (greal (format nil "Enter X scale factor, specify opposite corner, or [Corner/XYZ] <~A>: " (fmt-len sx))) sx))
                        (y (or (greal "Enter Y scale factor <use X scale factor>: ") x))
                        (r (or (gangle (format nil "Specify rotation angle <~A>: " (fmt-ang rot)) :base p
                                       :drag (lambda (cur) (list (list :data (funcall mk p x y (ang p cur))))))
                               rot)))
                   (add (funcall mk p x y r)))))))))))

(defcmd "PURGE" ("PU" "-PURGE") "Removes unused layers and block definitions."
  (let* ((used-layers (remove-duplicates (mapcar #'ent-layer (live-entities)) :test #'string-equal))
         (used-blocks (remove-duplicates (loop for e in (live-entities) when (dxf 2 (ent-data e)) collect (dxf 2 (ent-data e))) :test #'string-equal))
         (nl 0) (nb 0))
    (setf (drawing-layers *dwg*)
          (remove-if (lambda (l) (and (not (string-equal (layer-name l) "0")) (not (string-equal (layer-name l) (sysvar "CLAYER")))
                                      (not (member (layer-name l) used-layers :test #'string-equal))
                                      (incf nl)))
                     (drawing-layers *dwg*)))
    (setf (drawing-blocks *dwg*)
          (remove-if (lambda (b) (and (not (member (block-def-name b) used-blocks :test #'string-equal)) (incf nb)))
                     (drawing-blocks *dwg*)))
    (touch)
    (out (format nil "~D layer~:P and ~D block~:P deleted.~%" nl nb))))

;;; ---------- files ----------

(defun drawing-modified-p () (/= (drawing-modcount *dwg*) (drawing-saved-modcount *dwg*)))

(defun ask-save-changes ()
  "Returns NIL if the user cancelled."
  (when (block-editor-p)                ; the Block Editor saves into the block, not to a file
    (return-from ask-save-changes
      (if (drawing-modified-p)
          (let ((r (if (and (gui-p) (not (from-lisp-p)))
                       (gui-confirm (format nil "Save the changes to block ~A?" (block-editor-name)) "yesnocancel")
                       "no")))
            (cond ((string= r "yes") (block-editor-save) t) ((string= r "no") t) (t nil)))
          t)))
  (if (and (drawing-modified-p) (gui-p) (not (from-lisp-p)))
      (let ((r (gui-confirm (format nil "Save changes to ~A?" (drawing-display-name)) "yesnocancel")))
        (cond ((string= r "yes") (save-drawing nil) t) ((string= r "no") t) (t nil)))
      t))

(defun ask-file (save title &optional initial (ext "dxf"))
  (if (and (gui-p) (eql (sysvar "FILEDIA") 1) (not (from-lisp-p)))
      (file-dialog save title :initial initial :ext ext)
      (let ((s (gstring (format nil "~A <~A>: " title (or initial "")) t)))
        (cond ((plusp (length s)) s) (initial initial) (t nil)))))

(defun ensure-ext (path ext)
  (if (pathname-type (pathname path)) path (concatenate 'string path "." ext)))

(defun save-drawing (as &key (format nil))
  "Save as DXF (FORMAT :R2000, the default, or :R12), or as DWG when the name ends in .dwg."
  (when (and (block-editor-p) (not as))   ; Save in the Block Editor saves the block
    (return-from save-drawing (block-editor-save)))
  (let* ((f (if (or as (null (drawing-filename *dwg*)))
                (ask-file t "Save Drawing As" (or (drawing-filename *dwg*) "Drawing1.dxf") "dxf dwg")
                (drawing-filename *dwg*)))
         (fmt (or format
                  (and as (from-lisp-p) nil)
                  (and as (not (from-lisp-p)) (not (gui-p))
                       (if (equal "R12" (gkw "File format [R12/2000] <2000>: " '("R12" "2000") "2000")) :r12 :r2000))
                  (if (equalp (sysvar "SAVEFORMAT") "R12") :r12 :r2000))))
    (when (and f (drawing-skipped *dwg*) (drawing-filename *dwg*)
               (equal (ignore-errors (namestring (truename f))) (ignore-errors (namestring (truename (drawing-filename *dwg*))))))
      (if (and (gui-p) (not (from-lisp-p)))
          (unless (string= "yes" (gui-confirm (format nil "~A had ~A that Fancy Pencil can't keep. Saving over it will remove them.~%~%Save over it anyway? (No = choose a new name)"
                                                      (file-namestring f) (skipped-summary (drawing-skipped *dwg*)))))
            (setf f (ask-file t "Save Drawing As" (drawing-filename *dwg*) "dxf dwg")))
          (out (format nil "Note: ~A had ~A that are not kept.~%" (file-namestring f) (skipped-summary (drawing-skipped *dwg*))))))
    (when f
      (if (dwg-name-p f)
          (save-as-dwg f)
          (let ((f (ensure-ext f "dxf")))
            (if (eq fmt :r12) (dxf-export f) (dxf-export-2000 f))
            (note-recent f)
            (out (format nil "Saved ~A (~A DXF)~%" f (if (eq fmt :r12) "R12" "AutoCAD 2000"))))))))

(defun save-as-dwg (f)
  "Write F as DWG through ODA File Converter (keeping the previous file as .bak); without it, save
a DXF next to it instead and leave the DWG untouched."
  (if (eq (find-dwg-converter) :oda)
      (let* ((tmp (concatenate 'string (temp-dir "out") "drawing.dxf")))
        (dxf-export-2000 tmp)
        (when (probe-file f)
          (copy-file f (make-pathname :type "bak" :defaults (pathname f))))
        (multiple-value-bind (ok msg) (dxf->dwg tmp f)
          (if ok
              (progn (setf (drawing-filename *dwg*) (namestring f)
                           (drawing-saved-modcount *dwg*) (drawing-modcount *dwg*))
                     (note-recent f)
                     (out (format nil "Saved ~A (AutoCAD 2018 DWG)~%" f)))
              (progn (setf (drawing-saved-modcount *dwg*) -1)
                     (tell-user msg)))))
      (let ((dxf (namestring (make-pathname :type "dxf" :defaults (pathname f)))))
        (if (and (gui-p) (not (from-lisp-p)))
            ;; let the person pick (and confirm overwriting) the DXF name
            (progn
              (tell-user (format nil "Saving in DWG format needs ODA File Converter, so the drawing will be saved as DXF ~
                                      instead. The DWG file is not changed."))
              (setf dxf (ask-file t "Save Drawing As DXF" dxf "dxf")))
            ;; scripts: never overwrite an existing file
            (loop for i from 1 while (probe-file dxf)
                  do (setf dxf (namestring (make-pathname :name (format nil "~A-~D" (pathname-name (pathname f)) i)
                                                          :type "dxf" :defaults (pathname f))))))
        (when dxf
          (let ((dxf (ensure-ext dxf "dxf")))
            (dxf-export-2000 dxf)
            (out (format nil "Saved ~A (AutoCAD 2000 DXF). The DWG file was not changed.~%" dxf)))))))

(defun open-drawing (f)
  (cond
    ((not (probe-file f)) (tell-user (format nil "Cannot find ~A" f)))
    ((dwg-file-p f)
     (out (format nil "Converting ~A from DWG...~%" (file-namestring f)))
     (when (gui-p) (tk "update idletasks"))
     (multiple-value-bind (dxf msg) (dwg->dxf f)
       (if (null dxf)
           (tell-user msg)
           (handler-case
               (let ((n (dxf-import dxf)))
                 (setf (drawing-filename *dwg*) (namestring (truename f)))
                 (after-open f n)
                 (out (format nil "(converted from DWG with ~A)~%"
                              (if (eq (find-dwg-converter) :oda) "ODA File Converter" "LibreDWG")))
                 (when (zerop n)
                   (tell-user (format nil "~A was converted, but no objects came through.~%~%~
                                           ~:[ODA File Converter could not read anything from this file.~;~
                                           LibreDWG can't read some DWG files completely. ODA File Converter ~
                                           (free from opendesign.com) reads every AutoCAD version; once it is ~
                                           installed, Fancy Pencil uses it automatically.~]"
                                      (file-namestring f) (not (eq (find-dwg-converter) :oda))))))
             (error (c) (tell-user (format nil "The converted drawing could not be read: ~A" (cl-error-text c))))))))
    ((not (dxf-file-p f))
     (tell-user (format nil "~A is not a DXF or DWG drawing." (file-namestring f))))
    (t
     (handler-case (after-open f (dxf-import f))
       (file-error () (tell-user (format nil "Cannot open ~A" f)))))))

(defparameter *object-type-names*
  '(("IMAGE" "raster image" "raster images") ("PDFUNDERLAY" "PDF underlay" "PDF underlays")
    ("DWFUNDERLAY" "DWF underlay" "DWF underlays") ("DGNUNDERLAY" "DGN underlay" "DGN underlays")
    ("REGION" "region" "regions") ("3DSOLID" "3D solid" "3D solids") ("BODY" "body" "bodies")
    ("MLINE" "multiline" "multilines") ("MULTILEADER" "multileader" "multileaders")
    ("TABLE" "table" "tables") ("WIPEOUT" "wipeout" "wipeouts") ("OLE2FRAME" "OLE object" "OLE objects")
    ("ACAD_TABLE" "table" "tables") ("XLINE" "construction line" "construction lines") ("RAY" "ray" "rays")
    ("TOLERANCE" "tolerance frame" "tolerance frames") ("MESH" "mesh" "meshes") ("3DFACE" "3D face" "3D faces")))

(defun skipped-summary (skipped)
  (format nil "~{~A~^, ~}"
          (mapcar (lambda (c)
                    (let ((nm (assoc (car c) *object-type-names* :test #'string-equal)))
                      (format nil "~D ~A" (cdr c)
                              (cond ((null nm) (format nil "~A object~:P" (car c) (cdr c)))
                                    ((= (cdr c) 1) (second nm)) (t (third nm))))))
                  (sort (copy-list skipped) #'> :key #'cdr))))

(defun after-open (f n)
  (note-recent f)
  (setf *pickfirst* nil *view-history* nil)
  (clrhash *hl*) (clear-geom-cache)
  (setf (drawing-skipped *dwg*) *import-skipped*)
  (out (format nil "Opened ~A (~D object~:P).~%" f n))
  (when (gui-p) (zoom-extents) (gui-sync))
  (when *import-skipped*
    (tell-user (format nil "~A also contains ~A, which Fancy Pencil can't show or keep yet.~%~%~
                            The file itself is unchanged. Saving over it would leave these out, so use ~
                            Save As with a new name if you want to keep the original complete."
                       (file-namestring f) (skipped-summary *import-skipped*)))))

(defcmd "NEW" ("QNEW") "Creates a new drawing in its own tab."
  (new-doc)
  (out (format nil "~A~%" (doc-name (current-doc)))))
(defcmd "OPEN" () "Opens a drawing (DXF, or DWG through a converter)."
  (let ((f (ask-file nil "Select File" nil "dwg dxf")))
    (when f (open-in-tab f))))
(defcmd "QSAVE" () "Saves the current drawing." (save-drawing nil))
(defcmd "SAVE" () "Saves the drawing." (save-drawing nil))
(defcmd "SAVEAS" () "Saves the drawing under a new name." (save-drawing t))
(defcmd "DXFOUT" () "Exports the drawing as DXF (choose AutoCAD 2000 or R12)."
  (let ((fmt (if (equal "R12" (gkw "File format [R12/2000] <2000>: " '("R12" "2000") "2000")) :r12 :r2000)))
    (save-drawing t :format fmt)))
(defcmd "SAVEASR12" () "Saves an AutoCAD R12 DXF (no lineweights or hatches)." (save-drawing t :format :r12))
(defcmd "DXFIN" ("IMPORT") "Imports a DXF drawing into the current drawing."
  (let ((f (ask-file nil "Select DXF File")))
    (when f
      (let ((bn (load-block-from-dxf f)))
        (when bn
          (dolist (dd (block-def-entities (find-block bn))) (add-entity dd))
          (setf (drawing-blocks *dwg*) (remove bn (drawing-blocks *dwg*) :key #'block-def-name :test #'string-equal))
          (out (format nil "Imported ~A.~%" f)))))))

(defcmd "APPLOAD" ("AP") "Loads an AutoLISP (.lsp) file."
  (let ((f (ask-file nil "Load AutoLISP File")))
    (when f
      (let ((r (run-lisp-file f)))
        (declare (ignore r))
        (out (format nil "~A successfully loaded.~%" (file-namestring f)))))))

(defun tokenize-script (text)
  (let ((items nil) (cur (make-string-output-stream)) (depth 0) (instr nil))
    (flet ((flush () (push (get-output-stream-string cur) items)))
      (loop for c across text do
        (cond (instr (write-char c cur) (when (char= c #\") (setf instr nil)))
              ((char= c #\") (setf instr t) (write-char c cur))
              ((char= c #\() (incf depth) (write-char c cur))
              ((char= c #\)) (decf depth) (write-char c cur))
              ((and (zerop depth) (member c '(#\Space #\Newline))) (flush))
              ((char= c #\Return) nil)
              ((and (char= c #\;) (zerop depth)) nil)
              (t (write-char c cur))))
      (let ((last (get-output-stream-string cur))) (when (plusp (length last)) (push last items))))
    (nreverse items)))

(defcmd "SCRIPT" ("SCR") "Runs a sequence of commands from a script (.scr) file."
  (let ((f (ask-file nil "Select Script File")))
    (when f
      (let ((items (tokenize-script (format nil "~{~A~%~}" (read-file-lines f)))))
        (setf *script-queue* (append *script-queue* items))))))

;;; ---------- settings ----------

(defun setvar-prompt (name)
  (let* ((cur (getvar name))
         (s (gstring (format nil "Enter new value for ~A <~A>: " (string-upcase name) (if (stringp cur) cur (lisp-repr cur))) t)))
    (when (plusp (length s))
      (let ((v (cond ((integerp cur) (or (ignore-errors (parse-integer s)) (aerr "Requires an integer value.")))
                     ((realp cur) (or (parse-number-text s) (aerr "Requires a numeric value.")))
                     ((pointp cur) (or (parse-point-text s nil) (aerr "Requires a point.")))
                     (t s))))
        (setvar name v)))))

(defcmd "SETVAR" ("SET") "Lists or changes the values of system variables."
  (let ((n (gstring "Enter variable name or [?]: ")))
    (cond ((string= n "?")
           (let (names) (maphash (lambda (k v) (declare (ignore v)) (push k names)) *sysvars*)
             (dolist (k (sort names #'string<)) (out (format nil "~15A ~A~%" k (lisp-repr (getvar k)))))))
          ((plusp (length n)) (setvar-prompt n)))))

(defcmd "OSNAP" ("OS" "DSETTINGS" "DS" "DDOSNAP") "Sets running object snap modes (settings panel)."
  (if (and (gui-p) (not (from-lisp-p))) (osnap-dialog) (osnap-command-line)))
(defcmd "-OSNAP" () "Sets running object snap modes at the command line."
  (osnap-command-line))
(defun osnap-command-line ()
  (let* ((m (logand (sysvar "OSMODE") 16383))
         (names (loop for (bit . nm) in '((1 . "END") (2 . "MID") (4 . "CEN") (8 . "NOD") (16 . "QUA") (32 . "INT")
                                          (64 . "INS") (128 . "PER") (256 . "TAN") (512 . "NEA"))
                      when (logtest m bit) collect nm)))
    (out (format nil "Current osnap modes: ~:[None~;~:*~{~A~^,~}~]~%" names))
    (let ((s (gstring "Enter list of object snap modes (END,MID,CEN,NOD,QUA,INT,INS,PER,TAN,NEA or NONE): ")))
      (when (plusp (length s))
        (setf (sysvar "OSMODE") (if (string-equal s "none") 0 (osnap-bits-from-string s)))
        (when (gui-p) (gui-sync))))))

(defcmd "ORTHO" () "Turns ortho mode on or off."
  (let ((k (gkw (format nil "Enter mode [ON/OFF] <~A>: " (if (eql (sysvar "ORTHOMODE") 1) "ON" "OFF")) '("ON" "OFF"))))
    (when k (setf (sysvar "ORTHOMODE") (if (equal k "ON") 1 0)) (when (gui-p) (gui-sync)))))
(defcmd "GRID" () "Turns the grid on or off, or sets spacing."
  (let ((r (get-user-input :real :prompt (format nil "Specify grid spacing(X) or [ON/OFF] <~A>: " (fmt-len (first (sysvar "GRIDUNIT"))))
                                 :keywords '("ON" "OFF"))))
    (cond ((equal r "ON") (setf (sysvar "GRIDMODE") 1)) ((equal r "OFF") (setf (sysvar "GRIDMODE") 0))
          ((realp r) (setf (sysvar "GRIDUNIT") (list r r) (sysvar "GRIDMODE") 1)))
    (when (gui-p) (gui-regen) (gui-sync))))
(defcmd "SNAP" ("SN") "Sets snap spacing or turns snap on/off."
  (let ((r (get-user-input :real :prompt (format nil "Specify snap spacing or [ON/OFF] <~A>: " (fmt-len (first (sysvar "SNAPUNIT"))))
                                 :keywords '("ON" "OFF"))))
    (cond ((equal r "ON") (setf (sysvar "SNAPMODE") 1)) ((equal r "OFF") (setf (sysvar "SNAPMODE") 0))
          ((realp r) (setf (sysvar "SNAPUNIT") (list r r) (sysvar "SNAPMODE") 1)))
    (when (gui-p) (gui-sync))))
;;; ---------- UNITS ----------

(defparameter *length-unit-types* '((4 "Architectural") (2 "Decimal") (3 "Engineering") (5 "Fractional") (1 "Scientific")))
(defparameter *angle-unit-types* '((0 "Decimal Degrees") (1 "Deg/Min/Sec") (2 "Grads") (3 "Radians") (4 "Surveyor's Units")))
(defparameter *insunits-types* '((0 "Unitless") (1 "Inches") (2 "Feet") (4 "Millimeters") (5 "Centimeters") (6 "Meters")))

(defun length-precision-label (lu p)
  "How precision P looks for length type LU (the choices in the Units dialog)."
  (let ((dec (if (zerop p) "0" (format nil "0.~V,,,'0A" p ""))))
    (case lu
      (4 (if (zerop p) "0'-0\"" (format nil "0'-0 1/~D\"" (expt 2 p))))
      (5 (if (zerop p) "0" (format nil "0 1/~D" (expt 2 p))))
      (3 (format nil "0'-~A\"" dec))
      (1 (format nil "~AE+01" dec))
      (t dec))))

(defun angle-precision-label (au p)
  (let ((dec (if (zerop p) "0" (format nil "0.~V,,,'0A" p ""))))
    (case au
      (1 (cond ((zerop p) "0d") ((<= p 2) "0d00'") ((<= p 4) "0d00'00\"")
               (t (format nil "0d00'00.~V,,,'0A\"" (- p 4) ""))))
      (2 (format nil "~Ag" dec))
      (3 (format nil "~Ar" dec))
      (4 (cond ((zerop p) "N 0d E") ((<= p 2) "N 0d00' E") (t "N 0d00'00\" E")))
      (t dec))))

(defun units-dialog ()
  (let* ((lprec (format nil "{~{~A~^ ~}}"
                        (loop for (code nil) in *length-unit-types*
                              collect (format nil "~D {~{~A~^ ~}}" code
                                              (loop for p from 0 to 8 collect (tq (length-precision-label code p)))))))
         (aprec (format nil "{~{~A~^ ~}}"
                        (loop for (code nil) in *angle-unit-types*
                              collect (format nil "~D {~{~A~^ ~}}" code
                                              (loop for p from 0 to 8 collect (tq (angle-precision-label code p)))))))
         (samples (format nil "{~{~A~^ ~}}"
                          (loop for (code nil) in *length-unit-types*
                                append (loop for p from 0 to 8
                                             collect (format nil "~D,~D ~A" code p
                                                             (tq (format nil "~A, ~A" (rtos 1.5d0 code p) (rtos 22.75d0 code p))))))))
         (asamples (format nil "{~{~A~^ ~}}"
                           (loop for (code nil) in *angle-unit-types*
                                 append (loop for p from 0 to 8
                                              collect (format nil "~D,~D ~A" code p (tq (angtos (/ pi 4) code p)))))))
         (r (tk-ask (format nil "unitsdlg ~A ~A ~A ~A ~A ~A ~A ~D ~D ~D ~D ~D"
                            (tcl-list (mapcar (lambda (x) (format nil "~D ~A" (first x) (second x))) *length-unit-types*))
                            (tcl-list (mapcar (lambda (x) (format nil "~D ~A" (first x) (second x))) *angle-unit-types*))
                            (tcl-list (mapcar (lambda (x) (format nil "~D ~A" (first x) (second x))) *insunits-types*))
                            lprec aprec samples asamples
                            (sysvar "LUNITS") (sysvar "LUPREC") (sysvar "AUNITS") (sysvar "AUPREC") (sysvar "INSUNITS")))))
    (when (and (> (length r) 3) (string= (subseq r 0 3) "OK "))
      (destructuring-bind (lu lp au ap ins) (mapcar #'parse-integer (split-string (subseq r 3) #\Space))
        (setf (sysvar "LUNITS") lu (sysvar "LUPREC") lp (sysvar "AUNITS") au (sysvar "AUPREC") ap
              (sysvar "INSUNITS") ins)
        (touch)
        (update-coords) (refresh-palette)
        (out (format nil "Units: ~A, ~A; angles ~A, ~A; insertion units ~A.~%"
                     (second (assoc lu *length-unit-types*)) (length-precision-label lu lp)
                     (second (assoc au *angle-unit-types*)) (angle-precision-label au ap)
                     (second (assoc ins *insunits-types*))))))))

(defcmd "UNITS" ("UN" "-UNITS" "DDUNITS") "Sets the drawing's length and angle units, precision and insertion units."
  (if (and (gui-p) (not (from-lisp-p)))
      (units-dialog)
      (progn
        (let ((lu (get-user-input :int :prompt (format nil "Enter linear units: 1 Scientific, 2 Decimal, 3 Engineering, 4 Architectural, 5 Fractional <~D>: " (sysvar "LUNITS")))))
          (when (and lu (<= 1 lu 5)) (setf (sysvar "LUNITS") lu)))
        (let ((pr (get-user-input :int :prompt (format nil "Enter precision, 0 to 8 (digits after the point, or 1/2^n for fractions) <~D>: " (sysvar "LUPREC")))))
          (when (and pr (<= 0 pr 8)) (setf (sysvar "LUPREC") pr)))
        (let ((au (get-user-input :int :prompt (format nil "Enter angle units: 0 Decimal degrees, 1 Deg/min/sec, 2 Grads, 3 Radians, 4 Surveyor's <~D>: " (sysvar "AUNITS")))))
          (when (and au (<= 0 au 4)) (setf (sysvar "AUNITS") au)))
        (let ((ap (get-user-input :int :prompt (format nil "Enter angle precision (0 to 8) <~D>: " (sysvar "AUPREC")))))
          (when (and ap (<= 0 ap 8)) (setf (sysvar "AUPREC") ap)))
        (touch))))

(defcmd "HELP" ("?" "F1") "Lists commands."
  (let (names) (maphash (lambda (k v) (declare (ignore v)) (push k names)) *commands*)
    (out (format nil "~%Fancy Pencil commands (aliases in parentheses):~%"))
    (dolist (n (sort names #'string<))
      (let ((al (let (r) (maphash (lambda (a c) (when (string= c n) (push a r))) *aliases*) (sort r #'string<))))
        (out (format nil "  ~14A ~@[(~{~A~^ ~}) ~]~A~%" n al (gethash n *command-help*)))))
    (out (format nil "~%Type an AutoLISP expression such as (+ 1 2) at the Command: prompt, or !var to see a value.~%Functions named C:XXX become commands.~%"))))

(defcmd "ABOUT" () "Shows program information."
  (out (format nil "~A ~A — 2D drafting with AutoLISP. Running on ~A ~A.~%"
               *program-name* *version* (lisp-implementation-type) (lisp-implementation-version))))

(defvar *quit-requested* nil)
(defcmd "QUIT" ("EXIT") "Exits the program (offering to save each changed drawing)."
  (when (quit-all-docs) (setf *quit-requested* t)))

;;; ---------- AutoLISP: entity access ----------

(defun aename (x) (if (ename-p x) x (bad-arg "lentityp" x)))

(defun lisp-visible-data (data)
  "Entity data as AutoLISP sees it: DXF-style groups only."
  (if (equal (dxf 0 data) "HATCH")
      (hatch-to-groups data)
      (remove-if (lambda (p) (and (consp p) (keywordp (car p)))) data)))

(defsubr "entget" (e &optional apps)
  (declare (ignore apps))
  (let ((e (aename e)))
    (and (not (ename-deleted e))
         (cons (cons -1 e) (copy-tree (lisp-visible-data (ename-data e)))))))

(defun valid-entity-data-p (data)
  (let ((type (dxf 0 data)))
    (and (stringp type)
         (cond ((equal type "LINE") (and (pointp (dxf 10 data)) (pointp (dxf 11 data))))
               ((equal type "CIRCLE") (and (pointp (dxf 10 data)) (realp (dxf 40 data))))
               ((equal type "ARC") (and (pointp (dxf 10 data)) (realp (dxf 40 data)) (realp (dxf 50 data)) (realp (dxf 51 data))))
               ((equal type "POINT") (pointp (dxf 10 data)))
               ((member type '("TEXT" "MTEXT") :test #'equal) (and (pointp (dxf 10 data)) (stringp (dxf 1 data))))
               ((equal type "LWPOLYLINE") (assoc 10 data))
               ((equal type "INSERT") (and (stringp (dxf 2 data)) (find-block (dxf 2 data)) (pointp (dxf 10 data))))
               ((member type '("ELLIPSE" "SPLINE" "SOLID" "DIMENSION") :test #'equal) t)
               ((equal type "LEADER") (>= (count 10 data :key (lambda (p) (and (consp p) (car p)))) 2))
               ((equal type "HATCH") (and (dxf :loops data) t))
               (t nil)))))

(defun clean-entmake-data (lst)
  (when (and (equal (dxf 0 lst) "HATCH") (assoc 91 lst))
    (return-from clean-entmake-data (hatch-from-lisp lst)))
  (let ((data (remove-if (lambda (p) (and (consp p) (member (car p) '(-1 5 330 -2 100 410)))) (alst lst))))
    ;; coerce numbers in known real codes
    (mapcar (lambda (p)
              (cond ((and (consp p) (member (car p) '(40 41 42 43 50 51)) (realp (cdr p))) (cons (car p) (d (cdr p))))
                    ((and (consp p) (member (car p) '(10 11 12 13 14)) (pointp (cdr p)))
                     (if (and (equal (dxf 0 lst) "LWPOLYLINE")) (list (car p) (d (second p)) (d (third p))) (cons (car p) (pt3 (cdr p)))))
                    (t p)))
            data)))

(defsubr "entmake" (&optional lst)
  (let ((data (clean-entmake-data lst)))
    (when (valid-entity-data-p data) (add-entity data) lst)))
(defsubr "entmakex" (&optional lst)
  (let ((data (clean-entmake-data lst)))
    (when (valid-entity-data-p data) (add-entity data))))
(defsubr "entmod" (lst)
  (let ((e (dxf -1 (alst lst))))
    (when (and (ename-p e) (not (ename-deleted e)))
      (let ((data (cons (assoc 0 (ename-data e))
                        (cons (cons 5 (ename-handle e))
                              (remove-if (lambda (p) (and (consp p) (member (car p) '(-1 0 5)))) (clean-entmake-data lst))))))
        (when (valid-entity-data-p data)
          (set-entity-data e data)
          (when (gui-p) (rerender-entity e))
          lst)))))
(defsubr "entupd" (e) (when (gui-p) (rerender-entity (aename e))) e)
(defsubr "entdel" (e)
  (let ((e (aename e)))
    (if (ename-deleted e) (undelete-entity e) (delete-entity e))
    e))
(defsubr "entlast" () (last-entity))
(defsubr "entnext" (&optional e) (next-entity (and e (aename e))))
(defsubr "handent" (h) (let ((e (entity-by-handle (astr h)))) (and e (not (ename-deleted e)) e)))

;;; ---------- AutoLISP: tables ----------

(defvar *tblnext-state* (make-hash-table :test #'equalp))

(defun table-records (table)
  (let ((tb (string-upcase table)))
    (cond ((string= tb "LAYER")
           (mapcar (lambda (l) (list (cons 0 "LAYER") (cons 2 (layer-name l))
                                     (cons 70 (logior (if (layer-frozen l) 1 0) (if (layer-locked l) 4 0)))
                                     (cons 62 (if (layer-on l) (abs (layer-color l)) (- (abs (layer-color l)))))
                                     (cons 6 (layer-ltype l)) (cons 290 (if (layer-plot l) 1 0)) (cons 370 (layer-lineweight l))))
                   (drawing-layers *dwg*)))
          ((string= tb "BLOCK")
           (mapcar (lambda (b) (list (cons 0 "BLOCK") (cons 2 (block-def-name b)) (cons 70 (if (char= (char (block-def-name b) 0) #\*) 1 0))
                                     (cons 10 (block-def-base b))))
                   (reverse (drawing-blocks *dwg*))))
          ((string= tb "LTYPE")
           (cons (list (cons 0 "LTYPE") (cons 2 "CONTINUOUS") (cons 70 0) (cons 3 "Solid line") (cons 72 65) (cons 73 0) (cons 40 0d0))
                 (mapcar (lambda (lt) (append (list (cons 0 "LTYPE") (cons 2 (ltype-name lt)) (cons 70 0) (cons 3 (ltype-desc lt))
                                                    (cons 72 65) (cons 73 (length (ltype-pattern lt)))
                                                    (cons 40 (reduce #'+ (mapcar #'abs (ltype-pattern lt)))))
                                              (mapcar (lambda (x) (cons 49 x)) (ltype-pattern lt))))
                         (drawing-ltypes *dwg*))))
          ((string= tb "DIMSTYLE")
           (mapcar (lambda (ds) (list (cons 0 "DIMSTYLE") (cons 2 (dimstyle-name ds)) (cons 70 0)
                                      (cons 40 (dv (dimstyle-vars ds) :dimscale)) (cons 41 (dv (dimstyle-vars ds) :dimasz))
                                      (cons 140 (dv (dimstyle-vars ds) :dimtxt)) (cons 271 (dv (dimstyle-vars ds) :dimdec))))
                   (all-dimstyles)))
          ((string= tb "STYLE")
           (mapcar (lambda (st) (list (cons 0 "STYLE") (cons 2 (textstyle-name st)) (cons 70 0)
                                      (cons 40 (d (textstyle-height st))) (cons 41 (d (textstyle-width st)))
                                      (cons 50 (d (textstyle-oblique st))) (cons 71 (style-gen-flags st))
                                      (cons 42 (d (sysvar "TEXTSIZE"))) (cons 3 (style-font-file st))
                                      (cons 4 (textstyle-bigfont st))))
                   (all-textstyles)))
          ((member tb '("VIEW" "UCS" "VPORT" "DIMSTYLE" "APPID") :test #'string=)
           nil)
          (t nil))))

(defsubr "tblsearch" (table name &optional next)
  (declare (ignore next))
  (find (astr name) (table-records (astr table)) :key (lambda (r) (dxf 2 r)) :test #'string-equal))
(defsubr "tblnext" (table &optional rewind)
  (let* ((recs (table-records (astr table))) (key (string-upcase table))
         (i (if rewind 0 (gethash key *tblnext-state* 0))))
    (setf (gethash key *tblnext-state*) (1+ i))
    (nth i recs)))
(defsubr "tblobjname" (table name) (declare (ignore table name)) nil)
(defsubr "layoutlist" () nil)

;;; ---------- AutoLISP: system variables ----------

(defun current-date-julian ()
  (multiple-value-bind (s mi h dd mo y) (get-decoded-time)
    (let* ((a (floor (- 14 mo) 12)) (yy (- (+ y 4800) a)) (mm (- (+ mo (* 12 a)) 3))
           (jdn (+ dd (floor (+ (* 153 mm) 2) 5) (* 365 yy) (floor yy 4) (- (floor yy 100)) (floor yy 400) -32045)))
      (+ jdn (/ (+ (* h 3600) (* mi 60) s) 86400d0)))))

(defun getvar (name)
  (let ((n (string-upcase name)))
    (cond
      ((string= n "DWGNAME") (drawing-display-name))
      ((string= n "DWGPREFIX") (let ((f (drawing-filename *dwg*)))
                                 (if f (directory-namestring f) (namestring (truename ".")))))
      ((string= n "DWGTITLED") (if (drawing-filename *dwg*) 1 0))
      ((string= n "DBMOD") (if (drawing-modified-p) 1 0))
      ((string= n "ACADVER") "24.3s (Fancy Pencil)")
      ((string= n "PRODUCT") "Fancy Pencil")
      ((string= n "PLATFORM") (format nil "~A ~A" (software-type) (machine-type)))
      ((string= n "LOGINNAME") (or (getenv "USER") (getenv "USERNAME") "user"))
      ((string= n "CDATE") (multiple-value-bind (s mi h dd mo y) (get-decoded-time)
                             (+ (* y 10000) (* mo 100) dd (/ (+ (* h 10000) (* mi 100) s) 1000000d0))))
      ((string= n "DATE") (current-date-julian))
      ((string= n "MILLISECS") (mod (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second) (expt 2 31)))
      ((string= n "CMDACTIVE") (if (co-active-p) 1 0))
      ((string= n "CMDNAMES") (if (co-active-p) "COMMAND" ""))
      ((string= n "EXTMIN") (let ((b (drawing-extents))) (if b (pt (first b) (second b)) (pt 1d20 1d20 1d20))))
      ((string= n "EXTMAX") (let ((b (drawing-extents))) (if b (pt (third b) (fourth b)) (pt -1d20 -1d20 -1d20))))
      ((string= n "VIEWCTR") (pt *vx* *vy*))
      ((string= n "VIEWSIZE") (/ *ch* *vscale*))
      ((string= n "SCREENSIZE") (list (d *cw*) (d *ch*)))
      ((string= n "TILEMODE") 1)
      ((string= n "WORLDUCS") 1)
      ((string= n "UCSORG") (pt 0 0))
      ((string= n "UCSXDIR") (pt 1 0))
      ((string= n "UCSYDIR") (pt 0 1))
      ((string= n "CVPORT") 2)
      ((string= n "ELEVATION") (or (sysvar n) 0d0))
      ((string= n "THICKNESS") (or (sysvar n) 0d0))
      ((string= n "DIMSCALE") (or (sysvar n) 1d0))
      ((string= n "CURSORSIZE") (or (sysvar n) 100))
      ((string= n "CIRCLERAD") (or (sysvar n) 0d0))
      (t (multiple-value-bind (v found) (gethash n *sysvars*)
           (if found v nil))))))

(defparameter *read-only-sysvars*
  '("DWGNAME" "DWGPREFIX" "DWGTITLED" "DBMOD" "ACADVER" "PRODUCT" "PLATFORM" "LOGINNAME" "CDATE" "DATE"
    "MILLISECS" "CMDACTIVE" "CMDNAMES" "EXTMIN" "EXTMAX" "VIEWCTR" "VIEWSIZE" "SCREENSIZE"))

(defun setvar (name val)
  (let ((n (string-upcase name)))
    (cond
      ((member n *read-only-sysvars* :test #'string=) (aerr "AutoCAD variable setting rejected: ~S ~A" n (lisp-repr val)))
      ((string= n "CELTYPE")
       (unless (and (stringp val) (ensure-ltype val)) (aerr "AutoCAD variable setting rejected: \"CELTYPE\" ~A" (lisp-repr val)))
       (setf (sysvar "CELTYPE") (if (member val '("BYLAYER" "BYBLOCK") :test #'string-equal) (string-upcase val) val)))
      ((string= n "DIMSTYLE")
       (let ((ds (find-dimstyle val)))
         (unless ds (aerr "AutoCAD variable setting rejected: \"DIMSTYLE\" ~A" (lisp-repr val)))
         (apply-dimvars (dimstyle-vars ds)) (setf (sysvar "DIMSTYLE") (dimstyle-name ds))))
      ((string= n "CLAYER")
       (let ((l (find-layer (astr val))))
         (unless l (aerr "AutoCAD variable setting rejected: \"CLAYER\" ~A" (lisp-repr val)))
         (setf (sysvar "CLAYER") (layer-name l))))
      ((member n '("OSMODE" "ORTHOMODE" "SNAPMODE" "GRIDMODE" "CMDECHO" "LUPREC" "AUPREC" "LUNITS" "AUNITS"
                   "PICKBOX" "APERTURE" "PDMODE" "HIGHLIGHT" "BLIPMODE" "PICKFIRST" "MIRRTEXT" "FILEDIA"
                   "EXPERT" "ANGDIR" "SNAPSTYL" "SNAPISOPAIR" "ROLLOVERTIPS" "AUTOSNAPSIZE" "CURSORSIZE" "INSUNITS" "LWDISPLAY" "CELWEIGHT" "GRIPS" "GRIPOBJLIMIT"
                   "GRIPSIZE" "DIMDEC" "DIMTAD" "DIMCLRD" "DIMCLRE" "DIMCLRT" "DIMTIH" "DIMADEC" "PLINEGEN")
               :test #'string=)
       (unless (integerp val) (aerr "AutoCAD variable setting rejected: ~S ~A" n (lisp-repr val)))
       (setf (sysvar n) val))
      ((member n '("THICKNESS" "ELEVATION") :test #'string=)
       (unless (realp val) (aerr "AutoCAD variable setting rejected: ~S ~A" n (lisp-repr val)))
       (setf (sysvar n) (d val)))
      ((member n '("TEXTSIZE" "PDSIZE" "FILLETRAD" "OFFSETDIST" "ANGBASE" "DIMSCALE" "DIMTXT" "DIMASZ"
                   "DIMEXO" "DIMEXE" "DIMGAP" "PLINEWID" "CHAMFERA" "CHAMFERB" "CIRCLERAD" "LASTROTATION"
                   "LTSCALE" "CELTSCALE" "HPSCALE" "HPANG" "DIMCEN" "DIMLFAC" "DIMDLI" "DIMTSZ" "PEDITFUZZ"
                   "REVCLOUDARC" "BRKLSIZE")
               :test #'string=)
       (unless (realp val) (aerr "AutoCAD variable setting rejected: ~S ~A" n (lisp-repr val)))
       (setf (sysvar n) (d val)))
      ((member n '("SNAPUNIT" "GRIDUNIT" "LIMMIN" "LIMMAX" "LASTPOINT") :test #'string=)
       (unless (pointp val) (aerr "AutoCAD variable setting rejected: ~S ~A" n (lisp-repr val)))
       (setf (sysvar n) (if (string= n "LASTPOINT") (pt3 val) (list (d (first val)) (d (second val))))))
      (t (setf (sysvar n) val)))
    (when (gui-p)
      (when (member n '("CLAYER" "OSMODE" "ORTHOMODE" "SNAPMODE" "GRIDMODE" "SNAPSTYL" "SNAPISOPAIR") :test #'string=) (gui-sync))
      (when (member n '("GRIDMODE" "GRIDUNIT" "PDMODE" "PDSIZE" "LTSCALE" "LWDISPLAY" "SNAPSTYL") :test #'string=) (gui-regen)))
    val))

(defsubr "getvar" (name) (getvar (astr name)))
(defsubr "setvar" (name val) (setvar (astr name) val))
(defsubr "getcfg" (s) (declare (ignore s)) nil)
(defsubr "setcfg" (s v) (declare (ignore s)) v)

(defsubr "textbox" (elist)
  (let* ((data (remove-if (lambda (p) (and (consp p) (eql (car p) -1))) (alst elist)))
         (data (if (assoc 0 data) data (cons (cons 0 "TEXT") data)))
         (data (if (assoc 10 data) data (append data (list (cons 10 (pt 0 0))))))
         (data (set-dxf 10 (pt 0 0) (set-dxf 50 0d0 (if (assoc 40 data) data (append data (list (cons 40 (d (sysvar "TEXTSIZE"))))))))))
    (let ((c (text-box data)))
      (list (pt (px (first c)) (py (first c))) (pt (px (third c)) (py (third c)))))))
