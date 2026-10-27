;;;; express.lisp — command aliases from a .pgp file (ALIASEDIT, REINIT), Express Tools
;;;; commands, and the acet-* AutoLISP functions Express Tools programs rely on.

(in-package :fancy-pencil)

;;; ---------- aliases (.pgp) ----------
;;; Format is AutoCAD's acad.pgp:  ALIAS,  *COMMAND      ; comments start with ;
;;; External commands (no *) are also accepted:  NAME, shell-command, flags, *prompt,

(defvar *builtin-aliases* nil "Aliases defined in the source, used to write the default .pgp.")
(defvar *pgp-external* (make-hash-table :test #'equalp))

(defun user-pgp-path ()
  (let ((home (or (getenv "HOME") ".")))
    (merge-pathnames ".config/fancypencil/fancypencil.pgp" (concatenate 'string (string-right-trim "/" home) "/"))))

(defun old-user-pgp-path ()
  "Where LispDraft (this program's former name) kept the user's aliases."
  (let ((home (or (getenv "HOME") ".")))
    (merge-pathnames ".config/lispdraft/lispdraft.pgp" (concatenate 'string (string-right-trim "/" home) "/"))))

(defun default-pgp-path ()
  (merge-pathnames "../support/fancypencil.pgp" (or *gui-dir* (script-directory) "")))

(defun snapshot-builtin-aliases ()
  (unless *builtin-aliases*
    (let (pairs)
      (maphash (lambda (a c)
                 ;; "-LAYER" style names resolve to their command automatically, so they need no alias
                 (unless (and (> (length a) 1) (char= (char a 0) #\-) (string-equal (subseq a 1) c))
                   (push (cons a c) pairs)))
               *aliases*)
      (setf *builtin-aliases* (sort pairs #'string< :key #'car)))))

(defun parse-pgp (path)
  "Returns (values alias-pairs external-specs)."
  (let ((aliases nil) (ext nil))
    (dolist (raw (read-file-lines path))
      (let* ((line (string-trim '(#\Space #\Tab #\Return) (let ((c (position #\; raw))) (if c (subseq raw 0 c) raw)))))
        (when (and (plusp (length line)) (find #\, line))
          (let* ((parts (mapcar (lambda (x) (string-trim " " x)) (split-on line #\,)))
                 (alias (string-upcase (first parts))) (target (second parts)))
            (cond ((and target (plusp (length target)) (char= (char target 0) #\*))
                   (push (cons alias (string-upcase (string-trim " " (subseq target 1)))) aliases))
                  ((and target (plusp (length alias)))
                   (push (list alias target (or (fourth parts) "")) ext)))))))
    (values (nreverse aliases) (nreverse ext))))

(defun pgp-text (pairs)
  (with-output-to-string (o)
    (format o ";  Fancy Pencil command aliases (acad.pgp format)~%")
    (format o ";  ALIAS,  *COMMAND  - edit with ALIASEDIT or any text editor, then run REINIT.~%;~%")
    (dolist (p (sort (copy-list pairs) #'string< :key #'car))
      (format o "~8A *~A~%" (concatenate 'string (car p) ",") (cdr p)))))

(defun write-pgp (path pairs)
  (ensure-directories-exist path)
  (with-open-file (o path :direction :output :if-exists :supersede)
    (write-string (pgp-text pairs) o))
  path)

(defun load-aliases ()
  "Rebuild the alias table: the user's .pgp if present, else the shipped default (else built-ins)."
  (snapshot-builtin-aliases)
  (multiple-value-prog1 (load-aliases-1) (note-pgp-date)))

(defun load-aliases-1 ()
  (let ((path (cond ((probe-file (user-pgp-path)) (user-pgp-path))
                    ((probe-file (old-user-pgp-path)) (old-user-pgp-path))
                    ((probe-file (default-pgp-path)) (default-pgp-path)))))
    (if path
        (multiple-value-bind (pairs ext) (parse-pgp path)
          (clrhash *aliases*) (clrhash *pgp-external*)
          ;; built-in shortcuts first (so new ones, e.g. AL for ALIGN, work with an older file),
          ;; then the file's own, which win where they differ
          (dolist (p *builtin-aliases*) (setf (gethash (car p) *aliases*) (cdr p)))
          (dolist (p pairs) (setf (gethash (car p) *aliases*) (cdr p)))
          (dolist (x ext) (setf (gethash (first x) *pgp-external*) (rest x)))
          (values (length pairs) (namestring path)))
        (progn (clrhash *aliases*)
               (dolist (p *builtin-aliases*) (setf (gethash (car p) *aliases*) (cdr p)))
               (values (length *builtin-aliases*) "built-in defaults")))))

(defun current-alias-pairs ()
  (let (pairs) (maphash (lambda (a c) (push (cons a c) pairs)) *aliases*) (sort pairs #'string< :key #'car)))

(defcmd "REINIT" () "Reloads the command alias (.pgp) file."
  (multiple-value-bind (n from) (load-aliases)
    (out (format nil "~D aliases loaded from ~A.~%" n from))))

;;; ---------- the alias file itself (Tools > Command Aliases) ----------

(defvar *pgp-seen-date* nil "Write date of the user's .pgp when it was last loaded.")
(defvar *pgp-checked-at* 0)

(defun note-pgp-date ()
  (setf *pgp-seen-date* (ignore-errors (file-write-date (user-pgp-path)))))

(defun reload-aliases-if-changed ()
  "Pick up edits made to the alias file in a text editor (checked at most once a second)."
  (let ((now (get-universal-time)))
    (when (/= now *pgp-checked-at*)
      (setf *pgp-checked-at* now)
      (let ((d (ignore-errors (and (probe-file (user-pgp-path)) (file-write-date (user-pgp-path))))))
        (when (and d (not (eql d *pgp-seen-date*)))
          (let ((first-time (null *pgp-seen-date*)))
            (setf *pgp-seen-date* d)
            (unless first-time
              (multiple-value-bind (n from) (ignore-errors (load-aliases))
                (when n (out (format nil "Alias file changed: ~D aliases reloaded from ~A.~%" n from)))))))))))

(defun ensure-user-pgp ()
  "Make sure the user's own alias file exists (a copy of the shipped one, comments and all)."
  (let ((p (user-pgp-path)))
    (unless (probe-file p)
      (ensure-directories-exist p)
      (let ((src (cond ((probe-file (old-user-pgp-path)) (old-user-pgp-path))
                       ((probe-file (default-pgp-path)) (default-pgp-path)))))
        (if src
            (with-open-file (o p :direction :output :if-exists :supersede :external-format :utf-8)
              (dolist (l (read-file-lines src)) (write-line l o)))
            (write-pgp p (current-alias-pairs)))))
    (note-pgp-date)
    p))

(defun open-in-text-editor (path)
  "Open PATH in the desktop's text editor (a .pgp file would otherwise go to an encryption tool)."
  (let ((f (shell-quote (namestring path))))
    (run-shell
     (format nil "( d=$(xdg-mime query default text/plain 2>/dev/null); ~
                    if [ -n \"$d\" ] && command -v gtk-launch >/dev/null 2>&1 && gtk-launch \"${d%.desktop}\" ~A >/dev/null 2>&1; then exit 0; fi; ~
                    for e in gnome-text-editor gedit kate kwrite mousepad xed pluma featherpad leafpad; do ~
                      if command -v $e >/dev/null 2>&1; then exec $e ~A >/dev/null 2>&1; fi; done; ~
                    xdg-open ~A >/dev/null 2>&1 ) </dev/null >/dev/null 2>&1 &" f f f))))

(defcmd "ALIASFILE" ("PGP") "Opens your command alias file (fancypencil.pgp) in a text editor."
  (let ((p (ensure-user-pgp)))
    (out (format nil "Alias file: ~A~%Save it in the editor and the changes are used straight away.~%" (namestring p)))
    (when (gui-p) (open-in-text-editor p))))

(defcmd "ALIASFOLDER" () "Opens the folder that holds your command alias file."
  (let ((p (ensure-user-pgp)))
    (out (format nil "~A~%" (directory-namestring p)))
    (when (gui-p) (run-shell (format nil "(xdg-open ~A >/dev/null 2>&1 &)" (shell-quote (directory-namestring p)))))))

(defcmd "ALIASEDIT" ("ALIASES") "Edits command aliases."
  (if (and (gui-p) (not (from-lisp-p)))
      (let ((r (tk-ask (format nil "aliasdlg {~{~A~^ ~}}"
                               (loop for (a . c) in (current-alias-pairs) collect (tq a) collect (tq c))))))
        (unless (string= r "")
          (let ((pairs (loop for part in (split-on r (code-char #x1f))
                             for kv = (split-on part (code-char #x1e))
                             when (and (= (length kv) 2) (plusp (length (first kv))) (plusp (length (second kv))))
                               collect (cons (string-upcase (first kv)) (string-upcase (second kv))))))
            (write-pgp (user-pgp-path) pairs)
            (load-aliases)
            (out (format nil "~D aliases saved to ~A~%" (length pairs) (namestring (user-pgp-path)))))))
      (loop
        (let ((k (gkw "Enter an option [Add/Delete/List/Save/eXit] <eXit>: " '("Add" "Delete" "List" "Save" "eXit") "eXit")))
          (cond
            ((equal k "eXit") (return))
            ((equal k "List") (dolist (p (current-alias-pairs)) (out (format nil "  ~8A *~A~%" (car p) (cdr p)))))
            ((equal k "Add")
             (let ((a (string-upcase (gstring "Alias: "))) (c (string-upcase (string-left-trim "*" (gstring "Command: ")))))
               (when (and (plusp (length a)) (plusp (length c))) (setf (gethash a *aliases*) c))))
            ((equal k "Delete") (remhash (string-upcase (gstring "Alias to delete: ")) *aliases*))
            ((equal k "Save")
             (write-pgp (user-pgp-path) (current-alias-pairs))
             (out (format nil "Saved to ~A~%" (namestring (user-pgp-path))))))))))

(defun run-external-command (name)
  "PGP external commands: NAME, shell-command, flags, *prompt,"
  (let ((spec (gethash name *pgp-external*)))
    (when spec
      (destructuring-bind (cmd &optional flags prompt) spec
        (declare (ignore flags))
        (let* ((arg (if (and prompt (plusp (length (string-trim " *" prompt))))
                        (gstring (string-left-trim "*" prompt) t) ""))
               (line (format nil "~A ~A &" cmd arg)))
          #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" line) :wait nil :output nil :error nil)
          #+ecl (ext:run-program "/bin/sh" (list "-c" line) :wait nil :output nil :error nil)
          t)))))

;;; ---------- layer tools ----------

(defvar *layiso-saved* nil)

(defun pick-layer-of (prompt)
  (let ((e (sel-entity (get-user-input :entity :prompt prompt))))
    (and e (find-layer (ent-layer e)))))

(defun layer-pick-loop (prompt fn msg)
  (loop
    (let ((l (pick-layer-of prompt)))
      (unless l (return))
      (if (and (string-equal (layer-name l) (sysvar "CLAYER")) (member msg '("off" "frozen") :test #'string=))
          (out (format nil "Layer ~A is the current layer.~%" (layer-name l)))
          (progn (funcall fn l) (touch) (out (format nil "Layer \"~A\" has been ~A.~%" (layer-name l) msg)))))))

(defcmd "LAYISO" () "Express: isolates the layers of selected objects (turns the others off)."
  (let* ((ents (get-selection "Select objects on the layer(s) to be isolated: "))
         (keep (remove-duplicates (mapcar #'ent-layer ents) :test #'string-equal)))
    (when keep
      (setf *layiso-saved* (mapcar (lambda (l) (list (layer-name l) (layer-on l))) (drawing-layers *dwg*)))
      (dolist (l (drawing-layers *dwg*))
        (setf (layer-on l) (and (member (layer-name l) keep :test #'string-equal) t)))
      (unless (member (sysvar "CLAYER") keep :test #'string-equal) (setf (sysvar "CLAYER") (first keep)))
      (touch)
      (out (format nil "~D layer~:P ~:*~[were~;was~:;were~] isolated. Layer ~A is current.~%" (length keep) (sysvar "CLAYER"))))))

(defcmd "LAYUNISO" () "Express: restores layers turned off by LAYISO."
  (if (null *layiso-saved*) (out (format nil "LAYISO has not been used.~%"))
      (progn (loop for (n on) in *layiso-saved* for l = (find-layer n) when l do (setf (layer-on l) on))
             (setf *layiso-saved* nil) (touch) (out (format nil "Layers restored.~%")))))

(defcmd "LAYOFF" () "Express: turns off the layer of a selected object."
  (layer-pick-loop "Select an object on the layer to be turned off: " (lambda (l) (setf (layer-on l) nil)) "turned off"))
(defcmd "LAYFRZ" () "Express: freezes the layer of a selected object."
  (layer-pick-loop "Select an object on the layer to be frozen: " (lambda (l) (setf (layer-frozen l) t)) "frozen"))
(defcmd "LAYLCK" () "Express: locks the layer of a selected object."
  (layer-pick-loop "Select an object on the layer to be locked: " (lambda (l) (setf (layer-locked l) t)) "locked"))
(defcmd "LAYULK" () "Express: unlocks the layer of a selected object."
  (layer-pick-loop "Select an object on the layer to be unlocked: " (lambda (l) (setf (layer-locked l) nil)) "unlocked"))
(defcmd "LAYON" () "Express: turns all layers on."
  (dolist (l (drawing-layers *dwg*)) (setf (layer-on l) t)) (touch) (out (format nil "All layers have been turned on.~%")))
(defcmd "LAYTHW" () "Express: thaws all layers."
  (dolist (l (drawing-layers *dwg*)) (setf (layer-frozen l) nil)) (touch) (out (format nil "All layers have been thawed.~%")))

(defcmd "LAYCUR" () "Express: moves selected objects to the current layer."
  (let ((ents (get-selection "Select objects to be changed to the current layer: ")))
    (dolist (e ents) (set-entity-data e (set-dxf 8 (sysvar "CLAYER") (ent-data e))))
    (out (format nil "~D object~:P changed to layer \"~A\" (the current layer).~%" (length ents) (sysvar "CLAYER")))))

(defcmd "LAYMCH" ("LMH" "MATCHLAYER") "Changes selected objects to the layer of a destination object (Match Layer)."
  (let ((ents (let ((pre (remove-if-not #'ent-live-p *pickfirst*)))
                (if pre (take-pickfirst) (get-selection "Select objects to be changed: ")))))
    (when ents
      (ui-highlight ents t)
      (unwind-protect
           (let ((r (get-user-input :entity :prompt "Select object on destination layer or [Name]: " :keywords '("Name"))))
             (let ((l (cond ((equal r "Name")
                             (find-layer (if (and (gui-p) (not (from-lisp-p)))
                                             (let ((f (gui-form "Match Layer"
                                                                (list (list "L" "Change to layer:" (sysvar "CLAYER") :combo
                                                                            (mapcar #'layer-name (drawing-layers *dwg*))))
                                                                "OK")))
                                               (or (and f (cdr (assoc "L" f :test #'string=))) ""))
                                             (gstring "Enter layer name: " t))))
                            ((consp r) (let ((e (sel-entity r))) (and e (find-layer (ent-layer e))))))))
               (when l
                 (dolist (e (filter-editable ents)) (set-entity-data e (set-dxf 8 (layer-name l) (ent-data e))))
                 (out (format nil "~D object~:P changed to layer \"~A\".~%" (length ents) (layer-name l))))))
        (ui-highlight ents nil)
        (when (gui-p) (gui-regen))))))

(defun pick-layer-dialog (title current)
  "A list of the drawing's layers with their colors; returns the chosen layer name or NIL."
  (let ((r (tk-ask (format nil "layerpickdlg ~A {~{~A~^ ~}} ~A" (tq title)
                           (loop for l in (drawing-layers *dwg*)
                                 collect (format nil "{~A ~A ~D ~D}" (tq (layer-name l)) (aci->hex (abs (layer-color l)))
                                                 (if (layer-frozen l) 1 0) (if (layer-locked l) 1 0)))
                           (tq current)))))
    (and (> (length r) 3) (string= (subseq r 0 3) "OK ") (subseq r 3))))

(defun change-layer-of (ents name)
  (let ((l (find-layer name)) (ok (filter-editable ents)))
    (when l
      (dolist (e ok) (set-entity-data e (set-dxf 8 (layer-name l) (ent-data e))))
      (out (format nil "~D object~:P moved to layer \"~A\".~%" (length ok) (layer-name l)))
      (when (gui-p) (gui-regen) (refresh-palette)))))

(defcmd "CHANGELAYER" ("CL" "-CHANGELAYER" "LAYCHANGE") "Moves selected objects to a layer you choose from a list."
  (let ((ents (let ((pre (remove-if-not #'ent-live-p *pickfirst*)))
                (if pre (take-pickfirst) (get-selection "Select objects to move to another layer: ")))))
    (when ents
      (ui-highlight ents t)
      (unwind-protect
           (let ((name (if (and (gui-p) (not (from-lisp-p)))
                           (pick-layer-dialog (format nil "Change ~D object~:P to layer" (length ents))
                                              (let ((ls (remove-duplicates (mapcar #'ent-layer ents) :test #'string-equal)))
                                                (if (= (length ls) 1) (first ls) (sysvar "CLAYER"))))
                           (let ((s (gstring (format nil "Enter layer name <~A>: " (sysvar "CLAYER")) t)))
                             (if (string= s "") (sysvar "CLAYER") s)))))
             (cond ((null name) nil)
                   ((not (find-layer name)) (out (format nil "No layer named ~A.~%" name)))
                   (t (change-layer-of ents name))))
        (ui-highlight ents nil)))))

(defcmd "LAYMRG" ("-LAYMRG") "Express: merges layers into a target layer."
  (let* ((from (remove-duplicates (mapcar #'ent-layer (select-objects :prompt "Select object on layer to merge: " :editable nil))
                                  :test #'string-equal))
         (r (get-user-input :entity :prompt "Select object on target layer or [Name]: " :keywords '("Name")))
         (target (cond ((equal r "Name") (find-layer (gstring "Enter layer name: " t)))
                       ((consp r) (let ((e (sel-entity r))) (and e (find-layer (ent-layer e))))))))
    (when (and from target)
      (let ((n 0))
        (dolist (e (all-live-entities))
          (when (member (ent-layer e) from :test #'string-equal)
            (set-entity-data e (set-dxf 8 (layer-name target) (ent-data e))) (incf n)))
        (setf (drawing-layers *dwg*)
              (remove-if (lambda (l) (and (member (layer-name l) from :test #'string-equal)
                                          (not (string-equal (layer-name l) (layer-name target)))
                                          (not (string-equal (layer-name l) "0"))))
                         (drawing-layers *dwg*)))
        (when (member (sysvar "CLAYER") from :test #'string-equal) (setf (sysvar "CLAYER") (layer-name target)))
        (touch)
        (out (format nil "~D object~:P moved; layers ~{~A~^, ~} merged into \"~A\".~%" n from (layer-name target)))))))

(defcmd "LAYDEL" ("-LAYDEL") "Express: deletes a layer and every object on it."
  (let* ((r (get-user-input :entity :prompt "Select object on layer to delete or [Name]: " :keywords '("Name")))
         (l (cond ((equal r "Name") (find-layer (gstring "Enter layer name: " t)))
                  ((consp r) (let ((e (sel-entity r))) (and e (find-layer (ent-layer e))))))))
    (cond ((null l) nil)
          ((string-equal (layer-name l) "0") (out (format nil "Layer 0 cannot be deleted.~%")))
          ((equal "Yes" (gkw (format nil "Delete layer \"~A\" and all its objects? [Yes/No] <No>: " (layer-name l)) '("Yes" "No") "No"))
           (let ((n 0))
             (dolist (e (all-live-entities)) (when (string-equal (ent-layer e) (layer-name l)) (delete-entity e) (incf n)))
             (when (string-equal (sysvar "CLAYER") (layer-name l)) (setf (sysvar "CLAYER") "0"))
             (setf (drawing-layers *dwg*) (remove l (drawing-layers *dwg*)))
             (touch)
             (out (format nil "Deleted layer \"~A\" and ~D object~:P.~%" (layer-name l) n)))))))

;;; ---------- OVERKILL ----------

(defun round-key (x) (round (* x 1d6)))
(defun entity-signature (data)
  "A comparable signature of an entity's geometry and properties, or NIL."
  (let ((type (dxf 0 data))
        (props (list (dxf 8 data) (dxf 62 data) (dxf 6 data))))
    (flet ((p (code) (let ((q (pt3 (dxf code data)))) (list (round-key (px q)) (round-key (py q))))))
      (cond ((equal type "LINE")
             (let ((a (p 10)) (b (p 11))) (list* type props (if (or (< (first a) (first b)) (and (= (first a) (first b)) (<= (second a) (second b)))) (list a b) (list b a)))))
            ((equal type "CIRCLE") (list* type props (p 10) (round-key (dxf 40 data)) nil))
            ((equal type "ARC") (list* type props (p 10) (mapcar #'round-key (list (dxf 40 data) (dxf 50 data) (dxf 51 data)))))
            ((equal type "POINT") (list* type props (p 10) nil))
            ((member type '("TEXT" "MTEXT") :test #'equal) (list* type props (p 10) (dxf 1 data) (round-key (or (dxf 40 data) 0)) nil))
            ((equal type "INSERT") (list* type props (dxf 2 data) (p 10) (mapcar (lambda (c) (round-key (or (dxf c data) 0))) '(41 42 50))))
            ((equal type "LWPOLYLINE") (list* type props (mapcar (lambda (v) (list (round-key (px (first v))) (round-key (py (first v))) (round-key (second v))))
                                                               (lwpoly-vertices data))))
            (t nil)))))

(defcmd "OVERKILL" ("-OVERKILL") "Removes duplicate and overlapping objects."
  (let* ((ents (get-selection)) (seen (make-hash-table :test #'equal)) (dups 0) (merged 0))
    ;; exact duplicates
    (dolist (e ents)
      (let ((sig (entity-signature (ent-data e))))
        (when sig
          (if (gethash sig seen) (progn (delete-entity e) (incf dups)) (setf (gethash sig seen) t)))))
    ;; collinear overlapping lines on the same layer get merged
    (let ((groups (make-hash-table :test #'equal)))
      (dolist (e (remove-if-not #'ent-live-p ents))
        (when (equal (ent-type e) "LINE")
          (let* ((d0 (ent-data e)) (a (pt3 (dxf 10 d0))) (b (pt3 (dxf 11 d0))))
            (when (> (dist2d a b) 1d-9)
              (let* ((th (mod (ang a b) +pi+)) (th (if (> th (- +pi+ 1d-9)) 0d0 th))
                     (off (- (* (px a) (sin th)) (* (py a) (cos th))))
                     (key (list (dxf 8 d0) (dxf 62 d0) (round (* th 1d6)) (round (* off 1d5)))))
                (push e (gethash key groups)))))))
      (maphash (lambda (key es)
                 (when (> (length es) 1)
                   (let* ((th (/ (third key) 1d6)) (u (pt (cos th) (sin th)))
                          (iv (sort (mapcar (lambda (e) (let* ((d0 (ent-data e))
                                                                (s1 (+ (* (px (dxf 10 d0)) (px u)) (* (py (dxf 10 d0)) (py u))))
                                                                (s2 (+ (* (px (dxf 11 d0)) (px u)) (* (py (dxf 11 d0)) (py u)))))
                                                           (list (min s1 s2) (max s1 s2) e)))
                                            es)
                                    #'< :key #'first))
                          (cur (first iv)))
                     (dolist (x (rest iv))
                       (if (<= (first x) (+ (second cur) 1d-9))
                           (let* ((keep (third cur)) (d0 (ent-data keep))
                                  (pts (list (pt3 (dxf 10 d0)) (pt3 (dxf 11 d0)) (pt3 (dxf 10 (ent-data (third x)))) (pt3 (dxf 11 (ent-data (third x))))))
                                  (proj (lambda (p) (+ (* (px p) (px u)) (* (py p) (py u)))))
                                  (lo (reduce (lambda (a b) (if (< (funcall proj a) (funcall proj b)) a b)) pts))
                                  (hi (reduce (lambda (a b) (if (> (funcall proj a) (funcall proj b)) a b)) pts)))
                             (set-entity-data keep (set-dxf 11 hi (set-dxf 10 lo d0)))
                             (delete-entity (third x)) (incf merged)
                             (setf cur (list (first cur) (max (second cur) (second x)) keep)))
                           (setf cur x))))))
               groups))
    (out (format nil "~D duplicate(s) deleted~%~D overlapping object(s) or segment(s) deleted~%" dups merged))))

;;; ---------- BREAKLINE / EXTRIM ----------

(defcmd "BREAKLINE" ("BREAKL") "Express: draws a break-line symbol."
  (let* ((size (or (sysvar "BRKLSIZE") 1d0)))
    (out (format nil "Block= BRKLINE.DWG, Size= ~A, Extension= 0~%" (fmt-len size)))
    (let ((p1 (gpoint "Specify first point for breakline or [Size]: " :keywords '("Size"))))
      (when (equal p1 "Size")
        (let ((s (gdist (format nil "Breakline symbol size <~A>: " (fmt-len size)) :flags 6)))
          (when s (setf size s (sysvar "BRKLSIZE") (d s))))
        (setf p1 (gpoint "Specify first point for breakline: ")))
      (when (pointp p1)
        (let ((p2 (gpoint "Specify second point for breakline: " :base p1)))
          (when (pointp p2)
            (let* ((mid (or (let ((m (gpoint "Specify location for break symbol <Midpoint>: " :base p1))) (and (pointp m) (foot-on-line m p1 p2)))
                            (midpt p1 p2)))
                   (a (ang p1 p2)) (w size)
                   (sa (polar mid (+ a +pi+) (/ w 2))) (sd (polar mid a (/ w 2)))
                   (sb (polar (polar mid (+ a +pi+) (/ w 6)) (+ a (/ +pi+ 2)) (* 0.6 w)))
                   (sc (polar (polar mid a (/ w 6)) (- a (/ +pi+ 2)) (* 0.6 w))))
              (add (make-lwpoly-data (list p1 sa sb sc sd p2) nil)))))))))

(defun side-test-fn (e pick)
  "A function (point -> boolean) true on the same side of cutting entity E as PICK."
  (let ((lp (closed-entity-loop e)))
    (if lp
        (let* ((poly (loop-polygon-directed lp 48)) (inside (point-in-polygon-p pick poly)))
          (lambda (p) (eq inside (point-in-polygon-p p poly))))
        (let ((pcs (snap-pieces e)))
          (flet ((side (p) (let* ((pc (reduce (lambda (a b) (if (<= (piece-dist a p) (piece-dist b p)) a b)) pcs)))
                             (case (first pc)
                               (:seg (> (cross2 (second pc) (third pc) p) 0))
                               (t (< (dist2d (second pc) p) (third pc)))))))
            (let ((s0 (side pick))) (lambda (p) (eq s0 (side p)))))))))

(defun trim-by-side (e cuts on-side)
  "Remove the parts of E (line, arc or polyline) for which ON-SIDE is true between cut points."
  (let* ((data (ent-data e)) (type (dxf 0 data)))
    (cond
      ((equal type "LINE")
       (let* ((a (pt3 (dxf 10 data))) (b (pt3 (dxf 11 data)))
              (ts (append (list 0d0) (line-int-params a b cuts) (list 1d0))))
         (when (> (length ts) 2)
           (replace-with e (loop for (t1 t2) on ts while t2
                                 unless (funcall on-side (at-param a b (/ (+ t1 t2) 2)))
                                   collect (line-data (at-param a b t1) (at-param a b t2)))))))
      ((equal type "ARC")
       (let* ((c (pt3 (dxf 10 data))) (r (dxf 40 data)) (a1 (dxf 50 data)) (a2 (dxf 51 data))
              (ss (append (list 0d0) (arc-int-params c r a1 a2 cuts) (list (arc-sweep a1 a2)))))
         (when (> (length ss) 2)
           (replace-with e (loop for (s1 s2) on ss while s2
                                 unless (funcall on-side (polar c (+ a1 (/ (+ s1 s2) 2)) r))
                                   collect (arc-data c r (+ a1 s1) (+ a1 s2)))))))
      ((equal type "LWPOLYLINE")
       (let* ((segs (pline-segs data)) (n (length segs))
              (us (pline-params-of-cuts segs cuts)))
         (when us
           (let* ((bounds (if (lwpoly-closed-p data) (append us (list (+ (first us) n))) (append (list 0d0) us (list (float n 1d0)))))
                  (pieces (loop for (u1 u2) on bounds while u2
                                for mid = (let ((um (/ (+ u1 u2) 2))) (seg-point-at (aref segs (mod (floor um) n)) (- um (floor um))))
                                unless (funcall on-side mid) collect (pline-from-verts (pline-sub segs u1 u2) data))))
             (replace-with e pieces)))))
      (t nil))))

(defcmd "EXTRIM" () "Express: trims every object crossing a cutting edge on one side."
  (let ((cut (sel-entity (get-user-input :entity :prompt "Pick a POLYLINE, LINE, CIRCLE, ARC, ELLIPSE, IMAGE or TEXT for cutting edge: "))))
    (when cut
      (let ((side (gpoint "Specify the side to trim on: ")))
        (when (pointp side)
          (let ((on-side (side-test-fn cut side)) (cuts (snap-pieces cut)) (n 0))
            (dolist (e (live-entities))
              (unless (or (eq e cut) (not (entity-editable-p e)) (not (entity-visible-p e)))
                (when (some (lambda (pc) (some (lambda (c) (piece-ints pc c)) cuts)) (snap-pieces e))
                  (trim-by-side e cuts on-side) (incf n))))
            (out (format nil "~D object~:P trimmed.~%" n))))))))

;;; ---------- text tools ----------

(defun text-ents (prompt)
  (remove-if-not (lambda (e) (member (ent-type e) '("TEXT" "MTEXT" "ATTDEF") :test #'equal)) (get-selection prompt)))

(defun title-case (s)
  (let ((out (copy-seq (string-downcase s))) (start t))
    (dotimes (i (length out) out)
      (if (alpha-char-p (char out i))
          (progn (when start (setf (char out i) (char-upcase (char out i)))) (setf start nil))
          (setf start t)))))

(defcmd "TCASE" ("CHANGECASE") "Changes the case of text, MText and dimension text: Sentence, lower, UPPER, Title, tOGGLE."
  (let ((ents (funcall 'text-case-ents (get-selection "Select text-based objects: "))))
    (if (null ents)
        (out (format nil "No text selected.~%"))
        (let* ((k (gkw "Select case [Sentence/lower/UPPER/Title/toGGLE] <UPPER>: " '("Sentence" "lower" "UPPER" "Title" "toGGLE") "UPPER"))
               (n (funcall 'change-text-case ents (funcall 'case-mode k))))
          (out (format nil "~D text object~:P changed.~%" n))
          (when (gui-p) (gui-regen))))))

(defcmd "TCOUNT" () "Express: numbers selected text objects."
  (let ((ents (text-ents "Select objects: ")))
    (when ents
      (let* ((k (gkw "Sort selected objects by [X/Y/Select-order] <Y>: " '("X" "Y" "Select") "Y"))
             (sorted (cond ((equal k "X") (sort (copy-list ents) #'< :key (lambda (e) (px (dxf 10 (ent-data e))))))
                           ((equal k "Y") (sort (copy-list ents) #'> :key (lambda (e) (py (dxf 10 (ent-data e))))))
                           (t ents)))
             (si (gstring "Specify starting number and increment (Start,increment) <1,1>: "))
             (parts (mapcar #'parse-number-text (split-on (if (string= si "") "1,1" si) #\,)))
             (start (round (or (first parts) 1))) (inc (round (or (second parts) 1)))
             (mode (gkw "Placement of numbers in text [Overwrite/Prefix/Suffix/Find&replace..] <Prefix>: "
                        '("Overwrite" "Prefix" "Suffix" "Find") "Prefix"))
             (find (and (equal mode "Find") (gstring "Enter search string: " t))))
        (loop for e in sorted for n from start by inc
              do (let* ((s (or (dxf 1 (ent-data e)) "")) (num (princ-to-string n))
                        (ns (cond ((equal mode "Overwrite") num)
                                  ((equal mode "Suffix") (concatenate 'string s " " num))
                                  ((equal mode "Find") (let ((p (search find s)))
                                                         (if p (concatenate 'string (subseq s 0 p) num (subseq s (+ p (length find)))) s)))
                                  (t (concatenate 'string num " " s)))))
                   (set-entity-data e (set-dxf 1 ns (ent-data e)))))))))

(defcmd "TXT2MTXT" ("T2M") "Express: combines single-line text into one MTEXT object."
  (let ((ents (remove-if-not (lambda (e) (equal (ent-type e) "TEXT")) (text-ents "Select text objects: "))))
    (when ents
      (let* ((sorted (sort (copy-list ents) #'> :key (lambda (e) (py (dxf 10 (ent-data e))))))
             (top (ent-data (first sorted)))
             (h (or (dxf 40 top) 0.2d0)))
        (add (append (list (cons 0 "MTEXT") (cons 10 (polar (pt3 (dxf 10 top)) (/ +pi+ 2) h)) (cons 40 h) (cons 71 1)
                           (cons 1 (format nil "~{~A~^\\P~}" (mapcar (lambda (e) (dxf 1 (ent-data e))) sorted)))
                           (cons 50 (or (dxf 50 top) 0d0)))
                     (common-props top)))
        (dolist (e ents) (delete-entity e))
        (out (format nil "~D text object~:P combined.~%" (length ents)))))))

;;; ---------- MOCORO / COPYM / FLATTEN ----------

(defcmd "MOCORO" () "Express: moves, copies, rotates and scales with one base point."
  (let ((ents (get-selection)))
    (when ents
      (let ((base (gpoint "Base point: ")) (copy nil))
        (when (pointp base)
          (loop
            (let ((r (gpoint (format nil "Second point or [Copy/Rotate/Scale/Base/Undo]<eXit>: ") :base base
                             :keywords '("Copy" "Rotate" "Scale" "Base" "Undo" "eXit")
                             :drag (preview-xf ents (lambda (cur) (xf-move (v- cur base)))))))
              (cond ((or (null r) (equal r "eXit")) (return))
                    ((equal r "Copy") (setf copy (not copy)))
                    ((equal r "Base") (let ((b (gpoint "Base point: "))) (when (pointp b) (setf base b))))
                    ((equal r "Undo") (undo-one))
                    ((equal r "Rotate")
                     (let ((a (gangle "Rotation angle: " :base base :drag (preview-xf ents (lambda (cur) (xf-rotate base (ang base cur)))))))
                       (when a (transform-entities ents (xf-rotate base a) :copy copy))))
                    ((equal r "Scale")
                     (let ((k (gdist "Scale factor: " :base base :flags 6)))
                       (when k (transform-entities ents (xf-scaler base k) :copy copy))))
                    ((pointp r) (transform-entities ents (xf-move (v- r base)) :copy copy)
                     (unless copy (setf base r)))))))))))

(defcmd "COPYM" () "Express: copies objects repeatedly, by repeat, divide or measure."
  (let ((ents (get-selection)))
    (when ents
      (let ((base (gpoint "Base point: ")) (last nil))
        (when (pointp base)
          (loop
            (let ((r (gpoint "Second point or [Repeat (last)/Divide/Measure/Undo] <exit>: " :base base
                             :keywords '("Repeat" "Divide" "Measure" "Undo")
                             :drag (preview-xf ents (lambda (cur) (xf-move (v- cur base)))))))
              (cond ((null r) (return))
                    ((equal r "Undo") (undo-one))
                    ((equal r "Repeat")
                     (if last (progn (transform-entities ents (xf-move last) :copy t)
                                     (setf ents (last-n-entities (length ents))))
                         (out (format nil "Nothing to repeat.~%"))))
                    ((equal r "Divide")
                     (let* ((n (or (gint "Number of copies: " :flags 6) 1)) (q (gpoint "Specify end point: " :base base)))
                       (when (pointp q)
                         (loop for i from 1 to n do (transform-entities ents (xf-move (v* (v- q base) (/ i n))) :copy t)))))
                    ((equal r "Measure")
                     (let* ((step (gdist "Distance between copies: " :flags 6)) (q (gpoint "Specify end point: " :base base)))
                       (when (and step (pointp q))
                         (let* ((len (dist2d base q)) (a (ang base q)))
                           (loop for s from step to (+ len 1d-9) by step
                                 do (transform-entities ents (xf-move (v- (polar base a s) base)) :copy t))))))
                    ((pointp r) (setf last (v- r base)) (transform-entities ents (xf-move last) :copy t))))))))))

(defun last-n-entities (n)
  (let ((v (drawing-entities *dwg*)))
    (coerce (subseq v (max 0 (- (length v) n))) 'list)))

(defcmd "FLATTEN" () "Express: sets the Z coordinate of selected objects to zero."
  (let ((ents (get-selection)))
    (dolist (e ents)
      (set-entity-data e (map-points (ent-data e) '(10 11 12 13 14 15 16) (lambda (p) (pt (px p) (py p) 0)))))
    (out (format nil "~D object~:P flattened.~%" (length ents)))))

;;; ---------- selection tools ----------

(defun select-matching (proto &key (by '(:type :layer)))
  (remove-if-not (lambda (e)
                   (and (entity-visible-p e)
                        (or (not (member :type by)) (equal (ent-type e) (dxf 0 proto)))
                        (or (not (member :layer by)) (string-equal (ent-layer e) (dxf 8 proto)))
                        (or (not (member :color by)) (eql (dxf 62 (ent-data e)) (dxf 62 proto)))))
                 (live-entities)))

(defun set-pickfirst (ents)
  (ui-highlight *pickfirst* nil)
  (setf *pickfirst* ents)
  (ui-highlight ents t)
  (when (gui-p) (refresh-grips) (refresh-palette))
  (out (format nil "~D object~:P selected.~%" (length ents))))

(defcmd "SELECTSIMILAR" ("SSIM") "Selects all objects of the same type and layer as the selection."
  (let ((ents (get-selection "Select objects: ")))
    (when ents
      (set-pickfirst (remove-duplicates (loop for e in ents append (select-matching (ent-data e))))))))

(defcmd "SSX" () "Express: selects objects matching a picked object's type, layer and color."
  (let ((e (sel-entity (get-user-input :entity :prompt "Select object <None>: "))))
    (when e (set-pickfirst (select-matching (ent-data e) :by '(:type :layer :color))))))

(defcmd "GETSEL" () "Express: selects objects by layer and type."
  (let* ((l (sel-entity (get-user-input :entity :prompt "Select an object on the source layer <*>: ")))
         (ty (sel-entity (get-user-input :entity :prompt "Select an object of the type you want <*>: "))))
    (set-pickfirst (remove-if-not (lambda (e) (and (entity-visible-p e)
                                                   (or (null l) (string-equal (ent-layer e) (ent-layer l)))
                                                   (or (null ty) (equal (ent-type e) (ent-type ty)))))
                                  (live-entities)))))

(defcmd "XLIST" () "Express: lists the properties of an object nested in a block."
  (let* ((sel (get-user-input :entity :prompt "Select nested xref or block object to list: "))
         (e (sel-entity sel)))
    (when e
      (let* ((data (ent-data e))
             (sub (if (equal (dxf 0 data) "INSERT")
                      (let* ((b (find-block (dxf 2 data))) (xf (and b (insert-xf data (block-def-base b)))) (best nil) (bd nil))
                        (dolist (s (and b (block-def-entities b)) best)
                          (dolist (pc (data-pieces s))
                            (dolist (tp (transform-piece pc xf))
                              (let ((dd (piece-dist tp (sel-point sel))))
                                (when (or (null bd) (< dd bd)) (setf bd dd best s)))))))
                      data)))
        (when sub
          (out (format nil "~%Object: ~A~%Block name: ~A~%Layer: ~A~%Color: ~A~%Linetype: ~A~%"
                       (dxf 0 sub) (if (equal (dxf 0 data) "INSERT") (dxf 2 data) "N/A") (dxf 8 sub)
                       (color-label (or (dxf 62 sub) 256)) (ltype-label (dxf 6 sub)))))))))

(defcmd "BURST" () "Express: explodes blocks, turning attribute definitions into text."
  (let ((ents (remove-if-not (lambda (e) (equal (ent-type e) "INSERT")) (get-selection))) (n 0))
    (dolist (e ents)
      (let ((parts (explode-data (ent-data e))))
        (when parts
          (delete-entity e) (incf n)
          (dolist (p parts)
            (add-entity (rem-dxf 5 (if (equal (dxf 0 p) "ATTDEF")
                                       (cons (cons 0 "TEXT") (remove-if (lambda (x) (and (consp x) (member (car x) '(0 2 3 70 74)))) p))
                                       p)))))))
    (out (format nil "~D block~:P burst.~%" n))))

;;; ---------- acet-* AutoLISP functions ----------

(defvar *acet-saved-vars* nil)

(defun acet-save-and-set (pairs)
  (loop for (name val) on pairs by #'cddr
        when (stringp name)
          do (push (cons name (getvar name)) *acet-saved-vars*)
             (ignore-errors (setvar name val))))

(defsubr "acet-error-init" (lst)
  (setf *acet-saved-vars* nil)
  (when (consp lst) (acet-save-and-set (first lst)))
  nil)
(defsubr "acet-error-restore" ()
  (loop for (n . v) in *acet-saved-vars* do (ignore-errors (setvar n v)))
  (setf *acet-saved-vars* nil)
  *null-sym*)
(defsubr "acet-sysvar-set" (lst) (acet-save-and-set lst) nil)
(defsubr "acet-sysvar-restore" ()
  (loop for (n . v) in *acet-saved-vars* do (ignore-errors (setvar n v)))
  (setf *acet-saved-vars* nil) nil)
(defsubr ("acet-ui-progress-init" "acet-ui-progress") (&optional msg max) (declare (ignore msg)) (or max t))
(defsubr "acet-ui-progress-safe" (n) n)
(defsubr "acet-ui-progress-done" () nil)
(defsubr ("acet-layerp-mark" "acet-sys-beep" "acet-file-backup" "acet-ui-status" "acet-spinner") (&rest a) (declare (ignore a)) nil)
(defsubr "acet-ui-message" (msg &optional title flags) (declare (ignore title flags))
  (funcall-a (sym-value (sym "ALERT")) msg) 1)
(defsubr "acet-ui-getcorner" (p &optional msg) (lisp-get :corner (list p msg) :base-allowed t))
(defsubr "acet-str-replace" (find repl s)
  (let ((s (astr s)) (find (astr find)) (repl (astr repl)))
    (if (zerop (length find)) s
        (with-output-to-string (o)
          (loop with start = 0 for pos = (search find s :start2 start :test #'char-equal)
                do (write-string s o :start start :end (or pos (length s)))
                   (if pos (progn (write-string repl o) (setf start (+ pos (length find)))) (return)))))))
(defsubr "acet-str-find" (sub s &optional ignore-case regexp)
  (declare (ignore regexp))
  (let ((p (search (astr sub) (astr s) :test (if ignore-case #'char-equal #'char=)))) (and p (1+ p))))
(defsubr "acet-str-lr-trim" (chars s) (string-trim (astr chars) (astr s)))
(defsubr "acet-str-to-list" (delim s) (split-on (astr s) (char (astr delim) 0)))
(defsubr "acet-str-format" (fmt &rest args)
  (let ((s (astr fmt)))
    (loop for a in args for i from 1
          do (let ((tok (format nil "%~D" i)))
               (loop for p = (search tok s) while p
                     do (setf s (concatenate 'string (subseq s 0 p) (lisp-repr a nil) (subseq s (+ p (length tok))))))))
    s))
(defsubr "acet-list-remove-duplicates" (lst &optional (fuzz 0))
  (let ((out nil)) (dolist (x (alst lst) (nreverse out)) (unless (member x out :test (lambda (a b) (aequal a b (or fuzz 0)))) (push x out)))))
(defsubr "acet-list-m-assoc" (key lst) (remove-if-not (lambda (p) (and (consp p) (aequal (car p) key))) (alst lst)))
(defsubr "acet-geom-midpoint" (a b) (midpt (pt3 (apt a)) (pt3 (apt b))))
(defsubr "acet-geom-vertex-list" (e)
  (let ((data (ent-data (aename (if (consp e) (car e) e)))))
    (cond ((equal (dxf 0 data) "LWPOLYLINE") (mapcar (lambda (v) (pt3 (first v))) (lwpoly-vertices data)))
          ((equal (dxf 0 data) "LINE") (list (pt3 (dxf 10 data)) (pt3 (dxf 11 data))))
          (t nil))))
(defsubr "acet-ent-geomextents" (e)
  (let ((b (entity-bbox (aename e)))) (and b (list (pt (first b) (second b)) (pt (third b) (fourth b))))))
(defsubr "acet-geom-textbox" (elist &optional (offset 0))
  (let* ((data (remove-if (lambda (p) (and (consp p) (eql (car p) -1))) (alst elist))) (c (text-box data)) (o (d offset)))
    (if (zerop o) c
        (let ((m (midpt (first c) (third c)))) (mapcar (lambda (p) (polar p (ang m p) o)) c)))))
(defsubr "acet-layer-locked" (name) (let ((l (find-layer (astr name)))) (bool (and l (layer-locked l)))))
(defsubr "acet-ss-to-list" (ss) (and ss (copy-list (pickset-items (apick ss)))))
(defsubr "acet-ss-union" (lst)
  (let ((items (loop for ss in (alst lst) when (pickset-p ss) append (pickset-items ss)))) (and items (make-pickset items))))
(defsubr "acet-ss-ssget-filter" (ss filter)
  (let ((items (remove-if-not (lambda (e) (dxf-filter-match-p (ent-data e) filter)) (pickset-items (apick ss)))))
    (and items (make-pickset items))))
(defsubr "acet-ss-entdel" (ss) (dolist (e (pickset-items (apick ss))) (delete-entity e)) nil)
(defsubr "acet-ss-drag-move" (ss base &optional msg)
  (let ((ents (pickset-items (apick ss))) (b (pt3 (apt base))))
    (get-user-input :point :prompt (or msg "Specify second point: ") :base b
                            :drag (preview-xf ents (lambda (cur) (xf-move (v- cur b)))))))
(defsubr "acet-pline-make" (lst)
  (let ((last nil))
    (dolist (pts (alst lst) last)
      (setf last (add-entity (make-lwpoly-data (mapcar #'pt3 pts) nil))))))
(defsubr "acet-filename-ext-remove" (f)
  (let* ((s (astr f)) (dot (position #\. s :from-end t)) (slash (position #\/ s :from-end t)))
    (if (and dot (or (null slash) (> dot slash))) (subseq s 0 dot) s)))
(defsubr "acet-file-find" (f) (find-file-in-support (astr f)))

(defsubr "fancypencil-alias" (a) (values (gethash (astr a) *aliases*)))
