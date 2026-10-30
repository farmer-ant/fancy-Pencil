;;;; uicmds.lisp — ALIGN, the right-click menus on the drawing, the Object Snap settings panel
;;;; and the Dimension Style Manager.

(in-package :fancy-pencil)

;;; ---------- ALIGN ----------

(defvar *align-scale* "No" "Last answer to ALIGN's scale question (offered as the default next time).")

;;; ALIGN works in 3D: one pair of points moves; two pairs also turn the objects so the first
;;; source edge lies along the first destination edge (the smallest turn that does it, so objects
;;; in the working plane stay in it); three pairs also bring the plane through the three source
;;; points onto the plane through the three destination points - the way to fit a panel against
;;; another one at any angle. Scaling (from the first edge) is optional, as in AutoCAD.

(defun mat-apply (m p) (pt (vdot3 (first m) p) (vdot3 (second m) p) (vdot3 (third m) p)))
(defun mat-from-frames (e1 e2 e3 f1 f2 f3)
  "Rows of the rotation taking the frame E onto the frame F."
  (flet ((row (i) (let ((sel (lambda (v) (case i (0 (px v)) (1 (py v)) (t (pz v))))))
                    (v+ (v+ (v* e1 (funcall sel f1)) (v* e2 (funcall sel f2))) (v* e3 (funcall sel f3))))))
    (list (row 0) (row 1) (row 2))))

(defun any-perpendicular (u)
  (unit3 (if (< (abs (pz u)) 0.9d0) (cross3 u (pt 0 0 1)) (cross3 u (pt 1 0 0)))))

(defun rotation-taking (u v)
  "The smallest rotation (rows) taking unit vector U onto unit vector V."
  (let* ((c (vdot3 u v)) (ax (cross3 u v)) (s (sqrt (vdot3 ax ax))))
    (cond ((> c (- 1 1d-12)) (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1)))
          ((< s 1d-12)                      ; opposite: half a turn about any perpendicular axis
           (let ((k (any-perpendicular u)))
             (flet ((r (i) (let ((ki (case i (0 (px k)) (1 (py k)) (t (pz k)))))
                             (v- (v* k (* 2 ki)) (case i (0 (pt 1 0 0)) (1 (pt 0 1 0)) (t (pt 0 0 1)))))))
               (list (r 0) (r 1) (r 2)))))
          (t (let* ((k (v* ax (/ 1 s))) (e2 (unit3 (cross3 k u))) (f2 (unit3 (cross3 k v))))
               (mat-from-frames u e2 k v f2 k))))))

(defun align-xf (s1 d1 s2 d2 scale &optional s3 d3)
  "Move S1 to D1; turn S1->S2 onto D1->D2 (and the plane S1 S2 S3 onto D1 D2 D3); when SCALE,
stretch so S1->S2 is as long as D1->D2. Points in 3D."
  (let* ((s1 (pt3 s1)) (d1 (pt3 d1))
         (m (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1))) (k 1d0))
    (when (and s2 d2 (> (dist (pt3 s1) (pt3 s2)) 1d-12) (> (dist d1 (pt3 d2)) 1d-12))
      (let* ((e1 (unit3 (v- (pt3 s2) s1))) (f1 (unit3 (v- (pt3 d2) d1))))
        (setf m (rotation-taking e1 f1))
        (when (and s3 d3)
          (let* ((a (v- (pt3 s3) s1)) (b (v- (pt3 d3) d1))
                 (a2 (v- a (v* e1 (vdot3 a e1)))) (b2 (v- b (v* f1 (vdot3 b f1)))))
            (when (and (> (vdot3 a2 a2) 1d-18) (> (vdot3 b2 b2) 1d-18))
              (let* ((e2 (unit3 a2)) (f2 (unit3 b2)))
                (setf m (mat-from-frames e1 e2 (cross3 e1 e2) f1 f2 (cross3 f1 f2)))))))
        (when scale (setf k (/ (dist d1 (pt3 d2)) (dist s1 (pt3 s2)))))))
    (let ((m m) (k k))
      (make-xf :fn (lambda (p) (v+ d1 (v* (mat-apply m (v- (pt3 p) s1)) k)))
               :scale (d k)))))

(defun align-flat-p (xf)
  "True when XF keeps the X,Y plane (a turn about Z only): objects can be handled the 2D way."
  (let* ((fn (xf-fn xf)) (o (funcall fn (pt 0 0 0))) (z (v- (funcall fn (pt 0 0 1)) o)))
    (and (< (abs (px z)) 1d-9) (< (abs (py z)) 1d-9) (plusp (pz z)))))

(defun transform-entities-3d (ents xf)
  (dolist (e ents)
    (set-entity-data e (if (align-flat-p xf)
                           (xform-data (ent-data e) (align-2d-xf xf))
                           (xform-any (ent-data e) xf)))))

(defun align-2d-xf (xf)
  "The same transform, with the angle and mirror information the 2D object code needs."
  (let* ((fn (xf-fn xf)) (o (funcall fn (pt 0 0 0))) (x (v- (funcall fn (pt 1 0 0)) o))
         (a (atan (py x) (px x))))
    (make-xf :fn fn :ang (lambda (v) (norm-ang (+ v a))) :scale (xf-scale xf))))

(defcmd "ALIGN" ("AL" "3DALIGN") "Moves, rotates and optionally scales objects so up to three points line up with three others, in 3D."
  (let ((ents (get-selection)))
    (when ents
      (let* ((s1 (gpoint "Specify first source point: "))
             (d1 (and (pointp s1) (gpoint "Specify first destination point: " :base s1))))
        (when (pointp d1)
          ;; like AutoCAD, nothing moves until the last question is answered: only rubber-band lines
          ;; from each source point to its destination
          (let* ((pairs (list (list s1 d1)))
                 (drag (lambda (c) (declare (ignore c))
                         (mapcar (lambda (pr) (list :data (line-data (first pr) (second pr)))) pairs)))
                 (s2 (gpoint "Specify second source point or <continue>: " :drag drag))
                 (d2 (and (pointp s2) (gpoint "Specify second destination point: " :base s2 :drag drag))))
            (if (not (and (pointp s2) (pointp d2)))
                ;; one pair: a plain move
                (transform-entities ents (xf-move (v- (pt3 d1) (pt3 s1))))
                (progn
                  (setf pairs (append pairs (list (list s2 d2))))
                  (let* ((s3 (gpoint "Specify third source point or <continue>: " :drag drag))
                         (d3 (and (pointp s3) (gpoint "Specify third destination point: " :base s3 :drag drag)))
                         (ls (dist (pt3 s1) (pt3 s2))) (ld (dist (pt3 d1) (pt3 d2)))
                         (ans (if (pointp d3)
                                  "No"            ; three pairs: AutoCAD does not offer scaling
                                  (gkw (format nil "Scale objects based on alignment points? [Yes/No] <~A>: " *align-scale*)
                                       '("Yes" "No") *align-scale*)))
                         (scale (string-equal ans "Yes")))
                    (unless (pointp d3) (setf *align-scale* (if scale "Yes" "No")))
                    (transform-entities-3d ents (align-xf s1 d1 s2 d2 scale (and (pointp d3) s3) (and (pointp d3) d3)))
                    (when (and scale (> ls 1d-12))
                      (out (format nil "Scaled by ~A.~%" (fmt-num (/ ld ls))))))))))))))

;;; ---------- right-click on the drawing ----------

(defvar *recent-commands* nil "Most recent first; shown under Recent Input.")

(defun note-recent-command (word)
  (let* ((w (string-upcase word))
         (l (cons w (remove w *recent-commands* :test #'string-equal))))
    (setf *recent-commands* (subseq l 0 (min 10 (length l))))))

(defun tcl-menu-items (items)
  "ITEMS: (label command) | :sep | (:sub label items) -> a Tcl list for ctxmenu."
  (format nil "{~{~A~^ ~}}"
          (mapcar (lambda (it)
                    (cond ((eq it :sep) "-")
                          ((eq (first it) :sub) (format nil "{> ~A ~A}" (tq (second it)) (tcl-menu-items (third it))))
                          (t (format nil "{~A ~A}" (tq (first it)) (tq (second it))))))
                  items)))

(defun command-context-items ()
  (let* ((sel (remove-if-not #'ent-live-p *pickfirst*))
         (ins (find-if (lambda (e) (equal (ent-type e) "INSERT")) sel))
         (last *last-command*)
         (head (append
                (when last (list (list (format nil "Repeat ~A" (string-upcase (or (ignore-errors (canonical-command-name last)) last))) last)))
                (when *recent-commands*
                  (list (list :sub "Recent Input" (mapcar (lambda (c) (list c c)) *recent-commands*))))
                (when (block-editor-p)
                  (list (list "Save Block (BSAVE)" "BSAVE") (list "Close Block Editor (BCLOSE)" "BCLOSE")))
                (when (or last *recent-commands* (block-editor-p)) (list :sep)))))
    (append
     head
     (remove nil
     (if sel
         (list (list "Erase" "ERASE") (list "Move" "MOVE") (list "Copy Selection" "COPY")
               (list "Scale" "SCALE") (list "Rotate" "ROTATE") (list "Mirror" "MIRROR")
               (list "Align" "ALIGN") (list "Array" "ARRAY")
               (list "Match Properties" "MATCHPROP")
               (list "Match Layer" "LAYMCH")
               (list "Change Layer..." "CHANGELAYER")
               (when (= (length sel) 1) (list "Make Object's Layer Current" "LAYMCUR"))
               :sep
               (list "Cut" "CUTCLIP") (list "Copy to Clipboard" "COPYCLIP")
               (list "Copy with Base Point" "COPYBASE")
               :sep
               (list :sub "Draw Order" (list (list "Bring to Front" "DRAWORDER Front") (list "Send to Back" "DRAWORDER Back")
                                             (list "Bring Above Objects" "DRAWORDER Above") (list "Send Under Objects" "DRAWORDER Under")))
               (when (some (lambda (e) (member (ent-type e) '("TEXT" "MTEXT" "ATTDEF" "DIMENSION") :test #'equal)) sel)
                 (list :sub "Change Text Case" (list (list "UPPER CASE" "TCASE UPPER") (list "lower case" "TCASE lower")
                                                     (list "Sentence case" "TCASE Sentence") (list "Title Case" "TCASE Title")
                                                     (list "tOGGLE cASE" "TCASE toGGLE"))))
               :sep
               (list "Create Block from Selection..." "BLOCK")
               (when ins (list "Edit Block..." "BEDIT"))
               (when ins (list "Rename Block..." "RENAME"))
               :sep
               (list "Deselect All" "DESELECT") (list "Quick Properties" "QUICKPROPERTIES") (list "Properties" "PROPERTIES"))
         (list (list "Undo" "U") (list "Redo" "REDO")
               :sep
               (list "Zoom Extents" "ZOOM E") (list "Zoom Window" "ZOOM W")
               :sep
               (list "Paste" "PASTECLIP") (list "Paste as Block" "PASTEBLOCK")
               :sep
               (list "Create Block..." "BLOCK") (list "Edit Block..." "BEDIT")
               :sep
               (list "Select All" "SELECTALL"))))
     (list :sep (list "Object Snap Settings..." "'OSNAP")))))

(defun show-command-context-menu (rx ry)
  (tk "ctxmenu ~D ~D ~A" rx ry (tcl-menu-items (command-context-items))))

(defcmd "DESELECT" () "Clears the current selection (grips)."
  (take-pickfirst)
  (when (gui-p) (sweep-highlights) (refresh-grips) (refresh-palette)))

;;; ---------- Object Snap settings ----------

(defun osnap-dialog ()
  (let ((r (tk-ask (format nil "osnapdlg ~D" (or (sysvar "OSMODE") 0)))))
    (unless (or (string= r "CANCEL") (string= r ""))
      (let ((m (ignore-errors (parse-integer r))))
        (when m
          ;; keep bits the panel doesn't show (e.g. 4096 extension)
          (setf (sysvar "OSMODE") (logior m (logand (sysvar "OSMODE") 4096)))
          (save-prefs)                    ; object snaps are kept for every drawing and next time
          (gui-sync))))))

;;; ---------- Dimension Style Manager ----------

(defparameter *dimstyle-dialog-keys*
  '(:dimclrd :dimdli :dimclre :dimexe :dimexo :dimblk :dimasz :dimtsz :dimcen
    :dimtxt :dimclrt :dimtad :dimgap :dimtih :dimscale :dimdec :dimlfac :dimpost :dimadec))

(defun dimvar-display (k v)
  (cond ((eq k :dimblk) (arrow-code v))
        ((integerp v) (princ-to-string v))
        ((realp v) (fmt-num v))
        (t (princ-to-string (or v "")))))

(defun dimvar-parse (k text)
  "Convert TEXT from the dialog to the value type of dimension variable K (NIL if invalid)."
  (let ((std (getf (standard-dimvars) k)))
    (cond ((eq k :dimblk) (arrow-code text))
          ((stringp std) text)
          ((integerp std) (ignore-errors (round (parse-number-text (string-trim " " text)))))
          (t (let ((n (parse-number-text (string-trim " " text)))) (and n (d n)))))))

(defun dimstyle-dialog ()
  ;; make sure the current style's record holds the live settings
  ;; the current style shows the live settings (changes made with SETVAR or DIMBLK count as part of it)
  (let ((name (sysvar "DIMSTYLE")))
    (setf (drawing-dimstyles *dwg*)
          (append (remove name (all-dimstyles) :key #'dimstyle-name :test #'string-equal)
                  (list (make-dimstyle :name name :vars (current-dimvars))))))
  (let* ((recs (format nil "{~{~A~^ ~}}"
                       (mapcar (lambda (ds)
                                 (format nil "{~A {~{~A~^ ~}}}" (tq (dimstyle-name ds))
                                         (loop for k in *dimstyle-dialog-keys*
                                               append (list (symbol-name k) (tq (dimvar-display k (dv (dimstyle-vars ds) k)))))))
                               (all-dimstyles))))
         (arrows (format nil "{~{{~A ~A}~^ ~}}" (loop for (code label) in *arrowheads* append (list (tq code) (tq label)))))
         (r (tk-ask (format nil "dimstyledlg ~A ~A ~A" recs (tq (sysvar "DIMSTYLE")) arrows))))
    (unless (or (string= r "CANCEL") (string= r ""))
      (let* ((parts (split-string r (code-char 30)))
             (current (first parts))
             (kept nil) (updated 0))
        (dolist (rec (rest parts))
          (let* ((f (split-string rec (code-char 31)))
                 (name (first f))
                 (old (find-dimstyle name))
                 (vars (copy-list (if old (dimstyle-vars old) (standard-dimvars)))))
            (loop for (ks v) on (rest f) by #'cddr
                  for k = (intern (string-upcase ks) :keyword)
                  for val = (and v (dimvar-parse k v))
                  when (and (member k *dimvar-names*) val) do (setf (getf vars k) val))
            (push name kept)
            (setf (drawing-dimstyles *dwg*)
                  (append (remove name (all-dimstyles) :key #'dimstyle-name :test #'string-equal)
                          (list (make-dimstyle :name name :vars vars))))
            ;; dimensions drawn with a changed style follow it, as in AutoCAD
            (when (and old (not (equal (dimstyle-vars old) vars)))
              (dolist (e (all-live-entities))
                (when (and (equal (ent-type e) "DIMENSION") (string-equal (or (dxf 3 (ent-data e)) "Standard") name))
                  (set-entity-data e (set-dxf :dimvars vars (ent-data e)))
                  (incf updated))))))
        ;; deleted styles, unless a dimension still uses them
        (dolist (ds (copy-list (all-dimstyles)))
          (unless (member (dimstyle-name ds) kept :test #'string-equal)
            (if (some (lambda (e) (and (equal (ent-type e) "DIMENSION")
                                       (string-equal (or (dxf 3 (ent-data e)) "") (dimstyle-name ds))))
                      (all-live-entities))
                (out (format nil "Dimension style ~A is in use and was not deleted.~%" (dimstyle-name ds)))
                (setf (drawing-dimstyles *dwg*) (remove ds (drawing-dimstyles *dwg*))))))
        (let ((ds (find-dimstyle current)))
          (when ds (apply-dimvars (dimstyle-vars ds)) (setf (sysvar "DIMSTYLE") (dimstyle-name ds))))
        (touch)
        (gui-regen)
        (out (format nil "Current dimension style: ~A~@[ (~D dimension~:P updated)~]~%"
                     (sysvar "DIMSTYLE") (and (plusp updated) updated)))))))
