;;;; uicmds.lisp — ALIGN, the right-click menus on the drawing, the Object Snap settings panel
;;;; and the Dimension Style Manager.

(in-package :fancy-pencil)

;;; ---------- ALIGN ----------

(defvar *align-scale* "No" "Last answer to ALIGN's scale question (offered as the default next time).")

(defun align-xf (s1 d1 s2 d2 scale)
  "Move S1 to D1, turn S1->S2 onto D1->D2 and, when SCALE, stretch it to the same length."
  (let ((xf (xf-move (v- d1 s1))))
    (when (and s2 d2 (> (dist2d s1 s2) 1d-12) (> (dist2d d1 d2) 1d-12))
      (setf xf (xf-compose xf (xf-rotate d1 (- (ang d1 d2) (ang s1 s2)))))
      (when scale (setf xf (xf-compose xf (xf-scaler d1 (/ (dist2d d1 d2) (dist2d s1 s2)))))))
    xf))

(defcmd "ALIGN" ("AL") "Moves, rotates and optionally scales objects so two points line up with two others."
  (let ((ents (get-selection)))
    (when ents
      (let* ((s1 (gpoint "Specify first source point: "))
             (d1 (and (pointp s1) (gpoint "Specify first destination point: " :base s1))))
        (when (pointp d1)
          ;; like AutoCAD, nothing moves until the last question is answered: only rubber-band lines
          ;; from each source point to the cursor
          (let* ((pair1 (lambda (c) (declare (ignore c)) (list (list :data (line-data s1 d1)))))
                 (s2 (gpoint "Specify second source point or <continue>: " :drag pair1))
                 (d2 (and (pointp s2) (gpoint "Specify second destination point: " :base s2 :drag pair1))))
            (if (not (and (pointp s2) (pointp d2)))
                ;; one pair: a plain move
                (transform-entities ents (xf-move (v- d1 s1)))
                (progn
                  ;; a third pair only matters in 3D; accept and ignore it like AutoCAD does for flat objects
                  (let ((s3 (gpoint "Specify third source point or <continue>: ")))
                    (when (pointp s3) (gpoint "Specify third destination point: " :base s3)))
                  (let* ((ls (dist2d s1 s2)) (ld (dist2d d1 d2))
                         (ans (gkw (format nil "Scale objects based on alignment points? [Yes/No] <~A>: " *align-scale*)
                                   '("Yes" "No") *align-scale*))
                         (scale (string-equal ans "Yes")))
                    (setf *align-scale* (if scale "Yes" "No"))
                    (transform-entities ents (align-xf s1 d1 s2 d2 scale))
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
                (when last (list (list (format nil "Repeat ~A" (string-upcase last)) last)))
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
               (when (= (length sel) 1) (list "Make Object's Layer Current" "LAYMCUR"))
               :sep
               (list "Cut" "CUTCLIP") (list "Copy to Clipboard" "COPYCLIP")
               (list "Copy with Base Point" "COPYBASE")
               :sep
               (list "Create Block from Selection..." "BLOCK")
               (when ins (list "Edit Block..." "BEDIT"))
               (when ins (list "Rename Block..." "RENAME"))
               :sep
               (list "Deselect All" "DESELECT") (list "Properties" "PROPERTIES"))
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
              (dolist (e (live-entities))
                (when (and (equal (ent-type e) "DIMENSION") (string-equal (or (dxf 3 (ent-data e)) "Standard") name))
                  (set-entity-data e (set-dxf :dimvars vars (ent-data e)))
                  (incf updated))))))
        ;; deleted styles, unless a dimension still uses them
        (dolist (ds (copy-list (all-dimstyles)))
          (unless (member (dimstyle-name ds) kept :test #'string-equal)
            (if (some (lambda (e) (and (equal (ent-type e) "DIMENSION")
                                       (string-equal (or (dxf 3 (ent-data e)) "") (dimstyle-name ds))))
                      (live-entities))
                (out (format nil "Dimension style ~A is in use and was not deleted.~%" (dimstyle-name ds)))
                (setf (drawing-dimstyles *dwg*) (remove ds (drawing-dimstyles *dwg*))))))
        (let ((ds (find-dimstyle current)))
          (when ds (apply-dimvars (dimstyle-vars ds)) (setf (sysvar "DIMSTYLE") (dimstyle-name ds))))
        (touch)
        (gui-regen)
        (out (format nil "Current dimension style: ~A~@[ (~D dimension~:P updated)~]~%"
                     (sysvar "DIMSTYLE") (and (plusp updated) updated)))))))
