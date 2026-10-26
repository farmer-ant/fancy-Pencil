;;;; blockedit.lisp — editing existing blocks (BEDIT, the Block Editor) and RENAME.
;;;;
;;;; BEDIT opens the block's contents in a tab of its own ("Block Editor: NAME"), with the block's
;;;; base point at 0,0. Draw and modify there as usual; BSAVE writes the changes back into the
;;;; drawing (every copy of the block updates), BCLOSE (or closing the tab) finishes.

(in-package :fancy-pencil)

(defun block-editor-p (&optional (d (current-doc))) (and d (doc-block-edit d)))
(defun block-editor-name () (second (block-editor-p)))

(defun user-block-names ()
  (sort (loop for b in (drawing-blocks *dwg*)
              unless (anonymous-block-name-p (block-def-name b)) collect (block-def-name b))
        #'string-lessp))

(defun choose-block-to-edit ()
  "A block name: from a selected block reference, else asked for."
  (let* ((pre (remove-if-not (lambda (e) (equal (ent-type e) "INSERT")) (remove-if-not #'ent-live-p *pickfirst*))))
    (cond
      ((= (length pre) 1) (take-pickfirst) (dxf 2 (ent-data (first pre))))
      ((null (user-block-names)) (out (format nil "This drawing has no blocks to edit.~%")) nil)
      ((and (gui-p) (not (from-lisp-p)))
       (let ((r (gui-form "Edit Block Definition"
                          (list (list "NAME" "Block to edit:" (first (user-block-names)) :combo (user-block-names)))
                          "Edit")))
         (and r (cdr (assoc "NAME" r :test #'string=)))))
      (t (let ((s (gstring "Enter block name to edit or [?]: " t)))
           (cond ((string= s "?") (dolist (n (user-block-names)) (out (format nil "  ~A~%" n))) nil)
                 ((string= s "") nil)
                 (t s)))))))

(defun open-block-editor (name)
  (let* ((parent (current-doc))
         (b (find-block name)))
    (cond
      ((null b) (out (format nil "Block \"~A\" not found.~%" name)) nil)
      ;; already being edited: go there
      ((find-if (lambda (d) (and (doc-block-edit d) (eq (first (doc-block-edit d)) parent)
                                 (string-equal (second (doc-block-edit d)) name)))
                *docs*)
       (switch-to-doc (find-if (lambda (d) (and (doc-block-edit d) (string-equal (second (doc-block-edit d)) name))) *docs*)))
      (t
       (let* ((layers (mapcar #'copy-layer (drawing-layers *dwg*)))
              (ltypes (copy-list (drawing-ltypes *dwg*)))
              (styles (mapcar #'copy-structure (all-textstyles)))
              (dimstyles (mapcar #'copy-structure (all-dimstyles)))
              (blocks (copy-list (drawing-blocks *dwg*)))
              (carry (loop for k in '("LUNITS" "LUPREC" "AUNITS" "AUPREC" "INSUNITS" "LTSCALE" "TEXTSIZE" "DIMSTYLE"
                                      "TEXTSTYLE" "PDMODE" "PDSIZE")
                           collect (cons k (sysvar k))))
              (base (pt3 (block-def-base b)))
              (datas (mapcar (lambda (dd) (xform-data (copy-tree dd) (xf-move (v- (pt 0 0) base))))
                             (block-def-entities b)))
              (d (new-doc)))
         (setf (doc-block-edit d) (list parent (block-def-name b))
               (doc-untitled d) (format nil "Block Editor: ~A" (block-def-name b)))
         (setf (drawing-layers *dwg*) layers (drawing-ltypes *dwg*) ltypes (drawing-styles *dwg*) styles
               (drawing-dimstyles *dwg*) dimstyles (drawing-blocks *dwg*) blocks)
         (loop for (k . v) in carry when v do (setf (sysvar k) v))
         (let ((*keep-dimension-blocks* t))
           (dolist (dd datas) (add-entity (rem-dxf 5 dd))))
         (setf (drawing-saved-modcount *dwg*) (drawing-modcount *dwg*) (drawing-undo *dwg*) nil)
         (clear-geom-cache)
         (when (gui-p)
           (let ((ext (drawing-extents)))
             (if ext
                 (let ((m (* 0.15 (max 1d-6 (- (third ext) (first ext)) (- (fourth ext) (second ext))))))
                   (zoom-to-box (min 0 (- (first ext) m)) (min 0 (- (second ext) m))
                                (max 0 (+ (third ext) m)) (max 0 (+ (fourth ext) m))))
                 (zoom-to-box -5 -5 5 5)))
           (gui-regen) (refresh-tabs t))
         (out (format nil "Editing block ~A (~D object~:P). Its base point is 0,0 (the red cross).~%~
                           BSAVE saves the changes; BCLOSE or closing the tab finishes.~%"
                      (block-def-name b) (length datas)))
         d)))))

(defun block-editor-save ()
  "Write the Block Editor's objects back into the block in the drawing it came from."
  (let* ((d (current-doc)) (be (doc-block-edit d)))
    (destructuring-bind (parent name) be
      (unless (member parent *docs*)
        (tell-user (format nil "The drawing this block came from has been closed, so block ~A can't be saved back." name))
        (return-from block-editor-save nil))
      (let* ((ents (live-entities))
             (self (remove-if-not (lambda (e) (and (equal (ent-type e) "INSERT") (string-equal (dxf 2 (ent-data e)) name))) ents))
             (ents (set-difference ents self))
             (clip (collect-clip-definitions (mapcar #'ent-data ents)))
             (datas (mapcar (lambda (e) (copy-tree (rem-dxf 5 (ent-data e)))) ents)))
        (when self
          (out (format nil "~D cop~:@P of ~A inside itself left out (a block can't contain itself).~%" (length self) name)))
        ;; the block itself is redefined below; don't let its old copy overwrite anything
        (setf (clip-blocks clip) (remove name (clip-blocks clip) :key #'block-def-name :test #'string-equal))
        (save-doc-state d)
        (load-doc-state parent)
        (unwind-protect
             (progn
               (push (take-snapshot) (drawing-undo *dwg*))       ; one U in the drawing undoes the edit
               (setf (drawing-redo *dwg*) nil)
               (let ((renames (merge-clip-definitions clip)))
                 (setf datas (mapcar (lambda (dd)
                                       (let ((r (and (stringp (dxf 2 dd)) (equal (dxf 0 dd) "DIMENSION")
                                                     (assoc (dxf 2 dd) renames :test #'string-equal))))
                                         (if r (set-dxf 2 (cdr r) dd) dd)))
                                     datas)))
               (setf (drawing-blocks *dwg*)
                     (cons (make-block-def :name name :base (pt 0 0 0) :entities datas)
                           (remove name (drawing-blocks *dwg*) :key #'block-def-name :test #'string-equal)))
               (clear-geom-cache)
               (touch))
          (save-doc-state parent)
          (load-doc-state d))
        (setf (drawing-saved-modcount *dwg*) (drawing-modcount *dwg*))
        (when (gui-p) (refresh-tabs t))
        (out (format nil "Block ~A saved (~D object~:P). Every copy of it in ~A now shows the change.~%"
                     name (length datas) (doc-name parent)))
        t))))

(defun block-editor-close (&optional (ask t))
  "Leave the Block Editor (asking whether to save changes). Returns NIL if cancelled."
  (let* ((d (current-doc)) (be (doc-block-edit d)))
    (when be
      (let ((choice (if (and ask (drawing-modified-p))
                        (cond
                          ((from-lisp-p) "yes")                       ; scripts: keep the changes
                          ((gui-p)
                           (gui-confirm (format nil "Save the changes to block ~A?" (second be)) "yesnocancel"))
                          (t (string-downcase (gkw (format nil "Save changes to block ~A? [Yes/No/Cancel] <Yes>: " (second be))
                                                  '("Yes" "No" "Cancel") "Yes"))))
                        "no")))
        (cond ((string= choice "cancel") nil)
              (t (when (string= choice "yes") (block-editor-save))
                 (remove-doc d (first be))
                 (out (format nil "Block Editor closed.~%"))
                 t))))))

(defcmd "BEDIT" ("BE" "BLOCKEDIT" "REFEDIT") "Opens a block in the Block Editor to change it."
  (let ((name (choose-block-to-edit)))
    (when name (open-block-editor name))))

(defcmd "BSAVE" () "Saves the Block Editor's changes into the block."
  (if (block-editor-p) (block-editor-save) (out (format nil "BSAVE works in the Block Editor (BEDIT).~%"))))

(defcmd "BCLOSE" () "Closes the Block Editor."
  (if (block-editor-p) (block-editor-close) (out (format nil "BCLOSE works in the Block Editor (BEDIT).~%"))))

(defun block-editor-banner-cmds ()
  "The base point marker and a note across the top while editing a block."
  (when (block-editor-p)
    (multiple-value-bind (x y) (w->s (pt 0 0))
      (let ((x (round x)) (y (round y)))
        (list (format nil ".c create line ~D ~D ~D ~D -fill #ff5050 -width 2 -tags ucs" (- x 9) y (+ x 9) y)
              (format nil ".c create line ~D ~D ~D ~D -fill #ff5050 -width 2 -tags ucs" x (- y 9) x (+ y 9))
              (format nil ".c create oval ~D ~D ~D ~D -outline #ff5050 -width 2 -tags ucs" (- x 5) (- y 5) (+ x 5) (+ y 5))
              (format nil ".c create rectangle 0 0 ~D 24 -fill #4a3d14 -outline #8a7424 -tags ucs" *cw*)
              (format nil ".c create text 10 12 -anchor w -fill #ffe7a0 -font {Helvetica 10 bold} -tags ucs -text ~A"
                      (tq (format nil "BLOCK EDITOR: ~A     base point = red cross (0,0)     BSAVE saves  -  BCLOSE finishes"
                                  (block-editor-name)))))))))

;;; ---------- RENAME ----------

(defun rename-block (old new)
  (let ((b (find-block old)))
    (setf (block-def-name b) new)
    (dolist (e (all-live-entities))
      (when (and (equal (ent-type e) "INSERT") (string-equal (dxf 2 (ent-data e)) old))
        (setf (ename-data e) (set-dxf 2 new (ename-data e)))))
    (dolist (bd (drawing-blocks *dwg*))
      (setf (block-def-entities bd)
            (mapcar (lambda (dd) (if (and (equal (dxf 0 dd) "INSERT") (string-equal (dxf 2 dd) old)) (set-dxf 2 new dd) dd))
                    (block-def-entities bd))))))

(defun rename-object (kind old new)
  "Rename a block, layer, text style or dimension style. Returns an error message or NIL."
  (cond
    ((or (string= old "") (string= new "")) "Enter both names.")
    ((string-equal kind "Block")
     (cond ((not (find-block old)) (format nil "No block named ~A." old))
           ((and (find-block new) (not (string-equal old new))) (format nil "A block named ~A already exists." new))
           ((not (valid-block-name-p new)) (format nil "~A can't be used as a block name." new))
           (t (rename-block old new) nil)))
    ((string-equal kind "Layer")
     (let ((l (find-layer old)))
       (cond ((null l) (format nil "No layer named ~A." old))
             ((string= old "0") "Layer 0 can't be renamed.")
             ((string-equal old "Defpoints") "The Defpoints layer can't be renamed.")
             ((and (find-layer new) (not (string-equal old new))) (format nil "A layer named ~A already exists." new))
             ((or (> (length new) 255) (find-if (lambda (c) (find c "<>/\\\":;?*|,=`")) new))
              (format nil "A layer name can't contain any of  < > / \\ \" : ; ? * | , = `"))
             (t (setf (layer-name l) new)
                (dolist (e (all-live-entities))
                  (when (string-equal (ent-layer e) old) (setf (ename-data e) (set-dxf 8 new (ename-data e)))))
                ;; objects inside blocks too
                (dolist (b (drawing-blocks *dwg*))
                  (setf (block-def-entities b)
                        (mapcar (lambda (sd) (if (string-equal (or (dxf 8 sd) "") old) (set-dxf 8 new sd) sd))
                                (block-def-entities b))))
                (when (string-equal (sysvar "CLAYER") old) (setf (sysvar "CLAYER") new))
                nil))))
    ((string-equal kind "Text style")
     (let ((s (find-textstyle old)))
       (cond ((null s) (format nil "No text style named ~A." old))
             ((string-equal old "Standard") "The Standard style can't be renamed.")
             (t (setf (textstyle-name s) new)
                (dolist (e (all-live-entities))
                  (when (and (member (ent-type e) '("TEXT" "MTEXT" "ATTDEF" "ATTRIB") :test #'equal)
                             (string-equal (or (dxf 7 (ent-data e)) "") old))
                    (setf (ename-data e) (set-dxf 7 new (ename-data e)))))
                (when (string-equal (sysvar "TEXTSTYLE") old) (setf (sysvar "TEXTSTYLE") new))
                nil))))
    ((string-equal kind "Dimension style")
     (let ((s (find-dimstyle old)))
       (cond ((null s) (format nil "No dimension style named ~A." old))
             (t (setf (dimstyle-name s) new)
                (dolist (e (all-live-entities))
                  (when (and (member (ent-type e) '("DIMENSION" "LEADER") :test #'equal)
                             (string-equal (or (dxf 3 (ent-data e)) "") old))
                    (setf (ename-data e) (set-dxf 3 new (ename-data e)))))
                (when (string-equal (sysvar "DIMSTYLE") old) (setf (sysvar "DIMSTYLE") new))
                nil))))
    (t "Unknown kind of name.")))

(defcmd "RENAME" ("REN" "-RENAME" "DDRENAME") "Renames a block, layer, text style or dimension style."
  (let ((kinds '("Block" "Layer" "Text style" "Dimension style")))
    (multiple-value-bind (kind old new)
        (if (and (gui-p) (not (from-lisp-p)))
            (let* ((pre (find-if (lambda (e) (equal (ent-type e) "INSERT")) (remove-if-not #'ent-live-p *pickfirst*)))
                   (r (gui-form "Rename"
                                (list (list "KIND" "Rename a:" "Block" :combo kinds)
                                      (list "OLD" "Old name:" (if pre (dxf 2 (ent-data pre)) (or (first (user-block-names)) "")) :combo
                                            (append (user-block-names) (mapcar #'layer-name (drawing-layers *dwg*))))
                                      (list "NEW" "New name:" "" :entry))
                                "Rename")))
              (if r (values (cdr (assoc "KIND" r :test #'string=)) (string-trim " " (cdr (assoc "OLD" r :test #'string=)))
                            (string-trim " " (cdr (assoc "NEW" r :test #'string=))))
                  (return-from-cmd)))
            (let ((k (gkw "Enter object type to rename [Block/Layer/Style/Dimstyle]: " '("Block" "Layer" "Style" "Dimstyle"))))
              (unless k (return-from-cmd))
              (values (cond ((equal k "Style") "Text style") ((equal k "Dimstyle") "Dimension style") (t k))
                      (string-trim " " (gstring "Enter old name: " t))
                      (string-trim " " (gstring "Rename to: " t)))))
      (let ((err (rename-object kind old new)))
        (if err (if (gui-p) (gui-confirm err "ok") (out (format nil "~A~%" err)))
            (progn (touch) (clear-geom-cache) (when (gui-p) (gui-regen) (gui-sync))
                   (out (format nil "~A ~A renamed to ~A.~%" kind old new))))))))
