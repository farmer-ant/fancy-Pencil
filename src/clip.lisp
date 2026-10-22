;;;; clip.lisp — copy and paste between drawings (AutoCAD's COPYCLIP, CUTCLIP, COPYBASE,
;;;; PASTECLIP, PASTEORIG and PASTEBLOCK).
;;;;
;;;; The clipboard carries the objects together with everything they need in another drawing:
;;;; their layers, linetypes, text styles, dimension styles and block definitions (nested blocks
;;;; too). Pasting adds only the definitions the target drawing doesn't have; one it already has
;;;; with the same name is used as it is, as in AutoCAD.
;;;;
;;;; The clipboard is also written to ~/.cache/fancypencil/clipboard.lisp, so it works between
;;;; two separate Fancy Pencil windows as well as between tabs.

(in-package :fancy-pencil)

(defstruct clip base datas layers ltypes styles dimstyles blocks)

(defun clipboard-path ()
  (merge-pathnames ".cache/fancypencil/clipboard.lisp"
                   (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/")))

(defun anonymous-block-name-p (name)
  (and (stringp name) (plusp (length name)) (char= (char name 0) #\*)))

(defun collect-clip-definitions (datas)
  "Layers, linetypes, styles, dimension styles and blocks used by DATAS (and by the blocks they use)."
  (let ((layers nil) (ltypes nil) (styles nil) (dimstyles nil) (blocks nil))
    (labels ((note (lst name) (if (and (stringp name) (plusp (length name)) (not (member name lst :test #'string-equal)))
                                  (cons name lst) lst))
             (walk (data)
               (setf layers (note layers (dxf 8 data))
                     ltypes (note ltypes (dxf 6 data)))
               (let ((type (dxf 0 data)))
                 (when (member type '("TEXT" "MTEXT" "ATTRIB" "ATTDEF") :test #'equal)
                   (setf styles (note styles (dxf 7 data))))
                 (when (member type '("DIMENSION" "LEADER") :test #'equal)
                   (setf dimstyles (note dimstyles (dxf 3 data))))
                 (when (member type '("INSERT" "DIMENSION") :test #'equal)
                   (let ((bn (dxf 2 data)))
                     (when (and (stringp bn) (not (member bn blocks :test #'string-equal)))
                       (let ((b (find-block bn)))
                         (when b
                           (push bn blocks)
                           (mapc #'walk (block-def-entities b))))))))))
      (mapc #'walk datas)
      (dolist (l layers) (let ((lr (find-layer l))) (when lr (setf ltypes (note ltypes (layer-ltype lr)))))))
    (make-clip :layers (remove nil (mapcar #'find-layer layers))
               :ltypes (remove nil (mapcar (lambda (n) (unless (continuous-name-p n) (find-ltype n))) ltypes))
               :styles (remove nil (mapcar #'find-textstyle styles))
               :dimstyles (remove nil (mapcar #'find-dimstyle dimstyles))
               :blocks (remove nil (mapcar #'find-block blocks)))))

(defun write-clipboard-file (clip)
  (ignore-errors
   (let ((p (clipboard-path)))
     (ensure-directories-exist p)
     (with-open-file (o p :direction :output :if-exists :supersede :external-format :utf-8)
       (with-standard-io-syntax
         (let ((*package* (find-package :fancy-pencil)) (*print-readably* t) (*print-circle* nil))
           (prin1 (list :fancy-pencil-clipboard 1 (copy-structure clip)) o))))
     t)))

(defun read-clipboard-file ()
  (ignore-errors
   (with-open-file (i (clipboard-path) :external-format :utf-8)
     (with-standard-io-syntax
       (let* ((*package* (find-package :fancy-pencil)) (*read-eval* nil)
              (x (read i nil nil)))
         (and (consp x) (eq (first x) :fancy-pencil-clipboard) (clip-p (third x)) (third x)))))))

(defun set-clipboard (ents base)
  (let ((clip (collect-clip-definitions (mapcar #'ent-data ents))))
    (setf (clip-base clip) base
          (clip-datas clip) (mapcar (lambda (e) (copy-tree (rem-dxf 5 (ent-data e)))) ents)
          *clipboard* clip)
    (write-clipboard-file clip)
    clip))

(defun current-clipboard ()
  "The newest clipboard: the shared file (another window may have copied since), else this window's."
  (let ((f (read-clipboard-file)))
    (or f (and (clip-p *clipboard*) *clipboard*))))

;;; ---------- bringing definitions into this drawing ----------

(defun merge-clip-definitions (clip)
  "Add the definitions the target lacks. Returns an alist renaming anonymous (dimension) blocks."
  (dolist (lt (clip-ltypes clip))
    (unless (find-ltype (ltype-name lt)) (add-ltype (copy-structure lt))))
  (dolist (l (clip-layers clip))
    (unless (find-layer (layer-name l))
      (setf (drawing-layers *dwg*) (append (drawing-layers *dwg*) (list (copy-structure l))))))
  (dolist (s (clip-styles clip))
    (unless (find-textstyle (textstyle-name s))
      (setf (drawing-styles *dwg*) (append (all-textstyles) (list (copy-structure s))))))
  (dolist (ds (clip-dimstyles clip))
    (unless (find-dimstyle (dimstyle-name ds))
      (setf (drawing-dimstyles *dwg*) (append (all-dimstyles) (list (make-dimstyle :name (dimstyle-name ds)
                                                                                     :vars (copy-list (dimstyle-vars ds))))))))
  ;; blocks: named ones only if missing; anonymous (*D dimension pictures) always, under a fresh name
  (let ((renames nil))
    (dolist (b (clip-blocks clip))
      (when (anonymous-block-name-p (block-def-name b))
        (push (cons (block-def-name b) (unique-dim-block-name)) renames)))
    (flet ((fix (data)
             (let ((r (and (stringp (dxf 2 data)) (assoc (dxf 2 data) renames :test #'string-equal))))
               (if r (set-dxf 2 (cdr r) data) data))))
      (dolist (b (clip-blocks clip))
        (let* ((r (assoc (block-def-name b) renames :test #'string-equal))
               (name (if r (cdr r) (block-def-name b))))
          (unless (find-block name)
            (push (make-block-def :name name :base (copy-tree (block-def-base b))
                                  :entities (mapcar (lambda (dd) (fix (copy-tree dd))) (block-def-entities b)))
                  (drawing-blocks *dwg*)))))
      (touch)
      renames)))

(defun paste-datas (clip xf)
  "Add the clipboard's objects to this drawing through transform XF. Returns the new entities."
  (let ((renames (merge-clip-definitions clip))
        (made nil))
    (let ((*keep-dimension-blocks* t))
      (dolist (dd (clip-datas clip))
        (let* ((dd (copy-tree dd))
               (r (and (equal (dxf 0 dd) "DIMENSION") (stringp (dxf 2 dd))
                       (assoc (dxf 2 dd) renames :test #'string-equal)))
               (dd (if r (set-dxf 2 (cdr r) dd) dd)))
          (push (add-entity (rem-dxf 5 (xform-data dd xf))) made))))
    (nreverse made)))

(defun clip-drag (clip base)
  (lambda (cur) (mapcar (lambda (dd) (list :data (xform-data dd (xf-move (v- cur base))))) (clip-datas clip))))

(defun after-paste (made)
  (out (format nil "~D object~:P pasted.~%" (length made)))
  (when (gui-p) (gui-regen) (refresh-palette)))

;;; ---------- commands ----------

(defun selection-base (ents)
  (let ((b (drawing-extents ents))) (pt (first b) (second b))))

(defcmd "COPYCLIP" () "Copies selected objects to the clipboard (Ctrl+C); paste them into any drawing."
  (let ((ents (get-selection)))
    (when ents
      (set-clipboard ents (selection-base ents))
      (out (format nil "~D object~:P copied to the clipboard.~%" (length ents))))))

(defcmd "COPYBASE" () "Copies objects to the clipboard with a base point you pick (Ctrl+Shift+C)."
  (let ((base (gpoint "Specify base point: ")))
    (when (pointp base)
      (let ((ents (get-selection)))
        (when ents
          (set-clipboard ents base)
          (out (format nil "~D object~:P copied to the clipboard.~%" (length ents))))))))

(defcmd "CUTCLIP" () "Copies the selection to the clipboard and erases it (Ctrl+X)."
  (let ((ents (get-selection)))
    (when ents
      (set-clipboard ents (selection-base ents))
      (dolist (e ents) (delete-entity e))
      (out (format nil "~D object~:P cut to the clipboard.~%" (length ents)))
      (when (gui-p) (gui-regen)))))

(defcmd "PASTECLIP" () "Pastes the clipboard at a point you pick (Ctrl+V)."
  (let ((clip (current-clipboard)))
    (if (null clip) (out (format nil "Clipboard is empty.~%"))
        (let* ((base (clip-base clip))
               (p (gpoint "Specify insertion point: " :drag (clip-drag clip base))))
          (when (pointp p)
            (after-paste (paste-datas clip (xf-move (v- p base)))))))))

(defcmd "PASTEORIG" () "Pastes the clipboard at the same coordinates it was copied from."
  (let ((clip (current-clipboard)))
    (if (null clip) (out (format nil "Clipboard is empty.~%"))
        (after-paste (paste-datas clip (xf-move (pt 0 0)))))))

(defcmd "PASTEBLOCK" () "Pastes the clipboard as one block (Ctrl+Shift+V)."
  (let ((clip (current-clipboard)))
    (if (null clip) (out (format nil "Clipboard is empty.~%"))
        (let* ((base (clip-base clip))
               (p (gpoint "Specify insertion point: " :drag (clip-drag clip base))))
          (when (pointp p)
            (let ((name (loop for n = (format nil "A$C~8,'0X" (random #xFFFFFFFF)) unless (find-block n) return n)))
              ;; the objects become a block (with their definitions), inserted at P
              (let ((made (paste-datas clip (xf-move (pt 0 0)))))
                (push (make-block-def :name name :base base :entities (mapcar (lambda (e) (rem-dxf 5 (copy-tree (ent-data e)))) made))
                      (drawing-blocks *dwg*))
                (dolist (e made) (delete-entity e)))
              (add-entity (list (cons 0 "INSERT") (cons 2 name) (cons 10 p) (cons 41 1d0) (cons 42 1d0) (cons 43 1d0) (cons 50 0d0)))
              (out (format nil "Pasted as block ~A.~%" name))
              (when (gui-p) (gui-regen) (refresh-palette))))))))
