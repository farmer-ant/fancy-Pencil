;;;; textcmds.lisp — TEXT, MTEXT, TEXTEDIT with the in-place editor, the STYLE command,
;;;; and width grips for MTEXT boxes.

(in-package :fancy-pencil)

(defun use-inplace-editor-p () (and (gui-p) (not (from-lisp-p))))

(defun current-color-aci ()
  (let ((c (sysvar "CECOLOR")))
    (cond ((or (null c) (string-equal c "BYLAYER") (string-equal c "BYBLOCK"))
           (let ((l (find-layer (sysvar "CLAYER")))) (if l (abs (layer-color l)) 7)))
          (t (or (ignore-errors (parse-integer c)) 7)))))

(defun entity-color-aci (e)
  (let ((c (dxf 62 (ent-data e))))
    (if (and c (/= c 256) (/= c 0)) (abs c)
        (let ((l (find-layer (ent-layer e)))) (if l (abs (layer-color l)) 7)))))

;;; ---------- the in-place editor ----------

(defun styles-tcl-list ()
  (format nil "~{{~A}~^ ~}"
          (mapcar (lambda (st)
                    (format nil "~A ~A ~D ~D ~F" (tq (textstyle-name st)) (tq (textstyle-font st))
                            (if (textstyle-bold st) 1 0) (if (textstyle-italic st) 1 0) (d (textstyle-height st))))
                  (all-textstyles))))

(defun inplace-edit (mode p width-world h &key (init "") (justify "left") aci (min-height 0)
                                             style font bold italic underline)
  "Open the in-place editor on the drawing at world point P (MTEXT: top-left corner of the box;
:LINE: start of the baseline). Returns a plist (:text :style :font :height :bold :italic
:underline :justify); :text is MTEXT-encoded in :mtext mode. Esc cancels the command."
  ;; clear the rubber band / crosshair left by the previous prompt
  (tk-send-string ".c delete tmp")
  ;; spelling: the window underlines unknown words using this word list
  (ignore-errors
   (ensure-spell-dictionary)
   (tk "set ::spell_file ~A; set ::spell_custom ~A" (tq (namestring (spell-cache-path))) (tq (namestring (custom-dict-path)))))
  (let* ((st (or (find-textstyle style) (current-textstyle)))
         (given font)
         (font (or font (textstyle-font st)))
         (bold (if given bold (textstyle-bold st)))
         (italic (if given italic (textstyle-italic st)))
         (k (/ *vscale* 0.72d0))
         (fpx (round (* h k)))
         (color (aci->hex (or aci (current-color-aci))))
         (opts (format nil "~A ~A ~D ~D ~D ~F ~F {~A}" (tq (textstyle-name st)) (tq font)
                       (if bold 1 0) (if italic 1 0) (if underline 1 0) (d h) k (styles-tcl-list))))
    (multiple-value-bind (x y) (w->s p)
      (let ((r (tk-ask (format nil "mtedit ~A ~D ~D ~D ~D ~A ~A ~A ~D ~A"
                               (if (eq mode :mtext) "mtext" "line") (round x) (round y)
                               (if (and width-world (plusp width-world)) (max 20 (round (* width-world *vscale*))) 0)
                               fpx (tq color) (tq init) justify
                               (min 2000 (round (* (max 0 min-height) *vscale*)))
                               (tq opts)))))
        (unless (and (>= (length r) 2) (string= (subseq r 0 2) "OK"))
          (out (format nil "*Cancel*~%"))
          (error 'alisp-cancel))
        (let ((f (split-string (if (> (length r) 3) (subseq r 3) "") (code-char 31))))
          (flet ((fld (i) (or (nth i f) "")))
            (list :style (fld 0) :font (fld 1)
                  :height (let ((v (parse-number-text (fld 2)))) (if (and v (plusp v)) (d v) (d h)))
                  :bold (string= (fld 3) "1") :italic (string= (fld 4) "1") :underline (string= (fld 5) "1")
                  :justify (fld 6)
                  :text (format nil "~{~A~^~C~}" (loop for x in (nthcdr 7 f) for i from 0
                                                       append (if (zerop i) (list x) (list (code-char 31) x)))))))))))

(defun editor-result-content (r)
  "MTEXT content for an editor result R, with whole-object formatting only where it differs from the style."
  (let* ((st (or (find-textstyle (getf r :style)) (current-textstyle)))
         (font (getf r :font))
         (override (or (not (string-equal font (textstyle-font st)))
                       (not (eq (getf r :bold) (textstyle-bold st)))
                       (not (eq (getf r :italic) (textstyle-italic st))))))
    (mtext-make-content (getf r :text)
                        :font (and override font) :bold (and override (getf r :bold))
                        :italic (and override (getf r :italic)) :underline (getf r :underline))))

(defun justify-column (j) (cond ((string-equal j "center") 1) ((string-equal j "right") 2) (t 0)))
(defun column-justify (attach) (case (mod (1- (or attach 1)) 3) (0 "left") (1 "center") (t "right")))

;;; ---------- TEXT (DTEXT) ----------

(defparameter *text-justify*
  '(("Left" 0 0) ("Center" 1 0) ("Right" 2 0) ("Middle" 4 0) ("TL" 0 3) ("TC" 1 3) ("TR" 2 3)
    ("ML" 0 2) ("MC" 1 2) ("MR" 2 2) ("BL" 0 1) ("BC" 1 1) ("BR" 2 1)))

(defun ask-text-style ()
  (let ((s (gstring (format nil "Enter style name or [?] <~A>: " (textstyle-name (current-textstyle))))))
    (cond ((string= s "") nil)
          ((string= s "?") (list-text-styles))
          ((find-textstyle s) (setf (sysvar "TEXTSTYLE") (textstyle-name (find-textstyle s))))
          (t (out (format nil "Cannot find text style \"~A\".~%" s))))))

(defun list-text-styles ()
  (out (format nil "~%Text styles:~%"))
  (dolist (st (all-textstyles))
    (out (format nil "  ~A~20T~A~@[ bold~]~@[ italic~]  height ~A  width ~A~%"
                 (textstyle-name st) (textstyle-font st) (textstyle-bold st) (textstyle-italic st)
                 (fmt-len (textstyle-height st)) (fmt-num (textstyle-width st)))))
  (out (format nil "Current text style:  ~A~%" (textstyle-name (current-textstyle)))))

(defcmd "TEXT" ("DT" "DTEXT") "Creates single-line text, typed in place on the drawing."
  (let ((just '(0 0)))
    (loop
      (let* ((st (current-textstyle)))
        (out (format nil "Current text style:  \"~A\"  Text height:  ~A~%" (textstyle-name st)
                     (fmt-len (if (plusp (textstyle-height st)) (textstyle-height st) (sysvar "TEXTSIZE")))))
        (let ((p (gpoint "Specify start point of text or [Justify/Style]: " :keywords '("Justify" "Style"))))
          (cond
            ((equal p "Style") (ask-text-style))
            (t
             (when (equal p "Justify")
               (let ((j (gkw "Enter an option [Left/Center/Right/Middle/TL/TC/TR/ML/MC/MR/BL/BC/BR]: "
                             (mapcar #'first *text-justify*) "Left")))
                 (setf just (rest (assoc j *text-justify* :test #'string-equal)))
                 (setf p (gpoint (format nil "Specify ~A point of text: " (string-downcase j))))))
             (when (pointp p)
               (let* ((fixed (and (plusp (textstyle-height st)) (textstyle-height st)))
                      (h (or fixed
                             (gdist (format nil "Specify height <~A>: " (fmt-len (sysvar "TEXTSIZE"))) :base p :flags 6)
                             (sysvar "TEXTSIZE")))
                      (rot (or (gangle "Specify rotation angle of text <0>: " :base p) 0d0)))
                 (unless fixed (setf (sysvar "TEXTSIZE") h))
                 (loop for i from 0
                       for ip = (polar p (- rot (/ +pi+ 2)) (* i h 1.667d0))
                       ;; in the GUI the text is typed on the drawing; Enter starts the next line below
                       for s = (if (use-inplace-editor-p)
                                   (progn (when (= i 0) (out (format nil "Enter text (Enter on an empty line to finish):~%")))
                                          (getf (inplace-edit :line ip nil h :style (textstyle-name st)) :text))
                                   (gstring "Enter text: " t))
                       until (string= s "")
                       do (add (append (list (cons 0 "TEXT") (cons 10 ip) (cons 40 (d h)) (cons 1 s) (cons 50 (d rot))
                                             (cons 7 (textstyle-name st)) (cons 41 (d (textstyle-width st))))
                                       (unless (equal just '(0 0))
                                         (list (cons 72 (first just)) (cons 73 (second just)) (cons 11 ip)))))
                          (when (use-inplace-editor-p) (gui-regen)))))
             (return))))))))

;;; ---------- MTEXT ----------

(defun mtext-insertion (x0 y0 x1 y1 attach)
  "Insertion point inside the box (X0,Y0)-(X1,Y1) (top-left / bottom-right) for attachment 1-9."
  (let ((col (mod (1- attach) 3)) (row (floor (1- attach) 3)))
    (pt (case col (0 x0) (1 (/ (+ x0 x1) 2)) (t x1))
        (case row (0 y0) (1 (/ (+ y0 y1) 2)) (t y1)))))

(defcmd "MTEXT" ("MT" "T") "Creates multiline (paragraph) text, typed in place on the drawing."
  (let* ((st (current-textstyle))
         (h (d (if (plusp (textstyle-height st)) (textstyle-height st) (sysvar "TEXTSIZE"))))
         (attach 1) (rot 0d0) (width nil) (corner nil))
    (out (format nil "Current text style:  \"~A\"  Text height:  ~A~%" (textstyle-name st) (fmt-len h)))
    (let ((p (gpoint "Specify first corner: ")))
      (when (pointp p)
        (loop
          (let ((q (get-user-input :corner :base p
                                   :prompt "Specify opposite corner or [Height/Justify/Rotation/Style/Width]: "
                                   :keywords '("Height" "Justify" "Rotation" "Style" "Width"))))
            (cond ((pointp q) (setf corner q) (return))
                  ((null q) (return))
                  ((equal q "Height")
                   (setf h (d (or (gdist (format nil "Specify height <~A>: " (fmt-len h)) :base p :flags 6) h)))
                   (setf (sysvar "TEXTSIZE") h))
                  ((equal q "Justify")
                   (let ((j (gkw "Enter justification [TL/TC/TR/ML/MC/MR/BL/BC/BR] <TL>: "
                                 (mapcar #'car *mtext-justify*) "TL")))
                     (setf attach (or (cdr (assoc j *mtext-justify* :test #'string-equal)) 1))))
                  ((equal q "Rotation")
                   (setf rot (or (gangle "Specify rotation angle <0>: " :base p) 0d0)))
                  ((equal q "Style")
                   (ask-text-style) (setf st (current-textstyle))
                   (when (plusp (textstyle-height st)) (setf h (d (textstyle-height st)))))
                  ((equal q "Width")
                   (setf width (gdist "Specify width: " :base p :flags 6))
                   (return)))))
        ;; box: top-left (x0,y0) to bottom-right (x1,y1)
        (let* ((x0 (if corner (min (px p) (px corner)) (px p)))
               (y0 (if corner (max (py p) (py corner)) (py p)))
               (x1 (if corner (max (px p) (px corner)) (+ (px p) (or width 0))))
               (y1 (if corner (min (py p) (py corner)) (py p)))
               (w (cond (corner (- x1 x0)) (width width) (t 0d0)))
               (r (if (use-inplace-editor-p)
                      (inplace-edit :mtext (pt x0 y0) w h :min-height (- y0 y1)
                                    :justify (column-justify attach) :style (textstyle-name st))
                      (list :text (format nil "~{~A~^\\P~}" (loop for s = (gstring "MText: " t) until (string= s "") collect s))
                            :style (textstyle-name st) :font (textstyle-font st) :height h
                            :bold (textstyle-bold st) :italic (textstyle-italic st)
                            :justify (column-justify attach)))))
          (when (plusp (length (string-trim '(#\Space) (text-string-display (getf r :text)))))
            (let* ((attach (+ (* 3 (floor (1- attach) 3)) (justify-column (getf r :justify)) 1))
                   (ins (mtext-insertion x0 y0 x1 y1 attach))
                   (h (getf r :height)))
              (setf (sysvar "TEXTSIZE") h)
              (add (list (cons 0 "MTEXT") (cons 10 (pt3 ins)) (cons 40 h) (cons 41 (d w))
                         (cons 71 attach) (cons 72 1) (cons 1 (editor-result-content r)) (cons 50 (d rot))
                         (cons 7 (textstyle-name (or (find-textstyle (getf r :style)) st))))))))))))

;;; ---------- TEXTEDIT ----------

(defun mtext-top-left (data)
  "World top-left corner of an MTEXT's box (where the in-place editor opens)."
  (fourth (text-box data)))

(defun edit-text-entity (e)
  (let* ((data (ent-data e)) (type (ent-type e)) (old (or (dxf 1 data) "")))
    (cond
      ((not (use-inplace-editor-p))
       (let ((s (gstring (format nil "Enter new text <~A>: " old) t)))
         (when (plusp (length s)) (set-entity-data e (set-dxf 1 s data)))))
      ((equal type "MTEXT")
       (let* ((st (or (find-textstyle (dxf 7 data)) (find-textstyle "Standard")))
              (fmt (mtext-overall-format old))
              (font (or (getf fmt :font) (textstyle-font st)))
              (bold (if (getf fmt :font) (getf fmt :bold) (or (getf fmt :bold) (textstyle-bold st))))
              (italic (if (getf fmt :font) (getf fmt :italic) (or (getf fmt :italic) (textstyle-italic st))))
              (w (d (or (dxf 41 data) 0)))
              (box (text-box data))
              (r (inplace-edit :mtext (mtext-top-left data) w (text-effective-height data)
                               :init (text-string-display old) :aci (entity-color-aci e)
                               :justify (column-justify (dxf 71 data)) :style (textstyle-name st)
                               :font font :bold bold :italic italic :underline (getf fmt :underline)))
              (attach (or (dxf 71 data) 1))
              (new-attach (+ (* 3 (floor (1- attach) 3)) (justify-column (getf r :justify)) 1))
              (nd data))
         (when (plusp (length (string-trim '(#\Space) (text-string-display (getf r :text)))))
           ;; a new alignment keeps the box where it is: move the insertion point within it
           (when (and (/= new-attach attach) (plusp w) (zerop (or (dxf 50 data) 0)))
             (let ((left (px (fourth box))))
               (setf nd (set-dxf 10 (pt (+ left (* w (/ (justify-column (getf r :justify)) 2)))
                                        (py (pt3 (dxf 10 data))) (pz (pt3 (dxf 10 data))))
                                 nd))))
           (setf nd (set-dxf 1 (editor-result-content r) nd))
           (setf nd (set-dxf 40 (getf r :height) nd))
           (setf nd (set-dxf 71 new-attach nd))
           (setf nd (set-dxf 7 (textstyle-name (or (find-textstyle (getf r :style)) st)) nd))
           (unless (equal nd data) (set-entity-data e nd)))))
      (t
       (let ((new (getf (inplace-edit :line (text-insertion data) nil (d (or (dxf 40 data) 0.2d0))
                                      :init old :aci (entity-color-aci e) :style (dxf 7 data))
                        :text)))
         (when (and (plusp (length new)) (string/= new old))
           (set-entity-data e (set-dxf 1 new data))))))))

(defcmd "TEXTEDIT" ("ED" "DDEDIT" "MTEDIT") "Edits the contents of a text object (in place on the drawing)."
  (if *edit-target*
      (let ((e *edit-target*)) (setf *edit-target* nil) (when (ent-live-p e) (edit-text-entity e)))
      (loop
        (let ((r (get-user-input :entity :prompt "Select an annotation object or [Undo]: " :keywords '("Undo"))))
          (cond ((null r) (return))
                ((consp r)
                 (let ((e (sel-entity r)))
                   (if (and e (member (ent-type e) '("TEXT" "MTEXT" "ATTDEF") :test #'equal))
                       (edit-text-entity e)
                       (out (format nil "That is not a text object.~%"))))))))))

;;; ---------- STYLE ----------

(defun style-in-use-p (name)
  (or (some (lambda (e) (equalp (dxf 7 (ename-data e)) name)) (all-live-entities))
      (some (lambda (b) (some (lambda (dd) (equalp (dxf 7 dd) name)) (block-def-entities b))) (drawing-blocks *dwg*))))

(defun style-dialog ()
  (let* ((recs (format nil "{~{{~A}~^ ~}}"
                       (mapcar (lambda (st)
                                 (format nil "~A ~A ~D ~D ~A ~A ~A" (tq (textstyle-name st)) (tq (textstyle-font st))
                                         (if (textstyle-bold st) 1 0) (if (textstyle-italic st) 1 0)
                                         (tq (fmt-num (textstyle-height st))) (tq (fmt-num (textstyle-width st)))
                                         (tq (fmt-num (* (textstyle-oblique st) (/ 180 pi))))))
                               (all-textstyles))))
         (r (tk-ask (format nil "styledlg ~A ~A" recs (tq (textstyle-name (current-textstyle)))))))
    (unless (or (string= r "CANCEL") (string= r ""))
      (let* ((parts (split-string r (code-char 30)))
             (current (first parts))
             (kept nil))
        (dolist (rec (rest parts))
          (destructuring-bind (name font bold italic height width oblique &rest ignore)
              (append (split-string rec (code-char 31)) (list "" "" "" "" "" "" ""))
            (declare (ignore ignore))
            (let ((old (find-textstyle name)))
              (push name kept)
              (define-textstyle
                  (make-textstyle :name name :font (if (string= (string-trim " " font) "") "Arial" (string-trim " " font))
                                  :bold (string= bold "1") :italic (string= italic "1")
                                  :height (d (max 0 (or (parse-number-text height) 0)))
                                  :width (d (let ((v (parse-number-text width))) (if (and v (plusp v)) v 1)))
                                  :oblique (* (d (or (parse-number-text oblique) 0)) (/ pi 180))
                                  :bigfont (if old (textstyle-bigfont old) "")
                                  :backwards (and old (textstyle-backwards old))
                                  :upside-down (and old (textstyle-upside-down old)))))))
        ;; deleted styles (only if nothing uses them)
        (dolist (st (copy-list (all-textstyles)))
          (unless (member (textstyle-name st) kept :test #'string-equal)
            (if (style-in-use-p (textstyle-name st))
                (out (format nil "Style ~A is in use and was not deleted.~%" (textstyle-name st)))
                (setf (drawing-styles *dwg*) (remove st (drawing-styles *dwg*))))))
        (when (find-textstyle current) (setf (sysvar "TEXTSTYLE") (textstyle-name (find-textstyle current))))
        (touch)
        (gui-regen)
        (out (format nil "Current text style:  ~A~%" (textstyle-name (current-textstyle))))))))

(defun style-command-line ()
  (let* ((cur (current-textstyle))
         (name (let ((s (gstring (format nil "Enter name of text style or [?] <~A>: " (textstyle-name cur)) t)))
                 (cond ((string= s "") (textstyle-name cur))
                       ((string= s "?") (list-text-styles) (return-from style-command-line nil))
                       (t (string-trim " " s)))))
         (old (or (find-textstyle name) cur))
         (font (let ((s (gstring (format nil "Specify full font name or font filename (TTF or SHX) <~A>: " (textstyle-font old)) t)))
                 (if (string= s "") (textstyle-font old) (string-trim " " s))))
         (height (or (greal (format nil "Specify height of text <~A>: " (fmt-len (textstyle-height old)))) (textstyle-height old)))
         (width (or (greal (format nil "Specify width factor <~A>: " (fmt-num (textstyle-width old)))) (textstyle-width old)))
         (oblique (let ((v (greal (format nil "Specify obliquing angle <~A>: " (fmt-num (* (textstyle-oblique old) (/ 180 pi)))))))
                    (if v (* v (/ pi 180)) (textstyle-oblique old))))
         (back (string-equal (gkw (format nil "Display text backwards? [Yes/No] <~:[No~;Yes~]>: " (textstyle-backwards old))
                                  '("Yes" "No") (if (textstyle-backwards old) "Yes" "No")) "Yes"))
         (upside (string-equal (gkw (format nil "Display text upside-down? [Yes/No] <~:[No~;Yes~]>: " (textstyle-upside-down old))
                                    '("Yes" "No") (if (textstyle-upside-down old) "Yes" "No")) "Yes")))
    (define-textstyle (make-textstyle :name (if (find-textstyle name) (textstyle-name (find-textstyle name)) name)
                                      :font font :height (d (max 0 height)) :width (d (if (plusp width) width 1))
                                      :oblique (d oblique) :bold (textstyle-bold old) :italic (textstyle-italic old)
                                      :bigfont (textstyle-bigfont old) :backwards back :upside-down upside))
    (setf (sysvar "TEXTSTYLE") (textstyle-name (find-textstyle name)))
    (touch)
    (when (gui-p) (gui-regen))
    (out (format nil "\"~A\" is now the current text style.~%" (sysvar "TEXTSTYLE")))))

(defcmd "STYLE" ("ST" "DDSTYLE") "Creates, modifies, or sets the current text style."
  (if (use-inplace-editor-p) (style-dialog) (style-command-line)))
(defcmd "-STYLE" () "Text styles at the command line."
  (style-command-line))

;;; ---------- MTEXT width grips ----------

(defun mtext-width-stretch (data id to)
  "Drag the left or right top corner of an MTEXT box to TO: the other side stays put, the wrap
width (41) changes, and the insertion point moves to stay at its place in the box."
  (let* ((p (pt3 (dxf 10 data))) (rot (or (dxf 50 data) 0d0))
         (h (text-effective-height data))
         (box (text-box data))
         (w (dist2d (fourth box) (third box)))
         (colf (/ (mod (1- (or (dxf 71 data) 1)) 3) 2d0))
         (v (v- to p))
         (lx (+ (* (px v) (cos rot)) (* (py v) (sin rot))))   ; TO along the text direction
         (left (- (* w colf))) (right (* w (- 1 colf)))
         (minw (* 2 h))
         (nl (if (eq id :mtw-left) (min lx (- right minw)) left))
         (nr (if (eq id :mtw-right) (max lx (+ left minw)) right))
         (nw (- nr nl))
         (ix (+ nl (* nw colf))))
    (set-dxf 41 (d nw) (set-dxf 10 (pt3 (pt (+ (px p) (* ix (cos rot))) (+ (py p) (* ix (sin rot))) (pz p))) data))))
