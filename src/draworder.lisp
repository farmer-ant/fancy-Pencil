;;;; draworder.lisp — DRAWORDER (bring to front / send to back), TEXTTOFRONT, HATCHTOBACK, and
;;;; changing the case of text (TCASE, also in the text editor's right-click menu and Shift+F3).
;;;;
;;;; Draw order is the order of the drawing's object list: later objects are drawn over earlier
;;;; ones, on the screen and in plots. Files keep it (AutoCAD's SORTENTSTABLE in DXF 2000).

(in-package :fancy-pencil)

;;; ---------- draw order ----------

(defun reorder-entities (ents where &optional refs)
  "Move ENTS (keeping their order among themselves) to the :FRONT or :BACK of everything, or
just :ABOVE or :UNDER the reference objects REFS."
  (let* ((moving-set (make-hash-table :test #'eq)) (ref-set (make-hash-table :test #'eq))
         (all (coerce (drawing-entities *dwg*) 'list)))
    (dolist (e ents) (setf (gethash e moving-set) t))
    (dolist (e refs) (unless (gethash e moving-set) (setf (gethash e ref-set) t)))
    (let* ((moving (remove-if-not (lambda (e) (gethash e moving-set)) all))
           (others (remove-if (lambda (e) (gethash e moving-set)) all))
           (new (ecase where
                  (:front (append others moving))
                  (:back (append moving others))
                  (:above (let ((k (position-if (lambda (e) (gethash e ref-set)) others :from-end t)))
                            (if k (append (subseq others 0 (1+ k)) moving (nthcdr (1+ k) others)) (append others moving))))
                  (:under (let ((k (position-if (lambda (e) (gethash e ref-set)) others)))
                            (if k (append (subseq others 0 k) moving (nthcdr k others)) (append moving others))))))
           (v (drawing-entities *dwg*)))
      (unless (equal new all)
        (setf (drawing-reordered *dwg*) t)
        (setf (fill-pointer v) 0)
        (dolist (e new) (vector-push-extend e v))
        (touch))
      (length moving))))

(defun draw-order-done (n where)
  (out (format nil "~D object~:P ~A.~%" n
               (ecase where (:front "brought to the front") (:back "sent to the back")
                 (:above "brought above the reference objects") (:under "sent under the reference objects"))))
  (when (gui-p) (gui-regen)))

(defcmd "DRAWORDER" ("DR") "Changes which objects are drawn on top: Front, Back, Above or Under other objects."
  (let ((sel (get-selection "Select objects: ")))
    (when sel
      (let* ((k (gkw "Enter object ordering option [Above objects/Under objects/Front/Back] <Back>: "
                     '("Above" "Under" "Front" "Back") "Back"))
             (where (cond ((equal k "Above") :above) ((equal k "Under") :under) ((equal k "Front") :front) (t :back)))
             (refs (when (member where '(:above :under)) (get-selection "Select reference objects: "))))
        (unless (and (member where '(:above :under)) (null refs))
          (draw-order-done (reorder-entities sel where refs) where))))))

(defcmd "BRINGTOFRONT" () "Draws the selected objects over everything else."
  (let ((sel (get-selection "Select objects: "))) (when sel (draw-order-done (reorder-entities sel :front) :front))))
(defcmd "SENDTOBACK" () "Draws the selected objects under everything else."
  (let ((sel (get-selection "Select objects: "))) (when sel (draw-order-done (reorder-entities sel :back) :back))))

(defcmd "TEXTTOFRONT" () "Brings text, dimensions and leaders in front of everything else."
  (let* ((k (gkw "Bring to front: [Text/Dimensions/Leaders/All] <All>: " '("Text" "Dimensions" "Leaders" "All") "All"))
         (types (cond ((equal k "Text") '("TEXT" "MTEXT" "ATTDEF"))
                      ((equal k "Dimensions") '("DIMENSION"))
                      ((equal k "Leaders") '("LEADER"))
                      (t '("TEXT" "MTEXT" "ATTDEF" "DIMENSION" "LEADER"))))
         (ents (remove-if-not (lambda (e) (member (ent-type e) types :test #'equal)) (live-entities))))
    (draw-order-done (reorder-entities ents :front) :front)))

(defcmd "HATCHTOBACK" () "Sends every hatch and fill behind everything else."
  (let ((ents (remove-if-not (lambda (e) (member (ent-type e) '("HATCH" "SOLID") :test #'equal)) (live-entities))))
    (draw-order-done (reorder-entities ents :back) :back)))

;;; ---------- text case ----------

(defun mtext-code-length (s i)
  "Length of the MTEXT formatting code starting at I (a backslash or brace), or NIL for plain text."
  (let ((c (char s i)) (n (length s)))
    (cond ((member c '(#\{ #\})) 1)
          ((and (char= c #\\) (< (1+ i) n))
           (if (find (char s (1+ i)) "fFHCcTQWApS")
               ;; codes with a value (font, height, color ...) end at ';'
               (let ((semi (position #\; s :start i))) (if semi (- (1+ semi) i) 2))
               2))                                  ; \P \L \~ \\ \{ ...
          (t nil))))

(defun change-case (s mode &optional mtext)
  "S in MODE :upper :lower :sentence :title or :toggle. MTEXT: leave formatting codes alone.
Special codes such as %%c and %%d are left alone too."
  (let* ((n (length s)) (keep (make-array n :initial-element nil))
         (break-at (make-array n :initial-element nil)) (i 0))
    ;; mark what is not words: formatting codes and %% codes; a new paragraph starts a sentence
    (loop while (< i n)
          do (let ((len (cond ((and mtext (mtext-code-length s i)))
                              ((and (char= (char s i) #\%) (< (+ i 2) n) (char= (char s (1+ i)) #\%)) 3)
                              (t nil))))
               (if len
                   (progn (when (and mtext (< (1+ i) n) (char= (char s i) #\\) (char= (char s (1+ i)) #\P))
                            (setf (aref break-at i) t))
                          (loop for j from i below (min n (+ i len)) do (setf (aref keep j) t))
                          (incf i len))
                   (incf i))))
    (let ((out (copy-seq s)) (start-word t) (start-sentence t))
      (dotimes (j n out)
        (if (aref keep j)
            (when (aref break-at j) (setf start-word t start-sentence t))
            (let ((c (char s j)))
              (setf (char out j)
                    (ecase mode
                      (:upper (char-upcase c))
                      (:lower (char-downcase c))
                      (:toggle (if (upper-case-p c) (char-downcase c) (char-upcase c)))
                      (:title (if (and start-word (alpha-char-p c)) (char-upcase c) (char-downcase c)))
                      (:sentence (if (and start-sentence (alpha-char-p c)) (char-upcase c) (char-downcase c)))))
              (cond ((alphanumericp c) (setf start-word nil start-sentence nil))
                    ((member c '(#\. #\! #\? #\Newline)) (setf start-word t start-sentence t))
                    (t (setf start-word t)))))))))

(defun case-mode (k)
  (cond ((string-equal k "lower") :lower) ((string-equal k "Sentence") :sentence)
        ((string-equal k "Title") :title) ((string-equal k "toGGLE") :toggle) (t :upper)))

(defun text-case-ents (ents)
  (remove-if-not (lambda (e) (let ((d (ename-data e)))
                               (or (member (dxf 0 d) '("TEXT" "MTEXT" "ATTDEF") :test #'equal)
                                   (and (equal (dxf 0 d) "DIMENSION") (plusp (length (or (dxf 1 d) "")))))))
                 ents))

(defun change-text-case (ents mode)
  (let ((n 0))
    (dolist (e (filter-editable (text-case-ents ents)) n)
      (let* ((d (ent-data e)) (s (or (dxf 1 d) ""))
             (ns (change-case s mode (member (dxf 0 d) '("MTEXT" "DIMENSION") :test #'equal))))
        (unless (string= s ns)
          (set-entity-data e (set-dxf 1 ns d)))
        (incf n)))))
