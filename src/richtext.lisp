;;;; richtext.lisp — MTEXT with formatting on parts of the text: reading the inline codes into
;;;; runs (stretches of text sharing one format), writing runs back as MTEXT codes, and laying
;;;; out mixed runs for the screen and for plots.
;;;;
;;;; A run is (TEXT FONT BOLD ITALIC UNDERLINE HEIGHT ACI): HEIGHT in drawing units, ACI NIL for
;;;; the object's own color. Paragraph breaks are #\Newline inside TEXT.

(in-package :fancy-pencil)

(defun run-text (r) (first r))
(defun run-font (r) (second r))
(defun run-bold (r) (third r))
(defun run-italic (r) (fourth r))
(defun run-underline (r) (fifth r))
(defun run-height (r) (sixth r))
(defun run-aci (r) (seventh r))
(defun run-stack (r)
  "A stacked fraction or tolerance (\\S): (TOP BOTTOM KIND), KIND :bar (a/b over a line),
:diagonal (a#b) or :tolerance (a^b, no line); NIL for ordinary text. Its text is the flat a/b."
  (eighth r))

(defun same-height-p (a b) (<= (abs (- a b)) (* 1d-6 (max (abs a) (abs b) 1d-9))))

(defun run-format= (a b)
  (and (null (run-stack a)) (null (run-stack b))
       (string-equal (run-font a) (run-font b))
       (eq (and (run-bold a) t) (and (run-bold b) t))
       (eq (and (run-italic a) t) (and (run-italic b) t))
       (eq (and (run-underline a) t) (and (run-underline b) t))
       (same-height-p (run-height a) (run-height b))
       (eql (run-aci a) (run-aci b))))

(defun merge-runs (runs)
  "Join neighbouring runs of one format and drop empty ones."
  (let ((out nil))
    (dolist (r runs)
      (when (plusp (length (run-text r)))
        (if (and out (run-format= (first out) r))
            (setf (first out) (cons (concatenate 'string (run-text (first out)) (run-text r)) (rest (first out))))
            (push (copy-list r) out))))
    (nreverse out)))

(defun shx-font-p (font)
  (let ((f (string-downcase (or font ""))))
    (and (> (length f) 4) (string= (subseq f (- (length f) 4)) ".shx"))))

(defun mtext-special-chars (s)
  (loop for (code rep) in '(("%%c" "Ø") ("%%C" "Ø") ("%%d" "°") ("%%D" "°") ("%%p" "±") ("%%P" "±"))
        do (loop for pos = (search code s) while pos
                 do (setf s (concatenate 'string (subseq s 0 pos) rep (subseq s (+ pos 3))))))
  s)

(defun mtext-runs (content &key (font "Arial") bold italic (height 0.2d0))
  "CONTENT (MTEXT codes) as a list of runs, starting from the given (style) format."
  (let ((n (length content)) (i 0)
        (fmt (list font (and bold t) (and italic t) nil (d height) nil))
        (stack nil) (runs nil) (buf (make-string-output-stream)) (buf-fmt nil))
    (labels ((flush ()
               (let ((s (get-output-stream-string buf)))
                 (when (plusp (length s)) (push (cons (mtext-special-chars s) buf-fmt) runs))))
             (emit (x)
               (unless (and buf-fmt (equalp buf-fmt fmt))
                 (flush) (setf buf-fmt (copy-list fmt)))
               (if (characterp x) (write-char x buf) (write-string x buf)))
             (setf-fmt (k v) (setf (nth k fmt) v))
             (to-semi () (or (position #\; content :start i) n)))
      (loop while (< i n) do
        (let ((ch (char content i)))
          (cond
            ((char= ch #\{) (push (copy-list fmt) stack) (incf i))
            ((char= ch #\}) (when stack (setf fmt (pop stack))) (incf i))
            ((and (char= ch #\\) (< (1+ i) n))
             (let ((c (char content (1+ i))))
               (case c
                 ((#\P #\N) (emit #\Newline) (incf i 2))
                 ((#\\ #\{ #\}) (emit c) (incf i 2))
                 (#\~ (emit #\Space) (incf i 2))
                 ((#\f #\F)
                  (let* ((semi (to-semi))
                         (parts (split-string (subseq content (+ i 2) semi) #\|))
                         (name (string-trim " " (first parts))))
                    (when (and (char= c #\F) (plusp (length name)) (not (find #\. name)))
                      (setf name (concatenate 'string name ".shx")))
                    (when (plusp (length name)) (setf-fmt 0 name))
                    (setf-fmt 1 nil) (setf-fmt 2 nil)
                    (dolist (p (rest parts))
                      (when (> (length p) 1)
                        (case (char p 0)
                          (#\b (setf-fmt 1 (string= (subseq p 1) "1")))
                          (#\i (setf-fmt 2 (string= (subseq p 1) "1"))))))
                    (setf i (1+ semi))))
                 (#\H
                  (let* ((semi (to-semi))
                         (txt (string-trim " " (subseq content (+ i 2) semi)))
                         (rel (and (plusp (length txt)) (char-equal (char txt (1- (length txt))) #\x)))
                         (v (ignore-errors (parse-number-text (string-right-trim "xX" txt)))))
                    (when (and v (plusp v))
                      (setf-fmt 4 (d (if rel (* (nth 4 fmt) v) v))))
                    (setf i (1+ semi))))
                 (#\C
                  (let* ((semi (to-semi))
                         (v (ignore-errors (parse-integer (string-trim " " (subseq content (+ i 2) semi))))))
                    (setf-fmt 5 (and v (<= 1 v 255) v))
                    (setf i (1+ semi))))
                 (#\L (setf-fmt 3 t) (incf i 2))
                 (#\l (setf-fmt 3 nil) (incf i 2))
                 (#\S
                  ;; stacked fraction or tolerance: a run of its own, drawn as two small rows
                  (let* ((semi (to-semi))
                         (body (subseq content (+ i 2) semi))
                         (k (position-if (lambda (x) (member x '(#\/ #\# #\^))) body)))
                    (flush) (setf buf-fmt nil)
                    (if k
                        (let ((top (string-trim " " (subseq body 0 k))) (bottom (string-trim " " (subseq body (1+ k)))))
                          (push (append (list (mtext-special-chars (format nil "~A/~A" top bottom))) (copy-list fmt)
                                        (list (list (mtext-special-chars top) (mtext-special-chars bottom)
                                                    (case (char body k) (#\/ :bar) (#\# :diagonal) (t :tolerance)))))
                                runs))
                        (emit body))
                    (setf i (1+ semi))))
                 ;; \X: what follows goes under the dimension line (alternate units): a new line here
                 (#\X (emit #\Newline) (incf i 2))
                 ((#\c #\A #\Q #\W #\T #\p)
                  (setf i (1+ (to-semi))))
                 (t (incf i 2)))))
            ((and (char= ch #\^) (< (1+ i) n) (find (char content (1+ i)) "JIM @"))
             (case (char content (1+ i))
               (#\J (emit #\Newline))
               (#\I (emit "    "))
               (#\Space (emit #\^)))
             (incf i 2))
            (t (emit ch) (incf i)))))
      (flush)
      (merge-runs (nreverse runs)))))

(defun mtext-data-runs (data)
  "The runs of an MTEXT entity, starting from its style."
  (let ((st (or (find-textstyle (dxf 7 data)) (find-textstyle "Standard"))))
    (mtext-runs (or (dxf 1 data) "")
                :font (style-display-font st)
                :bold (and st (textstyle-bold st)) :italic (and st (textstyle-italic st))
                :height (or (dxf 40 data) 0.2d0))))

(defun mtext-rich-p (data)
  "True when an MTEXT has formatting on part of its text (or a color of its own), so it is drawn run by run."
  (and (equal (dxf 0 data) "MTEXT")
       (let ((c (or (dxf 1 data) "")))
         (and (find #\\ c)
              (let ((runs (mtext-data-runs data)))
                (or (and (some #'run-aci runs) t)
                    (and (some #'run-stack runs) t)
                    (and (rest runs)
                         (notevery (lambda (r) (run-format= r (first runs))) (rest runs)))))))))

(defun runs-base-height (runs)
  "The height most of the characters have: the object's own height (code 40)."
  (let ((tally nil))
    (dolist (r runs)
      (let* ((h (run-height r)) (cell (assoc h tally :test #'same-height-p)))
        (if cell (incf (cdr cell) (count #\Newline (run-text r) :test-not #'char=))
            (push (cons h (count #\Newline (run-text r) :test-not #'char=)) tally))))
    (if tally (car (first (sort tally #'> :key #'cdr))) 0.2d0)))

(defun mtext-escape (s)
  (with-output-to-string (o)
    (loop for c across s do
      (case c
        (#\\ (write-string "\\\\" o)) (#\{ (write-string "\\{" o)) (#\} (write-string "\\}" o))
        (#\Newline (write-string "\\P" o))
        (t (write-char c o))))))

(defun num-text (x)
  (let ((s (string-right-trim "." (string-right-trim "0" (format nil "~,4F" x)))))
    (if (string= s "") "0" s)))

(defun mtext-encode-runs (runs &key (font "Arial") bold italic base-height)
  "MTEXT content for RUNS: plain text where a run has the default (style) format, a {...} group
with only the codes that differ elsewhere. Heights are written relative to BASE-HEIGHT."
  (let ((base (or base-height (runs-base-height runs))))
    (with-output-to-string (o)
      (dolist (r (merge-runs runs))
        (let* ((fontp (or (not (string-equal (run-font r) font))
                          (not (eq (and (run-bold r) t) (and bold t)))
                          (not (eq (and (run-italic r) t) (and italic t)))))
               (hp (not (same-height-p (run-height r) base)))
               (codes (with-output-to-string (c)
                        (when fontp
                          (if (shx-font-p (run-font r))
                              (format c "\\F~A;" (run-font r))
                              (format c "\\f~A|b~D|i~D|c0|p0;" (run-font r) (if (run-bold r) 1 0) (if (run-italic r) 1 0))))
                        (when hp (format c "\\H~Ax;" (num-text (/ (run-height r) base))))
                        (when (run-aci r) (format c "\\C~D;" (run-aci r)))
                        (when (run-underline r) (write-string "\\L" c)))))
          (if (string= codes "")
              (write-string (mtext-escape (run-text r)) o)
              (format o "{~A~A}" codes (mtext-escape (run-text r)))))))))

;;; ---------- layout (plots, extents) ----------

(defun run-size (r) (/ (run-height r) +cap+))

(defparameter *stack-scale* 1d0
  "Each row of a stack is the run's own height (AutoCAD sets the size with \\H: DIMTFAC).")

(defun stack-rows (r)
  "How a stack sits: (values TOP-BASELINE BOTTOM-BASELINE LINE-Y TOP-WIDTH BOTTOM-WIDTH), heights
above the text's baseline, from run R (its rows are run-height high)."
  (destructuring-bind (top bottom kind) (run-stack r)
    (let* ((h (* *stack-scale* (run-height r)))
           (plain (append (list "") (rest (butlast r 1)) (list nil)))
           (wt (run-width top plain)) (wb (run-width bottom plain)))
      (case kind
        (:diagonal (values (* 0.55d0 h) (* -0.25d0 h) nil wt wb))
        (t (values (* 0.8d0 h) (* -0.5d0 h) (and (eq kind :bar) (* 0.65d0 h)) wt wb))))))

(defun stack-parts (r x y)
  "A stack placed with its left end at X on baseline Y (Y up): a list of (:text STRING X Y) for
each row's baseline-left corner and (:line X1 Y1 X2 Y2) for its bar or slash."
  (destructuring-bind (top bottom kind) (run-stack r)
    (multiple-value-bind (tb bb ly wt wb) (stack-rows r)
      (let ((h (run-height r)) (w (max wt wb)))
        (case kind
          (:diagonal
           (list (list :text top x (+ y tb))
                 (list :line (+ x wt (* 0.05d0 h)) (+ y bb) (+ x wt (* 0.45d0 h)) (+ y tb h))
                 (list :text bottom (+ x wt (* 0.5d0 h)) (+ y bb))))
          (:tolerance                     ; rows left-aligned, no line
           (list (list :text top x (+ y tb)) (list :text bottom x (+ y bb))))
          (t
           (let ((ww (+ w (* 0.1d0 h))))
             (list (list :text top (+ x (/ (- ww wt) 2)) (+ y tb))
                   (list :line x (+ y ly) (+ x ww) (+ y ly))
                   (list :text bottom (+ x (/ (- ww wb) 2)) (+ y bb))))))))))

(defun stack-width (r)
  (multiple-value-bind (tb bb ly wt wb) (stack-rows r)
    (declare (ignore tb bb ly))
    (if (eq (third (run-stack r)) :diagonal)
        (+ wt wb (* 0.5d0 (run-height r)))
        (+ (max wt wb) (* 0.1d0 (run-height r))))))

(defun run-width (s r)
  "Approximate width of S in run R's format, in drawing units."
  (when (run-stack r) (return-from run-width (stack-width r)))
  (let ((fam (string-downcase (font-family-for (run-font r)))))
    (* (run-size r)
       (if (or (search "mono" fam) (search "cour" fam)) (* 0.6d0 (length s)) (text-em-width s))
       (if (run-bold r) 1.06d0 1d0))))

(defun mtext-rich-layout (data)
  "Lay out a rich MTEXT: returns (values PIECES TOTAL-HEIGHT BOX-WIDTH). Each piece is
(TEXT RUN X BASELINE-Y) from the box's top-left corner, Y measured downwards."
  (let* ((width (and (dxf 41 data) (plusp (dxf 41 data)) (dxf 41 data)))
         (runs (mtext-data-runs data))
         (lines nil) (cur nil) (curw 0d0) (pend nil) (word nil) (last-run (first runs)))
    (labels ((wordw (chunks) (loop for (s r) in chunks sum (run-width s r)))
             (place (chunks)
               (dolist (ch chunks)
                 (push (list (first ch) (second ch) curw) cur)
                 (incf curw (run-width (first ch) (second ch)))))
             (flush-word ()
               (when word
                 (let ((ww (wordw word)) (sw (wordw pend)))
                   (if (and width cur (> (+ curw sw ww) width))
                       (progn (end-line) (place word))
                       (progn (place pend) (place word))))
                 (setf word nil pend nil)))
             (end-line ()
               (push (list (nreverse cur) curw last-run) lines)
               (setf cur nil curw 0d0 pend nil)))
      (dolist (r runs)
        (setf last-run r)
        (let ((s (if (run-stack r) "" (run-text r))) (i 0))
          (when (run-stack r)             ; a stack is one piece: never split
            (setf word (append word (list (list (run-text r) r)))))
          (loop while (< i (length s)) do
            (let* ((c (char s i))
                   (j (cond ((char= c #\Newline) (1+ i))
                            ((char= c #\Space) (or (position #\Space s :start i :test-not #'char=) (length s)))
                            (t (or (position-if (lambda (x) (member x '(#\Space #\Newline))) s :start i) (length s)))))
                   (chunk (subseq s i j)))
              (cond ((char= c #\Newline) (flush-word) (end-line))
                    ((char= c #\Space) (flush-word) (setf pend (append pend (list (list chunk r)))))
                    (t (setf word (append word (list (list chunk r))))))
              (setf i j)))))
      (flush-word)
      (end-line))
    (setf lines (nreverse lines))
    (let* ((box-w (or width (reduce #'max lines :key #'second :initial-value 0d0)))
           (col (mod (1- (or (dxf 71 data) 1)) 3))
           (y 0d0) (pieces nil) (n (length lines)))
      (loop for (items lw lrun) in lines for k from 1 do
        (let* ((mh (reduce #'max items :key (lambda (it) (run-height (second it)))
                                       :initial-value (if items 0d0 (run-height lrun))))
               (dx (case col (0 0d0) (1 (/ (- box-w lw) 2)) (t (- box-w lw)))))
          (dolist (it items)
            (push (list (first it) (second it) (+ dx (third it)) (+ y mh)) pieces))
          (incf y (if (= k n) mh (* mh 5/3)))))
      (values (nreverse pieces) y box-w))))

(defun mtext-rich-box (data)
  "Top-left corner offset is from the attachment; returns (values WIDTH HEIGHT) of a rich MTEXT."
  (multiple-value-bind (pieces total w) (mtext-rich-layout data)
    (declare (ignore pieces))
    (values w (* total 1.1d0))))

;;; ---------- drawing on the screen: Tk measures and places each run ----------

(defun tcl-elt (s)
  "S as one Tcl list element (safe inside an enclosing {...})."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across (string s) do
      (case c
        ((#\\ #\" #\$ #\[ #\] #\{ #\}) (write-char #\\ o) (write-char c o))
        (#\Newline (write-string "\\n" o))
        (#\Return nil)
        (t (write-char c o))))
    (write-char #\" o)))

(defun run-font-spec (r px)
  (format nil "{~A} -~D~:[~; bold~]~:[~; italic~]~:[~; underline~]"
          (installed-family (font-family-for (run-font r))) (max 1 px)
          (run-bold r) (run-italic r) (run-underline r)))

(defun render-rich-mtext (data x y color tags hl)
  "One Tcl command drawing a rich MTEXT run by run (see mtrich in gui.tcl)."
  (let* ((k (/ *vscale* 0.72d0))
         (w (dxf 41 data))
         (runs (mtext-data-runs data)))
    (format nil "mtrich ~A ~A ~D ~,3F ~D {~A} {~{{~A}~^ ~}}"
            (fmtn x) (fmtn y) (or (dxf 71 data) 1) (deg (or (dxf 50 data) 0d0))
            (if (and w (plusp w)) (max 1 (round (* w *vscale*))) 0) tags
            (mapcar (lambda (r)
                      (format nil "~A {~A} ~A~@[ {~A}~]" (tcl-elt (run-text r))
                              (run-font-spec r (round (* (run-height r) k)))
                              (cond (hl "#8fc4ff") ((run-aci r) (aci->hex (run-aci r))) (t color))
                              (let ((st (run-stack r)))
                                (and st (format nil "~A ~A ~(~A~)" (tcl-elt (first st)) (tcl-elt (second st)) (third st))))))
                    runs))))

;;; ---------- the editor: runs in and out ----------

(defun runs-tcl-list (runs default-hex)
  (format nil "~{{~A}~^ ~}"
          (mapcar (lambda (r)
                    (format nil "~A ~A ~D ~D ~D ~F ~A ~A" (tcl-elt (run-text r)) (tcl-elt (run-font r))
                            (if (run-bold r) 1 0) (if (run-italic r) 1 0) (if (run-underline r) 1 0)
                            (d (run-height r)) (or (run-aci r) "{}")
                            (if (run-aci r) (aci->hex (run-aci r)) default-hex)))
                  runs)))

(defun parse-editor-runs (s)
  "The editor's runs: runs separated by RS (30), fields by GS (29)."
  (when (plusp (length s))
    (loop for part in (split-string s (code-char 30))
          for f = (split-string part (code-char 29))
          when (>= (length f) 7)
            collect (list (substitute #\Newline (code-char 28) (first f)) (second f) (string= (third f) "1") (string= (fourth f) "1")
                          (string= (fifth f) "1")
                          (let ((v (parse-number-text (sixth f)))) (if (and v (plusp v)) (d v) 0.2d0))
                          (let ((v (ignore-errors (parse-integer (seventh f))))) (and v (<= 1 v 255) v))))))
