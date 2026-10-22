;;;; spell.lisp — spell checking for text in the drawing (SPELL) and while typing (the in-place editor).
;;;; Uses a Hunspell dictionary: the system's (/usr/share/hunspell/<lang>.dic + .aff) or the en_US one
;;;; shipped in support/dict. Stems are expanded with the .aff prefix/suffix rules into a word set.
;;;; Words added with "Add to Dictionary" go to ~/.config/fancypencil/custom.dic.

(in-package :fancy-pencil)

(defvar *spell-words* nil "Hash table of known words (lower case).")
(defvar *spell-ignore* nil "Words ignored for this session (Ignore All).")
(defvar *spell-dict-name* nil)

(defparameter *drafting-words*
  '("typ" "sim" "nts" "ref" "dwg" "dwgs" "dxf" "dim" "dims" "elev" "elevs" "flr" "clg" "ceil" "ht" "hgt" "dia"
    "oc" "o.c" "eq" "ext" "int" "fin" "conc" "cmu" "gyp" "bd" "gwb" "osb" "plywd" "ply" "stl" "wd" "dbl" "hdr"
    "jst" "jsts" "tji" "lvl" "glb" "glulam" "psl" "lsl" "galv" "mtl" "alum" "approx" "min" "max" "sht" "rcp"
    "dn" "wh" "wc" "lav" "rm" "bdrm" "ba" "kit" "clo" "gar" "pwdr" "mech" "elec" "plmb" "hvac" "sf" "lf" "cf"
    "abv" "blw" "btm" "ctr" "cl" "clr" "dtl" "dtls" "ea" "exist" "existg" "fdn" "ftg" "ftgs" "fnd" "mfr" "mfg"
    "nom" "pl" "pt" "rd" "req" "reqd" "rev" "sched" "sect" "spec" "specs" "sq" "std" "str" "struct" "t&g"
    "thk" "thru" "tos" "bos" "tob" "uno" "u.n.o" "vif" "v.i.f" "w/" "w/o" "wwf" "wwm" "ga" "pvc" "abs" "ul"
    "ada" "ibc" "irc" "cbc" "nec" "ansi" "astm" "iso" "osha" "hdu" "hd" "simpson" "strongtie" "drywall"
    "sheetrock" "rebar" "subfloor" "backfill" "flashings" "soffit" "soffits" "fascia" "joist" "joists"
    "stucco" "mullion" "mullions" "casework" "millwork" "plenum" "grout" "rafters" "purlin" "purlins"
    "ridgeboard" "sheathing" "lintel" "lintels" "footings" "subgrade" "redline" "redlines" "autocad"
    "lisp" "autolisp" "dimstyle" "mtext" "polyline" "polylines" "xref" "xrefs" "layout" "layouts"))

(defun custom-dict-path ()
  (merge-pathnames ".config/fancypencil/custom.dic"
                   (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/")))

(defun spell-dict-files ()
  "(values aff dic name): the system dictionary for the user's language, else the bundled en_US."
  (let* ((lang (let ((l (or (getenv "LC_MESSAGES") (getenv "LANG") "en_US")))
                 (subseq l 0 (or (position-if (lambda (c) (member c '(#\. #\@))) l) (length l)))))
         (cands (remove-duplicates (list lang "en_US") :test #'string=)))
    (dolist (n cands)
      (dolist (dir '("/usr/share/hunspell/" "/usr/share/myspell/" "/usr/share/myspell/dicts/"))
        (let ((aff (format nil "~A~A.aff" dir n)) (dic (format nil "~A~A.dic" dir n)))
          (when (and (probe-file aff) (probe-file dic) (string-equal (subseq n 0 2) "en"))
            (return-from spell-dict-files (values aff dic n))))))
    (let ((base (merge-pathnames "../support/dict/" (or *gui-dir* (script-directory) ""))))
      (values (namestring (merge-pathnames "en_US.aff" base)) (namestring (merge-pathnames "en_US.dic" base)) "en_US"))))

;;; ---------- .aff rules ----------

(defstruct affix kind flag cross strip add cond)

(defun parse-affix-condition (s)
  "\"[^aeiou]y\" -> list of atoms: a character, :any, or (:set negate chars)."
  (if (or (null s) (string= s ".")) nil
      (let ((out nil) (i 0) (n (length s)))
        (loop while (< i n)
              do (let ((c (char s i)))
                   (cond ((char= c #\.) (push :any out) (incf i))
                         ((char= c #\[)
                          (let* ((end (or (position #\] s :start i) n))
                                 (neg (and (< (1+ i) n) (char= (char s (1+ i)) #\^)))
                                 (chars (subseq s (if neg (+ i 2) (1+ i)) end)))
                            (push (list :set neg chars) out)
                            (setf i (1+ end))))
                         (t (push c out) (incf i)))))
        (nreverse out))))

(defun atom-matches (atom ch)
  (cond ((eq atom :any) t)
        ((characterp atom) (char= atom ch))
        (t (destructuring-bind (neg chars) (rest atom)
             (if neg (not (find ch chars)) (find ch chars))))))

(defun condition-matches (atoms word suffixp)
  (let ((n (length atoms)) (len (length word)))
    (and (<= n len)
         (loop for a in atoms for k from 0
               for ch = (char word (if suffixp (+ (- len n) k) k))
               always (atom-matches a ch)))))

(defun load-affixes (path)
  (let ((rules (make-hash-table :test #'equal)))
    (with-open-file (in path :external-format :utf-8)
      (loop for line = (read-line in nil) while line
            do (let ((f (remove "" (split-string (substitute #\Space #\Tab (string-trim '(#\Return) line)) #\Space)
                                :test #'string=)))
                 (when (and (>= (length f) 4) (member (first f) '("PFX" "SFX") :test #'string=))
                   (cond
                     ;; header: PFX flag Y count
                     ((and (= (length f) 4) (member (third f) '("Y" "N") :test #'string=)
                           (every #'digit-char-p (fourth f)))
                      (setf (gethash (cons (first f) (second f)) rules)
                            (list :cross (string= (third f) "Y") :rules nil)))
                     (t
                      (let* ((key (cons (first f) (second f)))
                             (hdr (gethash key rules))
                             (add (let ((a (fourth f))) (subseq a 0 (or (position #\/ a) (length a))))))
                        (when hdr
                          (push (make-affix :kind (if (string= (first f) "PFX") :pfx :sfx) :flag (second f)
                                            :cross (getf hdr :cross)
                                            :strip (if (string= (third f) "0") "" (third f))
                                            :add (if (string= add "0") "" add)
                                            :cond (parse-affix-condition (fifth f)))
                                (getf (gethash key rules) :rules))))))))))
    rules))

(defun apply-affix (a word)
  "WORD with affix A applied, or NIL if its condition doesn't match."
  (let ((strip (affix-strip a)) (len (length word)))
    (if (eq (affix-kind a) :sfx)
        (and (condition-matches (affix-cond a) word t)
             (<= (length strip) len)
             (string= strip word :start2 (- len (length strip)))
             (concatenate 'string (subseq word 0 (- len (length strip))) (affix-add a)))
        (and (condition-matches (affix-cond a) word nil)
             (<= (length strip) len)
             (string= strip word :end2 (length strip))
             (concatenate 'string (affix-add a) (subseq word (length strip)))))))

(defun load-spell-dictionary ()
  (multiple-value-bind (aff dic name) (spell-dict-files)
    (let ((rules (load-affixes aff))
          (words (make-hash-table :test #'equal :size 300000)))
      (flet ((add (w) (setf (gethash (string-downcase w) words) t))
             (rules-for (kind flag) (getf (gethash (cons kind (string flag)) rules) :rules)))
        (with-open-file (in dic :external-format :utf-8)
          (read-line in nil)                              ; word count
          (loop for line = (read-line in nil) while line
                do (let* ((line (string-trim '(#\Return #\Space) line))
                          (tab (position #\Tab line))
                          (line (if tab (subseq line 0 tab) line))
                          (slash (position #\/ line))
                          (word (if slash (subseq line 0 slash) line))
                          (flags (if slash (subseq line (1+ slash)) "")))
                     (when (plusp (length word))
                       (add word)
                       (let ((pfx nil))
                         (loop for fl across flags
                               do (dolist (a (rules-for "PFX" fl))
                                    (let ((w (apply-affix a word))) (when w (add w) (when (affix-cross a) (push w pfx))))))
                         (loop for fl across flags
                               do (dolist (a (rules-for "SFX" fl))
                                    (let ((w (apply-affix a word)))
                                      (when w
                                        (add w)
                                        ;; cross products: prefix + suffix
                                        (when (affix-cross a)
                                          (loop for fl2 across flags
                                                do (dolist (p (rules-for "PFX" fl2))
                                                     (when (affix-cross p)
                                                       (let ((w2 (apply-affix p w))) (when w2 (add w2))))))))))))))))
        (dolist (w *drafting-words*) (add w))
        (when (probe-file (custom-dict-path))
          (with-open-file (in (custom-dict-path) :external-format :utf-8)
            (loop for l = (read-line in nil) while l
                  do (let ((w (string-trim '(#\Space #\Return #\Tab) l))) (when (plusp (length w)) (add w)))))))
      (setf *spell-words* words *spell-dict-name* name
            *spell-ignore* (or *spell-ignore* (make-hash-table :test #'equal)))
      (write-spell-cache)
      words)))

(defun ensure-spell-dictionary ()
  (or *spell-words* (load-spell-dictionary)))

(defun spell-cache-path ()
  (merge-pathnames ".cache/fancypencil/words.txt"
                   (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/")))

(defun write-spell-cache ()
  "The word list as plain text, for the in-place editor's underlining (read by the window)."
  (ignore-errors
   (let ((p (spell-cache-path)))
     (ensure-directories-exist p)
     (with-open-file (o p :direction :output :if-exists :supersede :external-format :utf-8)
       (maphash (lambda (k v) (declare (ignore v)) (write-line k o)) *spell-words*))
     p)))

(defun spell-add-word (w)
  (ensure-spell-dictionary)
  (setf (gethash (string-downcase w) *spell-words*) t)
  (let ((p (custom-dict-path)))
    (ensure-directories-exist p)
    (with-open-file (o p :direction :output :if-exists :append :if-does-not-exist :create :external-format :utf-8)
      (write-line w o))))

;;; ---------- checking ----------

(defun word-char-p (c) (or (alpha-char-p c) (char= c #\')))

(defun text-words (s)
  "List of (word start end) in S, skipping words with digits and single letters."
  (let ((out nil) (i 0) (n (length s)))
    (loop while (< i n)
          do (if (alpha-char-p (char s i))
                 (let* ((j (or (position-if-not (lambda (c) (or (word-char-p c) (digit-char-p c))) s :start i) n))
                        (w (string-right-trim "'" (subseq s i j))))
                   (when (and (> (length w) 1) (notany #'digit-char-p w))
                     (push (list w i (+ i (length w))) out))
                   (setf i j))
                 (incf i)))
    (nreverse out)))

(defun spelled-ok-p (w)
  (let ((lw (string-downcase w)))
    (or (gethash lw *spell-words*)
        (gethash lw *spell-ignore*)
        ;; possessives and simple abbreviations with a trailing period
        (and (> (length lw) 2) (string= (subseq lw (- (length lw) 2)) "'s") (gethash (subseq lw 0 (- (length lw) 2)) *spell-words*)))))

(defun misspellings (s)
  (ensure-spell-dictionary)
  (remove-if (lambda (x) (spelled-ok-p (first x))) (text-words s)))

(defun match-case (model w)
  (cond ((every (lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) model) (string-upcase w))
        ((upper-case-p (char model 0)) (string-capitalize w))
        (t w)))

(defun edits1 (w)
  (let ((letters "abcdefghijklmnopqrstuvwxyz'") (out nil) (n (length w)))
    (dotimes (i (1+ n))
      (let ((a (subseq w 0 i)) (b (subseq w i)))
        (when (plusp (length b)) (push (concatenate 'string a (subseq b 1)) out))
        (when (> (length b) 1) (push (concatenate 'string a (string (char b 1)) (string (char b 0)) (subseq b 2)) out))
        (loop for c across letters
              do (when (plusp (length b)) (push (concatenate 'string a (string c) (subseq b 1)) out))
                 (push (concatenate 'string a (string c) b) out))))
    out))

(defun anagram-p (a b)
  "Same letters (a swapped-letter typo)."
  (string= (sort (copy-seq a) #'char<) (sort (copy-seq b) #'char<)))

(defun spell-suggestions (w &optional (max 8))
  "Known words one change away (then two), closest first, in the capitalisation of W."
  (ensure-spell-dictionary)
  (let* ((lw (string-downcase w))
         (e1 (remove-duplicates (edits1 lw) :test #'string=))
         (hits1 (remove-if-not (lambda (x) (gethash x *spell-words*)) e1))
         (hits2 nil))
    (when (and (< (length hits1) max) (<= (length lw) 12))
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (x hits1) (setf (gethash x seen) t))
        (dolist (x e1)
          (dolist (y (edits1 x))
            (when (and (gethash y *spell-words*) (not (gethash y seen)) (string/= y lw))
              (setf (gethash y seen) t)
              (push y hits2))))))
    (flet ((rank (lst)
             (stable-sort (copy-list lst)
                          (lambda (a b)
                            (let ((fa (and (plusp (length a)) (char= (char a 0) (char lw 0))))
                                  (fb (and (plusp (length b)) (char= (char b 0) (char lw 0)))))
                              (cond ((and (anagram-p a lw) (not (anagram-p b lw))) t)
                                    ((and (anagram-p b lw) (not (anagram-p a lw))) nil)
                                    ((and fa (not fb)) t) ((and fb (not fa)) nil)
                                    (t (< (abs (- (length a) (length lw))) (abs (- (length b) (length lw)))))))))))
      (let ((all (append (rank hits1) (rank (nreverse hits2)))))
        (mapcar (lambda (x) (match-case w x)) (subseq all 0 (min max (length all))))))))

;;; ---------- SPELL ----------

(defun spell-target-p (e)
  (let ((d (ent-data e)))
    (or (member (ent-type e) '("TEXT" "MTEXT" "ATTDEF") :test #'equal)
        (and (equal (ent-type e) "DIMENSION") (stringp (dxf 1 d)) (plusp (length (dxf 1 d)))
             (string/= (dxf 1 d) "<>")))))

(defun replace-word-at (raw word new from)
  "Replace the first whole-word WORD in RAW at or after FROM. Returns (values string next-position)."
  (loop for p = (search word raw :start2 from) then (search word raw :start2 (1+ p))
        while p
        do (let ((before (and (> p 0) (char raw (1- p)))) (after (and (< (+ p (length word)) (length raw)) (char raw (+ p (length word))))))
             (when (and (not (and before (word-char-p before)))
                        (not (and after (word-char-p after))))
               (return-from replace-word-at
                 (values (concatenate 'string (subseq raw 0 p) new (subseq raw (+ p (length word))))
                         (+ p (length new)))))))
  (values raw from))

(defun show-entity-on-screen (e)
  (when (gui-p)
    (let ((b (entity-bbox e)))
      (when b
        (multiple-value-bind (x0 y0 x1 y1) (view-rect 0)
          (let ((cx (/ (+ (first b) (third b)) 2)) (cy (/ (+ (second b) (fourth b)) 2)))
            (when (or (< cx x0) (> cx x1) (< cy y0) (> cy y1))
              (setf *vx* cx *vy* cy))))
        (sweep-highlights)
        (gui-highlight (list e) t)
        (gui-regen)))))

(defcmd "SPELL" ("SP") "Checks the spelling of text in the drawing."
  (let* ((sel (select-objects :prompt "Select text to check, or press Enter to check the whole drawing: " :editable nil))
         (ents (or sel (live-entities)))
         (targets (remove-if-not (lambda (e) (and (spell-target-p e) (layer-visible-p (find-layer (ent-layer e))))) ents))
         (change-all (make-hash-table :test #'equal))
         (found 0) (changed 0))
    (ensure-spell-dictionary)
    (out (format nil "Checking ~D text object~:P (~A dictionary)...~%" (length targets) *spell-dict-name*))
    (block checking
      (dolist (e targets)
        (let ((pos 0))
          (dolist (m (misspellings (text-string-display (or (dxf 1 (ent-data e)) ""))))
            (let ((w (first m)))
              (unless (spelled-ok-p w)            ; may have been added/ignored meanwhile
                (incf found)
                (let ((auto (gethash (string-downcase w) change-all)))
                  (if auto
                      (multiple-value-bind (s np) (replace-word-at (dxf 1 (ent-data e)) w (match-case w auto) pos)
                        (set-entity-data e (set-dxf 1 s (ent-data e))) (setf pos np) (incf changed))
                      (if (not (use-inplace-editor-p))
                          (out (format nil "  ~A  (in \"~A\")  suggestions: ~{~A~^, ~}~%" w
                                       (text-string-display (dxf 1 (ent-data e))) (spell-suggestions w 5)))
                          (progn
                            (show-entity-on-screen e)
                            (let* ((r (tk-ask (format nil "spelldlg ~A ~A ~A ~A" (tq w)
                                                      (tq (text-string-display (dxf 1 (ent-data e))))
                                                      (tcl-list (spell-suggestions w)) (tq *spell-dict-name*))))
                                   (f (split-string r (code-char 31)))
                                   (action (first f)) (new (or (second f) "")))
                              (cond
                                ((or (string= r "") (string= action "CANCEL")) (return-from checking))
                                ((member action '("CHANGE" "CHANGEALL") :test #'string=)
                                 (when (plusp (length (string-trim " " new)))
                                   (when (string= action "CHANGEALL") (setf (gethash (string-downcase w) change-all) new))
                                   (multiple-value-bind (s np) (replace-word-at (dxf 1 (ent-data e)) w new pos)
                                     (set-entity-data e (set-dxf 1 s (ent-data e))) (setf pos np) (incf changed))))
                                ((string= action "IGNOREALL") (setf (gethash (string-downcase w) *spell-ignore*) t))
                                ((string= action "ADD") (spell-add-word w) (write-spell-cache))
                                (t nil)))))))))))))
    (when (gui-p) (sweep-highlights) (gui-regen))
    (out (format nil "Spelling check complete: ~D word~:P not found, ~D changed.~%" found changed))))
