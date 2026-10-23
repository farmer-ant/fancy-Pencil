;;;; dims.lisp — dimension styles and dimensions: linear, aligned, radius, diameter, angular,
;;;; continue and baseline. Dimension geometry lives in an anonymous *D block that is rebuilt
;;;; from the definition points whenever the dimension changes, as in AutoCAD.

(in-package :fancy-pencil)

;;; ---------- dimension variables & styles ----------

(defparameter *dimvar-names*
  '(:dimscale :dimasz :dimtxt :dimexo :dimexe :dimgap :dimdec :dimtad :dimtsz :dimclrd :dimclre
    :dimclrt :dimlfac :dimpost :dimcen :dimdli :dimtih :dimadec :dimblk))

(defstruct dimstyle name (vars nil))

(defun current-dimvars ()
  (loop for k in *dimvar-names*
        append (list k (sysvar (symbol-name k)))))

(defun standard-dimvars ()
  (list :dimscale 1d0 :dimasz 0.18d0 :dimtxt 0.18d0 :dimexo 0.0625d0 :dimexe 0.18d0 :dimgap 0.09d0
        :dimdec 4 :dimtad 1 :dimtsz 0d0 :dimclrd 0 :dimclre 0 :dimclrt 0 :dimlfac 1d0 :dimpost ""
        :dimcen 0.09d0 :dimdli 0.38d0 :dimtih 0 :dimadec 0 :dimblk ""))

(defun all-dimstyles ()
  (or (drawing-dimstyles *dwg*)
      (setf (drawing-dimstyles *dwg*) (list (make-dimstyle :name "Standard" :vars (standard-dimvars))))))

(defun find-dimstyle (name)
  (and (stringp name) (find name (all-dimstyles) :key #'dimstyle-name :test #'string-equal)))

(defun apply-dimvars (vars)
  (loop for (k v) on vars by #'cddr
        when (member k *dimvar-names*) do (setf (sysvar (symbol-name k)) v)))

(defun dv (vars key) (getf vars key (getf (standard-dimvars) key)))

;;; ---------- shared drawing helpers ----------

(defvar *dim-counter* 0)
(defun unique-dim-block-name ()
  (loop for n = (format nil "*D~D" (incf *dim-counter*)) unless (find-block n) return n))

(defun colored (data c) (if (and c (/= c 0)) (append data (list (cons 62 c))) data))

(defun dim-text-string (meas vars override &key (kind :linear))
  (let* ((val (* meas (d (dv vars :dimlfac))))
         (num (if (eq kind :angular)
                  (format nil "~A%%d" (fmt-decimal (deg meas) (dv vars :dimadec)))
                  (rtos val (or (sysvar "LUNITS") 2) (dv vars :dimdec))))
         (num (case kind (:radius (concatenate 'string "R" num)) (:diameter (concatenate 'string "%%c" num)) (t num)))
         (post (dv vars :dimpost))
         (num (cond ((or (null post) (string= post "")) num)
                    ((search "<>" post) (let ((p (search "<>" post))) (concatenate 'string (subseq post 0 p) num (subseq post (+ p 2)))))
                    (t (concatenate 'string num post)))))
    (cond ((or (null override) (string= override "")) num)
          ((search "<>" override)
           (let ((p (search "<>" override))) (concatenate 'string (subseq override 0 p) num (subseq override (+ p 2)))))
          (t override))))

(defparameter *arrowheads*
  '(("" "Closed filled") ("CLOSEDBLANK" "Closed blank") ("OPEN" "Open") ("OPEN90" "Right angle")
    ("DOT" "Dot") ("DOTSMALL" "Dot small") ("ARCHTICK" "Architectural tick") ("OBLIQUE" "Oblique")
    ("NONE" "None"))
  "DIMBLK values (AutoCAD's names without the leading underscore) and their names in the dialog.")

(defparameter *arrow-block-names*
  '(("CLOSEDBLANK" . "_ClosedBlank") ("OPEN" . "_Open") ("OPEN90" . "_Open90") ("DOT" . "_Dot")
    ("DOTSMALL" . "_DotSmall") ("ARCHTICK" . "_ArchTick") ("OBLIQUE" . "_Oblique") ("NONE" . "_None"))
  "Arrowhead codes and the block names AutoCAD uses for them in drawing files.")

(defun arrow-block-name (code) (cdr (assoc (arrow-code code) *arrow-block-names* :test #'string=)))

(defun known-arrow-name-p (name)
  "True for a DIMBLK block name we can draw (\"_ArchTick\", \"ArchTick\", \"_Closed\"...)."
  (let ((c (arrow-code name)))
    (or (string= c "") (assoc c *arrow-block-names* :test #'string=))))

(defun arrow-block-entities (code)
  "Entities of the arrowhead block for CODE, AutoCAD-style: arrow size 1, tip at the origin, drawn
for the right-hand end of a horizontal dimension line, colour ByBlock."
  (dim-arrow (pt 0 0) (pt -1 0) (list :dimscale 1d0 :dimasz 1d0 :dimtsz 0d0 :dimclrd 0 :dimblk (arrow-code code))))

(defun arrow-codes-in-use ()
  "Arrowhead codes (other than the default) used by any dimension style or the current settings."
  (remove-duplicates
   (remove "" (mapcar #'arrow-code (cons (sysvar "DIMBLK") (mapcar (lambda (ds) (dv (dimstyle-vars ds) :dimblk)) (all-dimstyles))))
           :test #'string=)
   :test #'string=))

(defun arrow-code (v)
  "Normalise a DIMBLK value: \"_ArchTick\" -> \"ARCHTICK\", \"CLOSED\" -> \"\"."
  (let ((u (string-upcase (string-left-trim "_" (or v "")))))
    (if (member u '("CLOSED" "CLOSEDFILLED") :test #'string=) "" u)))

(defun dim-arrow (tip toward vars)
  "Arrowhead (or tick) at TIP, for a dimension line running from TIP toward TOWARD."
  (let* ((sc (d (dv vars :dimscale))) (tsz (* sc (d (dv vars :dimtsz)))) (asz (* sc (d (dv vars :dimasz))))
         (clr (dv vars :dimclrd)) (a (ang tip toward))
         (kind (arrow-code (dv vars :dimblk))))
    (flet ((quad (p1 p2 p3 p4)          ; filled 4-sided shape, corners in order
             (colored (list (cons 0 "SOLID") (cons 10 (pt3 p1)) (cons 11 (pt3 p2)) (cons 12 (pt3 p4)) (cons 13 (pt3 p3))) clr))
           (seg (p q) (colored (line-data p q) clr)))
      (cond
        ((> tsz 0)                       ; DIMTSZ: oblique ticks instead of arrows (as in AutoCAD)
         (list (seg (polar tip (+ a (/ +pi+ 4)) tsz) (polar tip (+ a (* 1.25 +pi+)) tsz))))
        ((string= kind "ARCHTICK")       ; heavy 45-degree stroke through the tip
         (let* ((h (/ asz 2)) (w (* asz 0.08d0)) (da (+ a (/ +pi+ 4))) (pa (+ da (/ +pi+ 2)))
                (p (polar tip da h)) (q (polar tip (+ da +pi+) h)))
           (list (quad (polar p pa w) (polar q pa w) (polar q (+ pa +pi+) w) (polar p (+ pa +pi+) w)))))
        ((string= kind "OBLIQUE")
         (let ((h (/ asz 2)) (da (+ a (/ +pi+ 4))))
           (list (seg (polar tip da h) (polar tip (+ da +pi+) h)))))
        ((string= kind "NONE") nil)
        ((member kind '("DOT" "DOTSMALL") :test #'string=)
         (let ((r (if (string= kind "DOT") (/ asz 4) (/ asz 8))))
           (list (colored (list (cons 0 "SOLID") (cons 10 (polar tip 0 r)) (cons 11 (polar tip (/ +pi+ 2) r))
                                (cons 12 (polar tip (* 1.5 +pi+) r)) (cons 13 (polar tip +pi+ r))) clr))))
        ((string= kind "OPEN")
         (let ((b (polar tip a asz)) (wdt (/ asz 6)))
           (list (seg tip (polar b (+ a (/ +pi+ 2)) wdt)) (seg tip (polar b (- a (/ +pi+ 2)) wdt)))))
        ((string= kind "OPEN90")
         (let ((b (polar tip a (/ asz 2))))
           (list (seg tip (polar b (+ a (/ +pi+ 2)) (/ asz 2))) (seg tip (polar b (- a (/ +pi+ 2)) (/ asz 2))))))
        ((string= kind "CLOSEDBLANK")
         (let* ((b (polar tip a asz)) (wdt (/ asz 6))
                (l (polar b (+ a (/ +pi+ 2)) wdt)) (r (polar b (- a (/ +pi+ 2)) wdt)))
           (list (seg tip l) (seg l r) (seg r tip))))
        (t (let* ((b (polar tip a asz)) (wdt (/ asz 6)))
             (list (colored (list (cons 0 "SOLID") (cons 10 (pt3 tip)) (cons 11 (polar b (+ a (/ +pi+ 2)) wdt))
                                  (cons 12 (polar b (- a (/ +pi+ 2)) wdt)) (cons 13 (polar b (- a (/ +pi+ 2)) wdt)))
                            clr))))))))

(defun readable-angle (a)
  (let ((a (norm-ang a))) (if (and (> a (/ +pi+ 2)) (<= a (* 1.5 +pi+))) (norm-ang (+ a +pi+)) a)))

(defun dim-text-entity (pos rot s vars attach)
  (colored (list (cons 0 "MTEXT") (cons 10 (pt3 pos)) (cons 40 (* (d (dv vars :dimscale)) (d (dv vars :dimtxt))))
                 (cons 71 attach) (cons 1 s) (cons 50 (d rot)))
           (dv vars :dimclrt)))

(defun text-width-est (s vars)
  (* 0.8d0 (d (dv vars :dimscale)) (d (dv vars :dimtxt)) (length (text-string-display s))))

;;; ---------- linear / aligned ----------

(defun linear-dim-geometry (p1 p2 loc theta override vars)
  (let* ((sc (d (dv vars :dimscale)))
         (asz (* sc (d (dv vars :dimasz)))) (txt (* sc (d (dv vars :dimtxt))))
         (exo (* sc (d (dv vars :dimexo)))) (exe (* sc (d (dv vars :dimexe)))) (gap (* sc (d (dv vars :dimgap))))
         (u (pt (cos theta) (sin theta))) (n (pt (- (sin theta)) (cos theta)))
         (dot (lambda (a b) (+ (* (px a) (px b)) (* (py a) (py b)))))
         (d1 (v+ p1 (v* n (funcall dot (v- loc p1) n))))
         (d2 (v+ p2 (v* n (funcall dot (v- loc p2) n))))
         (meas (abs (funcall dot (v- p2 p1) u)))
         (s (dim-text-string meas vars override))
         (ents nil))
    (flet ((ext (p dd)
             (when (> (dist2d p dd) exo)
               (let ((dir (ang p dd)))
                 (push (colored (line-data (polar p dir exo) (polar dd dir exe)) (dv vars :dimclre)) ents)))))
      (ext p1 d1) (ext p2 d2)
      (let* ((len (dist2d d1 d2))
             (tr (readable-angle theta))
             (mid (midpt d1 d2))
             (tw (text-width-est s vars))
             (inside (> len (+ (* 2.2 asz) (if (eql (dv vars :dimtad) 0) (+ tw (* 2 gap)) 0)))))
        ;; dimension line (broken around centered text when DIMTAD = 0)
        (if (and (eql (dv vars :dimtad) 0) inside)
            (let ((a (ang d1 d2)) (half (+ (/ tw 2) gap)))
              (push (colored (line-data d1 (polar mid (+ a +pi+) half)) (dv vars :dimclrd)) ents)
              (push (colored (line-data (polar mid a half) d2) (dv vars :dimclrd)) ents))
            (push (colored (line-data d1 d2) (dv vars :dimclrd)) ents))
        (when (> len 1d-9)
          (if inside
              (setf ents (append (dim-arrow d1 d2 vars) (dim-arrow d2 d1 vars) ents))
              ;; not enough room: arrows outside pointing in, with short tails
              (let ((a (ang d1 d2)))
                (push (colored (line-data (polar d1 (+ a +pi+) (* 2 asz)) d1) (dv vars :dimclrd)) ents)
                (push (colored (line-data d2 (polar d2 a (* 2 asz))) (dv vars :dimclrd)) ents)
                (setf ents (append (dim-arrow d1 (polar d1 (+ a +pi+) 1) vars)
                                   (dim-arrow d2 (polar d2 a 1) vars) ents)))))
        (let* ((up (polar (pt 0 0) (+ tr (/ +pi+ 2)) 1))
               (tp (if (eql (dv vars :dimtad) 0) mid (v+ mid (v* up gap)))))
          (push (dim-text-entity tp tr s vars (if (eql (dv vars :dimtad) 0) 5 8)) ents)
          (values (nreverse ents) meas d2 tp))))))

;;; ---------- radius / diameter ----------

(defun radial-dim-geometry (c r loc diameter override vars)
  (let* ((sc (d (dv vars :dimscale))) (gap (* sc (d (dv vars :dimgap)))) (cen (* sc (d (dv vars :dimcen))))
         (a (if (< (dist2d c loc) 1d-9) 0d0 (ang c loc)))
         (q (polar c a r)) (opp (polar c (+ a +pi+) r))
         (meas (if diameter (* 2 r) r))
         (s (dim-text-string meas vars override :kind (if diameter :diameter :radius)))
         (outside (> (dist2d c loc) r))
         (tr (readable-angle a))
         (ents nil))
    (declare (ignorable tr))
    (if outside
        (progn (push (colored (line-data q loc) (dv vars :dimclrd)) ents)
               (setf ents (append (dim-arrow q c vars) ents))
               (when diameter
                 (push (colored (line-data opp (polar opp (+ a +pi+) (* 2 sc (d (dv vars :dimasz))))) (dv vars :dimclrd)) ents)
                 (setf ents (append (dim-arrow opp c vars) ents))))
        (progn (push (colored (line-data (if diameter opp c) q) (dv vars :dimclrd)) ents)
               (setf ents (append (dim-arrow q c vars) ents))
               (when diameter (setf ents (append (dim-arrow opp q vars) ents)))))
    ;; center mark
    (when (and (> cen 0) (not diameter))
      (push (colored (line-data (polar c 0 cen) (polar c +pi+ cen)) (dv vars :dimclrd)) ents)
      (push (colored (line-data (polar c (/ +pi+ 2) cen) (polar c (* 1.5 +pi+) cen)) (dv vars :dimclrd)) ents))
    (if outside
        ;; text outside: horizontal, on a short landing (AutoCAD's default DIMTOH behaviour)
        (let* ((right (>= (cos a) 0)) (land (* 1.5 sc (d (dv vars :dimasz))))
               (lend (polar loc (if right 0d0 +pi+) land)))
          (push (colored (line-data loc lend) (dv vars :dimclrd)) ents)
          (push (dim-text-entity (polar lend (if right 0d0 +pi+) gap) 0d0 s vars (if right 4 6)) ents))
        (push (dim-text-entity (polar loc (+ tr (/ +pi+ 2)) gap) tr s vars 8) ents))
    (values (nreverse ents) meas q opp)))

;;; ---------- angular ----------

(defun angular-dim-geometry (vertex pa pb loc override vars)
  "Angle at VERTEX between rays to PA and PB; the arc passes through LOC."
  (let* ((sc (d (dv vars :dimscale))) (gap (* sc (d (dv vars :dimgap))))
         (exo (* sc (d (dv vars :dimexo)))) (exe (* sc (d (dv vars :dimexe))))
         (a1 (ang vertex pa)) (a2 (ang vertex pb)) (al (ang vertex loc))
         (r (max 1d-6 (dist2d vertex loc)))
         ;; choose the pair of rays (possibly opposite rays) whose sector contains LOC
         (cands (list (list a1 a2) (list a2 a1)
                      (list (norm-ang (+ a1 +pi+)) a2) (list a2 (norm-ang (+ a1 +pi+)))
                      (list a1 (norm-ang (+ a2 +pi+))) (list (norm-ang (+ a2 +pi+)) a1)))
         (best (or (find-if (lambda (c) (and (ang-in-arc-p al (first c) (second c))
                                             (< (arc-sweep (first c) (second c)) +pi+)))
                            cands)
                   (list a1 a2)))
         (s1 (first best)) (s2 (second best))
         (meas (arc-sweep s1 s2))
         (s (dim-text-string meas vars override :kind :angular))
         (ents nil))
    (push (colored (arc-data vertex r s1 s2) (dv vars :dimclrd)) ents)
    (let ((e1 (polar vertex s1 r)) (e2 (polar vertex s2 r)))
      (setf ents (append (dim-arrow e1 (polar vertex (+ s1 0.2d0) r) vars)
                         (dim-arrow e2 (polar vertex (- s2 0.2d0) r) vars) ents))
      ;; extension lines when the arc lies beyond the defining points
      (dolist (pr (list (list s1 e1 pa pb) (list s2 e2 pb pa)))
        (destructuring-bind (sa e p q) pr
          (declare (ignore q))
          (let ((near (if (< (abs (- (norm-ang (- (ang vertex p) sa)) 0)) 1d-6) p vertex)))
            (when (> (dist2d near e) exo)
              (push (colored (line-data (polar near sa exo) (polar e sa exe)) (dv vars :dimclre)) ents))))))
    (let* ((mid (+ s1 (/ meas 2))) (tp (polar vertex mid (+ r gap))) (tr (readable-angle (- mid (/ +pi+ 2)))))
      (push (dim-text-entity tp tr s vars 8) ents)
      (values (nreverse ents) meas tp))))

;;; ---------- rebuilding the block of a dimension ----------

(defun dimension-vars (data)
  (or (dxf :dimvars data)
      (let ((ds (find-dimstyle (dxf 3 data)))) (and ds (dimstyle-vars ds)))
      (current-dimvars)))

(defun maybe-rebuild-dimension (data)
  "Regenerate the anonymous block of a dimension we know how to draw."
  (let* ((type (logand (or (dxf 70 data) 0) 7))
         (vars (dimension-vars data))
         (ents
           (ignore-errors
            (case type
              ((0 1)
               (when (and (dxf 13 data) (dxf 14 data) (dxf 10 data))
                 (let* ((p1 (pt3 (dxf 13 data))) (p2 (pt3 (dxf 14 data)))
                        (theta (if (= type 1) (ang p1 p2) (or (dxf 50 data) 0d0))))
                   (multiple-value-bind (es meas) (linear-dim-geometry p1 p2 (pt3 (dxf 10 data)) theta (dxf 1 data) vars)
                     (setf data (set-dxf 42 meas data))
                     es))))
              ((3 4)
               (when (and (dxf 15 data) (dxf 10 data) (dxf 11 data))
                 (let* ((q (pt3 (dxf 15 data)))
                        (c (if (= type 4) (pt3 (dxf 10 data)) (midpt q (pt3 (dxf 10 data)))))
                        (r (dist2d c q)))
                   (multiple-value-bind (es meas) (radial-dim-geometry c r (pt3 (dxf 11 data)) (= type 3) (dxf 1 data) vars)
                     (setf data (set-dxf 42 meas data))
                     es))))
              (2
               (when (and (dxf 13 data) (dxf 14 data) (dxf 15 data) (dxf 10 data) (dxf 16 data))
                 (let ((v (line-line-int (pt3 (dxf 13 data)) (pt3 (dxf 14 data)) (pt3 (dxf 15 data)) (pt3 (dxf 10 data)) nil)))
                   (when v
                     (let ((pa (if (> (dist2d v (pt3 (dxf 14 data))) (dist2d v (pt3 (dxf 13 data)))) (pt3 (dxf 14 data)) (pt3 (dxf 13 data))))
                           (pb (if (> (dist2d v (pt3 (dxf 10 data))) (dist2d v (pt3 (dxf 15 data)))) (pt3 (dxf 10 data)) (pt3 (dxf 15 data)))))
                       (multiple-value-bind (es meas) (angular-dim-geometry v pa pb (pt3 (dxf 16 data)) (dxf 1 data) vars)
                         (setf data (set-dxf 42 meas data))
                         es))))))
              (5
               (when (and (dxf 13 data) (dxf 14 data) (dxf 15 data) (dxf 10 data))
                 (multiple-value-bind (es meas)
                     (angular-dim-geometry (pt3 (dxf 15 data)) (pt3 (dxf 13 data)) (pt3 (dxf 14 data)) (pt3 (dxf 10 data)) (dxf 1 data) vars)
                   (setf data (set-dxf 42 meas data))
                   es)))))))
    ;; text the user dragged elsewhere (flag 128) stays where it was put; 11 is its middle point
    (when (and ents (logtest 128 (or (dxf 70 data) 0)) (dxf 11 data))
      (setf ents (mapcar (lambda (e)
                           (if (equal (dxf 0 e) "MTEXT")
                               (set-dxf 71 5 (set-dxf 10 (pt3 (dxf 11 data)) e))
                               e))
                         ents)))
    (if ents
        (let ((name (unique-dim-block-name)))
          (push (make-block-def :name name :base (pt 0 0) :entities ents) (drawing-blocks *dwg*))
          (set-dxf 2 name data))
        data)))

(defun dim-data (type &rest pairs)
  (append (list (cons 0 "DIMENSION") (cons 2 "*D") (cons 70 (logior 32 type)) (cons 1 "")
                (cons 3 (sysvar "DIMSTYLE")) (cons :dimvars (current-dimvars)))
          pairs))

;;; ---------- commands ----------

(defvar *last-dim* nil "Last linear dimension created (for DIMCONTINUE / DIMBASELINE).")
(defvar *dim-p2* nil)

(defun auto-linear-theta (p1 p2 loc)
  (let ((m (midpt p1 p2)))
    (if (or (> (px loc) (max (px p1) (px p2))) (< (px loc) (min (px p1) (px p2))))
        (if (and (<= (min (py p1) (py p2)) (py loc) (max (py p1) (py p2))))
            (/ +pi+ 2)
            (if (> (abs (- (px loc) (px m))) (abs (- (py loc) (py m)))) (/ +pi+ 2) 0d0))
        0d0)))

(defun dim-command (aligned)
  (let ((p1 (gpoint "Specify first extension line origin or <select object>: ")))
    (when (null p1)
      (let ((e (sel-entity (get-user-input :entity :prompt "Select object to dimension: "))))
        (when e
          (let ((data (ent-data e)))
            (cond ((equal (dxf 0 data) "LINE") (setf p1 (pt3 (dxf 10 data)) *dim-p2* (pt3 (dxf 11 data))))
                  ((member (dxf 0 data) '("CIRCLE" "ARC") :test #'equal)
                   (let ((c (pt3 (dxf 10 data))) (r (dxf 40 data)))
                     (if (equal (dxf 0 data) "ARC")
                         (setf p1 (polar c (dxf 50 data) r) *dim-p2* (polar c (dxf 51 data) r))
                         (setf p1 (polar c +pi+ r) *dim-p2* (polar c 0 r))))))))))
    (when p1
      (let* ((p2 (or (shiftf *dim-p2* nil) (gpoint "Specify second extension line origin: " :base p1)))
             (theta-fn (lambda (loc) (if aligned (ang p1 p2) (auto-linear-theta p1 p2 loc))))
             (text nil) (loc nil) (vars (current-dimvars)))
        (when (pointp p2)
          (setf *dimline-snap* (lambda (c) (funcall theta-fn c)) *dimline-exclude* nil)
          (unwind-protect
          (loop
            (let ((r (gpoint (if aligned "Specify dimension line location or [Text/Angle]: "
                                 "Specify dimension line location or [Text/Horizontal/Vertical/Rotated]: ")
                             :keywords (if aligned '("Text" "Angle") '("Text" "Horizontal" "Vertical" "Rotated"))
                             :drag (lambda (cur) (mapcar (lambda (e) (list :data e))
                                                         (linear-dim-geometry p1 p2 cur (funcall theta-fn cur) text vars))))))
              (cond ((equal r "Text") (setf text (gstring "Enter dimension text <>: " t)))
                    ((equal r "Horizontal") (setf theta-fn (constantly 0d0)))
                    ((equal r "Vertical") (setf theta-fn (constantly (/ +pi+ 2))))
                    ((equal r "Rotated") (let ((a (or (gangle "Specify angle of dimension line <0>: ") 0d0)))
                                           (setf theta-fn (constantly a))))
                    ((pointp r) (setf loc r) (return))
                    (t (return)))))
            (setf *dimline-snap* nil *dimline-match* nil))
          (when loc
            (let ((theta (funcall theta-fn loc)))
              (multiple-value-bind (ents meas d2 tp) (linear-dim-geometry p1 p2 loc theta text vars)
                (declare (ignore ents))
                (let ((e (add (dim-data (if aligned 1 0) (cons 10 d2) (cons 11 tp) (cons 13 (pt3 p1)) (cons 14 (pt3 p2))
                                        (cons 50 (d theta)) (cons 42 meas)))))
                  (when text (set-entity-data e (set-dxf 1 text (ent-data e))))
                  (setf *last-dim* e))
                (out (format nil "Dimension text = ~A~%" (fmt-len meas)))))))))))

(defcmd "DIMLINEAR" ("DLI" "DIMLIN") "Creates a horizontal or vertical linear dimension." (dim-command nil))
(defcmd "DIMALIGNED" ("DAL" "DIMALI") "Creates an aligned linear dimension." (dim-command t))

(defun pick-circular (prompt)
  (loop
    (let* ((sel (get-user-input :entity :prompt prompt)))
      (when (null sel) (return nil))
      (let ((e (sel-entity sel)))
        (if (and e (member (ent-type e) '("CIRCLE" "ARC") :test #'equal))
            (return e)
            (out (format nil "Object selected is not a circle or arc.~%")))))))

(defun radial-command (diameter)
  (let ((e (pick-circular (if diameter "Select arc or circle: " "Select arc or circle: "))))
    (when e
      (let* ((data (ent-data e)) (c (pt3 (dxf 10 data))) (r (dxf 40 data)) (vars (current-dimvars)) (text nil))
        (out (format nil "Dimension text = ~A~%" (fmt-len (if diameter (* 2 r) r))))
        (loop
          (let ((loc (gpoint "Specify dimension line location or [Text/Angle]: " :keywords '("Text" "Angle")
                             :drag (lambda (cur) (mapcar (lambda (x) (list :data x))
                                                         (radial-dim-geometry c r cur diameter text vars))))))
            (cond ((equal loc "Text") (setf text (gstring "Enter dimension text <>: " t)))
                  ((equal loc "Angle") (gangle "Specify angle of dimension text: "))
                  ((pointp loc)
                   (multiple-value-bind (ents meas q opp) (radial-dim-geometry c r loc diameter text vars)
                     (declare (ignore ents))
                     (add (append (dim-data (if diameter 3 4) (cons 10 (if diameter opp c)) (cons 11 (pt3 loc)) (cons 15 q)
                                            (cons 40 (d (if diameter (* 2 r) (max 0d0 (- (dist2d c loc) r))))) (cons 42 meas))
                                  (if text (list (cons 1 text)) nil))))
                   (return))
                  (t (return)))))))))

(defcmd "DIMRADIUS" ("DRA" "DIMRAD") "Creates a radius dimension for a circle or arc." (radial-command nil))
(defcmd "DIMDIAMETER" ("DDI" "DIMDIA") "Creates a diameter dimension for a circle or arc." (radial-command t))

(defcmd "DIMANGULAR" ("DAN" "DIMANG") "Creates an angular dimension."
  (let* ((sel (get-user-input :entity :prompt "Select arc, circle, line, or <specify vertex>: "))
         (vars (current-dimvars)) (text nil)
         (geom nil))
    (cond
      ((null sel)
       (let* ((v (gpoint "Specify angle vertex: "))
              (a (and v (gpoint "Specify first angle endpoint: " :base v)))
              (b (and a (gpoint "Specify second angle endpoint: " :base v))))
         (when (pointp b) (setf geom (list :three v a b)))))
      (t (let ((e (sel-entity sel)))
           (cond ((null e) nil)
                 ((equal (ent-type e) "ARC")
                  (let* ((d0 (ent-data e)) (c (pt3 (dxf 10 d0))) (r (dxf 40 d0)))
                    (setf geom (list :three c (polar c (dxf 50 d0) r) (polar c (dxf 51 d0) r)))))
                 ((equal (ent-type e) "CIRCLE")
                  (let* ((d0 (ent-data e)) (c (pt3 (dxf 10 d0))))
                    (let ((b (gpoint "Specify second angle endpoint: " :base c)))
                      (when (pointp b) (setf geom (list :three c (sel-point sel) b))))))
                 ((equal (ent-type e) "LINE")
                  (let ((e2 (sel-entity (get-user-input :entity :prompt "Select second line: "))))
                    (when (and e2 (equal (ent-type e2) "LINE"))
                      (let ((d1 (ent-data e)) (d2 (ent-data e2)))
                        (setf geom (list :lines (pt3 (dxf 10 d1)) (pt3 (dxf 11 d1)) (pt3 (dxf 10 d2)) (pt3 (dxf 11 d2))))))))
                 (t (out (format nil "Object selected cannot be dimensioned with angular dimensions.~%")))))))
    (when geom
      (multiple-value-bind (vertex pa pb)
          (if (eq (first geom) :three)
              (values (second geom) (third geom) (fourth geom))
              (destructuring-bind (a b c dd) (rest geom)
                (let ((v (line-line-int a b c dd nil)))
                  (unless v (aerr "Lines are parallel."))
                  (values v (if (> (dist2d v b) (dist2d v a)) b a) (if (> (dist2d v dd) (dist2d v c)) dd c)))))
        (loop
          (let ((loc (gpoint "Specify dimension arc line location or [Text]: " :keywords '("Text")
                             :drag (lambda (cur) (mapcar (lambda (x) (list :data x))
                                                         (angular-dim-geometry vertex pa pb cur text vars))))))
            (cond ((equal loc "Text") (setf text (gstring "Enter dimension text <>: " t)))
                  ((pointp loc)
                   (multiple-value-bind (ents meas tp) (angular-dim-geometry vertex pa pb loc text vars)
                     (declare (ignore ents))
                     (add (append (if (eq (first geom) :lines)
                                      (destructuring-bind (a b c dd) (rest geom)
                                        (dim-data 2 (cons 13 a) (cons 14 b) (cons 15 c) (cons 10 dd) (cons 16 (pt3 loc))
                                                  (cons 11 tp) (cons 42 meas)))
                                      (dim-data 5 (cons 15 vertex) (cons 13 pa) (cons 14 pb) (cons 10 (pt3 loc))
                                                (cons 11 tp) (cons 42 meas)))
                                  (if text (list (cons 1 text)) nil)))
                     (out (format nil "Dimension text = ~A~%" (fmt-decimal (deg meas) (dv vars :dimadec)))))
                   (return))
                  (t (return)))))))))

(defun chain-dims (baseline)
  (let ((prev (and *last-dim* (ent-live-p *last-dim*) *last-dim*)))
    (unless prev
      (let ((e (sel-entity (get-user-input :entity :prompt (if baseline "Select base dimension: " "Select continued dimension: ")))))
        (if (and e (equal (ent-type e) "DIMENSION") (member (logand (or (dxf 70 (ent-data e)) 0) 7) '(0 1)))
            (setf prev e)
            (progn (out (format nil "Select a linear or aligned dimension.~%")) (return-from chain-dims nil)))))
    (loop
      (let* ((pd (ent-data prev))
             (type (logand (or (dxf 70 pd) 0) 7))
             (p1 (pt3 (dxf 13 pd))) (p2 (pt3 (dxf 14 pd))) (loc (pt3 (dxf 10 pd)))
             (theta (if (= type 1) (ang p1 p2) (or (dxf 50 pd) 0d0)))
             (vars (dimension-vars pd))
             (n (pt (- (sin theta)) (cos theta)))
             (side (if (>= (+ (* (- (px loc) (px p1)) (px n)) (* (- (py loc) (py p1)) (py n))) 0) 1 -1))
             (start (if baseline p1 p2))
             (newloc (if baseline (v+ loc (v* n (* side (d (dv vars :dimscale)) (d (dv vars :dimdli))))) loc))
             (q (gpoint "Specify a second extension line origin or [Undo] <Select>: " :base start :keywords '("Undo")
                        :drag (lambda (cur) (mapcar (lambda (x) (list :data x))
                                                    (linear-dim-geometry start cur newloc theta nil vars))))))
        (cond ((null q) (return))
              ((equal q "Undo") (return))
              ((pointp q)
               (multiple-value-bind (ents meas d2 tp) (linear-dim-geometry start q newloc theta nil vars)
                 (declare (ignore ents))
                 (setf prev (add (dim-data type (cons 10 d2) (cons 11 tp) (cons 13 start) (cons 14 q)
                                           (cons 50 (d theta)) (cons 42 meas)))
                       *last-dim* prev)
                 (out (format nil "Dimension text = ~A~%" (fmt-len meas))))))))))

(defcmd "DIMCONTINUE" ("DCO" "DIMCONT") "Creates a dimension starting at the second extension line of the last one." (chain-dims nil))
(defcmd "DIMBASELINE" ("DBA" "DIMBASE") "Creates a dimension from the baseline of the last one." (chain-dims t))

(defun dimvar-rows ()
  (loop for k in *dimvar-names* collect (list (symbol-name k) (sysvar (symbol-name k)))))

(defcmd "DIMSTYLE" ("D" "DST" "DDIM" "-DIMSTYLE" "DIMSTY") "Creates and restores dimension styles."
  (if (and (gui-p) (not (from-lisp-p)))
      (dimstyle-dialog)
      (dimstyle-command-line)))

(defun dimstyle-command-line ()
  (out (format nil "Current dimension style: ~A~%" (sysvar "DIMSTYLE")))
  (let ((k (gkw "Enter a dimension style option [Save/Restore/STatus/Variables/Apply/?] <Restore>: "
                '("Save" "Restore" "STatus" "Variables" "Apply" "?") "Restore")))
    (cond
      ((equal k "Save")
       (let ((n (gstring "Enter name for new dimension style or [?]: " t)))
         (when (plusp (length n))
           (setf (drawing-dimstyles *dwg*)
                 (append (remove n (all-dimstyles) :key #'dimstyle-name :test #'string-equal)
                         (list (make-dimstyle :name n :vars (current-dimvars)))))
           (setf (sysvar "DIMSTYLE") n) (touch))))
      ((equal k "Restore")
       (let* ((n (gstring "Enter dimension style name, [?] or <select dimension>: " t)) (ds (find-dimstyle n)))
         (cond (ds (apply-dimvars (dimstyle-vars ds)) (setf (sysvar "DIMSTYLE") (dimstyle-name ds)))
               ((plusp (length n)) (out (format nil "Dimension style ~A not found.~%" n))))))
      ((member k '("STatus" "Variables") :test #'equal)
       (dolist (r (dimvar-rows)) (out (format nil "~10A ~A~%" (first r) (lisp-repr (second r))))))
      ((equal k "Apply")
       (dolist (e (get-selection))
         (when (equal (ent-type e) "DIMENSION")
           (set-entity-data e (set-dxf 3 (sysvar "DIMSTYLE") (set-dxf :dimvars (current-dimvars) (ent-data e)))))))
      ((equal k "?") (dolist (ds (all-dimstyles)) (out (format nil "  ~A~%" (dimstyle-name ds))))))))


;;; ---------- dimension lines: line up with other dimensions; grips at the arrow ends ----------

(defun linear-dim-p (data)
  (and (equal (dxf 0 data) "DIMENSION") (member (logand (or (dxf 70 data) 0) 7) '(0 1))
       (dxf 10 data) (dxf 13 data) (dxf 14 data)))

(defun dim-line-theta (data)
  (if (= (logand (or (dxf 70 data) 0) 7) 1)
      (ang (pt3 (dxf 13 data)) (pt3 (dxf 14 data)))
      (or (dxf 50 data) 0d0)))

(defun foot-on-angle-line (p base theta)
  "Foot of the perpendicular from P to the line through BASE at angle THETA."
  (let* ((u (pt (cos theta) (sin theta)))
         (tt (+ (* (- (px p) (px base)) (px u)) (* (- (py p) (py base)) (py u)))))
    (pt (+ (px base) (* tt (px u))) (+ (py base) (* tt (py u))))))

(defun dim-line-ends (data)
  "The two ends of a linear dimension's dimension line (where the arrows are)."
  (let ((th (dim-line-theta data)) (b (pt3 (dxf 10 data))))
    (list (foot-on-angle-line (pt3 (dxf 13 data)) b th) (foot-on-angle-line (pt3 (dxf 14 data)) b th))))

(defun parallel-angle-p (a b)
  (let ((d (mod (- a b) pi))) (or (< d 1d-6) (> d (- pi 1d-6)))))

(defun dimline-snap-point (p theta tol)
  "When P is within TOL of the dimension line of another dimension parallel to THETA, the point
on that line (keeping P's position along it). Returns (values point (from to)) or NIL."
  (let ((best nil) (bd nil) (guide nil))
    (dolist (e (live-entities))
      (let ((data (ent-data e)))
        (when (and (not (eq e *dimline-exclude*)) (linear-dim-p data)
                   (layer-visible-p (find-layer (ent-layer e)))
                   (parallel-angle-p (dim-line-theta data) theta))
          (let* ((q (foot-on-angle-line p (pt3 (dxf 10 data)) (dim-line-theta data)))
                 (dd (dist2d p q)))
            (when (and (< dd tol) (or (null bd) (< dd bd)))
              (let* ((ends (dim-line-ends data))
                     (far (if (> (dist2d q (first ends)) (dist2d q (second ends))) (first ends) (second ends))))
                (setf best q bd dd guide (list far q))))))))
    (when best (values best guide))))

(defun apply-dimline-snap (p)
  "Used by the cursor: P moved onto a lined-up dimension line, or NIL."
  (when *dimline-snap*
    (let ((theta (ignore-errors (funcall *dimline-snap* p))))
      (when theta
        (multiple-value-bind (q guide) (dimline-snap-point p theta (aperture-world))
          (setf *dimline-match* guide)
          q)))))

(defun dimline-grip-stretch (data id to)
  "Drag a dimension line end grip: the whole dimension line moves to pass through TO (the
measurement stays the same); the text moves with it."
  (let* ((th (dim-line-theta data))
         (old (pt3 (dxf 10 data)))
         (new (foot-on-angle-line (pt3 (dxf 14 data)) to th))
         (dv (v- new old)))
    (declare (ignore id))
    (let ((d1 (set-dxf 10 new data)))
      (if (dxf 11 d1) (set-dxf 11 (v+ (pt3 (dxf 11 d1)) dv) d1) d1))))
