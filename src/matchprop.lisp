;;;; matchprop.lisp — MATCHPROP (MA, PAINTER): copy properties from one object to others,
;;;; like AutoCAD's Match Properties. Basic properties (color, layer, linetype, linetype scale,
;;;; lineweight, thickness) go to any object; special properties go between objects of the
;;;; same kind: text style/height/width/oblique, dimension style and overrides, hatch pattern/
;;;; scale/angle, polyline width. Which ones are copied is chosen with the Settings option.

(in-package :fancy-pencil)

(defparameter *matchprop-items*
  '((:color "Color") (:layer "Layer") (:ltype "Linetype") (:ltscale "Linetype Scale")
    (:lineweight "Lineweight") (:thickness "Thickness")
    (:text "Text") (:dim "Dimension") (:hatch "Hatch") (:polyline "Polyline"))
  "Settings in the order the panel shows them: first the basic, then the special properties.")

(defvar *matchprop-on* (mapcar #'first *matchprop-items*) "Properties MATCHPROP copies (all, as in AutoCAD).")

(defun mp-on (k) (member k *matchprop-on*))

(defun mp-copy-codes (src dst codes)
  "DST with each of CODES set as in SRC (removed where SRC doesn't have it, i.e. ByLayer/default)."
  (let ((d dst))
    (dolist (c codes d)
      (setf d (if (assoc c src) (set-dxf c (dxf c src) d) (rem-dxf c d))))))

(defun mp-type (data) (dxf 0 data))

(defun lwpoly-strip-vertex-widths (data)
  (remove-if (lambda (p) (and (consp p) (member (car p) '(40 41)))) data))

(defun lwpoly-width (data)
  "A polyline's width: its constant width, else the width at its first vertex."
  (or (dxf 43 data) (dxf 40 data) 0d0))

(defun match-props (src dst)
  "DST's data with the selected properties of SRC copied onto it."
  (let ((d dst) (st (mp-type src)) (dt (mp-type dst)))
    ;; basic properties, any object
    (when (mp-on :layer) (setf d (set-dxf 8 (dxf 8 src) d)))
    (when (mp-on :color) (setf d (mp-copy-codes src d '(62 420 440))))
    (when (mp-on :ltype) (setf d (mp-copy-codes src d '(6))))
    (when (mp-on :ltscale) (setf d (mp-copy-codes src d '(48))))
    (when (mp-on :lineweight) (setf d (mp-copy-codes src d '(370))))
    (when (mp-on :thickness) (setf d (mp-copy-codes src d '(39))))
    ;; special properties, between objects of the same kind
    (when (mp-on :text)
      (let ((texts '("TEXT" "ATTDEF" "ATTRIB")))
        (cond ((and (member st texts :test #'equal) (member dt texts :test #'equal))
               (setf d (mp-copy-codes src d '(7 40 41 51))))
              ((and (or (member st texts :test #'equal) (equal st "MTEXT"))
                    (or (member dt texts :test #'equal) (equal dt "MTEXT")))
               ;; MTEXT's 41 is its box width, not a width factor: only style and height carry over
               (when (dxf 7 src) (setf d (set-dxf 7 (dxf 7 src) d)))
               (when (dxf 40 src) (setf d (set-dxf 40 (dxf 40 src) d)))))))
    (when (and (mp-on :dim) (equal st "DIMENSION") (equal dt "DIMENSION"))
      (setf d (set-dxf 3 (or (dxf 3 src) "Standard") d))
      ;; (set-entity-data redraws the dimension with its new style)
      (setf d (if (dxf :dimvars src) (set-dxf :dimvars (copy-list (dxf :dimvars src)) d) (rem-dxf :dimvars d))))
    (when (and (mp-on :dim) (equal st "LEADER") (equal dt "LEADER"))
      (setf d (set-dxf 3 (or (dxf 3 src) "Standard") d))
      (setf d (mp-copy-codes src d '(:ldr-asz :ldr-scale :ldr-blk 71 72))))
    (when (and (mp-on :hatch) (equal st "HATCH") (equal dt "HATCH"))
      (setf d (mp-copy-codes src d '(2 70 41 52 :pattern))))
    (when (and (mp-on :polyline) (equal st "LWPOLYLINE") (equal dt "LWPOLYLINE"))
      (let ((w (lwpoly-width src)))
        (setf d (lwpoly-strip-vertex-widths d))
        (setf d (if (and w (/= w 0)) (set-dxf 43 (d w) d) (rem-dxf 43 d)))
        ;; linetype generation (flag 128)
        (let ((f (or (dxf 70 d) 0)))
          (setf d (set-dxf 70 (if (logtest 128 (or (dxf 70 src) 0)) (logior f 128) (logand f (lognot 128))) d)))))
    d))

(defun apply-match (s e)
  (unless (eq s e)
    (set-entity-data e (match-props (ent-data s) (ent-data e)))))

;;; ---------- Settings ----------

(defun matchprop-settings-text ()
  (format nil "~{~A~^ ~}" (loop for (k label) in *matchprop-items* when (mp-on k) collect label)))

(defun matchprop-settings ()
  (if (gui-p)
      (let ((r (tk-ask (format nil "matchpropdlg {~{~A~^ ~}}"
                               (loop for (k label) in *matchprop-items*
                                     collect (format nil "{~A ~A ~D}" (string-downcase (symbol-name k)) (tq label) (if (mp-on k) 1 0)))))))
        (when (and (>= (length r) 2) (string= (subseq r 0 2) "OK"))
          (setf *matchprop-on*
                (loop for (k nil) in *matchprop-items*
                      when (member (string-downcase (symbol-name k)) (split-string r #\Space) :test #'string=)
                        collect k))))
      ;; command line: toggle settings by name
      (let ((kws '("Color" "LAyer" "LType" "LTScale" "LWeight" "THickness" "Text" "Dimension" "Hatch" "Polyline" "All" "None")))
        (loop
          (out (format nil "Current active settings:  ~A~%" (matchprop-settings-text)))
          (let ((r (get-user-input :keyword :prompt "Turn a setting on/off [Color/LAyer/LType/LTScale/LWeight/THickness/Text/Dimension/Hatch/Polyline/All/None] <done>: "
                                            :keywords kws)))
            (cond ((or (null r) (not (stringp r))) (return))
                  ((equal r "All") (setf *matchprop-on* (mapcar #'first *matchprop-items*)))
                  ((equal r "None") (setf *matchprop-on* nil))
                  (t (let ((k (nth (position r kws :test #'string-equal) (mapcar #'first *matchprop-items*))))
                       (setf *matchprop-on* (if (mp-on k) (remove k *matchprop-on*)
                                                (remove-if-not (lambda (x) (or (eq x k) (mp-on x))) (mapcar #'first *matchprop-items*))))))))))))

;;; ---------- the command ----------

(defcmd "MATCHPROP" ("MA" "PAINTER") "Copies properties (layer, color, linetype, text style, dimension style, hatch ...) from one object to others."
  (let* ((pre (take-pickfirst))
         (s (if (= (length pre) 1)
                (first pre)
                (let ((src (get-user-input :entity :prompt "Select source object: ")))
                  (and (consp src) (sel-entity src))))))
    (when s
      (ui-highlight (list s) t)
      (unwind-protect
           (let ((n 0))
             (out (format nil "Current active settings:  ~A~%" (matchprop-settings-text)))
             (loop
               (let ((r (get-user-input :select-point :prompt "Select destination object(s) or [Settings]: "
                                                      :keywords '("Settings" "Window" "Crossing" "ALL" "Previous"))))
                 (flet ((apply-all (ents)
                          (dolist (e (filter-editable ents))
                            (unless (eq e s) (apply-match s e) (incf n)))
                          (when (gui-p) (gui-regen))))
                   (cond ((null r) (return))
                         ((equal r "Settings") (matchprop-settings)
                          (out (format nil "Current active settings:  ~A~%" (matchprop-settings-text))))
                         ((member r '("Window" "Crossing") :test #'equal)
                          (let* ((p1 (get-user-input :point :prompt "Specify first corner: " :nosnap t))
                                 (p2 (and (pointp p1) (get-user-input :corner :base p1 :prompt "Specify opposite corner: "
                                                                             :drag (window-selection-drag p1) :nosnap t))))
                            (when (pointp p2) (apply-all (entities-in-window p1 p2 (equal r "Crossing"))))))
                         ((equal r "ALL") (apply-all (remove-if-not (lambda (e) (layer-visible-p (find-layer (ent-layer e)))) (live-entities))))
                         ((equal r "Previous") (apply-all (remove-if-not #'ent-live-p *previous-selection*)))
                         ((pickset-p r) (apply-all (remove-if-not #'ent-live-p (pickset-items r))))
                         ((ename-p r) (when (ent-live-p r) (apply-all (list r))))
                         ((pointp r)
                          (let ((e (pick-entity-at r)))
                            (if e (apply-all (list e))
                                (let ((p2 (get-user-input :corner :base r :prompt "Specify opposite corner: "
                                                                  :drag (window-selection-drag r) :nosnap t)))
                                  (when (pointp p2)
                                    (apply-all (entities-in-window r p2 (< (px p2) (px r))))))))))))))
        (ui-highlight (list s) nil)))))
