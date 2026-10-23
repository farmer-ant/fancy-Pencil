;;;; views.lisp — the standard views (Top, Bottom, Left, Right, Front, Back and the four
;;;; isometric views, as on AutoCAD's View > 3D Views menu) and isometric drafting (ISODRAFT,
;;;; ISOPLANE, F5).
;;;;
;;;; Drawings are 2D with optional heights: objects can sit at a Z elevation and have a THICKNESS
;;;; (they then show as walls in the 3D views). In Top and the isometric views points you pick land
;;;; on the ground (XY) plane, so you can draw and edit there; Front, Back, Left and Right look at
;;;; that plane edge-on and are for looking.

(in-package :fancy-pencil)

(defun unit3 (v) (let ((l (sqrt (vdot3 v v)))) (if (< l 1d-12) v (v* v (/ 1 l)))))
(defun cross3 (a b)
  (pt (- (* (py a) (pz b)) (* (pz a) (py b)))
      (- (* (pz a) (px b)) (* (px a) (pz b)))
      (- (* (px a) (py b)) (* (py a) (px b)))))

(defparameter *standard-views*
  ;; name  direction toward the viewer  (right up), or NIL to work them out
  '(("Top" (0 0 1) ((1 0 0) (0 1 0)))
    ("Bottom" (0 0 -1) ((1 0 0) (0 -1 0)))
    ("Front" (0 -1 0) ((1 0 0) (0 0 1)))
    ("Back" (0 1 0) ((-1 0 0) (0 0 1)))
    ("Right" (1 0 0) ((0 1 0) (0 0 1)))
    ("Left" (-1 0 0) ((0 -1 0) (0 0 1)))
    ("SW Isometric" (-1 -1 1) nil)
    ("SE Isometric" (1 -1 1) nil)
    ("NE Isometric" (1 1 1) nil)
    ("NW Isometric" (-1 1 1) nil)))

(defun make-view (name)
  (let ((spec (assoc name *standard-views* :test #'string-equal)))
    (when spec
      (destructuring-bind (nm dir axes) spec
        (if (string-equal nm "Top")
            nil
            (let* ((d (unit3 (pt3 dir)))
                   (r (if axes (pt3 (first axes)) (unit3 (cross3 (pt 0 0 1) d))))
                   (u (if axes (pt3 (second axes)) (unit3 (cross3 d r)))))
              (list nm r u d)))))))

(defun edge-on-view-p () (and *view* (< (abs (pz (fourth *view*))) 1d-9)))

(defun set-view (name)
  (let ((spec (assoc name *standard-views* :test #'string-equal)))
    (if (null spec)
        (out (format nil "Unknown view ~A.~%" name))
        (progn
          (setf *view* (make-view (first spec)))
          (setf *view-history* nil)
          (clear-geom-cache)
          (when (gui-p) (zoom-extents) (refresh-grips) (gui-sync))
          (out (format nil "~A view.~A~%" (first spec)
                       (cond ((null *view*) "")
                             ((edge-on-view-p)
                              (format nil "~%This view looks at the drawing plane edge-on: use it to look (objects with a thickness or height show as~%~
                                           walls). Draw and edit in Top or an isometric view."))
                             (t "  You can draw and edit on the ground plane here; text shows as its outline."))))))))

(defparameter *view-keywords*
  '(("Top" "Top") ("Bottom" "Bottom") ("Left" "Left") ("Right" "Right") ("Front" "Front") ("BAck" "Back")
    ("SWiso" "SW Isometric") ("SEiso" "SE Isometric") ("NEiso" "NE Isometric") ("NWiso" "NW Isometric")))

(defcmd "-VIEW" ("VIEW" "V" "VIEWS") "Changes to a standard view: Top, Bottom, Left, Right, Front, Back or an isometric view."
  (let ((k (gkw (format nil "Enter a view [Top/Bottom/Left/Right/Front/BAck/SWiso/SEiso/NEiso/NWiso] <~A>: "
                        (if *view* (first *view*) "Top"))
                (mapcar #'first *view-keywords*))))
    (when k (set-view (second (assoc k *view-keywords* :test #'string-equal))))))

(defcmd "PLAN" () "Back to the plan (Top) view." (set-view "Top"))

;;; ---------- heights: THICKNESS and ELEVATION for new objects ----------

(defun apply-new-object-height (data)
  "New objects take THICKNESS (extrusion) and ELEVATION (height above the ground plane)."
  (let ((th (sysvar "THICKNESS")) (el (sysvar "ELEVATION")) (type (dxf 0 data)))
    (when (and (realp th) (/= th 0) (member type '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POINT") :test #'equal)
               (not (assoc 39 data)))
      (setf data (append data (list (cons 39 (d th))))))
    (when (and (realp el) (/= el 0) (member type '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POINT" "TEXT" "MTEXT" "INSERT") :test #'equal))
      (if (equal type "LWPOLYLINE")
          (unless (assoc 38 data) (setf data (append data (list (cons 38 (d el))))))
          (setf data (map-points data '(10 11) (lambda (p) (if (zerop (pz p)) (pt (px p) (py p) el) p))))))
    data))

(defcmd "THICKNESS" ("TH") "Sets the thickness (extrusion height) for new lines, arcs, circles and polylines."
  (let ((v (gdist (format nil "Enter new value for THICKNESS <~A>: " (fmt-len (or (sysvar "THICKNESS") 0d0))))))
    (when (realp v) (setf (sysvar "THICKNESS") (d v)))))

(defcmd "ELEV" ("ELEVATION") "Sets the elevation and thickness for new objects."
  (let ((e (gdist (format nil "Specify new default elevation <~A>: " (fmt-len (or (sysvar "ELEVATION") 0d0))))))
    (when (realp e) (setf (sysvar "ELEVATION") (d e))))
  (let ((v (gdist (format nil "Specify new default thickness <~A>: " (fmt-len (or (sysvar "THICKNESS") 0d0))))))
    (when (realp v) (setf (sysvar "THICKNESS") (d v)))))

;;; ---------- isometric drafting ----------

(defun set-iso (on &optional plane)
  (setf (sysvar "SNAPSTYL") (if on 1 0))
  (when plane (setf (sysvar "SNAPISOPAIR") plane))
  (when (gui-p) (gui-regen) (gui-sync)))

(defcmd "ISODRAFT" () "Turns isometric drafting on (choosing an isoplane) or off."
  (let ((k (gkw (format nil "Enter option [Orthographic/isoplaneLeft/isoplaneTop/isoplaneRight] <~A>: "
                        (if (iso-mode-p) "Orthographic" (format nil "isoplane~A" (isoplane-name))))
                '("Orthographic" "isoplaneLeft" "isoplaneTop" "isoplaneRight")
                (if (iso-mode-p) "Orthographic" (format nil "isoplane~A" (isoplane-name))))))
    (cond ((string-equal k "Orthographic") (set-iso nil) (out (format nil "Isometric drafting off.~%")))
          (t (set-iso t (cond ((string-equal k "isoplaneLeft") 0) ((string-equal k "isoplaneRight") 2) (t 1)))
             (out (format nil "Isometric drafting on: isoplane ~A. F5 switches isoplanes; ELLIPSE > Isocircle draws circles.~%"
                          (isoplane-name)))))))

(defun cycle-isoplane ()
  ;; AutoCAD's order: Left -> Top -> Right -> Left
  (setf (sysvar "SNAPISOPAIR") (mod (1+ (isoplane)) 3))
  (out (format nil "<Isoplane ~A>~%" (isoplane-name)))
  (when (gui-p) (draw-temp) (gui-sync)))

(defcmd "ISOPLANE" () "Chooses the isoplane for isometric drafting (Left, Top or Right)."
  (let ((k (gkw (format nil "Enter isometric plane setting [Left/Top/Right] <~A>: "
                        (isoplane-name (mod (1+ (isoplane)) 3)))
                '("Left" "Top" "Right"))))
    (if (null k) (cycle-isoplane)
        (progn (setf (sysvar "SNAPISOPAIR") (cond ((string-equal k "Left") 0) ((string-equal k "Right") 2) (t 1)))
               (out (format nil "Current isoplane: ~A~%" (isoplane-name)))
               (when (gui-p) (draw-temp) (gui-sync))))))

;;; ---------- the view picker (a small panel of cube pictures, top right of the drawing) ----------

(defparameter *cube-faces*
  ;; normal, corner indices (cube corners are (+-1 +-1 +-1)), color
  '(((0 0 1) ((-1 -1 1) (1 -1 1) (1 1 1) (-1 1 1)) "#7fb2e5")       ; top: blue, like AutoCAD's
    ((0 0 -1) ((-1 -1 -1) (-1 1 -1) (1 1 -1) (1 -1 -1)) "#56606c")
    ((0 -1 0) ((-1 -1 -1) (1 -1 -1) (1 -1 1) (-1 -1 1)) "#d9dde3")   ; front
    ((0 1 0) ((1 1 -1) (-1 1 -1) (-1 1 1) (1 1 1)) "#9aa3ad")
    ((1 0 0) ((1 -1 -1) (1 1 -1) (1 1 1) (1 -1 1)) "#b8c0c9")       ; right
    ((-1 0 0) ((-1 1 -1) (-1 -1 -1) (-1 -1 1) (-1 1 1)) "#8a939e")))

(defun view-cube-picture (name)
  "Polygons (color x1 y1 ...) of a cube seen in view NAME, fitted to about 22 pixels, y down."
  (let* ((v (make-view name))
         (r (if v (second v) (pt 1 0 0))) (u (if v (third v) (pt 0 1 0))) (dir (if v (fourth v) (pt 0 0 1)))
         (scale (if (and v (> (abs (pz dir)) 1d-9) (> (+ (abs (px dir)) (abs (py dir))) 1d-9)) 6.4d0 9.5d0)))
    (loop for (n corners col) in *cube-faces*
          when (> (vdot3 (pt3 n) dir) 1d-6)
            collect (cons col (loop for c in corners
                                    for p = (pt3 c)
                                    append (list (round (* scale (vdot3 p r))) (round (- (* scale (vdot3 p u))))))))))

(defun send-view-picker ()
  (when (gui-p)
    (tk "viewcube_set {~{~A~^ ~}}"
        (loop for (kw name) in *view-keywords*
              collect (format nil "{~A ~A {~{~A~^ ~}}}" (tq kw) (tq name)
                              (mapcar (lambda (poly) (format nil "{~{~A~^ ~}}" poly)) (view-cube-picture name)))))))
