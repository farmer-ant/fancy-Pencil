;;;; surfaces.lisp — REVSURF: a revolved surface mesh, as AutoCAD makes it (the path curve spun
;;;; about an axis object, divided SURFTAB1 times around and SURFTAB2 times along a curve, every
;;;; line of the mesh showing). REVOLVE's MOde option (solids.lisp) makes the smooth kind.

(in-package :fancy-pencil)

(defun straight-path-p (data)
  (let ((type (dxf 0 data)))
    (or (equal type "LINE")
        (and (equal type "LWPOLYLINE") (every (lambda (v) (zerop (or (second v) 0))) (lwpoly-vertices data))))))

(defun revsurf-path (e)
  "Points along the path curve E: a line or straight polyline at its corners, a curve in SURFTAB2
equal steps. (values points closed)."
  (multiple-value-bind (pts closed) (shape-of e)
    (if (straight-path-p (ent-data e))
        (values pts closed)
        (let ((n (max 2 (or (sysvar "SURFTAB2") 6))))
          (values (resample-loop pts closed (if closed n (1+ n))) closed)))))

(defun revsurf-mesh (path closed origin axis start angle around)
  (let* ((k (unit3 axis))
         (full (> (abs angle) (- +2pi+ 1d-9)))
         (rings (loop for i from 0 below (if full around (1+ around))
                      collect (let ((a (+ start (* angle (/ i around)))))
                                (mapcar (lambda (p) (v+ (pt3 origin) (rotate-about (v- (pt3 p) (pt3 origin)) k a))) path)))))
    (append (rings-mesh rings :closed-ring closed :closed-path full
                        :ring-visible (constantly t) :long-visible (constantly t))
            (list (cons :kind "Revolved surface")))))

(defcmd "REVSURF" () "Creates a revolved surface mesh: a curve spun about an axis (SURFTAB1 x SURFTAB2 faces)."
  (out (format nil "Current wire frame density:  SURFTAB1=~D  SURFTAB2=~D~%" (sysvar "SURFTAB1") (sysvar "SURFTAB2")))
  (let ((pe (sel-entity (get-user-input :entity :prompt "Select object to revolve: "))))
    (cond
      ((null pe) nil)
      ((not (member (ent-type pe) '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "ELLIPSE" "SPLINE" "HELIX") :test #'equal))
       (out (format nil "Object not usable to define surface of revolution.~%")))
      (t
       (let* ((ar (get-user-input :entity :prompt "Select object that defines the axis of revolution: "))
              (ae (sel-entity ar))
              (pick (and (consp ar) (pointp (third ar)) (pt3 (third ar)))))
         (cond
           ((null ae) nil)
           ((not (member (ent-type ae) '("LINE" "LWPOLYLINE") :test #'equal))
            (out (format nil "Object not usable as rotation axis.~%")))
           (t
            (let* ((apts (shape-of ae))
                   (a (first apts)) (b (car (last apts)))
                   ;; as in AutoCAD: the axis points away from the end nearest where it was picked
                   (from-b (and pick (< (dist2d pick b) (dist2d pick a))))
                   (o (if from-b b a)) (dir (if from-b (v- a b) (v- b a))))
              (if (< (sqrt (vdot3 dir dir)) 1d-12)
                  (out (format nil "Object not usable as rotation axis.~%"))
                  (let* ((s (get-user-input :angle :prompt "Specify start angle <0>: "))
                         (start (if (realp s) (d s) 0d0))
                         (ia (get-user-input :angle :prompt "Specify included angle (+=ccw, -=cw) <360>: "))
                         (angle (if (and (realp ia) (> (abs ia) 1d-9)) (d ia) +2pi+))
                         (around (max 2 (or (sysvar "SURFTAB1") 6))))
                    (multiple-value-bind (path closed) (revsurf-path pe)
                      (let ((e (add (revsurf-mesh path closed o dir start angle around))))
                        (out (format nil "Revolved surface: ~D x ~D faces.~%" around (if closed (length path) (1- (length path)))))
                        (when (gui-p) (gui-regen))
                        e))))))))))))

(defcmd "SURFTAB1" () "Number of faces around a revolved surface (REVSURF)."
  (let ((n (gint (format nil "Enter new value for SURFTAB1 <~D>: " (sysvar "SURFTAB1")) :flags 6)))
    (when n (setf (sysvar "SURFTAB1") (max 2 (min 32766 n))))))
(defcmd "SURFTAB2" () "Number of faces along a curve of a revolved surface (REVSURF)."
  (let ((n (gint (format nil "Enter new value for SURFTAB2 <~D>: " (sysvar "SURFTAB2")) :flags 6)))
    (when n (setf (sysvar "SURFTAB2") (max 2 (min 32766 n))))))
