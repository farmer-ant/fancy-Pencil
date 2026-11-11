;;;; dwg.lisp — opening and saving AutoCAD .dwg files through an external converter.
;;;; DWG is a closed binary format; Fancy Pencil converts it to DXF (and back) with
;;;;   ODA File Converter  (free from the Open Design Alliance; reads and writes every DWG version), or
;;;;   LibreDWG dwg2dxf    (open source; reads well, writing DWG is experimental so it is not used for saving).
;;;; FANCYPENCIL_DWG_CONVERTER may name either program explicitly.

(in-package :fancy-pencil)

(defun dwg-file-p (path)
  "True when PATH starts with a DWG version signature (AC1012 ... AC1032)."
  (ignore-errors
   (with-open-file (s path :element-type '(unsigned-byte 8))
     (let ((buf (make-array 6 :element-type '(unsigned-byte 8))))
       (and (= (read-sequence buf s) 6)
            (string= (map 'string #'code-char (subseq buf 0 4)) "AC10")
            (every (lambda (b) (<= 48 b 57)) (subseq buf 4 6)))))))

(defun dwg-name-p (path)
  (string-equal (or (pathname-type (pathname path)) "") "dwg"))

(defun run-shell (cmd)
  "Run CMD with /bin/sh, wait, and return (values exit-code output-text)."
  (let ((out (make-string-output-stream)))
    #+sbcl
    (let ((p (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output out :error out :wait t :input nil)))
      (values (sb-ext:process-exit-code p) (get-output-stream-string out)))
    #+ecl
    (multiple-value-bind (stream code) (ext:run-program "/bin/sh" (list "-c" cmd) :output out :error out :wait t)
      (declare (ignore stream))
      (values code (get-output-stream-string out)))
    #-(or sbcl ecl) (values 1 "not supported")))

(defun shell-quote (s)
  (with-output-to-string (o)
    (write-char #\' o)
    (loop for c across (namestring s) do (if (char= c #\') (write-string "'\\''" o) (write-char c o)))
    (write-char #\' o)))

(defun find-dwg-converter ()
  "Returns (values kind path), kind :oda or :libredwg, or NIL when no converter is installed."
  (let ((env (getenv "FANCYPENCIL_DWG_CONVERTER")))
    (when (and env (plusp (length env)) (probe-file env))
      (return-from find-dwg-converter
        (values (if (search "oda" (string-downcase (file-namestring env))) :oda :libredwg) env))))
  (flet ((which (name)
           (multiple-value-bind (code text) (run-shell (format nil "command -v ~A" name))
             (and (eql code 0) (string-trim '(#\Newline #\Space) text)))))
    (let ((oda (or (which "ODAFileConverter")
                   (find-if #'probe-file '("/usr/bin/ODAFileConverter" "/usr/local/bin/ODAFileConverter"))
                   (let ((g (directory "/opt/ODAFileConverter*/ODAFileConverter")))
                     (and g (namestring (first g))))
                   ;; the AppImage, as the Fancy Pencil Kit installer puts it (or in ~/Applications)
                   (let ((home (getenv "HOME")))
                     (and home
                          (or (let ((f (format nil "~A/.local/bin/ODAFileConverter" home))) (and (probe-file f) f))
                              (let ((g (directory (format nil "~A/Applications/ODAFileConverter*.AppImage" home))))
                                (and g (namestring (first g))))))))))
      (cond (oda (values :oda oda))
            ((which "dwg2dxf") (values :libredwg (which "dwg2dxf")))
            (t nil)))))

(defun temp-dir (tag)
  (let ((d (format nil "~A/fancypencil-~A-~D-~D/" (or (getenv "TMPDIR") "/tmp") tag (get-universal-time) (random 100000))))
    (ensure-directories-exist d)
    d))

(defun dwg-install-help ()
  (format nil "Opening and saving .dwg files needs a free DWG converter, and none is installed.~%~%~
Recommended: ODA File Converter (reads and writes every AutoCAD version).~%~
The Fancy Pencil Kit installer (\"Install Fancy Pencil\") can fetch and install it for you, or~%~
download it from https://www.opendesign.com/guestfiles/oda_file_converter~%~
(.deb for Ubuntu/Debian, .rpm for Fedora/openSUSE, AppImage for others), then install it with~%~
    sudo apt install ./ODAFileConverter_*.deb~%~%~
Or: LibreDWG's dwg2dxf (open source; opening only).~%~%~
Meanwhile you can save the drawing from AutoCAD (or another CAD program) as DXF and open that."))

(defun dwg->dxf (dwg)
  "Convert DWG to a temporary DXF. Returns the DXF path, or (values nil message)."
  (multiple-value-bind (kind exe) (find-dwg-converter)
    (case kind
      ((nil) (values nil (dwg-install-help)))
      (:libredwg
       (let ((out (concatenate 'string (temp-dir "in") "drawing.dxf")))
         (multiple-value-bind (code text)
             (run-shell (format nil "~A -y -o ~A ~A" (shell-quote exe) (shell-quote out) (shell-quote dwg)))
           (if (and (probe-file out) (> (with-open-file (s out) (file-length s)) 0))
               out
               (values nil (format nil "LibreDWG could not read this DWG (exit ~A).~%~A" code (last-lines text 6)))))))
      (:oda
       (let* ((in (temp-dir "odain")) (outd (temp-dir "odaout"))
              (copy (concatenate 'string in "drawing.dwg")))
         (copy-file dwg copy)
         (multiple-value-bind (code text)
             (run-shell (format nil "~A ~A ~A ACAD2013 DXF 0 1 \"*.DWG\"" (shell-quote exe) (shell-quote in) (shell-quote outd)))
           (let ((out (concatenate 'string outd "drawing.dxf")))
             (if (probe-file out) out
                 (values nil (format nil "ODA File Converter could not convert this DWG (exit ~A).~%~A" code (last-lines text 6)))))))))))

(defun dxf->dwg (dxf dwg)
  "Write DWG from DXF with ODA File Converter. Returns T, or (values nil message)."
  (multiple-value-bind (kind exe) (find-dwg-converter)
    (if (not (eq kind :oda))
        (values nil (if kind
                        (format nil "Saving as .dwg needs ODA File Converter (LibreDWG can only open DWG files reliably).~%~
Save as .dxf instead: AutoCAD and every other CAD program open DXF directly.~%~
To save .dwg, install ODA File Converter (free from opendesign.com):~%    sudo apt install ./ODAFileConverter_*.deb")
                        (dwg-install-help)))
        (let* ((in (temp-dir "odain")) (outd (temp-dir "odaout")))
          (copy-file dxf (concatenate 'string in "drawing.dxf"))
          (multiple-value-bind (code text)
              (run-shell (format nil "~A ~A ~A ACAD2018 DWG 0 1 \"*.DXF\"" (shell-quote exe) (shell-quote in) (shell-quote outd)))
            (let ((out (concatenate 'string outd "drawing.dwg")))
              (if (probe-file out)
                  (progn (copy-file out dwg) t)
                  ;; ODA leaves its reasons in a .err file next to the output
                  (let ((err (format nil "~{~A~^~%~}"
                                     (loop for e in (append (directory (concatenate 'string outd "*.err"))
                                                            (directory (concatenate 'string in "*.err")))
                                           collect (last-lines (ignore-errors (read-text-file e)) 12)))))
                    (format *error-output* "~&DWG save failed (exit ~A):~%~A~%~A~%" code text err)
                    (values nil (format nil "ODA File Converter could not write the DWG (exit ~A).~%~A~@[~%~A~]"
                                        code (last-lines text 6) (and (plusp (length err)) err)))))))))))

(defun read-text-file (path)
  (with-open-file (s path :external-format :latin-1)
    (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))

(defun copy-file (from to)
  (with-open-file (i from :element-type '(unsigned-byte 8))
    (with-open-file (o to :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
      (let ((buf (make-array 65536 :element-type '(unsigned-byte 8))))
        (loop for n = (read-sequence buf i) while (plusp n) do (write-sequence buf o :end n)))))
  to)

(defun last-lines (text n)
  (let ((lines (remove "" (split-string (or text "") #\Newline) :test #'string=)))
    (format nil "~{~A~^~%~}" (last lines n))))

(defun tell-user (msg)
  "Show MSG in a message box (GUI) and on the command line."
  (out (format nil "~A~%" msg))
  (when (and (gui-p) (not (from-lisp-p)))
    (tk-ask (format nil "tk_messageBox -parent . -title {Fancy Pencil} -icon info -message ~A" (tq msg)))))

(defun dxf-file-p (path)
  "Rough check that PATH is an ASCII DXF: a 0/SECTION pair near the start."
  (ignore-errors
   (with-open-file (s path :external-format :latin-1)
     (let ((buf (make-string 4096)))
       (let ((n (read-sequence buf s)))
         (search "SECTION" (subseq buf 0 n)))))))
