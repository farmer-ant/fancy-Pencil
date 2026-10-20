(asdf:defsystem "lispdraft"
  :description "2D drafting program with an AutoLISP-compatible interpreter"
  :version "0.1"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :components ((:file "package") (:file "geom") (:file "db") (:file "dxf") (:file "alisp")
                             (:file "input") (:file "gui") (:file "commands") (:file "modify")
                             (:file "utilcmds") (:file "main")
                             (:static-file "gui.tcl")))))
