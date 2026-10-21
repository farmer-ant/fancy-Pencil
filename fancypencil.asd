(asdf:defsystem "fancypencil"
  :description "2D drafting program with an AutoLISP-compatible interpreter"
  :version "0.3.2"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :components ((:file "package") (:file "geom") (:file "db") (:file "ltype") (:file "hatch") (:file "dxf")
                             (:file "dxf2000") (:file "alisp") (:file "input") (:file "gui") (:file "grips")
                             (:file "commands") (:file "dims") (:file "modify") (:file "modify2") (:file "cmds2")
                             (:file "utilcmds") (:file "props") (:file "express") (:file "main")
                             (:static-file "gui.tcl")))))
