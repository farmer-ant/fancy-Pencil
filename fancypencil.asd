(asdf:defsystem "fancypencil"
  :description "2D drafting program with an AutoLISP-compatible interpreter"
  :version "0.3.33"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :components ((:file "package") (:file "geom") (:file "db") (:file "textstyle") (:file "ltype") (:file "hatch") (:file "dxf")
                             (:file "dxf2000") (:file "alisp") (:file "input") (:file "gui") (:file "grips")
                             (:file "commands") (:file "dims") (:file "leader") (:file "modify") (:file "modify2") (:file "cmds2")
                             (:file "utilcmds") (:file "dwg") (:file "props") (:file "express") (:file "textcmds") (:file "docs") (:file "uicmds") (:file "clip") (:file "matchprop") (:file "hover") (:file "prefs") (:file "recent") (:file "blockedit") (:file "views") (:file "spell") (:file "plot") (:file "main")
                             (:static-file "gui.tcl")))))
