(asdf:defsystem "fancypencil"
  :description "2D drafting program with an AutoLISP-compatible interpreter"
  :version "0.4.9"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :components ((:file "package") (:file "geom") (:file "db") (:file "plane") (:file "mesh") (:file "vector-font") (:file "text3d") (:file "textstyle") (:file "ltype") (:file "hatch") (:file "dxf")
                             (:file "dxf2000") (:file "alisp") (:file "input") (:file "gui") (:file "grips")
                             (:file "commands") (:file "dims") (:file "leader") (:file "mleader") (:file "modify") (:file "modify2") (:file "cmds2")
                             (:file "utilcmds") (:file "dwg") (:file "props") (:file "express") (:file "textcmds") (:file "docs") (:file "uicmds") (:file "clip") (:file "matchprop") (:file "hover") (:file "prefs") (:file "recent") (:file "blockedit") (:file "views") (:file "solids") (:file "csg") (:file "visual") (:file "spell") (:file "plot") (:file "layouts") (:file "massprop") (:file "draworder") (:file "tiles") (:file "snap3d") (:file "raster") (:file "history") (:file "helix") (:file "print3d") (:file "imgview") (:file "surfaces") (:file "acis") (:file "zip") (:file "dwf") (:file "dgn") (:file "underlay") (:file "calc") (:file "spline") (:file "richtext") (:file "navigate") (:file "polyhedra") (:file "struts") (:file "autosave") (:file "main")
                             (:static-file "gui.tcl") (:static-file "toolbars.tcl")))))
