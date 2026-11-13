// fancypencil-solids - exact solid modelling for Fancy Pencil, done by Open CASCADE.
//
// Fancy Pencil starts this program and talks to it a line at a time, as you would at a command
// line: one request per line on standard input, the answer on standard output. Every answer ends
// with a line that starts "ok" or "err". Shapes are kept here by number until deleted.
//
// Requests (numbers are plain decimals; angles in degrees; points as x y z):
//   ping
//   box ID  x y z  dx dy dz
//   wedge ID  x y z  dx dy dz                  (slopes from the top at x down to the bottom at x+dx)
//   cyl ID  ox oy oz  nx ny nz  ux uy uz  r h  (base centre, axis, start direction for the seam)
//   cone ID  ox oy oz  nx ny nz  ux uy uz  r1 r2 h
//   sphere ID  cx cy cz  r
//   torus ID  ox oy oz  nx ny nz  R r
//   prism ID  dx dy dz                         then a profile (below)
//   revol ID  ox oy oz  dx dy dz  angle        then a profile
//   fuse ID A B [C ...]   cut ID A B [C ...]   common ID A B [C ...]
//   halves ID1 ID2 A  ox oy oz  nx ny nz       (A cut by a plane: ID1 on the n side, ID2 the other)
//   fillet ID SRC r  E1 E2 ...                 (edges by number, as mesh lists them)
//   chamfer ID SRC d  E1 E2 ...
//   shell ID SRC t  F1 F2 ...                  (hollow out, leaving faces F open; t<0 inward)
//   trsf ID SRC  a11 a12 a13 a14  a21 a22 a23 a24  a31 a32 a33 a34
//   polyhedron ID N                            then N lines "k x y z ..." (flat faces): a solid of flat faces
//   copy ID SRC      del ID      clear
//   mesh ID deflection angle                   (the shape as triangles, face by face, with its edges)
//   props ID                                   (volume area cx cy cz)
//   bbox ID
//   save ID                                    (the exact shape as BREP text)
//   load ID N                                  then N lines of BREP text
//   stepwrite PATH-ON-NEXT-LINE ID ...         stepwrite ID ... then the path on the next line
//   stepread FIRSTID                           then the path on the next line: makes FIRSTID, FIRSTID+1 ...
// A profile is lines up to "end":
//   loop                                       (starts a closed outline; the first is the outside)
//   line  x1 y1 z1  x2 y2 z2
//   arc   x1 y1 z1  xm ym zm  x2 y2 z2         (start, a point on it, end)
//   circle cx cy cz  nx ny nz  r
//   spline N  x y z ...                        (N points the curve passes through)

#include <BRepAdaptor_Curve.hxx>
#include <BRepAlgoAPI_Common.hxx>
#include <BRepAlgoAPI_Cut.hxx>
#include <BRepAlgoAPI_Fuse.hxx>
#include <BRepBndLib.hxx>
#include <BRepBuilderAPI_Copy.hxx>
#include <BRepBuilderAPI_GTransform.hxx>
#include <BRepBuilderAPI_MakeEdge.hxx>
#include <BRepBuilderAPI_MakeFace.hxx>
#include <BRepBuilderAPI_MakePolygon.hxx>
#include <BRepBuilderAPI_MakeSolid.hxx>
#include <BRepBuilderAPI_Sewing.hxx>
#include <TopoDS_Shell.hxx>
#include <BRepBuilderAPI_MakeWire.hxx>
#include <BRepBuilderAPI_Transform.hxx>
#include <BRepCheck_Analyzer.hxx>
#include <BRepFilletAPI_MakeChamfer.hxx>
#include <BRepFilletAPI_MakeFillet.hxx>
#include <BRepGProp.hxx>
#include <BRepMesh_IncrementalMesh.hxx>
#include <BRepOffsetAPI_MakeThickSolid.hxx>
#include <BRepPrimAPI_MakeBox.hxx>
#include <BRepPrimAPI_MakeCone.hxx>
#include <BRepPrimAPI_MakeCylinder.hxx>
#include <BRepPrimAPI_MakeHalfSpace.hxx>
#include <BRepPrimAPI_MakePrism.hxx>
#include <BRepPrimAPI_MakeRevol.hxx>
#include <BRepPrimAPI_MakeSphere.hxx>
#include <BRepPrimAPI_MakeTorus.hxx>
#include <BRepPrimAPI_MakeWedge.hxx>
#include <BRepTools.hxx>
#include <BRep_Builder.hxx>
#include <BRep_Tool.hxx>
#include <BRepAdaptor_Surface.hxx>
#include <Bnd_Box.hxx>
#include <GCPnts_TangentialDeflection.hxx>
#include <GC_MakeArcOfCircle.hxx>
#include <GProp_GProps.hxx>
#include <GeomAPI_Interpolate.hxx>
#include <Geom_BSplineCurve.hxx>
#include <Geom_TrimmedCurve.hxx>
#include <IFSelect_ReturnStatus.hxx>
#include <Interface_Static.hxx>
#include <Message.hxx>
#include <Message_Messenger.hxx>
#include <Poly_PolygonOnTriangulation.hxx>
#include <Poly_Triangulation.hxx>
#include <STEPControl_Reader.hxx>
#include <STEPControl_Writer.hxx>
#include <ShapeFix_Face.hxx>
#include <ShapeFix_Shape.hxx>
#include <ShapeUpgrade_UnifySameDomain.hxx>
#include <Standard_Failure.hxx>
#include <TColgp_HArray1OfPnt.hxx>
#include <TopExp.hxx>
#include <TopExp_Explorer.hxx>
#include <TopLoc_Location.hxx>
#include <TopTools_IndexedMapOfShape.hxx>
#include <TopTools_ListOfShape.hxx>
#include <TopoDS.hxx>
#include <TopoDS_Compound.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Face.hxx>
#include <TopoDS_Wire.hxx>
#include <gp_Ax2.hxx>
#include <gp_GTrsf.hxx>
#include <gp_Pln.hxx>
#include <gp_Trsf.hxx>

#include <cmath>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

static std::map<long, TopoDS_Shape> shapes;

static void fail(const std::string& why) { throw std::runtime_error(why); }

static double rad(double deg) { return deg * M_PI / 180.0; }

static const TopoDS_Shape& get(long id) {
  auto it = shapes.find(id);
  if (it == shapes.end()) fail("no shape " + std::to_string(id));
  return it->second;
}

static gp_Pnt readPnt(std::istream& in) { double x, y, z; if (!(in >> x >> y >> z)) fail("a point is missing"); return gp_Pnt(x, y, z); }
static gp_Dir readDir(std::istream& in) {
  double x, y, z; if (!(in >> x >> y >> z)) fail("a direction is missing");
  if (x * x + y * y + z * z < 1e-24) fail("a direction of length 0");
  return gp_Dir(x, y, z);
}
static double readNum(std::istream& in, const char* what) { double v; if (!(in >> v)) fail(std::string(what) + " is missing"); return v; }

// "outline" in a profile: keep it as an outline (for a surface), not a flat face.
static bool profileOutline = false;

// The lines of a profile, up to "end", as wires: the first is the outside.
static std::vector<TopoDS_Wire> readProfile() {
  profileOutline = false;
  std::vector<TopoDS_Wire> wires;
  BRepBuilderAPI_MakeWire* mw = nullptr;
  std::string line;
  auto finish = [&]() {
    if (mw) {
      if (!mw->IsDone()) { delete mw; fail("the outline's pieces don't join up"); }
      wires.push_back(mw->Wire()); delete mw; mw = nullptr;
    }
  };
  while (std::getline(std::cin, line)) {
    std::istringstream in(line);
    std::string w; in >> w;
    if (w.empty()) continue;
    if (w == "end") break;
    if (w == "loop") { finish(); mw = new BRepBuilderAPI_MakeWire(); continue; }
    if (w == "outline") { profileOutline = true; continue; }
    if (!mw) mw = new BRepBuilderAPI_MakeWire();
    TopoDS_Edge e;
    if (w == "line") {
      gp_Pnt a = readPnt(in), b = readPnt(in);
      if (a.Distance(b) < 1e-9) continue;
      e = BRepBuilderAPI_MakeEdge(a, b);
    } else if (w == "arc") {
      gp_Pnt a = readPnt(in), m = readPnt(in), b = readPnt(in);
      GC_MakeArcOfCircle arc(a, m, b);
      if (!arc.IsDone()) fail("an arc through three points in a line");
      e = BRepBuilderAPI_MakeEdge(arc.Value());
    } else if (w == "circle") {
      gp_Pnt c = readPnt(in); gp_Dir n = readDir(in); double r = readNum(in, "the radius");
      e = BRepBuilderAPI_MakeEdge(gp_Circ(gp_Ax2(c, n), r));
    } else if (w == "spline") {
      int n = (int)readNum(in, "the number of points");
      Handle(TColgp_HArray1OfPnt) pts = new TColgp_HArray1OfPnt(1, n);
      for (int i = 1; i <= n; i++) pts->SetValue(i, readPnt(in));
      GeomAPI_Interpolate ip(pts, Standard_False, 1e-7);
      ip.Perform();
      if (!ip.IsDone()) fail("a spline that can't be made");
      e = BRepBuilderAPI_MakeEdge(ip.Curve());
    } else fail("unknown profile piece: " + w);
    mw->Add(e);
    if (!mw->IsDone()) { delete mw; fail("the outline's pieces don't join up"); }
  }
  finish();
  if (wires.empty()) fail("an empty profile");
  return wires;
}

// A flat face from the profile (holes cut out), or the outline alone if it is open.
static TopoDS_Shape profileShape(const std::vector<TopoDS_Wire>& wires) {
  if (profileOutline || !BRep_Tool::IsClosed(wires[0])) return wires[0];
  BRepBuilderAPI_MakeFace mf(wires[0], Standard_True);
  if (!mf.IsDone()) fail("the outline isn't flat");
  for (size_t i = 1; i < wires.size(); i++) mf.Add(wires[i]);
  ShapeFix_Face fix(mf.Face());
  fix.FixOrientation();
  fix.Perform();
  return fix.Face();
}

// Merge faces that lie on one surface (a union's seams), and check the result.
static TopoDS_Shape tidy(const TopoDS_Shape& s) {
  TopoDS_Shape r = s;
  try {
    ShapeUpgrade_UnifySameDomain u(s, Standard_True, Standard_True, Standard_False);
    u.Build();
    r = u.Shape();
  } catch (Standard_Failure&) { r = s; }
  return r;
}

static void put(long id, const TopoDS_Shape& s) {
  if (s.IsNull()) fail("the result is empty");
  shapes[id] = s;
}

static std::vector<long> readIds(std::istream& in) {
  std::vector<long> v; long x;
  while (in >> x) v.push_back(x);
  return v;
}

template <class Op>
static TopoDS_Shape boolean(const std::vector<long>& ids) {
  if (ids.size() < 2) fail("two shapes are needed");
  TopTools_ListOfShape args, tools;
  args.Append(get(ids[0]));
  for (size_t i = 1; i < ids.size(); i++) tools.Append(get(ids[i]));
  Op op;
  op.SetArguments(args);
  op.SetTools(tools);
  op.SetRunParallel(Standard_False);
  op.Build();
  if (!op.IsDone() || op.HasErrors()) fail("the shapes could not be combined");
  return tidy(op.Shape());
}

static void doMesh(long id, double defl, double angle) {
  const TopoDS_Shape& s = get(id);
  BRepMesh_IncrementalMesh mesher(s, defl, Standard_False, rad(angle), Standard_True);
  TopTools_IndexedMapOfShape faces, edges;
  TopExp::MapShapes(s, TopAbs_FACE, faces);
  TopExp::MapShapes(s, TopAbs_EDGE, edges);
  std::cout << "faces " << faces.Extent() << " edges " << edges.Extent() << "\n";
  for (int fi = 1; fi <= faces.Extent(); fi++) {
    TopoDS_Face f = TopoDS::Face(faces(fi));
    TopLoc_Location loc;
    Handle(Poly_Triangulation) tri = BRep_Tool::Triangulation(f, loc);
    BRepAdaptor_Surface surf(f, Standard_False);
    const char* kind = "other";
    switch (surf.GetType()) {
      case GeomAbs_Plane: kind = "plane"; break;
      case GeomAbs_Cylinder: kind = "cylinder"; break;
      case GeomAbs_Cone: kind = "cone"; break;
      case GeomAbs_Sphere: kind = "sphere"; break;
      case GeomAbs_Torus: kind = "torus"; break;
      default: kind = "freeform";
    }
    if (tri.IsNull()) { std::cout << "face " << fi << " " << kind << " 0 0\n"; continue; }
    gp_Trsf t = loc.Transformation();
    int nn = tri->NbNodes(), nt = tri->NbTriangles();
    std::cout << "face " << fi << " " << kind << " " << nn << " " << nt << "\n";
    for (int i = 1; i <= nn; i++) {
      gp_Pnt p = tri->Node(i).Transformed(t);
      std::cout << p.X() << " " << p.Y() << " " << p.Z() << "\n";
    }
    bool rev = (f.Orientation() == TopAbs_REVERSED);
    for (int i = 1; i <= nt; i++) {
      int a, b, c;
      tri->Triangle(i).Get(a, b, c);
      if (rev) std::swap(b, c);
      std::cout << a - 1 << " " << b - 1 << " " << c - 1 << "\n";
    }
    // the face's edges along its triangles (seams and collapsed edges are not drawn)
    for (TopExp_Explorer ex(f, TopAbs_EDGE); ex.More(); ex.Next()) {
      TopoDS_Edge e = TopoDS::Edge(ex.Current());
      if (BRep_Tool::Degenerated(e) || BRep_Tool::IsClosed(e, f)) continue;
      Handle(Poly_PolygonOnTriangulation) pol = BRep_Tool::PolygonOnTriangulation(e, tri, loc);
      if (pol.IsNull()) continue;
      const TColStd_Array1OfInteger& nodes = pol->Nodes();
      std::cout << "bound " << edges.FindIndex(e) << " " << nodes.Length();
      for (int i = nodes.Lower(); i <= nodes.Upper(); i++) std::cout << " " << nodes(i) - 1;
      std::cout << "\n";
    }
  }
  // each edge as a chain of points, for picking (fillets) and for loose edges
  for (int ei = 1; ei <= edges.Extent(); ei++) {
    TopoDS_Edge e = TopoDS::Edge(edges(ei));
    if (BRep_Tool::Degenerated(e)) { std::cout << "edge " << ei << " 0\n"; continue; }
    BRepAdaptor_Curve c(e);
    GCPnts_TangentialDeflection d(c, rad(angle), defl);
    std::cout << "edge " << ei << " " << d.NbPoints();
    for (int i = 1; i <= d.NbPoints(); i++) { gp_Pnt p = d.Value(i); std::cout << " " << p.X() << " " << p.Y() << " " << p.Z(); }
    std::cout << "\n";
  }
}

static void handle(const std::string& line) {
  std::istringstream in(line);
  std::string cmd; in >> cmd;
  if (cmd.empty()) return;
  if (cmd == "ping") { std::cout << "ok fancypencil-solids 1 occt " << OCC_VERSION_COMPLETE << "\n"; return; }
  if (cmd == "clear") { shapes.clear(); std::cout << "ok\n"; return; }
  long id;
  if (!(in >> id)) fail("a shape number is missing");
  if (cmd == "box") {
    gp_Pnt p = readPnt(in); double dx = readNum(in, "dx"), dy = readNum(in, "dy"), dz = readNum(in, "dz");
    put(id, BRepPrimAPI_MakeBox(p, dx, dy, dz).Shape());
  } else if (cmd == "wedge") {
    gp_Pnt p = readPnt(in); double dx = readNum(in, "dx"), dy = readNum(in, "dy"), dz = readNum(in, "dz");
    // AutoCAD's wedge: full height at x, sloping to nothing at x+dx
    // (Open CASCADE's wedge rises along its own Y axis: turn it so that is our Z, its Z our -Y)
    gp_Ax2 ax(gp_Pnt(p.X(), p.Y() + dy, p.Z()), -gp::DY(), gp::DX());
    put(id, BRepPrimAPI_MakeWedge(ax, dx, dz, dy, 0.0).Shape());
  } else if (cmd == "cyl" || cmd == "cone") {
    gp_Pnt o = readPnt(in); gp_Dir n = readDir(in); gp_Dir u = readDir(in);
    gp_Ax2 ax;
    try { ax = gp_Ax2(o, n, u); } catch (Standard_Failure&) { ax = gp_Ax2(o, n); }
    if (cmd == "cyl") {
      double r = readNum(in, "the radius"), h = readNum(in, "the height");
      put(id, BRepPrimAPI_MakeCylinder(ax, r, h).Shape());
    } else {
      double r1 = readNum(in, "the base radius"), r2 = readNum(in, "the top radius"), h = readNum(in, "the height");
      put(id, BRepPrimAPI_MakeCone(ax, r1, r2, h).Shape());
    }
  } else if (cmd == "sphere") {
    gp_Pnt c = readPnt(in); double r = readNum(in, "the radius");
    put(id, BRepPrimAPI_MakeSphere(c, r).Shape());
  } else if (cmd == "torus") {
    gp_Pnt o = readPnt(in); gp_Dir n = readDir(in);
    double R = readNum(in, "the radius"), r = readNum(in, "the tube radius");
    put(id, BRepPrimAPI_MakeTorus(gp_Ax2(o, n), R, r).Shape());
  } else if (cmd == "prism") {
    double dx = readNum(in, "dx"), dy = readNum(in, "dy"), dz = readNum(in, "dz");
    TopoDS_Shape prof = profileShape(readProfile());
    BRepPrimAPI_MakePrism mk(prof, gp_Vec(dx, dy, dz));
    if (!mk.IsDone()) fail("the extrusion failed");
    put(id, mk.Shape());
  } else if (cmd == "revol") {
    gp_Pnt o = readPnt(in); gp_Dir d = readDir(in); double a = readNum(in, "the angle");
    TopoDS_Shape prof = profileShape(readProfile());
    BRepPrimAPI_MakeRevol mk(prof, gp_Ax1(o, d), rad(a));
    if (!mk.IsDone()) fail("the revolution failed");
    put(id, mk.Shape());
  } else if (cmd == "fuse") {
    put(id, boolean<BRepAlgoAPI_Fuse>(readIds(in)));
  } else if (cmd == "cut") {
    put(id, boolean<BRepAlgoAPI_Cut>(readIds(in)));
  } else if (cmd == "common") {
    put(id, boolean<BRepAlgoAPI_Common>(readIds(in)));
  } else if (cmd == "halves") {
    long id2, a; if (!(in >> id2 >> a)) fail("shape numbers are missing");
    gp_Pnt o = readPnt(in); gp_Dir n = readDir(in);
    TopoDS_Face big = BRepBuilderAPI_MakeFace(gp_Pln(o, n), -1e9, 1e9, -1e9, 1e9).Face();
    TopoDS_Shape plus = BRepPrimAPI_MakeHalfSpace(big, o.Translated(gp_Vec(n))).Solid();
    TopoDS_Shape minus = BRepPrimAPI_MakeHalfSpace(big, o.Translated(-gp_Vec(n))).Solid();
    const TopoDS_Shape& s = get(a);
    BRepAlgoAPI_Common c1(s, plus), c2(s, minus);
    if (!c1.IsDone() || !c2.IsDone()) fail("the slice failed");
    put(id, tidy(c1.Shape()));
    put(id2, tidy(c2.Shape()));
  } else if (cmd == "fillet" || cmd == "chamfer") {
    long src; if (!(in >> src)) fail("the shape is missing");
    double r = readNum(in, "the size");
    const TopoDS_Shape& s = get(src);
    TopTools_IndexedMapOfShape edges;
    TopExp::MapShapes(s, TopAbs_EDGE, edges);
    std::vector<long> es = readIds(in);
    if (es.empty()) fail("no edges");
    if (cmd == "fillet") {
      BRepFilletAPI_MakeFillet mk(s);
      for (long e : es) { if (e < 1 || e > edges.Extent()) fail("no such edge"); mk.Add(r, TopoDS::Edge(edges(e))); }
      mk.Build();
      if (!mk.IsDone()) fail("the fillet can't be made with that radius");
      put(id, mk.Shape());
    } else {
      BRepFilletAPI_MakeChamfer mk(s);
      for (long e : es) { if (e < 1 || e > edges.Extent()) fail("no such edge"); mk.Add(r, TopoDS::Edge(edges(e))); }
      mk.Build();
      if (!mk.IsDone()) fail("the chamfer can't be made with that size");
      put(id, mk.Shape());
    }
  } else if (cmd == "shell") {
    long src; if (!(in >> src)) fail("the shape is missing");
    double t = readNum(in, "the thickness");
    const TopoDS_Shape& s = get(src);
    TopTools_IndexedMapOfShape faces;
    TopExp::MapShapes(s, TopAbs_FACE, faces);
    TopTools_ListOfShape open;
    for (long f : readIds(in)) { if (f < 1 || f > faces.Extent()) fail("no such face"); open.Append(faces(f)); }
    BRepOffsetAPI_MakeThickSolid mk;
    mk.MakeThickSolidByJoin(s, open, t, 1e-6);
    if (!mk.IsDone()) fail("the shell can't be made with that thickness");
    put(id, mk.Shape());
  } else if (cmd == "trsf") {
    long src; if (!(in >> src)) fail("the shape is missing");
    double m[12];
    for (double& v : m) v = readNum(in, "the transformation");
    const TopoDS_Shape& s = get(src);
    // a plain move / turn / even scale / mirror keeps the exact shapes; anything else is general
    gp_Mat r(m[0], m[1], m[2], m[4], m[5], m[6], m[8], m[9], m[10]);
    double det = r.Determinant();
    double sc = std::cbrt(std::fabs(det));
    gp_Mat rn = r; rn.Divide(sc);
    gp_Mat chk = rn.Multiplied(rn.Transposed());
    bool rigid = std::fabs(chk.Value(1, 1) - 1) < 1e-9 && std::fabs(chk.Value(2, 2) - 1) < 1e-9 && std::fabs(chk.Value(3, 3) - 1) < 1e-9 &&
                 std::fabs(chk.Value(1, 2)) < 1e-9 && std::fabs(chk.Value(1, 3)) < 1e-9 && std::fabs(chk.Value(2, 3)) < 1e-9;
    if (rigid) {
      gp_Trsf t;
      t.SetValues(m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9], m[10], m[11]);
      put(id, BRepBuilderAPI_Transform(s, t, Standard_True).Shape());
    } else {
      gp_GTrsf g;
      g.SetVectorialPart(r);
      g.SetTranslationPart(gp_XYZ(m[3], m[7], m[11]));
      put(id, BRepBuilderAPI_GTransform(s, g, Standard_True).Shape());
    }
  } else if (cmd == "polyhedron") {
    long n; if (!(in >> n)) fail("the face count is missing");
    BRepBuilderAPI_Sewing sew(1e-7);
    std::string l;
    for (long i = 0; i < n && std::getline(std::cin, l); i++) {
      std::istringstream fl(l);
      int k; if (!(fl >> k) || k < 3) continue;
      BRepBuilderAPI_MakePolygon poly;
      for (int j = 0; j < k; j++) poly.Add(readPnt(fl));
      poly.Close();
      if (!poly.IsDone()) continue;
      BRepBuilderAPI_MakeFace mf(poly.Wire(), Standard_True);
      if (mf.IsDone()) sew.Add(mf.Face());
    }
    sew.Perform();
    TopoDS_Shape sewn = sew.SewedShape();
    TopoDS_Shape result = sewn;
    // a closed skin becomes a solid
    if (sewn.ShapeType() == TopAbs_SHELL && BRep_Tool::IsClosed(sewn)) {
      BRepBuilderAPI_MakeSolid ms(TopoDS::Shell(sewn));
      if (ms.IsDone()) {
        ShapeFix_Shape fix(ms.Solid());
        fix.Perform();
        result = fix.Shape();
      }
    }
    put(id, tidy(result));
  } else if (cmd == "copy") {
    long src; if (!(in >> src)) fail("the shape is missing");
    put(id, BRepBuilderAPI_Copy(get(src)).Shape());
  } else if (cmd == "del") {
    shapes.erase(id);
  } else if (cmd == "mesh") {
    double defl = readNum(in, "the deflection"), ang = readNum(in, "the angle");
    doMesh(id, defl, ang);
  } else if (cmd == "props") {
    GProp_GProps v, a;
    BRepGProp::VolumeProperties(get(id), v);
    BRepGProp::SurfaceProperties(get(id), a);
    gp_Pnt c = v.CentreOfMass();
    std::cout.precision(17);
    std::cout << "ok " << v.Mass() << " " << a.Mass() << " " << c.X() << " " << c.Y() << " " << c.Z() << "\n";
    return;
  } else if (cmd == "bbox") {
    Bnd_Box b; BRepBndLib::Add(get(id), b);
    double x0, y0, z0, x1, y1, z1; b.Get(x0, y0, z0, x1, y1, z1);
    std::cout << "ok " << x0 << " " << y0 << " " << z0 << " " << x1 << " " << y1 << " " << z1 << "\n";
    return;
  } else if (cmd == "check") {
    BRepCheck_Analyzer an(get(id));
    int solids = 0; for (TopExp_Explorer ex(get(id), TopAbs_SOLID); ex.More(); ex.Next()) solids++;
    std::cout << "ok " << (an.IsValid() ? "valid" : "invalid") << " " << solids << "\n";
    return;
  } else if (cmd == "save") {
    std::ostringstream os;
    BRepTools::Write(get(id), os);
    std::string text = os.str();
    int n = 0; for (char ch : text) if (ch == '\n') n++;
    std::cout << "brep " << n << "\n" << text;
    if (!text.empty() && text.back() != '\n') std::cout << "\n";
  } else if (cmd == "load") {
    long n; if (!(in >> n)) fail("the line count is missing");
    std::string text, l;
    for (long i = 0; i < n && std::getline(std::cin, l); i++) { text += l; text += "\n"; }
    std::istringstream is(text);
    TopoDS_Shape s; BRep_Builder bb;
    BRepTools::Read(s, is, bb);
    put(id, s);
  } else if (cmd == "stepwrite") {
    std::vector<long> ids; ids.push_back(id);
    for (long x : readIds(in)) ids.push_back(x);
    std::string path; std::getline(std::cin, path);
    STEPControl_Writer w;
    Interface_Static::SetCVal("write.step.schema", "AP214IS");
    for (long x : ids) if (w.Transfer(get(x), STEPControl_AsIs) != IFSelect_RetDone) fail("a shape couldn't be written as STEP");
    if (w.Write(path.c_str()) != IFSelect_RetDone) fail("the STEP file couldn't be written");
  } else if (cmd == "stepread") {
    std::string path; std::getline(std::cin, path);
    STEPControl_Reader r;
    if (r.ReadFile(path.c_str()) != IFSelect_RetDone) fail("the file couldn't be read as STEP");
    r.TransferRoots();
    TopoDS_Shape all = r.OneShape();
    long next = id; std::ostringstream made;
    for (TopExp_Explorer ex(all, TopAbs_SOLID); ex.More(); ex.Next()) { put(next, ex.Current()); made << " " << next++; }
    // loose faces and shells (surfaces) too
    for (TopExp_Explorer ex(all, TopAbs_SHELL, TopAbs_SOLID); ex.More(); ex.Next()) { put(next, ex.Current()); made << " " << next++; }
    for (TopExp_Explorer ex(all, TopAbs_FACE, TopAbs_SHELL); ex.More(); ex.Next()) { put(next, ex.Current()); made << " " << next++; }
    std::cout << "ok" << made.str() << "\n";
    return;
  } else fail("unknown request: " + cmd);
  std::cout << "ok\n";
}

int main() {
  std::ios::sync_with_stdio(false);
  std::cout.precision(15);
  Message::DefaultMessenger()->RemovePrinters(STANDARD_TYPE(Message_Printer));  // keep stdout for answers
  std::string line;
  while (std::getline(std::cin, line)) {
    try {
      handle(line);
    } catch (Standard_Failure& e) {
      std::cout << "err " << (e.GetMessageString() && *e.GetMessageString() ? e.GetMessageString() : "the geometry engine failed") << "\n";
    } catch (std::exception& e) {
      std::cout << "err " << e.what() << "\n";
    }
    std::cout.flush();
  }
  return 0;
}
