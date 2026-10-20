# LispDraft

![LispDraft moving a hole on a drawing produced by an AutoLISP program](docs/screenshot.png)

A 2D drafting program written in Common Lisp that works like AutoCAD and runs AutoLISP.
Same command names and aliases, the same prompts, a command line with the same input rules,
object snaps, noun/verb selection, layers, blocks, dimensions, and DXF in and out. Your existing
AutoLISP routines (`defun c:...`, `command`, `entget`/`entmod`, `ssget` filters, `initget`/`getkword`,
`*error*` handlers) run unchanged in most cases.

## Install and run (Ubuntu / Debian)

```sh
sudo apt install sbcl tk
cd lispdraft
./lispdraft                      # GUI
./lispdraft drawing.dxf          # open a drawing
./lispdraft examples/tools.lsp   # load AutoLISP at startup
./lispdraft --tty                # terminal-only mode (command line + AutoLISP, no window)
```

The first start compiles the sources (a few seconds); later starts use the cached build in `build/`.
Nothing else is needed: no Quicklisp, no libraries. The GUI is Tk, driven over a pipe from Lisp.
If `wish` isn't on your PATH, set `LISPDRAFT_WISH=/path/to/wish`.

Any `acaddoc.lsp` or `lispdraft.lsp` in the current directory, and `~/.lispdraft.lsp`, are loaded
at startup, and `S::STARTUP` is called if defined, just like AutoCAD.

## Using it

It behaves like AutoCAD's command line:

- Type a command or alias (`L`, `C`, `PL`, `TR` ...) and press Enter or Space. Enter on an empty
  prompt repeats the last command. Esc cancels. Right-click acts as Enter.
- Coordinates: `3,4`  `@2,0` (relative)  `@5<45` (polar)  or a bare number for direct distance
  along the rubber band. Distances accept feet-inches (`2'6`) and fractions (`1 1/2`).
- Type `END`, `MID`, `CEN`, `INT`, `PER`, `TAN`, `QUA`, `NOD`, `INS`, `NEA` or `NON` at a point
  prompt for a one-time snap override.
- F3 OSNAP, F7 GRID, F8 ORTHO, F9 SNAP (or click the status bar toggles).
- Mouse wheel zooms at the cursor, middle-drag pans, middle double-click zooms extents.
- Click objects at the `Command:` prompt to select them first (noun/verb), drag left-to-right for a
  window, right-to-left for a crossing window, then run MOVE, ERASE (or press Delete), etc.
- Type AutoLISP straight at the prompt: `(+ 1 2)`, `!myvar`, or a multi-line expression (the
  prompt changes to `(_>` until the parentheses balance). Expressions also work inside prompts,
  e.g. `(polar (getvar "lastpoint") 0 5)` at "Specify next point".

### Commands

| Area | Commands (aliases) |
|---|---|
| Draw | LINE (L), PLINE (PL) with Arc/Width, RECTANG (REC), POLYGON (POL), CIRCLE (C) incl. 2P/3P, ARC (A) 3-point/Center/End, ELLIPSE (EL), POINT (PO), TEXT (DT), MTEXT (T), DIMLINEAR (DLI), DIMALIGNED (DAL), BLOCK (B), INSERT (I) |
| Modify | ERASE (E), OOPS, MOVE (M), COPY (CO), ROTATE (RO), SCALE (SC), MIRROR (MI), OFFSET (O), TRIM (TR), EXTEND (EX), BREAK (BR), FILLET (F), CHAMFER (CHA), JOIN (J), STRETCH (S), ARRAY (AR) rectangular/polar, EXPLODE (X), CHPROP, MATCHPROP (MA), COLOR, TEXTEDIT (ED), DIVIDE (DIV), COPYCLIP/PASTECLIP |
| View | ZOOM (Z) Extents/All/Window/Previous/Center, PAN (P), REGEN (RE) |
| Inquiry | DIST (DI), ID, AREA (AA), LIST (LI) |
| Settings | LAYER (LA; dialog) and -LAYER (command line), LAYMCUR, OSNAP (OS), ORTHO, GRID, SNAP, UNITS, SETVAR, or type any system variable name |
| File | NEW, OPEN, QSAVE, SAVEAS, DXFIN, DXFOUT, APPLOAD (AP), SCRIPT (SCR), PURGE (PU), QUIT |
| Other | U, UNDO, REDO, SELECTALL (Ctrl+A), HELP |

### AutoLISP

The interpreter follows AutoLISP semantics, not Common Lisp's: dynamic scoping with
`(defun f (args / locals) ...)`, case-insensitive symbols, functions and variables in one namespace,
integer division `(/ 7 2)` → 3, 32-bit-style integers and doubles, `nil`/`T`, AutoCAD's real-number
printing, and the `*error*` mechanism (including a local `*error*` inside a function).

About 250 functions plus the special forms are built in, including:

- **Core**: all `c[ad]r` combinations, `cons list append reverse nth member assoc subst last length
  mapcar apply lambda foreach repeat while cond if progn and or not null eval read set setq defun defun-q`
- **Math**: `+ - * / rem 1+ 1- abs fix float max min gcd sqrt expt exp log sin cos atan ~ logand logior lsh boole`
- **Strings**: `strcat strlen substr strcase itoa atoi atof rtos distof angtos angtof chr ascii wcmatch`
  and the `vl-string-*` family
- **Lists (Visual LISP)**: `vl-sort vl-sort-i vl-remove vl-remove-if vl-remove-if-not vl-position
  vl-member-if vl-some vl-every vl-catch-all-apply vl-catch-all-error-p vl-princ-to-string ...`
- **Geometry**: `distance angle polar inters trans osnap textbox`
- **Input**: `getpoint getcorner getdist getangle getorient getreal getint getstring getkword initget
  entsel nentsel grread getfiled alert`
- **Drawing database**: `entget entmod entmake entmakex entdel entlast entnext entupd handent
  tblsearch tblnext ssget ssadd ssdel sslength ssname ssmemb sssetfirst ssgetfirst`
- **ssget** modes: interactive, `"X"`, `"W"`, `"C"`, `"WP"`, `"CP"`, `"F"`, `"L"`, `"P"`, `"I"`, `":S"`,
  with filter lists including wildcards and `-4` operators (`<OR`, `<AND`, `<NOT`, `>`, `<=`, `&` ...)
- **Commands**: `command`, `command-s`, `vl-cmdf`, including `pause` and commands left active across
  several `command` calls, as in AutoCAD (`(command "line" p1 p2)` then `(command "")`), and `CMDACTIVE`
- **Files & system**: `open close read-line write-line read-char write-char findfile load getenv
  getvar setvar vl-directory-files vl-file-delete ...`

### Example programs

- `examples/demo-plate.lsp` — draws a dimensioned plate entirely through `command`. Load it and type `PLATE`.
- `examples/tools.lsp` — TLEN (total length), BCIRC (bolt circle with `initget`/`getkword` and `*error*`),
  NUM (auto-numbering), LAYOFF, CL (center marks).

## DXF

LispDraft saves AutoCAD R12 DXF (AC1009), which every AutoCAD version and nearly every CAD program
opens. It reads ASCII DXF from R12 through current releases: lines, circles, arcs, points,
LWPOLYLINE/POLYLINE (with bulges and widths), TEXT, MTEXT, INSERT with ATTRIBs, nested blocks,
DIMENSION (drawn from its block), ELLIPSE, SPLINE and SOLID; layers with color, on/off, freeze and lock.

## What's not there (yet)

Compared with AutoCAD, this first version does not have: DWG files (proprietary), paper space and
layouts, linetypes and lineweights (everything draws continuous), hatching (HATCH entities are kept
but not drawn), text styles and fonts, dimension styles beyond the basic linear/aligned dims, grips,
UCS, 3D, plotting, and ActiveX (`vla-*`/`vlax-*`) or DCL dialog functions. TRIM/EXTEND work on lines,
arcs and circles (not yet on polylines). FILLET and CHAMFER work on two lines.

## Tests

```sh
./lispdraft --tty tests/test-alisp.lsp tests/test-cad.lsp tests/test-cad2.lsp < /dev/null
```

## Layout

```
lispdraft          launcher script
load.lisp          compiles & loads the sources (no ASDF needed); lispdraft.asd is provided too
src/package.lisp   packages
src/geom.lisp      points, intersections, transforms
src/db.lisp        drawing database (entities are DXF association lists, like entget)
src/dxf.lisp       DXF reader/writer
src/alisp.lisp     AutoLISP reader, printer, evaluator and core functions
src/input.lisp     prompts, coordinate entry, keywords, object snaps, selection, (command)
src/gui.lisp       Tk front end: view, rendering, events
src/gui.tcl        Tk window layout
src/commands.lisp  drawing commands
src/modify.lisp    editing commands
src/utilcmds.lisp  view/file/layer/block/inquiry commands; entget/tblsearch/getvar...
src/main.lisp      command loop and entry point
```
