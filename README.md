# Fancy Pencil

<img src="support/icons/fancypencil-128.png" alt="Fancy Pencil icon" width="96" align="right">

![Fancy Pencil: a drawing made by an AutoLISP program, with grips and the Properties palette](docs/screenshot.png)

A 2D drafting program written in Common Lisp that works like AutoCAD and runs AutoLISP.
Same command names and aliases, the same prompts, a command line with the same input rules,
object snaps, noun/verb selection, layers, blocks, dimensions, and DXF in and out. Your existing
AutoLISP routines (`defun c:...`, `command`, `entget`/`entmod`, `ssget` filters, `initget`/`getkword`,
`*error*` handlers) run unchanged in most cases.

## Install and run (Ubuntu / Debian)

```sh
sudo apt install sbcl tk
cd fancypencil
./fancypencil                      # GUI
./fancypencil drawing.dxf          # open a drawing
./fancypencil examples/tools.lsp   # load AutoLISP at startup
./fancypencil --tty                # terminal-only mode (command line + AutoLISP, no window)
```

The first start compiles the sources (a few seconds); later starts use the cached build in `build/`.
Nothing else is needed: no Quicklisp, no libraries. The GUI is Tk, driven over a pipe from Lisp.
If `wish` isn't on your PATH, set `FANCYPENCIL_WISH=/path/to/wish`.
When started from the app launcher, error messages go to `~/.cache/fancypencil.log`.

Open/Save dialogs use Fancy Pencil's own file chooser, which always opens in front of the drawing:
your home folders, every mounted drive and your recent folders down the left, the folder's files
(with size and date) on the right, and a type filter. Type a path in the bar at the top and press
Enter to go straight there. (The desktop's chooser can be had with `FANCYPENCIL_FILEDIALOG=zenity`
or `kdialog`, but it is a separate program and Ubuntu often opens it behind Fancy Pencil;
`FANCYPENCIL_FILEDIALOG=tk` gives Tk's plain dialog.)

### Add it to the application launcher

```sh
sh install.sh              # adds "Fancy Pencil" with its icon to your app menu / dash
sh install.sh --uninstall  # removes it again
```

This installs the icon (`support/icons`, SVG plus 16–512 px PNGs) into
`~/.local/share/icons/hicolor` and a `fancypencil.desktop` entry into `~/.local/share/applications`,
pointing at wherever this folder is. If you move the folder, run `install.sh` again. Once it shows
in the launcher you can right-click it and choose *Add to Favorites* to pin it to the dock. DXF files
can be opened with it from the file manager (*Open With → Fancy Pencil*).

Fancy Pencil was called LispDraft before 0.3. Aliases you saved with ALIASEDIT under the old name
(`~/.config/lispdraft/lispdraft.pgp`) and a `~/.lispdraft.lsp` startup file are still picked up.

### Right-click, object snaps, ALIGN, dimension styles

- **Right-click on the drawing** at the Command: prompt opens a menu: Repeat last command, Recent Input,
  and with objects selected Erase, Move, Copy, Scale, Rotate, Mirror, Align, Array, Cut/Copy to the
  clipboard, Deselect All and Properties (Undo/Redo, Zoom, Paste, Select All when nothing is selected).
  While a command is running, right-click is Enter, as in AutoCAD.
- **Object snaps:** OSNAP (OS, DS, Tools → Object Snap Settings, or right-click the OSNAP button) opens
  a panel with Endpoint, Midpoint, Center, Node, Quadrant, Intersection, Insertion, Perpendicular,
  Tangent and Nearest. **Shift + right-click** while picking a point gives a one-time snap menu.
  -OSNAP sets them at the command line.
- **ALIGN (AL):** pick source/destination point pairs; one pair moves, two pairs move and rotate, and
  optionally scale to fit.
- **Dimension Style Manager (D, DIMSTYLE):** styles list with Set Current / New (start from any style) /
  Delete, tabs for Lines, Symbols and Arrows, Text, Fit and Primary Units, and a live preview. Arrowheads:
  closed filled, closed blank, open, right angle, dot, small dot, **architectural tick**, oblique, none.
  Changing a style updates the dimensions that use it. EXPLODE breaks a dimension into its parts.
  Arrowheads are saved the way AutoCAD stores them (`_ArchTick`, `_Oblique` ... blocks referenced by
  the dimension style), in both DXF formats and so in DWG, and are read back from AutoCAD drawings.

### What survives opening an AutoCAD drawing

Dimensions keep the appearance AutoCAD saved with them (and are only redrawn by Fancy Pencil when you
edit one); leaders and multileaders (callout arrows) are kept and editable; draw order ("Send to Back") is honoured; hatches
keep their island style and full-circle boundaries. Not yet supported: raster images, PDF/DWF/DGN
underlays, regions, 3D solids, tables and paper-space layouts. When a drawing contains
any of these, opening it says so, and saving over that file asks first.

### Safety copies (automatic save) and recovery

Every 10 minutes, each open drawing with unsaved changes is copied to
`~/.local/share/fancypencil/autosave/`. Your own file is not touched, and nothing is copied while a
drawing has no changes. The copies are deleted when you save or close normally, so any left behind
mean Fancy Pencil stopped unexpectedly (crash, power cut, forced quit): the next start lists them
and offers to open them. A recovered drawing opens under its own name, marked as changed, so Save
puts it back where it belongs. File > Recover Safety Copies (DRAWINGRECOVERY) opens or deletes them
any time. Type `SAVETIME` to change the minutes between copies (0 turns them off).

Saving over a drawing also keeps the version it replaces as a `.bak` file beside it (rename it to
.dxf or .dwg to open it; Fancy Pencil also opens .bak files directly).

### Each drawing opens where you left it

When you close a drawing (its tab, or Fancy Pencil), its view is remembered: where you were zoomed
and panned, which tab (Model or a layout) was showing, and any 3D view. The next time you open
that file it comes back the same way; a drawing opened for the first time shows all of it. The
views are kept in `~/.config/fancypencil/views.lisp` (the last 300 drawings), not in the drawing,
so this works for DWG files, read-only folders and drawings closed without saving.

### Several drawings at once

Every drawing opens in its own tab above the drawing area, as in AutoCAD. OPEN and NEW add a tab
(OPEN reuses an untouched empty Drawing1, and switches to a file that is already open); click a tab
to switch, its × (or a middle-click) to close it, + for a new drawing. Ctrl+Tab / Ctrl+Shift+Tab cycle
through them, Ctrl+F4 or CLOSE closes the current one, CLOSEALL closes all, and the Window menu lists
them. Each drawing keeps its own view, selection, undo history, layers and settings; program-wide
settings (object snaps, Ortho, grid, pickbox ...) follow you between tabs. COPYCLIP / PASTECLIP work
between drawings. QUIT offers to save every changed drawing. `./fancypencil a.dxf b.dwg` opens both.

### Opening and saving DWG files

DWG is AutoCAD's closed binary format, so Fancy Pencil converts it with a free converter:

- **ODA File Converter** (recommended): reads and writes every AutoCAD version. Download the Ubuntu
  `.deb` from opendesign.com ("ODA File Converter"), then `sudo apt install ./ODAFileConverter_*.deb`.
- **LibreDWG** `dwg2dxf` (open source): opening only. Not packaged for Ubuntu 24.04; build it from
  github.com/LibreDWG/libredwg.

Fancy Pencil finds whichever is installed (or set `FANCYPENCIL_DWG_CONVERTER=/path/to/program`).
OPEN shows DWG and DXF files; a DWG opens like any drawing, and SAVE writes it back as DWG (AutoCAD
2018 format) with the previous file kept as `name.bak`. Without ODA File Converter, saving asks for a
DXF name instead and never touches the DWG.

Any `acaddoc.lsp` or `fancypencil.lsp` in the current directory, and `~/.fancypencil.lsp`, are loaded
at startup, and `S::STARTUP` is called if defined, just like AutoCAD.

## Using it

It behaves like AutoCAD's command line:

- Type a command or alias (`L`, `C`, `PL`, `TR` ...) and press Enter or Space. Enter on an empty
  prompt repeats the last command. Esc cancels. Right-click acts as Enter.
- Coordinates: `3,4`  `@2,0` (relative)  `@5<45` (polar)  or a bare number for direct distance.
  As with AutoCAD's dynamic input, a second or later point typed at the command line is relative
  to the last point: RECTANG, a click, then `11,8.5` draws an 11 x 8.5 rectangle; LINE `0,0` then
  `4,0` then `0,3` goes 4 across and 3 up. Put `#` in front for an absolute point (`#10,10`).
  `SETVAR DYNPICOORDS 1` makes typed points absolute again (remembered). Points given by AutoLISP
  `(command ...)` and scripts are always absolute.
  along the rubber band. Distances accept feet-inches (`2'6`) and fractions (`1 1/2`).
- Type `END`, `MID`, `CEN`, `INT`, `PER`, `TAN`, `QUA`, `NOD`, `INS`, `NEA` or `NON` at a point
  prompt for a one-time snap override.
- F3 OSNAP, F7 GRID, F8 ORTHO, F9 SNAP (or click the status bar toggles). In a tilted 3D view (the
  isometric views) Ortho follows the X and Y axes of the level plane you are drawing on, so lines
  keep their height; in Front/Back/Left/Right it follows the screen, so up the screen is up in Z.
- Mouse wheel zooms at the cursor, middle-drag pans, middle double-click zooms extents.
- Click objects at the `Command:` prompt to select them first (noun/verb), drag left-to-right for a
  window, right-to-left for a crossing window, then run MOVE, ERASE (or press Delete), etc.
- Type AutoLISP straight at the prompt: `(+ 1 2)`, `!myvar`, or a multi-line expression (the
  prompt changes to `(_>` until the parentheses balance). Expressions also work inside prompts,
  e.g. `(polar (getvar "lastpoint") 0 5)` at "Specify next point".

### Commands

| Area | Commands (aliases) |
|---|---|
| Draw | LINE (L), PLINE (PL) with Arc/Width, RECTANG (REC), POLYGON (POL), CIRCLE (C) incl. 2P/3P, ARC (A) 3-point/Center/End, ELLIPSE (EL), POINT (PO), TEXT (DT), MTEXT (T), HATCH (H, BH), REVCLOUD (RC), BLOCK (B), INSERT (I) |
| Modify | ERASE (E), OOPS, MOVE (M), COPY (CO), ROTATE (RO), SCALE (SC), MIRROR (MI), OFFSET (O), TRIM (TR), EXTEND (EX), BREAK (BR), FILLET (F), CHAMFER (CHA), JOIN (J), STRETCH (S), ARRAY (AR), EXPLODE (X), PEDIT (PE), HATCHEDIT (HE), CHPROP, MATCHPROP (MA), COLOR, TEXTEDIT (ED), DIVIDE (DIV), COPYCLIP/PASTECLIP |
| Dimension | DIMLINEAR (DLI), DIMALIGNED (DAL), DIMRADIUS (DRA), DIMDIAMETER (DDI), DIMANGULAR (DAN), DIMCONTINUE (DCO), DIMBASELINE (DBA), DIMSTYLE (D) |
| View | ZOOM (Z) Extents/All/Window/Previous/Center, PAN (P), REGEN (RE) |
| Inquiry | DIST (DI), ID, AREA (AA), LIST (LI), PROPERTIES (PR, Ctrl+1) |
| Settings | LAYER (LA; dialog) and -LAYER, LAYMCUR, LINETYPE (LT), LTSCALE (LTS), LWEIGHT (LW), OSNAP (OS), ORTHO, GRID, SNAP, UNITS, HPSET, SETVAR, or type any system variable name |
| File | NEW, OPEN, QSAVE, SAVEAS, SAVEASR12, DXFIN, DXFOUT, APPLOAD (AP), SCRIPT (SCR), PURGE (PU), QUIT |
| Aliases | ALIASEDIT, REINIT |
| Express Tools | LAYISO, LAYUNISO, LAYOFF, LAYFRZ, LAYON, LAYTHW, LAYLCK, LAYULK, LAYCUR, LAYMCH, LAYMRG, LAYDEL, OVERKILL, BREAKLINE, EXTRIM, MPEDIT, TCASE, TCOUNT, TXT2MTXT, MOCORO, COPYM, FLATTEN, SELECTSIMILAR, SSX, GETSEL, XLIST, BURST |
| Other | U, UNDO, REDO, SELECTALL (Ctrl+A), HELP |

**Text is typed on the drawing.** MTEXT (MT, T): pick two corners (or use Height/Justify/Rotation/Width),
and an editor opens in that box; text wraps at the box width. Enter starts a new line; Ctrl+Enter, OK or
a click outside finishes; Esc cancels. TEXT (DT) types each line in place at its insertion point;
Enter on an empty line ends. Double-click any text, or use TEXTEDIT (ED), to edit it the same way.
From AutoLISP or scripts, the same commands take their text on the command line.

The MTEXT editor has a formatting bar: text style, font, height, Bold/Italic/Underline (also Ctrl+B/I/U),
color, left/center/right alignment, and a Symbols menu (°, ±, ⌀ ...). Font, height, Bold/Italic/Underline
and color change only the selected characters; with nothing selected they set what you type next, and
moving the cursor picks up the format of the text there. Text style and alignment apply to the whole
note. Each part keeps its format on the drawing, in plots and when saved (as AutoCAD's inline MTEXT
codes, so formatting made in AutoCAD opens here too). Right-click in the editor or on the command
line for Cut/Copy/Paste, Select All, symbols and change case. A selected MTEXT shows grips on the top
corners of its box: drag one (object snaps work as usual) to change the wrap width.

STYLE (ST, or Format → Text Style) manages text styles: font (any installed font, or an AutoCAD .shx
name, shown with a similar font), bold, italic, fixed height, width factor and oblique angle; -STYLE
does the same at the command line. Styles are saved to and read from DXF, including the TrueType
font and bold/italic flags AutoCAD stores. Width factor and oblique angle are kept in the file but
text is drawn without them for now.

**Adding fonts.** Put .ttf / .otf files in a folder named `fonts` next to the `fancypencil` folder
(or inside it, or in `~/.config/fancypencil/fonts`). Each time Fancy Pencil starts it installs any new
ones for your user (in `~/.local/share/fonts/fancypencil`), and they appear in the Font lists.
Drawings that name AutoCAD's Century Gothic files (GOTHIC.TTF, GOTHICB.TTF ...) use Century Gothic
when it is installed, otherwise a free look-alike (URW Gothic / TeX Gyre Adventor) if there is one.
Fonts are not part of the Fancy Pencil download: commercial fonts stay under their own licenses.

### Toolbars

Every menu has a toolbar of command icons, and so does each sub-menu (3D Views, Visual Styles,
Working Plane, Viewports, Draw Order, Text Case ...). The Menus toolbar has one icon per menu that
opens that menu's whole tree of commands. Common commands have drawn icons; the rest show a
lettered tile. Rest the mouse on an icon to see its name.

- Turn toolbars on and off with View > Toolbars, by right-clicking any toolbar, or with the
  TOOLBAR command (TO). "Reset Toolbar Layout" brings back the standard layout (Menus and Draw on
  the left, Modify on the right).
- Drag a toolbar by its dotted grip to move it. Drop it near an edge of the drawing to dock it
  there (top, bottom, left or right), beside or between other toolbars, or in a new row or column
  of its own; drop it in the middle of the drawing to float it as a small window.
- **Toolbar Options** (in View > Toolbars, the right-click menu, or the TOOLBAROPTIONS command) is a
  small floating window that stays open while you work: sliders for icon size (16 to 64 pixels),
  line weight and spacing, and colors for the toolbar background, icon lines, highlights and filled
  shapes (pick from the swatches, or More... for any color), plus ready-made color schemes (Dark,
  Light, High contrast, Blueprint, Warm, Green screen). Changes show at once, with a preview;
  "Standard Look" puts everything back. Icons are drawn as lines, so they stay sharp at any size.
- Long toolbars wrap into extra columns (or rows) to fit. The layout is remembered between
  sessions, in `~/.config/fancypencil/toolbars.tcl`.

### Selection, grips and the Properties palette

Click objects at the `Command:` prompt to select them first. Selected objects show blue grips;
click a grip to make it hot (red) and drag. Enter or Space cycles the grip mode through
STRETCH, MOVE, ROTATE, SCALE and MIRROR, with the Base point, Copy, Undo and eXit options AutoCAD
has. Selecting several objects that share an endpoint and dragging that grip moves them all.

PROPERTIES (or Ctrl+1) opens the palette on the right. With nothing selected it shows and edits
the current color, layer, linetype and lineweight; with a selection it shows the shared properties
and, for a single object type, its geometry (line endpoints and length, circle radius and area,
text contents, hatch pattern, block scale and so on). Edit a field and press Enter. The toolbar's
Properties panel works the same way for color, linetype and lineweight.

### Linetypes, lineweights and hatching

LINETYPE loads the standard AutoCAD linetypes (CENTER, HIDDEN, DASHED, PHANTOM, DOT, DASHDOT,
BORDER, DIVIDE and their 2 / X2 variants) or reads any `.lin` file. Linetypes can be set per layer
(Layer Properties or `-LAYER Ltype`) or per object, scaled with LTSCALE and per object with
CELTSCALE / "Linetype scale". Lineweights work the same way; turn their display on with the LWT
button on the status bar.

HATCH works like AutoCAD's: pick a point inside an area and the boundary is found automatically,
with closed objects inside it left as islands; or choose "Select objects". Patterns include SOLID,
ANSI31–38, LINE, NET, NET3, DASH, DOTS, BRICK, SQUARE, CROSS, ANGLE, HONEY, ZIGZAG, EARTH, STEEL and
INSUL. HATCHEDIT changes pattern, scale and angle. Hatch patterns read from DXF files are drawn
exactly as defined.

**The Hatch dialog** (HATCH, H): choose the pattern from the list or from the palette of swatches
(the **...** button, or click the swatch), set the angle and scale (the swatch turns with the
angle), and the draw order (behind its boundary, the default as in AutoCAD; to the back; to the
front; or as drawn). **Add: Pick points** and **Add: Select objects** go to the drawing to add
boundaries; the hatch shows as you add them, and Enter brings the dialog back. **Remove all
boundaries** starts again; **OK** keeps the hatch, **Cancel** drops it. HATCHEDIT (HE) opens the
same dialog for an existing hatch. **-HATCH** asks at the command line instead (for scripts).

**LIST** (LI) shows each selected object's type, layer, space, color, linetype and lineweight,
and its geometry (lengths, areas, radii, vertices, a hatch's pattern and area, a solid's volume,
a dimension's measurement ...) in a window you can scroll and copy from, as well as on the
command line.

### Recent files

File → Recent Files lists the last 12 drawings you opened or saved (newest first, with the
folder they're in); click one to open it. A file that has been moved or deleted is taken off the
list when you click it. Clear Recent Files empties the list. It's kept in
`~/.config/fancypencil/recent.lisp`.

### Units (UNITS, Format → Units)

Choose how lengths are shown — Architectural (`12'-6 1/2"`), Decimal, Engineering (`12'-6.50"`),
Fractional or Scientific — and their precision; angles in decimal degrees, deg/min/sec, grads,
radians or surveyor's units; and the insertion units. The Sample output shows the result as you
choose. Coordinates in the status bar, distances, LIST, AREA (square inches and square feet),
the Properties palette and the rollover tips all follow it. The units are saved in the drawing
(and read from AutoCAD drawings), together with LTSCALE and the other drawing settings.

### Rollover tips

Rest the cursor on an object for half a second and a small box shows what it is and its sizes:
a line's length and angle, a rectangle's width × height and area, a circle's radius, diameter,
circumference and area, an arc's length, text style and height, a block's scale and size, a
hatch's pattern and area. Turn them off in Tools → Cursor & Selection (or `ROLLOVERTIPS` 0).

### Blocks (BLOCK, B)

Draw → Block: Make (or `B`) opens the Block Definition dialog: type a name, **Pick point** for the
base point (or type X and Y), **Select objects**, and choose whether the objects are retained,
converted to the block, or deleted. Objects selected before you start are used automatically.
`-BLOCK` does the same at the command line. Insert copies with INSERT.

### Properties and Quick Properties

**Properties** (Ctrl+1, Tools → Properties) is the panel docked at the right: every property of the
selected objects. **Quick Properties** (right-click a selection → Quick Properties, or `QP`) is a
small panel that opens next to the cursor with just the main ones (color, layer, linetype, and the
key sizes: length, radius, text, scale...). Both have a list at the top — "All (15)", "Line (10)",
"Circle (2)"... — to look at and change only one kind of object in a mixed selection.

### Drawing Limits (LIMITS, Format → Drawing Limits...)

The drawing's size: the rectangle that ZOOM All shows and Plot → Limits prints. Type the corners,
or pick a sheet size and scale (e.g. ARCH D at 1/4" = 1'-0" gives 144'-0" × 96'-0") and click Use
sheet. Options: show the grid only inside the limits, and refuse points picked outside them
(LIMCHECK). The limits are saved in the drawing.

### Point Style (DDPTYPE, Format → Point Style...)

The Point Style dialog shows the 20 point symbols (dot, none, plus, X, tick, and each with a
circle, a square, or both); click one (double-click to choose it at once). Point size is set
relative to the screen (a percentage of the drawing area's height, so points look the same at any
zoom) or in absolute drawing units. Points are drawn — and plotted — with the chosen symbol.

### Colors

Color choices (Properties panel → Select Color..., the Properties palette, the Layer Properties
Manager's Color button or Color cell, and COLOR) open the **Select Color** dialog: all 255
AutoCAD index colors as swatches, the standard colors and grays, ByLayer and ByBlock; click a
swatch (double-click to choose it at once) or type a number or name.

### Settings that stay put

A drawing opens on the layer that was current when it was saved. Object Snap settings belong to
you rather than to a drawing: the same snaps are on in every drawing, and they're remembered the
next time Fancy Pencil starts (with the cursor settings, in `~/.config/fancypencil/settings.lisp`).

### Leaders (LEADER, alias LE, QL)

Annotation → **Leader**, Dimension → Leader, or type LE. Click the arrow tip, then the points of
the leader line, then press Enter and type the note (Enter for a new line; click outside or
Ctrl+Enter to finish). While picking points:

* **Spline** / **STraight** switch the line between a smooth curve and straight segments
  (type S or ST at any prompt; the choice is remembered for the next leader).
* **Arrow** / **None** turn the arrowhead on or off.
* **NOText** finishes the leader without a note; **Undo** takes back the last point.

A short horizontal landing line is added before the note when the last segment slopes, and the
note goes on the side the leader points to. Arrow size and text height follow the current
dimension style (DIMASZ, DIMTXT, DIMSCALE). Leaders are saved as real AutoCAD LEADER objects.

**Arrow size.** Three ways:

* For one or more existing leaders: select them and change **Arrow size** (or **Overall scale**)
  in Properties or Quick Properties. Arrowhead on/off and Path (Straight/Spline) are there too.
* For the next leaders you draw: type `DIMASZ` and enter a size (or `DIMSCALE` for arrow and
  text together). Each new leader keeps that size, as an override on that leader only.
* For every leader and dimension using a style: Dimension → Dimension Style..., Arrow size.

### Splines (SPLINE, alias SPL)

Draw > Spline, or SPLINE: pick points and press Enter. As in AutoCAD 2012 and later:

* **Method Fit** (the default): the curve passes through every point picked. **Knots** sets how the
  points are spaced along it: Chord (by distance), Square root, or Uniform. **start Tangency** /
  **end Tangency** set the direction it leaves the first point and reaches the last; **Close** joins
  it smoothly back to the start; **Undo** takes back the last point; **toLerance** is kept for AutoCAD.
* **Method CV**: the points are control vertices; the curve is pulled toward them. **Degree** 1 to 10
  (3 to start with).
* **Object**: turns polylines into splines through their vertices.

A fit spline's grips are its fit points: dragging one reshapes the curve through the new point. A
CV spline's grips are its control vertices. Splines move, copy, rotate, scale, mirror and offset
like other objects and are saved as AutoCAD SPLINE objects.

### QuickCalc and CAL (Ctrl+8)

Tools > QuickCalc (QUICKCALC, QC or Ctrl+8) opens a calculator beside the drawing. Type an
expression and press Enter (or =); the history keeps each one (double-click to use it again).

* **Number Pad** and **Scientific** keys: memory (MS MR M+ MC), powers, roots, sin cos tan and their
  inverses (in degrees), logs, r2d / d2r, rounding, and the [ , < ] of points.
* **Units**: lengths, areas, volumes and angles, converted as you type; put the answer in the calculator.
* **Variables**: `name=expression` keeps a value (AutoLISP sees it too); pi and Phi are known.
* Toolbar: **Point**, **Distance**, **Angle** and **Intersection** read them from the drawing.
* **Paste to Command Line** puts the answer on the command line.

In the middle of a command, type `'QC` at a prompt for a point, distance, angle or number: work
out the value, then **Apply** gives it to the command. `'CAL` asks for an expression on the command
line instead, and CAL at the Command prompt prints the answer.

Expressions read as AutoCAD's CAL does: `+ - * / ^` and brackets; feet-inches (`5'6"`); points
`[x,y,z]` and `[dist<angle]` (+ and - add them, a number times a point scales it, `*` between points
is the dot product, `&` the cross product); `dist(p1,p2)`, `ang(p1,p2)`, `cvunit(1,"inch","mm")`,
`xof` `yof` `zof`, `min` `max`; and points picked in the drawing: `cur`, `end`, `mid`, `cen`, `int`,
`nea`, `qua`, `per`, `nod`, `ins`, with `rad` (a circle's radius), `dee` (distance between two
endpoints), `mee` (the midpoint between two endpoints) and `ille` (where two lines meet). For
example `dist(end,end)/2` or `rad*2`.

### DWF and DGN underlays (DWFATTACH, DGNATTACH)

File > Attach DWF / DWFx... and Attach DGN (MicroStation)... place another file's drawing in this
one as an underlay, as AutoCAD's DWFATTACH and DGNATTACH do: pick the insertion point, scale and
rotation. It shows inside its frame, is picked by its frame, and moves, scales and rotates as one
object; it is saved as a DWFUNDERLAY / DGNUNDERLAY, so AutoCAD shows it too, and the underlays in
AutoCAD drawings come in the same way. What can be shown:

* **DWFx** (AutoCAD 2009 and later): its lines, curves and text, in inches of the sheet.
* **DGN V7** (MicroStation up to V7): lines, line strings, shapes, arcs, ellipses and text, in the
  file's master units, in its colours.
* **Classic DWF** (version 6) and **DGN V8** (MicroStation V8 and later) are closed binary formats:
  the underlay keeps its place and file and shows its frame and file name.

Pictures (IMAGEATTACH: PNG, JPEG, TIFF, GIF, BMP) and PDF pages (PDFATTACH) are attached the same
way; see the Image Manager.

### Multileaders (MLEADER, alias MLD)

Multileaders are AutoCAD's newer callouts (AutoCAD 2008 and later): the arrow, its line, the short
landing and the note are one object. Type MLEADER, click the arrowhead, then where the landing goes,
and type the note. Drawings from AutoCAD show their multileaders as AutoCAD does: every leader
line and arrowhead (open, dot, tick ... arrows included), the landing, and the note or block.
They move, copy, rotate, scale and mirror as one object, EXPLODE breaks one into its lines,
arrowheads and note (MTEXT), and LIST describes it. They are saved as MULTILEADER objects, so
AutoCAD opens them as multileaders again (a DXF R12 copy gets their lines and note instead).

### Grid

The grid starts off. F7, the GRID button on the status bar and the GRID command turn it on and
off, and Fancy Pencil remembers your choice the next time it starts.

### Dimensions in copy/paste, DIMREGEN

Copying dimensions to another drawing brings their pictures along to wherever they are pasted.
When a drawing opens with dimensions whose picture was left at an old position (pasted with
version 0.3.37 or earlier), they are redrawn in place and the command line says so; save to keep
the repair. **DIMREGEN** redraws every dimension from its points and current style. Saving drops
dimension pictures no dimension uses any more, which keeps files small.

### Naming layers

In the Layer Properties Manager, **New Layer** adds a layer with its name ready to type over.
To rename a layer, double-click its name, select it and press F2, or use **Rename**; Enter keeps
the new name and Esc leaves the old one. Objects on the layer, including those inside blocks,
move to the new name. Layer 0 and Defpoints can't be renamed, and a name can't contain
< > / \ " : ; ? * | , = or `. The RENAME command does the same from the command line.

### Layer controls

The Layers panel has **Make Current** (click an object: its layer becomes the current layer —
LAYMCUR) and **Match Layer** (select objects, then click an object on the layer they should move
to, or type N to pick the layer by name — LAYMCH). Both use objects you selected first, and are
also in the Format menu and the right-click menu. Format → Change to Current Layer moves objects
to the current layer (LAYCUR).

**Change** (CHANGELAYER, alias CL) moves the selected objects to a layer you pick from a list
that shows each layer's color; double-click a layer or pick it and press OK. As in AutoCAD,
choosing a layer in the layer drop-down while objects are selected also moves them to that
layer (with nothing selected it sets the current layer, as before).

The layer drop-down also shows the layer of whatever you select: one object's layer (with its
color), or **VARIES** when the selection is on more than one layer. Selecting only shows it — the
current layer stays the same. Clear the selection (Esc) and it shows the current layer again.

### Views (View → 3D Views, or the view picker)

The **view picker** at the top right of the drawing shows a small cube for each standard view:
click one to switch (Top in the middle, the elevations around it, the isometric views in the
corners, Bottom underneath). Click "Views" to fold it up; View → Show View Picker hides it.

Top, Bottom, Left, Right, Front, Back and the SW, SE, NE and NW Isometric views, as in AutoCAD
(`-VIEW`, `PLAN` for Top). Objects can have a height: **THICKNESS** (or the Properties panel)
extrudes lines, arcs, circles and polylines into walls, and **Elevation** lifts objects off the
ground plane (`ELEV` sets both for new objects). In the isometric views, and wherever text stands
in a plane other than the working plane, it is drawn letter by letter in its own plane (with a
line font made from DejaVu Sans; see support/DejaVu-LICENSE.txt).

**Drawing and editing in every view (working planes).** Front, Back, Left, Right and Bottom each
work like an AutoCAD UCS: X to the right, Y up, Z toward you. Every command works there just as
in Top view — LINE, PLINE, RECTANG, CIRCLE, ARC, TEXT, OFFSET, TRIM, FILLET, grips, snaps — and
what you draw lies in that view's plane (a rectangle drawn in Front view stands upright).
Points you click are at the depth of the base point or the object you snap to, else in the plane
through the origin.

Objects drawn in another plane show edge-on (as lines) or slanted. You can select them, snap to
their corners, erase them, and MOVE, COPY, ROTATE, MIRROR and SCALE them in 3D — rotating is how a
flat panel is stood up: in the Right view, ROTATE the panel 90 degrees about its bottom edge, and it
stands facing the Front view. Commands that change an object's shape in its own plane (TRIM,
EXTEND, OFFSET, FILLET, CHAMFER, BREAK, PEDIT, STRETCH ...) skip objects in other planes and say
so; switch to a view that faces them.

Linetypes (hidden, center, phantom, dashed ...) show in every view: in the isometric views and on
objects standing in other planes the dashes are laid out along the object itself, at the same
size as in plan.

**Plotting 3D views.** PLOT prints the view you are in. In an isometric view the plot shows
what the screen shows: objects at their heights, with thickness, linetypes, and text drawn in
its plane. Extents and Display fit that view, and a plot Window is picked on the screen. In
Front, Back, Left, Right and the UCS views the working plane is plotted flat, as on screen.

**3D blocks.** A block made from objects at different heights or in other planes keeps its 3D
shape when inserted (its Z scale scales the heights), exploded, saved and opened again.

**Any plane as the working plane (UCS).** View → Working Plane (UCS), or type `UCS`:

* **Object / Face** — pick a panel (polyline, circle, text ...): its plane becomes the working
  plane, with the origin at its first corner and X along its first edge. This is the way to work
  on a slanted panel.
* **3point** — an origin, a point along X, and a point on the plane's +Y side.
* **Origin** — click a new origin; the directions stay.
* **X / Y / Z** — turn the working plane about one of its axes (90 degrees by default).
* **View** — a plane facing the current view. **Previous** — the one before. **World** — back to Top.

The view turns to look straight at the new plane ("UCS" on the view picker), and everything draws
and edits in it exactly as in the Front view. Choosing a standard view or PLAN leaves it.

**ALIGN in 3D.** ALIGN takes up to three pairs of points anywhere in 3D: one pair moves; two pairs
also turn the objects so the first source edge lies along the first destination edge (staying in
the working plane when all the points are in it, with optional scaling as before); three pairs
also bring the plane of the three source points onto the plane of the three destination points
- the way to fit a panel against another at any angle.

In the isometric views you see everything in 3D and can draw on the ground plane; points stay at
the height of the grip or base point you start from. Planes are saved in DXF (group 210, as
AutoCAD writes them) and read back from AutoCAD drawings. R12 DXF has no planes for most objects,
so there an object in another plane is saved as its 3D edges.

The axes icon (bottom left) shows X, Y and Z in the
current view; each drawing tab keeps its own view. Thickness and elevation are saved in DXF.

### Orbit and visual styles (View menu)

**3DORBIT** (View → Orbit, `ORBIT`, `3DO`): press and drag on the drawing to turn the view around it
- left-right turns it about the vertical, up-down tilts it - then release; drag again or press
Enter to finish. The view stays "Custom" until you pick a standard view.

**VSCURRENT** (View → Visual Styles, `VS`; `HIDE` for Hidden):

* **Wireframe** - every edge (as before).
* **Hidden** - solids hide what is behind them; curved solids show their outline.
* **Shaded** - solids filled and lit, no edges.
* **Shaded with Edges** - shaded, with the edges drawn.

Faces are painted far to near, so very large faces crossing others can occasionally overlap
wrongly. Plots follow the visual style (hidden-line and shaded PDFs).

### Draw order (Modify → Draw Order, DRAWORDER / DR)

Later objects are drawn over earlier ones - on the screen and in plots. **DRAWORDER**: select
objects, then **Front** (over everything), **Back** (under everything, the default), **Above
objects** or **Under objects** (just over / under the reference objects you pick next). Also on
the right-click menu when objects are selected. **TEXTTOFRONT** brings text, dimensions and/or
leaders to the front; **HATCHTOBACK** sends all hatches and fills to the back. The order is saved
in the file (AutoCAD's draw order table) and comes back when it is opened.

### Changing text case (Modify → Change Text Case, TCASE)

Select text, MText or dimensions with override text and choose **UPPER**, **lower**, **Sentence**,
**Title** or **tOGGLE**. MText formatting (fonts, heights, line breaks) and codes such as %%c and
%%d are kept. Also on the right-click menu when text is selected. While typing in the text editor,
**Shift+F3** switches the selected words (or all the text) UPPER → lower → Title → Sentence, and the
editor's right-click menu has Change Case.

### Helix (Draw → Helix / Spiral, or Solids → Helix; HELIX)

A spring or thread path, as in AutoCAD. Pick the centre of the base, the base radius (the point you
pick is where the helix starts; [Diameter] to give a diameter), the top radius (Enter: the same -
a different top radius makes a cone-shaped spring), then the height. At the height prompt:
**Axis endpoint** (pick the top of the axis, in any direction), **Turns** (default 3), **turn
Height** (the distance between turns; then the height sets the number of turns) and **tWist** (CW or
CCW, the default). A height of 0 makes a flat spiral. It stands up the working plane's Z.

Afterwards Properties changes its base and top radius, height, turns, turn height and twist; grips
move it (base centre), set the base radius (start point), the height (top centre) and the top
radius. **SWEEP** a small circle along it to make a spring or a thread. Saved in DXF 2000 files as
AutoCAD's HELIX object (R12 files get short lines along it).

### AutoCAD 3D solids, surfaces and regions

Drawings from AutoCAD 2013 and later (DWG opened through ODA File Converter, or DXF) bring their
3D solids, surfaces (revolved, extruded, lofted, swept, planar) and regions in as Fancy Pencil
meshes. AutoCAD keeps their exact shape in its ACIS form; Fancy Pencil reads it and divides each
face - flat, cylindrical or conical, spherical, toroidal or a spline surface - into triangles
that follow it closely, showing the object's real edges (and the outline of curved sides). They
can be viewed in every visual style, moved and copied, plotted, measured (MASSPROP) and saved for
3D printing. Saving writes them as meshes: AutoCAD shows them but can't edit them as solids. Older
files that keep ACIS as text (AutoCAD 2010 and before) are not read yet.

### Revolved surfaces (Solids → Revolved Surface; REVSURF, REVOLVE MOde)

**REVSURF** makes AutoCAD's revolved surface mesh: select the path curve (line, arc, circle,
polyline, ellipse, spline or helix), then the axis (a line or polyline - it points away from the
end you pick nearest), the start angle and the included angle (+ counterclockwise, - clockwise,
360 all round). **SURFTAB1** sets the faces around (default 6) and **SURFTAB2** the faces along a
curve (default 6); every line of the mesh shows.

**REVOLVE** makes the smooth kind: at its axis prompt, **MOde** chooses **SOlid** (closed shapes
become solids, the default) or **SUrface** (they become open surfaces, without ends). Open shapes
always give surfaces. LIST and Properties call them revolved surfaces.

### 3D printing (File or Solids → 3D Print; 3DPRINT, STLOUT, STLIN)

**3DPRINT** saves the solids you select as a file for a 3D printer's slicer (Cura, PrusaSlicer,
Bambu Studio, OrcaSlicer ...). The window asks for:

- **File type**: **STL (binary)** - what every slicer reads; **STL (text)**; **3MF** - the newer
  format, which records its units (written in the drawing's own); **OBJ**.
- **Units**: STL and OBJ files have no units and slicers read them as millimetres, so a drawing in
  inches is written x 25.4 (choose Drawing units to write the numbers as they are).
- **Place on the build plate**: moves the solids so their lowest corner is at 0,0,0.

Each solid is checked first: faces turned inward are put right, the small cracks boolean operations
(UNION, SUBTRACT) leave where faces meet are mended, and the report says whether every solid is
closed ("watertight") - what a slicer needs - with its size in the file's units. **STLOUT** is
AutoCAD's command: binary or text STL, in millimetres.

**STLIN** (Import STL / OBJ Model), or File → Open on an STL or OBJ file, brings in a model from a printing site or another program as a
solid: it asks the file's units (millimetres, inches or drawing units). Flat faces show only their
edges, and curved sides show their outline in each view.

### Pictures and PDF pages (File → Attach Image / Attach PDF Page; IMAGEATTACH, PDFATTACH, ATTACH)

Attach a JPEG, TIFF, PNG, GIF, BMP or WebP picture, or one page of a PDF, to trace over or to show on
a sheet. Choose the file (and the page of a PDF), click where its lower-left corner goes, then give
its width (Enter: its real size - 96 pixels to the inch for pictures, the page's own size for a
PDF) or **Scale**. Pictures go behind the drawing (DRAWORDER brings one forward). They move, copy,
scale and stretch like other objects and are picked by their frame; Properties sets their width,
height, fade and whether they show. They show at any angle and in every view, the 3D views too, drawn
smoothly at the screen's own scale (Fancy Pencil keeps averaged smaller copies of big photos, so
thin lines in a scan don't vanish when zoomed out). They plot (into the PDF, turned or not), and are saved in DXF 2000 files as AutoCAD keeps them
(IMAGE, and PDFUNDERLAY for PDF pages). The drawing keeps each file's name, not the picture, so keep
the files with the drawing.

PNG and GIF are read directly. JPEG, TIFF, BMP and WebP need ImageMagick (or GraphicsMagick, vips,
ffmpeg or Python's PIL), and PDF pages need poppler-utils: on Fedora,
`sudo apt install imagemagick poppler-utils` on Ubuntu (`sudo dnf install ImageMagick poppler-utils`
on Fedora). A conversion cut short (the program closed part way) is noticed and done again. Converted pictures are kept in
~/.cache/fancypencil/images.

**Image Manager** (IMAGE; File or Tools → Image Manager): every attached file with its type,
status (Loaded / Not found), size and how many times it is used. Attach, **Detach** (removes
every copy), **Reload** (after the file has been changed), **Show / Hide**, **Fade**, **Change
Path** (when a file has moved) and **Select in Drawing**.

### Command history (Tools → Command History, HISTORY / TEXTSCR, F2)

Everything shown in the command line - every command, prompt, answer and message - is kept for the
whole session; the command line itself shows only its last lines. The **Command History** window
shows all of it, follows along as you work, and has **Find**, **Copy All**, **Save As** (a text
file) and **Clear**. Tick **Keep a log file** (or type LOGFILEON / LOGFILEOFF) to have it written
as it happens to ~/.local/share/fancypencil/logs, one file a day (LOGFILEPATH chooses another
folder); **Open Log Folder** shows them. The setting is remembered.

### Mass properties (MASSPROP; Tools or Solids → Mass Properties)

Select closed flat objects - closed polylines (arcs included), circles, ellipses, closed splines -
and/or 3D solids. Regions report area, perimeter, bounding box, centroid, moments and product of
inertia, radii of gyration and the principal moments with their directions about the centroid.
Several regions are added together, and a closed object lying inside another selected one counts
as a hole (select a plate and its holes together). Solids report mass (density 1, so it equals the
volume), volume, bounding box, centroid, moments and products of inertia, radii of gyration and
principal moments. Values are in the working plane's coordinates. The report opens in its own
window (Copy puts it on the clipboard) and can be written to an .mpr file.

### Quick Properties (QP)

The Quick Properties window stays open until you press its Close button, beside the drawing at
the top right (move it anywhere; it comes back there next time). Keep working in the drawing while
it is up: it follows whatever you select, and with nothing selected it shows the current settings
that new objects get. Esc clears the selection without closing it.

### Viewports on the Model tab (View → Viewports, VPORTS)

Split the drawing area into several viewports, each with its own view, zoom and visual style -
for example Top, Front, Right and an isometric side by side while you draw. **VPORTS** (or View →
Viewports): **2** (side by side or above and below), **3** (a large one on the right or left, or
three in a row), **4** (Same view, or **Standard**: Top, SE Isometric, Front and Right, the flat
views at one scale and lined up), **Join** (the active viewport with the one beside it) and
**Single** (back to one). The active viewport has a blue border: you draw, select, zoom and change
its view or visual style as usual, and what you draw shows in all of them. Click in another
viewport (or turn the wheel over it) to make it the active one. VPORTS 2/3/4 split the active
viewport, so you can split again. The viewports are saved in the file (the VPORT table, as AutoCAD
keeps them) and come back when it is opened.

These are different from layout viewports (MVIEW), which are windows on a sheet of paper for
plotting.

### Layouts, paper space and viewports (Layout menu)

Under the drawing are tabs: **Model** and **Layout1** (add more with **+**; right-click a layout
tab to rename, copy, delete, move it or set its page). A layout is a sheet of paper at its real
size - inches, or millimeters in metric drawings - with a dashed line showing the printable area.
Draw the border, title block and notes on it; they belong to that sheet only.

**Viewports** are windows on the sheet that show model space, each with its own view, visual
style and scale:

* The Layout menu's viewport items work from the Model tab too: they switch to Layout1 first.
* **MVIEW** (`MV`): pick two corners, or **Fit** (the printable area), or **2**, **3** or **4** at
  once. **4 → Standard** makes Top, Front, Right and SE Isometric: the three flat views at one
  scale and lined up with each other, as a drawing's views are. MVIEW **Lock** keeps a viewport's
  scale and view from changing; **ON/OFF** shows or hides what is in it.
* **Double-click inside a viewport** (or **MSPACE**, `MS`) to work in model space through it
  (blue border): zoom, pan, pick a view, orbit, change the visual style or draw - the viewport
  keeps what you set. **Double-click outside** it (or **PSPACE**, `PS`) goes back to the sheet.
  The MODEL/PAPER button on the status bar switches too.
* **VPSCALE**: 1:2, 2:1, 1/4"=1'-0", 1:100, Fit ... (or `ZOOM 0.5XP` inside the viewport). The
  Properties palette shows a selected viewport's scale, view, visual style, lock and size, and
  changes them. Moving a viewport moves what it shows; a corner grip resizes it at the same scale.
* **PAGESETUP**: paper size, orientation and units of the sheet. **LAYOUT** (`LO`): New, Copy,
  Delete, Rename, Set, ?.
* **PLOT** on a layout plots the sheet at 1:1 (shrunk if the chosen paper is smaller), each
  viewport clipped to its edge. Viewport borders plot like any object: put viewports on a layer
  set to No Plot to hide them.

Layouts are saved in the DXF file as AutoCAD keeps them (LAYOUT objects, VIEWPORT entities and
paper space objects) and open in AutoCAD; R12 keeps the showing layout's objects but not its
viewports. Opening a file with layouts brings them back, viewports included.

### 3D solids (Solids menu)

Solids are built in the working plane (Top, a side view, or a UCS), so a box drawn in the Front
view stands upright. They are meshes - flat facets - shown as AutoCAD shows solids in wireframe:
outlines, sharp edges and a few lines (isolines) along curved sides.

* **BOX** (corners, [Center], [Cube], [Length] then height), **WEDGE**, **CYLINDER**, **CONE**
  (with [Top radius] for a cut-off cone), **SPHERE**, **TORUS**. Heights may be negative.
* **EXTRUDE** closed shapes (circles, closed polylines, ellipses, closed splines) into solids, open
  ones into surfaces: by a height, a [Direction], or along a [Path].
* **REVOLVE** shapes about an axis (two points, X, Y, Z, or a line) by any angle.
* **SWEEP** shapes along a path - line, arc, polyline (corners are mitred), spline.
* **LOFT** through cross sections picked in order (a circle to a square, say).
* **PRESSPULL**: click inside a closed area, or on a flat face of a solid (in any view), and give
  the height. On a solid's face, a positive height adds to the solid, a negative one cuts into it.
  As you move the mouse, the face a click would take lights up in orange: the face in front at the
  cursor. While you give the height, the pulled shape is previewed; move the mouse along the face's
  direction (back for negative) and click, or type the height. **Edge**: pick an edge of the solid
  (it lights up), then Next/Accept between the faces that meet there - handy when the face you want
  is seen edge-on or behind another.
* **UNION** joins solids, **SUBTRACT** removes solids from others (pick what to keep, Enter, then
  what to remove), **INTERSECT** keeps only the shared part, and **SLICE** cuts solids with a plane
  (two points square to the working plane, XY/YZ/ZX through a point, or 3 points), keeping one side
  or both. The result keeps the outlines and isolines of the originals plus the lines where they
  meet; flat faces merge without seams.
* **UCS Face** on a solid puts the working plane on the face you click, to draw on it.

The shapes used are removed afterwards (set DELOBJ to 0 to keep them). Properties and the
hover tip show a solid's volume. Solids are saved as AutoCAD polyface meshes, which AutoCAD opens
and shows exactly (there they are meshes, not ACIS solids); polyface meshes, polygon meshes and
3D faces in AutoCAD drawings open as solids/meshes here too.

### Snapping to solids

Solids are kept as meshes of flat faces, so a round edge is really many short straight ones.
Fancy Pencil finds the round shapes again from the mesh - for solids you make and for solids
opened from files - so the usual snaps work on them:

* **Center** and **Quadrant** of round edges on flat faces: a cylinder's ends, a hole drilled
  through a flat face, the rounded end of a slot (Center of the arc).
* **Center** of the ends of a round wall - a cylinder's side, or a hole bored through a round
  body: move over the end of the hole.
* **Center** of a sphere.
* Circles and arcs drawn in another plane (say in the Front view) snap to their **Center** and
  **Quadrants** from any view.

On a round edge, its centre takes the place of the many little corner (Endpoint) snaps along it.
When a line, circle or text lies on a solid, a click on it picks it rather than the solid. PEDIT
offers to turn a circle (as well as a line or arc) into a polyline.
**Solids → Keep the Shapes Solids Are Made From** (KEEPPROFILES, DELOBJ 0) keeps the circles,
polylines and paths that EXTRUDE, REVOLVE, SWEEP and LOFT use, as ordinary objects to snap to
and reuse.

### Isometric drafting (View → Isometric Drafting, ISODRAFT)

AutoCAD's 2D isometric mode for drawing isometric pictures: the grid becomes an isometric grid,
the crosshairs and Ortho follow the current isoplane, and snap uses the isometric lattice.
**F5** (or Ctrl+E) switches between the Left, Top and Right isoplanes; the **ISO** button in the
status bar turns the mode on and off and shows the current plane. **ELLIPSE → Isocircle** draws a
circle as it looks on the current isoplane.

### Editing blocks (BEDIT, RENAME)

Select a block and right-click → **Edit Block...** (or Draw → Block: Edit..., or `BE`). The block
opens in its own **Block Editor** tab, with its base point at 0,0 (the red cross). Change it with
any commands; **BSAVE** (or Save, Ctrl+S) writes it back and every copy in the drawing updates;
**BCLOSE** or closing the tab finishes (it asks whether to keep unsaved changes). Moving the
objects relative to the red cross moves the block's base point. One U in the drawing undoes the
edit. **RENAME** (Draw → Block: Rename..., or right-click a block) renames a block, layer, text
style or dimension style everywhere it is used.

The right-click menu on a selection also has **Create Block from Selection...**, and without a
selection **Create Block...**, **Edit Block...** and **Paste as Block**.

### Dimensions that line up

While placing a linear or aligned dimension, bring the dimension line near the line of an
existing parallel dimension: it snaps onto the same line (a dashed guide and "Lined up with
dimension" show it), so dimensions in a row sit at the same level. A selected linear dimension
has grips at both ends of its dimension line (where the arrows are): drag either one to move the
dimension line closer or further away — it snaps to the level of other dimensions the same way.
The extension line origins and the text have grips as before.

### Cursor, Selection & Background (OPTIONS, OP)

**Drawing background**: Tools → Cursor, Selection & Background → Drawing background offers dark
(the default), black, dark gray, dark blue, gray, light gray, paper and white, a Black-to-White slide for any shade of gray, or Custom... for any
color (`BACKGROUND` at the command line). On a light background, white (color 7) objects are shown
black, as in AutoCAD, and the grid and crosshairs switch to dark so they stay visible. Plots are
not affected.

Tools → Cursor, Selection & Background: crosshair size (percent of the screen; 100 = full screen) and color,
pickbox size, snap target (aperture) size, snap marker size, grip size, and rollover tips, with a
preview. These are your settings rather than the drawing's: they apply in every tab and are kept
in `~/.config/fancypencil/settings.lisp` for next time.

### Layers: No Plot

The Layer Properties Manager has a **Plot** column: click it to switch a layer between Plot and
No Plot. A No Plot layer is shown on screen but left out when you plot (as is AutoCAD's
Defpoints layer). It is saved in the drawing (DXF group 290), so AutoCAD sees it too. At the
command line: `-LAYER` → `Plot` → `No` → layer names.

### Align (AL)

Modify → Align. Select objects, then pick a source point and where it goes, and a second
source point and where it goes (guide lines show the pairs; the objects stay put until the end). Answer **Yes** to
"Scale objects based on alignment points?" to stretch them so the two source points land
exactly on the two destination points (the scale factor is shown); **No** only moves and turns
them. Your last answer is the default next time.

### Match Properties (MATCHPROP, MA, PAINTER)

Modify → Match Properties, or right-click a selected object → Match Properties. Pick the source
object (or select it first), then click or window the objects to change; Enter ends.
Type **S** (Settings) at the prompt to choose what is copied:

- **Basic** (any object): color, layer, linetype, linetype scale, lineweight, thickness.
- **Special** (same kind of object only): text style, height, width factor and oblique angle
  (MTEXT: style and height); dimension style and overrides, and leader arrows; hatch pattern,
  scale and angle (solid or pattern); polyline width and linetype generation.

Everything is on by default, as in AutoCAD. Objects on locked layers are skipped, and one U
undoes the whole command.

### Copy and paste between drawings

Edit → Copy (Ctrl+C) puts the selected objects on the clipboard; switch to another drawing tab, or
another Fancy Pencil window, and use Edit → Paste (Ctrl+V) to place them at a point you pick.
Objects arrive with what they need: their layers (colour, linetype, lineweight), linetypes, text
styles, dimension styles and block definitions. If the target drawing already has a layer, style
or block of the same name, its own definition is used, as in AutoCAD.

| Command | Keys | |
|---|---|---|
| COPYCLIP | Ctrl+C | copy the selection (base point: its lower-left corner) |
| COPYBASE | Ctrl+Shift+C | copy with a base point you pick |
| CUTCLIP | Ctrl+X | copy, then erase |
| PASTECLIP | Ctrl+V | paste at a point you pick |
| PASTEBLOCK | Ctrl+Shift+V | paste as one block |
| PASTEORIG | | paste at the same coordinates as the original |

While typing in the text editor or a dialog, Ctrl+C/V copy and paste text as usual; on the
command line they copy selected text, or paste text once you've started typing.

### Plotting (PLOT, Ctrl+P)

File → Plot... (or PLOT / PRINT, Ctrl+P) opens the Plot dialog:

- **Printer/plotter**: "PDF file" or any printer your system knows (from CUPS, `lpstat`).
- **Paper size**: Letter, Legal, Tabloid, ANSI C–E, ARCH A–E and ISO A4–A0; portrait or landscape.
- **Plot area**: Display, Extents, Limits or Window (pick two corners on the drawing).
- **Scale**: Fit to paper, 1:1, 1:2 … 1:100, or architectural scales such as `1/4" = 1'-0"` and
  `1/8" = 1'-0"`; drawing units (inches or feet) are taken from the drawing. Centre on paper.
- **Black only** (monochrome) and **plot with lineweights**.
- **Preview** shows the page before you plot. A PDF can be opened in your viewer when it's written;
  printing goes through `lp` at true scale (no "fit to page" by the printer).

`-PLOT` asks the same questions at the command line, for scripts and AutoLISP:
`(command "-plot" "" "ARCH D" "Landscape" "Extents" "1/8\"=1'-0\"" "No" "/tmp/plan.pdf")`.

### Spelling (SPELL, SP)

Tools → Spelling... checks TEXT, MTEXT, attributes and dimension text. Select objects, or press
Enter to check the whole drawing. Each unknown word is shown on screen with suggestions:
Change, Change All, Ignore, Ignore All, Add to Dictionary. The dictionary is your system's
Hunspell English dictionary when installed (`/usr/share/hunspell/en_US`), otherwise the copy in
`support/dict`, plus common drafting words and abbreviations; words you add go to
`~/.config/fancypencil/custom.dic`.

While typing in the text editor, unknown words get a red underline; right-click one for
suggestions, Add to Dictionary or Ignore.

### Command aliases

Fancy Pencil's built-in shortcuts are always available; your file adds to them and overrides
any it redefines. Tools → Command Aliases → **Open Alias File** opens your alias file (`fancypencil.pgp`) in your
text editor; save it there and the changes are used straight away (no REINIT needed).
**Open Alias Folder** shows where it lives, and **Edit Aliases...** is the built-in editor.

Aliases come from a `.pgp` file in AutoCAD's acad.pgp format (`L,  *LINE`). The shipped set is in
`support/fancypencil.pgp`. ALIASEDIT opens an editor; saving writes your own copy to
`~/.config/fancypencil/fancypencil.pgp`, which is used from then on. You can also edit that file in
any text editor and run REINIT. An alias may point at one of your AutoLISP commands, e.g.
`HH, *HELLO` runs `C:HELLO`. External commands (`NAME, shell command, flags, *prompt,`) are supported.

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
- **Express Tools library**: `acet-error-init`, `acet-error-restore`, `acet-sysvar-set`, `acet-ui-progress`,
  `acet-str-replace`, `acet-str-find`, `acet-str-format`, `acet-list-remove-duplicates`, `acet-list-m-assoc`,
  `acet-geom-midpoint`, `acet-geom-vertex-list`, `acet-ent-geomextents`, `acet-ss-to-list`, `acet-ss-union`,
  `acet-ss-ssget-filter`, `acet-ss-drag-move`, `acet-layer-locked`, `acet-pline-make` and more
- **Commands**: `command`, `command-s`, `vl-cmdf`, including `pause` and commands left active across
  several `command` calls, as in AutoCAD (`(command "line" p1 p2)` then `(command "")`), and `CMDACTIVE`
- **Files & system**: `open close read-line write-line read-char write-char findfile load getenv
  getvar setvar vl-directory-files vl-file-delete ...`

### Example programs

- `examples/demo-plate.lsp` — draws a dimensioned plate entirely through `command`. Load it and type `PLATE`.
- `examples/tools.lsp` — TLEN (total length), BCIRC (bolt circle with `initget`/`getkword` and `*error*`),
  NUM (auto-numbering), LAYOFF, CL (center marks).

## DXF

SAVE writes AutoCAD 2000 DXF (AC1015), which keeps lineweights, linetypes and scales, hatches,
dimensions and dimension styles, LWPOLYLINE, MTEXT, ELLIPSE and SPLINE, and opens in AutoCAD 2000
and every later release. SAVEASR12 (or DXFOUT and choosing R12) writes R12 DXF for very old
software; R12 has no lineweights or hatches. Exported files are checked in the test suite with
the independent ezdxf auditor and come back with zero errors.

Fancy Pencil reads ASCII DXF from R12 through current releases: lines, circles, arcs, points,
LWPOLYLINE/POLYLINE (with bulges and widths), TEXT, MTEXT, INSERT with ATTRIBs, nested blocks,
DIMENSION, HATCH (polyline, line, arc, ellipse and spline boundaries), ELLIPSE, SPLINE and SOLID;
layers with color, linetype, lineweight, on/off, freeze and lock; linetype and dimension style tables.

## What's not there (yet)

Compared with AutoCAD, this version does not have: gradient fills, associative hatches and dimensions (they don't follow edits to
their boundary objects), true NURBS solids (solids are meshes), and ActiveX (`vla-*`/`vlax-*`) or DCL dialog functions. DWG
files are opened and saved through a converter (ODA File Converter or LibreDWG). TRIM and EXTEND
(Shift+click switches between them) work on lines, arcs, circles and polylines but not ellipses
or splines. Express Tools that depend on external references (XREF tools) are not included.

## Tests

```sh
./fancypencil --tty tests/test-alisp.lsp tests/test-cad.lsp tests/test-cad2.lsp tests/test-v02.lsp tests/test-v03.lsp < /dev/null
sbcl --non-interactive --load load.lisp --load tests/test-views.lisp
sbcl --non-interactive --load load.lisp --load tests/test-layouts.lisp
sbcl --non-interactive --load load.lisp --load tests/test-massprop.lisp
sbcl --non-interactive --load load.lisp --load tests/test-draworder.lisp
sbcl --non-interactive --load load.lisp --load tests/test-tiles.lisp
sbcl --non-interactive --load load.lisp --load tests/test-snap3d.lisp
sbcl --non-interactive --load load.lisp --load tests/test-raster.lisp
sbcl --non-interactive --load load.lisp --load tests/test-helix.lisp
sbcl --non-interactive --load load.lisp --load tests/test-print3d.lisp
sbcl --non-interactive --load load.lisp --load tests/test-input.lisp
sbcl --non-interactive --load load.lisp --load tests/test-surfaces.lisp
sbcl --non-interactive --load load.lisp --load tests/test-acis.lisp
```

## Layout

```
fancypencil        launcher script
install.sh         adds Fancy Pencil and its icon to the desktop launcher
load.lisp          compiles & loads the sources (no ASDF needed); fancypencil.asd is provided too
src/package.lisp   packages
src/geom.lisp      points, intersections, transforms
src/db.lisp        drawing database (entities are DXF association lists, like entget)
src/ltype.lisp     linetypes (.lin), dashes, lineweights
src/hatch.lisp     hatch patterns, rendering, boundary detection
src/dxf.lisp       DXF reader and R12 writer
src/snap3d.lisp    snaps on solids: round edges, round walls, spheres; circles in other planes
src/tiles.lisp     VPORTS: the Model tab split into viewports
src/draworder.lisp DRAWORDER, TEXTTOFRONT, HATCHTOBACK, text case (TCASE)
src/massprop.lisp  MASSPROP: area and mass properties of regions and solids
src/raster.lisp    pictures and PDF pages: IMAGEATTACH, PDFATTACH, the Image Manager (IMAGE)
src/acis.lisp      AutoCAD 3D solids / surfaces / regions: ACIS (SAB) reader and face triangulation
src/surfaces.lisp  REVSURF revolved surface meshes (SURFTAB1, SURFTAB2)
src/imgview.lisp   drawing pictures on the screen: averaged copies, exact resampling, PNG views
src/history.lisp   the session record: HISTORY window, log files (LOGFILEON / LOGFILEOFF)
src/helix.lisp     HELIX: springs, thread paths and flat spirals
src/print3d.lisp   3D printing: STL, 3MF and OBJ out (3DPRINT, STLOUT); STL and OBJ in (STLIN)
src/layouts.lisp   paper space: layout tabs, sheets, viewports (MVIEW, MSPACE, VPSCALE), layout plots
src/dxf2000.lisp   AutoCAD 2000 DXF writer
src/alisp.lisp     AutoLISP reader, printer, evaluator and core functions
src/input.lisp     prompts, coordinate entry, keywords, object snaps, selection, (command)
src/gui.lisp       Tk front end: view, rendering, events
src/grips.lisp     grips and grip editing
src/gui.tcl        Tk window layout, palette, dialogs
src/commands.lisp  drawing commands
src/dims.lisp      dimensions and dimension styles
src/modify.lisp    editing commands
src/modify2.lisp   polyline trim/extend, fillet/chamfer, PEDIT
src/cmds2.lisp     LINETYPE, LWEIGHT, HATCH, REVCLOUD
src/props.lisp     Properties palette
src/express.lisp   .pgp aliases, Express Tools, acet-* functions
src/utilcmds.lisp  view/file/layer/block/inquiry commands; entget/tblsearch/getvar...
src/main.lisp      command loop and entry point
support/fancypencil.pgp  default command aliases
support/icons/     application icon (SVG source + PNG sizes)
```
