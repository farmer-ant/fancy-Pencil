# Fancy Pencil manual - the words and pictures, chapter by chapter.
# Blocks: ("h1", title, intro) starts a chapter on a new page; ("h2", t); ("p", t); ("ul", [items]);
# ("ol", [items]); ("img", file, width_in, caption); ("note", t); ("table", [widths], [rows]);
# ("callouts", file, width_in, [(x_px, y_px, n)...], caption); ("break",) new page.
# Inline: **bold**, `mono` (commands, keys, things you type).

VERSION = "0.4.17"

CHAPTERS = []
def ch(*blocks): CHAPTERS.append(list(blocks))

# ------------------------------------------------------------------ 1
ch(
("h1", "Welcome to Fancy Pencil",
 "A drafting program that works like AutoCAD: the same commands, the same prompts, and your AutoLISP."),
("p", "Fancy Pencil is a 2D and 3D drafting program for Linux. If you have used AutoCAD, you already know "
      "how to use it: you type `LINE` or `L`, you answer the same prompts, you pick points with object snaps, "
      "you select objects first or after, and you save drawings that AutoCAD opens. Layers, blocks, "
      "dimensions, hatches, text styles, layouts and plotting all work the way you expect."),
("p", "It also runs **AutoLISP**. Routines written for AutoCAD - `defun c:...`, `command`, `entget`, "
      "`ssget` filters, `getkword` and `*error*` handlers - run unchanged in most cases, and you can type "
      "AutoLISP straight at the command line."),
("img", "main.png", 8.4, "Fancy Pencil with a drawing made by an AutoLISP program (the demo plate)."),
("h2", "What it can do"),
("ul", [
 "**Draw** lines, polylines, rectangles, polygons, circles, arcs, ellipses, splines, helixes, points, revision clouds and hatches.",
 "**Edit** with Move, Copy, Rotate, Scale, Mirror, Offset, Trim, Extend, Fillet, Chamfer, Array, Stretch, Align, Explode and grips.",
 "**Annotate** with single-line and paragraph text, dimensions, leaders and multileaders, in text and dimension styles.",
 "**Organise** with layers, linetypes, lineweights, colors, blocks and draw order.",
 "**Model in 3D**: boxes, cylinders, spheres and more; extrude, revolve, sweep, loft, press-pull; union, subtract, intersect and slice; the Platonic solids and wireframe struts.",
 "**Look around** in Top, Front, Side and isometric views, orbit, shade, and split the screen into viewports.",
 "**Print** to PDF or any printer from the model or from layouts with viewports, at true scale.",
 "**Exchange** drawings with AutoCAD as DXF, and as DWG with the free ODA File Converter.",
]),
("h2", "How to read this manual"),
("p", "Chapters 2 to 4 get you started: installing, the window, and how commands are given. The chapters after "
      "them each cover one kind of work. Chapter 19 lists the keyboard shortcuts, and the Command Reference at "
      "the end lists every command with its aliases."),
("p", "This manual is also inside Fancy Pencil: press `F1` or choose **Help > User Manual** to open it on screen."),
("p", "Things you type are shown like `this`. A command's alias (its short name) is shown in brackets: "
      "LINE (`L`). Menu paths are written with arrows: **Format > Layer**. Keys are written `Ctrl+S`, `F3`."),
)

# ------------------------------------------------------------------ 2
ch(
("h1", "Installing Fancy Pencil",
 "One file installs everything: Install Fancy Pencil.deb, from the Fancy Pencil Kit."),
("h2", "What you need"),
("ul", [
 "A 64-bit computer with **Ubuntu** 22.04 or newer, **Linux Mint** 21 or newer, or **Debian** 12.",
 "The **Fancy Pencil Kit** folder (on a USB stick or drive, or downloaded). It holds `Install Fancy Pencil.deb` and `INSTRUCTIONS.txt`.",
 "Your password, to install software.",
]),
("h2", "Install"),
("ol", [
 "Open the Fancy Pencil Kit folder and double-click **Install Fancy Pencil.deb**.",
 "If it opens in Archive Manager instead (a list of folders called DEBIAN, opt and usr), close that window, right-click the file and choose **Open With... > App Center**.",
 "App Center shows Fancy Pencil. Click **Install** and enter your password. App Center also installs SBCL and Tk, the two programs Fancy Pencil runs on.",
 "When it has finished, press the Super (Windows) key, type **Fancy Pencil**, and click its icon. Right-click the icon in the dock and choose **Pin to Dash** to keep it there.",
]),
("img", "appcenter.png", 6.6, "App Center installing Fancy Pencil from the Kit."),
("note", "App Center shows **Potentially unsafe** and **Unknown publisher** for every program that comes from a "
         "file instead of its own store. It is not a fault in Fancy Pencil. Install it if you trust where the "
         "Kit came from."),
("h2", "If App Center won't open the file"),
("p", "Right-click an empty part of the Kit folder, choose **Open in Terminal**, and type:"),
("code", "sudo apt install \"./Install Fancy Pencil.deb\""),
("h2", "AutoCAD DWG files: ODA File Converter"),
("p", "Fancy Pencil reads and writes AutoCAD's DXF files by itself. For **.dwg** files it uses the free "
      "**ODA File Converter** from the Open Design Alliance. To install it:"),
("ol", [
 "Open **Get ODA File Converter** from the app menu (or, in Fancy Pencil, **Help > Get ODA File Converter**).",
 "Click **Download .deb**. Your web browser downloads the converter; accept ODA's terms if it asks.",
 "When the download has finished, click **Install the downloaded file...**, pick the file whose name starts with `ODAFileConverter` and ends in `.deb`, and enter your password.",
]),
("img", "oda.png", 5.4, "The Get ODA File Converter window."),
("p", "Download page: `www.opendesign.com/guestfiles/oda_file_converter`. Fancy Pencil finds the "
      "converter by itself once it is installed."),
("h2", "Updating and removing"),
("ul", [
 "**Update**: double-click a newer Install Fancy Pencil.deb and click **Install** (or **Update**). Your settings, toolbars and drawings are kept.",
 "**Remove**: in App Center, **Manage**, find fancypencil and remove it; or type `sudo apt remove fancypencil`.",
 "**Your settings** live in the hidden folder `~/.config/fancypencil`: preferences, recent files, aliases, toolbars and plot settings. Copy it to another computer to bring them along.",
]),
)

# ------------------------------------------------------------------ 3
ch(
("h1", "The Fancy Pencil Window",
 "Where everything is."),
("callouts", "main.png", 8.4,
 [(30, 10, 1), (300, 45, 2), (60, 128, 3), (40, 450, 4), (1505, 255, 5), (770, 520, 6),
  (100, 870, 7), (300, 930, 8), (1100, 987, 9), (1582, 450, 10)],
 "The parts of the window."),
("table", [0.55, 1.75, 6.1], [
 ["1", "**Menu bar**", "Every command, by subject: File, Edit, View, Layout, Draw, Solids, Dimension, Modify, Format, Tools, Express, Window, Help."],
 ["2", "**Ribbon**", "The everyday commands in groups (Draw, Modify, Annotation, Layers, Properties, View), the current layer list and the color, linetype and lineweight lists."],
 ["3", "**Drawing tabs**", "One tab for each open drawing. Click to switch, x to close, + for a new drawing."],
 ["4", "**Toolbars**", "Icons for commands. The Menus toolbar opens any menu's whole tree; Draw is beside it. Rest the mouse on an icon to see its name."],
 ["5", "**View picker**", "Click a cube to see the drawing from the Top, a side, or an isometric corner."],
 ["6", "**Drawing area**", "Where you draw. Wheel zooms at the cursor, middle-drag pans, middle double-click shows everything."],
 ["7", "**Model and layout tabs**", "Model is where you draw full size; layouts are sheets of paper for plotting."],
 ["8", "**Command line**", "Where commands are typed and prompts appear. The history above it shows what happened."],
 ["9", "**Status bar**", "The cursor's coordinates, and buttons for GRID, SNAP, ORTHO, POLAR, OSNAP, ISO, LWT and DYN."],
 ["10", "**Modify toolbar**", "Move, Copy, Rotate, Scale, Mirror, Trim, Extend, Fillet, Array and the other editing commands."],
]),
("h2", "The ribbon"),
("img", "ribbon.png", 8.4, "The ribbon: click a word to run that command."),
("h2", "Command line and status bar"),
("img", "cmdline.png", 8.4, "Prompts appear here; the status bar buttons turn drawing aids on and off."),
("h2", "The status bar buttons"),
("table", [1.3, 1.0, 6.1], [
 ["**GRID**", "`F7`", "A grid of dots or lines to help you judge sizes. It doesn't print."],
 ["**SNAP**", "`F9`", "The cursor jumps in steps (the snap spacing)."],
 ["**ORTHO**", "`F8`", "Lines go only straight across or straight up and down."],
 ["**POLAR**", "`F10`", "Guides at set angles while you draw."],
 ["**OSNAP**", "`F3`", "Object snaps: the cursor locks onto ends, middles, centres and more."],
 ["**ISO**", "`F5`", "Isometric drafting, and which isoplane is current."],
 ["**LWT**", "", "Show lineweights (line thickness) on screen."],
 ["**DYN**", "`F12`", "Show prompts and values next to the cursor."],
 ["**MODEL**", "", "Shows Model or Paper space; on a layout, click to switch."],
]),
("h2", "Several drawings at once"),
("p", "Every drawing opens in its own tab. `Ctrl+Tab` and `Ctrl+Shift+Tab` step through them, `Ctrl+F4` "
      "or `CLOSE` closes the current one, and the **Window** menu lists them all. Each drawing keeps its own "
      "view, selection, undo history, layers and settings. Copy (`Ctrl+C`) and Paste (`Ctrl+V`) work between "
      "drawings, bringing layers, styles and blocks along."),
("p", "When you close a drawing its view is remembered; the next time you open it, it comes back zoomed "
      "where you left it."),
)

# ------------------------------------------------------------------ 4
ch(
("h1", "Giving Commands",
 "The command line, coordinates, and drawing aids."),
("h2", "Typing commands"),
("ul", [
 "Type a command or its alias and press `Enter` or `Space`: `LINE` or `L`, `CIRCLE` or `C`, `TRIM` or `TR`.",
 "Answer the prompts the same way. Options are shown in square brackets: `[Close/Undo]`. Type the capital letters of one (`C`, `U`) to choose it, or click it.",
 "The value in angle brackets is the default: `<1.0000>`. Press `Enter` to accept it.",
 "`Enter` on an empty prompt repeats the last command. `Esc` cancels. While a command runs, right-click is `Enter`.",
 "Menus, the ribbon and toolbar icons type the command for you; the prompts are the same.",
]),
("h2", "Typing points and distances"),
("table", [2.0, 6.4], [
 ["`3,4`", "The point at X 3, Y 4 (the first point of a command)."],
 ["`@2,0`", "2 to the right of the last point (relative)."],
 ["`@5<45`", "5 away from the last point, at 45 degrees (polar)."],
 ["`4,0`", "After the first point, typed points are relative to the last one, as with AutoCAD's dynamic input: LINE `0,0` then `4,0` then `0,3` goes 4 across and 3 up."],
 ["`#10,10`", "An absolute point, even after the first."],
 ["`5`", "A bare number: 5 along the rubber band toward the cursor (direct distance)."],
 ["`2'6`  `1 1/2`", "Feet and inches, and fractions, wherever a distance is asked for."],
]),
("p", "`SETVAR DYNPICOORDS 1` makes every typed point absolute (remembered). Points given by AutoLISP and "
      "scripts are always absolute."),
("h2", "Object snaps"),
("p", "Object snaps put the point exactly on an object: its end, its middle, a circle's centre, where two "
      "lines cross. With **OSNAP** on (`F3`), the snap markers appear as the cursor nears an object; click to "
      "take the point. `OSNAP` (or right-click the OSNAP button) chooses which snaps are on."),
("img", "dlg-osnap.png", 4.6, "Object Snap settings (OSNAP)."),
("p", "For one point only, type the snap's short name at the prompt: `END`, `MID`, `CEN`, `INT`, `PER`, "
      "`TAN`, `QUA`, `NOD`, `INS`, `NEA` - or `NON` for none. **Shift + right-click** gives a menu of them."),
("h2", "Ortho, Polar, Grid and Snap"),
("p", "`F8` ORTHO keeps lines straight across or up and down. `F7` GRID shows a grid and `F9` SNAP makes "
      "the cursor jump in steps. In the isometric views Ortho follows the X and Y of the plane you are "
      "drawing on; in Front, Back, Left and Right it follows the screen, so up the screen is up in Z."),
("h2", "AutoLISP at the command line"),
("p", "Type AutoLISP straight in: `(+ 1 2)`, `!myvar` to see a variable, or a longer expression over "
      "several lines (the prompt shows `(_>` until the brackets balance). Expressions also work inside "
      "prompts: at \"Specify next point\" you can type `(polar (getvar \"lastpoint\") 0 5)`."),
("h2", "Undo and redo"),
("p", "`U` or `Ctrl+Z` undoes the last command, `REDO` or `Ctrl+Y` brings it back, and `UNDO` undoes several "
      "at once. Each drawing has its own undo history."),
)

# ------------------------------------------------------------------ 5
ch(
("h1", "Selecting, Grips and Properties",
 "Pick objects, change them by dragging, and read or edit everything about them."),
("h2", "Selecting"),
("ul", [
 "**Click** an object at the `Command:` prompt to select it. Click more objects to add them.",
 "**Drag left to right** for a window: it selects what is completely inside.",
 "**Drag right to left** for a crossing window: it selects what is inside or touches it.",
 "`Ctrl+A` (SELECTALL) selects everything. `Esc` clears the selection.",
 "Then run a command - MOVE, ERASE (or press `Delete`) - and it uses the selection. Or start the command first and select when it asks.",
]),
("h2", "Grips"),
("p", "Selected objects show blue squares - **grips** - at their ends, middles and centres. Click a grip to "
      "make it hot (red) and drag it: a line's end moves, a circle grows. With a hot grip, `Enter` or `Space` "
      "steps through STRETCH, MOVE, ROTATE, SCALE and MIRROR, with the Base point, Copy, Undo and eXit options. "
      "Grips on several objects that share a point move them all together."),
("img", "grips-crop.png", 6.4, "Six bolt holes selected: their grips show at the centres and quadrants."),
("h2", "The Properties palette"),
("p", "`PROPERTIES` (`Ctrl+1`) opens the palette on the right. With nothing selected it shows the settings new "
      "objects will get (color, layer, linetype, lineweight). With a selection it shows what the objects share, "
      "and for one kind of object its sizes too - a circle's centre, radius, diameter, circumference and area. "
      "Change a value and press `Enter`. The list at the top (\"All (6)\", \"Circle (6)\") narrows a mixed "
      "selection to one kind."),
("img", "properties-crop.png", 7.6, "The Properties palette showing the six selected circles."),
("h2", "Quick Properties and rollover tips"),
("p", "**Quick Properties** (`QP`, or right-click > Quick Properties) is a small panel with just the main "
      "properties; it stays open and follows whatever you select. Rest the cursor on any object for half a "
      "second and a **rollover tip** shows what it is and its sizes."),
("h2", "The right-click menu"),
("p", "Right-click the drawing at the Command prompt: you get Repeat, Recent Input and, with objects selected, "
      "Erase, Move, Copy, Scale, Rotate, Mirror, Align, Array, Draw Order, Solid Editing (Union, Subtract, "
      "Intersect, Slice) for solids, Create Block, Match Properties and Properties."),
("img", "dlg-rightclick.png", 2.0, "Right-click with objects selected."),
("h2", "Measuring"),
("table", [2.2, 6.2], [
 ["`DIST` (`DI`)", "Distance and angle between two points."],
 ["`ID`", "The coordinates of a point."],
 ["`AREA` (`AA`)", "Area and perimeter of points you pick or of an object."],
 ["`LIST` (`LI`)", "Everything about the selected objects, in a window you can copy from."],
 ["`MASSPROP`", "Area, centroid and moments of closed shapes; volume and mass of solids."],
]),
)

# ------------------------------------------------------------------ 6
ch(
("h1", "Drawing",
 "Lines, curves, shapes and fills."),
("img", "menu-draw.png", 3.2, "The Draw menu."),
("h2", "Lines and polylines"),
("ul", [
 "**LINE** (`L`): click points; each pair is a separate line. `C` closes back to the start, `U` takes back a point, `Enter` ends.",
 "**PLINE** (`PL`): one object made of straight and curved segments. `A` switches to arcs, `L` back to lines, `W` sets a width.",
 "**RECTANG** (`REC`): two corners. Typing `11,8.5` after the first click makes an 11 x 8.5 rectangle.",
 "**POLYGON** (`POL`): number of sides, the centre, inscribed or circumscribed, and the size.",
]),
("h2", "Circles, arcs and ellipses"),
("ul", [
 "**CIRCLE** (`C`): centre and radius, or `D` for a diameter; `2P`, `3P` and Tan-Tan-Radius.",
 "**ARC** (`A`): three points, or Start-Center-End and the other AutoCAD ways.",
 "**ELLIPSE** (`EL`): axis endpoints, or the centre; `Isocircle` in isometric drafting.",
]),
("h2", "Splines and helixes"),
("p", "**SPLINE** (`SPL`): pick points and press `Enter`. With **Method Fit** the curve passes through "
      "every point; **Method CV** treats them as control vertices that pull the curve. Close, start and end "
      "Tangency, and Knots shape it. **HELIX** makes springs, thread paths and flat spirals: base centre, base "
      "and top radius, then height, with Turns, turn Height and tWist."),
("h2", "Points, revision clouds and text"),
("p", "**POINT** (`PO`) places a point in the style set by **DDPTYPE** (Format > Point Style). **REVCLOUD** "
      "(`RC`) draws a cloud around changes. Text has its own chapter."),
("h2", "Hatching"),
("p", "**HATCH** (`H`) fills an area. In the dialog, choose a pattern (or click the swatch for the palette), "
      "the angle and the scale, then **Add: Pick points** and click inside the area: the boundary is found "
      "automatically, with closed objects inside left as islands. The hatch shows as you go; `Enter` brings "
      "the dialog back, and **OK** keeps it."),
("img", "dlg-hatch.png", 4.4, "The Hatch dialog."),
("p", "Patterns include SOLID, ANSI31 to ANSI38, LINE, NET, DASH, DOTS, BRICK, SQUARE, CROSS, HONEY, EARTH, "
      "STEEL, INSUL and more. **HATCHEDIT** (`HE`) changes an existing hatch; **-HATCH** asks at the "
      "command line, for scripts."),
)

# ------------------------------------------------------------------ 7
ch(
("h1", "Modifying",
 "Move, copy, trim, round off and rearrange."),
("h2", "Moving and copying"),
("table", [2.5, 5.9], [
 ["**MOVE** (`M`)", "Select, a base point, then where it goes."],
 ["**COPY** (`CO`)", "The same, leaving the original; keep clicking for more copies."],
 ["**ROTATE** (`RO`)", "A base point and an angle, or Reference to line one thing up with another."],
 ["**SCALE** (`SC`)", "A base point and a factor, or Reference: \"this length should be that length\"."],
 ["**MIRROR** (`MI`)", "Two points on the mirror line; keep or erase the originals."],
 ["**ARRAY** (`AR`)", "Copies in rows and columns (Rectangular) or around a centre (Polar)."],
 ["**ALIGN** (`AL`)", "Pairs of points: the source points move onto the destination points, turning and optionally scaling. Works in 3D with up to three pairs."],
 ["**STRETCH** (`S`)", "A crossing window over the part to stretch, then a base point and where it goes."],
]),
("h2", "Trimming and extending"),
("p", "**TRIM** (`TR`): select the cutting edges (or press `Enter` for all), then click the parts to cut off. "
      "**EXTEND** (`EX`) is the same, lengthening objects to the edges. `Shift`+click switches between them. "
      "They work on lines, arcs, circles and polylines. **BREAK** (`BR`) cuts a gap or splits an object at a point."),
("h2", "Offset, fillet and chamfer"),
("ul", [
 "**OFFSET** (`O`): a distance, then the object and the side - a parallel copy.",
 "**FILLET** (`F`): two lines (or a polyline with `P`) joined by an arc of the fillet radius (`R`). Radius 0 makes a sharp corner.",
 "**CHAMFER** (`CHA`): the same with a straight cut.",
 "**JOIN** (`J`) joins touching pieces into one; **EXPLODE** (`X`) breaks polylines, blocks, dimensions and text into their parts; **PEDIT** (`PE`) edits polylines.",
]),
("h2", "Changing properties"),
("ul", [
 "**MATCHPROP** (`MA`): pick a source object, then the objects to make like it. `S` chooses what is copied: color, layer, linetype, lineweight, text and dimension style, hatch pattern...",
 "**CHPROP**, the Properties palette, and the ribbon's Color, Linetype and Lineweight lists change selected objects directly.",
]),
("h2", "Draw order and text case"),
("p", "Later objects are drawn over earlier ones. **DRAWORDER** (`DR`) sends selected objects to the Front or "
      "Back, or Above or Under others; **TEXTTOFRONT** and **HATCHTOBACK** do it for all text or all hatches. "
      "**TCASE** changes text to UPPER, lower, Sentence, Title or tOGGLE case."),
("h2", "Erasing and getting things back"),
("p", "**ERASE** (`E`, or the `Delete` key) removes the selection; **OOPS** brings back the last erased "
      "objects; `U` undoes any command."),
)

# ------------------------------------------------------------------ 8
ch(
("h1", "Layers, Colors and Linetypes",
 "Keep a drawing organised and make it print the way you want."),
("p", "Layers are like sheets of clear film stacked on each other: walls on one, dimensions on another, "
      "notes on a third. Each layer has a color, linetype and lineweight that its objects use, and can be "
      "turned off, frozen or locked."),
("img", "dlg-layer.png", 8.0, "The Layer Properties Manager (LAYER)."),
("h2", "The Layer Properties Manager"),
("ul", [
 "Open it with **LAYER** (`LA`), **Format > Layer**, or Layer Properties on the ribbon.",
 "**New Layer** adds one with its name ready to type. Double-click a name (or `F2`) to rename it.",
 "Click **On**, **Freeze**, **Lock** or **Plot** in a row to switch it; click **Color**, **Linetype** or **Lineweight** to change them.",
 "Double-click a row (or **Set Current**) to make it the current layer - new objects go on it.",
 "A **No Plot** layer shows on screen but is left out of plots, like Defpoints.",
]),
("h2", "Quick layer tools"),
("table", [2.6, 5.8], [
 ["**Make Current** (`LAYMCUR`)", "Click an object: its layer becomes the current layer."],
 ["**Match** (`LAYMCH`)", "Move the selected objects to the layer of an object you click."],
 ["**Change** (`CL`)", "Move the selected objects to a layer picked from a list."],
 ["`LAYCUR`", "Move the selected objects to the current layer."],
 ["`LAYISO` / `LAYUNISO`", "Show only the layers of the selected objects, and back."],
 ["`LAYOFF` `LAYFRZ` `LAYLCK`", "Turn off, freeze or lock an object's layer by clicking it (and LAYON, LAYTHW, LAYULK to undo)."],
]),
("p", "Choosing a layer in the ribbon's layer list while objects are selected moves them to it; with nothing "
      "selected it sets the current layer."),
("h2", "Colors"),
("p", "Colors open the **Select Color** dialog: the 255 AutoCAD index colors, the standard colors and "
      "grays, ByLayer (the layer's color) and ByBlock. White objects (color 7) show black on a light "
      "background and print black."),
("h2", "Linetypes and lineweights"),
("p", "**LINETYPE** (`LT`) loads CENTER, HIDDEN, DASHED, PHANTOM, DOT, DASHDOT, BORDER, DIVIDE and their "
      "variants, or any `.lin` file. **LTSCALE** (`LTS`) sets the size of the dashes for the whole drawing. "
      "**LWEIGHT** (`LW`) sets line thickness; turn on **LWT** in the status bar to see it."),
)

# ------------------------------------------------------------------ 9
ch(
("h1", "Text",
 "Notes, labels and title blocks, typed right on the drawing."),
("h2", "Paragraph text (MTEXT)"),
("p", "**MTEXT** (`T`, `MT`): pick two corners and an editor opens in that box. The text wraps at the box's "
      "width. `Enter` starts a new line; `Ctrl+Enter`, OK, or a click outside finishes; `Esc` cancels."),
("img", "mtext-crop.png", 6.2, "Typing a note with MTEXT: the formatting bar is under the text box."),
("p", "The formatting bar has the text style, font, height, **Bold**, Italic and Underline (`Ctrl+B`, "
      "`Ctrl+I`, `Ctrl+U`), color, alignment, and a Symbols menu (degree, plus-minus, diameter...). Font, "
      "height, bold, italic, underline and color change only the selected characters. Drag the grips on the "
      "top corners of a selected MTEXT to change its width."),
("h2", "Single-line text (TEXT)"),
("p", "**TEXT** (`DT`): pick the start point, the height and the angle, then type; each line is its own "
      "object. `Enter` on an empty line ends. Double-click any text (or **TEXTEDIT**, `ED`) to edit it."),
("h2", "Text styles and fonts"),
("p", "**STYLE** (`ST`, Format > Text Style) sets a style's font, bold, italic, fixed height, width factor "
      "and oblique angle. Any installed font works, and AutoCAD `.shx` font names are shown with a similar "
      "font. To add fonts, put `.ttf` or `.otf` files in `~/.config/fancypencil/fonts`; they are installed "
      "the next time Fancy Pencil starts. Drawings that use Century Gothic show it when it is installed."),
("h2", "Spelling and case"),
("p", "**SPELL** (`SP`) checks text, MTEXT, attributes and dimension text, with Change, Change All, Ignore "
      "and Add to Dictionary. While typing, unknown words are underlined in red; right-click one for "
      "suggestions. `Shift+F3` in the editor changes the case of the selected words."),
("h2", "Text into outlines (TXTEXP)"),
("p", "**TXTEXP** (Express > Explode Text) turns text into closed polylines that trace each letter, in the "
      "same font you see on the screen, with the capitals as tall as the text height. Use it for "
      "lettering to cut, engrave or extrude: **EXTRUDE** the outlines to make raised letters. "
      "**EXPLODE** (`X`) does the same to TEXT; on MTEXT it first makes one TEXT for each line, so explode "
      "twice to get outlines. The outlines keep the text's layer and color. Undo puts the text back."),
("img", "txtexp-crop.png", 4.0, "Above, ordinary text; below, the same text after TXTEXP."),
)

# ------------------------------------------------------------------ 10
ch(
("h1", "Dimensions and Leaders",
 "Measurements and callouts that look like AutoCAD's."),
("h2", "Dimension commands"),
("table", [2.9, 5.5], [
 ["**DIMLINEAR** (`DLI`)", "Horizontal or vertical: two extension-line origins (or `Enter` and pick an object), then where the dimension line goes."],
 ["**DIMALIGNED** (`DAL`)", "Parallel to the two points."],
 ["**DIMRADIUS** (`DRA`)", "A circle's or arc's radius."],
 ["**DIMDIAMETER** (`DDI`)", "A circle's diameter."],
 ["**DIMANGULAR** (`DAN`)", "The angle between two lines, or of an arc."],
 ["**DIMCONTINUE** (`DCO`)", "A chain of dimensions from the last one."],
 ["**DIMBASELINE** (`DBA`)", "Dimensions stacked from the same first point."],
]),
("p", "Bring a new dimension line near an existing parallel dimension and it snaps to the same line, so "
      "rows of dimensions line up. Grips at the arrows move the dimension line."),
("h2", "Dimension styles"),
("p", "**DIMSTYLE** (`D`) opens the Dimension Style Manager: styles on the left (Set Current, New, Delete), "
      "and tabs for Lines, Symbols and Arrows, Text, Fit and Primary Units, with a live preview. "
      "Arrowheads include closed filled, open, dot, architectural tick and oblique. Changing a style "
      "updates every dimension that uses it."),
("img", "dlg-dimstyle.png", 6.0, "The Dimension Style Manager."),
("h2", "Leaders and multileaders"),
("p", "**LEADER** (`LE`): click the arrow tip, then the points of the line, press `Enter` and type the note. "
      "**Spline** or **STraight**, **Arrow** or **None**, and **NOText** are options at every point prompt. "
      "**MLEADER** (`MLD`) makes AutoCAD's newer multileader, whose arrow, line, landing and note are one "
      "object. Arrow size follows `DIMASZ` and the dimension style."),
)

# ------------------------------------------------------------------ 11
ch(
("h1", "Blocks",
 "Draw a part once, then use it again and again."),
("p", "A block is a group of objects saved under a name - a bolt, a door, a title block. Every copy "
      "(insert) of it changes when the block is edited."),
("h2", "Making a block"),
("p", "**BLOCK** (`B`, Draw > Block: Make): type a name, **Pick point** for the base point (the point "
      "you will insert it by), **Select objects**, and choose whether the objects are retained, converted "
      "to the block, or deleted. Objects selected beforehand are used at once."),
("img", "dlg-block.png", 4.6, "The Block Definition dialog."),
("h2", "Inserting, editing and renaming"),
("ul", [
 "**INSERT** (`I`): pick the block, the insertion point, scale and rotation.",
 "**BEDIT** (`BE`, or right-click a block > Edit Block...): the block opens in its own Block Editor tab with its base point at the red cross. Change it, then **BSAVE** (or `Ctrl+S`) and every copy updates. **BCLOSE** finishes.",
 "**RENAME** renames a block, layer, text style or dimension style everywhere it is used.",
 "**EXPLODE** (`X`) turns one insert back into separate objects; **BURST** keeps attribute values as text.",
 "Right-click a selection > **Create Block from Selection...**; Edit > **Paste as Block** (`Ctrl+Shift+V`).",
]),
("p", "A block made from objects at different heights keeps its 3D shape when inserted."),
)

# ------------------------------------------------------------------ 12
ch(
("h1", "Views and 3D Navigation",
 "Zoom, pan, turn the drawing around and look at it from any side."),
("h2", "Zoom and pan"),
("ul", [
 "Turn the mouse **wheel** to zoom at the cursor; **middle-drag** to pan; **middle double-click** to see everything.",
 "**ZOOM** (`Z`): Extents (`E`), All, Window (`W`), Previous (`P`), Center, or a scale factor. **PAN** (`P`).",
]),
("h2", "The view picker and standard views"),
("img", "viewpicker.png", 1.25, "The view picker."),
("p", "The **view picker** at the top right shows a cube for each standard view: Top in the middle, the "
      "Front, Back, Left and Right views around it, the four isometric views in the corners, and Bottom "
      "underneath. Click one to switch. **-VIEW** and **PLAN** do the same at the command line."),
("p", "In every view you can draw and edit: Front, Back, Left, Right and Bottom each work like an AutoCAD "
      "UCS, with X to the right, Y up and Z toward you. A rectangle drawn in the Front view stands upright."),
("h2", "Orbit"),
("table", [2.6, 5.8], [
 ["**3DORBIT** (`3DO`)", "Drag to turn the view around the target; up stays up."],
 ["**3DFORBIT**", "Free orbit: turn any way inside the green circle, roll outside it."],
 ["**3DCORBIT**", "Drag and let go: the view keeps turning. Click to stop."],
 ["**3DDISTANCE**, **3DSWIVEL**", "Move the camera closer or away; turn the camera like turning your head."],
 ["`Shift` + middle-drag", "Orbit at any time, even in the middle of a command."],
]),
("h2", "Visual styles"),
("p", "**VSCURRENT** (`VS`, View > Visual Styles): **Wireframe** shows every edge, **Hidden** hides what is "
      "behind solids, **Shaded** fills and lights them, and **Shaded with Edges** adds the edges. Plots follow "
      "the visual style."),
("h2", "Working planes"),
("p", "**UCS** sets any plane as the working plane: an object's or face's plane, three points, a new origin, "
      "or turned about X, Y or Z. **DRAFTPLANE** (`F6`) changes the plane you draw on in a 3D view without "
      "changing the view: Ground, Front, Right, three points, or an object's plane."),
("h2", "Viewports on the Model tab"),
("p", "**VPORTS** splits the drawing area into 2, 3 or 4 viewports, each with its own view and style; "
      "**4 > Standard** gives Top, SE Isometric, Front and Right, lined up. Click a viewport to make it "
      "active (blue border); **Single** goes back to one."),
("h2", "Isometric drafting"),
("p", "**ISODRAFT** (the ISO button) turns on AutoCAD's 2D isometric mode: an isometric grid, and crosshairs "
      "and Ortho that follow the isoplane. `F5` switches between the Left, Top and Right isoplanes; ELLIPSE "
      "Isocircle draws a circle as it looks on the current one."),
)

# ------------------------------------------------------------------ 13
ch(
("h1", "3D Solids",
 "Build solid models, join and cut them, and print them in 3D."),
("img", "solids-crop.png", 6.4, "Box, cylinder, sphere, cone and torus in the SE Isometric view, Shaded with Edges."),
("h2", "Basic solids"),
("p", "**BOX**, **WEDGE**, **CYLINDER**, **CONE**, **SPHERE** and **TORUS** (Solids menu) are drawn in the "
      "working plane, so a box drawn in the Front view stands upright. Heights may be negative."),
("h2", "Solids from shapes"),
("table", [2.2, 6.2], [
 ["**EXTRUDE**", "Pulls a closed shape (circle, closed polyline, ellipse, closed spline) into a solid: by a height, a direction, or along a path."],
 ["**REVOLVE**", "Turns a shape about an axis by any angle."],
 ["**SWEEP**", "Carries a shape along a path - a line, arc, polyline, spline or helix."],
 ["**LOFT**", "A solid through cross sections picked in order (a circle to a square, say)."],
 ["**PRESSPULL**", "Click inside a closed area, or on a flat face of a solid, and give a height. On a face, a positive height adds; a negative one cuts in."],
]),
("h2", "Joining and cutting"),
("p", "**UNION** joins solids into one. **SUBTRACT** removes some solids from others: pick what to keep, "
      "`Enter`, then what to remove. **INTERSECT** keeps only the shared part. **SLICE** cuts with a plane, "
      "keeping one side or both. All four are on the right-click menu under **Solid Editing** when solids "
      "are selected."),
("h2", "Platonic solids, the soccer ball and struts"),
("p", "**TETRAHEDRON**, **OCTAHEDRON**, **DODECAHEDRON**, **ICOSAHEDRON** and **SOCCERBALL** (Solids > Platonic "
      "Solids & Soccer Ball): give the centre and the edge length. Type `W` for a wireframe (3D lines you can "
      "snap to), or `F` for a **Frame** of solid struts along every edge."),
("img", "soccer-crop.png", 5.0, "A soccer ball frame: round struts with a ball at every corner, Shaded."),
("p", "**STRUTS** (`WIRESOLID`) does the same for any wireframe you draw: choose the section - **Rectangle**, "
      "**Polygon**, **Circle**, or **PLine** (a closed polyline you drew) - and whether to put a ball at "
      "every corner. Curved polylines, arcs and circles get one strut that follows the curve. **Ends** sets "
      "how far the struts stop short of the wire ends (negative: past them)."),
("h2", "Measuring and printing"),
("p", "**MASSPROP** reports volume, centroid and moments. **3DPRINT** saves solids as STL, 3MF or OBJ for a "
      "3D printer's slicer, checking that each is watertight; **STLIN** brings in STL or OBJ models."),
)

# ------------------------------------------------------------------ 14
ch(
("h1", "Layouts and Plotting",
 "Sheets of paper, viewports at scale, and printing to PDF or a printer."),
("h2", "Layouts"),
("p", "Below the drawing are the **Model** tab and layout tabs (**Layout1**, and more with **+**). A layout "
      "is a sheet of paper at its real size, with a dashed line for the printable area. Draw the border, "
      "title block and notes on it; they belong to that sheet."),
("img", "layout-crop.png", 6.6, "A layout with four viewports (MVIEW 4, Standard): Top, Front, Right and SE Isometric."),
("h2", "Viewports on a layout"),
("ul", [
 "**MVIEW** (`MV`): two corners, **Fit**, or **2**, **3** or **4** at once; **4 > Standard** makes Top, Front, Right and SE Isometric, lined up.",
 "**Double-click inside** a viewport (or `MSPACE`) to work through it: zoom, pan, change the view. **Double-click outside** (or `PSPACE`) to go back to the sheet.",
 "**VPSCALE**: 1:2, 1/4\"=1'-0\", 1:100, Fit... Properties shows and changes a selected viewport's scale, view and lock.",
 "**PAGESETUP** sets the sheet's paper size and orientation; right-click a layout tab to rename, copy or delete it.",
]),
("h2", "The Plot dialog"),
("img", "dlg-plot.png", 5.6, "PLOT (Ctrl+P)."),
("ul", [
 "**Printer/plotter**: \"PDF file\" or any printer your computer knows.",
 "**Paper size**: Letter, Legal, Tabloid, ANSI C-E, ARCH A-E, ISO A4-A0; portrait or landscape.",
 "**What to plot**: Display, Extents, Limits or Window. On a layout, the sheet at 1:1.",
 "**Plot scale**: Fit to paper, 1:1, 1:2 ... or architectural scales such as `1/4\" = 1'-0\"`.",
 "**Black ink only**, **Plot lineweights**, and **Screening %** to print colors lighter.",
 "**Saved settings**: Save As keeps everything under a name. Your last settings come back next time.",
 "**Preview** shows the page first.",
]),
)

# ------------------------------------------------------------------ 15
ch(
("h1", "Files: Opening, Saving and Safety Copies",
 "DXF, DWG, pictures and PDFs - and what happens if the power goes."),
("h2", "Opening and saving"),
("p", "**OPEN** (`Ctrl+O`) shows Fancy Pencil's own file chooser: your folders, drives and recent folders on "
      "the left, the files with sizes and dates on the right. Type a path at the top and press `Enter` to go "
      "straight there. **File > Recent Files** lists the last 12 drawings."),
("img", "dlg-open.png", 7.0, "The Open dialog."),
("p", "**QSAVE** (`Ctrl+S`) saves; **SAVEAS** saves under a new name. Drawings are saved as AutoCAD 2000 "
      "DXF, which every AutoCAD since 2000 opens. **SAVEASR12** writes old R12 DXF. Saving over a drawing "
      "keeps the previous version beside it as a `.bak` file."),
("h2", "DWG files"),
("p", "With ODA File Converter installed (Chapter 2), OPEN shows DWG files too, and Save writes them back as "
      "DWG (AutoCAD 2018 format). Without it, saving a DWG asks for a DXF name instead and never touches the "
      "DWG."),
("h2", "Safety copies and recovery"),
("p", "Every 10 minutes, each drawing with unsaved changes is copied to a safe place; your own file is not "
      "touched. If Fancy Pencil stops unexpectedly, the next start offers to open the copies. "
      "**File > Recover Safety Copies** opens them at any time, and `SAVETIME` sets the minutes between copies."),
("h2", "Pictures, PDF pages and underlays"),
("p", "**IMAGEATTACH** places a JPEG, PNG, TIFF, GIF, BMP or WebP picture to trace over; **PDFATTACH** places "
      "a page of a PDF. Click the lower-left corner and give the width. **IMAGE** (the Image Manager) lists them, "
      "and Detach, Reload, Fade and Change Path. **DWFATTACH** and **DGNATTACH** attach DWF and MicroStation "
      "drawings as underlays. Keep the picture files with the drawing."),
("h2", "3D printing files"),
("p", "**3DPRINT** writes STL, 3MF or OBJ; **STLIN** (or File > Open on an STL or OBJ) reads them."),
)

# ------------------------------------------------------------------ 16
ch(
("h1", "Toolbars and Your Settings",
 "Arrange the screen the way you like it."),
("h2", "Turning toolbars on and moving them"),
("ul", [
 "**View > Toolbars**, or right-click any toolbar, to turn toolbars on and off. **Reset Toolbar Layout** brings back the standard one.",
 "Drag a toolbar by its dotted grip. Drop it near an edge to dock it there - top, bottom, left or right, even just outside the window's edge - or in the middle of the drawing to float it.",
 "A floating toolbar can also be dragged by its title bar: let go of it at an edge of the window and it docks there.",
 "**Clean Screen** (`Ctrl+0`, or View > Clean Screen) hides the ribbon and every toolbar so the drawing gets the whole window; `Ctrl+0` again brings them back.",
 "**Bring Floating Toolbars Back** (View > Toolbars) gathers every floating toolbar into the top left of the drawing, if any have wandered off or ended up behind other windows.",
]),
("h2", "Toolbar Options"),
("p", "**Toolbar Options** (View > Toolbars, or `TOOLBAROPTIONS`) stays open while you work: sliders for icon "
      "size (16 to 64 pixels), line weight and spacing, colors for the background, icon lines, highlights "
      "and filled shapes, and ready-made color schemes. **Standard Look** puts everything back."),
("img", "dlg-tboptions.png", 4.4, "Toolbar Options."),
("h2", "Customize Toolbars"),
("p", "**Customize Toolbars** is all drag and drop. At the top, the toolbar you are editing (or **New "
      "Toolbar...** for your own); below, every command's icon by menu, with a Find box. Drag a command onto the "
      "strip - or straight onto any toolbar on the screen. Drag icons along a toolbar to reorder them, or down "
      "onto the commands to remove them. At other times, `Shift`+drag moves a toolbar button."),
("img", "dlg-tbcustomize.png", 6.8, "Customize Toolbars."),
("h2", "Command aliases"),
("p", "**Tools > Command Aliases > Edit Aliases...** (`ALIASEDIT`) changes the short names: `L` for LINE, "
      "your own for your AutoLISP commands. Your aliases are kept in `~/.config/fancypencil/fancypencil.pgp`, in "
      "AutoCAD's acad.pgp format."),
("h2", "Cursor, selection and background"),
("p", "**OPTIONS** (`OP`, Tools > Cursor, Selection & Background): crosshair size and color, pickbox, snap "
      "marker and grip sizes, rollover tips, and the drawing background (dark, black, gray, paper, white or "
      "any color). These settings are yours: they apply in every drawing and are remembered."),
)

# ------------------------------------------------------------------ 17
ch(
("h1", "Tools",
 "Calculating, units, limits and the command history."),
("h2", "QuickCalc"),
("p", "**QUICKCALC** (`QC`, `Ctrl+8`) opens a calculator beside the drawing. Type an expression and press "
      "`Enter`: `5'6\"*2`, `sqrt(2)*3`, `dist(end,end)/2`. It has number and scientific keys, unit "
      "conversion and variables, and buttons that read a point, distance or angle from the drawing. In the "
      "middle of a command, type `'QC` at a prompt, work out the value, and **Apply** gives it to the command."),
("img", "dlg-quickcalc.png", 3.2, "QuickCalc."),
("h2", "Units"),
("p", "**UNITS** (Format > Units): how lengths are shown - Architectural (`12'-6 1/2\"`), Decimal, "
      "Engineering, Fractional or Scientific - with their precision; angles in degrees, deg/min/sec, grads, "
      "radians or surveyor's units. The sample shows the result as you choose."),
("img", "dlg-units.png", 4.4, "Drawing Units."),
("h2", "Limits, point style and settings"),
("ul", [
 "**LIMITS** (Format > Drawing Limits): the drawing's size, from corners or a sheet size and scale.",
 "**DDPTYPE** (Format > Point Style): the 20 point symbols, and their size.",
 "**SETVAR**, or type any system variable's name, to see or change it.",
]),
("h2", "Command history"),
("p", "**HISTORY** (`F2`) shows everything from the command line for the whole session, with Find, Copy All "
      "and Save As. Tick **Keep a log file** to have it saved every day."),
)

# ------------------------------------------------------------------ 18
ch(
("h1", "AutoLISP",
 "Run AutoCAD's programming language, and your own routines."),
("p", "Fancy Pencil's AutoLISP follows AutoCAD's rules: `(defun c:name (args / locals) ...)` makes a command, "
      "symbols aren't case-sensitive, `(/ 7 2)` is 3, and `*error*` handlers work. About 250 functions are "
      "built in."),
("h2", "Loading routines"),
("ul", [
 "**APPLOAD** (`AP`, Tools > Load AutoLISP...) loads a `.lsp` file.",
 "`(load \"myroutine.lsp\")` at the command line does the same.",
 "Files named `acaddoc.lsp` or `fancypencil.lsp` in the current folder, and `~/.fancypencil.lsp`, load at startup, and `S::STARTUP` runs if defined.",
 "Give a routine an alias in your alias file: `HH, *HELLO` runs `C:HELLO`.",
]),
("h2", "An example"),
("code", "(defun c:box2 (/ p)\n  (setq p (getpoint \"\\nCorner: \"))\n  (command \"rectang\" p (polar p 0 4) )\n  (princ))"),
("p", "Type that at the command line (or put it in a file and load it), then `BOX2`."),
("h2", "What is built in"),
("table", [2.2, 6.2], [
 ["**Core**", "car/cdr, cons, list, append, nth, member, assoc, mapcar, apply, lambda, foreach, repeat, while, cond, if, setq, defun"],
 ["**Math**", "+ - * / rem abs fix float max min sqrt expt exp log sin cos atan logand"],
 ["**Strings**", "strcat strlen substr strcase itoa atoi atof rtos distof angtos chr ascii wcmatch, vl-string-*"],
 ["**Geometry**", "distance angle polar inters trans osnap textbox"],
 ["**Input**", "getpoint getcorner getdist getangle getreal getint getstring getkword initget entsel grread getfiled alert"],
 ["**Drawing**", "entget entmod entmake entdel entlast entnext handent tblsearch tblnext ssget ssadd sslength ssname sssetfirst"],
 ["**Commands**", "command, command-s, vl-cmdf, with pause"],
 ["**Visual LISP**", "vl-sort vl-remove-if vl-position vl-some vl-every vl-catch-all-apply ..."],
 ["**Express Tools**", "acet-error-init, acet-str-*, acet-ss-*, acet-geom-*, acet-pline-make ..."],
]),
("p", "`ssget` supports interactive selection, \"X\", \"W\", \"C\", \"WP\", \"CP\", \"F\", \"L\", \"P\", \"I\" and "
      "\":S\" with filter lists, wildcards and `-4` operators. Not available: ActiveX (`vla-*`, `vlax-*`) and DCL "
      "dialogs."),
("h2", "Examples that come with Fancy Pencil"),
("p", "`/opt/fancypencil/examples/demo-plate.lsp` draws the dimensioned plate on page 1 (load it and type "
      "`PLATE`). `tools.lsp` has TLEN (total length), BCIRC (bolt circle), NUM (numbering) and CL (centre marks)."),
)

# ------------------------------------------------------------------ 19
ch(
("h1", "Keyboard Shortcuts",
 "The keys that save the most time."),
("table", [2.0, 2.3, 4.1], [
 ["**Key**", "**Command**", "**What it does**"],
 ["`Enter` `Space`", "", "Finish typing; repeat the last command at an empty prompt."],
 ["`Esc`", "", "Cancel the command; clear the selection."],
 ["`Delete`", "ERASE", "Erase the selected objects."],
 ["`Ctrl+N`", "NEW", "New drawing."],
 ["`Ctrl+O`", "OPEN", "Open a drawing."],
 ["`Ctrl+S`", "QSAVE", "Save."],
 ["`Ctrl+P`", "PLOT", "Plot."],
 ["`Ctrl+Z` / `Ctrl+Y`", "U / REDO", "Undo / redo."],
 ["`Ctrl+C` / `Ctrl+X`", "COPYCLIP / CUTCLIP", "Copy / cut to the clipboard."],
 ["`Ctrl+V`", "PASTECLIP", "Paste at a point."],
 ["`Ctrl+Shift+C`", "COPYBASE", "Copy with a base point."],
 ["`Ctrl+Shift+V`", "PASTEBLOCK", "Paste as a block."],
 ["`Ctrl+A`", "SELECTALL", "Select everything."],
 ["`Ctrl+0`", "CLEANSCREENON", "Clean Screen: hide the ribbon and toolbars (again to show)."],
 ["`Ctrl+1`", "PROPERTIES", "Properties palette."],
 ["`Ctrl+8`", "QUICKCALC", "Calculator."],
 ["`Ctrl+Tab`", "", "Next drawing tab (`Ctrl+Shift+Tab`: previous)."],
 ["`Ctrl+F4`", "CLOSE", "Close the drawing."],
 ["`F1`", "MANUAL", "Open this manual."],
 ["`F2`", "HISTORY", "Command history."],
 ["`F3`", "OSNAP", "Object snaps on/off."],
 ["`F5`", "ISOPLANE", "Next isoplane (isometric drafting)."],
 ["`F6`", "DRAFTPLANE", "Next drafting plane in 3D views."],
 ["`F7`", "GRID", "Grid on/off."],
 ["`F8`", "ORTHO", "Ortho on/off."],
 ["`F9`", "SNAP", "Snap on/off."],
 ["`F10`", "POLAR", "Polar tracking on/off."],
 ["`F12`", "DYN", "Dynamic input (prompts at the cursor) on/off."],
 ["Wheel", "", "Zoom at the cursor."],
 ["Middle-drag", "PAN", "Pan."],
 ["Middle double-click", "ZOOM E", "Zoom to everything."],
 ["`Shift`+middle-drag", "3DORBIT", "Orbit, at any time."],
 ["Right-click", "", "Menu at the Command prompt; Enter during a command."],
 ["`Shift`+right-click", "", "One-time object snap menu."],
]),
)

# ------------------------------------------------------------------ 20 (reference is generated)
ch(("h1", "Command Reference",
    "Every command, A to Z, with its aliases (the short names you can type instead)."),
   ("cmdref",))

# ------------------------------------------------------------------ 21
ch(
("h1", "When Something Goes Wrong",
 "Common problems and what to do."),
("table", [3.1, 5.3], [
 ["**Fancy Pencil doesn't start**", "Open a terminal and type `fancypencil` to see the message. When started from the menu, messages go to `~/.cache/fancypencil.log`."],
 ["**It seems stuck**", "Press `Esc` a few times. If it still doesn't answer, type `pkill -f fancypencil` in a terminal; the next start offers your safety copies."],
 ["**A .dwg file won't open**", "Install ODA File Converter (Chapter 2): Help > Get ODA File Converter."],
 ["**The .deb opens in Archive Manager**", "Right-click it > Open With... > App Center."],
 ["**Clicks land in the wrong place**", "An object snap may be taking the point: press `F3` to turn OSNAP off, or type `NON` before the point."],
 ["**Lines won't go at an angle**", "ORTHO is on: press `F8`."],
 ["**I can't select or change an object**", "Its layer may be locked or frozen: check the Layer Properties Manager."],
 ["**The dashes of a linetype don't show**", "Change LTSCALE: larger for a big drawing, smaller for a small one."],
 ["**Text looks wrong in AutoCAD**", "Use fonts AutoCAD has too, or send AutoCAD the font."],
 ["**A command did the wrong thing**", "`U` or `Ctrl+Z` undoes it; one U undoes a whole command."],
 ["**I lost my work in a crash**", "File > Recover Safety Copies. Saving also keeps the previous version as name.bak."],
 ["**Out of memory on a big operation**", "Fancy Pencil stops that command and keeps running. Work on fewer objects at once; U undoes any part done."],
]),
("h2", "Your files and settings"),
("table", [4.1, 4.3], [
 ["`~/.config/fancypencil/`", "Settings, recent files, aliases, toolbars, plot settings, views."],
 ["`~/.local/share/fancypencil/autosave/`", "Safety copies."],
 ["`~/.local/share/fancypencil/logs/`", "Command history logs (when turned on)."],
 ["`~/.cache/fancypencil.log`", "Start-up and error messages."],
 ["`/opt/fancypencil/`", "The program itself (installed by the .deb)."],
]),
)
