Bugs:

  * Fix local attribute types (they are missing now)

Short term:

  * [x] Make git & github repositories; fix project structure & split modules; use GtkDrawArea
  * [x] Add port/circle for each child node and appropriate spacing.
  * [x] Add local variables with one port on top and one port at the bottom.
  * [x] Proper layout.
  * [x] Parse a-terms and extract dependencies (consider using UUAGC)
  * [x] Add selection menu to select other nonterminals and productions
  * [x] Bug: Fix terminals and local variables
  * [x] Make implicit connections visually distinct (light, dotted, thin)
  * [x] Layout local attributes:
    * [x] Move downward attributes to the left side
    * [x] Multiple layers of local variables
  * [x] Filtering the display attributes:
    * [x] Hide copy rules only visually (Transitive closure should still include the copy rule dependencies)
    * [x] Popup window that can stay open besides main window.
    * [x] Global list of attributes and not per production. Maybe grouped into projects.
    * [x] Transitive dependencies toggle button
  * [x] Attributes interaction:
    * [x] For blue attributes, show type and location
    * [x] For orange attributes, show only the type
    * [x] Open window for source code (when clicked) and refresh contents
  * [x] Flip left local attributes
  * [x] Horizontal scrolling when productions are too large
  * [ ] Add arrow heads to indicate direction (make blue disks into triangles)
  * [ ] Filter window extras:
    * [ ] Transitive dependencies depth slider
    * [ ] "Selected production only"
  * [ ] Better vertical alignment of trapezoids (currently, they are centered)
  * [ ] Improve bezier curves
  * [ ] Hide all local attributes toggle

Future work:

  * [ ] Show node self disks (maybe also for locals) if they're used
  * [ ] Pretty print with wrapping and indentation in code window
  * [ ] Expand/collapse all for the side bar
  * [ ] Search in side bar (is already present, but not satisfactory)
  * [ ] Search bar at top of filter window
  * [ ] In filter window highlight on hover and also highlight transitive dependencies if enabled
  * [ ] Hide locals if they are not used (with fixed point iteration)
  * [ ] Project files (open file / add file) save filters
  * [ ] Ordering attributes
  * [ ] Smart filters / heuristic grouping
  * [ ] Persistance
  * [ ] Limit maximum number of visable attributes so that trapezoids never overlap
  * [ ] Indirect dependency analysis
  * [ ] Open multiple files at the same time (workaround: include all in one .ag file)
  * [ ] Drag local attributes
    * [ ] Flip local attributes (with right-click maybe?)
