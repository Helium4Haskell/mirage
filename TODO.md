Questions:

  * How to deal with constant attributes
  * Should the visual filter also merge local attributes and child/lhs attributes with the same name?

  * To differentiate the connections a bit more I could introduce a kind of
    randomized "wind" effect where each line moves a little bit. That should make
    it easier to see which parts are connected.

    Current door de graaf

    Beweeg alle nodes los van elkaar of alleen de control points

Bugs:

  * [ ] Fix local attribute arrangement "ioMatch"
  * [ ] Fix double displacement of local attributes
  * [ ] Hide filter window instead of closing

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
  * [x] Filtering the display attributes
    * [x] Hide copy rules only visually (Transitive closure should still include the copy rule dependencies)
    * [x] Popup window that can stay open besides main window.
    * [x] Global list of attributes and not per production. Maybe grouped into projects.
    * [x] Transitive dependencies toggle button
    * [ ] "Selected production only"
    * [ ] Search bar at top of filter window
  * [ ] Events:
    * [ ] Hover over attributes to see implementation and source location
    * [ ] Drag background to pan
    * [ ] Drag local variables
    * [ ] Flip local attributes (with right-click maybe?)
  * [ ] Improve bezier curves
  * [ ] Flip left local attributes
  * [ ] Expand all + search box for the side panel
  * [ ] Add arrow heads to indicate direction (make blue disks into triangles)
  * [ ] Better vertical alignment of trapezoids (currently, they are centered)
  * [ ] Search in sidebar (is already present, but not satisfactory)
  * [ ] Don't show node self disks (maybe also for locals) if they're not used

Long term:

  * [ ] In filter window highlight on hover and also highlight transitive dependencies if enabled
  * [ ] Hide locals if they are not used (with fixed point iteration)
  * [ ] Project files (open file / add file) save filters
  * [ ] Ordering attributes
  * [ ] Smart filters / heuristic grouping
  * [ ] Persistance
  * [ ] Limit maximum number of visable attributes so that trapezoids never overlap
  * [ ] Indirect dependency analysis
  * [ ] Open multiple files at the same time (workaround: include all in one .ag file)
