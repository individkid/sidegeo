Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

Notable functions in AffTopo/Naive.hs are topeFromSpace (classify space and regions as polytope), spaceFromTope and topeRegions (find sample space and regions that would classify to polytope), spaceFromPlanes (classify planes as space), planesFromSpace (find sample planes that would classify to space).

Another module, AffTopo/Sculpt.hs, displays polytopes with OpenGL, and allows a user to manipulate them. The default file for human readable history and configuration is ./.sculpt . The following command line arguments are processed in order.

  * -i start interactive mode  
  * -e \<metric> start animation that tweaks planes according to a metric  
  * -c \<file> change file for configuration and history  
  * -o \<file> save polytope in format indicated by file extension  
  * -f \<file> load polytope in format indicated by file extension  
  * -t \<ident> change current polytope to one from history  
  * -n \<shape> replace current polytope by builtin polytope  
  * -r randomize direction and color of light sources
  * -s resample current space to planes with same sidedness  
  * -S resample current polytope to space and planes  

Left mouse button selects pierce point, and activates menu selected action. Right mouse button deselects pierce point, and deactivates action. Keyboard enter selects, and letter or arrow moves to menu entry in console.

  * Additive -- click fills in region over pierce point  
  * Subtractive -- click hollows out region under pierce point  
  * Refine -- click adds random plane through pierce point  
  * Transform -- modify model or perspective matrix  
  * Manipulate -- modify pierced plane  
  * Mouse -- action of mouse motion in Transform/Manipulate modes  
    * Rotate -- tilt polytope/plane around pierce point  
    * Translate -- slide polytope/plane from pierce point  
    * Look -- tilt camera around focal point  
    * Screen -- window moves over display fixed to screen
    * Window -- move window and display on screen
  * Roller -- action of roller button in Transform/Manipulate modes  
    * Lever -- push or pull other end of tilt segment from pierce point
    * Clock -- rotate picture plane around perpendicular to pierce point  
    * Cylinder -- rotate polytope around tilt line  
    * Scale -- grow or shrink polytope with pierce point fixed  
    * Drive -- move picture plane forward or back  
    * Size -- resize window with display fixed to screen
    * Aspect -- change ratio between window dimensions
  * Window -- how operating system window move affects display
    * Physical -- display appears fixed on screen
    * Virtual -- display appears fixed in window
  * Corner -- how operating system window resize affects display
    * Opposite -- corner of display opposite dragged appears fixed
    * Northwest -- upper left corner of display appears fixed
    * Northeast -- upper right corner of display appears fixed
    * Southwest -- lower left corner of display appears fixed
    * Southeast -- lower right corner of display appears fixed
