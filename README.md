Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding, and https://en.wikipedia.org/wiki/Arrangement_of_hyperplanes for an academic treatment, of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

Notable functions in AffTopo/Naive.hs are topeFromSpace (classify space and regions as polytope), spaceFromTope and topeRegions (find sample space and regions that would classify to polytope), spaceFromPlanes (classify planes as space), planesFromSpace (find sample planes that would classify to space).

Another module, AffTopo/Sculpt.hs, displays polytopes with OpenGL, and allows a user to manipulate them. The following command line arguments are processed in order.

  * -h print usage  
  * -H print readme  
  * -f \<file> load polytope and append changes  
  * -F \<file> switch to file for perspective  
  * -o pack out garbage in graphics buffers  
  * -O \<ext> save minimal commands to produce polytopes  
  * -s prefix commands to save current state  
  * -S \<ext> overwrite commands to save current state  
  * -e \<config> append to last file  
  * -E \<file> change last file to indicated  
  * -t run sanity check  
  * -T run thorough tests  

Tests include the following, where "linear" refers to any linear space produced by tests run so far. Linear spaces produced by tests are added to a list if not already listed. The results of allSpace are saved for subsequent test runs.

  * classify of random planes should be linear  
  * anySpace should be linear  
  * every space from allSpace of linear should be linear  
  * allSpace of linear should be equal to allSpace of linear  
  * linear should be equivalent to one of allSpace of linear  
  * classify of sample of linear should be equivalent  
  * superspace of linear and linear should be linear  
  * polyant has the same facets no matter which facet it is in  
  * classify of sample of classify of any embed in a linear should be equivalent  
  * every plane through a point should have a coplane on the copoint  
  * every edge facet of a region should have two and only two vertex facets  

Left mouse button selects pierce point, and activates menu selected action. Right mouse button toggle suspends action. Keyboard enter selects, and letter moves to menu item in console. Keyboard is effective if display or console in focus. Exit by pressing \<esc>\<enter>. Arrow keys act like mouse motion. \<pgup> and \<pgdn> act like roller. \<home> and \<end> act like left and right buttons.

  * Additive -- click fills in region over pierce point  
  * Subtractive -- click hollows out region under pierce point  
  * Refine -- click adds random plane through pierce point  
  * Display -- click explains pierced plane facet polytope space  
  * Tweak -- click tweaks plane possibly holding space fixed  
  * Action -- click replaces polytope, opens equalizer, or calls function  
  * Transform -- modify world or perspective matrix  
  * Modify -- modify pierced polytope independent of others  
  * Manipulate -- modify pierced plane  
  * Mouse -- action of mouse motion in Transform/Modify/Manipulate modes  
    * Rotate -- tilt polytope/plane around pierce point  
    * Translate -- slide polytope/plane from pierce point  
    * Look -- tilt camera around focal point  
  * Roller -- action of roller button in Transform/Modify/Manipulate modes  
    * Cylinder -- rotate polytope around tilt line  
    * Clock -- rotate picture plane around perpendicular to pierce point  
    * Scale -- grow or shrink polytope with pierce point fixed  
    * Drive -- move picture plane forward or back  
  * Classify -- type of thing displayed in Display mode  
    * Vector -- display pierce point and coplane  
    * Graph -- display relation of facets  
    * Polyant -- display polyant representation  
    * Place -- display map from boundary to halfspaces  
  * Sample -- whether space fixed in Tweak mode  
    * Symbolic -- classification of space does not change  
    * Numeric -- configuration controls amount of change  

Configuration/history files consist of commands. User input appends to file. Appended commands immediately control display only when playback is at end of file; otherwise display is controlled from playback location.

  * --plane takes three scalars to set up for classify  
  * --point takes vector for construct and classify  
  * --inflate initializes to facets between inside and outside regions  
  * --fill adds faces attached to outside region and removes inside faces  
  * --hollow adds faces attached to inside region and removes outside faces  
  * --remove takes buffer type and subscript to invalidate  
  * --test check current state agains given value  
  * --sample takes per-boundary sidedness to sample with similar embed  
  * --dual takes per-region sidedness to sample with similar embed  
  * --embed interprets polyants as regions in polytope  
  * --polytope interprets polyants as significant facets  
  * --listen takes point for where track is recorded or audited  
  * --source takes sound file or source  
  * --filter takes plane subscript, per area, per flow equalization  
  * --color takes plane subscript and decoration  
  * --window takes plane subscript and file to decorate facets with  
  * --picture is like window except pierce point is fixed  
  * --mirror is like window except tetrahedron is fixed  
  * --system specify flow delta as linear combination of region stocks  
  * --action attaches Haskell function to boundary to be activated by click  
  * --matrix takes transformation of display, ignored if not -F file  
  * --project takes slope and cutoff, ignored if not -F file  
  * --configure warp, refine, tweak, color, filter, delay  
  * --inject specifies user action to inject, ignored if not at eof  
  * --jump causes playback to go to location in file  
  * --branch takes file and start stop locations for include  
  * --yield allow other files and command line options to proceed  
  * --delay takes duration for interpolation with next delay 
  * --import takes module name or file path to import for subsequent calls  
  * --call takes Haskell function of source to replace destination  

Preprocess prepends body length to each line starting with --. The preprocessed body of a command may contain endlines if lines in the original file did not start with --. The --call result string may be longer than the destination, and may contain newlines, to replace one or more by one or more. There are preprocess and unprocess functions in the default import for --call. Between successive --delay commands, transformations are made pseudocontinuous, and other commands are distributed evenly in time. The filter associated with a plane affects the color and sound of the plane's facets. Each slider of the equalizer is an input to a linear equation for a point on a curve. Other inputs the the equalizer equations are projected area of the facet and system flow through the plane. Equalizer curves determine the following.

  * color of facets as brightness in terms of color as a frequency  
  * tone of notes as amplitude in terms of cycles per second of sound  
  * envelope of notes as volume in terms of portion of note length  
  * duration of notes as hight above minimum at start of note  
  * rhythm of notes as passage of waveform up through zero  

