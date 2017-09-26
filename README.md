Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding, and https://en.wikipedia.org/wiki/Arrangement_of_hyperplanes for an academic treatment, of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

Notable functions in AffTopo/Naive.hs are topeFromSpace (classify space and regions as polytope), spaceFromTope and topeRegions (find sample space and regions that would classify to polytope), spaceFromPlanes (classify planes as space), planesFromSpace (find sample planes that would classify to space).

Another module, AffTopo/Sculpt.hs, displays polytopes with OpenGL, and allows a user to manipulate them. The following command line arguments are processed in order.

  * -h print usage  
  * -H print readme  
  * -i \<file> load polytope and append changes  
  * -I \<file> \<file> preprocess to add missing headers  
  * -o pack out garbage in graphics buffers  
  * -O \<ext> save minimal commands to produce polytopes  
  * -s prefix commands to save current state  
  * -S \<ext> overwrite commands to save current state  
  * -e \<config> append to last file  
  * -E \<count> change last file to indicated  
  * -t run sanity check  
  * -T run thorough tests  

Left mouse button selects pierce point, and activates menu selected action. Right mouse button toggle suspends action. Keyboard enter selects, and letter moves to menu item in console.

  * Additive -- click fills in region over pierce point  
  * Subtractive -- click hollows out region under pierce point  
  * Refine -- click adds random plane through pierce point  
  * Display -- click explains pierced plane facet polytope space  
  * Tweak -- click tweaks planes holding polytope or space fixed  
  * Action -- click switches to decoration file or opens equalizer panel  
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
  * Sample -- type of thing fixed during click in Tweak mode  
    * Polytope -- classification of space may change  
    * Space -- classification of space does not change  

Configuration/history files consist of commands. User input appends to file. Appended commands immediately control display only when playback is at end of file; otherwise display is controlled from playback location.

  * --plane takes three scalars to set up for classify  
  * --point takes vector for construct and classify  
  * --inflate initializes to facets between inside and outside regions  
  * --fill takes pierce point, removes face and adds outside faces  
  * --hollow takes pierce point, removes face and adds inside faces  
  * --remove takes buffer type and subscript to invalidate  
  * --test check current state agains given value  
  * --sample takes per-boundary sidedness to sample with similar embed  
  * --dual takes per-region sidedness to sample with similar embed  
  * --embed interprets polyants as regions in polytope  
  * --polytope interprets polyants as significant facets  
  * --listen takes point for where track is recorded or audited  
  * --source takes sound file or source  
  * --filter takes plane subscript, per area equalization of tempo, dynamic, tone  
  * --color takes plane subscript and decoration  
  * --window takes plane subscript and file to decorate facets with  
  * --picture is like window except pierce point is fixed  
  * --mirror is like window except tetrahedron is fixed  
  * --matrix takes transformation of display, ignored if not first file  
  * --project takes slope and cutoff, ignored if not first file  
  * --configure warp, refine, color, filter, delay, matrix, arrow, display  
  * --inject specifies user action to inject, ignored if not at eof  
  * --jump causes playback to go to location in file  
  * --branch takes file and start stop locations for include  
  * --yield allow other files and command line options to proceed  
  * --delay takes duration for interpolation with next delay 
  * --import takes module name or file path to import for subsequent calls  
  * --call takes Haskell function of source to replace destination  

Preprocess prepends body length to each line starting with --. The preprocessed body of a command may contain endlines if lines in the original file did not start with --. The --call result string may be longer than the destination, and may contain newlines, to replace one or more by one or more. There are preprocess and unprocess functions in the default import for --call. Between successive --delay commands, ransformations are made pseudocontinuous, and other commands are distributed evenly in time.
