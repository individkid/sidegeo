Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding, and https://en.wikipedia.org/wiki/Arrangement_of_hyperplanes for an academic treatment, of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

Notable functions in AffTopo/Naive.hs are topeFromSpace (classify space and regions as polytope), spaceFromTope and topeRegions (find sample space and regions that would classify to polytope), spaceFromPlanes (classify planes as space), planesFromSpace (find sample planes that would classify to space).

Another module, AffTopo/Sculpt.hs, displays polytopes with OpenGL, and allows a user to manipulate them. Sculpt.hs uses a foreign function interface to c code. The first of the following c files implements a thread intraface that communicates between the threads. Microcode is glsl code for graphics engines interfaced to with OpenGl.

  * intraface is queue macro parameterized by type that declares and defines functions to add-to remove-from discard-from the head or tail of queues, in the same thread, with mutex, or with mutex and condition variable.  
  * haskell is a separate thread because Haskell is a high level language with an rts. haskell thread uses condition variable to wait for requests from command thread. haskell thread sends responses with mutex queues to command thread.  
  * console is a separate thread because pselect is incompatible with glfwWait/Poll. console thread uses pselect to wait for user input or for signal from any other thread issuing output.  
  * timewheel is a separate thread because stocks and flows need realtime operation. timewheel thread uses pselect to wait for wallclock or for signal from a configure thread.  
  * process is a separate thread that reads through commandline arguments and waits for injected arguments.  
  * configure is a per-file thread that allows process to continue at eof or yield. non-owner configure tries for writelock on --pid to after end of file, becoming owner and temporarily preventing appends if it gets the writelock. new owner overwrites --pid and allows appends but keeps writelock on its --pid. the owner waits for signal, and moves appended commands to before its --pid. non-owners wait for input by readlock blocking on the owner's --pid, and append --pid terminated output if still at eof after writelock.  
  * command is the main thread because glfw needs the main thread and callbacks should be simple. command is woken by user action, or by glfwPostEmptyEvent from process, configure, timeheel, haskell.  
  * microcode has two versions of code for each task, one version for if the polygons are represented by 6 planes, and one version for if the polygons are represented by 3 points. the tasks are display, find coplanes/copoints, find sidedness of point/plane, find pierce points, reinterpret plane/point.  

The BRINGUP file describes in detail what should happen upon some specific inputs. BRINGUP consists of several pipeclean cases; each starts with a name, short description, goal for success, input conditions, and then describes flow as pseudocode, for cherry picked data state upon call and return, with the following features.

  * comma lists data states  
  * semicolon lists calls  
  * colon indicates return  
  * square brackets with vertical bars indicates parallelism  
  * curly brackets with vertical bars indicates alternation  
  * parentheses facilitates pseudocode flow  
  * redo n means go back skipping n open parenteses plus one per close encountered  
  * done n means go forward skipping n close parantheses plus one per open encountered  

Built in tests include the following, where "linear" refers to any linear space produced by tests run so far. Linear spaces produced by tests are added to a list if not already listed. The results of allSpace are saved for subsequent test runs.

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

The main display window is a hub from which parts or collections of polytopes can be moved to various alternate displays. But from alternate displays, things can only move back to the main display. The following command line arguments are processed in order.

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
  * -a \<name> open alternate display  
  * -A \<name> use indicated alternate display  
  * -t run sanity check  
  * -T run thorough tests  

Left mouse button selects pierce point, and activates menu selected action. Right mouse button toggle suspends action. Keyboard enter selects, and letter moves to menu item in console. Keyboard is effective if display or console in focus. Exit by pressing \<esc>\<enter>. Arrow keys act like mouse motion. \<pgup> and \<pgdn> act like roller. \<home> and \<end> act like left and right buttons. \<ctrl>\<num> binds selection of current menu location to number key. \<alt>\<num> binds current selections to number key.

  * Additive -- click fills in region over pierce point  
  * Subtractive -- click hollows out region under pierce point  
  * Refine -- click adds random plane through pierce point  
  * Display -- click explains pierced plane facet polytope space  
  * Tweak -- click tweaks plane possibly holding space fixed  
  * Perform -- click replaces polytope, opens equalizer, or calls function  
  * Alternate -- click moves pierced target to alternate display  
  * Transform -- modify transform matrix for pierced target  
  * Mouse -- action of mouse motion in Transform mode  
    * Rotate -- tilt polytope(s)/plane around pierce point  
    * Translate -- slide polytope(s)/plane from pierce point  
    * Look -- tilt camera around focal point  
  * Roller -- action of roller button in Transform mode  
    * Cylinder -- rotate polytope around tilt line  
    * Clock -- rotate picture plane around perpendicular to pierce point  
    * Scale -- grow or shrink polytope with pierce point fixed  
    * Drive -- move picture plane forward or back  
  * Level -- target of Alternate/Transform click mode  
    * Plane -- target is the pierced plane  
    * Polytope -- target is the pierced polytope  
    * File -- target is polytopes in the file of pierced  
    * Session -- target is all displayed polytopes  
  * Classify -- type of thing displayed in Display mode  
    * Vector -- display pierce point and coplane  
    * Graph -- display relation of facets  
    * Polyant -- display polyant representation  
    * Place -- display map from boundary to halfspaces  
  * Sample -- whether space fixed in Tweak mode  
    * Symbolic -- classification of space does not change  
    * Numeric -- configuration controls amount of change  
  * Action -- what Perform click does  
    * Configure -- open dialog to decorate plane's facets  
    * Hyperlink -- jump through facet to another space  
    * Execute -- call Haskell function attached to facet  

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
  * --stock name, initial value, minimum maximum saturation values
  * --flow formulae for value, reschedule delay, assignment delay  
  * --listen takes stock for track to record or audit  
  * --source takes sound file, microphone, or noise as volatile stock  
  * --color takes plane subscript and decoration  
  * --window takes plane subscript and file to decorate facets with  
  * --picture is like window except pierce point is fixed  
  * --mirror is like window except tetrahedron is fixed  
  * --action attaches Haskell function to boundary to be activated by click  
  * --matrix takes transformation of display, ignored if not -F file  
  * --project takes slope and cutoff, ignored if not -F file  
  * --configure warp, refine, tweak, color, filter, delay  
  * --inject specifies user action to inject, ignored if not at eof  
  * --jump causes playback to go to location in file  
  * --branch takes file and start stop locations for include  
  * --start goes to a new polytope with optional name for going back  
  * --yield allow other files and command line options to proceed  
  * --delay takes duration for interpolation with next delay 
  * --import takes module name or file path to import for subsequent calls  
  * --call takes Haskell function of source to replace destination  

The --call result string may be longer than the destination, and may contain newlines, to anywhere replace zero or more by zero or more. Between successive --delay commands, transformations are made pseudocontinuous, and other commands are distributed evenly in time. The --flow --color --source --listen commands work together with polytope shape, orientation, and juxtaposition to produce nonlinear sound and shade from simple equations. The simple equations are sums of terms of one coefficient and up to two variables, saturated at maximum and minimum. Note that values can have defaults for when topological features necessary to make the value meaningful do not exist. For example, the area of a face or length of an edge is only meaningful when the specified face or edge exists as a facet of a polytope. The values for the variables come from the following.

  * --stock or --source value  
  * metric of facet qualified by topology  
  * projected metric of facet wrt some point or focal point  

Stock values are used in the following places.

  * --flow value equations  
  * --flow reschedule delay equations
  * --flow value delay equations
  * --delay file stepping rate  
  * --call or --action expressions  
  * --color specifications  
  * --listen waveform sound pipe  

For exmple, a system could consist of --stock --source --listen points at the vertices of a polytope constructed with --point, and --flow faces in several overlapping --plane polytopes.

This is covered by GNU GENERAL PUBLIC LICENSE https://github.com/individkid/sidegeo/blob/master/LICENSE

