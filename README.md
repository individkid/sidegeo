Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding, and https://en.wikipedia.org/wiki/Arrangement_of_hyperplanes for an academic treatment, of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

Notable functions in AffTopo/Naive.hs are topeFromSpace (classify space and regions as polytope), spaceFromTope and topeRegions (find sample space and regions that would classify to polytope), spaceFromPlanes (classify planes as space), planesFromSpace (find sample planes that would classify to space).

Another module, AffTopo/Sculpt.hs, displays polytopes with OpenGL, and allows a user to manipulate them. Sculpt.hs uses a foreign function interface to c code. The first of the following c files implements a thread intraface that communicates between the threads. Microcode is glsl code for graphics engines interfaced to with OpenGl. Common is shared types, utility functions, and mutex queues.

  * queue is macro parameterized by type that declares and defines functions to add-to remove-from discard-from the head or tail of queues, in the same thread, with mutex, or with mutex and condition variable.  
  * haskell is a separate thread because Haskell is a high level language with an rts. haskell thread uses condition variable to wait for requests from command thread. haskell thread sends responses with mutex queues to command thread.  
  * console is a separate thread because pselect is incompatible with glfwWait/Poll. console thread uses pselect to wait for user input or for signal from any other thread issuing output.  
  * timewheel is a separate thread because stocks and flows need realtime operation. timewheel thread uses pselect to wait for wallclock or for signal from a configure thread.  
  * process is a separate thread that reads through commandline and configurefile arguments, and waits for injected arguments. process has per-file threads that allow process to continue at eof or yield.  
  * command is the main thread because glfw needs the main thread and callbacks should be simple. command is woken by user action, or by glfwPostEmptyEvent from process, configure, timeheel, haskell.  
  * microcode has two versions of code for each task, one version for if the polygons are represented by 6 planes, and one version for if the polygons are represented by 3 points. the tasks are display, find coplanes/copoints, find sidedness of point/plane, find pierce points, reinterpret plane/point.  
  * common contains mutex queues that are staged to and from thread local queues. command has void function pointer. haskell has event enumeration. console has endline terminated string. process has command lines. Command, Event, Kind, Data, Output, Option, File are unique types. CmdChar, CmdInt, HsChar, HsInt are uniquefied types. the basename identifies the consumer of the queue items. Cmn prefixed queues in common map onto unprefixed queues in threads. Hs prefixed queues in haskell and Cmd prefixed queues in command map into Cmn queues in common.  

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
  * -H run tests  
  * -o \<file> open or toggle polytope file  
  * -O \<name> open or select alternate display  
  * -a \<config> append to last file  
  * -A \<file> change last file to indicated  
  * -l \<plane> link selected and given plane  
  * -L \<plane> select plane in last file  
  * -s \<name> serve remote framebuffer clients  
  * -S \<file> serve sculpt command clients  

Left mouse button selects pierce point, and activates menu selected action. Right mouse button toggle suspends action. Keyboard enter selects, and letter moves to menu item in console. Keyboard is effective if display or console in focus. Exit by pressing \<esc>\<enter>. Arrow keys act like mouse motion. \<pgup> and \<pgdn> act like roller. \<home> and \<end> act like left and right buttons. Macros may be bound to planes as Lua scripts, and activated in Execute mode. The -O option opens a display over the main display with same size and position. In Panel mode, a left mouse click over the main, or an alternate display, opens a widget panel with same size and position.

  * Additive -- click fills in region over pierce point  
  * Subtractive -- click hollows out region under pierce point  
  * Refine -- click adds random plane through pierce point  
  * Execute -- click calls Lua function attached to plane  
  * Mouse -- action of mouse motion in Transform mode  
    * Rotate -- tilt polytope(s)/plane around pierce point  
    * Translate -- slide polytope(s)/plane from pierce point  
    * Look -- tilt camera around focal point  
  * Roller -- action of roller button in Transform mode  
    * Cylinder -- rotate polytope around tilt line  
    * Clock -- rotate picture plane around perpendicular to pierce point  
    * Scale -- grow or shrink polytope with pierce point fixed  
    * Drive -- move picture plane forward or back  
  * Target -- target of Move/Copy/Transform click mode  
    * Plane -- target is the pierced plane  
    * Polytope -- target is the pierced polytope  
    * Alternate -- target is planes in focused display  
    * Session -- target is all displayed polytopes  
  * Transform -- modify transform matrix for pierced target  
  * Move -- click moves pierced target to alternate display  
  * Copy -- click copies pierced target to alternate display  
  * Sample -- whether space fixed in Tweak mode  
    * Symbolic -- classification of space does not change  
    * Numeric -- configuration controls amount of change  
  * Tweak -- click tweaks plane possibly holding space fixed  
  * Classify -- type of thing displayed in Display mode  
    * Vector -- display pierce point and coplane  
    * Graph -- display relation of facets  
    * Polyant -- display polyant representation  
    * Place -- display map from boundary to halfspaces  
  * Display -- click explains pierced plane facet polytope space  
  * Virtual -- whether cursor is captured or forwarded  
    * Surface -- cursor controls plane in polytope  
    * Content -- cursor controls decoration on plane  
  * Widget -- which panel opened by Panel  
    * Topology -- open panel for classifications to sample  
    * Decorate -- open panel for bitmap, screen saver, rfb  
    * System -- open panel for stocks flows and attachments  
  * Panel -- click opens panel to modify clicked items  

Configuration/history files consist of commands. User input appends to file. Appended commands immediately control display only when playback is at end of file; otherwise display is controlled from playback location. Incorrectly formatted commands are treated as intervening text. Correctly formatted commands with incorrect arguments from files cause warnings the first few times, then cause the remainder of the file to be ignored. Incorrect arguments on the command line cause warnings only. Command formats are specified by lists of the following subexpressions.

  * Dis - match and discard to pass, or mismatch to fail
  * Not - mismatch discard and rewind to pass, or match to fail
  * While - match repeat, or mismatch discard and rewind, to pass
  * Loop - match repeat to pass
  * If - continume from one or other depending on match to minimum
  * Char White Text Int Float - match to pass
  * Term - produce to pass

For example, a whitespace delimited nonempty string optionally preceded by whitespace is matched by Dis,2,While,1,White,Char,While,3,Not,1,White,Char.

  * --plane takes three scalars to set up for classify  
  * --point takes vector for construct and classify  
  * --inflate initializes to facets between inside and outside regions  
  * --fill adds faces attached to outside region and removes inside faces  
  * --hollow adds faces attached to inside region and removes outside faces  
  * --sample takes per-boundary sidedness to sample with similar embed  
  * --dual takes per-region sidedness to sample with similar embed  
  * --embed interprets polyants as regions in polytope  
  * --polytope interprets polyants as significant facets  
  * --matrix transforms the polytpe created by this file  
  * --time formulae for value, reschedule delay, assignment delay  
  * --change value for change to named timewheel state  
  * --metric funtion name, facet specifications for term values  
  * --listen takes stock for track to record or audit  
  * --source takes sound file, microphone, or noise as volatile stock  
  * --script binds Lua expression to evaluate when stock changes  
  * --color takes plane subscript decoration and transformation  
  * --action attaches Lua function to boundary to be activated by click  
  * --configure change behaviors by degree  
  * --option specifies command line option to inject  
  * --menu changes to menu item to inject to console  
  * --motion mouse and roller motion and click to inject  
  * --bind binds Lua function to function key in console  
  * --yield allow other files and command line options to proceed  
  * --call takes Lua function and arguments to start  
  * --side enque command for debug and internal use  

The --call --yield --inject -o commands synchronize actions. To wait for a --call Lua script to complete before proceeding, issue --inject-o_ --callLuaScript --yield. The --inject disables the current file after --yield allows injected options to be executed. The --call kicks off the Lua script. A -o injected by the Lua script will be after the one injected by the --inject, because -o is processed by the same thread as the --inject.

The --time --color --source --listen --metric commands work together with polytope shape, orientation, and juxtaposition to produce nonlinear sound and shade from simple equations. The simple equations are quotients of sums of terms of one coefficient and up to two variables. Each --time has a value used as variables and for other purposes.

  * values for --color are given by --time values  
  * wave for --listen port is piped from --time value  
  * term variables are --time --metric --source values  
  * new --time value comes from term sum quotient and saturation values  
  * value change delay comes from term sum quotient  
  * reschedule delay comes from term sum quotient   

For exmple, a system could consist of --time --source --listen points at the vertices of a polytope constructed with --point, and --metric faces in several overlapping --plane polytopes. Stock values in --time --source --listen items associated with points could flow along lines of sight. A line of sight metric could be 0 if any impermeable face intervenes between the points, and 1 if no impermeable faces intervene. Then, the --time formula for the stock associated with a point could multiply the metric by the stock associated with the other point.

Functions in Metric.c are used in stock flow formulae specified by —time and —metric. Also are functions used to pretify collections of polytopes such as tetrahedron overlaps. For example, sightLine does pershader on each file with feather arrow for pierce points on line between vertex in one file and vertex in another file.

  * sightLine finds if any planes between vertices are impermeable  
  * regexPlane gets properties such as permeable from name  
  * inverseSquare returns inverse square of distance between vertices  
  * solidMetric tweaks polytopes to not overlap  
  * localMetric tweaks polytopes to within a volume  
  * downMetric tweaks polytopes to rest on a plane  
  * printMetric tweaks precarious planes  
  * lengthMetric tweaks outlier lengths  
  * areaMetric tweaks outlier areas  
  * volumeMetric tweaks outlier volumes  

Functions in Callback.c allow user action like mouse motion to transform displayed polytopes. There is a global matrix, affineMat, that is applied to all polytopes. And there are compensating matrices per polytope per display to allow any polytopes to appear fixed even as affineMat transforms any unfixed polytopes. When a display loses focus, affineMat is saved in the display's context, so changes to affineMat by other displays can be incorporated into a display's context when focus returns to it. For example, if the mode is Transform, and the right mouse button is clicked, the transformation is suspended while the mouse is moved to another display. If transformation continues in the other display, in the same or in a different Transformation mode, affineMat changes. Now, if the mouse moves back to the original display, and the right button is pressed, transformation resumes in the original mode fixed point and mouse position, but transformations from the other display are incorporated and not lost. Transforming individual planes is accomplished by moving them to a special polytope, transforming them, and then moving them back as altered planes. Altering planes through the Transform Plane mode, or changing the polytope with the Additive or Subtractive Sculpt modes, updates the representation of the topology of the polytope by calling functions in Interface.c to send events to the Haskell.c thread. Commands in Configure.c executed from files read by Process.c also change polytope topology by scheduling functions from Interface.c in the Command.c thread.

The --color command may specify a color per region in a 2d space, wave buffer piped from --time value, or remote framebuffer from Socket.c as the texture on a plane. If Virtual mode is Content, the cursor controls the remote cursor when it is over the a remote framebuffer plane. The --listen command may specify speaker, -S file, or libsndfile. In case --listen specifies -S file, it pipelines a --time value to a wave buffer that the -S clients can read as framebuffers. The -s option presents the indicated display, main or alternate, to remote framebuffer clients. The -S option appends commands from cutbuffer text to the given file.

The development plan is to complete the namesake usage features first, then proceed to more detailed uses.

  * Sculpt: -o --plane (_) --inflate Transform (Session Rotate Cylinder) Refine Additive Subtractive Event (Locate Fill Hollow Inflate Faces Divide Vertex Index Done)  
  * Edit: -h -O -a -A -l -L --yield --inject --menu --plane --point Move Copy Mouse Roller Target Tweak (Numeric) Event  
  * Analyze: Sample Display Classify Panel (Topology) --sample --dual --embed --polytope -H  
  * Synthesize: --call --bind --action Execute --time --change --listen (speaker)  
  * Decorate: libsndfile --script --source --metric --color --configure -s -S Panel  

The C code leverages the following libraries, languages, and protocols:

  * Haskell/Cabal  
  * Opengl/Glsl  
  * C++  
  * Posix threads  
  * github/libpqueue/rbtree  
  * portaudio/PaUtil  
  * libsndfile  
  * Lua  
  * Fltk/Fluid  
  * Posix sockets  
  * Remote framebuffer  

The code is partitioned to the following files grouped by thread:

  * Memory management: Types.h Queue.h Queue.cpp Common.h Common.cpp Convert.c Wrap.c  
  * Opengl commands: Main.h Main.c Setup.c Render.c Microcode.c Callback.c  
  * Command thread: Debug.c Interface.c Metric.c Command.c  
  * Process thread: Configure.c Option.c Process.c  
  * Haskell thread: Sculpt.hs Naive.hs Haskell.c  
  * Panel thread: Topology.fl Decorate.fl System.fl Fltk.cpp Panel.c
  * Other threads: Timewheel.c Lua.c Console.c Socket.c  

This is covered by GNU GENERAL PUBLIC LICENSE https://github.com/individkid/sidegeo/blob/master/LICENSE

