Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

Notable functions in AffTopo/Naive.hs are topeFromSpace (classify space and regions as polytope), spaceFromTope and topeRegions (find sample space and regions that would classify to polytope), spaceFromPlanes (classify planes as space), planesFromSpace (find sample planes that would classify to space).

Another module, AffTopo/Sculpt.hs, displays polytopes with OpenGL, and allows a user to manipulate them. The following command line arguments are processed in order.

  * -i \<file> load and append to configuration file  
  * -I \<file> follow file for readonly polytope  
  * -b \<file> start new file with reference to disabled jump  
  * -B \<file> append changes to make current same as given file  
  * -f \<file> load polytope in format indicated by file extension  
  * -F \<file> save polytope in format indicated by file extension  
  * -p \<name> replace current polytope by builtin polytope  
  * -P \<name> change current polytope to one from history  
  * -s resample current space to planes with same sidedness  
  * -S resample current polytope to space and planes  
  * -o optimize away unused boundaries
  * -O split polytopes into disjoint covering subpolytopes
  * -d display space and regions  
  * -D display polyants  
  * -t run sanity check 
  * -T run thorough tests 

Left mouse button selects pierce point, and activates menu selected action. Right mouse button toggle suspends action. Keyboard enter selects, and letter moves to menu item in console.

  * Additive -- click fills in region over pierce point  
  * Subtractive -- click hollows out region under pierce point  
  * Refine -- click adds random plane through pierce point  
  * Transform -- modify model or perspective matrix  
  * Manipulate -- modify pierced plane  
  * Mouse -- action of mouse motion in Transform/Manipulate modes  
    * Rotate -- tilt polytope/plane around pierce point  
    * Translate -- slide polytope/plane from pierce point  
    * Look -- tilt camera around focal point  
  * Roller -- action of roller button in Transform/Manipulate modes  
    * Cylinder -- rotate polytope around tilt line  
    * Clock -- rotate picture plane around perpendicular to pierce point  
    * Scale -- grow or shrink polytope with pierce point fixed  
    * Drive -- move picture plane forward or back  

Configuration/history files consist of commands.

 * --plane takes three scalars  
 * --face takes six plane subscripts  
 * --frame takes three triples of plane subscripts  
 * --remove takes buffer type and subscript to invalidate  
 * --sample takes sidednesses to sample  
 * --region takes polyant for region to inflate  
 * --polyant takes polyant to add to polytope  
 * --volume takes polyant, amplitude, fundamental, harmonics, envelope, rhythm  
 * --filter takes plane subscript, projected area parameterized equalization of tempo, dynamic, tone  
 * --color takes plane subscript and decoration  
 * --matrix takes transformation of display  
 * --project takes slope and cutoff
 * --configure takes autowarp, autorefine, autodecorate, autovibrate, autodelay, automerge  
 * --branch takes file and location for starting state  
 * --jump optionally causes playback to go to location in file  
 * --delay takes duration for playback, ignored until playback  
 * --call takes argument for Haskell command, that can access entire file including command being appended to file, and that returns zero or more replacement(s) for command being appended
 * --comment allows call action to record data for future use  
 * --inject specifies user action to inject, ignored during playback  
 * --schedule appends a comment at a wallclock time, ignored during playback  
 * --toggle changes jump option at given location, ignored during playback  
 * --replace changes comment at location with same size string, ignored during playback  
