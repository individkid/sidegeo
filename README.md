Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding, and https://en.wikipedia.org/wiki/Arrangement_of_hyperplanes for an academic treatment, of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

Notable functions in AffTopo/Naive.hs are topeFromSpace (classify space and regions as polytope), spaceFromTope and topeRegions (find sample space and regions that would classify to polytope), spaceFromPlanes (classify planes as space), planesFromSpace (find sample planes that would classify to space).

Another module, AffTopo/Sculpt.hs, displays polytopes with OpenGL, and allows a user to manipulate them. The following command line arguments are processed in order.

  * -i \<file> load polytope and append changes  
  * -I \<file> preprocess to add missing headers  
  * -b \<file> start file with reference to disabled jump  
  * -B \<file> \<ext> backup and merge all to one (re)opened  
  * -f \<file> load from format indicated by file extension  
  * -F \<ext> save to format indicated by file extension  
  * -o pack out garbage in graphics buffers  
  * -O \<ext> save minimal commands to produce polytopes  
  * -l \<module> add import for call configures  
  * -L \<path> add path for module files  
  * -s resample current space to planes with same sidedness  
  * -S resample current polytope to space and planes  
  * -d display space and regions  
  * -D display polyants  
  * -t run sanity check  
  * -T run thorough tests  

Left mouse button selects pierce point, and activates menu selected action. Right mouse button toggle suspends action. Keyboard enter selects, and letter moves to menu item in console.

  * Additive -- click fills in region over pierce point  
  * Subtractive -- click hollows out region under pierce point  
  * Refine -- click adds random plane through pierce point  
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

Configuration/history files consist of commands. Append commands, schedule commands, and user input append to file. Appended commands immediately control display only when playback is at end of file; otherwise display is controlled from playback location.

  * --plane takes three scalars to set up for classify  
  * --point takes vector for construct and classify  
  * --inflate initializes to facets between inside and outside regions  
  * --fill takes pierce point, removes face and adds outside faces  
  * --hollow takes pierce point, removes face and adds inside faces  
  * --remove takes buffer type and subscript to invalidate  
  * --sample takes per-boundary sidedness to sample with similar embed  
  * --dual takes per-region sidedness to sample with similar embed  
  * --polyant specifies corner for subsequent commands  
  * --embed interprets polyants as regions in polytope  
  * --polytope interprets polyants as significant facets  
  * --source takes sound file or source  
  * --filter takes plane subscript, per area equalization of tempo, dynamic, tone  
  * --color takes plane subscript and decoration  
  * --window takes plane subscript and file to decorate facets with
  * --matrix takes transformation of display, ignored if not first -i  
  * --project takes slope and cutoff, ignored if not first -i  
  * --configure takes autowarp, autorefine, autocolor, autofilter, autodelay, automatrix  
  * --inject specifies user action to inject, ignored if not at eof  
  * --schedule appends at a wallclock time, ignored if not at eof  
  * --jump optionally causes playback to go to location in file  
  * --branch takes file and start stop locations for include  
  * --delay takes duration for playback, ignored if at eof  
  * --call takes Haskell function of source to replace destination  

Preprocess prepends body length to each line starting with --. For preprocess -I files, --- indicates continuation of multiline body. The --call result string may be longer than the destination, and may contain newlines, to replace one or more by one or more.