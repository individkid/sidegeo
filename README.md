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
  * -q run QhickCheck tests  

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
    * Lever -- push or pull other end of tilt segment from pierce point
    * Clock -- rotate picture plane around perpendicular to pierce point  
    * Cylinder -- rotate polytope around tilt line  
    * Scale -- grow or shrink polytope with pierce point fixed  
    * Drive -- move picture plane forward or back  

The -e argument is a little language with reverse lisp syntax. The (int <) and (int val >) functions redo and undo given number of enclosing parentheses. The val in (int val >) is cast to the correct type. In fact, casting is ubiquitous. To keep it simple, there are only int float que ident exp, where val represents any of those. The (ident val ^) function rebinds the given ident to the given which is often a braced exp constant. The (int int $) function refers to given arg in given frame. Thus, all functions have variable number of args. Syntactic sugar labels parenthesis and arguments, so the label can be used in place of int in < > $ functions. Other default functions are (val val +) (val val -) (val val *) (val val /). Default que of que functions transpose (que que xps), determinant (que det), adjugate (que adj), flatten (que vec), together with arithmetic functions suffice for many matrix operations. Default discontinuous functions are (val pos) (val neg) (val zer) (val abs) (val val min) (val val max) (val val div) (val val mod) (que siz). Enque deque unque head tail prefix postfix append deque-term unque-term head-term tail-term deque-chunk head-chunk functions are (que val enq) (que deq) (que unq) (que hdq) (que tlq) (que int prq) (que int poq) (que que apq) (que val dtq) (que val utq) (que htq) (que ttq) (que dcq) (que hcq). Default second order functions are (exp que map) (exp que flt) (exp que fnd) (exp que val fld). There are values and constants of each type. Code, ques, frames, namespaces are containers for data. Since frames and namespaces do not exist as values, they do not have corresponding types. Since ques only exist as values, they are their corresponding type. Since code sometimes exists as values, it has a corresponding type. Values are just constants in non-code containers. Constants are just values in code. Examples of constants of each type are 5 1.9 [a,7] 'id {(+ 1 2)}. Note that bare identifiers are evaluated to the target of the namespace lookup, constants evaluate to themselves, except que constant evaluates its args, and parenthesized code pushes a frame to evaluate in. Thus, open parenthesis starts a frame, whitespace delimited code is evaluated to leave one value in the frame, and close parenthesis replaces the frame by the evaluation of the last value in the frame stripped of one set of braces if any. For example, ({code}) is the same as code, (({{code}})) is the same as code, (('id {code} ^) id) is the same as code, ({code} (0 0 $)) is the same as code. Sculpt calls the given expression for whether to tweak given plane that pertains to given metrics, and whether to keep tweak that changes metrics from first given to second.
