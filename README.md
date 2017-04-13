Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

Notable functions in AffTopo/Naive.hs are topeFromSpace (classify space and regions as polytope), spaceFromTope and topeRegions (find sample space and regions that would classify to polytope), spaceFromPlanes (classify planes as space), planesFromSpace (find sample planes that would classify to space).

Another module, AffTopo/Sculpt.hs, displays polytopes with OpenGL, and allows a user to manipulate them. The default directory for human readable history and configuration is .sculpt/. The following command line arguments are processed in order.

  * -i start interactive mode  
  * -e \<metric> start animation that tweaks planes according to a metric  
  * -d \<directory> changes directory for history and configuration  
  * -n \<directory> copies current state to new directory  
  * -o \<file> save polytope in format indicated by file extension  
  * -f \<file> load polytope in format indicated by file extension  
  * -l \<shape> replace current polytope by builtin polytope  
  * -t \<ident> change current polytope to one from history  
  * -r randomize direction and color of light sources
  * -s resample current space to planes with same sidedness  
  * -S resample current polytope to space and planes  

Left mouse button (de)selects pierce point(s). Right mouse button deselects pierce point(s) and changes the mode by menu.

  * Mouse -- submode of Transform for action of mouse motion  
    * Rotate -- rotate about pierce point  
    * Translate -- slide starting from pierce point  
    * Look -- rotate about focal point  
  * Roller -- submode of Transform for action of roller button  
    * Rotate -- rotate about line on pierce and focal points  
    * Scale -- bigger or smaller with pierce point fixed  
    * Drive -- move picture plane forward or back  
  * Transform -- multiply the model view matrix  
  * Refine -- add random boundary through pierce point  
  * Additive -- add faces around region above pierce point  
  * Subtractive -- remove face of pierce point  
  * Drag -- pin first two and move third pierce point by mouse and roller  
