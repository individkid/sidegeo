Can polytopes be represented without resort to vectors? A polytope as a graph of facets fails to capture convexity, so a different representation is required. See http://www.sidegeo.blogspot.com/ for my understanding of the relevant math. In particular, note that a polytope can be embedded in a finite space, and any finite space with a property I call "linear" can be converted to numeric planes. These operations implemented here can form the kernel of applications for graphically exploring 2 or 3 dimensional objects, and for symbolically exploring spaces of more dimensions than 3.

The following data and functions are exported.

Data types Boundary, Region, Sidedness, Color, and Space are opaque.

Function side takes Space, Boundary, Region, returns Sidedness, and indicates in the space, which "side" of the boundary the region is on.

Function bounds takes Space and returns list of Boundary in the space.

Function regs takes Space and returns list of Region in the space.

Fucntion sides takes Space and returns list of Sidedness in the space, typically just two sides.

Function color takes Space, Region, and returns the Color of the region in the space. Polytopes are embedded in the space by giving the regions in the polytope the same color.

Function rename takes two different Space, list of Region from the first space, and returns list of Region on same sides of boundaries in the second Space. Only shared boundaries determine the result.

Function empty returns an empty Space. Empty spaces have one region and no boundaries.

Function order takes an Int and returns a 1-dimensional Space with the given number of boundaries.

Function system takes an Int and returns an n-dimensional Space with n boundaries. The space is "linear" so it has 2^n regions.

Function simplex takes an Int and returns an n-dimensional Space with n+1 boundaries. The space is "linear" so it has (2^(n+1))-1 regions.

Function subspace takes Space, list of Boundary, and returns Space of just the boundaries with sidednesses from the given space.

Function superspace takes list of Space, and returns Space that contains all the boundaries and the same sidednesses. For this to work, the subspaces of the shared boundaries must be the same.

Function spaces takes two Int, and returns list of Space. All spaces up to equivalence, of given dimension and number of boundaries, are listed.

Function overlaps takes Int and returns list of Space with regions of embeded overlap polytope colored. See http://www.sidegeo.blogspot.com/ for explanation of what simplex overlap is. All overlaps up to equivalence, of given dimension, are listed.

