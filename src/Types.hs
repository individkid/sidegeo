module Types where

class (Ord a, Enum a) => Holes a where
 zero :: a

-- undefined
data Boundary = Boundary Int deriving (Show, Eq, Ord) -- b
data Region = Region Int deriving (Show, Eq, Ord) -- r
data Sidedness = Sidedness Int deriving (Show, Eq, Ord) -- s
data Vertex = Vertex Int deriving (Show, Eq, Ord) -- v
data Color = Color Int deriving (Show, Eq, Ord) -- c

instance Enum Boundary where
 succ (Boundary x) = Boundary (succ x); pred (Boundary x) = Boundary (pred x)
 toEnum x = Boundary x; fromEnum (Boundary x) = x
instance Enum Region where
 succ (Region x) = Region (succ x); pred (Region x) = Region (pred x)
 toEnum x = Region x; fromEnum (Region x) = x
instance Enum Sidedness where
 succ (Sidedness x) = Sidedness (succ x); pred (Sidedness x) = Sidedness (pred x)
 toEnum x = Sidedness x; fromEnum (Sidedness x) = x
instance Enum Vertex where
 succ (Vertex x) = Vertex (succ x); pred (Vertex x) = Vertex (pred x)
 toEnum x = Vertex x; fromEnum (Vertex x) = x
instance Enum Color where
 succ (Color x) = Color (succ x); pred (Color x) = Color (pred x)
 toEnum x = Color x; fromEnum (Color x) = x

instance Holes Boundary where zero = Boundary 0
instance Holes Region where zero = Region 0
instance Holes Sidedness where zero = Sidedness 0
instance Holes Vertex where zero = Vertex 0
instance Holes Color where zero = Color 0

-- collections of undefined
type Boundaries = Set Boundary
type Regions = Set Region
type Sidednesses = Set Sidedness
type Vertices = Set Vertex
type Colors = Set Color
type Direction = Map Boundary Sidedness
type Directions = Set Direction
type Rainbow = Map Color Directions
type Polytope = Set Rainbow
type Reflection = Map Sidedness Sidedness
type Symmetry = Map Boundary (Boundary, Reflection)
type Symmetries = Set Symmetry

-- representations
type Side = Map Boundary (Map Region Sidedness) -- st
type Dual = Map Sidedness (Map Region Boundaries) -- dt
type Duali = Map Sidedness (Map Boundaries Region) -- di
type Duals = Directions -- ds
type Half = Map Sidedness (Map Boundary Regions) -- ht
type Halfi = Map Sidedness (Map Regions Boundary) -- hi
type Bounds = Boundaries -- bs
type Regs = Regions -- rs
type Sides = Sidednesses -- ss
type Neighbor = Map Boundary (Map Region Region) -- nt
type Attached = Map Sidedness (Map Boundary Regions) -- at
type Flat = Map Boundary Regions -- am
type Shell = Map Sidedness (Map Region Boundaries) -- bt
type Cage = Map Region Boundaries -- bm
type Disk = Map Sidedness (Map Region Regions) -- rt
type Blot = Map Region Regions -- rm
type Vert = Map Boundaries Vertex -- vm
type Verti = Map Vertex Boundaries -- vi
type Verts = Vertices -- vs
type Pencil = Map Vertex Regions -- pm
type Corner = Map Region Vertices -- qm
type Inside = Regions -- is
type Outside = Regions -- os
type Signs = Vertices -- gs
type Signb = Map Vertex Boundaries -- gb
type Signr = Map Vertex Rainbow -- gr
type Tope = Map Region Color -- tm
type Topei = Map Color Regions -- ti
type Topes = Colors -- ts
type Topez = Polytope -- tz
type Plane = Map Boundary Vector -- cb
type Coplane = Map Vertex Vector -- cv
type Basis = Map Int (Map Int Vector) -- ci

type Space = Map Tag Rep

