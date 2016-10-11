module AffTopo.Naive where

-- naive in the sense of just one representation

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric.LinearAlgebra.HMatrix (Matrix)
import qualified Numeric.LinearAlgebra.HMatrix as Matrix
import System.Random as Random

type Boundary = Int
type Region = Int
type Sidedness = Int -- index into FullSpace
type HalfSpace = Set Region
type FullSpace = Set HalfSpace -- unordered pair
type RegionMap = Map Boundary Sidedness
type Space = Map Boundary FullSpace
type Plane = Matrix Double
type Point = Matrix Double
type Planes = Matrix Double
type Coeffs = Matrix Double
type Index = Int
type Dimension = Int -- assume three
type Area = Int
type Volume = Int

-- planes or points are columns in matrix
-- coefficients of an equation are a row

setUnions :: Ord a => Set (Set a) -> Set a
setUnions a = Set.unions (Set.toList a)

powerSet :: Ord a => Set a -> Set (Set a)
powerSet a = Set.unions (map (\b -> subSets b a) [0 .. (Set.size a)])

subSets :: Ord a => Int -> Set a -> Set (Set a)
subSets n a
 | n == 0 = Set.signleton (Set.empty)
 | otherwise = setUnions (Set.map (\b -> Set.map (\c -> Set.insert c b) (Set.difference a b)) (subSets (n - 1) a))

-- return all linear spaces of given dimension and boundaries.
allSpaces :: Dimension -> Area -> Set Space
allSpaces n m =
 -- start with space of m random planes of dimension n
 f 0 (minEquivSpace (spaceFromPlanes (randomPlanes n m)) Set.empty Set.empty where
 -- migrate all possible from current space, and go on to next todo
 f :: Region -> Space -> Set Space -> Set Space -> Set Space
 f r s todo done
  | canMigrate r s = g (r+1) s (minEquivSpace (migrateSpace r s)) todo done
  | r < (Set.size (coverOfSpace s)) = f (r+1) s todo done
  | not (Set.null todo) = h todo (Set.insert s done)
  | otherwise = (Set.insert s done)
 -- if migration not already done or todo, recurse with migration added to todo
 g :: Region -> Space -> Space -> Set Space -> Set Space -> Set Space
 g r s t todo done
  | (s == t) || (Set.member t todo) || (Set.member t done) = f r s todo done
  | otherwise = f r s (Set.insert t todo) done
 -- recurse with choice removed from todo
 h :: Set Space -> Set Space -> Set Space
 h todo done = let s = Set.elemAt 0 todo in f 0 s (Set.delete s todo) done

-- return given number of planes in given number of dimensions
randomPlanes :: Dimension -> Area -> Planes
randomPlanes n m = map f (randoms)

intersectionOfPlanes :: Planes -> Point
intersectionOfPlanes w = undefined

isAbovePlane :: Point -> Plane -> Bool
isAbovePlane v w = undefined

-- return planes with sidednesses as specified by given dimension and space
planesFromSpace :: Dimension -> Space -> Planes
planesFromSpace n s = undefined
-- recurse with one fewer boundary
-- find vertices, interpret as coplanes
-- add simplex containing all covertices
-- find coregion corresponding to regions divided by boundary to add
-- find average of corners of coregion, interpret as plane to add

-- return space with sidednesses determined by given planes
spaceFromPlanes :: Planes -> Space
spaceFromPlanes p
 | numPlanes == 0 = Map.empty
 | otherwise = let
 -- recurse with one fewer plane
 headSpace :: Space
 headSpace = spaceFromPlanes headPlanes
 -- find (n-1)-tuples of recursed planes
 colTuples :: Set (Set Index)
 colTuples = subSets planeDims (Set.fromList headIdxs)
 -- find sub-regions of super-region containing indicated intersection point
 f :: Set Index -> Set Region
 f i = let
 tupl = Set.toList i
 cols = filter (\j -> elem j tupl) headIdxs
 subS = foldl (\s j -> subSpace j s) headSpace tupl
 subP = headPlanes Matrix.?? (Matrix.All, Matrix.Pos (Matrix.idxs cols))
 indP = headPlanes Matrix.?? (Matrix.All, Matrix.Pos (Matrix.idxs tupl))
 intP = intersectionOfPlanes (indP Matrix.||| tailPlane)
 in subRegions (Set.singleton (regionOfPoint point subP subS)) subS headSpace 
 -- find union of sub-regions of super-regions containing intersections
 headRegs :: Set Region
 headRegs = Set.unions (Set.toList (Set.map f colTuples))
 -- return space with found regions divided by new boundary
 in divideSpace headRegs headSpace where
 numPlanes = Matrix.cols p
 headPlanes = p Matrix.?? (Matrix.All, Matrix.DropLast 1)
 tailPlane = p Matrix.?? (Matrix.All, Matrix.Drop (numPlanes - 1))
 headIdxs = [0 .. (numPlanes - 2)]
 spaceDims = Matrix.rows p
 planeDims = spaceDims - 1

-- return region of point, assuming planes and space are homeomorphic
regionOfPoint :: Point -> Planes -> Space -> Region
retionOfPoint = undefined

-- return list of region permutations, given sorted halfspace lists, and args as described
minEquiv :: [[Halfspace]] -> [[Halfspace]] -> Region -> [Region] -> [Region] -> ([[Halfspace]], [[Region]])
-- p is incomplete space to add r to. q corresponds to p. s is regions in q.
-- r goes in same positions in p that some region from s is in q. t is regions in reverse order used.
minEquiv p _ _ [] t = (p, (reverse t))
minEquiv p q r s t = let
 -- find spaces with just one added
 added = map (\a -> f0 p q r a) s
 -- find position regions with just one removed
 removed = filter (\a -> f3 q a) s
 -- find min with just one added
 minAdded = minimum added
 -- list those with just one added that is min
 recurseArgs = filter (\(a,(b,c)) -> b == minAdded) (zip s (zip added removed))
 -- recurse on each that is minimum
 recursed = map (\(a,(b,c)) -> minEquiv b c (r+1) (delete a s) (a:t)) recurseArgs
 -- find minimum recursion
 result = minimum (fst (unzip recursed))
 -- find recursions with minimum
 results = filter (\(a,b) -> a == result) recursed
 -- concat permutations of minima
 in (result, concat (snd (unzip results))) where
 -- add r to p in positions of s in q
 f0 :: [[Halfspace]] -> [[Halfspace]] -> Region -> Region -> [[Halfspace]]
 f0 p q r s = sort (map (f1 r s) (zip p q))
 -- add r to p in position of s in q
 f1 Region -> Region -> ([Halfspace],[Halfspace]) -> [Halfspace]
 f1 r s (p,q) = sort (map (f2 r s) (zip p q))
 -- add r to p if s in q
 f2 Region -> Region -> (Halfspace,Halfspace) -> Halfspace
 f2 r s (p,q)
  | Set.member s q = Set.insert r p
  | otherwise = p
 -- remove s from q
 f3 :: [[Halfspace]] -> Region -> [[Halfspace]]
 f3 q s = map (filter (\a->a!=s)) q

-- call minEquiv with halspaces from given space
-- convert region lists to region maps
-- lfigure out each boundary map from its region map
minEquivSpaces :: Space -> Set (Space, Map Boundary Boundary, Map Region Region)
minEquivSpaces s = undefined

-- return space with regions permuted such that result is smallest possible
minEquivSpace :: Space -> Space
minEquivSpace s = undefined

-- return whether local opposite of given region is empty and all of its neighbor regions are non-empty
canMigrate :: Region -> Space -> Bool
canMigrate r s = undefined

-- return space with given region changed to its local opposite
migrateSpace :: Region -> Space -> Space
migrateSpace r s = undefined

-- return per boundary side of region
sidesFromRegion :: Region -> Space -> RegionMap
sidesFromRegion r s = undefined

-- return region from boundary to pair index map
regionFromSides :: RegionMap -> Space -> Region
regionFromSides r s = undefined

-- return whether region with given side map exists in space
regionWithSidesExists :: RegionMap -> Space -> Bool
regionWithSidesExists r s = undefined

-- return boundary to side map with given boundaries reversed
oppositeOfRegion :: RegionMap -> Set Boundary -> RegionMap
oppositeOfRegion r s = undefined

-- return boundaries that given region has neighbors wrt
localOfRegion :: Region -> Space -> Set Boundary
localOfRegion r s = undefined

-- return all boundaries in space
globalOfSpace :: Space -> Set Boundary
globalOfSpace s = undefined

-- return all regions in space
coverOfSpace :: Space -> Set Region
coverOfSpace s = undefined

-- return boundaries attached to region
attachedBoundaries :: Region -> Space -> Set Boundary
attachedBoundaries r s = undefined

-- return vertices attached to region
attachedVertices :: Region -> Space -> Set (Set Boundary)
attachedVertices r s = undefined

-- return whether all subspaces have correct number of regions
isLinear :: Space -> Bool
isLinear s = undefined

-- return how many regions a space of given dimension and boundaries has
defineLinear :: Dimension -> Area -> Volume
defineLinear n m
 | (n == 0) || (m == 0) = 1
 | otherwise = (defineLinear n (m-1)) + (defineLinear (n-1) (m-1))

-- return space of same dimension with given boundary removed
subSpace :: Boundary -> Space -> Space
subSpace m s = undefined

-- return space of one less dimension homeomorphic to regions attached to given boundary
sectionSpace :: Boundary -> Space -> Space
sectionSpace m s = undefined

-- return regions attached to indicated boundary
attachedRegions :: Boundary -> Space -> Set Region
attachedRegions m s = undefined

-- return neighbor region of given region wrt given boundary
neighborRegion :: Boundary -> Region -> Space -> Region
neighborRegion m r s = regionFromSides (oppositeOfRegion (sidesFromRegion r s) (Set.singleton m)) s

-- return whether neighbor region exists
neighborRegionExists :: Boundary -> Region -> Space -> Bool
neighborRegionExists m r s = undefined

-- return space with given regions divided by new boundary
divideSpace :: Set Region -> Space -> Space
divideSpace r s = undefined

-- assume given spaces are subspaces in a superspace
-- return regions in second space that contain any of the given regions in first space
superRegions :: Set Region -> Space -> Space -> Set Region
superRegions r s = undefined

-- assume given spaces are subspaces in a superspace
-- return regions in second space that are contained by any of the given regions in first space
subRegions :: Set Region -> Space -> Space -> Set Region
subRegions r s = undefined

-- return superspace with given spaces as subspaces
superSpace :: Space -> Space -> Space
superSpace r s = undefined
