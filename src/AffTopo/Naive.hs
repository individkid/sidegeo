module AffTopo.Naive where

-- naive in the sense of just one representation

import Data.list
import qualified Numeric.LinearAlgebra.Data as Matrix
import qualified System.Random as Random

type Boundary = Int -- index into Space
type Region = Int -- arbitrary identifier
type Sidedness = Int -- index into FullSpace
type HalfSpace = [Region] -- assume proper set
type FullSpace = [HalfSpace] -- assume disjoint covering pair
type Sidednesses = [Sidedness] -- represents region
type Space = [FullSpace] -- assume equal covers
type Plane = Matrix Double -- single column of distances above base
type Point = Matrix Double -- single column of coordinates
type Planes = Matrix Double -- multiple columns of parallel distances
type Coeffs = Matrix Double -- each row is a linear equation
type Index = Int -- row or column
type Dimensions = Int -- more than zero
type HypAreas = Int -- lengths/points in 2/1 dimensions
type HypVolumes = Int -- areas/lengths in 2/1 dimensions

sortNub :: Ord a => [a] -> [a]
sortNub a = f (sort a) where
 f :: Ord a => [a] -> [a]
 f (h0:(h1:t)) | h0 == h1 = h1:t
 f a = a

member :: Eq a => a -> [a] -> Bool
member a b = (find a b) \= Nothing

insert :: Eq a => a -> [a] -> [a]
insert a b = sortNub (a:b)

choose :: [a] -> a
choose (h:t) = h

delete :: Eq a => a -> [a] -> [a]
delete a b = filter (\c -> c \= a) b

subLists :: Ord a => Int -> [a] -> [[a]]
subLists n a
 | n == 0 = [[]]
 | otherwise = concat (map (\b -> map (\c -> c:b) (a \\ b)) (subLists (n - 1) a))

-- return all linear spaces of given dimension and boundaries.
allSpaces :: Dimensions -> HypAreas -> [Space]
allSpaces n m = allSpacesF 0 (minEquiv (spaceFromPlanes (randomPlanes n m)) [] []

-- migrate all possible from current space, and go on to next todo
allSpacesF :: Region -> Space -> [Space] -> [Space] -> [Space]
allSpacesF r s todo done
 | canMigrate r s = allSpacesG (r+1) s (minEquiv (migrateSpace r s)) todo done
 | r < (length (coverOfSpace s)) = allSpacesF (r+1) s todo done
 | not (null todo) = allSpacesH todo (insert s done)
 | otherwise = (insert s done)

-- if migration not already done or todo, recurse with migration added to todo
allSpacesG :: Region -> Space -> Space -> [Space] -> [Space] -> [Space]
allSpacesG r s t todo done
 | (s == t) || (member t todo) || (member t done) = allSpacesF r s todo done
 | otherwise = allSpacesF r s (t:todo) done

-- recurse with choice removed from todo
allSpacesH :: [Space] -> [Space] -> [Space]
allSpacesH (s:todo) done = f 0 s todo done

-- return given number of planes in given number of dimensions
randomPlanes :: Dimensions -> HypAreas -> Planes
randomPlanes n m = undefined

-- assume first rows are distances above points in base plane
-- assume last row is distances above origin
-- each column specifies points that a plane passes through
intersectPlanes :: Planes -> Maybe Point
intersectPlanes w = let
 dim = rows w
 square = Matrix.matrix dim [intersectPlanesF w dim a b | a <- [0..dim-1], b <- [0..dim-1]]
 rhs = Matrix.matrix 1 [intersectPlanesG w dim a | a <- [0..dim-1]]
 in Matrix.linearSolve square rhs

intersectPlanesF :: Planes -> Int -> Int -> Int -> Double
intersectPlanesF w d a b
 | b == d = -1.0
 | otherwise = (Matrix.atIndex w (b,a)) - (Matrix.atIndex w (d,a))

intersectPlanesG :: Planes -> Int -> Int -> Double
intersectPlanesG w d a = negate (Matrix.atIndex w (d,a))

isAbovePlane :: Point -> Plane -> Bool
isAbovePlane v w = let
 dim = rows w
 planeV = Matrix.flatten (w Matrix.?? (Matrix.DropLast 1, Matrix.All))
 pointV = Matrix.flatten (v Matrix.?? (Matrix.DropLast 1, Matrix.All))
 planeS = Matrix.atIndex w (dim,0)
 pointS = Matrix.atIndex v (dim,0)
 in pointS > ((dot planeV pointV) + planeS)

-- return planes with sidednesses as specified by given dimension and space
planesFromSpace :: Dimensions -> Space -> Planes
planesFromSpace n s = undefined
-- recurse with one fewer boundary
-- find vertices, interpret as coplanes
-- add simplex containing all covertices
-- find coregion corresponding to regions divided by boundary to add
-- find average of corners of coregion, interpret as plane to add

-- return space with sidednesses determined by given planes
spaceFromPlanes :: Planes -> Space
spaceFromPlanes p
 | numPlanes == 0 = []
 | otherwise = let
 -- recurse with one fewer plane
 headSpace :: Space
 headSpace = spaceFromPlanes headPlanes
 -- find (n-1)-tuples of recursed planes
 colTuples :: [[Index]]
 colTuples = subLists planeDims headIdxs
 -- find union of sub-regions of super-regions containing intersections
 headRegs :: [Region]
 headRegs = sortNub (concat (map (spaceFromPlanesF headSpace headPlanes tailPlane headIdxs) colTuples))
 -- return space with found regions divided by new boundary
 in divideSpace headRegs headSpace where
 numPlanes = Matrix.cols p
 headPlanes = p Matrix.?? (Matrix.All, Matrix.DropLast 1)
 tailPlane = p Matrix.?? (Matrix.All, Matrix.Drop (numPlanes - 1))
 headIdxs = [0 .. (numPlanes - 2)]
 spaceDims = Matrix.rows p
 planeDims = spaceDims - 1

 -- find sub-regions of super-region containing indicated intersection point
spaceFromPlanesF :: Space -> Matrix -> Matrix -> [Index] -> [Index] -> [Region]
spaceFromPlanesF headSpace headPlanes tailPlane headIdxs tupl = let
 cols = filter (\j -> elem j tupl) headIdxs
 subS = foldl (\s j -> subSpace j s) headSpace tupl
 subP = headPlanes Matrix.?? (Matrix.All, Matrix.Pos (Matrix.idxs cols))
 indP = headPlanes Matrix.?? (Matrix.All, Matrix.Pos (Matrix.idxs tupl))
 intP = intersectPlanes (indP Matrix.||| tailPlane)
 in sort (subRegions [regionOfPoint point subP subS] subS headSpace)

-- return region of point, assuming planes and space are homeomorphic
regionOfPoint :: Point -> Planes -> Space -> Region
retionOfPoint = undefined

-- return list of region permutations, given sorted halfspace lists, and args as described
minEquivWithPerms :: Space -> Space -> Region -> [Region] -> [Region] -> (Space, [[Region]])
-- p is incomplete space to add r to. q corresponds to p. s is regions in q.
-- r goes in same positions in p that some region from s is in q. t is regions in reverse order used.
minEquivWithPerms p _ _ [] t = (p, [reverse t])
minEquivWithPerms p q r s t = let
 -- find spaces with just one added
 added :: [Space]
 added = map (\a -> minEquivWithPermsF0 p q r a) s
 -- find position regions with just one removed
 removed :: [Space]
 removed = map (\a -> minEquivWithPermsF3 q a) s
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
 in (result, concat (snd (unzip results)))

-- add r to p in positions of s in q
minEquivWithPermsF0 :: Space -> Space -> Region -> Region -> Space
minEquivWithPermsF0 p q r s = sort (map (minEquivWithPermsF1 r s) (zip p q))

-- add r to p in position of s in q
minEquivWithPermsF1 Region -> Region -> ([Halfspace],[Halfspace]) -> [Halfspace]
minEquivWithPermsF1 r s (p,q) = sort (map (minEquivWithPermsF2 r s) (zip p q))

-- add r to p if s in q
minEquivWithPermsF2 Region -> Region -> (Halfspace,Halfspace) -> Halfspace
minEquivWithPermsF2 r s (p,q)
 | member s q = insert r p
 | otherwise = p

-- remove s from q
minEquivWithPermsF3 :: Space -> Region -> Space
minEquivWithPermsF3 q s = map (filter (\a -> a !=s )) q

-- return space with regions permuted such that result is smallest possible
minEquiv :: Space -> Space
minEquiv s = undefined

-- return whether local opposite of given region is empty and all of its neighbor regions are non-empty
canMigrate :: Region -> Space -> Bool
canMigrate r s = undefined

-- return space with given region changed to its local opposite
migrateSpace :: Region -> Space -> Space
migrateSpace r s = undefined

-- return per boundary side of region
sidesOfRegion :: Region -> Space -> Sidednesses
sidesOfRegion r s = undefined

-- return region from boundary to pair index map
regionOfSides :: Sidednesses -> Space -> Region
regionOfSides r s = undefined

-- return whether region with given side map exists in space
regionOfSidesExists :: Sidednesses -> Space -> Bool
regionOfSidesExists r s = undefined

-- return boundary to side map with given boundaries reversed
oppositeOfRegion :: Sidednesses -> [Boundary] -> Sidednesses
oppositeOfRegion r s = undefined

-- return boundaries that given region has neighbors wrt
localOfRegion :: Region -> Space -> [Boundary]
localOfRegion r s = undefined

-- return all boundaries in space
globalOfSpace :: Space -> [Boundary]
globalOfSpace s = undefined

-- return all regions in space
coverOfSpace :: Space -> [Region]
coverOfSpace s = undefined

-- return boundaries attached to region
attachedBoundaries :: Region -> Space -> [Boundary]
attachedBoundaries r s = undefined

-- return vertices attached to region
attachedVertices :: Region -> Space -> [[Boundary]]
attachedVertices r s = undefined

-- return whether all subspaces have correct number of regions
isLinear :: Space -> Bool
isLinear s = undefined

-- return how many regions a space of given dimension and boundaries has
defineLinear :: Dimensions -> HypAreas -> HypVolumes
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
attachedRegions :: Boundary -> Space -> [Region]
attachedRegions m s = undefined

-- return neighbor region of given region wrt given boundary
neighborRegion :: Boundary -> Region -> Space -> Region
neighborRegion m r s = regionOfSides (oppositeOfRegion (sidesOfRegion r s) [m]) s

-- return whether neighbor region exists
neighborRegionExists :: Boundary -> Region -> Space -> Bool
neighborRegionExists m r s = undefined

-- return space with given regions divided by new boundary
divideSpace :: [Region] -> Space -> Space
divideSpace r s = undefined

-- assume given spaces are subspaces in a superspace
-- return regions in second space that contain any of the given regions in first space
superRegions :: [Region] -> Space -> Space -> [Region]
superRegions r s = undefined

-- assume given spaces are subspaces in a superspace
-- return regions in second space that are contained by any of the given regions in first space
subRegions :: [Region] -> Space -> Space -> [Region]
subRegions r s = undefined

-- return superspace with given spaces as subspaces
superSpace :: Space -> Space -> Space
superSpace r s = undefined
