module AffTopo.Naive where

-- naive in the sense of just one representation

import Data.list
import qualified Numeric.LinearAlgebra as Matrix
import qualified System.Random as Random

type Boundary = Int -- index into Space
type Region = Int -- arbitrary identifier
type Sidedness = Bool -- index into FullSpace
type HalfSpace = [Region] -- assume proper set
type FullSpace = [HalfSpace] -- assume disjoint covering pair
type Space = [FullSpace] -- assume equal covers
type Plane = Matrix.Vector Double -- single column of distances above base
type Point = Matrix.Vector Double -- single column of coordinates

sortNub :: Ord a => [a] -> [a]
sortNub a = f (sort a)

sortNubF :: Ord a => [a] -> [a]
sortNubF (h0:(h1:t)) | h0 == h1 = h1:t
sortNubF a = a

member :: Eq a => a -> [a] -> Bool
member a b = (find a b) \= Nothing

insert :: Ord a => a -> [a] -> [a]
insert a b = sortNub (a:b)

choose :: [a] -> a
choose [] = error "cannot choose from null"
choose (h:t) = h

remove :: Eq a => a -> [a] -> [a]
remove a b = filter (\c -> c \= a) b

replace :: Int -> a -> [a] -> [a]
replace a b c = (take a c) ++ (b : (drop (a+1) c))

-- TODO: change to infix like ++ and \\
intersect :: Ord a => [a] -> [a] -> [a]
intersect a b = intersectF (sortNub a) (sortNub b)

intersectF :: Ord a => [a] -> [a] -> [a]
intersectF (a:s) (b:t)
 | a < b = a:(intersectF s (b:t))
 | a > b = b:(intersectF (a:s) t)
 | a == b = a:(intersectF s t)

subsets :: Ord a => Int -> [a] -> [[a]]
subsets n a
 | n == 0 = [[]]
 | null a = []
 | otherwise = concat (map (\b -> map (\c -> c:b) (a \\ b)) (subsets (n - 1) a))

subset :: Ord a => [Int] -> [a] -> [a]
subset p a = undefined

generate :: Ord a => (a -> [a]) -> a -> [a]
generate f a = undefined

-- return all linear spaces of given dimension and boundaries.
allSpaces :: RandomGen g => g -> Int -> Int -> [Space]
allSpaces g n m = allSpacesF 0 (minEquiv (spaceFromPlanes (randomPlanes g n m)) [] []

-- migrate all possible from current space, and go on to next todo
allSpacesF :: Region -> Space -> [Space] -> [Space] -> [Space]
allSpacesF r s todo done
 | canMigrate r s = allSpacesG (r+1) s (minEquiv (migrateSpace r s)) todo done
 | r < (length (regionsOfSpace s)) = allSpacesF (r+1) s todo done
 | not (null todo) = allSpacesH todo (insert s done)
 | otherwise = (insert s done)

-- if migration not already done or todo, recurse with migration added to todo
allSpacesG :: Region -> Space -> Space -> [Space] -> [Space] -> [Space]
allSpacesG r s t todo done
 | (s == t) || (member t todo) || (member t done) = allSpacesF r s todo done
 | otherwise = allSpacesF r s (t:todo) done

-- recurse with choice removed from todo
allSpacesH :: [Space] -> [Space] -> [Space]
allSpacesH (s:todo) done = allSpacesF 0 s todo done

-- return given number of planes in given number of dimensions
randomPlanes :: RandomGen g => g -> Int -> Int -> [Plane]
-- TODO: tweak until all vertices are in -1.0 to 1.0 hypsquare on/in base
-- TODO: tweak coincidences found by trying all n+1 tuples
randomPlanes g n m = take m [(take n (Random.randomRs (0.0,1.0) g))..]

-- assume first rows are distances above points in base plane
-- assume last row is distances above origin
-- each column specifies points that a plane passes through
intersectPlanes :: [Plane] -> Maybe Point
intersectPlanes w = let
 dim = rows w
 square = Matrix.matrix dim [intersectPlanesF w dim a b | a <- [0..dim-1], b <- [0..dim-1]]
 rhs = Matrix.matrix 1 [intersectPlanesG w dim a | a <- [0..dim-1]]
 -- TODO: return Nothing if not every dim-tuple solves to same point
 in Matrix.toColumns (Matrix.linearSolve square rhs)

intersectPlanesF :: [Plane] -> Int -> Int -> Int -> Double
intersectPlanesF w d a b
 | b == d = -1.0
 | otherwise = (Matrix.atIndex (w !! a) b) - (Matrix.atIndex (w !! a) d)

intersectPlanesG :: [Plane] -> Int -> Int -> Double
intersectPlanesG w d a = negate (Matrix.atIndex (w !! a) d)

isAbovePlane :: Point -> Plane -> Bool
isAbovePlane v w = let
 dim = rows w
 planeV = Matrix.subVector 0 (dim-1) w
 pointV = Matrix.subVector 0 (dim-1) v
 planeS = Matrix.atIndex w (dim-1)
 pointS = Matrix.atIndex v (dim-1)
 in pointS > ((Matrix.dot planeV pointV) + planeS)

-- return space with sidednesses determined by given planes
spaceFromPlanes :: [Plane] -> Space
spaceFromPlanes p
 | numPlanes == 0 = []
 | otherwise = let
 -- recurse with one fewer plane
 headSpace :: Space
 headSpace = spaceFromPlanes headPlanes
 -- find (n-1)-tuples of recursed planes
 colTuples :: [[Int]]
 colTuples = subsets planeDims headIdxs
 -- find union of sub-regions of super-regions containing intersections
 headRegs :: [Region]
 headRegs = sortNub (concat (map (spaceFromPlanesF headSpace headPlanes tailPlane headIdxs) colTuples))
 -- return space with found regions divided by new boundary
 in divideSpace headRegs headSpace where
 numPlanes = length p
 headPlanes = p take (numPlanes - 1)
 tailPlane = p drop (numPlanes - 1)
 headIdxs = [0 .. (numPlanes - 2)]
 spaceDims = size tailPlane
 planeDims = spaceDims - 1

 -- find sub-regions of super-region containing indicated intersection point
spaceFromPlanesF :: Space -> [Plane] -> Plane -> [Int] -> [Int] -> [Region]
spaceFromPlanesF headSpace headPlanes tailPlane headIdxs tupl = let
 cols = filter (\j -> elem j tupl) headIdxs
 subS = foldl (\s j -> subSpace j s) headSpace tupl
 subP = subset cols headPlanes
 indP = subset tupl headPlanes
 intP = intersectPlanes (indP ++ tailPlane)
 in sort (takeRegions [regionOfPoint point subP subS] subS headSpace)

-- return region of point, assuming planes and space are homeomorphic
regionOfPoint :: Point -> [Plane] -> Space -> Region
regionOfPoint v w s = let
 dim = Matrix.size v
 num = length w
 -- for each boundary/plane, choose n others to find vertex
 vertices :: [Point]
 vertices = map (regionOfPointF w) [0..num-1]
 -- find sides of vertex wrt boundaries/planes
 sidesV :: [Bool]
 sidesV = map (\(v,w) -> isAbovePlane v w) (zip vertices p)
 -- find sides of point wrt planes
 sidesP :: [Bool]
 sidesP = map (\w -> isAbovePlane v w) w
 -- use transitivity to complete sides of point wrt boundaries
 sidesR :: [Sidedness]
 sidesR = map (\(a,b) -> a == b) (zip sidesV sidesP)
 -- return regionOfSides
 in regionOfSides sidesR s

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
 recursed = map (\(a,(b,c)) -> minEquivWithPerms b c (r+1) (remove a s) (a:t)) recurseArgs
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
minEquiv s = minEquivWithPerms [] s 0 (regionsOfSpace s) []

-- return whether local opposite of given region is empty and all of its oppositeOf regions are non-empty
canMigrate :: Region -> Space -> Bool
canMigrate r s = let
 boundaries = attachedBoundaries r s
 sides = sidesOfRegion r s
 opposite = oppositeOfSides boundaries sides
 empty = not (regionOfSidesExists opposite s)
 neighbors = map (\a -> oppositeOfSides [a] opposite) boundaries
 exists = map (\a -> regionOfSidesExists a s) neighbors
 in foldl (\a b -> a && b) empty exists

-- return space with given region changed to its local opposite
migrateSpace :: Region -> Space -> Space
migrateSpace r s = map (migrateSpaceF (attachedBoundaries r s) r) (zip [0..(length s)-1] s)

migrateSpaceF :: [Boundary] -> Region -> (Boundary,Fullspace) -> FullSpace
migrateSpaceF b r (a,s)
 | (member a b) && (member r (s !! 0)) = [(remove r (s !! 0)),(insert r (s !! 1))]
 | (member a b) = [(insert r (s !! 0)),(remove r (s !! 1))]
 | otherwise = s

-- return per boundary side of region
sidesOfRegion :: Region -> Space -> [Sidedness]
sidesOfRegion r s = map (\a -> member r (a !! 1)) s

-- return region from boundary to pair Int map
regionOfSides :: [Sidedness] -> Space -> Region
regionOfSides r s = choose (regionOfSidesF r s)

-- return whether region with given side map exists in space
regionOfSidesExists :: [Sidedness] -> Space -> Bool
regionOfSidesExists r s = (length (regionOfSidesF r s)) == 1

regionOfSidesF :: [Sidedness] -> Space -> [Region]
regionOfSidesF r s = foldl (\a (r,s) -> intersect a (s !! r)) (regionsOfSpace s) (zip r s)

-- return all boundaries in space
boundariesOfSpace :: Space -> [Boundary]
boundariesOfSpace s = [0..((length s)-1)]

-- return all regions in space
regionsOfSpace :: Space -> [Region]
regionsOfSpace s = sortNub (concat (concat s))

-- return boundaries attached to region
attachedBoundaries :: Region -> Space -> [Boundary]
attachedBoundaries r s = filter (attachedBoundariesF (sidesOfRegion r s) s) (boundariesOfSpace s)

attachedBoundariesF :: [Sidedness] -> Boundary -> Bool
attachedBoundariesF r s b = regionOfSideExists (oppositeOfSides [b] r) s

-- return vertices attached to region
attachedVertices :: Region -> Space -> Dimension -> [[Boundary]]
attachedVertices r s d = filter (attachedVerticesF (sidesOfRegion r s) s) (subsets d (boundariesOfSpace s))

attachedVerticesF :: [Sidedness] -> [Boundary] -> Bool
attachedVerticesF r s b = regionOfSideExists (oppositeOfSides b r) s

-- return regions in corners of boundaries
attachedRegions :: [Boundary] -> Space -> [Region]
attachedRegions b s = filter (attachedRegionsF b s) (regionsOfSpace s)

attachedRegionsF :: [Boundary] -> Space -> Region -> Bool
attachedRegionsF b s r = regionOfSidesExists (oppositeOfSides b (sidesOfRegion r s)) s

-- return neighbor region of given region wrt given boundary
oppositeOfRegion :: Boundary -> Region -> Space -> Region
oppositeOfRegion m r s = regionOfSides (oppositeOfSides [m] (sidesOfRegion r s)) s

-- return whether neighbor region exists
oppositeOfRegionExists :: Boundary -> Region -> Space -> Bool
oppositeOfRegionExists m r s = regionOfSidesExists (oppositeOfSides [m] (sidesOfRegion r s)) s

-- return sidedness with boundaries reversed
oppositeOfSides :: [Boundary] -> [Sidedness] -> [Sidedness]
oppositeOfSides b r = foldl (\r b -> replace b (not (r !! b)) r) r b

-- return how many regions a space of given dimension and boundaries has
defineLinear :: Int -> Int -> Int
defineLinear n m
 | (n == 0) || (m == 0) = 1
 | otherwise = (defineLinear n (m-1)) + (defineLinear (n-1) (m-1))

-- return whether all subspaces have correct number of regions
isLinear :: Int -> Space -> Bool
isLinear n s = let
 boundaries = [0..(length s)-1]
 sizes = boundaries
 subsets = foldl (\a b -> a ++ (subsets b boundaries)) [] sizes
 in foldl (\a b -> a && (isLinearF n s b)) True subsets

isLinearF :: Int -> Space -> [Boundary] -> Bool
isLinearF d s b = let
 subset = sortNub b
 fixed = map (\(a,b) -> b - a) (zip [0..(length subset)-1] subset)
 subspace = foldl (\a b -> subSpace b a) s fixed
 regions = regionsOfSpace subspace
 boundaries = boundariesOfSpace subspace
 in (defineLinear d (length boundaries)) == (length regions)

-- assume given spaces are subspaces in a superspace
-- return regions in second space that overlap any of the given regions in the first space
takeRegions :: [Region] -> Space -> Space -> [(Int,Int)] -> [Region]
takeRegions r s t p = undefined
 -- find boundaries in first space that are not in second
 -- find boundaries in second space thate are not in first
 -- find permutation for boundaries in both spaces
 -- for each given region, find sidednesses, remove sidednesses for boundaries not in second space
 -- permute sidednesses for boundaries in both spaces
 -- add each permutation of sidednesses for boundaries not in first space, and add region to result

-- return planes with sidednesses as specified by given dimension and space
planesFromSpace :: Int -> Space -> [Plane]
planesFromSpace n s = undefined
-- recurse with one fewer boundary
-- find vertices, interpret as coplanes
-- add simplex containing all covertices
-- find inside coregion corresponding to regions divided by boundary to add
-- find average of corners of coregion, interpret as plane to add

-- return space with given regions divided by new boundary
divideSpace :: [Region] -> Space -> Space
divideSpace r s = undefined

-- return space of same dimension with given boundary removed
subSpace :: Boundary -> Space -> Space
subSpace m s = undefined

-- return space of one less dimension homeomorphic to regions attached to given boundary
sectionSpace :: Boundary -> Space -> Space
sectionSpace m s = undefined

-- return superspace with given spaces as subspaces
superSpace :: Space -> Space -> [(Int,Int)] -> Space
superSpace s t p = undefined
