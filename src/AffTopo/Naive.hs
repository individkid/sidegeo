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

cast :: Bool -> Int
cast False = 0
cast True = 1

uncast :: Int -> Bool
uncast 0 = False
uncast 1 = True

sortNub :: Ord a => [a] -> [a]
sortNub a = f (sort a)

sortNubF :: Ord a => [a] -> [a]
sortNubF (h0:(h1:t)) | h0 == h1 = h1:t
sortNubF a = a

member :: Eq a => a -> [a] -> Bool
member a b = (find a b) \= Nothing

insert :: Ord a => a -> [a] -> [a]
insert a b = sortNub (a:b)

choose :: RandomGen g => g -> [a] -> Maybe a
choose g [] = (Nothing, g)
choose g a = let (i,h) = Random.randomR (0,(length a)-1) g in (Just (a !! i), h)

remove :: Eq a => a -> [a] -> [a]
remove a b = filter (\c -> c \= a) b

replace :: Int -> a -> [a] -> [a]
replace a b c = (take a c) ++ (b : (drop (a+1) c))

holes :: Ord a => Int -> [a] -> [a]
holes n a = take n ([0..(length a)+n-1] \\ a)

image :: Ord a => Ord b => [a] -> [(a,b)] -> [b]
image a m = map (\(x,y) -> y) (filter (\(x,y) -> member x a) m)

inverse :: Ord a => Ord b => [b] -> [(a,b)] -> [a]
inverse b m = map (\(x,y) -> x) (filter (\(x,y) -> member y b) m)

domain :: Ord a => Ord b => [(a,b)] -> [a]
domain m = map (\(x,y) -> x) m

range :: Ord a => Ord b => [(a,b)] -> [b]
range m = map (\(x,y) -> y) m

-- ++ and \\ are from Data.List
(++) :: Ord a => [a] -> [a] -> [a]
a ++ b = unionF (sortNub a) (sortNub b)

unionF :: Ord a => [a] -> [a] -> [a]
unionF (a:s) (b:t)
 | a < b = a:(unionF s (b:t))
 | a == b = a:(unionF s t)
 | a > b = b:(unionF (a:s) t)

(\\) :: Ord a => [a] -> [a] -> [a]
a \\ b =differenceF (sortNub a) (sortNub b)

differenceF :: Ord a => [a] -> [a] -> [a]
differenceF (a:s) (b:t)
 | a < b = a:(differenceF s (b:t))
 | a == b = (differenceF s t)
 | a > b = (differenceF (a:s) t)

(+\) :: Ord a => [a] -> [a] -> [a]
a +\ b = intersectF (sortNub a) (sortNub b)

intersectF :: Ord a => [a] -> [a] -> [a]
intersectF (a:s) (b:t)
 | a < b = (intersectF s (b:t))
 | a == b = a:(intersectF s t)
 | a > b = (intersectF (a:s) t)

(\+) :: Ord a => [a] -> [a] -> [a]
a \+ b = symdiffF (sortNub a) (sortNub b)

symdiffF :: Ord a => [a] -> [a] -> [a]
symdiffF (a:s) (b:t)
 | a < b = a:(symdiffF s (b:t))
 | a == b = (symdiffF s t)
 | a > b = b:(symdiffF (a:s) t)

-- all sublists of given size
subsets :: Ord a => Int -> [a] -> [[a]]
subsets n a
 | n == 0 = [[]]
 | null a = []
 | otherwise = concat (map (\b -> map (\c -> c:b) (a \\ b)) (subsets (n - 1) a))

-- those indexed by list of indices
subset :: Ord a => [Int] -> [a] -> [a]
subset p a = sortNub (foldl (\b p -> (a !! p) : b) [] p)

-- all connected by given function to given start
generate :: Ord a => (a -> [a]) -> a -> [a]
generate f a = generateF f a [] []

generateF :: Ord a => (a -> [a]) -> a -> [a] -> [a] -> [a]
generateF f a todo done
 | ((length newTodo) == 0) = newDone
 | otherwise = generateF f (head newTodo) (tail newTodo) newDone where
 newTodo = ((f a) \\ (todo ++ done)) ++ todo
 newDone = [a] ++ done

catalyze :: (a -> (b,a)) -> a -> Int -> ([b], a)
catalyze f a n = foldl (\(b,c) _ -> let (d,e) = f c in (d:b,e)) ([],a) [0..n-1]

-- return all linear spaces of given dimension and boundaries.
allSpaces :: RandomGen g => g -> Int -> Int -> ([Space], g)
allSpaces g n m = let
 (p,h) = randomPlanes g n m
 in (allSpacesF 0 (minEquiv (spaceFromPlanes p [] [], h)

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
randomPlanes :: RandomGen g => g -> Int -> Int -> ([Plane], g)
-- TODO: tweak until all vertices are in -1.0 to 1.0 hypsquare on/in base
-- TODO: tweak coincidences found by trying all n+1 tuples
randomPlanes g n m = let
 (a,b) = catalyze (\g -> Random.randomR (-1.0,1.0) g) g (n*m)
 in (Matrix.toColumns (Matrix.matrix m a), b)

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
minEquiv s = minEquivWithPerms [] (sortSpace s) 0 (regionsOfSpace s) []

-- return sorted equivalent
sortSpace :: Space -> Space
sortSpace s = sort (map sort (map (map sort) s))

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
regionOfSides r s = (regionOfSidesF r s) !! 0

-- return whether region with given side map exists in space
regionOfSidesExists :: [Sidedness] -> Space -> Bool
regionOfSidesExists r s = (length (regionOfSidesF r s)) == 1

regionOfSidesF :: [Sidedness] -> Space -> [Region]
regionOfSidesF r s = foldl (\a (r,s) -> a +\ (s !! r)) (regionsOfSpace s) (zip r s)

-- return all boundaries in space
boundariesOfSpace :: Space -> [Boundary]
boundariesOfSpace s = [0..((length s)-1)]

-- return all regions in space
regionsOfSpace :: Space -> [Region]
regionsOfSpace s = sortNub (concat (concat s))

-- return boundaries attached to region
attachedBoundaries :: Region -> Space -> [Boundary]
attachedBoundaries r s = filter (\b -> oppositeOfRegionExists [b] r) (boundariesOfSpace s)

-- return facets attached to region
attachedFacets Region -> Space -> Dimension -> [[Boundary]]
attachedFacets r s d = filter (\b -> oppositeOfRegionExists b r) (subsets d (boundariesOfSpace s))

-- return regions in corners of boundaries
attachedRegions :: [Boundary] -> Space -> [Region]
attachedRegions b s = filter (\r -> oppositeOfRegionExists b r) (regionsOfSpace s)

-- return neighbor region of given region wrt given boundary
oppositeOfRegion :: [Boundary] -> Region -> Space -> Region
oppositeOfRegion b r s = regionOfSides (oppositeOfSides b (sidesOfRegion r s)) s

-- return whether neighbor region exists
oppositeOfRegionExists :: [Boundary] -> Region -> Space -> Bool
oppositeOfRegionExists b r s = regionOfSidesExists (oppositeOfSides b (sidesOfRegion r s)) s

-- return shell of regions around given region
neighborsOfRegion :: Region -> Space -> [Region]
neighborsOfRegion r s = map (\b -> oppositeOfRegion [b] r s) (attachedBoundaries r s)

-- return corresponding outside region
complementOfRegion :: Region -> Space -> Region
complementOfRegion r s = oppositeOfRegion (boundariesOfSpace s) r s

-- return whether the region is an outside region
complementOfRegionExists :: Region -> Space -> Bool
complementOfRegionExists r s = oppositeOfRegionExists (boundariesOfSpace s) r s

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
takeRegions :: [Boundary] -> Space -> [Boundary] -> Space -> [Region] -> [Region]
takeRegions p s q t r = let
 -- for each given region, find sides in first space
 firstSides :: [[Sidedness]]
 firstSides = map (\r -> sidesOfRegion r s) r
 -- for each boundary in second space, find corresponding boundary in first space or Nothing
 secondToFirst :: [Maybe Int]
 secondToFirst = map (\q -> elemIndex q p) q
 -- for each given region, for each boundary in second space, find sidedness or Nothing by composition
 secondSides :: [Maybe [Sidedness]]
 secondSides = map (\r -> map (\b -> fmap (\b -> r !! b) b) secondToFirst) firstSides
 -- for each boundary in second space, count number of Nothing
 count :: Int
 count = foldl (\a b -> a + (cast (b == Nothing))) 0 secondToFirst
 -- find sidedness permutations of same length as indices
 permutes :: [[Sidedness]]
 permutes = map (\a -> map (\b -> uncast ((a >> b) & 1)) [0..count-1]) [0..(1<<count)-1]
 -- for each given region, for each permutation, fix second sides of region, consuming head of permutation as needed
 fixedSides :: [[Sidedness]]
 fixedSides = map (\(a,b) -> takeRegionsF a b) [(a,b) | a -> secondSides, b -> permutes]
 -- map sides to regions
 in map (\r -> regionOfSides r t) fixedSides

takeRegionsF :: [Maybe Sidedness] -> [Sidednesss] -> [Sidedness]
takeRegionsF ((Just a):b) c = a:(takeRegionsF b c)
takeRegionsF (Nothing:b) (c:d) = c:(takeRegionsF b d)
takeRegionsF [] [] = []

-- return space with given regions divided by new boundary
divideSpace :: [Region] -> Space -> Space
divideSpace r s = let
 regions = regionsOfSpace s
 complement = regions \\ r
 halfspace = divideSpaceF complement 
 duplicates = holes (length r) regions
 mapping = zip r duplicates
 withDups = map (map (divideSpaceG mapping)) s
 in withDups ++ [(halfspace ++ duplicate),(regions \\ halfspace)]

divideSpaceF :: [Region] -> Space -> [Region]
divideSpaceF c s
 | (length c) > 0 = generate (\a -> c +\ (neighborsOfRegion a s)) (head c)
 | otherwise = []

divideSpaceG :: [(Region,Region)] -> [Region] -> [Region]
divideSpaceG m r = r ++ (image ((domain m) +\ r) m)

-- return planes with sidednesses as specified by given dimension and space
planesFromSpace :: Int -> Space -> [Plane]
planesFromSpace n s = undefined
-- recurse with one fewer boundary
-- find vertices, interpret as coplanes
-- add simplex containing all covertices
-- find inside coregion corresponding to regions divided by boundary to add
-- find average of corners of coregion, interpret as plane to add

-- return space of same dimension with given boundary removed
subSpace :: Boundary -> Space -> Space
subSpace m s = undefined

-- return space of one less dimension homeomorphic to regions attached to given boundary
sectionSpace :: Boundary -> Space -> Space
sectionSpace m s = undefined

-- return superspace with given spaces as subspaces
superSpace :: [Boundary] -> Space -> [Boundary] -> Space -> [Boundary] -> Space
superSpace p s q t r = undefined
