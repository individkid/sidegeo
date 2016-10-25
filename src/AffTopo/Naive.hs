
module AffTopo.Naive where

-- naive in the sense of just one representation

import Prelude hiding ((++))
import Data.List hiding ((\\), (++), insert)
import Data.Maybe
import Data.Bits
import qualified Numeric.LinearAlgebra as Matrix
import qualified System.Random as Random

type Boundary = Int -- index into Space
type Region = Int -- arbitrary identifier
type Sidedness = Bool -- index into FullSpace
type HalfSpace = [Region] -- assume proper set
type FullSpace = [HalfSpace] -- assume disjoint covering pair
type Space = [FullSpace] -- assume equal covers
type SubSpace = [(Boundary,FullSpace)]
type Plane = Matrix.Vector Double -- single column of distances above base
type Point = Matrix.Vector Double -- single column of coordinates

boolToSidedness :: Bool -> Sidedness
boolToSidedness a = a

sidednessToBool :: Sidedness -> Bool
sidednessToBool a = a

sidednessToInt :: Sidedness -> Int
sidednessToInt a = if a then 1 else 0

intToSidedness :: Int -> Sidedness
intToSidedness a = a == 1

boundaryToInt :: Boundary -> Int
boundaryToInt a = a

intToBoundary :: Int -> Boundary
intToBoundary a = a

regionToInt :: Region -> Int
regionToInt a = a

intToRegion :: Int -> Region
intToRegion a = a

-- all sublists of given size
subsets :: Ord a => Int -> [a] -> [[a]]
subsets n a
 | n == 0 = [[]]
 | null a = []
 | otherwise = concat (map (\b -> map (\c -> c:b) (a \\ b)) (subsets (n - 1) a))

-- those indexed by list of indices
subset :: Ord a => [Int] -> [a] -> [a]
subset p a = sortNub (foldl (\b p -> (a !! p) : b) [] p)

sortNub :: Ord a => [a] -> [a]
sortNub a = sortNubF (sort a)

sortNubF :: Ord a => [a] -> [a]
sortNubF (h0:(h1:t)) | h0 == h1 = h1:t
sortNubF a = a

member :: Eq a => a -> [a] -> Bool
member a b = (find (\b -> a == b) b) /= Nothing

insert :: Ord a => a -> [a] -> [a]
insert a b = sortNub (a:b)

remove :: Eq a => a -> [a] -> [a]
remove a b = filter (\c -> c /= a) b

replace :: Ord a => Int -> a -> [a] -> [a]
replace a b c = (take a c) ++ (b : (drop (a+1) c))

choose :: Random.RandomGen g => g -> [a] -> Maybe (a, g)
choose g [] = Nothing
choose g a = let (b,h) = Random.randomR (0,(length a)-1) g in Just ((a !! b), h)

holes :: Ord a => Num a => Int -> [a] -> [a]
holes n a = take n ((indices ((length a)+n)) \\ a)

indices :: Num a => Int -> [a]
indices n = take n (iterate (\a -> a + 1) 0)

enumerate :: [a] -> [(Int,a)]
enumerate a = zip (indices (length a)) a

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
a \+ b = symmetricF (sortNub a) (sortNub b)

symmetricF :: Ord a => [a] -> [a] -> [a]
symmetricF (a:s) (b:t)
 | a < b = a:(symmetricF s (b:t))
 | a == b = (symmetricF s t)
 | a > b = b:(symmetricF (a:s) t)

-- all connected by given function to given start
generate :: Ord a => (a -> [a]) -> a -> [a]
generate f a = generateF f a [] []

generateF :: Ord a => (a -> [a]) -> a -> [a] -> [a] -> [a]
generateF f a todo done
 | ((length newTodo) == 0) = newDone
 | otherwise = generateF f (head newTodo) (tail newTodo) newDone where
 newTodo = ((f a) \\ (todo ++ done)) ++ todo
 newDone = [a] ++ done

-- given number of firsts found by calling function on second
catalyze :: (g -> (a,g)) -> g -> Int -> ([a],g)
catalyze f g n = foldl (\(a,h) _ -> let (b,i) = f h in (b:a,i)) ([],g) (indices n)

-- call function for new result until it returns Nothing
tweak :: [(a -> Maybe a)] -> a -> Maybe a
tweak (f:g) a = tweakF g Nothing (f a)
tweak [] _ = Nothing

tweakF :: [(a -> Maybe a)] -> Maybe a -> Maybe a -> Maybe a
tweakF (f:g) _ (Just b) = tweakF g (Just b) (f b)
tweakF [] _ (Just b) = Just b
tweakF _ a Nothing = a

-- return all linear spaces of given dimension and boundaries.
allSpaces :: Random.RandomGen g => g -> Int -> Int -> ([Space], g)
allSpaces g n m = let
 (p,h) = randomPlanes g n m
 in (allSpacesF 0 (minEquiv (spaceFromPlanes n p)) [] [], h)

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
randomPlanes :: Random.RandomGen g => g -> Int -> Int -> ([Plane], g)
randomPlanes g n m = let
 (a,h) = catalyze (\g -> Random.randomR (-100.0,100.0) g) g (n*m)
 b = Matrix.toColumns (Matrix.matrix m a)
 tweaks = [randomPlanesF n m, randomPlanesG n m, randomPlanesH n m]
 in fromMaybe (b,h) (tweak (repeat (tweak tweaks)) (b,h))

-- shift by half to origin some plane from some n tuple that intersects outside -1.0 to 1.0 hypercube
randomPlanesF :: Random.RandomGen g => Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesF n m (a,g) = fmap (randomPlanesF0 g n a) (find (randomPlanesF1 n a) (subsets n (indices m)))

randomPlanesF0 :: Random.RandomGen g => g -> Int -> [Plane] -> [Int] -> ([Plane], g)
randomPlanesF0 g n a b = let
 (c,h) = fromJust (choose g b)
 d = Matrix.toList (a !! c)
 e = (d !! (n-1)) / 2.0
 in (replace c (Matrix.fromList (map (\x -> x - e) d)) a, h)

randomPlanesF1 :: Int -> [Plane] -> [Int] -> Bool
randomPlanesF1 n a b = let
 e = intersectPlanes n (subset b a)
 in maybe False (\c -> all (\d -> d > -1.0 && d < 1.0) (Matrix.toList c)) e

-- rerandomize some plane from some n tuple that does not intersect in Just
randomPlanesG :: Random.RandomGen g => Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesG n m (a,g) = fmap (randomPlanesG0 g n a) (find (randomPlanesG1 n a) (subsets n (indices m)))

randomPlanesG0 :: Random.RandomGen g => g -> Int -> [Plane] -> [Int] -> ([Plane], g)
randomPlanesG0 g n a b = let
 (c,h) = fromJust (choose g b)
 (d,i) = catalyze (\g -> Random.randomR (-100.0,100.0) g) h n
 in (replace c (Matrix.fromList d) a, i)

randomPlanesG1 :: Int -> [Plane] -> [Int] -> Bool
randomPlanesG1 n a b = let
 e = intersectPlanes n (subset b a)
 in e == Nothing

-- rerandomize some plane from some n+1 tuple that does intersect to Just
randomPlanesH :: Random.RandomGen g => Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesH n m (a,g) = fmap (randomPlanesH0 g n a) (find (randomPlanesH1 n a) (subsets (n+1) (indices m)))

randomPlanesH0 :: Random.RandomGen g => g -> Int -> [Plane] -> [Int] -> ([Plane], g)
randomPlanesH0 = randomPlanesG0

randomPlanesH1 :: Int -> [Plane] -> [Int] -> Bool
randomPlanesH1 n a b = let
 e = intersectPlanes n (subset b a)
 in e /= Nothing

-- assume first rows are distances above points in base plane
-- assume last row is distances above origin
-- each column specifies points that a plane passes through
intersectPlanes :: Int -> [Plane] -> Maybe Point
intersectPlanes n w = let
 first =  intersectPlanesH n w
 -- return Nothing if not every n-tuple solves to same point
 points = map (\a -> intersectPlanesH n (subset a w)) (subsets n (indices (length w)))
 same = maybe False (\c -> all (\a -> maybe False (\b -> (Matrix.dot c b) < 0.001) a) points) first
 in if same then first else Nothing

intersectPlanesF :: [Plane] -> Int -> Int -> Int -> Double
intersectPlanesF w n a b
 | b == n = -1.0
 | otherwise = (Matrix.atIndex (w !! a) b) - (Matrix.atIndex (w !! a) n)

intersectPlanesG :: [Plane] -> Int -> Int -> Double
intersectPlanesG w n a = negate (Matrix.atIndex (w !! a) n)

intersectPlanesH :: Int -> [Plane] -> Maybe Point
intersectPlanesH n w = let
 square = Matrix.matrix n [intersectPlanesF w n a b | a <- (indices n), b <- (indices n)]
 rhs = Matrix.matrix 1 [intersectPlanesG w n a | a <- (indices n)]
 in fmap Matrix.flatten (Matrix.linearSolve square rhs)

isAbovePlane :: Point -> Plane -> Bool
isAbovePlane v w = let
 dim = Matrix.size w
 planeS = Matrix.atIndex w (dim-1)
 pointS = Matrix.atIndex v (dim-1)
 planeV = Matrix.fromList (map (\x -> x - planeS) (Matrix.toList (Matrix.subVector 0 (dim-1) w)))
 pointV = Matrix.subVector 0 (dim-1) v
 in pointS > ((Matrix.dot planeV pointV) + planeS)

-- return space with sidednesses determined by given planes
spaceFromPlanes :: Int -> [Plane] -> Space
spaceFromPlanes n p
 | numPlanes == 0 = []
 | otherwise = let
 -- recurse with one fewer plane
 headSpace :: Space
 headSpace = spaceFromPlanes n headPlanes
 -- find (n-1)-tuples of recursed planes
 colTuples :: [[Int]]
 colTuples = subsets planeDims headIdxs
 -- find union of sub-regions of super-regions containing intersections
 headRegs :: [Region]
 headRegs = sortNub (concat (map (spaceFromPlanesF n headSpace headPlanes tailPlane headIdxs) colTuples))
 -- return space with found regions divided by new boundary
 in divideSpace headRegs headSpace where
 numPlanes = (length p) - 1
 headPlanes = take numPlanes p
 tailPlane = p !! numPlanes
 headIdxs = indices numPlanes
 spaceDims = Matrix.size tailPlane
 planeDims = spaceDims - 1

 -- find sub-regions of super-region containing indicated intersection point
spaceFromPlanesF :: Int -> Space -> [Plane] -> Plane -> [Int] -> [Int] -> [Region]
spaceFromPlanesF n headSpace headPlanes tailPlane headIdxs tupl = let
 headBounds = boundariesOfSpace headSpace
 cols = filter (\j -> elem j tupl) headIdxs
 (subB,subS) = foldl (\(b,s) j -> (remove j b, subSpace j s)) (headBounds,headSpace) tupl
 subP = subset cols headPlanes
 indP = subset tupl headPlanes
 intP = fromJust (intersectPlanes n (indP ++ [tailPlane]))
 in sort (takeRegions (zip subB subS) (zip headBounds headSpace) [regionOfPoint intP subP subS])

-- return region of point, assuming planes and space are homeomorphic
regionOfPoint :: Point -> [Plane] -> Space -> Region
regionOfPoint v w s = let
 dim = Matrix.size v
 num = length w
 planes :: [Int]
 planes = indices num
 -- find n others for each boundary
 tuplesI :: [[Int]]
 tuplesI = map (\b -> take dim (remove b planes)) planes
 tuplesB :: [[Boundary]]
 tuplesB = map (map intToBoundary) tuplesI
 -- for each plane, find reference point not on plane
 zero :: Point
 zero = Matrix.fromList (replicate dim 0.0)
 vertices :: [Point]
 vertices = map (\b -> fromMaybe zero (intersectPlanes dim (subset b w))) tuplesI
 -- find sides of reference points wrt planes
 sidesV :: [Bool]
 sidesV = map (\(v,w) -> isAbovePlane v w) (zip vertices w)
 -- find sides of point wrt planes
 sidesP :: [Bool]
 sidesP = map (\w -> isAbovePlane v w) w
 -- find sides of n-tuples wrt boundaries
 sidesS :: [Bool]
 sidesS = map (\(a,b) -> vertexWrtBoundary a (map intToBoundary b) s) (enumerate tuplesB)
 -- use transitivity to complete sides of point wrt boundaries
 sidesR :: [Sidedness]
 sidesR = map (\(a,b) -> a == b) (zip sidesV sidesP)
 -- return regionOfSides
 in regionOfSides sidesR s

regionWrtBoundary :: Boundary -> Region -> Space -> Sidedness
regionWrtBoundary b r s = undefined

vertexWrtBoundary :: Boundary -> [Boundary] -> Space -> Sidedness
vertexWrtBoundary b r s = undefined

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
minEquivWithPermsF1 :: Region -> Region -> ([HalfSpace],[HalfSpace]) -> [HalfSpace]
minEquivWithPermsF1 r s (p,q) = sort (map (minEquivWithPermsF2 r s) (zip p q))

-- add r to p if s in q
minEquivWithPermsF2 :: Region -> Region -> (HalfSpace,HalfSpace) -> HalfSpace
minEquivWithPermsF2 r s (p,q)
 | member s q = insert r p
 | otherwise = p

-- remove s from q
minEquivWithPermsF3 :: Space -> Region -> Space
minEquivWithPermsF3 q s = map (map (filter (\a -> a /= s))) q

-- return space with regions permuted such that result is smallest possible
minEquiv :: Space -> Space
minEquiv s = fst (minEquivWithPerms [] (sortSpace s) 0 (regionsOfSpace s) [])

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
migrateSpace r s = map (migrateSpaceF (attachedBoundaries r s) r) (enumerate s)

migrateSpaceF :: [Boundary] -> Region -> (Boundary,FullSpace) -> FullSpace
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
regionOfSidesF r s = foldl (\a (r,s) -> a +\ (s !! (sidednessToInt r))) (regionsOfSpace s) (zip r s)

-- return all boundaries in space
boundariesOfSpace :: Space -> [Boundary]
boundariesOfSpace s = indices (length s)

-- return all regions in space
regionsOfSpace :: Space -> [Region]
regionsOfSpace s = sortNub (concat (concat s))

-- return boundaries attached to region
attachedBoundaries :: Region -> Space -> [Boundary]
attachedBoundaries r s = filter (\b -> oppositeOfRegionExists [b] r s) (boundariesOfSpace s)

-- return facets attached to region
attachedFacets :: Int -> Region -> Space -> [[Boundary]]
attachedFacets n r s = filter (\b -> oppositeOfRegionExists b r s) (subsets n (boundariesOfSpace s))

-- return regions in corners of boundaries
attachedRegions :: [Boundary] -> Space -> [Region]
attachedRegions b s = filter (\r -> oppositeOfRegionExists b r s) (regionsOfSpace s)

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
 boundaries = boundariesOfSpace s
 sizes = boundaries
 subs = foldl (\a b -> a ++ (subsets b boundaries)) [] sizes
 in foldl (\a b -> a && (isLinearF n s b)) True subs

isLinearF :: Int -> Space -> [Boundary] -> Bool
isLinearF d s b = let
 subset = sortNub b
 fixed = map (\(a,b) -> b - a) (enumerate subset)
 subspace = foldl (\a b -> subSpace b a) s fixed
 regions = regionsOfSpace subspace
 boundaries = boundariesOfSpace subspace
 in (defineLinear d (length boundaries)) == (length regions)

-- assume given spaces are subspaces in a superspace
-- return regions in second space that overlap any of the given regions in the first space
takeRegions :: SubSpace -> SubSpace -> [Region] -> [Region]
takeRegions s t r = let
 (firstBoundaries,firstSpace) = unzip s
 (secondBoundaries,secondSpace) = unzip t
 -- for each given region, find sides in first space
 firstSides :: [[Sidedness]]
 firstSides = map (\r -> sidesOfRegion r firstSpace) r
 -- for each boundary in second space, find corresponding boundary in first space or Nothing
 secondToFirst :: [Maybe Int]
 secondToFirst = map (\a -> elemIndex a firstBoundaries) secondBoundaries
 -- for each given region, for each boundary in second space, find sidedness or Nothing by composition
 secondSides :: [Maybe [Sidedness]]
 secondSides = map (\r -> map (\b -> fmap (\b -> r !! b) b) secondToFirst) firstSides
 -- for each boundary in second space, count number of Nothing
 count :: Int
 count = foldl (\a b -> if b == Nothing then a+1 else a) 0 secondToFirst
 -- find sidedness permutations of same length as indices
 permutes :: [[Sidedness]]
 permutes = map (\a -> map (\b -> sidednessToBool (mod (shift a (negate b)) .&. 1)) (indices count)) (indices (shift 1 count))
 -- for each given region, for each permutation, fix second sides of region, consuming head of permutation as needed
 fixedSides :: [[Sidedness]]
 fixedSides = map (\(a,b) -> takeRegionsF a b) [(a,b) | a <- secondSides, b <- permutes]
 -- map sides to regions
 in map (\r -> regionOfSides r secondSpace) fixedSides

takeRegionsF :: [Maybe Sidedness] -> [Sidedness] -> [Sidedness]
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
 in withDups ++ [(halfspace ++ duplicates),(regions \\ halfspace)]

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
superSpace :: SubSpace -> SubSpace -> [Boundary] -> Space
superSpace p s q t r = undefined
