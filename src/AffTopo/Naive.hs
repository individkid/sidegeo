
module AffTopo.Naive where

-- naive in the sense of just one representation

import Prelude hiding ((++))
import qualified Prelude
import Data.List hiding ((\\), (++), insert)
import Data.Maybe
import Data.Bits
import qualified Numeric.LinearAlgebra as Matrix
import qualified System.Random as Random

type Boundary = Int -- index into Space
type Region = Int -- arbitrary identifier
type Sidedness = Bool -- index into FullSpace
type HalfSpace = [Region] -- assume welldef set
type FullSpace = [HalfSpace] -- assume disjoint covering pair
type Space = [FullSpace] -- assume equal covers
type SubSpace = [(Boundary,FullSpace)] -- assume one-to-one
type Plane = Matrix.Vector Double -- single column of distances above base
type Point = Matrix.Vector Double -- single column of coordinates

boolToSidedness :: Bool -> Sidedness
boolToSidedness a = a

sidednessToBool :: Sidedness -> Bool
sidednessToBool a = a

sidednessToInt :: Sidedness -> Int
sidednessToInt a = if a then 1 else 0

intToSidedness :: Int -> Sidedness
intToSidedness a = a /= 0

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
subset :: [Int] -> [a] -> [a]
subset p a = foldl (\b q -> (a !! q) : b) [] p

welldef :: Ord a => [a] -> [a]
welldef a = welldefF (sort a)

welldefF :: Ord a => [a] -> [a]
welldefF (a:(b:c))
 | a == b = b:(welldefF c)
 | otherwise = a:(welldefF (b:c))
welldefF (a:b) = a:(welldefF b)
welldefF [] = []

member :: Eq a => a -> [a] -> Bool
member a b = (find (\c -> a == c) b) /= Nothing

insert :: Ord a => a -> [a] -> [a]
insert a b = welldef (a:b)

remove :: Eq a => a -> [a] -> [a]
remove a b = filter (\c -> c /= a) b

unplace :: Int -> [a] -> [a]
unplace a b = (take a b) Prelude.++ (drop (a+1) b)

replace :: Int -> a -> [a] -> [a]
replace a b c = (take a c) Prelude.++ (b : (drop (a+1) c))

choose :: Random.RandomGen g => g -> [a] -> (a, g)
choose g a = let (b,h) = Random.randomR (0,(length a)-1) g in ((a !! b), h)

holes :: Ord a => Num a => Int -> [a] -> [a]
holes n a = take n ((indices ((length a)+n)) \\ a)

indices :: Num a => Int -> [a]
indices n = take n (iterate (\a -> a + 1) 0)

enumerate :: [a] -> [(Int,a)]
enumerate a = zip (indices (length a)) a

image :: Eq a => [a] -> [(a,b)] -> [b]
image a m = range (filter (\(x,_) -> member x a) m)

preimage :: Eq b => [b] -> [(a,b)] -> [a]
preimage b m = domain (filter (\(_,y) -> member y b) m)

domain :: [(a,b)] -> [a]
domain m = map (\(x,_) -> x) m

range :: [(a,b)] -> [b]
range m = map (\(_,y) -> y) m

-- ++ and \\ are as in Data.List except welldef
(++) :: Ord a => [a] -> [a] -> [a]
a ++ b = unionF (welldef a) (welldef b)

unionF :: Ord a => [a] -> [a] -> [a]
unionF (a:s) (b:t)
 | a < b = a:(unionF s (b:t))
 | a == b = a:(unionF s t)
 | otherwise = b:(unionF (a:s) t)
unionF (a:s) [] = (a:s)
unionF [] b = b

(\\) :: Ord a => [a] -> [a] -> [a]
a \\ b =differenceF (welldef a) (welldef b)

differenceF :: Ord a => [a] -> [a] -> [a]
differenceF (a:s) (b:t)
 | a < b = a:(differenceF s (b:t))
 | a == b = (differenceF s t)
 | otherwise = (differenceF (a:s) t)
differenceF (a:s) [] = (a:s)
differenceF [] b = b

(+\) :: Ord a => [a] -> [a] -> [a]
a +\ b = intersectF (welldef a) (welldef b)

intersectF :: Ord a => [a] -> [a] -> [a]
intersectF (a:s) (b:t)
 | a < b = (intersectF s (b:t))
 | a == b = a:(intersectF s t)
 | otherwise = (intersectF (a:s) t)
intersectF (a:s) [] = (a:s)
intersectF [] b = b

(\+) :: Ord a => [a] -> [a] -> [a]
a \+ b = symmetricF (welldef a) (welldef b)

symmetricF :: Ord a => [a] -> [a] -> [a]
symmetricF (a:s) (b:t)
 | a < b = a:(symmetricF s (b:t))
 | a == b = (symmetricF s t)
 | otherwise = b:(symmetricF (a:s) t)
symmetricF (a:s) [] = (a:s)
symmetricF [] b = b

-- all connected by given function to given start
generate :: Ord a => (a -> [a]) -> a -> [a]
generate f a = generateF f a [] []

generateF :: Ord a => (a -> [a]) -> a -> [a] -> [a] -> [a]
generateF f a todo done
 | (length newTodo) == 0 = newDone
 | otherwise = generateF f (head newTodo) (tail newTodo) newDone where
 newTodo = remove a (((f a) \\ done) ++ todo)
 newDone = [a] ++ done

-- given number of firsts found by calling function on second
catalyze :: (g -> (a,g)) -> g -> Int -> ([a],g)
catalyze f g n = foldl (\(a,h) _ -> catalyzeF f h a) ([],g) ((indices n)::[Int])

catalyzeF :: (g -> (a,g)) -> g -> [a] -> ([a],g)
catalyzeF f g a = let (b,h) = f g in (b:a,h)

-- call function for new result until it returns Nothing
chainJust :: [(a -> Maybe a)] -> a -> Maybe a
chainJust (f:g) a = chainJustF g a Nothing (f a)
chainJust [] _ = Nothing

chainJustF :: [(a -> Maybe a)] -> a -> Maybe a -> Maybe a -> Maybe a
chainJustF (f:g) _ _ (Just c) = chainJustF g c (Just c) (f c)
chainJustF (_:_) _ b Nothing = b
chainJustF [] _ _ (Just c) = Just c
chainJustF [] _ b Nothing = b

-- call each funcion returning last Just
turnJust :: [(a -> Maybe a)] -> a -> Maybe a
turnJust (f:g) a = turnJustF g a Nothing (f a)
turnJust [] _ = Nothing

turnJustF :: [(a -> Maybe a)] -> a -> Maybe a -> Maybe a -> Maybe a
turnJustF (f:g) _ _ (Just c) = turnJustF g c (Just c) (f c)
turnJustF (f:g) a b Nothing = turnJustF g a b (f a)
turnJustF [] _ _ (Just c) = Just c
turnJustF [] _ b Nothing = b

-- return all boundaries in space
boundariesOfSpace :: Space -> [Boundary]
boundariesOfSpace s = map intToBoundary (indices (length s))

-- return all regions in space
regionsOfSpace :: Space -> [Region]
regionsOfSpace s = welldef (concat (concat s))

-- side of region with regard to boundary
regionWrtBoundary :: Boundary -> Region -> Space -> Sidedness
regionWrtBoundary b r s = intToSidedness (fromJust (findIndex (\a -> member r a) (s !! b)))

-- side of vertex identified by n boundaries
vertexWrtBoundary :: Boundary -> [Boundary] -> Space -> Sidedness
vertexWrtBoundary b r s = regionWrtBoundary b (fromJust (find (\a -> oppositeOfRegionExists r a s) (regionsOfSpace s))) s

-- return per boundary side of region
sidesOfRegion :: Region -> Space -> [Sidedness]
sidesOfRegion r s = map (\a -> member r (a !! 1)) s

-- return region from per boundary side
regionOfSides :: [Sidedness] -> Space -> Region
regionOfSides r s = (regionOfSidesF r s) !! 0

-- return whether region with given side map exists in space
regionOfSidesExists :: [Sidedness] -> Space -> Bool
regionOfSidesExists r s = (length (regionOfSidesF r s)) == 1

regionOfSidesF :: [Sidedness] -> Space -> [Region]
regionOfSidesF r s = foldl (\a (b,c) -> a +\ (c !! (sidednessToInt b))) (regionsOfSpace s) (zip r s)

-- return sidedness with boundaries reversed
oppositeOfSides :: [Boundary] -> [Sidedness] -> [Sidedness]
oppositeOfSides b r = foldl (\x y -> replace y (not (x !! y)) x) r b

-- return neighbor region of given region wrt given boundaries
oppositeOfRegion :: [Boundary] -> Region -> Space -> Region
oppositeOfRegion b r s = regionOfSides (oppositeOfSides b (sidesOfRegion r s)) s

-- return whether neighbor region exists
oppositeOfRegionExists :: [Boundary] -> Region -> Space -> Bool
oppositeOfRegionExists b r s = regionOfSidesExists (oppositeOfSides b (sidesOfRegion r s)) s

-- return shell of regions around given region
oppositesOfRegion :: Region -> Space -> [Region]
oppositesOfRegion r s = map (\b -> oppositeOfRegion [b] r s) (attachedBoundaries r s)

-- return boundaries attached to region
attachedBoundaries :: Region -> Space -> [Boundary]
attachedBoundaries r s = filter (\b -> oppositeOfRegionExists [b] r s) (boundariesOfSpace s)

-- return facets attached to region
attachedFacets :: Int -> Region -> Space -> [[Boundary]]
attachedFacets n r s = filter (\b -> oppositeOfRegionExists b r s) (subsets n (attachedBoundaries r s))

-- return regions in corners of boundaries
attachedRegions :: [Boundary] -> Space -> [Region]
attachedRegions b s = filter (\r -> oppositeOfRegionExists b r s) (regionsOfSpace s)

-- return corresponding outside region
outsideOfRegion :: Region -> Space -> Region
outsideOfRegion r s = oppositeOfRegion (boundariesOfSpace s) r s

-- return whether the region is an outside region
outsideOfRegionExists :: Region -> Space -> Bool
outsideOfRegionExists r s = oppositeOfRegionExists (boundariesOfSpace s) r s

-- return all linear spaces of given dimension and boundaries.
allSpaces :: Random.RandomGen g => g -> Int -> Int -> ([Space], g)
allSpaces g n m = let
 boundaries = map intToBoundary (indices m)
 fullspace = [[0],[1]]
 emptyspace = []
 (p,h) = foldl (\(x,y) z -> superSpace y n x [(z,fullspace)]) (emptyspace,g) boundaries
 q = minEquiv (range p)
 in (allSpacesF (regionsOfSpace q) q [] [], h)

-- migrate all possible from current space, and go on to next todo
allSpacesF :: [Region] -> Space -> [Space] -> [Space] -> [Space]
allSpacesF (p:q) s todo done
 | canMigrate p s = allSpacesG q s (minEquiv (migrateSpace p s)) todo done
 | otherwise = allSpacesF q s todo done
allSpacesF [] s todo done = allSpacesH todo (insert s done)

-- if migration not already done or todo, recurse with migration added to todo
allSpacesG :: [Region] -> Space -> Space -> [Space] -> [Space] -> [Space]
allSpacesG r s t todo done
 | (s == t) || (member t todo) || (member t done) = allSpacesF r s todo done
 | otherwise = allSpacesF r s (t:todo) done

-- recurse with choice removed from todo
allSpacesH :: [Space] -> [Space] -> [Space]
allSpacesH (s:todo) done = allSpacesF (regionsOfSpace s) s todo done
allSpacesH [] done = done

minEquiv :: Space -> Space
minEquiv posit = let
 todo = regionsOfSpace posit
 regions = map intToRegion (indices (length todo))
 results = foldl minEquivPrefix [([],posit,todo,[])] regions
 (result,_,_,_) = head results
 in sortSpace result

minEquivWithPerms :: Space -> (Space,[[Region]])
minEquivWithPerms posit = let
 todo = regionsOfSpace posit
 regions = map intToRegion (indices (length todo))
 results = foldl minEquivPrefix [([],posit,todo,[])] regions
 (result,_,_,_) = head results
 dones = map (\(_,_,_,z) -> z) results
 in (sortSpace result, dones)

-- members of listed tuples are unsorted minimum sofar, positions of regions, position regions todo, inorder regions done
minEquivPrefix :: [(Space, Space, [Region], [Region])] -> Region -> [(Space, Space, [Region], [Region])]
minEquivPrefix s r = let
 branches = concat (map (\(w,x,y,z) -> minEquivPrefixF w x y z r) s)
 comparable = map (\(w,x,y,z) -> (sortSpace w, (w, x, y, z))) branches
 result = minimum (map fst comparable)
 results = filter (\(x,_) -> x == result) comparable
 in map snd results

-- given minimum sofar, positional todo, listed todo, inorder done, region to add to given minimum
-- return new possible minimum for each listed todo
minEquivPrefixF :: Space -> Space -> [Region] -> [Region] -> Region -> [(Space, Space, [Region], [Region])]
minEquivPrefixF sofar posit todo done toadd = map (minEquivPrefixG sofar posit todo done toadd) todo

-- return new possible minimum using given from todo
minEquivPrefixG :: Space -> Space -> [Region] -> [Region] -> Region -> Region -> (Space, Space, [Region], [Region])
minEquivPrefixG sofar posit todo done toadd torem = let
 newSofar = minEquivPrefixH sofar posit toadd torem
 newTodo = remove torem todo
 newDone = torem:done
 in (newSofar, posit, newTodo, newDone)

-- add toadd to sofar in positions of torem in posit
minEquivPrefixH :: Space -> Space -> Region -> Region -> Space
minEquivPrefixH sofar posit toadd torem = map (minEquivPrefixI toadd torem) (zip sofar posit)

-- add toadd to sofar in position of torem in posit
minEquivPrefixI :: Region -> Region -> ([HalfSpace],[HalfSpace]) -> [HalfSpace]
minEquivPrefixI toadd torem (sofar,posit) = map (minEquivPrefixJ toadd torem) (zip sofar posit)

-- add toadd to sofar if torem in posit
minEquivPrefixJ :: Region -> Region -> (HalfSpace,HalfSpace) -> HalfSpace
minEquivPrefixJ toadd torem (sofar,posit)
 | member torem posit = insert toadd sofar
 | otherwise = sofar

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

-- return space with given regions divided by new boundary
divideSpace :: [Region] -> Space -> Space
divideSpace figure space = let
 whole = regionsOfSpace space
 ground = whole \\ figure
 halfspace = divideSpaceF ground space
 duplicates = holes (length figure) whole
 mapping = zip figure duplicates
 withDups = map (map (divideSpaceG mapping figure)) space
 newBoundary = [halfspace ++ duplicates, whole \\ halfspace]
 in withDups Prelude.++ [newBoundary]

divideSpaceF :: [Region] -> Space -> [Region]
divideSpaceF r s
 | (length r) > 0 = generate (\a -> r +\ (oppositesOfRegion a s)) (head r)
 | otherwise = []

divideSpaceG :: [(Region,Region)] -> [Region] -> [Region] -> [Region]
divideSpaceG m a b = b ++ (image (a +\ b) m)

-- return space of same dimension with given boundary removed
subSpace :: Boundary -> Space -> Space
subSpace b s = let
 regions = filter (\r -> regionWrtBoundary b r s) (attachedRegions [b] s)
 in map (map (\x -> x \\ regions)) (unplace b s)

-- return space of one less dimension homeomorphic to regions attached to given boundary
sectionSpace :: Boundary -> Space -> Space
sectionSpace b s = let
 regions = filter (\r -> regionWrtBoundary b r s) (attachedRegions [b] s)
 in map (map (\x -> x +\ regions)) (unplace b s)

-- assume given spaces are subspaces in a superspace
-- return regions in second space that overlap any of the given regions in the first space
takeRegions :: SubSpace -> SubSpace -> [Region] -> [Region]
takeRegions s t r = let
 (firstBoundaries,firstSpace) = unzip s
 (secondBoundaries,secondSpace) = unzip t
 -- for each given region, find sides in first space
 firstSides :: [[Sidedness]] -- given of SRegion -> SBoundary -> Sidedness
 firstSides = map (\x -> sidesOfRegion x firstSpace) r
 -- for each boundary in second space, find corresponding boundary in first space or Nothing
 secondToFirst :: [Maybe Boundary] -- TBoundary -> Maybe SBoundary
 secondToFirst = map (\x -> elemIndex x firstBoundaries) secondBoundaries
 -- for each given region, for each boundary in second space, find sidedness or Nothing in first space
 secondSides :: [[Maybe Sidedness]] -- given of SRegion -> TBoundary -> Maybe Sidedness
 secondSides = map (\x -> map (\y -> fmap (\z -> x !! z) y) secondToFirst) firstSides
 -- for each boundary in second space, count number of Nothing
 count :: Int
 count = foldl (\x y -> if y == Nothing then x+1 else x) 0 secondToFirst
 -- find sidedness permutations of same length as indices
 permutes :: [[Sidedness]] -- every possible -> count of TBoundary -> Sidedness
 permutes = map (\x -> map (\y -> intToSidedness (x .&. (shift 1 y))) (indices count)) (indices (shift 1 count))
 -- for each given region, for each permutation, fix second sides of region, consuming head of permutation as needed
 fixedSides :: [[Sidedness]] -- given of SRegion -> TBoundary -> Sidedness
 fixedSides = map (\(x,y) -> takeRegionsF x y) [(x,y) | x <- secondSides, y <- permutes]
 -- ignore empty regions
 extantSides :: [[Sidedness]] -- given of SRegion -> TBoundary -> Sidedness
 extantSides = filter (\x -> regionOfSidesExists x secondSpace) fixedSides
 -- map sides to regions
 in map (\x -> regionOfSides x secondSpace) extantSides

takeRegionsF :: [Maybe Sidedness] -> [Sidedness] -> [Sidedness]
takeRegionsF ((Just a):b) c = a:(takeRegionsF b c)
takeRegionsF (Nothing:b) (c:d) = c:(takeRegionsF b d)
takeRegionsF [] [] = []
takeRegionsF (Nothing:_) [] = error "not enough permutations"
takeRegionsF [] (_:_) = error "too many permutations"

-- return superspace with given spaces as subspaces
superSpace :: Random.RandomGen g => g -> Int -> SubSpace -> SubSpace -> (SubSpace, g)
superSpace g n s t
 | n == 0 = (map (\x -> (x,[[0],[]])) (sBounds ++ tBounds), g)
 | (length (tBounds \\ sBounds)) == 0 = (s,g)
 | (length (sBounds \\ tBounds)) == 0 = (t,g)
 | (length (sBounds ++ tBounds)) <= n = let
  -- every possible region
  boundaries = sBounds ++ tBounds
  regions = indices (shift 1 (length boundaries))
  in (map (\x -> (x, [superSpaceI x regions 0, superSpaceI x regions 1])) boundaries, g)
 | ((length (sBounds +\ tBounds)) == 0) && ((length tBounds) == 1) = superSpace g n t s
 | ((length (sBounds +\ tBounds)) == 0) && ((length sBounds) == 1) = let
  -- choose boundary to immitate
  -- find section by chosen
  -- recurse to add singleton boundary to chosen section
  -- take from section for linear subset of regions attached to immitate
  -- divide by this linear subset of regions
  sBound = head sBounds -- boundary to add
  (bound,h) = choose g tBounds -- boundary to immitate
  section = superSpaceG bound t -- homeomorphic to immitated or added
  (supersect,i) = superSpace h (n-1) [(bound,[[0],[1]])] section -- homeomorphic after restoring immitated
  regions = takeRegions supersect t (regionsOfSpace (range supersect)) -- regions in t that are homeomorphic
  in (superSpaceH sBound regions t, i)
 | (length (sBounds +\ tBounds)) == 0 = let
  -- choose boundary to remove
  -- recurse with one fewer boundary
  -- recurse to restore boundary
  (bound,h) = choose g sBounds
  subspace = superSpaceF bound s
  (space,i) = superSpace h n subspace t
  singlespace = [(bound,[[0],[1]])]
  in superSpace i n space singlespace
 | ((length (sBounds \\ tBounds)) == 1) && ((length (tBounds \\ sBounds)) == 1) = let
  -- choose shared boundary for sections
  -- recurse to find subspace (wrt chosen) of supersection
  -- find subspace (wrt left unshared) of full supersection
  -- recurse to find full supersection
  -- take regions to space with right unshared to find those divided by left unshared
  sBound = head (sBounds \\ tBounds) -- left unshared
  (bound,h) = choose g (sBounds +\ tBounds) -- shared
  tSect = superSpaceG bound t -- missing bound and sBound
  section = superSpaceG sBound s -- missing sBound and tBound
  (sSect,i) = superSpace h (n-1) tSect section -- missing sBound
  regions = takeRegions sSect t (regionsOfSpace (range sSect))
  in ((superSpaceH sBound regions t), i)
 | ((length (sBounds \\ tBounds)) == 1) = superSpace g n t s
 | otherwise = let -- s and t are not proper
  -- 0 1 2 3 5 7 + 0 1 2 4 =
  -- 0 1 2 3 5 7 + (0 1 2 3 5 + 0 1 2 4) =
  -- 0 1 2 3 5 7 + (0 1 2 3 5 + (0 1 2 3 + 0 1 2 4))
  -- or
  -- 0 1 2 3 5 7 + 0 1 2 4 6 8 =
  -- 0 1 2 3 5 7 + (0 1 2 3 5 + 0 1 2 4 6 8) =
  -- 0 1 2 3 5 7 + (0 1 2 3 5 + (0 1 2 3 + 0 1 2 4 6 8))
  (b,h) = choose g (sBounds \\ tBounds) -- choice is from more than one
  sub = superSpaceF b s -- since choice was from more than one, sub and t are not proper
  (sup,i) = superSpace h n sub t -- adds something to t because sub and t are not proper
  in superSpace i n s sup where -- easier because sup contains t plus one other than b that t did not
  sBounds = domain s
  tBounds = domain t

-- subspace with boundary map
superSpaceF :: Boundary -> SubSpace -> SubSpace
superSpaceF b s = let
 (bounds,space) = unzip s
 index = fromJust (elemIndex b bounds)
 in zip (unplace index bounds) (subSpace (intToBoundary index) space)

-- section space with boundary map
superSpaceG :: Boundary -> SubSpace -> SubSpace
superSpaceG b s = let
 (bounds,space) = unzip s
 index = fromJust (elemIndex b bounds)
 in zip (unplace index bounds) (sectionSpace (intToBoundary index) space)

-- divide space with boundary map
superSpaceH :: Boundary -> [Region] -> SubSpace -> SubSpace
superSpaceH b r s = let
 (bounds,space) = unzip s
 in zip (bounds Prelude.++ [b]) (divideSpace r space)

-- regions indicated by bits of boundary
superSpaceI :: Int -> [Int] -> Int -> [Region]
superSpaceI b r s = map intToRegion (filter (\y -> (y .&. (shift 1 b)) == s) r)

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
isLinearF n s b = let
 fixed = map (\(x,y) -> y - x) (enumerate (welldef b))
 subspace = foldl (\x y -> subSpace y x) s fixed
 regions = regionsOfSpace subspace
 boundaries = boundariesOfSpace subspace
 in (defineLinear n (length boundaries)) == (length regions)

-- return given number of planes in given number of dimensions
randomPlanes :: Random.RandomGen g => g -> Int -> Int -> ([Plane], g)
randomPlanes g n m = let
 (a,h) = catalyze (\i -> Random.randomR (-100.0,100.0) i) g (n*m)
 b = Matrix.toColumns (Matrix.matrix m a)
 tweaks = [randomPlanesF n m, randomPlanesG n m, randomPlanesH n m]
 in fromMaybe (b,h) (chainJust (repeat (turnJust tweaks)) (b,h))

-- shift by half to origin some plane from some n tuple that intersects outside -1.0 to 1.0 hypercube
randomPlanesF :: Random.RandomGen g => Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesF n m (a,g) = fmap (randomPlanesF0 g n a) (find (randomPlanesF1 n a) (subsets n (indices m)))

randomPlanesF0 :: Random.RandomGen g => g -> Int -> [Plane] -> [Int] -> ([Plane], g)
randomPlanesF0 g n a b = let
 (c,h) = choose g b
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
 (c,h) = choose g b
 (d,i) = catalyze (\j -> Random.randomR (-100.0,100.0) j) h n
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

-- return region of point, assuming planes and space are homeomorphic
regionOfPoint :: Point -> [Plane] -> Space -> Region
regionOfPoint v w s = let
 dim = Matrix.size v
 num = length w
 planes :: [Int]
 planes = indices num
 -- find n others for each boundary
 tuples :: [[Int]]
 tuples = map (\b -> take dim (remove b planes)) planes
 -- for each plane, find reference point not on plane
 vertices :: [Point]
 vertices = map (\b -> fromJust (intersectPlanes dim (subset b w))) tuples
 -- find sides of reference points wrt planes
 sidesV :: [Bool]
 sidesV = map (\(x,y) -> isAbovePlane x y) (zip vertices w)
 -- find sides of point wrt planes
 sidesP :: [Bool]
 sidesP = map (\x -> isAbovePlane v x) w
 -- find sides of n-tuples wrt boundaries
 sidesS :: [Bool]
 sidesS = map (\(a,b) -> vertexWrtBoundary (intToBoundary a) (map intToBoundary b) s) (enumerate tuples)
 -- use transitivity to complete sides of point wrt boundaries
 sidesR :: [Sidedness]
 sidesR = map (\(a,(b,c)) -> boolToSidedness (a == (b == c))) (zip sidesV (zip sidesP sidesS))
 -- return regionOfSides
 in regionOfSides sidesR s

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
 headRegs = welldef (concat (map (spaceFromPlanesF n headSpace headPlanes tailPlane headIdxs) colTuples))
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

-- return planes with sidednesses as specified by given dimension and space
planesFromSpace :: Int -> Space -> [Plane]
planesFromSpace n s
 | (length s) <= n = take (length s) (Matrix.toColumns (Matrix.ident n))
 | otherwise = let
 -- recurse with one fewer boundary
 space = subSpace (intToBoundary ((length s) - 1)) s
 planes = planesFromSpace n space
 -- find vertices, interpret as coplanes
 tuples = subsets n (indices (length space))
 coplanes = map (\x -> fromJust (intersectPlanes n (subset x planes))) tuples
 -- convert coplanes to cospace with up-down sidedeness
 cospace = spaceFromPlanes n coplanes
 -- find coregion that separates coboundaries like given space boundary separates vertices
 bound = intToBoundary ((length s) - 1)
 vertices = map (map intToBoundary) tuples
 separate :: [[[Boundary]]] -- Sidedness -> VertexSet -> Tuple -> Boundary
 separate = planesFromSpaceF bound vertices s
 coseparate :: [[Boundary]] -- Sidedness -> CoBoundarySet -> CoBoundary
 coseparate = map (map (\x -> fromJust (elemIndex x vertices))) separate
 coseparaterev = reverse coseparate
 coseparates :: [[[Boundary]]] -- CoRegion -> CoSidedness -> CoBoundarySet -> CoBoundary
 coseparates = planesFromSpaceG cospace
 coregion = intToRegion (fromJust (findIndex (\x -> (x == coseparate) || (x == coseparaterev)) coseparates))
 -- find point in coregion, interpret it as plane
 outin = outsideOfRegionExists coregion cospace
 outpoint = planesFromSpaceI n coregion cospace coplanes
 inpoint = planesFromSpaceH n coregion cospace coplanes
 point = if outin then outpoint else inpoint
 in planes Prelude.++ [point]

-- return vertices on each side of given boundary
planesFromSpaceF :: Boundary -> [[Boundary]] -> Space -> [[[Boundary]]]
planesFromSpaceF b v s = let
 test = (\x -> vertexWrtBoundary b x s)
 first = (\x y -> [insert y (x !! 0), x !! 1])
 second = (\x y -> [x !! 0, insert y (x !! 1)])
 func = (\x y -> if (test y) then (second x y) else (first x y))
 in foldl func [[],[]] v

-- return per-region boundaries on each side of region
planesFromSpaceG :: Space -> [[[Boundary]]]
planesFromSpaceG s = let
 bounds = boundariesOfSpace s
 test = (\x y -> regionWrtBoundary x y s)
 first = (\x y -> [insert y (x !! 0), x !! 1])
 second = (\x y -> [x !! 0, insert y (x !! 1)])
 func = (\x y z -> if (test z x) then (second y z) else (first y z))
 in map (\x -> foldl (func x) [[],[]] bounds) (regionsOfSpace s)

-- return average of corners of coregeion
planesFromSpaceH :: Int -> Region -> Space -> [Plane] -> Point
planesFromSpaceH n coregion cospace coplanes = let
 corners = attachedFacets n coregion cospace
 points = map (\x -> fromJust (intersectPlanes n (subset x coplanes))) corners
 zero = Matrix.fromList (replicate n 0.0)
 in Matrix.scale (1.0 / (fromIntegral (length points))) (foldl (\x y -> Matrix.add x y) zero points)

 -- find point some distance out on line from coregion and other outside
planesFromSpaceI :: Int -> Region -> Space -> [Plane] -> Point
planesFromSpaceI n r s p = let
 arrow = planesFromSpaceH n r s p
 feather = planesFromSpaceH n (outsideOfRegion r s) s p
 shaft = Matrix.add arrow (Matrix.scale (negate 1.0) feather)
 factor = 0.1 / (sqrt (Matrix.dot shaft shaft))
 in Matrix.add arrow (Matrix.scale factor shaft)
