--    AffTopo.Naive functions for symbolically manipulating affine planes
--    Copyright (C) 2016  Paul Coelho
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.

module AffTopo.Naive where

import Prelude hiding ((++))
import qualified Prelude
import Data.List hiding ((\\), (++), insert)
import qualified Data.List
import Data.Maybe
import Data.Bits
import qualified Numeric.LinearAlgebra as Matrix
import qualified System.Random as Random

type Boundary = Int -- index into Space
type Region = Int -- arbitrary identifier
type Side = Int -- index into Full
type Half = [Region] -- assume welldef set
type Full = [Half] -- assume disjoint covering pair
type Space = [Full] -- assume equal covers
type Dual = [[[Boundary]]] -- now Boundary is arbitrary
type Place = [(Boundary,Full)] -- assume one-to-one
type Pack = Int -- bits indicate membership
type Plane = Matrix.Vector Double -- distances above base
type Point = Matrix.Vector Double -- coordinates

intToBool :: Int -> Bool
intToBool a = a /= 0

boolToInt :: Bool -> Int
boolToInt a = if a then 1 else 0

notOfInt :: Int -> Int
notOfInt a = boolToInt (not (intToBool a))

packToBools :: Int -> Pack -> [Bool]
packToBools a b = map (\x -> belongs x b) (indices a)

boolsToPack :: [Bool] -> Pack
boolsToPack a = fold' (\x y -> (shift y 1) + (boolToInt x)) a 0

intsToPack :: [Int] -> Pack
intsToPack a = fold' (\x y -> y + (shift 1 x)) (welldef a) 0

packToInts :: Pack -> [Int]
packToInts a = let
 arith = iterate (1+) 0
 geom = map (flip shift $ 1) arith
 index = fromJust (findIndex (a<) geom)
 in filter (\x -> belongs x a) (take index arith)

intsToBools :: Int -> [Int] -> [Bool]
intsToBools a b = packToBools a (intsToPack b)

boolsToInts :: [Bool] -> [Int]
boolsToInts a = packToInts (boolsToPack a)

-- pack out masked bits
collapse :: Pack -> Pack -> Pack
collapse a b = let
 limit = (maximum ((packToInts a) ++ (packToInts b))) + 1
 zipped = zip (packToBools limit a) (packToBools limit b)
 masked = filter fst zipped
 in boolsToPack (range masked)

-- all subsets of non-negative Int less than given
power :: Int -> [Pack]
power a = indices (shift 1 a)

polybools :: Int -> [[Bool]]
polybools a = map (packToBools a) (power a)

polyants :: Int -> [[Side]]
polyants a = map2 boolToInt (polybools a)

-- whether given Int is in given set
belongs :: Int -> Pack -> Bool
belongs a b = testBit b a

-- all sublists of given size
subsets :: Ord a => Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (a:b) = (map (a:) (subsets (n-1) b)) Prelude.++ (subsets n b)

-- those indexed by list of indices
subset :: [Int] -> [a] -> [a]
subset a b = image a (enumerate b)

-- make elements sorted and unique
welldef :: Ord a => [a] -> [a]
welldef a = welldefF (sort a)

welldefF :: Ord a => [a] -> [a]
welldefF (a:(b:c))
 | a == b = welldefF (b:c)
 | otherwise = a:(welldefF (b:c))
welldefF a = a

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

emplace :: Int -> a -> [a] -> [a]
emplace a b c = (take a c) Prelude.++ (b : (drop a c))

choose :: Random.RandomGen g => g -> [a] -> (a, g)
choose g a = let (b,h) = Random.randomR (0,(length a)-1) g in ((a !! b), h)

holes :: Ord a => Num a => Int -> [a] -> [a]
holes n a = take n ((indices ((length a)+n)) \\ a)

indices :: Num a => Int -> [a]
indices n = take n (iterate (1+) 0)

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

-- ++ is as in Data.List except welldef
(++) :: Ord a => [a] -> [a] -> [a]
a ++ b = a Prelude.++ (b \\ a)

(\\) :: Ord a => [a] -> [a] -> [a]
a \\ b = a Data.List.\\ b

-- intersection
(+\) :: Ord a => [a] -> [a] -> [a]
a +\ b = a \\ (a \\ b)

-- symmetric difference
(\+) :: Ord a => [a] -> [a] -> [a]
a \+ b = (a ++ b) \\ (a +\ b)

-- all connected by given function to given start
generate :: Ord a => (a -> [a]) -> a -> [a]
generate f a = generateF f a [] []

generateF :: Ord a => (a -> [a]) -> a -> [a] -> [a] -> [a]
generateF f a todo done
 | (length newTodo) == 0 = newDone
 | otherwise = generateF f (head newTodo) (tail newTodo) newDone where
 newTodo = remove a (((f a) \\ done) ++ todo)
 newDone = insert a done

-- given number of firsts found by calling function on second
catalyze :: (g -> (a,g)) -> g -> Int -> ([a],g)
catalyze f g n = fold' (\_ (a,h) -> catalyzeF f h a) ((indices n)::[Int]) ([],g)

catalyzeF :: (g -> (a,g)) -> g -> [a] -> ([a],g)
catalyzeF f g a = let (b,h) = f g in (b:a,h)

-- call function for new result until it returns Nothing
foldMaybe :: (a -> b -> Maybe b) -> [a] -> b -> b
foldMaybe f (a:b) c = foldMaybeF f b c (f a c)
foldMaybe _ [] c = c

foldMaybeF :: (a -> b -> Maybe b) -> [a] -> b -> Maybe b -> b
foldMaybeF f (a:b) _ (Just c) = foldMaybeF f b c (f a c)
foldMaybeF _ _ a _ = a

-- call function until it returns Just
findMaybe :: (b -> Maybe a) -> [b] -> Maybe a
findMaybe f (b:c) = findMaybeF f c (f b)
findMaybe _ [] = Nothing

findMaybeF :: (b -> Maybe a) -> [b] -> Maybe a -> Maybe a
findMaybeF _ _ (Just b) = Just b
findMaybeF f (b:c) Nothing = findMaybeF f c (f b)
findMaybeF _ [] Nothing = Nothing

-- modify function taking single to function taking list
fold' :: (a -> b -> b) -> [a] -> b -> b
fold' = flip . foldr

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

--
-- now for something new
--

-- return how many regions a space of given dimension and boundaries has
defineLinear :: Int -> Int -> Int
defineLinear n m
 | (n < 0) || (m < 0) = undefined
 | (n == 0) || (m == 0) = 1
 | otherwise = (defineLinear n (m-1)) + (defineLinear (n-1) (m-1))

-- return whether all subspaces have correct number of regions
isLinear :: Int -> Space -> Bool
isLinear n s
 | n < 0 = undefined
 | (n == 0) || ((length s) == 0) = (length (regionsOfSpace s)) == 1
 | n == 1 = let
  halves = concat (map (\(x,y) -> map (\z -> (x,z)) y) (enumerate s))
  ends = filter (\(_,x) -> (length x) == 1) halves
  dirs = map (\(_,x) -> filter (\(_,y) -> (length (x \\ y)) == 0) halves) ends
  func x y
   | (length (y \\ x)) == 0 = GT
   | (length (x \\ y)) == 0 = LT
   | otherwise = EQ
  comp a b = let ((_,x),(_,y)) = (a,b) in func x y
  sorts = map (\z -> sortBy comp z) dirs
  domains = map domain sorts
  valid [x,y] = x == (reverse y)
  valid _ = False
  in valid domains
 | otherwise = let
  boundaries = boundariesOfSpace s
  sizes = indices (length boundaries)
  subs = fold' (\a b -> b Prelude.++ (subsets a boundaries)) sizes []
  in fold' (\a b -> b && (isLinearF n s a)) subs True

isLinearF :: Int -> Space -> [Boundary] -> Bool
isLinearF n s b = let
 subspace = range (fold' subSpace b (enumerate s))
 regions = regionsOfSpace subspace
 boundaries = boundariesOfSpace subspace
 in (defineLinear n (length boundaries)) == (length regions)

-- return all boundaries in space
boundariesOfSpace :: Space -> [Boundary]
boundariesOfSpace s = indices (length s)

-- return all regions in space
regionsOfSpace :: Space -> [Region]
regionsOfSpace [] = [0]
regionsOfSpace s = concat (head s)

-- side of region with regard to boundary
regionWrtBoundary :: Boundary -> Region -> Space -> Side
regionWrtBoundary b r s = fromJust (findIndex (\a -> member r a) (s !! b))

-- side of vertex identified by n boundaries
vertexWrtBoundary :: Boundary -> [Boundary] -> Space -> Side
vertexWrtBoundary b r s = regionWrtBoundary b (head (attachedRegions r s)) s

-- return per boundary side of region
sidesOfRegion :: Region -> Space -> [Side]
sidesOfRegion r s = map (\b -> regionWrtBoundary b r s) (boundariesOfSpace s)

-- return region from per boundary side
regionOfSides :: [Side] -> Space -> Region
regionOfSides r s = let [x] = (regionOfSidesF r s) in x

-- return whether region with given side map exists in space
regionOfSidesExists :: [Side] -> Space -> Bool
regionOfSidesExists r s = (length (regionOfSidesF r s)) == 1

regionOfSidesF :: [Side] -> Space -> [Region]
regionOfSidesF r s = fold' (\(b,c) a -> a +\ (c !! b)) (zip r s) (regionsOfSpace s)

-- return sidedness with boundaries reversed
oppositeOfSides :: [Boundary] -> [Side] -> [Side]
oppositeOfSides b r = fold' (\x y -> replace x (notOfInt (y !! x)) y) b r

-- return neighbor region of given region wrt given boundaries
oppositeOfRegion :: [Boundary] -> Region -> Space -> Region
oppositeOfRegion b r s = let
 opposite = oppositeOfSides b (sidesOfRegion r s)
 in regionOfSides opposite s

-- return whether neighbor region exists
oppositeOfRegionExists :: [Boundary] -> Region -> Space -> Bool
oppositeOfRegionExists b r s = let
 opposite = oppositeOfSides b (sidesOfRegion r s)
 in regionOfSidesExists opposite s

-- return shell of regions around given region
oppositesOfRegion :: Region -> Space -> [Region]
oppositesOfRegion r s = let
 opposite b = oppositeOfRegion [b] r s
 in map (\b -> opposite b) (attachedBoundaries r s)

-- return boundaries attached to region
attachedBoundaries :: Region -> Space -> [Boundary]
attachedBoundaries r s = let
 opposite b = oppositeOfRegionExists [b] r s
 in filter (\b -> opposite b) (boundariesOfSpace s)

-- return facets attached to region
attachedFacets :: Int -> Region -> Space -> [[Boundary]]
attachedFacets n r s = let
 opposite b = oppositeOfRegionExists b r s
 in filter (\b -> opposite b) (subsets n (attachedBoundaries r s))

-- return regions in corners of boundaries
attachedRegions :: [Boundary] -> Space -> [Region]
attachedRegions b s = let
 place = enumerate s
 in takeRegions (fold' sectionSpace b place) place

-- return corresponding outside region
outsideOfRegion :: Region -> Space -> Region
outsideOfRegion r s = oppositeOfRegion (boundariesOfSpace s) r s

-- return whether the region is an outside region
outsideOfRegionExists :: Region -> Space -> Bool
outsideOfRegionExists r s = oppositeOfRegionExists (boundariesOfSpace s) r s

-- return space with given region changed to its local opposite
migrateSpace :: Region -> Space -> Space
migrateSpace r s = map (migrateSpaceF (attachedBoundaries r s) r) (enumerate s)

migrateSpaceF :: [Boundary] -> Region -> (Boundary,Full) -> Full
migrateSpaceF b r (a,s)
 | (member a b) && (member r (s !! 0)) = [(remove r (s !! 0)),(insert r (s !! 1))]
 | (member a b) = [(insert r (s !! 0)),(remove r (s !! 1))]
 | otherwise = s

-- return whether local opposite of given region is empty
-- and all of its oppositeOf regions are non-empty
migrateSpaceExists :: Region -> Space -> Bool
migrateSpaceExists r s = let
 boundaries = attachedBoundaries r s
 sides = sidesOfRegion r s
 opposite = oppositeOfSides boundaries sides
 empty = not (regionOfSidesExists opposite s)
 neighbors = map (\a -> oppositeOfSides [a] opposite) boundaries
 exists = map (\a -> regionOfSidesExists a s) neighbors
 in fold' (\a b -> a && b) exists empty

-- return space of same dimension with given boundary removed
subSpace :: Boundary -> Place -> Place
subSpace b s = let
 (bounds,space) = unzip s
 index = fromJust (elemIndex b bounds)
 in zip (unplace index bounds) (subSpaceF index space)

subSpaceF :: Boundary -> Space -> Space
subSpaceF b s = let
 regions = filter (\r -> intToBool (regionWrtBoundary b r s)) (attachedRegions [b] s)
 in map2 (\x -> x \\ regions) (unplace b s)

-- return space of less dimension homeomorphic to regions attached to given boundary
sectionSpace :: Boundary -> Place -> Place
sectionSpace b s = let
 (bounds,space) = unzip s
 index = fromJust (elemIndex b bounds)
 in zip (unplace index bounds) (sectionSpaceF index space)

sectionSpaceF :: Boundary -> Space -> Space
sectionSpaceF b s = let
 regions = filter (\r -> intToBool (regionWrtBoundary b r s)) (attachedRegions [b] s)
 in map2 (\x -> x +\ regions) (unplace b s)

-- divide regions of s in t by new boundary b
divideSpace :: Boundary -> Place -> Place -> [Place]
divideSpace b s t = let
 place = zip (b : (domain t)) (divideSpaceF (takeRegions s t) (range t))
 in [place, mirrorSpace b place]

-- return space with given regions divided by new boundary
divideSpaceF :: [Region] -> Space -> Space
divideSpaceF figure space = let
 whole = regionsOfSpace space
 ground = whole \\ figure
 halfspace = divideSpaceG ground space
 duplicates = holes (length figure) whole
 mapping = zip figure duplicates
 withDups = map2 (divideSpaceH mapping figure) space
 newBoundary = [halfspace ++ duplicates, whole \\ halfspace]
 in newBoundary : withDups

divideSpaceG :: [Region] -> Space -> [Region]
divideSpaceG [] _ = []
divideSpaceG r s = generate (\a -> r +\ (oppositesOfRegion a s)) (head r)

divideSpaceH :: [(Region,Region)] -> [Region] -> [Region] -> [Region]
divideSpaceH m a b = (image (a +\ b) m) ++ b

-- return regions in second homeomorphic to regions in first
takeRegions :: Place -> Place -> [Region]
takeRegions s t = let
 regions = regionsOfSpace (range t)
 shared = (domain s) +\ (domain t)
 sSub = map2 (\x -> sort (shared +\ x)) (placeToDual s)
 tSub = map2 (\x -> sort (shared +\ x)) (placeToDual t)
 in preimage sSub (zip regions tSub) -- welldef because tSub is because regionsOfSpace is because tSpace is

-- all possible regions
powerSpace :: [Boundary] -> Place
powerSpace b = let
 regions = power (length b)
 in map (\(x,y) -> (y, [powerSpaceF x regions 0, powerSpaceF x regions 1])) (enumerate b)

-- regions indicated by boundary as bit position
powerSpaceF :: Int -> [Pack] -> Int -> [Region]
powerSpaceF b r s = filter (\y -> (boolToInt (belongs b y)) == s) r

-- remove region to produce non-linear space
degenSpace :: Region -> Place -> Place
degenSpace r s = zip (domain s) (map2 (remove r) (range s))

-- divide regions by space into non-linear space
crossSpace :: Place -> Place -> Place
crossSpace s t = let
 sSpace = range s
 tSpace = range t
 sRegions = regionsOfSpace sSpace
 tRegions = regionsOfSpace tSpace
 sPairs x = [(p,q) | p <- x, q <- tRegions]
 tPairs x = [(p,q) | p <- sRegions, q <- x]
 regions = enumerate [(x,y) | x <- sRegions, y <- tRegions]
 sCross = map2 (\x -> preimage (sPairs x) regions) sSpace
 tCross = map2 (\x -> preimage (tPairs x) regions) tSpace
 in zip ((domain s) ++ (domain t)) (sCross ++ tCross)

-- reverse sidedness of given boundary
mirrorSpace :: Boundary -> Place -> Place
mirrorSpace b s = map (\(x,[y,z]) -> if x == b then (x,[z,y]) else (x,[y,z])) s

-- subplace of each other
isEquSpace :: Place -> Place -> Bool
isEquSpace s t = (isSubSpace s t) && (isSubSpace t s)

-- each dual region of superspace is superset of some dual region of subspace
isSubSpace :: Place -> Place -> Bool
isSubSpace s t = let
 a = (length ((domain s) \\ (domain t))) == 0
 b = (length (takeRegions t s)) == (length (regionsOfSpace (range s)))
 c = (length (takeRegions s t)) == (length (regionsOfSpace (range t)))
 in a && b && c

-- subset is section space in dual representation
isSectionSpace :: Place -> Place -> Bool
isSectionSpace s t = let
 a = (length ((domain s) \\ (domain t))) == 0
 b = (length ((domain t) \\ (domain s))) == 0
 c = (length (takeRegions s t)) == (length (regionsOfSpace (range s)))
 in a && b && c

-- representation converters
placeToDual :: Place -> Dual
placeToDual s = let
 regions = regionsOfSpace (range s)
 left = map (\x -> domain (filter (\(_,[y,_]) -> member x y) s)) regions
 right = map (\x -> domain (filter (\(_,[_,y]) -> member x y) s)) regions
 in map (\(x,y) -> [x,y]) (zip left right)

dualToPlace :: Dual -> Place
dualToPlace s = let
 bounds = concat (head s)
 plual = enumerate s
 left = map (\x -> domain (filter (\(_,[y,_]) -> member x y) plual)) bounds
 right = map (\x -> domain (filter (\(_,[_,y]) -> member x y) plual)) bounds
 in zip bounds (map (\(x,y) -> [x,y]) (zip left right))

--
-- so far so simple
--

-- optimize this
minEquiv :: Space -> Space
minEquiv s = let
 bounds = boundariesOfSpace s
 place = zip bounds s
 dual = placeToDual place
 perms = permutations bounds
 equivs = map (\x -> minEquivF x dual) perms
 equiv = minimum equivs
 in range (dualToPlace equiv)

minEquivF :: [Boundary] -> Dual -> Dual
minEquivF b s = let
 perm = map (\x -> map (\y -> map (\z -> fromJust (elemIndex z b)) y) x) s
 in sort (map (\x -> sort (map (\y -> sort y) x)) perm)

-- return space by calling superSpace with singleton space
anySpace :: Random.RandomGen g => Show g => g -> Int -> Int -> (Space, g)
anySpace g n m = let
 (s,h) = fold' (\z (x,y) -> superSpace y n x (powerSpace [z])) (indices m) ([],g)
 in (range s, h)

-- return all linear spaces given any space to start
allSpaces :: Space -> [Space]
allSpaces s = let
 space = minEquiv s
 regions = regionsOfSpace space
 in allSpacesF regions space [] []

-- migrate all possible from current space, and go on to next todo
allSpacesF :: [Region] -> Space -> [Space] -> [Space] -> [Space]
allSpacesF (p:q) s todo done
 | migrateSpaceExists p s = allSpacesG q s (minEquiv (migrateSpace p s)) todo done
 | otherwise = allSpacesF q s todo done
allSpacesF [] s todo done = allSpacesH todo (insert s done)

-- if migration not already done or todo, recurse with migration added to todo
allSpacesG :: [Region] -> Space -> Space -> [Space] -> [Space] -> [Space]
allSpacesG r s t todo done
 | (s == t) || (member t todo) || (member t done) = allSpacesF r s todo done
 | otherwise = allSpacesF r s (insert t todo) done

-- recurse with choice removed from todo
allSpacesH :: [Space] -> [Space] -> [Space]
allSpacesH (s:todo) done = allSpacesF (regionsOfSpace s) s todo done
allSpacesH [] done = done

-- return space that is section of given spaces
subSection :: Random.RandomGen g => Show g => g -> Int -> Int -> Int -> Place -> Place -> Place -> (Place,g)
subSection g p q n s t u
 | (p > n) || (q > n) || (n < 0) || (dim < 0) = undefined
 | not (subSectionF s t u) = undefined
 | p == n = (t,g)
 | q == n = (s,g)
 | dim == 0 = let
  (region,h) = choose g (takeRegions s t)
  regions = remove region (regionsOfSpace (range t))
  in (fold' degenSpace regions t, h)
 | dim >= (length u) = (powerSpace (domain u), g)
 | dim > 0 = let
  bounds = domain u
  (bound,h) = choose g bounds
  sSub = subSpace bound s
  tSub = subSpace bound t
  uSub = subSpace bound u
  (sub,i) = subSection h p q n sSub tSub uSub
  uSect = sectionSpace bound u
  (sect,j) = subSection i dim (n-1) n sub uSect uSub
  anti = divideSpace bound sect sub
  valid = filter (subSectionF s t) anti
  in choose j valid
 | otherwise = undefined where
 dim = (p+q)-n

subSectionF :: Place -> Place -> Place -> Bool
subSectionF s t u = (isSectionSpace u s) && (isSectionSpace u t)

-- return superspace with given spaces as subspaces
superSpace :: Random.RandomGen g => Show g => g -> Int -> Place -> Place -> (Place, g)
superSpace g n s t
 | n < 0 = undefined
 | (n == 0) && (shared /= bounds) = undefined
 | (length tOnly) == 0 = (s,g)
 | (length sOnly) == 0 = (t,g)
 | (length bounds) <= n = (powerSpace bounds, g)
 | ((length shared) > 0) && ((length sOnly) == 1) && ((length tOnly) > 1) = superSpace g n t s
 | ((length shared) > 0) && ((length sOnly) > 1) = let
  (bound,h) = choose g sOnly
  (sup,i) = superSpace h n (subSpace bound s) t -- sOnly gets smaller because it is missing bound
  in superSpace i n s sup -- sOnly gets smaller because it goes from more than bound to only bound
 | ((length shared) == 0) && ((length sBounds) == 1) && ((length tBounds) > 1) = superSpace g n t s
 | ((length shared) == 0) && ((length sBounds) > 1) = let
  (bound,h) = choose g sBounds
  (sup,i) = superSpace h n (powerSpace [bound]) t -- sBounds becomes just bound from more than bound 
  in superSpace i n s sup -- s and sup share bound
 | ((length sOnly) == 1) && ((length tOnly) == 1) = rabbitSpace g n s t -- independent boundary case
 | otherwise = undefined where
 sBounds = domain s
 tBounds = domain t
 sOnly = sBounds \\ tBounds
 tOnly = tBounds \\ sBounds
 shared = sBounds +\ tBounds
 bounds = sBounds ++ tBounds

-- cases where sOnly and tOnly are singletons
rabbitSpace :: Random.RandomGen g => Show g => g -> Int -> Place -> Place -> (Place, g)
rabbitSpace g n s t
 | n >= 2 = let
  sSect = sectionSpace sBound s
  tSect = sectionSpace tBound t
  (sect,h) = subSection g (n-1) (n-1) n sSect tSect place
  sSup = divideSpace tBound sect sSect
  tSup = concat (map (\x -> divideSpace sBound x t) sSup)
  in rabbitSpaceF h n s t tSup
 | n == 1 = let
  sDual = placeToDual (crossSpace s (powerSpace [tBound]))
  tDual = placeToDual (crossSpace t (powerSpace [sBound]))
  double = powerSpace [sBound, tBound]
  regions = regionsOfSpace (range double)
  cross = map (\x -> crossSpace place (degenSpace x double)) regions
  dual = map placeToDual cross
  result = map (\x -> dualToPlace (sDual +\ tDual +\ x)) dual
  in rabbitSpaceF g n s t result
 | otherwise = undefined where
 sBounds = domain s
 tBounds = domain t
 [sBound] = sBounds \\ tBounds
 [tBound] = tBounds \\ sBounds
 place = subSpace sBound s

-- return given that is linear and contains both given
rabbitSpaceF :: Random.RandomGen g => Show g => g -> Int -> Place -> Place -> [Place] -> (Place,g)
rabbitSpaceF g n s t u = let
 result = filter (\x -> (isLinear n (range x)) && (isSubSpace x s) && (isSubSpace x t)) u
 in choose g result

--
-- between symbolic and numeric
--

-- return given number of planes in given number of dimensions
randomPlanes :: Random.RandomGen g => g -> Int -> Int -> ([Plane], g)
randomPlanes g n m = let
 (a,h) = catalyze (\i -> Random.randomR (-100.0,100.0) i) g (n * m)
 b = Matrix.toColumns (Matrix.matrix m a)
 tweak = repeat [randomPlanesH0 n m, randomPlanesH1 n m, randomPlanesH2 n m]
 func x y = findMaybe (\z -> z y) x
 in foldMaybe func tweak (b,h)

-- tweak some plane from tuple found by testing intersection of planes in tuple
randomPlanesF :: Random.RandomGen g => ((Plane, g) -> (Plane, g)) -> (Maybe Point -> Bool) -> (Int -> Int) ->
 Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesF i j k n m (a,g) = let
 b = find (\x -> j (intersectPlanes n (subset x a))) (subsets (k n) (indices m))
 in fmap (randomPlanesG i (a,g)) b

-- use function to replace choice from given tuple
randomPlanesG :: Random.RandomGen g => ((Plane, g) -> (Plane, g)) -> ([Plane], g) -> [Int] -> ([Plane], g)
randomPlanesG i (a,f) b = let
 x = choose f b
 (c,g) = x
 (d,h) = i (a !! c, g)
 in (replace c d a, h)

-- shift by half to origin some plane from some n tuple that intersects outside -1.0 to 1.0 hypercube
randomPlanesH0 :: Random.RandomGen g => Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesH0 n m = randomPlanesF randomPlanesI0 randomPlanesJ0 randomPlanesK0 n m

randomPlanesI0 :: Random.RandomGen g => (Plane, g) -> (Plane, g)
randomPlanesI0 (a,g) = let
 b = Matrix.toList a
 c = (b !! ((length b)-1)) / 2.0
 in (Matrix.fromList (map (\x -> x - c) b), g)

randomPlanesJ0 :: Maybe Point -> Bool
randomPlanesJ0 a = maybe False (\b -> any (\c -> c < -1.0 || c > 1.0) (Matrix.toList b)) a

randomPlanesK0 :: Int -> Int
randomPlanesK0 n = n

-- rerandomize some plane from some n tuple that does not intersect in Just
randomPlanesH1 :: Random.RandomGen g => Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesH1 n m = randomPlanesF randomPlanesI1 randomPlanesJ1 randomPlanesK1 n m

randomPlanesI1 :: Random.RandomGen g => (Plane, g) -> (Plane, g)
randomPlanesI1 (a,g) = let
 (b,h) = catalyze (\i -> Random.randomR (-100.0,100.0) i) g (Matrix.size a)
 in (Matrix.fromList b, h)

randomPlanesJ1 :: Maybe Point -> Bool
randomPlanesJ1 a = a == Nothing

randomPlanesK1 :: Int -> Int
randomPlanesK1 n = n

-- rerandomize some plane from some n+1 tuple that does intersect to Just
randomPlanesH2 :: Random.RandomGen g => Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesH2 n m = randomPlanesF randomPlanesI2 randomPlanesJ2 randomPlanesK2 n m

randomPlanesI2 :: Random.RandomGen g => (Plane, g) -> (Plane, g)
randomPlanesI2 = randomPlanesI1

randomPlanesJ2 :: Maybe Point -> Bool
randomPlanesJ2 a = a /= Nothing

randomPlanesK2 :: Int -> Int
randomPlanesK2 n = n + 1

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
 | b == index = -1.0
 | otherwise = let
  vector = w !! a
  scalar = Matrix.atIndex vector b
  origin = Matrix.atIndex vector index
  in scalar - origin where
 index = n - 1

intersectPlanesG :: [Plane] -> Int -> Int -> Double
intersectPlanesG w n a = negate (Matrix.atIndex (w !! a) (n - 1))

-- plane is z0, z1, ... zm. equation for plane is z = zm + x*(z0-zm) + y*(z1-zm) + ...
-- each row is z0-zm z1-zm ... -1 | -zm
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
spaceFromPlanes n w
 | m == 0 = []
 | n == 0 = replicate m [[0],[]]
 | otherwise = spaceFromPlanesF n m w where
 m = length w

spaceFromPlanesF :: Int -> Int -> [Plane] -> Space
spaceFromPlanesF n m w
 | m <= n = divideSpaceF (regionsOfSpace space) space
 | otherwise = let
  -- find intersection points on plane to add
  vertices = map welldef (subsets (n - 1) (indices (m - 1)))
  tuples = map (\x -> (m - 1):x) vertices
  points = map (\x -> fromJust (intersectPlanes n (subset x w))) tuples
  -- convert intersection points to sides in each permutation of sides wrt intersection planes
  perms = polyants (n - 1)
  sides = concat (map (spaceFromPlanesG perms w) (zip vertices points))
  -- convert sides to regions to divide
  divided = welldef (map (\x -> regionOfSides x space) sides)
  -- return space with found regions divided by new boundary
  in divideSpaceF divided space where
 planes = take (m - 1) w
 space = spaceFromPlanes n planes

spaceFromPlanesG :: [[Side]] -> [Plane] -> ([Int], Point) -> [[Side]]
spaceFromPlanesG s w (b, v) = map (spaceFromPlanesH v w 0 b) s

spaceFromPlanesH :: Point -> [Plane] -> Int -> [Int] -> [Side] -> [Side]
spaceFromPlanesH v (u:w) a (b:c) (s:t)
 | a == b = s:(spaceFromPlanesH v (u:w) (a+1) c t)
 | otherwise = (boolToInt (isAbovePlane v u)):(spaceFromPlanesH v w (a+1) (b:c) (s:t))
spaceFromPlanesH _ [] _ _ (s:t) = s:t
spaceFromPlanesH v (u:w) _ _ [] = (boolToInt (isAbovePlane v u)):(spaceFromPlanesH v w 0 [] [])
spaceFromPlanesH _ [] _ _ [] = []
spaceFromPlanesH _ _ _ _ _ = undefined

-- return planes with sidednesses as specified by given dimension and space
planesFromSpace :: Int -> Space -> [Plane]
planesFromSpace n s
 | n == 0 = replicate m (Matrix.vector [])
 | m <= n = take m (Matrix.toColumns (Matrix.ident n))
 | otherwise = let
  -- recurse with one fewer boundary
  bound = m - 1
  space = subSpaceF bound s
  planes = planesFromSpace n space
  -- find vertices
  vertices = subsets n (indices (length space))
  -- find sides of vertices wrt chosen boundary
  sides = map (\x -> vertexWrtBoundary bound x s) vertices
  mirror = map notOfInt sides
  -- interpret vertices as coplanes
  coplanes = map (\x -> fromJust (intersectPlanes n (subset x planes))) vertices
  -- convert coplanes to cospace with up-down sidedeness
  cospace = spaceFromPlanes n coplanes -- uses isAbovePlane for sidedness in cospace
  -- find sidesOfRegion of each coregion
  cosides = map (\x -> sidesOfRegion x cospace) (regionsOfSpace cospace)
  -- find cosides that matches vertices wrt chosen boundary
  [match] = filter (\x -> (x == sides) || (x == mirror)) cosides
  coregion = regionOfSides match cospace
  -- find copoint in coregion
  outin = outsideOfRegionExists coregion cospace
  inpoint = planesFromSpaceH n coregion cospace coplanes
  outpoint = planesFromSpaceI n coregion cospace coplanes inpoint
  copoint = if outin then outpoint else inpoint
  -- interpret copoint as plane to add
  in planes Prelude.++ [copoint] where
 m = length s

-- return average of corners of coregeion
planesFromSpaceH :: Int -> Region -> Space -> [Plane] -> Point
planesFromSpaceH n region space planes = let
 corners = attachedFacets n region space
 points = map (\x -> fromJust (intersectPlanes n (subset x planes))) corners
 zero = Matrix.fromList (replicate n 0.0)
 in Matrix.scale (1.0 / (fromIntegral (length points))) (fold' (\x y -> Matrix.add x y) points zero)

 -- find point some distance out on line to coregion from other outside
planesFromSpaceI :: Int -> Region -> Space -> [Plane] -> Point -> Point
planesFromSpaceI n r s p arrow = let
 feather = planesFromSpaceH n (outsideOfRegion r s) s p
 shaft = Matrix.add arrow (Matrix.scale (negate 1.0) feather)
 factor = 0.1 / (sqrt (Matrix.dot shaft shaft))
 in Matrix.add arrow (Matrix.scale factor shaft)
