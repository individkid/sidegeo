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
import Data.List (sort, sortBy, foldl', elemIndex, findIndex, find)
import Data.Maybe (fromJust)
import Data.Bits (xor, shift, testBit)
import qualified Numeric.LinearAlgebra as Matrix
import qualified System.Random as Random

newtype Boundary = Boundary Int deriving (Eq, Ord, Show) -- index into Space
newtype Region = Region Int deriving (Eq, Ord, Show) -- arbitrary identifier
newtype Side = Side Int deriving (Eq, Ord, Show) -- index into pair of halfspaces
type Space = [[[Region]]] -- assume equal covers
type Dual = [[[Boundary]]] -- now Boundary is arbitrary
type Place = [(Boundary,[[Region]])] -- assume one-to-one
type Plual = [(Region,[[Boundary]])] -- dual of place
type Part = [(Boundary,Side)]
class Perm p where
 refinePerm :: p -> [p]
 comparePerm :: p -> p -> Ordering
data Spacer = Spacer {
 doneOfSpacer :: Part,
 todoOfSpacer :: Part,
 origOfSpacer :: Dual,
 permOfSpacer :: Dual,
 sortOfSpacer :: Dual}
instance Perm Spacer where
 refinePerm = refineSpace
 comparePerm (Spacer {sortOfSpacer = s}) (Spacer {sortOfSpacer = t}) = compare s t
type Poly = [(Boundary,Side)] -- facet is intersection between subsection and polyant
type Tope = [(Poly,Poly)] -- polytope is map from facet to set of facet
data Toper = Toper {
 doneOfToper :: Part,
 todoOfToper :: Part,
 origOfToper :: Tope,
 permOfToper :: Tope,
 sortOfToper :: Tope}
instance Perm Toper where
 refinePerm = refineTope
 comparePerm (Toper {sortOfToper = s}) (Toper {sortOfToper = t}) = compare s t
type Plane = Matrix.Vector Double -- distances above base
type Point = Matrix.Vector Double -- coordinates
type Vector = Matrix.Vector Double

sideToBool :: Side -> Bool
sideToBool a = a /= (Side 0)

boolToSide :: Bool -> Side
boolToSide a = if a then Side 1 else Side 0

notOfSide :: Side -> Side
notOfSide a = boolToSide (not (sideToBool a))

xorOfSide :: Side -> Side -> Side
xorOfSide a b = boolToSide (xor (sideToBool a) (sideToBool b))

subsets :: Ord a => Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (a:b) = (map (a:) (subsets (pred n) b)) `append` (subsets n b)

power :: Ord a => [a] -> [[a]]
power a = let
 sizes = indices (length a)
 in fold' (\x y -> y ++ (subsets x a)) sizes []

subset :: [Int] -> [a] -> [a]
subset a b = image a (zip (indices (length b)) b)

indices :: Num a => Int -> [a]
indices n = take n (iterate (1+) 0)

unplace :: Int -> [a] -> [a]
unplace a b = (take a b) `append` (drop (succ a) b)

replace :: Int -> a -> [a] -> [a]
replace a b c = (take a c) `append` (b : (drop (succ a) c))

emplace :: Int -> a -> [a] -> [a]
emplace a b c = (take a c) `append` (b : (drop a c))

append :: [a] -> [a] -> [a]
append a b = fold' (:) (reverse a) b

choose :: [a] -> a
choose = head

holes :: Ord a => Num a => Int -> [a] -> [a]
holes n a = take n ((indices ((length a)+n)) \\ a)

image :: Eq a => [a] -> [(a,b)] -> [b]
image a m = range (filter (\(x,_) -> elem x a) m)

preimage :: Eq b => [b] -> [(a,b)] -> [a]
preimage b m = domain (filter (\(_,y) -> elem y b) m)

domain :: [(a,b)] -> [a]
domain m = map fst m

range :: [(a,b)] -> [b]
range m = map snd m

welldef :: Ord a => Ord b => [(a,b)] -> [(a,[b])]
welldef a = welldefF (checkSort a)

welldefF :: Eq a => [(a,b)] -> [(a,[b])]
welldefF ((a0,b0):(a1,b1):c2)
 | a0 == a1 = (a0,b0:b2):c3
 | otherwise = (a0,[b0]):(a2,b2):c3 where
 (a2,b2):c3 = welldefF ((a1,b1):c2)
welldefF [(a,b)] = [(a,[b])]
welldefF [] = []

-- ++ is as in Data.List except welldef
(++) :: Ord a => [a] -> [a] -> [a]
a ++ b = a `append` (b \\ a)

(\\) :: Ord a => [a] -> [a] -> [a]
a \\ b = slashSlashF (checkSort a) (checkSort b)

slashSlashF :: Ord a => [a] -> [a] -> [a]
slashSlashF (a:b) (c:d)
 | a > c = slashSlashF (a:b) d
 | a < c = a : (slashSlashF b (c:d))
 | a == c = slashSlashF b d
slashSlashF a _ = a

-- intersection
(+\) :: Ord a => [a] -> [a] -> [a]
a +\ b = a \\ (a \\ b)

-- all connected by given function to given start
generate :: Ord a => (a -> [a]) -> [a] -> [a]
generate f a = generateF f (head a) (tail a) []

generateF :: Ord a => (a -> [a]) -> a -> [a] -> [a] -> [a]
generateF f a todo done
 | null newTodo = newDone
 | otherwise = generateF f (head newTodo) (tail newTodo) newDone where
 newTodo = (((f a) \\ done) ++ todo) \\ [a]
 newDone = a : done

-- given number of firsts found by calling function on second
catalyze :: (g -> (a,g)) -> g -> Int -> ([a],g)
catalyze f g n = fold' (\_ (a,h) -> catalyzeF f h a) ((indices n)::[Int]) ([],g)

catalyzeF :: (g -> (a,g)) -> g -> [a] -> ([a],g)
catalyzeF f g a = let (b,h) = f g in (b:a,h)

-- call function for new result until it returns Nothing
foldMaybe :: (a -> b -> Maybe b) -> [a] -> b -> b
foldMaybe f (a:b) c = foldMaybeF f b c (f a c)
foldMaybe _ [] a = a

foldMaybeF :: (a -> b -> Maybe b) -> [a] -> b -> Maybe b -> b
foldMaybeF f (a:b) _ (Just c) = foldMaybeF f b c (f a c)
foldMaybeF _ _ a _ = a

-- call function until it returns Just
findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f (a:b) = findMaybeF f b (f a)
findMaybe _ [] = Nothing

findMaybeF :: (a -> Maybe b) -> [a] -> Maybe b -> Maybe b
findMaybeF _ _ (Just a) = Just a
findMaybeF f (a:b) Nothing = findMaybeF f b (f a)
findMaybeF _ [] Nothing = Nothing

checkSort :: Ord a => [a] -> [a]
checkSort a = if (checkSortF a) then a else sort a

checkSortF :: Ord a => [a] -> Bool
checkSortF (a:b:c)
 | a > b = False
 | otherwise = checkSortF (b:c)
checkSortF _ = True

-- modify function taking single to function taking list
fold' :: (a -> b -> b) -> [a] -> b -> b
fold' f a b = foldl' (\x y -> f y x) b a

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

map3 :: (a -> b) -> [[[a]]] -> [[[b]]]
map3 = map2 . map

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' a b = fromJust (elemIndex a b)

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' f a = fromJust (findIndex f a)

find' :: (a -> Bool) -> [a] -> a
find' f a = fromJust (find f a)

justIf :: Bool -> a -> Maybe a
justIf True a = Just a
justIf False _ = Nothing

nub' :: Ord a => [a] -> [a]
nub' a = nubF (checkSort a)

nubF :: Eq a => [a] -> [a]
nubF (a:b:c)
 | a == b = nubF (b:c)
 | otherwise = a:(nubF (b:c))
nubF a = a

figurate :: Int -> Int -> Int
figurate a b
 | a < 0 || b < 0 = undefined
 | b == 0 = 0
 | a == 0 = 1
 | otherwise = (figurate (a-1) b) + (figurate a (b-1))

--
-- now for something new
--

-- return how many regions a space of given dimension and boundaries has
defineLinear :: Int -> Int -> Int
defineLinear n m
 | (n < 0) || (m < 0) = undefined
 | (n == 0) || (m == 0) = 1
 | otherwise = (defineLinear n (pred m)) + (defineLinear (pred n) (pred m))

-- return whether all subspaces have correct number of regions
isLinear :: Int -> Space -> Bool
isLinear n s
 | n < 0 = undefined
 | (n == 0) || (null s) = (length (regionsOfSpace s)) == 1
 | n == 1 = let
  boundaries = length s
  regions = map Region (indices (succ boundaries))
  sample = map (isLinearG regions) (indices boundaries)
  in (equivSpace sample) == (equivSpace s)
 | otherwise = let
  bounds = boundariesOfPlace place
  subs = power bounds
  in all (\a -> isLinearF n place a) subs where
 place = spaceToPlace s

isLinearF :: Int -> Place -> [Boundary] -> Bool
isLinearF n s b = let
 subspace = fold' subSpace b s
 regions = regionsOfPlace subspace
 boundaries = boundariesOfPlace subspace
 in (defineLinear n (length boundaries)) == (length regions)

isLinearG :: [Region] -> Int -> [[Region]]
isLinearG regions index = let
 half = map Region (indices (succ index))
 other = regions \\ half
 in [half,other]

-- return all boundaries in space
boundariesOfSpace :: Space -> [Boundary]
boundariesOfSpace s = map Boundary (indices (length s))

boundariesOfDual :: Dual -> [Boundary]
boundariesOfDual [] = undefined
boundariesOfDual s = concat (head s)

boundariesOfPlace :: Place -> [Boundary]
boundariesOfPlace = domain

boundariesOfPlual :: Plual -> [Boundary]
boundariesOfPlual [] = undefined
boundariesOfPlual s = concat (snd (head s))

boundariesOfTope :: Tope -> [Boundary]
boundariesOfTope p = concat (map domain (domain p))

-- return all regions in space
regionsOfSpace :: Space -> [Region]
regionsOfSpace [] = [Region 0]
regionsOfSpace s = concat (head s)

regionsOfDual :: Dual -> [Region]
regionsOfDual s = map Region (indices (length s))

regionsOfPlace :: Place -> [Region]
regionsOfPlace [] = [Region 0]
regionsOfPlace s = concat (snd (head s))

regionsOfPlual :: Plual -> [Region]
regionsOfPlual = domain

-- side of region with regard to boundary
regionWrtBoundary :: Boundary -> Region -> Space -> Side
regionWrtBoundary (Boundary b) r s = Side (findIndex' (\a -> elem r a) (s !! b))

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
regionOfSidesF r s = fold' (\((Side b),c) a -> a +\ (c !! b)) (zip r s) (regionsOfSpace s)

-- return sidedness with boundaries reversed
oppositeOfSides :: [Boundary] -> [Side] -> [Side]
oppositeOfSides b r = fold' (\(Boundary x) y -> replace x (notOfSide (y !! x)) y) b r

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
 in map opposite (attachedBoundaries r s)

-- return boundaries attached to region
attachedBoundaries :: Region -> Space -> [Boundary]
attachedBoundaries r s = let
 opposite b = oppositeOfRegionExists [b] r s
 in filter opposite (boundariesOfSpace s)

-- return facets attached to region
attachedFacets :: Int -> Region -> Space -> [[Boundary]]
attachedFacets n r s = let
 opposite b = oppositeOfRegionExists b r s
 in filter opposite (subsets n (attachedBoundaries r s))

-- return regions in corners of boundaries
attachedRegions :: [Boundary] -> Space -> [Region]
attachedRegions b s = let
 place = spaceToPlace s
 in takeRegions (fold' sectionSpace b place) place

-- return corresponding outside region
outsideOfRegion :: Region -> Space -> Region
outsideOfRegion r s = oppositeOfRegion (boundariesOfSpace s) r s

-- return whether the region is an outside region
outsideOfRegionExists :: Region -> Space -> Bool
outsideOfRegionExists r s = oppositeOfRegionExists (boundariesOfSpace s) r s

-- return space of same dimension with given boundary removed
subSpace :: Boundary -> Place -> Place
subSpace = subSpaceF (flip (\\))

unsubSpace :: [Boundary] -> Place -> Place
unsubSpace = unsubSpaceF (flip (\\))

subSpaceF :: ([Region] -> [Region] -> [Region]) -> Boundary -> Place -> Place
subSpaceF f b s = let
 (bounds,space) = unzipPlace s
 regions = regionsOfSpace space
 index = elemIndex' b bounds
 bound = Boundary index
 section = filter (subSpaceG bound space) regions
 result = map2 (f section) (unplace index space)
 in zipPlace (unplace index bounds) result

unsubSpaceF :: ([Region] -> [Region] -> [Region]) -> [Boundary] -> Place -> Place
unsubSpaceF f b s = let
 bounds = boundariesOfPlace s
 unbounds = bounds \\ b
 in fold' (subSpaceF f) unbounds s

-- attachedRegions will not work here because of recursion
subSpaceG :: Boundary -> Space -> Region -> Bool
subSpaceG b s r = let
 x = (regionWrtBoundary b r s) == (Side 0)
 y = oppositeOfRegionExists [b] r s
 in x && y

-- return space of less dimension homeomorphic to regions attached to given boundary
sectionSpace :: Boundary -> Place -> Place
sectionSpace = subSpaceF (+\)

unsectionSpace :: [Boundary] -> Place -> Place
unsectionSpace = unsubSpaceF (+\)

-- divide regions of s in t by new boundary b
divideSpace :: Boundary -> Place -> Place -> [Place]
divideSpace b s t = let
 (bounds,space) = unzipPlace t
 place = zipPlace (b : bounds) (divideSpaceF (takeRegions s t) space)
 in [place, mirrorSpace b place]

-- return space with given regions divided by new boundary
divideSpaceF :: [Region] -> Space -> Space
divideSpaceF figure space = let
 whole = regionsOfSpace space
 ground = whole \\ figure
 halfspace = divideSpaceG ground space
 duplicates = regionHoles (length figure) whole
 mapping = zip figure duplicates
 withDups = map2 (divideSpaceH mapping figure) space
 newBoundary = [halfspace ++ duplicates, whole \\ halfspace]
 in newBoundary : withDups

divideSpaceG :: [Region] -> Space -> [Region]
divideSpaceG [] _ = []
divideSpaceG r s = generate (\a -> r +\ (oppositesOfRegion a s)) r

divideSpaceH :: [(Region,Region)] -> [Region] -> [Region] -> [Region]
divideSpaceH m a b = (image (a +\ b) m) ++ b

-- return space with given region changed to its local opposite
migrateSpace :: Region -> Space -> Space
migrateSpace r s = map (migrateSpaceF (attachedBoundaries r s) r) (spaceToPlace s)

migrateSpaceF :: [Boundary] -> Region -> (Boundary,[[Region]]) -> [[Region]]
migrateSpaceF b r (a,s)
 | (elem a b) && (elem r (s !! 0)) = [((s !! 0) \\ [r]), (r : (s !! 1))]
 | (elem a b) = [(r : (s !! 0)), ((s !! 1) \\ [r])]
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

-- return regions in second homeomorphic to regions in first
takeRegions :: Place -> Place -> [Region]
takeRegions s t = let
 shared = (boundariesOfPlace s) +\ (boundariesOfPlace t)
 plual = placeToPlual t
 (regions,dual) = unzipPlual plual
 sSub = map2 (\x -> checkSort (shared +\ x)) (placeToDual s)
 tSub = map2 (\x -> checkSort (shared +\ x)) dual
 in preimage sSub (zipPlual regions tSub)

-- reverse sidedness of given boundary
mirrorSpace :: Boundary -> Place -> Place
mirrorSpace b s = map (\(x,[y,z]) -> if x == b then (x,[z,y]) else (x,[y,z])) s

-- remove region to produce non-linear space
degenSpace :: Region -> Place -> Place
degenSpace r s = let
 (bounds,space) = unzipPlace s
 in zipPlace bounds (map2 (\x -> x \\ [r]) space)

embedSpace :: [Region] -> Place -> Place
embedSpace r s = let
 regions = regionsOfPlace s
 unregions = regions \\ r
 in fold' degenSpace unregions s

-- all possible regions
powerSpace :: [Boundary] -> Place
powerSpace b = let
 count = length b
 regions = indices (shift 1 count)
 boundaries = indices count
 in map (\(x,y) -> (y, [powerSpaceF x regions 0, powerSpaceF x regions 1])) (zip boundaries b)

-- regions indicated by boundary as bit position
powerSpaceF :: Int -> [Int] -> Int -> [Region]
powerSpaceF b r s = map Region (filter (\y -> (boolToSide (testBit y b)) == (Side s)) r)

-- divide regions by space into non-linear space
crossSpace :: Place -> Place -> Place
crossSpace s t = let
 (sBounds,sSpace) = unzipPlace s
 (tBounds,tSpace) = unzipPlace t
 sRegions = regionsOfSpace sSpace
 tRegions = regionsOfSpace tSpace
 sPairs x = [(p,q) | p <- x, q <- tRegions]
 tPairs x = [(p,q) | p <- sRegions, q <- x]
 pairs = [(x,y) | x <- sRegions, y <- tRegions]
 regions = regionHoles (length pairs) []
 mapping = zip regions pairs
 sCross = map2 (\x -> preimage (sPairs x) mapping) sSpace
 tCross = map2 (\x -> preimage (tPairs x) mapping) tSpace
 in (zipPlace sBounds sCross) ++ (zipPlace tBounds tCross)

-- each dual region of superspace is superset of some dual region of subspace
isSubSpace :: Place -> Place -> Bool
isSubSpace s t = let
 a = null ((boundariesOfPlace s) \\ (boundariesOfPlace t))
 b = (length (takeRegions t s)) == (length (regionsOfSpace (placeToSpace s)))
 c = (length (takeRegions s t)) == (length (regionsOfSpace (placeToSpace t)))
 in a && b && c

-- subset is section space in dual representation
isSectionSpace :: Place -> Place -> Bool
isSectionSpace s t = let
 a = null ((boundariesOfPlace s) \\ (boundariesOfPlace t))
 b = null ((boundariesOfPlace t) \\ (boundariesOfPlace s))
 c = (length (takeRegions s t)) == (length (regionsOfSpace (placeToSpace s)))
 in a && b && c

-- representation converters
plualToPlace :: Plual -> Place
plualToPlace s = let
 bounds = boundariesOfDual (plualToDual s)
 left = map (\x -> (x,domain (filter (\(_,[y,_]) -> elem x y) s))) bounds
 right = map (\x -> domain (filter (\(_,[_,y]) -> elem x y) s)) bounds
 in map (\((x,y),z) -> (x,[y,z])) (zip left right)

placeToPlual :: Place -> Plual
placeToPlual s = let
 regions = regionsOfSpace (placeToSpace s)
 left = map (\x -> (x,domain (filter (\(_,[y,_]) -> elem x y) s))) regions
 right = map (\x -> domain (filter (\(_,[_,y]) -> elem x y) s)) regions
 in map (\((x,y),z) -> (x,[y,z])) (zip left right)

placeToDual :: Place -> Dual
placeToDual = plualToDual . placeToPlual

dualToPlace :: Dual -> Place
dualToPlace = plualToPlace . dualToPlual

dualToSpace :: Dual -> Space
dualToSpace = placeToSpace . plualToPlace . dualToPlual

spaceToDual :: Space -> Dual
spaceToDual = plualToDual . placeToPlual . spaceToPlace

spaceToPlace :: Space -> Place
spaceToPlace s = zip (boundaryHoles (length s) []) s

dualToPlual :: Dual -> Plual
dualToPlual s = zip (regionHoles (length s) []) s

placeToSpace :: Place -> Space
placeToSpace = range

plualToDual :: Plual -> Dual
plualToDual = range

unzipPlace :: Place -> ([Boundary],Space)
unzipPlace = unzip

unzipPlual :: Plual -> ([Region],Dual)
unzipPlual = unzip

zipPlace :: [Boundary] -> Space -> Place
zipPlace = zip

zipPlual :: [Region] -> Dual -> Plual
zipPlual = zip

unzipBoundary :: Boundary -> [Boundary] -> Boundary
unzipBoundary a b = Boundary (elemIndex' a b)

unzipBoundaries :: [Boundary] -> [Boundary] -> [Boundary]
unzipBoundaries a b = map (\x -> unzipBoundary x b) a

zipBoundary :: Boundary -> [Boundary] -> Boundary
zipBoundary (Boundary a) b = b !! a

zipBoundaries :: [Boundary] -> [Boundary] -> [Boundary]
zipBoundaries a b = map (\x -> zipBoundary x b) a

pointToVector :: Point -> Vector
pointToVector v = v

vectorToPoint :: Vector -> Point
vectorToPoint v = v

planeToVector :: Plane -> Vector
planeToVector w = w

vectorToPlane :: Vector -> Plane
vectorToPlane w = w

boundaryHoles :: Int -> [Boundary] -> [Boundary]
boundaryHoles m b = map Boundary (holes m (map (\(Boundary x) -> x) b))

regionHoles :: Int -> [Region] -> [Region]
regionHoles m r = map Region (holes m (map (\(Region x) -> x) r))

allSides :: [Side]
allSides = map Side (indices 2)

--
-- so far so simple
--

-- return space by calling superSpace with singleton space
anySpace :: Int -> Int -> Space
anySpace n m = let
 bounds = boundaryHoles m []
 in placeToSpace (fold' (\x y -> superSpace n y (powerSpace [x])) bounds [])

-- return all linear spaces given any space to start
allSpace :: Space -> [Space]
allSpace s = let
 space = equivSpace s
 regions = regionsOfSpace space
 in allSpaceF regions space [] []

-- migrate all possible from current space, and go on to next todo
allSpaceF :: [Region] -> Space -> [Space] -> [Space] -> [Space]
allSpaceF (p:q) s todo done
 | migrateSpaceExists p s = allSpaceG q s (equivSpace (migrateSpace p s)) todo done
 | otherwise = allSpaceF q s todo done
allSpaceF [] s todo done = allSpaceH todo (s : done)

-- if migration not already done or todo, recurse with migration added to todo
allSpaceG :: [Region] -> Space -> Space -> [Space] -> [Space] -> [Space]
allSpaceG r s t todo done
 | (s == t) || (elem t todo) || (elem t done) = allSpaceF r s todo done
 | otherwise = allSpaceF r s (t : todo) done

-- recurse with choice removed from todo
allSpaceH :: [Space] -> [Space] -> [Space]
allSpaceH (s:todo) done = allSpaceF (regionsOfSpace s) s todo done
allSpaceH [] done = done

-- return superspace with given spaces as subspaces
-- given spaces have given dimension. subspaces by shared boundaries are equal
superSpace :: Int -> Place -> Place -> Place
superSpace n s t
 | n < 0 = undefined
 | (n == 0) && (shared /= bounds) = undefined
 | (length tOnly) == 0 = s
 | (length sOnly) == 0 = t
 | (length bounds) <= n = powerSpace bounds
 | ((length shared) > 0) && ((length sOnly) == 1) && ((length tOnly) > 1) = superSpace n t s
 | ((length shared) > 0) && ((length sOnly) > 1) = let
  bound = choose sOnly
  sup = superSpace n (subSpace bound s) t -- sOnly gets smaller because it is missing bound
  in superSpace n s sup -- sOnly gets smaller because it goes from more than bound to only bound
 | ((length shared) == 0) && ((length sBounds) == 1) && ((length tBounds) > 1) = superSpace n t s
 | ((length shared) == 0) && ((length sBounds) > 1) = let
  bound = choose sBounds
  sup = superSpace n (powerSpace [bound]) t -- sBounds becomes just bound from more than bound 
  in superSpace n s sup -- s and sup share bound
 | ((length sOnly) == 1) && ((length tOnly) == 1) = rabbitSpace n s t -- independent boundary case
 | otherwise = undefined where
 sBounds = boundariesOfPlace s
 tBounds = boundariesOfPlace t
 sOnly = sBounds \\ tBounds
 tOnly = tBounds \\ sBounds
 shared = sBounds +\ tBounds
 bounds = sBounds ++ tBounds

-- cases where sOnly and tOnly are singletons
-- like superSpace except one and only one unshared boundary in each space
rabbitSpace :: Int -> Place -> Place -> Place
rabbitSpace n s t
 | n >= 2 = let
  sSect = sectionSpace sBound s
  tSect = sectionSpace tBound t
  sect = snakeSpace (pred n) (pred n) n sSect tSect place
  sSup = divideSpace tBound sect sSect 
  tSup = concatMap (\x -> divideSpace sBound x t) sSup
  in choose (filter (rabbitSpaceF n s t) tSup)
 | n == 1 = let
  -- must sort boundary sets because checkSort is shallow
  sDual = map2 sort (placeToDual (crossSpace s (powerSpace [tBound])))
  tDual = map2 sort (placeToDual (crossSpace t (powerSpace [sBound])))
  double = powerSpace [sBound, tBound]
  space = placeToSpace double
  quad = regionsOfPlace double
  -- regions = quad
  regions = filter (\x -> not (outsideOfRegionExists x space)) quad
  cross = map (\x -> crossSpace place (degenSpace x double)) regions
  dual = map (\x -> map2 sort (placeToDual x)) cross
  -- cross-spaces are from each perspective; intersection is all perspectives
  result = map (\x -> dualToPlace (sDual +\ tDual +\ x)) dual
  in choose result
  -- in choose (filter (rabbitSpaceF n s t) result)
 | otherwise = undefined where
 sBounds = boundariesOfPlace s
 tBounds = boundariesOfPlace t
 [sBound] = sBounds \\ tBounds
 [tBound] = tBounds \\ sBounds
 place = subSpace sBound s

-- return whether linear and contains both given
rabbitSpaceF :: Int -> Place -> Place -> Place -> Bool
rabbitSpaceF n s t u = (isLinear n (placeToSpace u)) && (isSubSpace u s) && (isSubSpace u t)

-- return space that is section of given spaces
-- given dimensions correspond to given spaces. first two spaces are sections of third place
snakeSpace :: Int -> Int -> Int -> Place -> Place -> Place -> Place
snakeSpace p q n s t u
 | (p > n) || (q > n) || (n < 0) || (dim < 0) = undefined
 | p == n = t
 | q == n = s
 | dim == 0 = let
  regions = takeRegions s t
  region = choose regions
  in embedSpace [region] t
 | dim >= (length u) = powerSpace (boundariesOfPlace u)
 | dim > 0 = let
  bounds = boundariesOfPlace u
  bound = choose bounds
  sSub = subSpace bound s
  tSub = subSpace bound t
  uSub = subSpace bound u
  uSect = sectionSpace bound u
  sub = snakeSpace p q n sSub tSub uSub
  sect = snakeSpace dim (pred n) n sub uSect uSub
  result = divideSpace bound sect sub
  in choose (filter (snakeSpaceF s t) result)
 | otherwise = undefined where
 dim = (p+q)-n

-- return whether section of both
snakeSpaceF :: Place -> Place -> Place -> Bool
snakeSpaceF s t u = (isSectionSpace u s) && (isSectionSpace u t)

-- optimize this
equivPerm :: Perm p => p -> p
equivPerm p = equivPermF p (refinePerm p)

-- refine until no more refinements
equivPermF :: Perm p => p -> [p] -> p
equivPermF p [] = p
equivPermF _ p = let
 sorted = sortBy comparePerm p
 sample = head sorted
 equal = (EQ ==) . (comparePerm sample)
 prefix = takeWhile equal sorted
 refine = concatMap refinePerm prefix
 in equivPermF (choose prefix) refine

-- change to, change from, whether mirrored, done partial, todo partial
refinePart :: Part -> Part -> [(Boundary,Boundary,Side,Part,Part)]
refinePart _ [] = []
refinePart p q = let
 term = Boundary (length p)
 done (x,y) = p `append` [(x,y)]
 todo x = filter (\(y,_) -> x /= y) q
 tuple (x,y) = (term, x, y, done (x,y), todo x)
 in map tuple q

-- starting partial permutation
origPart :: [Boundary] -> Part
origPart b = [(x,y) | x <- b, y <- allSides]

-- find representative of the equivalence class of the given space
equivSpace :: Space -> Space
equivSpace s = let
 orig = spaceToDual s
 bounds = boundariesOfDual orig
 (Boundary bound) = maximum bounds
 term = Boundary (succ bound)
 part = origPart bounds
 dummy = replicate (length orig) [[term],[term]]
 spacer = Spacer {
  doneOfSpacer = [],
  todoOfSpacer = part,
  origOfSpacer = orig,
  permOfSpacer = dummy,
  sortOfSpacer = dummy}
 termed = sortOfSpacer (equivPerm spacer)
 strip x = take (pred (length x)) x
 dual = map2 strip termed
 in dualToSpace dual

-- permutation instance of space
refineSpace :: Spacer -> [Spacer]
refineSpace (Spacer {
 doneOfSpacer = p,
 todoOfSpacer = q,
 origOfSpacer = s,
 permOfSpacer = t}) = map (refineSpaceF s t) (refinePart p q)

-- add transposed boundary to fullspaces
refineSpaceF :: Dual -> Dual -> (Boundary,Boundary,Side,Part,Part) -> Spacer
refineSpaceF s t (a,b,c,p,q) = let
 refine = map (refineSpaceG a b c) (zip s t)
 sorted = sort (map (map sort) refine)
 in Spacer {
  doneOfSpacer = p,
  todoOfSpacer = q,
  origOfSpacer = s,
  permOfSpacer = refine,
  sortOfSpacer = sorted}

-- add transposed boundary to fullspace
refineSpaceG :: Boundary -> Boundary -> Side -> ([[Boundary]],[[Boundary]]) -> [[Boundary]]
refineSpaceG a b (Side 0) ([s,_],[u,v])
 | elem b s = [a:u,v]
 | otherwise = [u,a:v]
refineSpaceG a b (Side 1) ([s,_],[u,v])
 | elem b s = [u,a:v]
 | otherwise = [a:u,v]
refineSpaceG _ _ _ _ = undefined

--
-- between space and polytope
--

equivTope :: Tope -> Tope
equivTope s = let
 bounds = boundariesOfTope s
 (Boundary bound) = maximum bounds
 term = Boundary (succ bound)
 part = origPart bounds
 dummy = equivTopeF term s
 toper = Toper {
  doneOfToper = [],
  todoOfToper = part,
  origOfToper = s,
  permOfToper = dummy,
  sortOfToper = dummy}
 in sortOfToper (equivPerm toper)

equivTopeF :: Boundary -> Tope -> Tope
equivTopeF b p = map (\(x,y) -> (equivTopeG b x, equivTopeG b y)) p

equivTopeG :: Boundary -> Poly -> Poly
equivTopeG b p = map (\(_,x) -> (b,x)) p

refineTope :: Toper -> [Toper]
refineTope (Toper {
 doneOfToper = p,
 todoOfToper = q,
 origOfToper = s,
 permOfToper = t}) = map (refineTopeF s t) (refinePart p q)

refineTopeF :: Tope -> Tope -> (Boundary,Boundary,Side,Part,Part) -> Toper
refineTopeF p q (a,b,c,d,e) = let
 tope = zipWith (refineTopeG a b c) p q
 sorted = sort (map (\(x,y) -> (sort x, sort y)) tope)
 in Toper {
  doneOfToper = d,
  todoOfToper = e,
  origOfToper = p,
  permOfToper = tope,
  sortOfToper = sorted}

refineTopeG :: Boundary -> Boundary -> Side -> (Poly,Poly) -> (Poly,Poly) -> (Poly,Poly)
refineTopeG a b c (p,k) (q,l) = (refineTopeH a b c p q, refineTopeH a b c k l)

refineTopeH :: Boundary -> Boundary -> Side -> Poly -> Poly -> Poly
refineTopeH a b c p q = zipWith (refineTopeI a b c) p q

refineTopeI :: Boundary -> Boundary -> Side -> (Boundary,Side) -> (Boundary,Side) -> (Boundary,Side)
refineTopeI a b c (p,k) (q,l)
 | b == p = (a, xorOfSide c k)
 | otherwise = (q,l)

-- classify embedding with local invariance
topeFromSpace :: Int -> [Region] -> Place -> Tope
topeFromSpace n r s
 | n == 0 = let
  [region] = r
  [(bound,[half,_])] = s
  side = if elem region half then Side 0 else Side 1
  in [([(bound,side)],[])]
 | otherwise = let
  bounds = boundariesOfPlace s
  sides = allSides
  polys = [(x,y) | x <- bounds, y <- sides]
  facets = nub' (concat (map (topeFromSpaceF n r s) polys))
  faces = filter (\(x,_) -> (length x) == 1) facets
  poly = concat (map (\(x,_) -> x) faces)
  in ([],poly):facets

-- recurse with surface regions and add Poly element to Tope domain elements
topeFromSpaceF :: Int -> [Region] -> Place -> (Boundary,Side) -> Tope
topeFromSpaceF n r p (b,s) = let
 sect = sectionSpace b p
 surface = filter (topeFromSpaceG b s p r) (regionsOfPlace p)
 taken = takeRegions (embedSpace surface p) sect
 tope = topeFromSpace (pred n) taken sect
 in sort (map (\(x,y) -> (sort ((b,s):x), y)) tope)

-- test whether region is surface
topeFromSpaceG :: Boundary -> Side -> Place -> [Region] -> Region -> Bool
topeFromSpaceG b s p q r = let
 (bounds,space) = unzipPlace p
 bound = unzipBoundary b bounds
 side = regionWrtBoundary bound r space
 neighbor = oppositeOfRegion [bound] r space
 in (side == s) && (elem neighbor q)

-- find sample space that polytope could be embedded in
spaceFromTope :: Int -> Tope -> Place
spaceFromTope n t = let
 bounds = boundariesOfTope t
 spaces = allSpace (anySpace n (length bounds))
 places = map spaceToPlace spaces
 in choose (filter (spaceFromTopeF n t) places)

-- return whether place obeys sidednesses implied by tope
spaceFromTopeF :: Int -> Tope -> Place -> Bool
spaceFromTopeF n tope place
 | n < 2 = let
  inside = spaceFromTopeH (==) tope place
  outside = spaceFromTopeH (/=) tope place
  in ((inside +\ outside) == []) && ((((regionsOfPlace place) \\ inside) \\ outside) == [])
 | otherwise = let
  sections = [(x,y) | x <- boundariesOfPlace place, y <- allSides]
  sectionplace x = sectionSpace x place
  sectiontope x y = spaceFromTopeG x y tope
  in all (\(x,y) -> spaceFromTopeF (n-1) (sectiontope x y) (sectionplace x)) sections

-- find section tope wrt boundary
spaceFromTopeG :: Boundary -> Side -> Tope -> Tope
spaceFromTopeG boundary side tope = let
 branch = filter (\(x,_) -> any (\(y,z) -> (y,z) == (boundary,side)) x) tope
 in map (\(x,y) -> (filter (\(z,_) -> z /= boundary) x, y)) branch

-- find inside or outside regions of one dimensional polytope
spaceFromTopeH :: (Side -> Side -> Bool) -> Tope -> Place -> [Region]
spaceFromTopeH equal tope place = let
 boundaries = boundariesOfTope tope
 subplace = unsubSpace boundaries place
 (bounds,space) = unzipPlace subplace
 [([],mapping)] = tope
 func :: Boundary -> Side -> Region -> Bool
 func x y z = equal (regionWrtBoundary x z space) y
 filterer :: Boundary -> Side -> [Region]
 filterer x y = filter (\z -> func x y z) (attachedRegions [x] space)
 in fold' (\v w -> v ++ w) (map (\(x,y) -> filterer (unzipBoundary x bounds) y) mapping) []

-- find regions attached to top-level facets
topeRegions :: Int -> Tope -> Place -> [Region]
topeRegions n tope place
 | n < 2 = spaceFromTopeH (==) tope place
 | otherwise = let
  boundary = boundariesOfPlace place
  mappings = map (topeRegionsF n tope place) (zip boundary allSides)
  mapping = welldef (concat mappings)
  regions = concat (map domain mappings)
  in generate (topeRegionsG mapping place) regions

-- take section by boundary and map the taken regions to the boundary
topeRegionsF :: Int -> Tope -> Place -> (Boundary,Side) -> [(Region,Boundary)]
topeRegionsF n tope place (boundary,side) = let
 sectionplace = sectionSpace boundary place
 sectiontope = spaceFromTopeG boundary side tope
 regions = topeRegions (n-1) sectiontope sectionplace
 taken = takeRegions (embedSpace regions sectionplace) place
 space = placeToSpace place
 sided = filter (\x -> (regionWrtBoundary boundary x space) == side) taken
 in zip sided (repeat boundary)

-- return neighbors wrt attached boundaries except image
topeRegionsG :: [(Region,[Boundary])] -> Place -> Region -> [Region]
topeRegionsG mapping place region = let
 (boundaries,space) = unzipPlace place
 walls = unzipBoundaries (concat (image [region] mapping)) boundaries
 room = attachedBoundaries region space
 doors = room \\ walls
 in map (\x -> oppositeOfRegion [x] region space) doors

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
randomPlanesG f (a,g) b = let
 (c,h) = Random.randomR (0, pred (length b)) g
 d = b !! c
 (e,i) = f (a !! d, h)
 in (replace d e a, i)

-- shift by half to origin some plane from some n tuple that intersects outside -1.0 to 1.0 hypercube
randomPlanesH0 :: Random.RandomGen g => Int -> Int -> ([Plane], g) -> Maybe ([Plane], g)
randomPlanesH0 n m = randomPlanesF randomPlanesI0 randomPlanesJ0 randomPlanesK0 n m

randomPlanesI0 :: Random.RandomGen g => (Plane, g) -> (Plane, g)
randomPlanesI0 (a,g) = let
 b = Matrix.toList a
 c = (b !! (pred (length b))) / 2.0
 in (Matrix.fromList (map (\x -> x - c) b), g)

randomPlanesJ0 :: Maybe Point -> Bool
randomPlanesJ0 a = maybe False (\b -> any (\c -> c < (-1.0) || c > 1.0) (Matrix.toList b)) a

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
randomPlanesK2 n = succ n

solveVectors :: Int -> [Vector] -> (Int -> [Vector] -> Int -> Int -> Double) -> (Int -> [Vector] -> Int -> Double) -> Maybe Vector
solveVectors n w f g = let
 first = solveVectorsF n w f g
 -- return Nothing if not every n-tuple solves to same point
 points = map (\a -> solveVectorsF n a f g) (subsets n w)
 same = maybe False (\a -> all (\b -> maybe False (\c -> solveVectorsG a c) b) points) first
 in if same then first else Nothing

solveVectorsF :: Int -> [Vector] -> (Int -> [Vector] -> Int -> Int -> Double) -> (Int -> [Vector] -> Int -> Double) -> Maybe Vector
solveVectorsF n w f g = let
 lhs = Matrix.matrix n [f n w a b | a <- (indices n), b <- (indices n)]
 rhs = Matrix.matrix 1 [g n w a | a <- (indices n)]
 in fmap Matrix.flatten (Matrix.linearSolve lhs rhs)

solveVectorsG :: Vector -> Vector -> Bool
solveVectorsG a b = let
 c = Matrix.add a (Matrix.scale (-1.0) b)
 in (Matrix.dot c c) < 0.001

-- plane is z0, z1, ... zm. equation for plane is z = zm + x*(z0-zm) + y*(z1-zm) + ...
-- each row is z0-zm z1-zm ... -1 | -zm
intersectPlanes :: Int -> [Plane] -> Maybe Point
intersectPlanes n w = fmap vectorToPoint (solveVectors n (map planeToVector w) intersectPlanesF intersectPlanesG)

intersectPlanesF :: Int -> [Vector] -> Int -> Int -> Double
intersectPlanesF n w a b
 | b == index = (-1.0)
 | otherwise = (list !! b) - (list !! index) where
 index = pred n
 list = Matrix.toList (w !! a)

intersectPlanesG :: Int -> [Vector] -> Int -> Double
intersectPlanesG n w a = negate ((Matrix.toList (w !! a)) !! (pred n))

-- z0 = hm + x0*(h0-hm) + y0*(h1-hm) + ...
-- z0 = h0*x0 + h1*y0 + ... + hm*(1-x0-y0-...)
constructPlane :: Int -> [Point] -> Maybe Plane
constructPlane n v = fmap vectorToPlane (solveVectors n (map pointToVector v) constructPlaneF constructPlaneG)

constructPlaneF :: Int -> [Vector] -> Int -> Int -> Double
constructPlaneF n v a b
 | b == index = 1.0 - (fold' (+) (take index list) 0.0)
 | otherwise = list !! b where
 index = pred n
 list = Matrix.toList (v !! a)

constructPlaneG :: Int -> [Vector] -> Int -> Double
constructPlaneG n v a = (Matrix.toList (v !! a)) !! (pred n)

isAbovePlane :: Point -> Plane -> Bool
isAbovePlane v w = let
 pointL = Matrix.toList v
 planeL = Matrix.toList w
 pointS = last pointL
 planeS = last planeL
 pointT = take (pred (length pointL)) pointL
 planeT = take (pred (length planeL)) planeL
 pointV = Matrix.fromList pointT
 planeV = Matrix.fromList (map (\x -> x - planeS) planeT)
 in pointS > ((Matrix.dot planeV pointV) + planeS)

-- return space with sidednesses determined by given planes
spaceFromPlanes :: Int -> [Plane] -> Space
spaceFromPlanes n w
 | m == 0 = []
 | n == 0 = replicate m [[Region 0],[]]
 | m <= n = placeToSpace (powerSpace (boundaryHoles m []))
 | n == 1 = let
  vector = map planeToVector w
  scalar = map (\x -> head (Matrix.toList x)) vector
  sorted = sortBy compare scalar
  index = map (\x -> elemIndex' x sorted) scalar
  left = map (\x -> indices (succ x)) index
  right = map (\x -> map ((succ x)+) (indices (m-x))) index
  full = map (\(x,y) -> [x,y]) (zip left right)
  in map3 Region full
 | otherwise = let
  index = pred m
  plane = last w
  planes = take index w
  indexes = indices index
  bounds = map Boundary indexes
  space = spaceFromPlanes n planes -- assume one-to-one between space and planes
  place = zipPlace bounds space
  sample = take n planes
  vertex = take n bounds
  -- find intersection points on plane to add
  tuples = subsets (pred n) indexes
  vertices = map (\x -> plane:(subset x planes)) tuples
  points = map (\x -> fromJust (intersectPlanes n x)) vertices
  -- convert intersection points to sides and take to space
  keep = concatMap (spaceFromPlanesF planes place) (zip tuples points)
  sect = embedSpace keep place -- convert regions to place
  -- return space with found regions divided by new boundary
  [bound] = boundaryHoles 1 bounds
  [one,other] = divideSpace bound sect place
  point = fromJust (intersectPlanes n sample)
  side = vertexWrtBoundary bound vertex (placeToSpace one)
  valid = (sideToBool side) == (isAbovePlane point plane)
  result = if valid then one else other
  in placeToSpace result where
 m = length w

-- return super-region containing point that is intersection of planes with associated indices
-- assume place has sidedness equal to isAbovePlane of corresponding plane
spaceFromPlanesF :: [Plane] -> Place -> ([Int], Point) -> [Region]
spaceFromPlanesF w s (b, v) = let
 planes = fold' (\x y -> unplace x y) b w
 bounds = (boundariesOfPlace s) \\ (map Boundary b)
 degen = map (\x -> if isAbovePlane v x then [[],[Region 0]] else [[Region 0],[]]) planes
 in takeRegions (zipPlace bounds degen) s

-- return planes with sidednesses as specified by given dimension and space
planesFromSpace :: Int -> Space -> [Plane]
planesFromSpace n s
 | n == 0 = replicate m (Matrix.vector [])
 | m <= n = take m (Matrix.toColumns (Matrix.ident n))
 | n == 1 = let
  [[[region],_]] = filter (\[x,_] -> (length x) == 1) s
  half = map (\[x,y] -> if elem region x then x else y) s
  scalar = map (\x -> 1.0 * (fromIntegral (length x))) half
  vector = map (\x -> Matrix.fromList [x]) scalar
  in map vectorToPlane vector
 | otherwise = let
  index = pred m
  bound = Boundary index
  -- recurse with one fewer boundary
  place = zipPlace (map Boundary (indices m)) s
  sub = subSpace bound place
  space = placeToSpace sub
  planes = planesFromSpace n space
  -- find vertices
  indexes = indices index
  tuples = subsets n indexes
  vertices = map2 Boundary tuples
  -- find sides of vertices wrt chosen boundary
  sides = map (\x -> vertexWrtBoundary bound x s) vertices
  mirror = map notOfSide sides
  -- construct plane through vertex planes interpreted as copoints
  interpret = map (\x -> subset x planes) tuples
  copoints = map2 (\x -> vectorToPoint (planeToVector x)) interpret
  coplanes = map (\x -> fromJust (constructPlane n x)) copoints
  -- convert coplanes to cospace with up-down sidedeness
  cospace = spaceFromPlanes n coplanes -- uses isAbovePlane for sidedness in cospace
  -- find cosides that matches vertices wrt chosen boundary
  valid = regionOfSidesExists sides cospace
  cosides = if valid then sides else mirror
  coregion = regionOfSides cosides cospace
  -- find copoint in coregion
  copoint = planesFromSpaceG n coregion cospace coplanes
  -- interpret copoint as plane to add
  in planes ++ [vectorToPlane . pointToVector $ copoint] where
 m = length s

-- return average of corners of coregeion
planesFromSpaceF :: Int -> Region -> Space -> [Plane] -> Point
planesFromSpaceF n region space planes = let
 corners = attachedFacets n region space
 indexes = map2 (\(Boundary x) -> x) corners
 numeric = map (\x -> subset x planes) indexes
 points = map (\x -> fromJust (intersectPlanes n x)) numeric
 zero = Matrix.fromList (replicate n 0.0)
 factor = 1.0 / (fromIntegral (length points))
 total = fold' (\x y -> Matrix.add x y) points zero
 in Matrix.scale factor total

-- if outside, find point some distance out on line to coregion from other outside
planesFromSpaceG :: Int -> Region -> Space -> [Plane] -> Point
planesFromSpaceG n r s p
 | outsideOfRegionExists r s = let
  feather = planesFromSpaceF n (outsideOfRegion r s) s p
  shaft = Matrix.add arrow (Matrix.scale (-1.0) feather)
  factor = 0.1 / (sqrt (Matrix.dot shaft shaft))
  in Matrix.add arrow (Matrix.scale factor shaft)
 | otherwise = arrow where
 arrow = planesFromSpaceF n r s p
