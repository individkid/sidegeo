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
type Dual = [[[Boundary]]]
type Place = [(Boundary,Full)] -- assume one-to-one
type Plane = Matrix.Vector Double -- single column of distances above base
type Point = Matrix.Vector Double -- single column of coordinates
type Pack = Int -- bits indicate membership

intToBool :: Int -> Bool
intToBool a = a /= 0

boolToInt :: Bool -> Int
boolToInt a = if a then 1 else 0

notOfInt :: Int -> Int
notOfInt a = boolToInt (not (intToBool a))

packToBools :: Int -> Pack -> [Bool]
packToBools a b = map (\x -> belongs x b) (indices a)

boolsToPack :: [Bool] -> Pack
boolsToPack a = foldr (\x y -> (shift y 1) + (boolToInt x)) 0 a

-- all subsets of non-negative Int less than given
power :: Int -> [Pack]
power a = indices (shift 1 a)

polybools :: Int -> [[Bool]]
polybools a = map (packToBools a) (power a)

polyants :: Int -> [[Side]]
polyants a = map (map boolToInt) (polybools a)

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
subset p a = foldr (\q b -> (a !! q) : b) [] p

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
a ++ b = setF a b True True True

(\\) :: Ord a => [a] -> [a] -> [a]
a \\ b = setF a b True False False

-- intersection
(+\) :: Ord a => [a] -> [a] -> [a]
a +\ b = setF a b False True False

-- symmetric difference
(\+) :: Ord a => [a] -> [a] -> [a]
a \+ b = setF a b True False True

setF :: Ord a => [a] -> [a] -> Bool -> Bool -> Bool -> [a]
setF a b c d e = setG (welldef a) (welldef b) c d e

setG :: Ord a => [a] -> [a] -> Bool -> Bool -> Bool -> [a]
setG a _ True True False = a
setG _ b False True True = b
setG _ _ False False False = []
setG a b False False True = (b \\ a)
setG (a:s) (b:t) c d e
 | a < b = setH c a (setG s (b:t) c d e)
 | a == b = setH d a (setG s t c d e)
 | otherwise = setH e b (setG (a:s) t c d e)
setG (a:s) [] c d e = setH c a (setG s [] c d e)
setG [] (b:t) c d e = setH e b (setG [] t c d e)
setG [] [] _ _ _ = []

setH :: Bool -> a -> [a] -> [a]
setH True a b = a:b
setH False _ b = b

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
catalyze f g n = foldl' (\(a,h) _ -> catalyzeF f h a) ([],g) ((indices n)::[Int])

catalyzeF :: (g -> (a,g)) -> g -> [a] -> ([a],g)
catalyzeF f g a = let (b,h) = f g in (b:a,h)

-- given start and context, depth first search for path to first leaf
qualify :: (a -> b -> Maybe [(a,b)]) -> a -> b -> Maybe [a]
qualify f a b = fmap (\x -> a:x) ((f a b) >>= (\x -> findMaybe (qualifyF f) x))

qualifyF :: (a -> b -> Maybe [(a,b)]) -> (a,b) -> Maybe [a]
qualifyF f (a,b) = qualify f a b

-- call function for new result until it returns Nothing
foldMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldMaybe f a (b:c) = foldMaybeF f (Just a) (f a b) c
foldMaybe _ a [] = Just a

foldMaybeF :: (a -> b -> Maybe a) -> Maybe a -> Maybe a -> [b] -> Maybe a
foldMaybeF f _ (Just b) (c:d) = foldMaybeF f (Just b) (f b c) d
foldMaybeF _ a _ _ = a

-- call function until it returns Just
findMaybe :: (b -> Maybe a) -> [b] -> Maybe a
findMaybe f (b:c) = findMaybeF f (f b) c
findMaybe _ [] = Nothing

findMaybeF :: (b -> Maybe a) -> Maybe a -> [b] -> Maybe a
findMaybeF _ (Just b) _ = Just b
findMaybeF f Nothing (b:c) = findMaybeF f (f b) c
findMaybeF _ Nothing [] = Nothing

--
-- now for something new
--

-- return how many regions a space of given dimension and boundaries has
defineLinear :: Int -> Int -> Int
defineLinear n m
 | n < 0 = error "negative dimension"
 | m < 0 = error "negative boundaries"
 | (n == 0) || (m == 0) = 1
 | otherwise = (defineLinear n (m-1)) + (defineLinear (n-1) (m-1))

-- return whether all subspaces have correct number of regions
isLinear :: Int -> Space -> Bool
isLinear n s
 | n < 0 = error "negative dimension"
 | (n == 0) || ((length s) == 0) = (length (regionsOfSpace s)) == 1
 | n == 1 = let
  halves = concat (map (\(x,y) -> map (\z -> (x,z)) y) (enumerate s))
  ends = filter (\(_,x) -> (length x) == 1) halves
  dirs = map (\(_,x) -> filter (\(_,y) -> (length (x \\ y)) == 0) halves) ends
  func a b = let ((_,x),(_,y)) = (a,b) in if (length (y \\ x)) == 0 then GT else (if (length (x \\ y)) == 0 then LT else EQ)
  sorts = map (\z -> sortBy func z) dirs
  domains = map domain sorts
  in ((length domains) == 2) && ((domains !! 0) == (reverse (domains !! 1)) || ((domains !! 0) == (domains !! 1)))
 | otherwise = let
  boundaries = boundariesOfSpace s
  sizes = indices (length boundaries)
  subs = foldl' (\a b -> a Prelude.++ (subsets b boundaries)) [] sizes
  in foldl' (\a b -> a && (isLinearF n s b)) True subs

isLinearF :: Int -> Space -> [Boundary] -> Bool
isLinearF n s b = let
 fixed = map (\(x,y) -> y - x) (enumerate (welldef b))
 subspace = foldl' (\x y -> subSpace y x) s fixed
 regions = regionsOfSpace subspace
 boundaries = boundariesOfSpace subspace
 in (defineLinear n (length boundaries)) == (length regions)

-- return all boundaries in space
boundariesOfSpace :: Space -> [Boundary]
boundariesOfSpace s = indices (length s)

-- return all regions in space
regionsOfSpace :: Space -> [Region]
regionsOfSpace [] = [0]
regionsOfSpace s = welldef (concat (head s))

-- side of region with regard to boundary
regionWrtBoundary :: Boundary -> Region -> Space -> Side
regionWrtBoundary b r s = fromJust (findIndex (\a -> member r a) (s !! b))

-- side of vertex identified by n boundaries
vertexWrtBoundary :: Boundary -> [Boundary] -> Space -> Side
vertexWrtBoundary b r s = regionWrtBoundary b (head (attachedRegions r s)) s

-- return per boundary side of region
sidesOfRegion :: Region -> Space -> [Side]
sidesOfRegion r s = map (\a -> boolToInt (member r (a !! 1))) s

-- return region from per boundary side
regionOfSides :: [Side] -> Space -> Region
regionOfSides r s = let [x] = (regionOfSidesF r s) in x

-- return whether region with given side map exists in space
regionOfSidesExists :: [Side] -> Space -> Bool
regionOfSidesExists r s = (length (regionOfSidesF r s)) == 1

regionOfSidesF :: [Side] -> Space -> [Region]
regionOfSidesF r s = foldl' (\a (b,c) -> a +\ (c !! b)) (regionsOfSpace s) (zip r s)

-- return sidedness with boundaries reversed
oppositeOfSides :: [Boundary] -> [Side] -> [Side]
oppositeOfSides b r = foldl' (\x y -> replace y (notOfInt (x !! y)) x) r b

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
attachedRegions [] s = regionsOfSpace s
attachedRegions [b] s = filter (\r -> oppositeOfRegionExists [b] r s) (regionsOfSpace s)
attachedRegions b s = filter (\r -> (length (b \\ (attachedBoundaries r s))) == 0)
 (filter (\r -> oppositeOfRegionExists b r s) (regionsOfSpace s))

-- return corresponding outside region
outsideOfRegion :: Region -> Space -> Region
outsideOfRegion r s = oppositeOfRegion (boundariesOfSpace s) r s

-- return whether the region is an outside region
outsideOfRegionExists :: Region -> Space -> Bool
outsideOfRegionExists r s = oppositeOfRegionExists (boundariesOfSpace s) r s

-- return sorted equivalent
sortSpace :: Space -> Space
sortSpace s = sort (map sort (map (map sort) s))

-- return space of same dimension with given boundary removed
subSpace :: Boundary -> Space -> Space
subSpace b s = let
 regions = filter (\r -> intToBool (regionWrtBoundary b r s)) (attachedRegions [b] s)
 in map (map (\x -> x \\ regions)) (unplace b s)

subPlace :: Boundary -> Place -> Place
subPlace b s = let
 (bounds,space) = unzip s
 index = fromJust (elemIndex b bounds)
 in zip (unplace index bounds) (subSpace index space)

-- return space of one less dimension homeomorphic to regions attached to given boundary
sectionSpace :: Boundary -> Space -> Space
sectionSpace b s = let
 regions = filter (\r -> intToBool (regionWrtBoundary b r s)) (attachedRegions [b] s)
 in map (map (\x -> x +\ regions)) (unplace b s)

sectionPlace :: Boundary -> Place -> Place
sectionPlace b s = let
 (bounds,space) = unzip s
 index = fromJust (elemIndex b bounds)
 in zip (unplace index bounds) (sectionSpace index space)

-- return whether local opposite of given region is empty and all of its oppositeOf regions are non-empty
canMigrate :: Region -> Space -> Bool
canMigrate r s = let
 boundaries = attachedBoundaries r s
 sides = sidesOfRegion r s
 opposite = oppositeOfSides boundaries sides
 empty = not (regionOfSidesExists opposite s)
 neighbors = map (\a -> oppositeOfSides [a] opposite) boundaries
 exists = map (\a -> regionOfSidesExists a s) neighbors
 in foldl' (\a b -> a && b) empty exists

-- return space with given region changed to its local opposite
migrateSpace :: Region -> Space -> Space
migrateSpace r s = map (migrateSpaceF (attachedBoundaries r s) r) (enumerate s)

migrateSpaceF :: [Boundary] -> Region -> (Boundary,Full) -> Full
migrateSpaceF b r (a,s)
 | (member a b) && (member r (s !! 0)) = [(remove r (s !! 0)),(insert r (s !! 1))]
 | (member a b) = [(insert r (s !! 0)),(remove r (s !! 1))]
 | otherwise = s

-- reverse sidedness of given boundary
mirrorPlace :: Boundary -> Place -> Place
mirrorPlace b s = map (\(x,[y,z]) -> if x == b then (x,[z,y]) else (x,[y,z])) s

-- subset is subspace in dual representation
isSubPlace :: Place -> Place -> Bool
isSubPlace s t = let
 sDual = placeToDual s
 tDual = placeToDual t
 in (length ((sort sDual) \\ (sort tDual))) == 0

-- representation converter
placeToDual :: Place -> Dual
placeToDual s = let
 left = map (\x -> domain (filter (\(_,[y,_]) -> member x y) s)) (regionsOfSpace (range s))
 right = map (\x -> domain (filter (\(_,[_,y]) -> member x y) s)) (regionsOfSpace (range s))
 in map (\(x,y) -> [x,y]) (zip left right)

-- representation converter
dualToPlace :: Dual -> Place
dualToPlace s = let
 bounds = welldef (concat (head s))
 plual = enumerate s
 left = map (\x -> domain (filter (\(_,[y,_]) -> member x y) plual)) bounds
 right = map (\x -> domain (filter (\(_,[_,y]) -> member x y) plual)) bounds
 in zip bounds (map (\(x,y) -> [x,y]) (zip left right))

-- preserve sidedness, but halves are sets
sortDualRegion :: [[Boundary]] -> [[Boundary]]
sortDualRegion [a,b] = [sort a,sort b]
sortDualRegion _ = error "malformed dual region in sort"

-- same as sortSpace
sortDual :: Dual -> Dual
sortDual a = sort (map sortDualRegion a)

-- same as oppositeOfRegion of singleton
hopDualRegion :: Boundary -> [[Boundary]] -> [[Boundary]]
hopDualRegion b [l,r]
 | member b l = [remove b l, insert b r]
 | otherwise = [insert b l, remove b r]
hopDualRegion _ _ = error "malformed dual region in hop"

-- space of just one boundary
singleSpace :: Boundary -> Place
singleSpace b = [(b,[[0],[1]])]

-- return space by calling superSpace with singleton space
anySpace :: Random.RandomGen g => Show g => g -> Int -> Int -> (Space, g)
anySpace g n m = let
 (s,h) = foldl' (\(x,y) z -> superSpace y n x (singleSpace z)) ([],g) (indices m)
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
 | canMigrate p s = allSpacesG q s (minEquiv (migrateSpace p s)) todo done
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

--
-- so far so simple
--

-- optimize this
minEquiv :: Space -> Space
minEquiv s = head (sort (map (\x -> sortSpace (minEquivH (length s) (minEquivG x (minEquivF s)))) (minEquivI (length s))))

minEquivF :: Space -> [Pack]
minEquivF s = map (\x -> foldl' (\y (p,q) -> if member x (q !! 0) then setBit y p else y) 0 (enumerate s)) (regionsOfSpace s)

minEquivG :: [(Int, Bool)] -> [Pack] -> [Pack]
minEquivG a b = sort (map (\p -> foldl' (\q (x,(y,z)) -> if (belongs x p) /= z then setBit q y else q) 0 (enumerate a)) b)

minEquivH :: Int -> [Pack] -> Space
minEquivH m s = map (\x -> map (\y -> domain (filter (\(_,z) -> (belongs x z) == y) (enumerate s))) [True,False]) (indices m)

minEquivI :: Int -> [[(Int, Bool)]]
minEquivI m = [zip a b | a <- permutations (indices m), b <- (polybools m)]

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
divideSpaceG m a b = (image (a +\ b) m) ++ b

-- divide regions of s in t by new boundary b
dividePlace :: Boundary -> Place -> Place -> Place
dividePlace b s t = let
 space = range t
 boundaries = (domain t) Prelude.++ [b]
 regions = image (placeToDual s) (zip (placeToDual t) (regionsOfSpace space))
 in zip boundaries (divideSpace regions space)

superSpaceX :: [Boundary] -> [Place] -> String
superSpaceX b s = let
 boundaries = map domain s
 place = map (\z -> map (\[_,x] -> boolsToPack (map (\y -> member y x) b)) (placeToDual z)) s
 in show (zip boundaries place)

-- return superspace with given spaces as subspaces
superSpace :: Random.RandomGen g => Show g => g -> Int -> Place -> Place -> (Place, g)
superSpace g n s t = superSpaceF g n s t []

superSpaceF :: Random.RandomGen g => Show g => g -> Int -> Place -> Place -> [Place] -> (Place, g)
superSpaceF g n s t u
 | n < 0 = error (show ("negative",n))
 | n == 0 = let
  sLeft = domain (filter (\(_,[y,_]) -> (length y) /= 0) s)
  sRight = domain (filter (\(_,[_,z]) -> (length z) /= 0) s)
  tLeft = domain (filter (\(_,[y,_]) -> (length y) /= 0) t)
  tRight = domain (filter (\(_,[_,z]) -> (length z) /= 0) t)
  lBounds = sLeft ++ tLeft
  rBounds_ = sRight ++ tRight
  rBounds = if (length (lBounds +\ rBounds_)) == 0 then rBounds_ else error (show ("zero",superSpaceX boundaries [s,t]))
  left = map (\x -> (x,[[0],[]])) lBounds
  right = map (\x -> (x,[[],[0]])) rBounds
  in (left ++ right, g)
 | (length tOnly) == 0 = (s,g)
 | (length sOnly) == 0 = (t,g)
 | (length boundaries) <= n = let
  -- every possible region
  regions = power (length boundaries)
  in (map (\(x,y) -> (y, [superSpaceI x regions 0, superSpaceI x regions 1])) (enumerate boundaries), g)
 | ((length shared) == 0) && ((length sBounds) == 1) && ((length tBounds) == 1) = error (show ("guard",n,superSpaceX boundaries [s,t]))
 | ((length shared) == 0) && ((length sBounds) == 1) = superSpaceF g n t s u
 | (length shared) == 0 = let
  -- choose boundary to remove
  -- recurse with one fewer boundary
  -- recurse to restore boundary
  (bound,h) = choose g sBounds
  singlespace = singleSpace bound
  (space,i) = superSpaceF h n t singlespace u
  in superSpaceF i n s space u
 | ((length sOnly) == 1) && ((length tOnly) > 1) = superSpaceF g n t s u
 | ((length sOnly) > 1) = let -- s and t are not proper
  -- 0 1 2 3 5 7 + 0 1 2 4 =
  -- 0 1 2 3 5 7 + (0 1 2 3 5 + 0 1 2 4) =
  -- 0 1 2 3 5 7 + (0 1 2 3 5 + (0 1 2 3 + 0 1 2 4))
  -- or
  -- 0 1 2 3 5 7 + 0 1 2 4 6 8 =
  -- 0 1 2 3 5 7 + (0 1 2 3 5 + 0 1 2 4 6 8) =
  -- 0 1 2 3 5 7 + (0 1 2 3 5 + (0 1 2 3 + 0 1 2 4 6 8))
  (b,h) = choose g sOnly -- choice is from more than one
  sub = subPlace b s -- since choice was from more than one, sub and t are not proper
  (sup,i) = superSpaceF h n sub t u -- adds something to t because sub and t are not proper
  in superSpaceF i n s sup u -- easier because sup contains t plus one other than b that t did not
 | n == 1 = let
  [sBound] = sBounds
  [tBound] = tBounds
  -- divide all regions of s by tBound, and all regions of t by sBound
  sGuide = dividePlace tBound s s
  tGuide = dividePlace sBound t t
  -- find sorted duals of divided s, divided t, and u
  sDual = sortDual (placeToDual sGuide)
  tDual = sortDual (placeToDual tGuide)
  uDual = map (\x -> sortDual (placeToDual x)) u
  -- find intersection of sorted duals
  dual = foldl' (\x y -> x +\ y) (sDual +\ tDual) uDual
  -- find per-region maps from boundary to region-index
  mapped = map (\x -> map (\y -> (y,x)) boundaries) (enumerate dual)
  filtered = map (\x -> filter (\(y,(_,z)) -> member (sortDualRegion (hopDualRegion y z)) dual) x) mapped
  hops = map (\x -> map (\(y,(z,_)) -> (y,z)) x) filtered
  -- try each dual as start to qualify by boundaries with hops as function
  starts = filter (\x -> member (sortDualRegion (reverse (dual !! x))) dual) (indices (length hops)) 
  indexes = findMaybe (superSpaceG boundaries hops) starts -- TODO pass g to qualify
  -- indices = if indices_ /= Nothing then indices_ else error (show ("base",n,superSpaceX boundaries [s,t,u]))
  result :: [Int]
  result = fromJust indexes
  -- map indices to dual to place
  in (dualToPlace (map (\x -> dual !! x) result), g)
 | otherwise = let
  -- recurse with one fewer boundary
  (bound,h) = choose g shared
  sSub = subPlace bound s
  tSub = subPlace bound t
  sSect = sectionPlace bound s
  tSect = sectionPlace bound t
  (sub,i) = superSpaceF h n sSub tSub u
  (sect,j) = superSpaceF i (n-1) sSect tSect (sub:u)
  result = dividePlace bound sect sub
  mirror = mirrorPlace bound result
  test = (isLinear n (range result)) && (isSubPlace result s) && (isSubPlace result t)
  in if test then (result, j) else (mirror, j) where
 sBounds = domain s
 tBounds = domain t
 tOnly = tBounds \\ sBounds
 sOnly = sBounds \\ tBounds
 shared = sBounds +\ tBounds
 boundaries = sBounds ++ tBounds

-- given per-index map from boundary to index hopped to,
-- return index list that hops across all given boundaries or nothing
superSpaceG :: [Boundary] -> [[(Boundary,Int)]] -> Int -> Maybe [Int]
superSpaceG bounds hops index = qualify (superSpaceH hops) index bounds

-- given hop maps, current index, boundaries not hopped yet, return list of potential hops,
-- or nothing if not possible to nop across sime remaining boundary
superSpaceH :: [[(Boundary,Int)]] -> Int -> [Boundary] -> Maybe [(Int,[Boundary])]
superSpaceH hops index bounds = let
 result = filter (\(x,_) -> member x bounds) (hops !! index)
 test = ((length result) == 0) == ((length bounds) == 0)
 in if test then Just (map (\(x,y) -> (y, remove x bounds)) result) else Nothing

-- regions indicated by boundary as bit position
superSpaceI :: Int -> [Pack] -> Int -> [Region]
superSpaceI b r s = filter (\y -> (boolToInt (belongs b y)) == s) r

--
-- between symbolic and numeric
--

-- return given number of planes in given number of dimensions
randomPlanes :: Random.RandomGen g => g -> Int -> Int -> Maybe ([Plane], g)
randomPlanes g n m = let
 (a,h) = catalyze (\i -> Random.randomR (-100.0,100.0) i) g (n * m)
 b = Matrix.toColumns (Matrix.matrix m a)
 tweak = [randomPlanesH0 n m, randomPlanesH1 n m, randomPlanesH2 n m]
 func x = findMaybe (\y -> y x)
 in foldMaybe func (b,h) (repeat tweak)

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
 | m <= n = divideSpace (regionsOfSpace space) space
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
  in divideSpace divided space where
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
spaceFromPlanesH _ _ _ _ _ = error "too many or few guards"

-- return planes with sidednesses as specified by given dimension and space
planesFromSpace :: Int -> Space -> [Plane]
planesFromSpace n s
 | n == 0 = replicate m (Matrix.vector [])
 | m <= n = take m (Matrix.toColumns (Matrix.ident n))
 | otherwise = let
  -- recurse with one fewer boundary
  bound = m - 1
  space = subSpace bound s
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
 in Matrix.scale (1.0 / (fromIntegral (length points))) (foldl' (\x y -> Matrix.add x y) zero points)

 -- find point some distance out on line to coregion from other outside
planesFromSpaceI :: Int -> Region -> Space -> [Plane] -> Point -> Point
planesFromSpaceI n r s p arrow = let
 feather = planesFromSpaceH n (outsideOfRegion r s) s p
 shaft = Matrix.add arrow (Matrix.scale (negate 1.0) feather)
 factor = 0.1 / (sqrt (Matrix.dot shaft shaft))
 in Matrix.add arrow (Matrix.scale factor shaft)
