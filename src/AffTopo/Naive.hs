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
regionsOfSpace s = welldef (concat (concat s))

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

-- space of just one boundary
singleSpace :: Boundary -> Place
singleSpace b = [(b,[[0],[1]])]

-- return space by calling superSpace with singleton space
anySpace :: Random.RandomGen g => g -> Int -> Int -> (Space, g)
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

-- sub-regions in second space of super-regions in shared sub-space of given regions in first space
takeRegions :: Place -> Place -> [Region] -> [Region]
takeRegions _ [] [] = []
takeRegions _ [] _ = regionsOfSpace []
takeRegions s t r = let
 (firstBoundaries,firstSpace) = unzip s
 (secondBoundaries,secondSpace) = unzip t
 -- for each given region, find sides in first space
 firstSides :: [[Side]] -- given of SRegion -> SBoundary -> Side
 firstSides = map (\x -> sidesOfRegion x firstSpace) r
 -- for each boundary in second space, find corresponding boundary in first space or Nothing
 secondToFirst :: [Maybe Int] -- TBoundary -> Maybe SBoundary index
 secondToFirst = map (\x -> elemIndex x firstBoundaries) secondBoundaries
 -- for each given region, for each boundary in second space, find sidedness or Nothing in first space
 secondSides :: [[Maybe Side]] -- given of SRegion -> TBoundary -> Maybe Side
 secondSides = map (\x -> map (\y -> fmap (\z -> x !! z) y) secondToFirst) firstSides
 -- for each boundary in second space, count number of Nothing
 count :: Int
 count = foldl' (\x y -> if y == Nothing then x+1 else x) 0 secondToFirst
 -- find sidedness permutations of same length as indices
 permutes :: [[Side]] -- every possible -> count of TBoundary -> Side
 permutes = polyants count
 -- for each given region, for each permutation, fix second sides of region, consuming head of permutation as needed
 fixedSides :: [[Side]] -- given of SRegion -> TBoundary -> Side
 fixedSides = map (\(x,y) -> takeRegionsF x y) [(x,y) | x <- secondSides, y <- permutes]
 -- ignore empty regions
 extantSides :: [[Side]] -- given of SRegion -> TBoundary -> Side
 extantSides = filter (\x -> regionOfSidesExists x secondSpace) fixedSides
 -- map sides to regions
 in welldef (map (\x -> regionOfSides x secondSpace) extantSides)

takeRegionsF :: [Maybe Side] -> [Side] -> [Side]
takeRegionsF ((Just a):b) c = a:(takeRegionsF b c)
takeRegionsF (Nothing:b) (c:d) = c:(takeRegionsF b d)
takeRegionsF [] [] = []
takeRegionsF (Nothing:_) [] = error "not enough permutations"
takeRegionsF [] (_:_) = error "too many permutations"

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

-- divide t such that undivided region has same sidedness wrt divided regions as wrt b in u
dividePlace :: Boundary -> Place -> Place -> Place -> Place
dividePlace b s t u = let
 given = regionsOfSpace (range s)
 divided = takeRegions s t given
 (bounds,space) = unzip t
 index = length bounds
 bound = fromJust (elemIndex b (domain u))
 boundaries = bounds Prelude.++ [b]
 regions  = regionsOfSpace space
 undivided = regions \\ divided
 tochoose = if (length undivided) > 0 then undivided else regions
 chosen = head tochoose
 totake = takeRegions t u [chosen]
 taken = head totake
 expected = regionWrtBoundary bound taken (range u)
 super = divideSpace divided space
 place = zip boundaries super
 toregion = takeRegions t place [chosen]
 region = head toregion
 [left,right] = super !! index
 mirror = (take index super) Prelude.++ [[right,left]]
 actual = regionWrtBoundary index region super
 in if actual /= expected then zip boundaries mirror else place

-- return superspace with given spaces as subspaces
superSpace :: Random.RandomGen g => g -> Int -> Place -> Place -> (Place, g)
superSpace g n s t
 | n < 0 = error "negative dimension"
 | n == 0 = let
  sLeft = domain (filter (\(_,[y,_]) -> (length y) /= 0) s)
  sRight = domain (filter (\(_,[_,z]) -> (length z) /= 0) s)
  tLeft = domain (filter (\(_,[y,_]) -> (length y) /= 0) t)
  tRight = domain (filter (\(_,[_,z]) -> (length z) /= 0) t)
  lBounds = sLeft ++ tLeft
  rBounds = sRight ++ tRight
  left = map (\x -> (x,[[0],[]])) (lBounds \\ rBounds)
  right = map (\x -> (x,[[],[0]])) rBounds
  in (left ++ right, g)
 | (length (tBounds \\ sBounds)) == 0 = (s,g)
 | (length (sBounds \\ tBounds)) == 0 = (t,g)
 | (length (sBounds ++ tBounds)) <= n = let
  -- every possible region
  boundaries = sBounds ++ tBounds
  regions = power (length boundaries)
  in (map (\(x,y) -> (y, [superSpaceI x regions 0, superSpaceI x regions 1])) (enumerate boundaries), g)
 | n == 1 && ((length (sBounds +\ tBounds)) == 0) = let
  -- starting from either ends
  -- take from either
  (sBound,h) = choose g sBounds
  (tBound,i) = choose h tBounds
  (bounds,j) = superSpaceJ i (superSpaceK sBound s) (superSpaceK tBound t) []
  in (superSpaceL bounds [] (indices ((length bounds) + 1)), j)
 | n == 1 = let
  -- starting from ends on same side of head of shared
  -- take from either until one is shared
  -- then take from unshared until both are shared
  -- then take shared and go back to first until
  shared = sBounds +\ tBounds
  (bound,h) = choose g shared
  (bounds,i) = superSpaceJ h (superSpaceK bound s) (superSpaceK bound t) shared
  in (superSpaceL bounds [] (indices ((length bounds) + 1)), i)
 | ((length (sBounds +\ tBounds)) == 0) && ((length sBounds) == 1) && ((length tBounds) == 1) = error "unreachable"
 | ((length (sBounds +\ tBounds)) == 0) && ((length sBounds) == 1) = superSpace g n t s
 | (length (sBounds +\ tBounds)) == 0 = let
  -- choose boundary to remove
  -- recurse with one fewer boundary
  -- recurse to restore boundary
  (bound,h) = choose g sBounds
  singlespace = singleSpace bound
  (space,i) = superSpace h n singlespace t
  in superSpace i n s space
 | ((length (sBounds \\ tBounds)) == 1) && ((length (tBounds \\ sBounds)) == 1) = let
  -- recurse with one fewer boundary
  [sBound] = sBounds \\ tBounds
  u_ = subPlace sBound s
  u = if (sort u_) == (sort (subPlace (head (tBounds \\ sBounds)) t)) then u_ else error "unequal shared"
  (bound,h) = choose g (sBounds +\ tBounds)
  sSub = subPlace bound s
  tSub = subPlace bound t
  sSect = sectionPlace bound s
  tSect = sectionPlace bound t
  (sub,i) = superSpace h n sSub tSub
  (sect,j) = superSpace i (n-1) sSect tSect
  in (dividePlace bound sect sub u, j)
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
  sub = subPlace b s -- since choice was from more than one, sub and t are not proper
  (sup,i) = superSpace h n sub t -- adds something to t because sub and t are not proper
  in superSpace i n s sup where -- easier because sup contains t plus one other than b that t did not
 sBounds = domain s
 tBounds = domain t

superSpaceX :: Place -> Place -> Bool
superSpaceX s t = let
 given = regionsOfSpace (range s)
 taken = superSpaceY s t
 nonempty = filter (\x -> (length x) > 0) taken
 in (length given) == (length nonempty)

superSpaceY :: Place -> Place -> [[Region]]
superSpaceY s t = map (\x -> takeRegions s t [x]) (regionsOfSpace (range s))

superSpaceZ :: Place -> Place -> [(Region,[Region])]
superSpaceZ s t = map (\x -> (x,takeRegions s t [x])) (regionsOfSpace (range s))

-- regions indicated by boundary as bit position
superSpaceI :: Int -> [Pack] -> Int -> [Region]
superSpaceI b r s = filter (\y -> (boolToInt (belongs b y)) == s) r

-- shuffle together two one dimensional spaces carrying along mirror arrow
superSpaceJ :: Random.RandomGen g => g -> [(Boundary,Side)] -> [(Boundary,Side)] -> [Boundary] -> ([(Boundary,Side)], g)
superSpaceJ g [] [] _ = ([],g)
superSpaceJ g [] t _ = (t,g)
superSpaceJ g s [] _ = (s,g)
superSpaceJ g ((a,x):s) ((b,y):t) c
 | (member a c) && (member b c) = let (d,h) = superSpaceJ g s t c in (((a,x):d), h)
 | member a c = let (d,h) = superSpaceJ g ((a,x):s) t c in (((b,y):d), h)
 | member b c = let (d,h) = superSpaceJ g s ((b,y):t) c in (((a,x):d), h)
 | otherwise = let
  (d,h) = choose g [False,True]
  (e,i) = if d then superSpaceJ h ((a,x):s) t c else superSpaceJ h s ((b,y):t) c
  in if d then (((b,y):e), i) else (((a,x):e), i)

-- put boundaries in order with oudside region in first halfspace of given boundary as first region
superSpaceK :: Boundary -> Place -> [(Boundary,Side)]
superSpaceK b s = let
 given = domain s
 space = range s
 boundaries = boundariesOfSpace space
 boundary = fromJust (elemIndex b given)
 region = superSpaceN boundary space
 orders = sortBy (superSpaceM region space) boundaries
 mirrors = map (\x -> (x, regionWrtBoundary x region space)) boundaries
 results = map (\x -> mirrors !! x) orders
 in map (\(x,y) -> (given !! x, y)) results

-- convert ordered boundaries to space putting first regions in indicated halfspace
superSpaceL :: [(Boundary,Side)] -> [Region] -> [Region] -> Place
superSpaceL ((x,y):a) b (z:c) = (x, (if intToBool y then [c,z:b] else [z:b,c])) : (superSpaceL a (z:b) c)
superSpaceL _ _ _ = []

-- return boundary order wrt given outside region that is in first halfspace of all boundaries
superSpaceM :: Region -> Space -> Boundary -> Boundary -> Ordering
superSpaceM r s x y
 | x == y = EQ
 | ((regionWrtBoundary x r s) == 0) == ((vertexWrtBoundary x [y] s) == 0) = GT
 | otherwise = LT

-- return outside region on first side of given boundary
superSpaceN :: Boundary -> Space -> Region
superSpaceN b s = let
 regions = regionsOfSpace s
 func x = (outsideOfRegionExists x s) && ((regionWrtBoundary b x s) == 0)
 in fromJust (find func regions)

--
-- from symbolic to numeric
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
