--    Main tests for AffTopo
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

module Main (main) where

import AffTopo.Naive

import Prelude hiding ((++))
import Data.List hiding ((\\), (++), insert)
import Data.Maybe
import qualified System.Random as Random

main :: IO ()
main = putStrLn (show (find (\(_,x) -> x /= Nothing) [
  ("empty",empty)
  ,("simplex",simplex)
  ,("antisection",antisection)
  ,("migrate",migrate)
  ,("higher",higher)
  ,("meta",meta)
  ,("single",single)
  ,("rename",rename)
  ,("equivalent",equivalent)
  ,("section",section)
  ,("outside",outside)
  ,("ordering",ordering)
  ,("intervals",intervals)
  ,("special",special)
  ,("bug",bug)
  ,("disjoint",disjoint)
  ,("general",general)
  ,("complexity",complexity)
  ,("symbolic",symbolic)
  ]))

data Random.RandomGen g => Debug g = Debug [Int] [Int] g

instance Random.RandomGen g => Show (Debug g) where
 show (Debug a _ _) = show (reverse a)

instance Random.RandomGen g => Random.RandomGen (Debug g) where
 next (Debug a (b:c) d) = (b, (Debug (b:a) c d))
 next (Debug a [] d) = let (b,e) = Random.next d in (b, (Debug (b:a) [] e))
 genRange (Debug _ _ d) = Random.genRange d
 split (Debug _ _ d) = let (e,f) = Random.split d in ((Debug [] [] e), (Debug [] [] f))

subSubPlace :: [Boundary] -> Place -> Place
subSubPlace b s = foldl' (\x y -> subPlace y x) s ((domain s) \\ b)

isSubPlace :: Place -> Place -> Bool
isSubPlace a b = (minEquiv (range (subSubPlace (domain a) b))) == (minEquiv (range a))

rv :: Bool -> String -> Maybe String
rv a b = if a then Nothing else Just b

rva :: Maybe String -> Maybe String -> Maybe String
rva Nothing b = b
rva a _ = a

join :: Maybe (Maybe a) -> Maybe a
join Nothing = Nothing
join (Just a) = a

rvb :: (a -> Maybe String) -> [a] -> Maybe String
rvb a b = join (find (\x -> x /= Nothing) (map a b))

empty :: Maybe String
empty = rv ((regionsOfSpace []) == [0]) (show (regionsOfSpace []))

simplex :: Maybe String
simplex = let
 space = foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]]
 regions = regionsOfSpace space
 outsides = filter (\r -> outsideOfRegionExists r space) regions
 insides = regions \\ outsides
 cans = filter (\r -> canMigrate r space) regions
 cants = regions \\ cans
 migration = migrateSpace (head cans) space
 in (rv (isLinear 2 space) (show ("space",space))) `rva`
  (rv (insides == cans) (show ("in",insides,cans))) `rva`
  (rv (outsides == cants) (show ("out",outsides,cants))) `rva`
  (rv ((length cans) == 1) (show ("length",cans))) `rva`
  (rv (isLinear 2 migration) (show ("migration",migration)))

extendSpace :: Int -> Space -> [Space]
extendSpace 1 space = map (\x -> divideSpace [x] space) (regionsOfSpace space)
extendSpace 2 space = let
 func x = map (\y -> map (\z -> filter (\a -> member a x) z) y) space
 boundaries = boundariesOfSpace space
 regions = regionsOfSpace space
 num = defineLinear 1 (length boundaries)
 linears = filter (\x -> isLinear 1 (func x)) (subsets num regions)
 in map (\x -> divideSpace x space) linears
extendSpace _ _ = []

antisection :: Maybe String
antisection = let
 space = foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]]
 spaces = extendSpace 2 space
 in rvb (\x -> rv (isLinear 2 x) (show x)) spaces

migrate :: Maybe String
migrate = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 migrates = concat (map (\s -> map (\r -> migrateSpace r s) (filter (\r -> canMigrate r s) (regionsOfSpace s))) spaces)
 in rvb (\s -> rv (isLinear 2 s) (show s)) migrates

higher :: Maybe String
higher = let
 a :: [Int]
 a = indices 10
 b = map (\x -> x + 10) a
 c = map (\x -> x + 20) a
 func0 x = (map (\y -> x - (mod x 10) + (mod (x + y) 10)) (indices 3)) +\ (a AffTopo.Naive.++ c)
 unsorted0 = map (generate func0) a
 unsorted1 = map (generate func0) b
 unsorted2 = map (generate func0) c
 sorted0 = map welldef unsorted0
 sorted2 = map welldef unsorted2
 func1 x = (fromIntegral x,x+1)
 ints :: ([Int],Int)
 ints = catalyze func1 5 5
 doubles :: ([Double],Int)
 doubles = catalyze func1 5 5
 func2 x y = if y < 5 then Just (y:x) else Nothing
 justs :: Maybe [Int]
 justs = foldMaybe func2 [] [0,1..]
 in (rvb (\x -> rv ((length x) == (length a)) (show ("unsorted0",x,a))) unsorted0) `rva`
  (rvb (\x -> rv ((length x) == 1) (show ("unsorted1",x))) unsorted1) `rva`
  (rvb (\x -> rv ((length x) == (length c)) (show ("unsorted2",x,c))) unsorted2) `rva`
  (rvb (\x -> rv (x == a) (show ("sorted0",x,a))) sorted0) `rva`
  (rv ((concat unsorted1) == b) (show ("sorted1",unsorted1,b))) `rva`
  (rvb (\x -> rv (x == c) (show ("sorted2",x,c))) sorted2) `rva`
  (rv (ints == ([9,8,7,6,5],10)) (show ("ints",ints))) `rva`
  (rv (doubles == ([9.0,8.0,7.0,6.0,5.0],10)) (show ("doubles",doubles))) `rva`
  (rv (justs == (Just [4,3,2,1,0])) (show ("justs",justs)))

powerSets :: Ord a => [a] -> [[a]]
powerSets a = concat (map (\x -> subsets x a) (indices (length a)))

meta :: Maybe String
meta = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 places = map enumerate spaces
 tuples = [(a,b) | a <- places, b <- powerSets (domain a)]
 subs = map (\(a,b) -> (a, subSubPlace b a)) tuples
 in rvb (\(a,b) -> rv (isSubPlace b a) (show (a,b))) subs

single :: Maybe String
single = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 orders = extendSpace 1 (foldl (\x y -> divideSpace y x) [] [[0],[0],[0]])
 in (rvb (\x -> rv (isLinear 2 x) (show x)) spaces) `rva` (rvb (\x -> rv (isLinear 1 x) (show x)) orders)

equivSpace :: [Region] -> Space -> Space
equivSpace r s = map (\x -> map (\y -> map (\z -> r !! (fromJust (elemIndex z (regionsOfSpace s)))) y) x) s

rename :: Maybe String
rename = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 regions = map (\x -> x * 3) (indices (defineLinear 2 4))
 in rvb (\x -> rv ((regionsOfSpace (equivSpace regions x)) == regions) (show (x,regions))) spaces

equivalent :: Maybe String
equivalent = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 linear = defineLinear 2 4
 perms = permutations (indices linear)
 equivs = map (\(a,b) -> (a, equivSpace b a)) (zip spaces perms)
 in (rvb (\x -> rv ((minEquivH (length x) (minEquivF x)) == x) (show x)) spaces) `rva`
  (rvb (\(a,b) -> rv ((minEquiv a) == (minEquiv b)) (show (a,b))) equivs)

section :: Maybe String
section = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 sections = [(x,y,sectionSpace y x) | x <- spaces, y <- boundariesOfSpace x]
 in rvb (\(x,y,z) -> rv (isLinear 1 z) (show (x,y,z))) sections

outside :: Maybe String
outside = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 sections = [sectionSpace y x | x <- spaces, y <- boundariesOfSpace x]
 func x s y = (outsideOfRegionExists y s) && ((regionWrtBoundary x y s) == 0)
 args = [(b,s) | s <- sections, b <- boundariesOfSpace s]
 in rvb (\(b,s) -> rv ((find (func b s) (regionsOfSpace s)) /= Nothing) (show (b,s))) args

negateOrdering :: Ordering -> Ordering
negateOrdering LT = GT
negateOrdering GT = LT
negateOrdering EQ = EQ

ordering :: Maybe String
ordering = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 sections = [sectionSpace y x | x <- spaces, y <- boundariesOfSpace x]
 orders = [(x,y,z,superSpaceN 0 x) | x <- sections, y <- boundariesOfSpace x, z <- boundariesOfSpace x]
 in rvb (\(x,y,z,r) -> rv ((superSpaceM r x y z) == (negateOrdering (superSpaceM r x z y))) (show (x,y,z))) orders

intervals :: Maybe String
intervals = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 sections = [sectionSpace y x | x <- spaces, y <- boundariesOfSpace x]
 orders = [(x,superSpaceK y (enumerate x)) | x <- sections, y <- indices (length x)]
 unorders = map (\(x,y) -> (x, y, range (superSpaceL y [] (indices ((length y) + 1))))) orders
 in rvb (\(x,y,z) -> rv ((minEquiv x) == (minEquiv z)) (show (x,y,z))) unorders

special :: Maybe String
special = let
 spaces = extendSpace 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 sections = [sectionSpace y x | x <- spaces, y <- boundariesOfSpace x]
 places = map enumerate sections
 tuples = [(a,b,c) | a <- places, b <- powerSets (domain a), c <- powerSets (domain a)]
 subs = map (\(a,b,c) -> (a, subSubPlace b a, subSubPlace c a)) tuples
 sups = map (\(d,(a,b,c)) -> (superSpace (Random.mkStdGen d) 1 b c, a, b, c,d)) (enumerate subs)
 in rvb (\((e,_),_,b,c,d) -> (rv ((isSubPlace b e) && (isSubPlace c e)) (show ("left",b,"right",c,"num",d,"super",e)))) sups

bug :: Maybe String
bug = let
 g = Debug [] [315069880,840462263,506335523,940130511,977375085,1593321438,1511714125,1127685618] (Random.mkStdGen 0)
 s = [(0,[[0,2,7,8],[1,3,9]]),(3,[[0,1,7],[2,3,8,9]]),(2,[[1,7,8,9],[0,2,3]])]
 t = [(4,[[0,1,4],[2,5,6,8]]),(3,[[1,4,5,6],[0,2,8]]),(0,[[0,4,5,8],[1,2,6]])]
 (place,_) = superSpace g 2 s t
 in rv (isLinear 2 (range place)) (show ("place",domain place,place,"s",domain s,s,"t",domain t,t))

disjoint :: Maybe String
disjoint = let
 disany = map (\(d,(n,m)) -> (n,m,anySpace (Debug [] [] (Random.mkStdGen d)) n m)) (enumerate [(n,m) | n <- [2..4], m <- [0..6]])
 in rvb (\(n,m,(s,g)) -> rv (isLinear n s) (show (g,n,m,defineLinear n m,length s,s))) disany

general :: Maybe String
general = let
 complexities = [(n,m) | n <- [3..4], m <- [3..6]]
 spaces = map (\(g,(n,m)) -> (n,anySpace (Random.mkStdGen g) n m)) (enumerate complexities)
 places = map (\(n,(a,_)) -> (n,enumerate a)) spaces
 tuples = [(n,a,b,c) | (n,a) <- places, b <- powerSets (domain a), c <- powerSets (domain a)]
 subs = map (\(n,a,b,c) -> (n, a, subSubPlace b a, subSubPlace c a)) tuples
 sups = map (\(d,(n,a,b,c)) -> (n, superSpace (Debug [] [] (Random.mkStdGen d)) n b c, a, b, c, d)) (enumerate subs)
 in rvb (\(n,(d,g),a,b,c,h) ->
  (rv (isLinear n (range b)) (show ("left",b))) `rva`
  (rv (isLinear n (range c)) (show ("right",c))) `rva`
  (rv (isLinear n (range d)) (show ("dimension",n,"number",g,h,"space",a,"left",b,"right",c,"super",d))) `rva`
  (rv (isSubPlace b d) (show ("dimension",n,"left",b,"super",d))) `rva`
  (rv (isSubPlace c d) (show ("dimension",n,"right",c,"super",d)))) sups

complexity :: Maybe String
complexity = let
 complexities = [(n,m) | n <- [3..5], m <- [3..8]]
 spaces = map (\(g,(n,m)) -> (n, anySpace (Random.mkStdGen g) n m)) (enumerate complexities)
 in rvb (\(n,(s,_)) -> rv (isLinear n s) (show (n,s))) spaces

symbolic :: Maybe String
symbolic = let
 (space,_) = anySpace (Random.mkStdGen 0) 2 5
 linears = allSpaces space
 in rvb (\s -> rv (isLinear 2 s) (show s)) linears
