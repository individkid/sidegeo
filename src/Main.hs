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

main :: IO ()
main = putStrLn (show [
  ("empty",empty),
  ("simplex",simplex),
  ("antisection",antisection),
  ("corners",corners),
  ("migrate",migrate),
  ("higher",higher)])

empty :: Bool
empty = (regionsOfSpace []) == [0]

simplex :: Bool
simplex = let
 space = foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]]
 regions = regionsOfSpace space
 outsides = filter (\r -> outsideOfRegionExists r space) regions
 insides = regions \\ outsides
 cans = filter (\r -> canMigrate r space) regions
 cants = regions \\ cans
 migration = migrateSpace (head cans) space
 in (isLinear 2 space) && (insides == cans) && (outsides == cants) && ((length cans) == 1) && (isLinear 2 migration)

extendSpace :: Space -> [Space]
extendSpace space = let
 func x = map (\y -> map (\z -> filter (\a -> member a x) z) y) space
 boundaries = boundariesOfSpace space
 regions = regionsOfSpace space
 num = defineLinear 1 (length boundaries)
 linears = filter (\x -> isLinear 1 (func x)) (subsets num regions)
 in map (\x -> divideSpace x space) linears

antisection :: Bool
antisection = let
 space = foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]]
 spaces = extendSpace space
 in all (\x -> isLinear 2 x) spaces

corners :: Bool
corners = let
 spaces = extendSpace (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 myAttachedRegions :: [Boundary] -> Space -> [Region]
 myAttachedRegions b s = let
  place = enumerate s
  subplace = foldl (\x y -> superSpaceF y x) place b
  supers = map (\x -> takeRegions subplace place [x]) (regionsOfSpace (range subplace))
  in concat (filter (\x -> all (\y -> oppositeOfRegionExists b y s) x) supers)
 crosses = [(b,s) | s <- spaces, b <- (subsets 2 (boundariesOfSpace s))]
 in all (\(b,s) -> (myAttachedRegions b s) == (attachedRegions b s)) crosses

migrate :: Bool
migrate = let
 spaces = extendSpace (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])
 migrates = concat (map (\s -> map (\r -> migrateSpace r s) (filter (\r -> canMigrate r s) (regionsOfSpace s))) spaces)
 in all (\s -> isLinear 2 s) migrates

higher :: Bool
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
 in (all (\x -> (length x) == (length a)) unsorted0) &&
  (all (\x -> (length x) == 1) unsorted1) &&
  (all (\x -> (length x) == (length c)) unsorted2) &&
  (all (\x -> x == a) sorted0) &&
  ((concat unsorted1) == b) &&
  (all (\x -> x == c) sorted2) &&
  (ints == ([9,8,7,6,5],10)) &&
  (doubles == ([9.0,8.0,7.0,6.0,5.0],10)) &&
  (justs == (Just [4,3,2,1,0]))

