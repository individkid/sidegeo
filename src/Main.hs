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
  ("corners",corners)])

empty :: Bool
empty = (regionsOfSpace []) == [0]

simplex :: Bool
simplex = isLinear 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])

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
 space = head (extendSpace (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]]))
 myAttachedRegions :: [Boundary] -> Space -> [Region]
 myAttachedRegions b s = let
  place = enumerate s
  subplace = foldl (\x y -> superSpaceF y x) place b
  supers = map (\x -> takeRegions subplace place [x]) (regionsOfSpace (range subplace))
  in concat (filter (\x -> all (\y -> oppositeOfRegionExists b y s) x) supers)
 in all (\b -> (myAttachedRegions b space) == (attachedRegions b space)) (subsets 2 (boundariesOfSpace space))
