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
  ("antisection",antisection)])

empty :: Bool
empty = (regionsOfSpace []) == [0]

simplex :: Bool
simplex = isLinear 2 (foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]])

antisection :: Bool
antisection = let
 space = foldl (\x y -> divideSpace y x) [] [[0],[0,1],[0,1,2]]
 func x = map (\y -> map (\z -> filter (\a -> member a x) z) y) space
 boundaries = boundariesOfSpace space
 regions = regionsOfSpace space
 num = defineLinear 1 (length boundaries)
 linears = filter (\x -> isLinear 1 (func x)) (subsets num regions)
 spaces = map (\x -> divideSpace x space) linears
 in all (\x -> isLinear 2 x) spaces
