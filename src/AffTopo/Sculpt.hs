--    AffTopo.Sculpt functions for manipulating polytopes
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

{-# LANGUAGE ForeignFunctionInterface #-}

module AffTopo.Sculpt where

import Prelude hiding ((++))
import Foreign.Ptr
import Foreign.C.Types
import AffTopo.Naive

foreign import ccall "place" placeC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "places" placesC :: CInt -> IO CInt
foreign import ccall "embed" embedC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "embeds" embedsC :: CInt -> IO CInt
foreign import ccall "inout" inoutC :: CInt -> IO (Ptr CInt)
foreign import ccall "inouts" inoutsC :: IO CInt
foreign export ccall handleEvent :: CInt -> IO Bool

data Event = Pierce | Fill | Hollow | Face | Frame | Inflate | Divide | Vertex | Migrate | Error

eventOf :: Int -> Event
eventOf 0 = Pierce
eventOf 1 = Fill
eventOf 2 = Hollow
eventOf 3 = Face
eventOf 4 = Frame
eventOf 5 = Inflate
eventOf 6 = Divide
eventOf 7 = Vertex
eventOf 8 = Migrate
eventOf _ = Error

ofEvent :: Event -> Int
ofEvent Pierce = 0
ofEvent Fill = 1
ofEvent Hollow = 2
ofEvent Face = 3
ofEvent Frame = 4
ofEvent Inflate = 5
ofEvent Divide = 6
ofEvent Vertex = 7
ofEvent Migrate = 8
ofEvent _ = undefined

split :: [a] -> [Int] -> [[a]]
split [] _ = []
split _ [] = []
split a (b:c) = (take b a):(split (drop b a) c)

decodePlace :: [Int] -> [Int] -> Place
decodePlace boundaryI list = let
 size = length boundaryI
 boundary = map Boundary boundaryI
 (firsts,list1) = splitAt size list
 (seconds,list2) = splitAt size list1
 (list3,list4) = splitAt (sum firsts) list2
 first = map2 Region (split list3 firsts)
 second = map2 Region (split list4 seconds)
 in zipWith3 (\x y z -> (x,[y,z])) boundary first second

encodePlace :: Place -> [Int]
encodePlace place = let
 firsts = map (length . head) (range place)
 seconds = map (length . last) (range place)
 first = concat (map head (range place))
 second = concat (map last (range place))
 in concat [firsts, seconds, map (\(Region x) -> x) first, map (\(Region x) -> x) second]

handleEvent :: CInt -> IO Bool
handleEvent event = case (eventOf (fromIntegral event)) of
 Pierce -> return False
 Fill -> return False
 Hollow -> return False
 Face -> return False
 Frame -> return False
 Inflate -> return False
 Divide -> return False
 Vertex -> return False
 Migrate -> return False
 _ -> return True
