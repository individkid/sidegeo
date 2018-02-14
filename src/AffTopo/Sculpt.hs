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
import Foreign.C.String
import AffTopo.Naive

foreign import ccall "place" placeC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "places" placesC :: CInt -> IO CInt
foreign import ccall "embed" embedC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "embeds" embedsC :: CInt -> IO CInt
foreign import ccall "filter" filterC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "filters" filtersC :: CInt -> IO CInt
foreign import ccall "inout" inoutC :: CInt -> IO (Ptr CInt)
foreign import ccall "inouts" inoutsC :: IO CInt
foreign export ccall handleEvent :: CInt -> IO Bool
foreign export ccall handleEnum :: Ptr CChar -> IO CInt

data Event =
    Locate | -- inout(wrt), place: inout(polyant)
    Fill | -- inout(polyant), place, embed: embed
    Hollow | -- inout(polyant), place, embed: embed
    Inflate | -- place: embed
    Face | -- inout(filter), place, embed, tag: inout(face)
    Frame |
    Slot |
    Free |
    Other |
    Both |
    Swap |
    Divide | -- inout(boundary, filter, wrt), place, embed, tag: place, embed, tag
    Vertex | -- place: inout(vertex)
    Corner | -- inout(boundary), place: inout(corner)
    Error

eventOf :: Int -> Event
eventOf 0 = Locate
eventOf 1 = Fill
eventOf 2 = Hollow
eventOf 3 = Inflate
eventOf 4 = Face
eventOf 5 = Frame
eventOf 6 = Slot
eventOf 7 = Free
eventOf 8 = Other
eventOf 9 = Both
eventOf 10 = Swap
eventOf 11 = Divide
eventOf 12 = Vertex
eventOf 13 = Corner
eventOf _ = Error

ofEvent :: Event -> Int
ofEvent Locate = 0
ofEvent Fill = 1
ofEvent Hollow = 2
ofEvent Inflate = 3
ofEvent Face = 4
ofEvent Frame = 5
ofEvent Slot = 6
ofEvent Free = 7
ofEvent Other = 8
ofEvent Both = 9
ofEvent Swap = 10
ofEvent Divide = 11
ofEvent Vertex = 12
ofEvent Corner = 13
ofEvent _ = (-1)

ofString :: [Char] -> Event
ofString "Locate" = Locate
ofString "Fill" = Fill
ofString "Hollow" = Hollow
ofString "Inflate" = Inflate
ofString "Face" = Face
ofString "Frame" = Frame
ofString "Slot" = Slot
ofString "Free" = Free
ofString "Other" = Other
ofString "Both" = Both
ofString "Swap" = Swap
ofString "Divide" = Divide
ofString "Vertex" = Vertex
ofString "Corner" = Corner
ofString _ = Error

handleEnum :: Ptr CChar -> IO CInt
handleEnum cstr = do
 str <- peekCString cstr
 return (fromIntegral (ofEvent (ofString str)))

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
 Locate -> return False
 Fill -> return False
 Hollow -> return False
 Inflate -> return False
 Face -> return False
 Frame -> return False
 Slot -> return False
 Free -> return False
 Other -> return False
 Both -> return False
 Swap -> return False
 Divide -> return False
 Vertex -> return False
 Corner -> return False
 _ -> return True
