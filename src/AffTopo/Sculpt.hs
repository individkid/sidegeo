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
foreign import ccall "sideband" sidebandC :: CInt -> IO (Ptr CInt)
foreign import ccall "sidebands" sidebandsC :: IO CInt
foreign import ccall "correlate" correlateC :: CInt -> IO (Ptr CInt)
foreign import ccall "correlates" correlatesC :: IO CInt
foreign import ccall "boundary" boundaryC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaries" boundariesC :: CInt -> IO CInt
foreign import ccall "client" clientC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "clients" clientsC :: CInt -> IO CInt
foreign import ccall "eventName" eventNameC :: CInt -> CInt -> IO (Ptr CChar)
foreign import ccall "kindName" kindNameC :: CInt -> CInt -> IO (Ptr CChar)
foreign import ccall "clientName" clientNameC :: CInt -> CInt -> IO (Ptr CChar)
foreign import ccall "eventArgument" eventArgumentC :: IO CInt
foreign import ccall "kindArgument" kindArgumentC :: IO CInt
foreign import ccall "stringArgument" stringArgumentC :: IO (Ptr CChar)
foreign import ccall "intArgument" intArgumentC :: IO CInt
foreign export ccall handleEvent :: IO Bool

data Event = Enumerate | Plane | Classify | Inflate | Fill | Hollow | Remove | Call | Error

eventOf :: Int -> Event
eventOf (-1) = Enumerate
eventOf 0 = Plane
eventOf 1 = Classify
eventOf 2 = Inflate
eventOf 3 = Fill
eventOf 4 = Hollow
eventOf 5 = Remove
eventOf 6 = Call
eventOf _ = Error

ofEvent :: Event -> Int
ofEvent Enumerate = (-1)
ofEvent Plane = 0
ofEvent Classify = 1
ofEvent Inflate = 2
ofEvent Fill = 3
ofEvent Hollow = 4
ofEvent Remove = 5
ofEvent Call = 6
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

handleEvent :: IO Bool
handleEvent = do
 event <- eventArgumentC
 case (eventOf (fromIntegral event)) of
  Plane -> return False
  Classify -> return False
  Inflate -> return False
  Fill -> return False
  Hollow -> return False
  Remove -> return False
  Call -> return False
  Enumerate -> handleInitEvent >> handleInitKind >> handleInitClient >> return False
  _ -> return True

handleInitEvent :: IO ()
handleInitEvent = undefined

handleInitKind :: IO ()
handleInitKind = undefined

handleInitClient :: IO ()
handleInitClient = undefined
