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

-- import Foreign.Ptr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.C.String
import AffTopo.Naive

type Generic = [(Place,[Region])]
type Sideband = ([[Boundary]],Boundary,[Int])

foreign import ccall "generic" genericC :: CInt -> IO (Ptr CInt)
foreign import ccall "sideband" sidebandC :: CInt -> IO (Ptr CInt)
foreign import ccall "side" sideC :: IO (Ptr CInt)
foreign import ccall "face" faceC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "point" pointC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaryWrt" boundaryWrtC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaryOk" boundaryOkC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "faceValid" faceValidC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaryCount" boundaryCountC :: IO CInt
foreign import ccall "print" printC :: CInt -> IO (Ptr CChar)
foreign import ccall "event" eventC :: IO CInt
foreign import ccall "intArgument" intArgumentC :: IO CInt

removeMe :: [Side]
removeMe = allSides

printStr :: [Char] -> IO ()
printStr str = do
 ptr <- printC (fromIntegral (length str))
 pokeArray ptr (map castCharToCChar str)

handleEvent :: IO Bool
handleEvent = do
 event <- eventC
 case event of
  0 -> do
   printStr "plane\n"
   return False
  1 -> do
   printStr "inflate\n"
   return False
  2 -> do
   face <- intArgumentC
   printStr (concat ["fill ",(show face),"\n"])
   return False
  3 -> do
   face <- intArgumentC
   printStr (concat ["hollow ",(show face),"\n"])
   return False
  4 -> return True
  5 -> return True
  _ -> return False

chip :: (a, Ptr CInt) -> IO (Int, Ptr CInt)
chip (_,ptr) = (peek ptr) >>= (\x -> return (fromIntegral x, plusPtr ptr 1))

peel :: (Int, Ptr CInt) -> (a, Ptr CInt) -> IO ([Int], Ptr CInt)
peel (len,_) (_,ptr) = (peekArray len ptr) >>= (\x -> return (map fromIntegral x, plusPtr ptr len))

cook :: [IO (a, Ptr CInt)] -> IO ([a], Ptr CInt)
cook (elm:mid:lst) = elm >>= (\(x,_) -> (cook (mid:lst)) >>= (\(y,z) -> return (x:y,z)))
cook [elm] = elm >>= (\(x,y) -> return ([x],y))
cook [] = return undefined

onion :: ([Int], Ptr CInt) -> (a, Ptr CInt) -> IO ([[Int]], Ptr CInt)
onion (len,_) (_,ptr) = cook (map (\x -> peel (x,ptr) (x,ptr)) len)

patch :: ([[Int]], Ptr CInt) -> (a, Ptr CInt) -> IO ([[[Int]]], Ptr CInt)
patch (len,_) (_,ptr) = cook (map (\x -> onion (x,ptr) (x,ptr)) len)

--num-places,[num-boundaries],[[boundary]],num-done,num-todo,[place-todo]--
readSideband :: IO Sideband
readSideband = (sidebandC 0) >>= (\ptr -> return (0::Int,ptr) >>=
 chip >>= (\places -> return places >>=
 (peel places) >>= (\boundaries -> return boundaries >>=
 (onion boundaries) >>= (\boundary -> return boundary >>=
 chip >>= (\dones -> return dones >>=
 chip >>= (\todos -> return todos >>=
 (peel todos) >>= (\todo ->
 return (map2 Boundary (fst boundary), Boundary (fst dones), (fst todo)))))))))

-- (num-places,[num-boundaries],[num-regions],[[boundary]],[[region]],
--  [[first-halfspace-size]],[[second-halfspace-size]],[[[first-halfspace-region]]],[[[second-halfspace-region]]],
--  [embeded-region-size],[[embeded-region]])
readGeneric :: IO Generic
readGeneric = (genericC 0) >>= (\ptr -> return (0::Int,ptr) >>=
 chip >>= (\places -> return places >>=
 (peel places) >>= (\boundaries -> return boundaries >>=
 (peel places) >>= (\regions -> return regions >>=
 (onion boundaries) >>= (\boundary -> return boundary >>=
 (onion regions) >>= (\region -> return region >>=
 (onion boundaries) >>= (\firsts -> return firsts >>=
 (onion boundaries) >>= (\seconds -> return seconds >>=
 (patch firsts) >>= (\first -> return first >>=
 (patch seconds) >>= (\second -> return second >>=
 (peel places) >>= (\embeds -> return embeds >>=
 (onion embeds) >>= (\embed -> let
 w = map2 Boundary (fst boundary)
 x = map3 Region (fst first)
 y = map3 Region (fst second)
 z = map2 Region (fst embed)
 in return (zip (zipWith3 (zipWith3 readGenericF) w x y) z)))))))))))))

readGenericF :: Boundary -> [Region] -> [Region] -> (Boundary,[[Region]])
readGenericF a b c = (a,[b,c])

paste :: Int -> Ptr CInt -> IO (Ptr CInt)
paste len ptr = poke ptr (fromIntegral len) >>= (\x -> seq x (return (plusPtr ptr 1)))

cover :: [Int] -> Ptr CInt -> IO (Ptr CInt)
cover len ptr = pokeArray ptr (map fromIntegral len) >>= (\x -> seq x (return (plusPtr ptr (length len))))

layer :: [[Int]] -> Ptr CInt -> IO (Ptr CInt)
layer len ptr = fold' (\x y -> y >>= (\z -> cover x z)) len (return ptr)

hoard :: [[[Int]]] -> Ptr CInt -> IO (Ptr CInt)
hoard len ptr = fold' (\x y -> y >>= (\z -> layer x z)) len (return ptr)

writeGeneric :: Generic -> IO (Ptr CInt)
writeGeneric a = let
 places = length a
 boundaries = map (length . boundariesOfPlace) (domain a)
 regions = map (length . regionsOfPlace) (domain a)
 boundary = map boundariesOfPlace (domain a)
 region = map regionsOfPlace (domain a)
 firsts = map ((map (length . head)) . range) (domain a)
 seconds = map ((map (length . head . tail)) . range) (domain a)
 first = map ((map head) . range) (domain a)
 second = map ((map (head . tail)) . range) (domain a)
 embeds = map length (range a)
 embed = range a
 size0 = 1 + places + places + (writeGenericF boundary) + (writeGenericF region)
 size1 = (writeGenericF firsts) + (writeGenericF seconds)
 size2 = (writeGenericG first) + (writeGenericG second)
 size3 = places + (writeGenericF embed)
 size = size0 + size1 + size2 + size3
 in (genericC (fromIntegral size)) >>=
 (paste places) >>=
 (cover boundaries) >>=
 (cover regions) >>=
 (layer (map2 (\(Boundary x) -> x) boundary)) >>=
 (layer (map2 (\(Region x) -> x) region)) >>=
 (layer firsts) >>=
 (layer seconds) >>=
 (hoard (map3 (\(Region x) -> x) first)) >>=
 (hoard (map3 (\(Region x) -> x) second)) >>=
 (cover embeds) >>=
 (layer (map2 (\(Region x) -> x) embed))

writeGenericF :: [[a]] -> Int
writeGenericF a = fold' (+) (map length a) 0

writeGenericG :: [[[a]]] -> Int
writeGenericG a = fold' (+) (map writeGenericF a) 0

--type Sideband = ([[Boundary]],Boundary,[Int])
--num-places,[num-boundaries],[[boundary]],num-done,num-todo,[place-todo]--
writeSideband :: Sideband -> IO (Ptr CInt)
writeSideband (a,b,c) = let
 places = length a
 boundaries = map length a
 boundary = map2 (\(Boundary x) -> x) a
 dones = (\(Boundary x) -> x) b
 todos = length c
 size = 1 + places + (writeGenericF a) + 2 + todos
 in (sidebandC (fromIntegral size)) >>=
 (paste places) >>=
 (cover boundaries) >>=
 (layer boundary) >>=
 (paste dones) >>=
 (paste todos) >>=
 (cover c)

