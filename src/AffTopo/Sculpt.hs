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
type Sideband = (Boundary,[Int])

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

chip :: Ptr CInt -> IO (Int, Ptr CInt)
chip ptr = do
 val <- peek ptr
 return (fromIntegral val, plusPtr ptr 1)

peel :: Int -> Ptr CInt -> IO ([Int], Ptr CInt)
peel len ptr = do
 list <- peekArray len ptr
 return (map fromIntegral list, plusPtr ptr len)

cook :: Monad m => (a -> b -> m (c, b)) -> [a] -> b -> m ([c], b)
cook fun len ptr = do
 (c,b) <- fold' (cookF fun) len (return ([],ptr))
 return ((reverse c), b)

cookF :: Monad m => (a -> b -> m (c, b)) -> a -> m ([c],b) -> m ([c],b)
cookF f a d = do
 (c,b) <- d
 (c',b') <- f a b
 return ((c' : c), b')

onion :: [Int] -> Ptr CInt -> IO ([[Int]], Ptr CInt)
onion len ptr = cook peel len ptr

patch :: [[Int]] -> Ptr CInt -> IO ([[[Int]]], Ptr CInt)
patch len ptr = cook onion len ptr

-- (num-places,[place-size],[num-regions],[[boundary]],[[region]],
--  [[first-halfspace-size]],[[second-halfspace-size]],[[[first-halfspace-region]]],[[[second-halfspace-region]]],
--  [embeded-region-size],[[embeded-region]])
readGeneric :: IO Generic
readGeneric = do
 ptr <- genericC 0
 (places,placesPtr) <- chip ptr
 (boundaries,boundariesPtr) <- peel places placesPtr
 (regions,regionsPtr) <- peel places boundariesPtr
 (boundary,boundaryPtr) <- onion boundaries regionsPtr
 (_,regionPtr) <- onion regions boundaryPtr
 (firsts,firstsPtr) <- onion boundaries regionPtr
 (seconds,secondsPtr) <- onion boundaries firstsPtr
 (first,firstPtr) <- patch firsts secondsPtr
 (second,secondPtr) <- patch seconds firstPtr
 (embeds,embedsPtr) <- peel places secondPtr
 (embed,_) <- onion embeds embedsPtr
 let a = map2 Boundary boundary
 let b = map3 Region first
 let c = map3 Region second
 let d = map2 Region embed
 return (zip (zipWith3 (zipWith3 readGenericF) a b c) d)

--num-done,num-todo,[place-todo]--
readSideband :: IO Sideband
readSideband = do
 ptr <- sidebandC 0
 (dones,donesPtr) <- chip ptr
 (todos,todosPtr) <- chip donesPtr
 (todo,_) <- peel todos todosPtr
 return (Boundary dones, todo)

readGenericF :: Boundary -> [Region] -> [Region] -> (Boundary,[[Region]])
readGenericF a b c = (a,[b,c])

paste :: Int -> Ptr CInt -> IO (Ptr CInt)
paste = undefined

cover :: [Int] -> Ptr CInt -> IO (Ptr CInt)
cover = undefined

layer :: [[Int]] -> Ptr CInt -> IO (Ptr CInt)
layer = undefined

hoard :: [[[Int]]] -> Ptr CInt -> IO (Ptr CInt)
hoard = undefined

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

writeSideband :: Sideband -> IO (Ptr CInt)
writeSideband (a,b) = let
 dones = (\(Boundary x) -> x) a
 todos = length b
 in (sidebandC (fromIntegral (todos+2))) >>=
 (paste dones) >>=
 (paste todos) >>=
 (cover b)

