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

foreign import ccall "generic" genericC :: CInt -> IO (Ptr CInt)
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

onion :: [Int] -> Ptr CInt -> IO ([[Int]], Ptr CInt)
onion len ptr = do
 ptrs <- return (scanl plusPtr ptr len)
 lens <- sequence (zipWith peekArray len (ptr:(init ptrs)))
 return (map2 fromIntegral lens, last ptrs)

patch :: [[Int]] -> Ptr CInt -> IO ([[[Int]]], Ptr CInt)
patch = undefined

-- (num-places,[place-size],[num-regions],[[boundary]],[[region]],
--  [[first-halfspace-size]],[[second-halfspace-size]],[[[first-halfspace-region]]],[[[second-halfspace-region]]],
--  [embeded-region-size],[[embeded-region]])
placeFromGeneric :: IO Place
placeFromGeneric = do
 -- ptr0 <- genericC 0
 -- (places,ptr1) <- chip ptr0
 -- (boundaries,ptr2) <- peel places ptr1
 -- (regions,ptr3) <- peel places ptr2
 -- (boundary,ptr4) <- onion boundaries ptr3
 -- (region,ptr5) <- onion regions ptr4
 -- (firsts,ptr6) <- onion boundaries ptr5
 -- (seconds,ptr7) <- onion boundaries ptr6
 -- (first,ptr8) <- patch firsts ptr7
 -- (second,ptr9) <- patch seconds ptr8
 -- (embeds,ptr10) <- peel places ptr9
 -- (embed,_) <- onion embeds ptr10
 return []
