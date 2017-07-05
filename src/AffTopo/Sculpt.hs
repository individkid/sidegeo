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
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.C.String
import AffTopo.Naive

foreign import ccall "generic" genericC :: CInt -> IO (Ptr CInt)
foreign import ccall "face" faceC :: CInt -> IO (Ptr CInt)
foreign import ccall "sidedness" sidednessC :: IO (Ptr CInt)
foreign import ccall "boundaryWrt" boundaryWrtC :: IO (Ptr CInt)
foreign import ccall "boundaryOk" boundaryOkC :: IO (Ptr CInt)
foreign import ccall "faceOk" faceOkC :: IO (Ptr CInt)
foreign import ccall "boundaryCount" boundaryCountC :: IO CInt
foreign import ccall "event" eventC :: IO CInt
foreign import ccall "print" printC :: CInt -> IO (Ptr CChar)

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
    printStr "inflate\n"
    return False
  3 -> return True
  4 -> return True
  _ -> return False
