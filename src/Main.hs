--    Glfw start glfw wait loop
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

module Main where

import Data.Char
import System.Environment
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.C.String
import AffTopo.Sculpt

foreign export ccall "showIntHs" showInt :: Ptr CChar -> CInt -> Ptr CChar -> IO ()

foreign import ccall "initialize" initializeC :: CInt -> Ptr (Ptr CChar) -> IO ()
foreign import ccall "finalize" finalizeC :: IO ()
foreign import ccall "waitForEvent" waitForEventC :: IO CInt

showInt :: Ptr CChar -> CInt -> Ptr CChar -> IO ()
showInt = undefined

main :: IO ()
main = do
 args <- getArgs
 let cargs = map (map castCharToCChar) args
 let term = castCharToCChar (chr 0)
 ptrs <- mapM (newArray0 term) cargs
 ptr <- newArray ptrs
 initializeC (fromIntegral (length ptrs)) ptr
 mainF
 free ptr
 mapM_ free ptrs

mainF :: IO ()
mainF = do
 code <- waitForEventC
 mainG (fromIntegral code)

mainG :: Int -> IO ()
mainG 0 = do
 handleSideband
 mainF
mainG 1 = do
 handleLeft
 mainF
mainG 2 = do
 handleRight
 mainF
mainG _ = finalizeC
