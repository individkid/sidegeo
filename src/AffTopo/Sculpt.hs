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

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import AffTopo.Naive

-- foreign import ccall "generic" genericC :: Ptr CInt -> Ptr CInt -> IO (Ptr CChar)
foreign import ccall "message" messageC :: IO (Ptr CChar)
-- foreign import ccall "mode" modeC :: IO CInt
-- foreign import ccall "mouse" mouseC :: IO CInt
-- foreign import ccall "roller" rollerC :: IO CInt
-- foreign import ccall "state" stateC :: IO CInt
foreign import ccall "event" eventC :: IO CInt
foreign export ccall "randomizeH" randomize :: IO ()

removeMe :: [Side]
removeMe = allSides

handleEvent :: IO Bool
handleEvent = do
 event <- eventC
 case event of
  3 -> do
   ptr <- messageC
   str <- peekCString ptr
   error str
  4 -> return True
  _ -> return False

randomize :: IO ()
randomize = undefined
