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
import AffTopo.Naive

foreign import ccall "generic" genericC :: Ptr CInt -> Ptr CInt -> IO (Ptr CChar)
foreign import ccall "click" clickC :: IO (Ptr CDouble)
foreign import ccall "error" errorC :: IO (Ptr CChar)
foreign import ccall "major" majorC :: IO CInt
foreign import ccall "mouse" mouseC :: IO CInt
foreign import ccall "roller" rollerC :: IO CInt
foreign import ccall "state" stateC :: IO CInt
foreign import ccall "event" eventC :: IO CInt

removeMe :: [Side]
removeMe = allSides

handleEvent :: IO Bool
handleEvent = fmap (4==) eventC
