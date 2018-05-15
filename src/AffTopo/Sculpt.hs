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

foreign import ccall "event" eventC :: IO CInt
foreign import ccall "mapping" mappingC :: CInt -> CInt -> IO CInt
foreign export ccall handleEvents :: IO Bool
foreign export ccall handleEnum :: Ptr CChar -> IO CInt

data State =
    PlaceState Place [Region] [Int] |
    DoneState |
    ErrorState

data Event =
    Locate | -- inout(wrt), place: inout(polyant)
    Fill | -- inout(polyant), place, embed: embed
    Hollow | -- inout(polyant), place, embed: embed
    Inflate | -- place: embed
    Faces | -- inout(filter), place, embed, tag: inout(face)
    Frames | -- inout(filter), place, embed, tag: inout(frame)
    Face | -- inout(boundary), place, embed: inout(face)
    Frame | -- inout(boundary), place, embed: inout(frame)
    Get | -- inout(boundary), tag: inout(mask)
    Set | -- inout(boundary,mask), tag: tag
    Divide | -- inout(boundary, filter, wrt), place, embed, tag: place, embed, tag
    Vertex | -- inout(boundary), place: inout(vertex)
    Index | -- inout(boundary), place: inout(index)
    Done |
    Error

eventOf :: Int -> Event
eventOf 0 = Locate
eventOf 1 = Fill
eventOf 2 = Hollow
eventOf 3 = Inflate
eventOf 4 = Faces
eventOf 5 = Frames
eventOf 6 = Face
eventOf 7 = Frame
eventOf 8 = Get
eventOf 9 = Set
eventOf 10 = Divide
eventOf 11 = Vertex
eventOf 12 = Index
eventOf 13 = Done
eventOf _ = Error

ofEvent :: Event -> Int
ofEvent Locate = 0
ofEvent Fill = 1
ofEvent Hollow = 2
ofEvent Inflate = 3
ofEvent Faces = 4
ofEvent Frames = 5
ofEvent Face = 6
ofEvent Frame = 7
ofEvent Get = 8
ofEvent Set = 9
ofEvent Divide = 10
ofEvent Vertex = 11
ofEvent Index = 12
ofEvent Done = 13
ofEvent _ = (-1)

ofString :: [Char] -> Event
ofString "Locate" = Locate
ofString "Fill" = Fill
ofString "Hollow" = Hollow
ofString "Inflate" = Inflate
ofString "Faces" = Faces
ofString "Frames" = Frames
ofString "Face" = Face
ofString "Frame" = Frame
ofString "Get" = Get
ofString "Set" = Set
ofString "Divide" = Divide
ofString "Vertex" = Vertex
ofString "Index" = Index
ofString "Done" = Done
ofString _ = Error

handleEnum :: Ptr CChar -> IO CInt
handleEnum cstr = do
 str <- peekCString cstr
 return (fromIntegral (ofEvent (ofString str)))

handleState :: Event -> State -> State
handleState event state = case event of
 Locate -> state
 Fill -> state
 Hollow -> state
 Inflate -> state
 Faces -> state
 Frames -> state
 Face -> state
 Frame -> state
 Get -> state
 Set -> state
 Divide -> state
 Vertex -> state
 Index -> state
 Done -> DoneState
 Error -> ErrorState

handleStates :: State -> IO State
handleStates state = do
 event <- eventC
 let e = eventOf (fromIntegral event) in
  return (handleState e state)

handleEvent :: State -> IO Bool
handleEvent state = do
 s <- handleStates state
 case s of
  DoneState -> return False
  ErrorState -> return True
  _ -> handleEvent s

handleEvents :: IO Bool
handleEvents = handleEvent (PlaceState [] [] [])
