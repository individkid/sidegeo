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
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import AffTopo.Naive

foreign import ccall "event" eventC :: IO CInt
foreign import ccall "input" inputC :: CInt -> IO (Ptr CInt)
foreign import ccall "output" outputC :: CInt -> IO (Ptr CInt)
foreign import ccall "mapping" mappingC :: CInt -> CInt -> IO CInt
foreign export ccall handleEvents :: IO Bool
foreign export ccall handleEnum :: Ptr CChar -> IO CInt

data State = State Place [Region] [Int]

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

handleInput :: IO Int
handleInput = (inputC 1) >>= peek >>= (return . fromIntegral)

handleInputs :: IO [Int]
handleInputs = do
 len <- handleInput
 lst <- (inputC (fromIntegral len)) >>= (peekArray len)
 return (map fromIntegral lst)

handleOutput :: Int -> IO ()
handleOutput a = do
 ptr <- (outputC 1)
 poke ptr (fromIntegral a)

handleOutputs :: [Int] -> IO ()
handleOutputs a = do
 ptr <- (outputC (fromIntegral (length a)))
 pokeArray ptr (map fromIntegral a)

handleLocateA :: [Int] -> State -> [Int]
handleLocateA = undefined -- TODO1 inside attachments

handleLocateB :: [Int] -> State -> [Int]
handleLocateB = undefined -- TODO1 outside attachments

handleState :: Event -> State -> IO State
handleState Locate state = do
 wrt <- handleInputs
 handleOutputs (handleLocateA wrt state)
 handleOutputs (handleLocateB wrt state)
 handleInput >>= handleOutput
 return state
handleState Fill state = return state -- TODO1
handleState Hollow state = return state -- TODO1
handleState Inflate state = return state -- TODO1
handleState Faces state = return state -- TODO1
handleState Frames state = return state -- TODO2
handleState Face state = return state -- TODO2
handleState Frame state = return state -- TODO2
handleState Get state = return state -- TODO2
handleState Set state = return state -- TODO2
handleState Divide state = return state -- TODO1
handleState Vertex state = return state -- TODO1
handleState Index state = return state -- TODO1
handleState _ state = return state

handleStates :: [State] -> Event -> IO Bool
handleStates _ Done = return False
handleStates _ Error = return True
handleStates s e = do
 index <- handleInput
 state <- (handleState e (s !! index))
 handleEvent (replace index state s)

handleEvent :: [State] -> IO Bool
handleEvent s = eventC >>= ((handleStates s) . eventOf . fromIntegral)

handleEvents :: IO Bool
handleEvents = handleEvent [State [] [] []]

