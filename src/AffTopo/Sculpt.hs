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
import Data.Bits ((.&.))
import Data.List (sort)
import AffTopo.Naive

foreign import ccall "event" eventC :: IO CInt
foreign import ccall "input" inputC :: CInt -> IO (Ptr CInt)
foreign import ccall "output" outputC :: CInt -> IO (Ptr CInt)
foreign import ccall "mapping" mappingC :: CInt -> CInt -> IO CInt
foreign export ccall handleEvents :: IO Bool
foreign export ccall handleEnum :: Ptr CChar -> IO CInt

data State = State Place [Region] [(Boundary,Int)]

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
    Filter | -- inout(mask), tag: tag
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
eventOf 10 = Filter
eventOf 11 = Divide
eventOf 12 = Vertex
eventOf 13 = Index
eventOf 14 = Done
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
ofEvent Filter = 10
ofEvent Divide = 11
ofEvent Vertex = 12
ofEvent Index = 13
ofEvent Done = 14
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
ofString "Filter" = Filter
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

handleLocate :: Int -> [Int] -> State -> ([Int],[Int])
handleLocate a b (State c _ _) = let
 space = placeToSpace c
 sides = map Side b
 region = regionOfSides sides space
 bound = Boundary a
 other = oppositeOfRegion [bound] region space
 onbound = attachedBoundaries region space
 offbound = attachedBoundaries other space
 bounds = (onbound +\ offbound) \\ [bound]
 regions d = filter (\x -> (regionWrtBoundary x region space) == (Side d)) bounds
 result d = map (\(Boundary x) -> x) (regions d)
 in (result 0, result 1)

handleFillG :: [Int] -> Int -> Place -> [Region]
handleFillG b c d = let
 bound = map Boundary b
 sub = unsubSpace bound d
 side = map (\_ -> Side c) bound
 reg = regionOfSides side (placeToSpace sub)
 embed = embedSpace [reg] sub
 in takeRegions embed d

handleFillF :: Int -> [Int] -> [Int] -> Place -> [Region]
handleFillF a b c d = let
 space = placeToSpace d
 regs = attachedRegions [Boundary a] space
 inregs = handleFillG b 0 d
 outregs = handleFillG c 1 d
 inside x = not (outsideOfRegionExists x space)
 in filter inside ((inregs +\ outregs) +\ regs)

handleFill :: Int -> [Int] -> [Int] -> State -> State
handleFill a b c (State d e f) = State d (e ++ (handleFillF a b c d)) f

handleHollow :: Int -> [Int] -> [Int] -> State -> State
handleHollow a b c (State d e f) = State d (e \\ (handleFillF a b c d)) f

handleInflate :: State -> State
handleInflate (State a _ c) = let
 regions = regionsOfPlace a
 space = placeToSpace a
 outside x = outsideOfRegionExists x space
 embed = filter outside regions
 in State a embed c

handleFacesI :: [Boundary] -> [Int]
handleFacesI = map (\(Boundary x) -> x)

handleFacesH :: Int -> [(Boundary,Int)] -> [Boundary]
handleFacesH a b = domain (filter (\(_,x) -> (a .&. x) /= 0) b)

handleFacesG :: Boundary -> Place -> ([Boundary] -> [Int]) -> Region -> [Int]
handleFacesG a b f c = let -- choose base segment and return sextuples of fan
 section = sectionSpace a b
 single = embedSpace [c] b
 region = head (takeRegions single section)
 corner = attachedFacets 2 region (placeToSpace section)
 bound = head (head corner)
 other [x,y]
  | x == bound = y
  | otherwise = x
 other _ = undefined
 either_ [x,y] = (x == bound) || (y == bound)
 either_ _ = undefined
 neither_ [x,y] = (x /= bound) && (y /= bound)
 neither_ _ = undefined
 endpoint = map other (filter either_ corner)
 apex = filter neither_ corner
 in concat (map (f . (append (concat [[a],[bound],endpoint]))) apex)

handleFacesF :: Place -> [Region] -> ([Boundary] -> [Int]) -> Boundary -> [Int]
handleFacesF a b f c = let -- concat of sextuples with given boundary as base
 space = placeToSpace a
 attached = attachedRegions [c] space
 mapping = map (\x -> (x, oppositeOfRegion [c] x space)) attached
 -- those of embed intersect attached whose neighbor is not in embed
 regions = (preimage (attached \\ b) mapping) +\ b
 in concat (map (handleFacesG c a f) regions)

handleFaces :: Int -> State -> [Int]
handleFaces a (State b c d) = let
 bounds = handleFacesH a d
 in concat (map (handleFacesF b c handleFacesI) bounds)

-- 0,1,2
-- 0,1,3 0,2,3 1,2,3
-- 0,1,4 0,2,4 1,2,4 0,3,4 1,3,4 2,3,4
-- 0,1,5 0,2,5 1,2,5 0,3,5 1,3,5 2,3,5 0,4,5 1,4,5 2,4,5 3,4,5
-- f(a,b,c) = tet(c-2)+tri(b-1)+a
handleFramesG :: [Int] -> Int
handleFramesG [a,b,c] = (figurate 3 (c-2)) + (figurate 2 (b-1)) + a
handleFramesG _ = undefined

handleFramesF :: [Boundary] -> [Int]
handleFramesF [a,b,c,d,e,f] = let
 triples = map2 (\(Boundary x) -> x) [[a,b,c],[a,b,d],[a,e,f]]
 in map (handleFramesG . sort) triples
handleFramesF _ = undefined

handleFrames :: Int -> State -> [Int]
handleFrames a (State b c d) = let -- return vertex index triples
 bounds = handleFacesH a d
 in concat (map (handleFacesF b c handleFramesF) bounds)

handleFace :: Int -> State -> [Int]
handleFace a (State b c _) = concat (map (handleFacesF b c handleFacesI) [Boundary a])

handleFrame :: Int -> State -> [Int]
handleFrame a (State b c _) = concat (map (handleFacesF b c handleFramesF) [Boundary a])

handleGet :: Int -> State -> Int
handleGet a (State _ _ b) = head (image [Boundary a] b) -- return mask associated with given boundary

handleSet :: Int -> Int -> State -> State
handleSet a b (State c d e) = let -- set mask associated with given boundary
 bound = Boundary a
 index = findIndex' (\(x,_) -> x == bound) e
 in State c d (replace index (bound,b) e)

handleFilter :: Int -> (Boundary,Int) -> IO (Boundary,Int)
handleFilter a (b,c) = do
 mask <- mappingC (fromIntegral a) (fromIntegral c)
 return (b, fromIntegral mask)

handleDivide :: Int -> Int -> [Int] -> State -> State
handleDivide a m b (State c d e) = let -- change or add given boundary with given vertex sidednesses
 boundary = Boundary a
 sides = map Side b
 boundaries = boundariesOfPlace c
 vertices = map sort (subsets 3 boundaries)
 triples = map2 (\(Boundary x) -> x) vertices
 sortable = map handleFramesG triples
 sorted = sort (zip sortable vertices)
 -- find map from boundary triple to sidedness
 mapping = map (\((_,x),y) -> (x,y)) (zip sorted sides)
 -- find subspace with given plane missing
 -- add mask if plane already missing
 (subspace,masks) = handleDivideF boundary m c e
 subregions = regionsOfPlace subspace
 -- find regions with vertex facets on both sides
 regions = filter (handleDivideG mapping subspace) subregions
 -- take embed to subspace
 embed = embedSpace d c
 embeds = takeRegions embed subspace
 -- divide those regions
 degen = embedSpace regions subspace
 divided = divideSpace boundary degen subspace
 -- find vertex without added boundary
 vertex = find' (\x -> not (any (\y -> y == boundary) x)) vertices
 side = head (image [vertex] mapping)
 -- choose divided that puts vertex on correct side
 chosen = handleDivideH boundary vertex side divided
 -- take embed from subspace to divided
 regen = embedSpace embeds subspace
 taken = takeRegions regen chosen
 in State chosen taken masks

handleDivideF :: Boundary -> Int -> Place -> [(Boundary,Int)] -> (Place, [(Boundary,Int)])
handleDivideF a m b c
 | a > bound = undefined -- TODO2 return error from handleEvents
 | a < bound = (subSpace a b, c)
 -- TODO2 use given mask
 | otherwise = (b, append c [(a,m)]) where
 bound = Boundary (length b)

handleDivideG :: [([Boundary],Side)] -> Place -> Region -> Bool
handleDivideG a b c = let
 corners = attachedFacets 3 c (placeToSpace b)
 side y = any (\x -> (head (image [x] a)) == y) corners
 in (side (Side 0)) && (side (Side 1))

handleDivideH :: Boundary -> [Boundary] -> Side -> [Place] -> Place
handleDivideH a b c [d,e]
 | (vertexWrtBoundary a b (placeToSpace d)) == c = d
 | otherwise = e
handleDivideH _ _ _ _ = undefined

handleVertexF :: Int -> State -> [[Int]]
handleVertexF a (State b _ _) = let
 pairs = subsets 2 ((indices (length b)) \\ [a])
 triples = map (\x -> a:x) pairs
 in map sort triples

handleVertex :: Int -> State -> [Int]
handleVertex a b = concat (handleVertexF a b) -- return triples of boundary with given

handleIndex :: Int -> State -> [Int]
handleIndex a b = map handleFramesG (handleVertexF a b) -- return indices of triples of boundary with given

handleState :: Event -> State -> IO State
handleState Locate state = do
 plane <- handleInput
 wrt <- handleInputs
 handleOutputs (fst (handleLocate plane wrt state))
 handleOutputs (snd (handleLocate plane wrt state))
 handleInput >>= handleOutput
 return state
handleState Fill state = do
 plane <- handleInput
 inside <- handleInputs
 outside <- handleInputs
 handleInput >>= handleOutput
 return (handleFill plane inside outside state)
handleState Hollow state = do
 plane <- handleInput
 inside <- handleInputs
 outside <- handleInputs
 handleInput >>= handleOutput
 return (handleHollow plane inside outside state)
handleState Inflate state = do
 handleInput >>= handleOutput
 return (handleInflate state)
handleState Faces state = do
 mask <- handleInput
 handleOutputs (handleFaces mask state)
 handleInput >>= handleOutput
 return state
handleState Frames state = do
 mask <- handleInput
 handleOutputs (handleFrames mask state)
 handleInput >>= handleOutput
 return state
handleState Face state = do
 plane <- handleInput
 handleOutputs (handleFace plane state)
 handleInput >>= handleOutput
 return state
handleState Frame state = do
 plane <- handleInput
 handleOutputs (handleFrame plane state)
 handleInput >>= handleOutput
 return state
handleState Get state = do
 plane <- handleInput
 handleOutput (handleGet plane state)
 handleInput >>= handleOutput
 return state
handleState Set state = do
 plane <- handleInput
 mask <- handleInput
 handleInput >>= handleOutput
 return (handleSet plane mask state)
handleState Filter (State a b c) = do
 arg <- handleInput
 result <- mapM (handleFilter arg) c
 handleInput >>= handleOutput
 return (State a b result)
handleState Divide state = do
 plane <- handleInput
 mask <- handleInput
 wrt <- handleInputs
 handleInput >>= handleOutput
 return (handleDivide plane mask wrt state)
handleState Vertex state = do
 plane <- handleInput
 handleOutputs (handleVertex plane state)
 handleInput >>= handleOutput
 return state
handleState Index state = do
 plane <- handleInput
 handleOutputs (handleIndex plane state)
 handleInput >>= handleOutput
 return state
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

