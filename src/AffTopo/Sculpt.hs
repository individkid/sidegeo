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
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.C.String
import AffTopo.Naive

type Generic = [(Place,[Region])]
type Sideband = ([[Int]],[Int],[Int],Int,[Int])
 -- ([[boundary]],[num-points],[num-classifications],num-done,[place-todo])

foreign import ccall "generic" genericC :: CInt -> IO (Ptr CInt)
foreign import ccall "sideband" sidebandC :: CInt -> IO (Ptr CInt)
foreign import ccall "side" sideC :: IO (Ptr CInt)
foreign import ccall "face" faceC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "point" pointC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaryWrt" boundaryWrtC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaryOk" boundaryOkC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "faceValid" faceValidC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "print" printC :: CInt -> IO (Ptr CChar)
foreign import ccall "event" eventC :: IO (Ptr CChar)
foreign import ccall "intArgument" intArgumentC :: IO CInt

printStr :: [Char] -> IO ()
printStr str = do
 ptr <- printC (fromIntegral (length str))
 pokeArray ptr (map castCharToCChar str)

inplace :: (a -> a) -> Int -> [a] -> [a]
inplace f i a = map (inplaceF f i) (zip (iterate (1+) 0) a)

inplaceF :: (a -> a) -> Int -> (Int, a) -> a
inplaceF f i (j, a)
 | i == j = f a
 | otherwise = a

handleEvent :: IO Bool
handleEvent = do
 event <- (eventC >>= peekCString)
 case event of
  "Plane" ->
   intArgumentC >>=
   (return . fromIntegral) >>= (\index ->
   readSideband >>=
   (handleEventF index)) >>=
   writeSideband >>
   printStr (concat ["plane\n"]) >>
   return False
  "Inflate" -> do
   printStr "inflate\n"
   return False
  "Fill" -> do
   face <- intArgumentC
   printStr (concat ["fill ",(show face),"\n"])
   return False
  "Hollow" -> do
   face <- intArgumentC
   printStr (concat ["hollow ",(show face),"\n"])
   return False
  "Error" -> return True
  "Done" -> return True
  _ -> do
   printStr (concat ["unknown event ",(show event),"\n"])
   return True

handleEventF :: Int -> Sideband -> IO Sideband
handleEventF index (boundary,points,classifications,done,todo) = let
 newBoundary = inplace (\x -> concat [x,[done]]) index boundary
 boundaries = length (boundary !! index)
 newPoints = inplace (\x -> x + (boundaries * (boundaries-1))) index points
 newClassifications = inplace (\x -> x + (boundaries * (boundaries-1) * (boundaries-2))) index classifications
 newDone = done + 1
 newTodo = concat [todo,[index]]
 selected = (boundary !! index)
 ordered = map (done:) (subsets 2 selected)
 removed = map (selected \\) ordered
 point = map fromIntegral (concat ordered)
 classification = map fromIntegral (concat removed)
 pointPtr :: IO (Ptr CInt)
 pointPtr = pointC (fromIntegral (points !! index)) (fromIntegral (newPoints !! index))
 classificationPtr :: IO (Ptr CInt)
 classificationPtr = boundaryWrtC (fromIntegral (classifications !! index)) (fromIntegral (newClassifications !! index))
 in pointPtr >>= (handleEventG point) >>
 classificationPtr >>= (handleEventG classification) >>
 return (newBoundary,newPoints,newClassifications,newDone,newTodo)

handleEventG :: [CInt] -> Ptr CInt -> IO ()
handleEventG list ptr = pokeArray ptr list

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

jump :: a -> (a -> b) -> b
jump a f = f a

--num-places,[num-boundaries],[[boundary]],[num-points],[num-classifications],num-done,num-todo,[place-todo]--
readSideband :: IO Sideband
readSideband = (sidebandC 0) >>= (\ptr -> jump (0::Int,ptr)
 chip >>= (\places -> jump places
 (peel places) >>= (\boundaries -> jump boundaries
 (onion boundaries) >>= (\boundary -> jump boundary
 (peel places) >>= (\points -> jump points
 (peel places) >>= (\classifications -> jump classifications
 chip >>= (\dones -> jump dones
 chip >>= (\todos -> jump todos
 (peel todos) >>= (\todo ->
 return ((fst boundary), (fst points), (fst classifications), (fst dones), (fst todo))
 )))))))))

-- (num-places,[num-boundaries],[num-regions],[[boundary]],[[region]],
--  [[first-halfspace-size]],[[second-halfspace-size]],[[[first-halfspace-region]]],[[[second-halfspace-region]]],
--  [embeded-region-size],[[embeded-region]])
readGeneric :: IO Generic
readGeneric = (genericC 0) >>= (\ptr -> jump (0::Int,ptr)
 chip >>= (\places -> jump places
 (peel places) >>= (\boundaries -> jump boundaries
 (peel places) >>= (\regions -> jump regions
 (onion boundaries) >>= (\boundary -> jump boundary
 (onion regions) >>= (\region -> jump region
 (onion boundaries) >>= (\firsts -> jump firsts
 (onion boundaries) >>= (\seconds -> jump seconds
 (patch firsts) >>= (\first -> jump first
 (patch seconds) >>= (\second -> jump second
 (peel places) >>= (\embeds -> jump embeds
 (onion embeds) >>= (\embed -> let
 w = map2 Boundary (fst boundary)
 x = map3 Region (fst first)
 y = map3 Region (fst second)
 z = map2 Region (fst embed)
 in return (zip (zipWith3 (zipWith3 readGenericF) w x y) z)
 ))))))))))))

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

-- type Sideband = ([[Int]],[Int],[Int],Int,[Int])
--num-places,[num-boundaries],[[boundary]],[num-points],[num-classifications],num-done,num-todo,[place-todo]--
writeSideband :: Sideband -> IO (Ptr CInt)
writeSideband (a,b,c,d,e) = let
 places = length a
 boundaries = map length a
 todos = length e
 size = 1 + places + places + places + 1 + 1 + todos
 in (sidebandC (fromIntegral size)) >>=
 (paste places) >>=
 (cover boundaries) >>=
 (layer a) >>=
 (cover b) >>=
 (cover c) >>=
 (paste d) >>=
 (paste todos) >>=
 (cover e)

