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
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.C.String
import AffTopo.Naive

type Generic = [(Place,[Region])]
type Sideband = ([[Int]],[Int],Int,Int,Int,Int,Int)

foreign import ccall "generic" genericC :: CInt -> IO (Ptr CInt)
foreign import ccall "sideband" sidebandC :: CInt -> IO (Ptr CInt)
foreign import ccall "correlate" correlateC :: CInt -> IO (Ptr CInt)
foreign import ccall "readFaceSub" readFaceSubC :: IO (Ptr CInt)
foreign import ccall "readFaceOk" readFaceOkC :: IO (Ptr CInt)
foreign import ccall "readSideBuf" readSideBufC :: IO (Ptr CInt)
foreign import ccall "writeFaceSub" writeFaceSubC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "writePointSub" writePointSubC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "writeSideSub" writeSideSubC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "writePlaneOk" writePlaneOkC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "writeFaceOk" writeFaceOkC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "print" printC :: CInt -> IO (Ptr CChar)
foreign import ccall "event" eventC :: IO (Ptr CChar)
foreign import ccall "intArgument" intArgumentC :: IO CInt

printStr :: [Char] -> IO ()
printStr str = do
 ptr <- printC (fromIntegral (length str))
 pokeArray ptr (map castCharToCChar str)

plusPtr' :: Ptr CInt -> Int -> Ptr CInt
plusPtr' ptr offset = let
 dummy :: Int
 dummy = 0
 dummyC :: CInt
 dummyC = fromIntegral dummy
 in plusPtr ptr (offset*(sizeOf dummyC))

length2 :: [[a]] -> Int
length2 a = fold' (\x y -> (length x) + y) a 0

split :: [a] -> [Int] -> [[a]]
split [] _ = []
split _ [] = []
split a (b:c) = (take b a):(split (drop b a) c)

recurse :: (a -> a) -> a -> Int -> [a]
recurse f a b = take b (iterate f a)

recurseF :: (a -> a) -> a -> Int -> [a]
recurseF f a b = drop 1 (recurse f a (b+1))

iterate2 :: (a -> a -> a) -> a -> a -> [a]
iterate2 f a b = a:(iterate2 f b (f a b))

recurse2 :: (a -> a -> a) -> a -> a -> Int -> [a]
recurse2 f a b c = take c (iterate2 f a b)

recurse2F :: (a -> a -> a) -> a -> a -> Int -> [a]
recurse2F f a b c = drop 1 (recurse2 f a b (c+1))

recurse2G :: (a -> a -> a) -> a -> a -> Int -> [a]
recurse2G f a b c = drop 1 (recurse2F f a b (c+1))

handleEvent :: IO Bool
handleEvent = do
 event <- (eventC >>= peekCString)
 case event of
  "Plane" ->
   handleEventF >>= handlePlane >>
   return False
  "Inflate" ->
   handleEventF >>= handleInflate >>
   return False
  "Fill" ->
   handleEventF >>= handleFill >>
   return False
  "Hollow" ->
   handleEventF >>= handleHollow >>
   return False
  "Error" -> return True
  "Done" -> return True
  _ ->
   printStr (concat ["unknown event ",(show event),"\n"]) >>
   return True

handleEventF :: IO Int
handleEventF = intArgumentC >>= (return . fromIntegral)

handlePlane :: Int -> IO ()
handlePlane index =
 readSideband >>= (\(boundary,todo,done,base,limit,points,relates) -> let
 inboundary = boundary !! index
 inboundaries = length inboundary
 point = map (done :) (subsets 2 inboundary)
 classify = map (inboundary \\) point
 relate = recurseF (inboundaries +) base (length point)
 newBoundary = replace index (inboundary `append` [done]) boundary
 newLimit = find' ((last relate) ==) [limit + (length2 classify)]
 newPoints = points + (length2 point)
 newRelates = relates + (length relate)
 newDone = done + 1
 newTodo = todo `append` [index]
 in writePointSubC (fromIntegral points) (fromIntegral newPoints) >>= writeBuffer (map fromIntegral (concat point)) >>
 writeSideSubC (fromIntegral limit) (fromIntegral newLimit) >>= writeBuffer (map fromIntegral (concat classify)) >>
 correlateC (fromIntegral newRelates) >>= (\x -> writeBuffer (map fromIntegral relate) (plusPtr' x relates)) >>
 writeSideband (newBoundary,newTodo,newDone,base,newLimit,newPoints,newRelates) >>
 return ()
 )

handleInflate :: Int -> IO ()
handleInflate index =
 readGeneric >>= (\generic ->
 readSideband >>= (\(boundary,todo,done,base,limit,points,relates) ->
 readSideBufC >>= readBuffer base limit >>= (\side ->
 readFaceOkC >>= readBuffer 0 0 >>= (\valid ->
 readFaceSubC >>= readBuffer 0 0 >>= (\face -> let
 -- add boundaries accoriding to sidedness
 generic1 = handleInflateF boundary todo done side generic
 -- maintain boundary lists in sideband
 boundary1 = map (boundariesOfPlace . fst) generic1
 boundary2 = map2 (\(Boundary x) -> x) boundary1
 -- replace embed for indicated place by all inside regions
 (inplace1,_) = generic1 !! index
 (inboundary1,inspace1) = unzipPlace inplace1
 inboundary2 = map (\(Boundary x) -> x) inboundary1
 inregions1 = regionsOfSpace inspace1
 inembed1 = filter (\x -> not (oppositeOfRegionExists inboundary1 x inspace1)) inregions1
 generic2 = replace index (inplace1,inembed1) generic1
 -- find boundaries between inside and outside regions
 attached1 = concat (map (\r -> map (\x -> (x, r, oppositeOfRegion [x] r inspace1)) (attachedBoundaries r inspace1)) inembed1)
 attached2 = filter (\(_,_,y) -> not (elem y inembed1)) attached1
 -- choose vertex per found boundary
 attached3 = map (\(x,r,_) -> (x, r, choose (filter (\w -> elem x w) (attachedFacets 3 r inspace1)))) attached2
 -- find all edges per boundary
 attached4 = map (\(x,r,y) -> (x, r, y, filter (\w -> elem x w) (attachedFacets 2 r inspace1))) attached3
 -- find vertex pair per found edge
 attached5 = map (\(x,r,y,z) -> (x, y, map (\w -> (w, filter (\v -> all (\u -> elem u v) w) (attachedFacets 3 r inspace1))) z)) attached4
 -- construct face from base vertex and edge
 face1 = map (\(x,y,z) -> (x, filter (/=x) y, map (\(w,v) -> (filter (/=x) w, map (\u -> filter (/=x) u) v)) z)) attached5
 face2 = map (\(x,y,z) -> (x, y, map (\(w,v) -> (w, map (\u -> u \\ w) v)) z)) face1
 face3 = concat (concat (map (\(x,y,z) -> map (\(w,v) -> concat [[x],w,(concat v),y]) z) face2))
 face4 = map (\(Boundary x) -> x) (zipBoundaries face3 inboundary1)
 -- remove faces of indexed place
 valid1 = map (\(x,y) -> if (x /= 0) && (not (elem (head y) inboundary2)) then 1 else 0) (zip valid (split face (repeat 6)))
 -- indicate new faces are valid
 valid2 :: [Int]
 valid2 = valid1 `append` (replicate (quot (length face4) 6) 1)
 -- append found faces
 in writeGeneric generic2 >>
 writeSideband (boundary2,[],done,limit,limit,points,relates) >>
 writeFaceOkC 0 (fromIntegral (length valid2)) >>= writeBuffer (map fromIntegral valid2) >>
 writeFaceSubC (fromIntegral (length face)) (fromIntegral (length face4)) >>= writeBuffer (map fromIntegral face4) >>
 return ()
 )))))

handleInflateF :: [[Int]] -> [Int] -> Int -> [Int] -> Generic -> Generic
handleInflateF boundary todo done sidedness generic = let
 (_,_,result) = fold' handleInflateG (zip (iterate (1+) (done - (length todo))) todo) (boundary, sidedness, generic)
 in result

handleInflateG :: (Int, Int) -> ([[Int]], [Int], Generic) -> ([[Int]], [Int], Generic)
handleInflateG (done, todo) (boundary, sidedness, generic) = let
 inboundary = map Boundary (boundary !! todo)
 boundaried = Boundary done
 inboundaried = map (\(Boundary x) -> x) (inboundary `append` [boundaried])
 pair = subsets 2 inboundary
 wrt = map (inboundary \\) pair
 polyant = map2 (\(x,y) -> (x, Side y)) (handleInflateH sidedness wrt)
 ingeneric = handleInflateI boundaried polyant (generic !! todo)
 in (replace todo inboundaried boundary, drop (length2 polyant) sidedness, replace todo ingeneric generic)

handleInflateH :: [Int] -> [[Boundary]] -> [[(Boundary,Int)]]
handleInflateH a b = map (\(x,y) -> zip x y) (zip b (split a (map length b)))

handleInflateI :: Boundary -> [[(Boundary,Side)]] -> (Place,[Region]) -> (Place,[Region])
handleInflateI a b (c,d) = let
 region = fold' (++) (map (handleInflateJ c) b) []
 place = choose (divideSpace a (embedSpace region c) c)
 in (place, takeRegions (embedSpace d c) place)

handleInflateJ :: Place -> [(Boundary,Side)] -> [Region]
handleInflateJ a b = fold' (+\) (map (\(x, Side y) -> (head (image [x] a)) !! y) b) (regionsOfPlace a)

handleFill :: Int -> IO ()
handleFill = undefined

handleHollow :: Int -> IO ()
handleHollow = undefined

readBuffer :: Int -> Int -> Ptr CInt -> IO [Int]
readBuffer base limit ptr = peekArray (limit-base) (plusPtr' ptr base) >>= return . (map fromIntegral)

writeBuffer :: [CInt] -> Ptr CInt -> IO ()
writeBuffer list ptr = pokeArray ptr list

chip :: (a, Ptr CInt) -> IO (Int, Ptr CInt)
chip (_,ptr) = (peek ptr) >>= (\x -> return (fromIntegral x, plusPtr' ptr 1))

peel :: (Int, Ptr CInt) -> (a, Ptr CInt) -> IO ([Int], Ptr CInt)
peel (len,_) (_,ptr) = (peekArray len ptr) >>= (\x -> return (map fromIntegral x, plusPtr' ptr len))

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

readSideband :: IO Sideband
readSideband = (sidebandC 0) >>= (\ptr -> jump (0::Int,ptr)
 chip >>= (\places -> jump places
 (peel places) >>= (\boundaries -> jump boundaries
 (onion boundaries) >>= (\boundary -> jump boundary
 chip >>= (\todos -> jump todos
 (peel todos) >>= (\todo -> jump todo
 chip >>= (\done -> jump done
 chip >>= (\base -> jump base
 chip >>= (\limit -> jump limit
 chip >>= (\points -> jump points
 chip >>= (\relates ->
 return ((fst boundary), (fst todo), (fst done), (fst base), (fst limit), (fst points), (fst relates))
 )))))))))))

paste :: Int -> Ptr CInt -> IO (Ptr CInt)
paste len ptr = poke ptr (fromIntegral len) >>= (\x -> seq x (return (plusPtr' ptr 1)))

cover :: [Int] -> Ptr CInt -> IO (Ptr CInt)
cover len ptr = pokeArray ptr (map fromIntegral len) >>= (\x -> seq x (return (plusPtr' ptr (length len))))

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

writeSideband :: Sideband -> IO (Ptr CInt)
writeSideband (a,b,c,d,e,f,g) = let
 places = length a
 boundaries = map length a
 todos = length b
 size = 7 + (length boundaries) + (length2 a) + (length b)
 in (sidebandC (fromIntegral size)) >>=
 (paste places) >>=
 (cover boundaries) >>=
 (layer a) >>=
 (paste todos) >>=
 (cover b) >>=
 (paste c) >>=
 (paste d) >>=
 (paste e) >>=
 (paste f) >>=
 (paste g)
