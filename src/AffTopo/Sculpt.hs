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
type Sideband = ([[Int]],[Int],[Int],[Int],Int,[Int],Int,Int)
 -- ([[boundary]],[num-points],[num-classifications],[num-correlates],num-done,[place-todo],side-done,side-todo)

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

plusPtr' :: Ptr CInt -> Int -> Ptr CInt
plusPtr' ptr offset = let
 dummy :: Int
 dummy = 0
 dummyC :: CInt
 dummyC = fromIntegral dummy
 in plusPtr ptr (offset*(sizeOf dummyC))

readBuffer :: Int -> Int -> Ptr CInt -> IO [Int]
readBuffer base limit ptr = peekArray (limit-base) (plusPtr' ptr base) >>= return . (map fromIntegral)

writeBuffer :: [CInt] -> Ptr CInt -> IO ()
writeBuffer list ptr = pokeArray ptr list

length2 :: [[a]] -> Int
length2 a = fold' (\x y -> (length x) + y) a 0

handlePlane :: Int -> IO ()
handlePlane index =
 readSideband >>= (\(boundary,points,classes,relates,done,todo,base,_) -> let
 inboundary = boundary !! index
 inpoints = points !! index
 inclasses = classes !! index
 inrelates = relates !! index
 inboundaries = length inboundary
 point = map (done:) (subsets 2 inboundary)
 classify = map (inboundary \\) point
 relate = map (\_ -> inboundaries) point
 pointed = map fromIntegral (concat point)
 classified = map fromIntegral (concat classify)
 related = map fromIntegral relate
 newInboundary = inboundary `append` [done]
 newInpoints = inpoints + (length pointed)
 newInclasses = inclasses + (length classified)
 newInrelates = inrelates + (length related)
 newBoundary = replace index newInboundary boundary
 newPoints = replace index newInpoints points
 newClasses = replace index newInclasses classes
 newRelates = replace index newInrelates relates
 newDone = done + 1
 newTodo = todo `append` [index]
 inpointed = fromIntegral inpoints
 inclassified = fromIntegral inclasses
 inrelated = fromIntegral inrelates
 newInpointed = fromIntegral newInpoints
 newInclassified = fromIntegral newInclasses
 newInrelated = fromIntegral newInrelates
 in writePointSubC inpointed newInpointed >>= writeBuffer pointed >>
 writeSideSubC inclassified newInclassified >>= writeBuffer classified >>
 correlateC newInrelated >>= (\x -> writeBuffer related (plusPtr' x inrelated)) >>
 writeSideband (newBoundary,newPoints,newClasses,newRelates,newDone,newTodo,base,newInclasses) >>
 return ()
 )

handleInflate :: Int -> IO ()
handleInflate index =
 readGeneric >>= (\generic ->
 readSideband >>= (\(boundary,points,classes,relates,done,todo,base,limit) ->
 readSideBufC >>= readBuffer base limit >>= (\sidedness -> let
 generic1 = handleInflateF boundary done todo sidedness generic
 (inplace1,_) = generic1 !! index
 (inboundary1,inspace1) = unzipPlace inplace1
 inregions1 = regionsOfSpace inspace1
 inembed1 = filter (\x -> not (oppositeOfRegionExists inboundary1 x inspace1)) inregions1
 boundary1 = map (boundariesOfPlace . fst) generic1
 boundary2 = map2 (\(Boundary x) -> x) boundary1
 generic2 = replace index (inplace1,inembed1) generic1
 -- remove faces of indexed place
 -- find faces between inside and outside regions
 -- append found faces
 in writeGeneric generic2 >>
 writeSideband (boundary2,points,classes,relates,done,[],limit,limit) >>
 return ()
 )))

handleInflateF :: [[Int]] -> Int -> [Int] -> [Int] -> Generic -> Generic
handleInflateF boundary done todo sidedness generic = let
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
 ingeneric = handleInflateJ boundaried polyant (generic !! todo)
 in (replace todo inboundaried boundary, drop (length2 polyant) sidedness, replace todo ingeneric generic)

handleInflateH :: [Int] -> [[Boundary]] -> [[(Boundary,Int)]]
handleInflateH a b = map (\(x,y) -> zip x y) (zip b (handleInflateI a (map length b)))

handleInflateI :: [a] -> [Int] -> [[a]]
handleInflateI [] _ = []
handleInflateI _ [] = []
handleInflateI a (b:c) = (take b a):(handleInflateI (drop b a) c)

handleInflateJ :: Boundary -> [[(Boundary,Side)]] -> (Place,[Region]) -> (Place,[Region])
handleInflateJ a b (c,d) = let
 region = fold' (++) (map (handleInflateK c) b) []
 place = choose (divideSpace a (embedSpace region c) c)
 in (place, takeRegions (embedSpace d c) place)

handleInflateK :: Place -> [(Boundary,Side)] -> [Region]
handleInflateK a b = fold' (+\) (map (\(x, Side y) -> (head (image [x] a)) !! y) b) (regionsOfPlace a)

handleFill :: Int -> IO ()
handleFill = undefined

handleHollow :: Int -> IO ()
handleHollow = undefined

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

--num-places,[num-boundaries],[[boundary]],[num-points],[num-classifications],[num-correlates],num-done,num-todo,[place-todo]--
readSideband :: IO Sideband
readSideband = (sidebandC 0) >>= (\ptr -> jump (0::Int,ptr)
 chip >>= (\places -> jump places
 (peel places) >>= (\boundaries -> jump boundaries
 (onion boundaries) >>= (\boundary -> jump boundary
 (peel places) >>= (\points -> jump points
 (peel places) >>= (\classifications -> jump classifications
 (peel places) >>= (\correlates -> jump correlates
 chip >>= (\dones -> jump dones
 chip >>= (\todos -> jump todos
 (peel todos) >>= (\todo -> jump todo
 chip >>= (\base -> jump base
 chip >>= (\limit ->
 return ((fst boundary), (fst points), (fst classifications), (fst correlates), (fst dones), (fst todo), (fst base), (fst limit))
 ))))))))))))

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

-- type Sideband = ([[Int]],[Int],[Int],[Int],Int,[Int],Int,Int)
--num-places,[num-boundaries],[[boundary]],[num-points],[num-classifications],[num-correlates],num-done,num-todo,[place-todo],side-done,side-todo--
writeSideband :: Sideband -> IO (Ptr CInt)
writeSideband (a,b,c,d,e,f,g,h) = let
 places = length a
 boundaries = map length a
 todos = length f
 size = 1 + places + places + places + places + 1 + 1 + todos + 1 + 1
 in (sidebandC (fromIntegral size)) >>=
 (paste places) >>=
 (cover boundaries) >>=
 (layer a) >>=
 (cover b) >>=
 (cover c) >>=
 (cover d) >>=
 (paste e) >>=
 (paste todos) >>=
 (cover f) >>=
 (paste g) >>=
 (paste h)
