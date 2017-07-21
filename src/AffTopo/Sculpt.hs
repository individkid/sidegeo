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

import Data.List (mapAccumL,mapAccumR,transpose)
import Data.Maybe (catMaybes)
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

mapAccumL2 :: (a -> b -> (a,c)) -> a -> [[b]] -> (a,[[c]])
mapAccumL2 f a b = mapAccumL (\acc lst -> mapAccumL f acc lst) a b

oneHot :: Int -> Int -> a -> [Maybe a]
oneHot pos lim val = map (\x -> if x == pos then Just val else Nothing) (indices lim)

zip2 :: [[a]] -> [b] -> [[(a,b)]]
zip2 a b = let
 zipped = snd (mapAccumL2 (\w x -> case w of (y:z) -> (z, Just (x,y)); [] -> ([],Nothing)) b a)
 in filter (not . null) (map catMaybes zipped)

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

-- [[Maybe Boundary]] -- each row has a single Just where Boundary is position in todo, and column is Place index from todo
-- [[Maybe Boundary]] -- transpose; each row has zero or more Just where each row is a Place to add to
-- [(Place, [Maybe Boundary])] -- zip with place from generic
-- [([Boundary], [Maybe Boundary])] -- take boundariesOfPlace
-- [[([Boundary], Maybe Boundary)]] -- fold by appending Justs
-- [[Maybe (Boundary,[[Boundary]])]] -- sparse map boundary-to-add to per pair, boundaries-so-far minus pair
-- [[Maybe (Boundary,[[Boundary]])]] -- transpose; rows are now in positions in todo
-- ([Sidedness],[[Maybe (Boundary,[[(Boundary,Sidedness]])]]) -- use mapAccumL2 to split sideBuf on each Just in double nested list 
-- [[Maybe (Boundary,[[(Boundary,Sidedness)]])]] -- fst should be empty
-- [[Maybe (Boundary,[[(Boundary,Sidedness)]])]] -- transpose; rows are now places
-- [(Place,[Maybe (Boundary,[[(Boundary,Sidedness)]])])] -- zip with places from generic
-- [(Place,[(Boundary,[[(Boundary,Sidedness)]])])] -- each place now has boundary to add together with polyant of prior boundaries
-- [Place] -- have to fold each place in one shot because finding regions to divide requires place with prior boundaries added
handleInflate :: Int -> IO ()
handleInflate index =
 readGeneric >>= (\generic ->
 readSideband >>= (\(boundary,points,classes,relates,done,todo,base,limit) ->
 readSideBufC >>= readBuffer base limit >>= (\sidedness -> let
 todos = length todo
 matrix1 :: [[Maybe Boundary]]
 matrix1 = map (\(x,y) -> oneHot x todos (Boundary y)) (zip todo (iterate (1+) done))
 matrix2 :: [[Maybe Boundary]]
 matrix2 = transpose matrix1
 matrix3 :: [(Place, [Maybe Boundary])]
 matrix3 = zip (domain generic) matrix2
 matrix4 :: [([Boundary], [Maybe Boundary])]
 matrix4 = map (\(x,y) -> ((boundariesOfPlace x), y)) matrix3
 matrix5 :: [[([Boundary], Maybe Boundary)]]
 matrix5 = map (\(x,y) -> snd (mapAccumR (\lst val -> case val of Just z -> ((z:lst),(lst,val)); Nothing -> (lst,(lst,val))) x y)) matrix4
 matrix6 :: [[Maybe (Boundary, [[Boundary]])]]
 matrix6 = map2 (\(x,y) -> case y of Just z -> Just (z, map (\w -> x \\ w) (subsets 2 x)); Nothing -> Nothing) matrix5
 matrix7 :: [[Maybe (Boundary, [[Boundary]])]]
 matrix7 = transpose matrix6
 matrix8 :: ([Side],[[Maybe (Boundary, [[(Boundary,Side)]])]])
 matrix8 = mapAccumL2 (\w x -> case x of Just (y,z) -> ((drop (length2 z) w), Just (y, zip2 z w)); Nothing -> (w,Nothing)) (map Side sidedness) matrix7
 matrix9 :: [[Maybe (Boundary, [[(Boundary,Side)]])]]
 ([],matrix9) = matrix8
 matrix10 :: [[Maybe (Boundary,[[(Boundary,Side)]])]]
 matrix10 = transpose matrix9
 matrix11 :: [(Place,[Maybe (Boundary,[[(Boundary,Side)]])])]
 matrix11 = zip (domain generic) matrix10
 matrix12 :: [(Place,[(Boundary,[[(Boundary,Side)]])])]
 matrix12 = map (\(x,y) -> (x, catMaybes y)) matrix11
 generic0 :: [Place]
 generic0 = map (\(x,y) -> fold' handleInflateF y x) matrix12
 generic1 = zip generic0 (range generic)
 (inplace1,_) = generic1 !! index
 (inboundary1,inspace1) = unzipPlace inplace1
 inregions1 = regionsOfSpace inspace1
 inembed1 = filter (\x -> not (oppositeOfRegionExists inboundary1 x inspace1)) inregions1
 boundary1 = replace index (map (\(Boundary x) -> x) inboundary1) boundary
 generic2 = replace index (inplace1,inembed1) generic1
 -- remove faces of indexed place
 -- find faces between inside and outside regions
 -- append found faces
 in writeGeneric generic2 >>
 writeSideband (boundary1,points,classes,relates,done,[],limit,limit) >>
 return ()
 )))

handleInflateF :: (Boundary,[[(Boundary,Side)]]) -> Place -> Place
handleInflateF = undefined

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
