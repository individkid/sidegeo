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

type Generic = (Place,[Region])
type Sideband = ([Int],Int,Int,Int,Int,Int)

foreign import ccall "generic" genericC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "sideband" sidebandC :: CInt -> IO (Ptr CInt)
foreign import ccall "correlate" correlateC :: CInt -> IO (Ptr CInt)
foreign import ccall "boundary" boundaryC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaries" boundariesC :: CInt -> IO CInt
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

readGeneric :: Int -> IO Generic
readGeneric index =
 (boundariesC (fromIntegral index)) >>= \boundaries ->
 (readBoundary (fromIntegral index)) >>= \boundary ->
 (genericC (fromIntegral index) 0) >>= \ptr -> jump (0::Int,ptr)
 (peel ((fromIntegral boundaries),ptr)) >>= \firsts -> jump firsts
 (peel ((fromIntegral boundaries),ptr)) >>= \seconds -> jump seconds
 (onion firsts) >>= \first -> jump first
 (onion seconds) >>= \second -> jump second
 chip >>= \embeds -> jump embeds
 (peel embeds) >>= \embed -> let
 x = map2 Region (fst first)
 y = map2 Region (fst second)
 z = map Region (fst embed)
 in return ((zipWith3 readGenericF boundary x y), z)

readGenericF :: Boundary -> [Region] -> [Region] -> (Boundary,[[Region]])
readGenericF a b c = (a,[b,c])

readSideband :: IO Sideband
readSideband =
 (sidebandC 0) >>= \ptr -> jump (0::Int,ptr)
 chip >>= \todos -> jump todos
 (peel todos) >>= \todo -> jump todo
 chip >>= \done -> jump done
 chip >>= \base -> jump base
 chip >>= \limit -> jump limit
 chip >>= \points -> jump points
 chip >>= \relates ->
 return ((fst todo), (fst done), (fst base), (fst limit), (fst points), (fst relates))

readBoundary :: Int -> IO [Boundary]
readBoundary index =
 boundariesC (fromIntegral index) >>= \boundaries ->
 boundaryC (fromIntegral index) 0 >>=
 peekArray (fromIntegral boundaries) >>= \boundary ->
 return (map (Boundary . fromIntegral) boundary)

paste :: Int -> Ptr CInt -> IO (Ptr CInt)
paste len ptr = poke ptr (fromIntegral len) >>= (\x -> seq x (return (plusPtr' ptr 1)))

cover :: [Int] -> Ptr CInt -> IO (Ptr CInt)
cover len ptr = pokeArray ptr (map fromIntegral len) >>= (\x -> seq x (return (plusPtr' ptr (length len))))

layer :: [[Int]] -> Ptr CInt -> IO (Ptr CInt)
layer len ptr = fold' (\x y -> y >>= (\z -> cover x z)) len (return ptr)

hoard :: [[[Int]]] -> Ptr CInt -> IO (Ptr CInt)
hoard len ptr = fold' (\x y -> y >>= (\z -> layer x z)) len (return ptr)

writeGeneric :: Int -> Generic -> IO ()
writeGeneric index a = let
 boundary = boundariesOfPlace (fst a)
 firsts = map (length . head) (range (fst a))
 seconds = map (length . last) (range (fst a))
 first = map head (range (fst a))
 second = map last (range (fst a))
 embeds = length (snd a)
 embed = snd a
 size = (length firsts) + (length seconds) + (length2 first) + (length2 second) + 1 + (length embed)
 in writeBoundary index boundary >>
 (genericC (fromIntegral index) (fromIntegral size)) >>=
 (cover firsts) >>=
 (cover seconds) >>=
 (layer (map2 (\(Region x) -> x) first)) >>=
 (layer (map2 (\(Region x) -> x) second)) >>=
 (paste embeds) >>=
 (cover (map (\(Region x) -> x) embed)) >>
 return ()

writeSideband :: Sideband -> IO ()
writeSideband (b,c,d,e,f,g) = let
 todos = length b
 size = 6 + todos
 in (sidebandC (fromIntegral size)) >>=
 (paste todos) >>=
 (cover b) >>=
 (paste c) >>=
 (paste d) >>=
 (paste e) >>=
 (paste f) >>=
 (paste g) >>
 return ()

writeBoundary :: Int -> [Boundary] -> IO ()
writeBoundary index boundary =
 (boundaryC (fromIntegral index) (fromIntegral (length boundary))) >>=
 (cover (map (\(Boundary x) -> x) boundary)) >>
 return ()

handleEvent :: IO Bool
handleEvent = do
 event <- (eventC >>= peekCString)
 case event of
  "Initialize" -> handleInitialize >> return False
  "Plane" -> handleEventF >>= handlePlane >> return False
  "Classify" -> handleClassify >> return False
  "Inflate" -> handleEventF >>= handleInflate >> return False
  "Fill" -> handleEventF >>= handleFill >> return False
  "Hollow" -> handleEventF >>= handleHollow >> return False
  "Remove" -> handleEventF >>= \index -> handleEventF >>= handleRemove index >> return False
  "Error" -> return True
  "Done" -> return True
  _ ->
   printStr (concat ["unknown event ",(show event),"\n"]) >>
   return True

handleEventF :: IO Int
handleEventF = intArgumentC >>= (return . fromIntegral)

handleInitialize :: IO ()
handleInitialize =
 writeSideband ([],0,0,0,0,0) >>
 return ()

handlePlane :: Int -> IO ()
handlePlane index =
 readSideband >>= \(todo,done,base,limit,points,relates) ->
 readBoundary index >>= \boundary -> let
 boundaries = length boundary
 point = map ((Boundary done) :) (subsets 2 boundary)
 classify = map (boundary \\) point
 relate = recurseF (boundaries +) base (length point)
 newBoundary = boundary `append` [Boundary done]
 newLimit = find' ((last relate) ==) [limit + (length2 classify)]
 newPoints = points + (length2 point)
 newRelates = relates + (length relate)
 newTodo = todo `append` [index]
 in writePointSubC (fromIntegral points) (fromIntegral newPoints) >>=
 writeBuffer (map (\(Boundary x) -> fromIntegral x) (concat point)) >>
 writeSideSubC (fromIntegral limit) (fromIntegral newLimit) >>=
 writeBuffer (map (\(Boundary x) -> fromIntegral x) (concat classify)) >>
 correlateC (fromIntegral newRelates) >>= (\x -> writeBuffer (map fromIntegral relate) (plusPtr' x relates)) >>
 writeSideband (newTodo,done,base,newLimit,newPoints,newRelates) >>
 writeBoundary index newBoundary >>
 return ()

handleClassify :: IO ()
handleClassify = readSideband >>= handleClassifyF >>= writeSideband

handleClassifyF :: Sideband -> IO Sideband
handleClassifyF (index:todo,done,base,limit,points,relates) =
 readGeneric index >>= \generic ->
 readSideBufC >>= readBuffer base limit >>= \sidedness -> let
 boundary = boundariesOfPlace (fst generic)
 -- add boundaries accoriding to sidedness
 pair = subsets 2 boundary
 wrt = map (boundary \\) pair
 polyant = map2 (\(x,y) -> (x, Side y)) (handleClassifyG sidedness wrt)
 generic1 = handleClassifyH (Boundary done) polyant generic
 in writeGeneric index generic1 >>
 return (todo,done+1,base+(length2 polyant),limit,points,relates)
handleClassifyF sideband = return sideband

handleClassifyG :: [Int] -> [[Boundary]] -> [[(Boundary,Int)]]
handleClassifyG a b = map (\(x,y) -> zip x y) (zip b (split a (map length b)))

handleClassifyH :: Boundary -> [[(Boundary,Side)]] -> (Place,[Region]) -> (Place,[Region])
handleClassifyH a b (c,d) = let
 region = fold' (++) (map (handleClassifyI c) b) []
 place = choose (divideSpace a (embedSpace region c) c)
 in (place, takeRegions (embedSpace d c) place)

handleClassifyI :: Place -> [(Boundary,Side)] -> [Region]
handleClassifyI a b = fold' (+\) (map (\(x, Side y) -> (head (image [x] a)) !! y) b) (regionsOfPlace a)

handleInflate :: Int -> IO ()
handleInflate index =
 readGeneric index >>= \generic ->
 readFaceOkC >>= readBuffer 0 0 >>= \valid ->
 readFaceSubC >>= readBuffer 0 0 >>= \face -> let
 -- replace embed for indicated place by all inside regions
 (place,_) = generic
 (boundary,space) = unzipPlace place
 boundaried = map (\(Boundary x) -> x) boundary
 regions = regionsOfSpace space
 embed = filter (\x -> not (oppositeOfRegionExists boundary x space)) regions
 generic1 = (place,embed)
 -- find boundaries between inside and outside regions
 attached1 = concat (map (\r -> map (\x -> (x, r, oppositeOfRegion [x] r space)) (attachedBoundaries r space)) embed)
 attached2 = filter (\(_,_,y) -> not (elem y embed)) attached1
 -- choose vertex per found boundary
 attached3 = map (\(x,r,_) -> (x, r, choose (filter (\w -> elem x w) (attachedFacets 3 r space)))) attached2
 -- find all edges per boundary
 attached4 = map (\(x,r,y) -> (x, r, y, filter (\w -> elem x w) (attachedFacets 2 r space))) attached3
 -- find vertex pair per found edge
 attached5 = map (\(x,r,y,z) -> (x, y, map (\w -> (w, filter (\v -> all (\u -> elem u v) w) (attachedFacets 3 r space))) z)) attached4
 -- construct face from base vertex and edge
 face1 = map (\(x,y,z) -> (x, filter (/=x) y, map (\(w,v) -> (filter (/=x) w, map (\u -> filter (/=x) u) v)) z)) attached5
 face2 = map (\(x,y,z) -> (x, y, map (\(w,v) -> (w, map (\u -> u \\ w) v)) z)) face1
 face3 = concat (concat (map (\(x,y,z) -> map (\(w,v) -> concat [[x],w,(concat v),y]) z) face2))
 face4 = map (\(Boundary x) -> x) (zipBoundaries face3 boundary)
 -- remove faces of indexed place
 valid1 = map (\(x,y) -> if (x /= 0) && (not (elem (head y) boundaried)) then 1 else 0) (zip valid (split face (repeat 6)))
 -- indicate new faces are valid
 valid2 :: [Int]
 valid2 = valid1 `append` (replicate (quot (length face4) 6) 1)
 -- append found faces
 in writeGeneric index generic1 >>
 writeFaceOkC 0 (fromIntegral (length valid2)) >>= writeBuffer (map fromIntegral valid2) >>
 writeFaceSubC (fromIntegral (length face)) (fromIntegral (length face4)) >>= writeBuffer (map fromIntegral face4) >>
 return ()

handleFill :: Int -> IO ()
handleFill = undefined

handleHollow :: Int -> IO ()
handleHollow = undefined

handleRemove :: Int -> Int -> IO ()
handleRemove = undefined
