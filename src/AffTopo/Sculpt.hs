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

foreign import ccall "place" placeC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "embed" embedC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "embeds" embedsC :: CInt -> IO CInt
foreign import ccall "sideband" sidebandC :: CInt -> IO (Ptr CInt)
foreign import ccall "sidebands" sidebandsC :: IO CInt
foreign import ccall "correlate" correlateC :: CInt -> IO (Ptr CInt)
foreign import ccall "correlates" correlatesC :: IO CInt
foreign import ccall "boundary" boundaryC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaries" boundariesC :: CInt -> IO CInt
foreign import ccall "readFaceSub" readFaceSubC :: IO (Ptr CInt)
foreign import ccall "readFaceOk" readFaceOkC :: IO (Ptr CInt)
foreign import ccall "readFaces" readFacesC :: IO CInt
foreign import ccall "readSideBuf" readSideBufC :: IO (Ptr CInt)
foreign import ccall "readSides" readSidesC :: IO CInt
foreign import ccall "readPlanes" readPlanesC :: IO CInt
foreign import ccall "writeFaceSub" writeFaceSubC :: CInt -> IO (Ptr CInt)
foreign import ccall "writePointSub" writePointSubC :: CInt -> IO (Ptr CInt)
foreign import ccall "writeSideSub" writeSideSubC :: CInt -> IO (Ptr CInt)
foreign import ccall "writePlaneOk" writePlaneOkC :: CInt -> IO (Ptr CInt)
foreign import ccall "writeFaceOk" writeFaceOkC :: CInt -> IO (Ptr CInt)
foreign import ccall "writePlanes" writePlanesC :: CInt -> IO ()
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

readBuffer :: CInt -> Ptr CInt -> IO [Int]
readBuffer size ptr = peekArray (fromIntegral size) ptr >>= return . (map fromIntegral)

writeBuffer :: [Int] -> Ptr CInt -> IO ()
writeBuffer list ptr = pokeArray ptr (map fromIntegral list)

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

prejump :: Ptr CInt -> IO (Int,Ptr CInt)
prejump a = return (0::Int,a)

nojump :: CInt -> IO (Int, Ptr CInt)
nojump a = return (fromIntegral a, nullPtr)

readPlace :: Int -> IO Place
readPlace index = let indexC = fromIntegral index in
 (boundariesC indexC) >>= nojump >>= \boundaries ->
 (readBoundary index) >>= \boundary ->
 (placeC indexC 0) >>= prejump >>=
 (peel boundaries) >>= \firsts -> jump firsts
 (peel boundaries) >>= \seconds -> jump seconds
 (onion firsts) >>= \first -> jump first
 (onion seconds) >>= \second -> let
 x = map2 Region (fst first)
 y = map2 Region (fst second)
 in return (zipWith3 readPlaceF boundary x y)

readPlaceF :: Boundary -> [Region] -> [Region] -> (Boundary,[[Region]])
readPlaceF a b c = (a,[b,c])

readEmbed :: Int -> IO [Region]
readEmbed index = let indexC = fromIntegral index in
 embedsC indexC >>= \embeds -> embedC indexC 0 >>=
 peekArray (fromIntegral embeds) >>= \embed -> return (map (Region . fromIntegral) embed)

readSideband :: IO [Int]
readSideband = sidebandsC >>= \todosC -> (sidebandC todosC) >>=
 peekArray (fromIntegral todosC) >>= \todoC -> return (map fromIntegral todoC)

readBoundary :: Int -> IO [Boundary]
readBoundary index = let indexC = fromIntegral index in
 boundariesC indexC >>= \boundaries -> boundaryC indexC 0 >>=
 peekArray (fromIntegral boundaries) >>= \boundary -> return (map (Boundary . fromIntegral) boundary)

paste :: Int -> Ptr CInt -> IO (Ptr CInt)
paste len ptr = poke ptr (fromIntegral len) >>= (\x -> seq x (return (plusPtr' ptr 1)))

cover :: [Int] -> Ptr CInt -> IO (Ptr CInt)
cover len ptr = pokeArray ptr (map fromIntegral len) >>= (\x -> seq x (return (plusPtr' ptr (length len))))

layer :: [[Int]] -> Ptr CInt -> IO (Ptr CInt)
layer len ptr = fold' (\x y -> y >>= (\z -> cover x z)) len (return ptr)

hoard :: [[[Int]]] -> Ptr CInt -> IO (Ptr CInt)
hoard len ptr = fold' (\x y -> y >>= (\z -> layer x z)) len (return ptr)

writePlace :: Int -> Place -> IO ()
writePlace index place = let
 indexC = fromIntegral index
 boundary = boundariesOfPlace place
 firsts = map (length . head) (range place)
 seconds = map (length . last) (range place)
 first = map head (range place)
 second = map last (range place)
 sizeC = fromIntegral ((length firsts) + (length seconds) + (length2 first) + (length2 second))
 in writeBoundary index boundary >>
 placeC indexC sizeC >>=
 cover firsts >>=
 cover seconds >>=
 layer (map2 (\(Region x) -> x) first) >>=
 layer (map2 (\(Region x) -> x) second) >>
 return ()

writeEmbed :: Int -> [Region] -> IO ()
writeEmbed index region =
 embedC (fromIntegral index) (fromIntegral (length region)) >>=
 cover (map (\(Region x) -> x) region) >>
 return ()

writeSideband :: [Int] -> IO ()
writeSideband todo = let
 todosC = fromIntegral (length todo)
 in sidebandC todosC >>= cover todo >> return ()

writeBoundary :: Int -> [Boundary] -> IO ()
writeBoundary index boundary =
 (boundaryC (fromIntegral index) (fromIntegral (length boundary))) >>=
 (cover (map (\(Boundary x) -> x) boundary)) >>
 return ()

handleEvent :: IO Bool
handleEvent = do
 event <- (eventC >>= peekCString)
 case event of
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

handlePlane :: Int -> IO ()
handlePlane index =
 readSideband >>= \todo ->
 readBoundary index >>= \boundary ->
 readPlanesC >>= \doneC ->
 correlatesC >>= \relatesC -> let
 done = Boundary (fromIntegral doneC)
 relates = fromIntegral relatesC
 point = map (done :) (subsets 2 boundary)
 classify = map (boundary \\) point
 boundaries = (length boundary) - 2
 relate = recurseF (boundaries +) relates (length point)
 in writePointSubC (fromIntegral (length point)) >>=
 writeBuffer (map (\(Boundary x) -> x) (concat point)) >>
 writeSideSubC (fromIntegral (length classify)) >>=
 writeBuffer (map (\(Boundary x) -> x) (concat classify)) >>
 correlateC (fromIntegral (length relate)) >>=
 writeBuffer relate >>
 writeSideband (todo `append` [index]) >>
 writeBoundary index (boundary `append` [done]) >>
 writePlanesC (doneC + 1) >>
 return ()

handleClassify :: IO ()
handleClassify = readPlanesC >>= \doneC -> sidebandsC >>= \todosC ->
 readSideband >>= handleClassifyF ((fromIntegral doneC) - (fromIntegral todosC)) >>= writeSideband

handleClassifyF :: Int -> [Int] -> IO [Int]
handleClassifyF done (index:todo) =
 readPlace index >>= \place ->
 readEmbed index >>= \embed ->
 readSidesC >>= \sidesC ->
 readSideBufC >>= readBuffer sidesC >>= \side -> let
 boundary = boundariesOfPlace place
 -- add boundaries accoriding to sidedness
 pair = subsets 2 boundary
 wrt = map (boundary \\) pair
 polyant = map2 (\(x,y) -> (x, Side y)) (handleClassifyG side wrt)
 (place1,embed1) = handleClassifyH (Boundary done) polyant place embed
 in writePlace index place1 >>
 writeEmbed index embed1 >>
 handleClassifyF (done+1) todo
handleClassifyF _ todo = return todo

handleClassifyG :: [Int] -> [[Boundary]] -> [[(Boundary,Int)]]
handleClassifyG a b = map (\(x,y) -> zip x y) (zip b (split a (map length b)))

handleClassifyH :: Boundary -> [[(Boundary,Side)]] -> Place -> [Region] -> (Place,[Region])
handleClassifyH a b c d = let
 region = fold' (++) (map (handleClassifyI c) b) []
 place = choose (divideSpace a (embedSpace region c) c)
 in (place, takeRegions (embedSpace d c) place)

handleClassifyI :: Place -> [(Boundary,Side)] -> [Region]
handleClassifyI a b = fold' (+\) (map (\(x, Side y) -> (head (image [x] a)) !! y) b) (regionsOfPlace a)

handleInflate :: Int -> IO ()
handleInflate index =
 readPlace index >>= \place ->
 readFacesC >>= \faces ->
 readFaceOkC >>= readBuffer faces >>= \valid ->
 readFaceSubC >>= readBuffer (faces * 6) >>= \face -> let
 -- replace embed for indicated place by all inside regions
 (boundary,space) = unzipPlace place
 boundaried = map (\(Boundary x) -> x) boundary
 regions = regionsOfSpace space
 embed = filter (\x -> not (oppositeOfRegionExists boundary x space)) regions
 -- find boundaries between inside and outside regions
 embed1 r x = oppositeOfRegion [x] r space
 embed2 r x = (x, r, embed1 r x)
 embed3 r = attachedBoundaries r space
 embed4 r = map (embed2 r) (embed3 r)
 embed5 = map embed4 embed
 -- [(base,inside,opposite)]
 attached1 :: [(Boundary,Region,Region)]
 attached1 = concat embed5
 -- [(base,inside,outside)]
 attached2 :: [(Boundary,Region,Region)]
 attached2 = filter (\(_,_,y) -> not (elem y embed)) attached1
 -- choose vertex per found boundary
 -- [(base,inside,chosen)]
 attached3 :: [(Boundary,Region,[Boundary])]
 attached3 = map (\(x,r,_) -> (x, r, choose (filter (\w -> elem x w) (attachedFacets 3 r space)))) attached2
 -- find all edges per boundary
 -- [(base,inside,chosen,[edge])]
 attached4 :: [(Boundary,Region,[Boundary],[[Boundary]])]
 attached4 = map (\(x,r,y) -> (x, r, y, filter (\w -> elem x w) (attachedFacets 2 r space))) attached3
 -- find vertex pair per found edge
 -- [(base,chosen,[(edge,endpoint)])]
 attached5 :: [(Boundary,[Boundary],[([Boundary],[[Boundary]])])]
 attached5 = map (\(x,r,y,z) -> (x, y, map (\w -> (w, filter (\v -> all (\u -> elem u v) w) (attachedFacets 3 r space))) z)) attached4
 -- remove edges containg chosen vertex
 -- [(base,chosen,[(edge,[endpoint])])]
 attached6 :: [(Boundary,[Boundary],[([Boundary],[[Boundary]])])]
 attached6 = map (\(x,y,z) -> (x, y, filter (\(_,v) -> all (any (\u -> not (elem u y))) v) z)) attached5
 -- construct face from base vertex and edge
 face1 = map (\(x,y,z) -> (x, filter (/=x) y, map (\(w,v) -> (filter (/=x) w, map (\u -> filter (/=x) u) v)) z)) attached6
 face2 = map (\(x,y,z) -> (x, y, map (\(w,v) -> (w, map (\u -> u \\ w) v)) z)) face1
 face3 = concat (concat (map (\(x,y,z) -> map (\(w,v) -> concat [[x],w,(concat v),y]) z) face2))
 face4 = map (\(Boundary x) -> x) (zipBoundaries face3 boundary)
 -- remove faces of indexed place
 valid1 = map (\(x,y) -> if (x /= 0) && (not (elem (head y) boundaried)) then 1 else 0) (zip valid (split face (repeat 6)))
 -- indicate new faces are valid
 valid2 :: [Int]
 valid2 = valid1 `append` (replicate (quot (length face4) 6) 1)
 -- append found faces
 in writeEmbed index embed >>
 writeFaceOkC (fromIntegral (length valid2)) >>= writeBuffer valid2 >>
 writeFaceSubC (fromIntegral (length face4)) >>= writeBuffer face4 >>
 return ()

handleFill :: Int -> IO ()
handleFill = undefined

handleHollow :: Int -> IO ()
handleHollow = undefined

handleRemove :: Int -> Int -> IO ()
handleRemove = undefined
