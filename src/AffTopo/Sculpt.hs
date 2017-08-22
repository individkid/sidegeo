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
foreign import ccall "places" placesC :: CInt -> IO CInt
foreign import ccall "embed" embedC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "embeds" embedsC :: CInt -> IO CInt
foreign import ccall "sideband" sidebandC :: CInt -> IO (Ptr CInt)
foreign import ccall "sidebands" sidebandsC :: IO CInt
foreign import ccall "correlate" correlateC :: CInt -> IO (Ptr CInt)
foreign import ccall "correlates" correlatesC :: IO CInt
foreign import ccall "faceToPlane" faceToPlaneC :: CInt -> IO (Ptr CInt)
foreign import ccall "planeToPlace" planeToPlaceC :: CInt -> IO (Ptr CInt)
foreign import ccall "planeToPlaces" planeToPlacesC :: IO CInt
foreign import ccall "planeToPoint" planeToPointC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "planeToPoints" planeToPointsC :: CInt -> IO CInt
foreign import ccall "boundary" boundaryC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaries" boundariesC :: CInt -> IO CInt
foreign import ccall "readFaceSub" readFaceSubC :: IO (Ptr CInt)
foreign import ccall "readFaces" readFacesC :: IO CInt
foreign import ccall "readFrameSub" readFrameSubC :: IO (Ptr CInt)
foreign import ccall "readFrames" readFramesC :: IO CInt
foreign import ccall "readPointSub" readPointSubC :: IO (Ptr CInt)
foreign import ccall "readPoints" readPointsC :: IO CInt
foreign import ccall "readSideBuf" readSideBufC :: IO (Ptr CInt)
foreign import ccall "readSides" readSidesC :: IO CInt
foreign import ccall "writeFaceSub" writeFaceSubC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "writeFrameSub" writeFrameSubC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "writePointSub" writePointSubC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "writeSideSub" writeSideSubC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "print" printC :: CInt -> IO (Ptr CChar)
foreign import ccall "error" errorC :: CInt -> IO (Ptr CChar)
foreign import ccall "event" eventC :: IO (Ptr CChar)
foreign import ccall "stringArgument" stringArgumentC :: IO (Ptr CChar)
foreign import ccall "intArgument" intArgumentC :: IO CInt

printStr :: [Char] -> IO ()
printStr str = do
 ptr <- printC (fromIntegral (length str))
 pokeArray ptr (map castCharToCChar str)

errorStr :: [Char] -> IO ()
errorStr str = do
 ptr <- errorC (fromIntegral (length str))
 pokeArray ptr (map castCharToCChar str)

plusPtr' :: Int -> Ptr CInt -> Ptr CInt
plusPtr' offset ptr = let
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

readBuffer :: IO CInt -> IO (Ptr CInt) -> IO [Int]
readBuffer size ptr = size >>= \sizeC -> ptr >>= peekArray (fromIntegral sizeC) >>= return . (map fromIntegral)

readQueue :: IO CInt -> (CInt -> IO (Ptr CInt)) -> IO [Int]
readQueue size fun = size >>= \sizeC -> fun 0 >>= peekArray (fromIntegral sizeC) >>= return . (map fromIntegral)

peekBuffer :: IO CInt -> IO (Ptr CInt) -> IO Int
peekBuffer offset ptr = offset >>= \offsetC -> ptr >>= peek . (plusPtr' (fromIntegral offsetC)) >>= return . fromIntegral

readSize :: IO CInt -> IO Int
readSize size = size >>= return . fromIntegral

writeBuffer :: (CInt -> CInt -> IO (Ptr CInt)) -> [Int] -> IO ()
writeBuffer fun list = fun 0 (fromIntegral (length list)) >>= \ptr -> pokeArray ptr (map fromIntegral list)

writeQueue :: (CInt -> IO (Ptr CInt)) -> [Int] -> IO ()
writeQueue fun list = fun (fromIntegral (length list)) >>= \ptr -> pokeArray ptr (map fromIntegral list)

appendBuffer :: IO CInt -> (CInt -> CInt -> IO (Ptr CInt)) -> [Int] -> IO ()
appendBuffer size fun list = size >>= \sizeC -> fun sizeC (fromIntegral (length list)) >>= \ptr ->
 pokeArray ptr (map fromIntegral list)

appendQueue :: IO CInt -> (CInt -> IO (Ptr CInt)) -> [Int] -> IO ()
appendQueue size fun list = size >>= \sizeC -> fun (sizeC + (fromIntegral (length list))) >>=
 return . (plusPtr' (fromIntegral sizeC)) >>= \ptr -> pokeArray ptr (map fromIntegral list)

decodePlace :: [Int] -> [Int] -> Place
decodePlace boundaryI list = let
 size = length boundaryI
 boundary = map Boundary boundaryI
 (firsts,list1) = splitAt size list
 (seconds,list2) = splitAt size list1
 (list3,list4) = splitAt (sum firsts) list2
 first = map2 Region (split list3 firsts)
 second = map2 Region (split list4 seconds)
 in zipWith3 (\x y z -> (x,[y,z])) boundary first second

encodePlace :: Place -> [Int]
encodePlace place = let
 firsts = map (length . head) (range place)
 seconds = map (length . last) (range place)
 first = concat (map head (range place))
 second = concat (map last (range place))
 in concat [firsts, seconds, map (\(Region x) -> x) first, map (\(Region x) -> x) second]

handleEvent :: IO Bool
handleEvent = do
 event <- (eventC >>= peekCString)
 case event of
  "Plane" -> handleEventF >>= handlePlane >> return False
  "Classify" -> handleClassify >> return False
  "Inflate" -> handleEventF >>= handleInflate >> return False
  "Pierce" -> handleEventF >>= handlePierce >> return False
  "Fill" -> handleFill <$> handleEventF <*> handleEventF >> return False
  "Hollow" -> handleHollow <$> handleEventF <*> handleEventF >> return False
  "Remove" ->  handleRemove <$> handleEventG <*> handleEventF >> return False
  "Conform" -> handleConform >> return False
  "Error" -> return True
  "Done" -> return True
  _ -> printStr (concat ["unknown event ",(show event),"\n"]) >> return True

handleEventF :: IO Int
handleEventF = intArgumentC >>= (return . fromIntegral)

handleEventG :: IO String
handleEventG = stringArgumentC >>= peekCString

handlePlane :: Int -> IO ()
handlePlane index = let indexC = fromIntegral index in
 readQueue (boundariesC indexC) (boundaryC indexC) >>= return . (map Boundary) >>= \boundary ->
 readSize planeToPlacesC >>= \done ->
 peekBuffer (fmap ((negate 1) +) correlatesC) (correlateC 0) >>= \base ->
 readPointsC >>= \count -> let
 point = map ((Boundary done) :) (subsets 2 boundary)
 classify = map (boundary \\) point
 boundaries = (length boundary) - 2
 relate = map (\x -> base + (boundaries * (1 + x))) (indices (length point))
 counts = map (count+) (indices (length point))
 mapping = welldef (concat (map (\z -> zipWith (\x y -> (x !! z, y)) point counts) (indices 3)))
 mapped = map (\(Boundary x, y) -> (fromIntegral x, map fromIntegral y)) mapping
 in writeBuffer writeSideSubC (map (\(Boundary x) -> x) (concat classify)) >>
 appendBuffer readPointsC writePointSubC (map (\(Boundary x) -> x) (concat point)) >>
 appendQueue correlatesC correlateC relate >>
 appendQueue sidebandsC sidebandC [index] >>
 appendQueue (boundariesC indexC) (boundaryC indexC) [done] >>
 appendQueue planeToPlacesC planeToPlaceC [index] >>
 sequence_ (map (\(x,y) -> appendQueue (planeToPointsC x) (planeToPointC x) y) mapped) >>
 return ()

handleClassify :: IO ()
handleClassify =
 readSize planeToPlacesC >>= \done ->
 readSize sidebandsC >>= \todos ->
 readQueue sidebandsC sidebandC >>=
 handleClassifyF (done - todos) >>=
 writeQueue sidebandC

handleClassifyF :: Int -> [Int] -> IO [Int]
handleClassifyF done (index:todo) = let indexC = fromIntegral index in
 readQueue (placesC indexC) (placeC indexC) >>= \placeI ->
 readQueue (embedsC indexC) (embedC indexC) >>= \embedI ->
 readBuffer readSidesC readSideBufC >>= \side ->
 readQueue (boundariesC indexC) (boundaryC indexC) >>= \boundaryI -> let
 boundary = map Boundary boundaryI
 place = decodePlace boundaryI placeI
 embed = map Region embedI
 -- add boundaries accoriding to sidedness
 pair = subsets 2 boundary
 wrt = map (boundary \\) pair
 polyant = map2 (\(x,y) -> (x, Side y)) (handleClassifyG side wrt)
 (place1,embed1) = handleClassifyH (Boundary done) polyant place embed
 in writeQueue (placeC indexC) (encodePlace place1) >>
 writeQueue (embedC indexC) (map (\(Region x) -> x) embed1) >>
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
handleInflate index = handleInflateF handleInflateG index

handleInflateF :: (Place -> [Region]) -> Int -> IO ()
handleInflateF fun index = let
 indexC = fromIntegral index in
 readBuffer readFacesC readFaceSubC >>= \face ->
 readQueue (placesC indexC) (placeC indexC) >>= \placeI ->
 readQueue (boundariesC indexC) (boundaryC indexC) >>= \boundaryI -> let
 place = decodePlace boundaryI placeI
 -- replace embed for indicated place by all inside regions
 (boundary,space) = unzipPlace place
 boundaried = map (\(Boundary x) -> x) boundary
 -- find boundaries between inside and outside regions
 embed = fun place
 embed1 r x = oppositeOfRegion [x] r space
 embed2 r x = (x, r, embed1 r x)
 embed3 r = attachedBoundaries r space
 embed4 r = map (embed2 r) (embed3 r)
 embed5 = map embed4 embed
 attached1 = concat embed5
 attached2 = filter (\(_,_,y) -> not (elem y embed)) attached1
 -- choose vertex per found boundary
 attached3 = map (\(x,r,_) -> (x, r, choose (filter (\w -> elem x w) (attachedFacets 3 r space)))) attached2
 -- find all edges per boundary
 attached4 = map (\(x,r,y) -> (x, r, y, filter (\w -> elem x w) (attachedFacets 2 r space))) attached3
 -- find vertex pair per found edge
 attached5 = map (\(x,r,y,z) -> (x, y, map (\w -> (w, filter (\v -> all (\u -> elem u v) w) (attachedFacets 3 r space))) z)) attached4
 -- remove edges containg chosen vertex
 attached6 = map (\(x,y,z) -> (x, y, filter (\(_,v) -> all (any (\u -> not (elem u y))) v) z)) attached5
 -- construct face from base vertex and edge
 face1 = map (\(x,y,z) -> (x, filter (/=x) y, map (\(w,v) -> (filter (/=x) w, map (\u -> filter (/=x) u) v)) z)) attached6
 face2 = map (\(x,y,z) -> (x, y, map (\(w,v) -> (w, map (\u -> u \\ w) v)) z)) face1
 face3 = concat (concat (map (\(x,y,z) -> map (\(w,v) -> concat [[x],w,(concat v),y]) z) face2))
 face4 = map (\(Boundary x) -> x) (zipBoundaries face3 boundary)
 -- remove faces of indexed place
 valid1 :: [[Int]]
 valid1 = filter (\x -> not (elem (head x) boundaried)) (split face (repeat 6))
 -- indicate new faces are valid
 valid2 :: [Int]
 valid2 = (concat valid1) `append` face4
 -- map faces to base boundaries
 valid3 :: [Int]
 valid3 = map head (split valid2 (repeat 6))
 -- append found faces
 in writeQueue (embedC indexC) (map (\(Region x) -> x) embed) >>
 writeBuffer writeFaceSubC valid2 >>
 writeQueue faceToPlaneC valid3 >>
 return ()

handleInflateG :: Place -> [Region]
handleInflateG place = let
 (boundary,space) = unzipPlace place
 in filter (\x -> not (oppositeOfRegionExists boundary x space)) (regionsOfSpace space)

-- fill sideSub with all boundaries from given place
handlePierce :: Int -> IO ()
handlePierce index = let indexC = fromIntegral index in
 readQueue (boundariesC indexC) (boundaryC indexC) >>= writeBuffer writeSideSubC >> return ()

handleFill :: Int -> Int -> IO ()
handleFill = handleFillF (++)

handleFillF :: ([Region] -> [Region] -> [Region]) -> Int -> Int -> IO ()
handleFillF fun index boundI = let
 indexC = fromIntegral index
 bound = Boundary boundI in
 readQueue (embedsC indexC) (embedC indexC) >>= \embedI ->
 readBuffer readSidesC readSideBufC >>= \sideI -> let
 embed = map Region embedI
 side = map Side sideI
 in handleInflateF (handleFillG fun bound embed side) index

handleFillG :: ([Region] -> [Region] -> [Region]) -> Boundary -> [Region] -> [Side] -> Place -> [Region]
handleFillG fun bound embed side place = let
 (boundary,space) = unzipPlace place
 region = regionOfSides side space
 opposite = oppositeOfRegion [unzipBoundary bound boundary] region space
 both = [region,opposite]
 in fun embed both

handleHollow :: Int -> Int -> IO ()
handleHollow = handleFillF (\\)

-- if string is Face, invalidate faceSub with base of given boundary
-- if string is Boundary, invalidate faceSub involving boundary
handleRemove :: String -> Int -> IO ()
handleRemove "Place" index =
 readBuffer readFacesC readFaceSubC >>= \face ->
  readBuffer planeToPlacesC (planeToPlaceC 0) >>= \place -> let
  face1 = filter (\x -> (place !! (head x)) /= index) (split face (repeat 6))
  face2 = concat face1
  in writeBuffer writeFaceSubC face2 >>
  return ()
handleRemove "Face" index =
 readBuffer readFacesC readFaceSubC >>= \face -> let
 face1 = filter (\x -> not ((head x) == index)) (split face (repeat 6))
 face2 = concat face1
 in writeBuffer writeFaceSubC face2 >>
 return ()
handleRemove "Boundary" index =
 readBuffer readFacesC readFaceSubC >>= \face -> let
 face1 = filter (\x -> not (any (index ==) x)) (split face (repeat 6))
 face2 = concat face1
 in writeBuffer writeFaceSubC face2 >>
 -- TODO: remove boundary from place embed boundary
 return ()
handleRemove kind _ = errorStr (concat ["unknown kind ",kind,"\n"])

-- fill in frameSub
handleConform :: IO ()
handleConform = undefined
 -- read planeToPoint to get points on each plane
 -- read faceSub to get corners of frames
 -- intersect planeToPoint of corners to find subscript triples
 -- write subscript triples to frameSub


