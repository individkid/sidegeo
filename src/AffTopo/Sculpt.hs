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

-- import Foreign.Ptr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.C.String
import Data.IORef
import AffTopo.Naive

type Generic = [(Place,[Region])]
type Sideband = ([[Boundary]],Boundary,[Int])

foreign import ccall "generic" genericC :: CInt -> IO (Ptr CInt)
foreign import ccall "sideband" sidebandC :: CInt -> IO (Ptr CInt)
foreign import ccall "side" sideC :: IO (Ptr CInt)
foreign import ccall "face" faceC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "point" pointC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaryWrt" boundaryWrtC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaryOk" boundaryOkC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "faceValid" faceValidC :: CInt -> CInt -> IO (Ptr CInt)
foreign import ccall "boundaryCount" boundaryCountC :: IO CInt
foreign import ccall "print" printC :: CInt -> IO (Ptr CChar)
foreign import ccall "event" eventC :: IO CInt
foreign import ccall "intArgument" intArgumentC :: IO CInt

removeMe :: [Side]
removeMe = allSides

printStr :: [Char] -> IO ()
printStr str = do
 ptr <- printC (fromIntegral (length str))
 pokeArray ptr (map castCharToCChar str)

handleEvent :: IO Bool
handleEvent = do
 event <- eventC
 case event of
  0 -> do
   printStr "plane\n"
   return False
  1 -> do
   printStr "inflate\n"
   return False
  2 -> do
   face <- intArgumentC
   printStr (concat ["fill ",(show face),"\n"])
   return False
  3 -> do
   face <- intArgumentC
   printStr (concat ["hollow ",(show face),"\n"])
   return False
  4 -> return True
  5 -> return True
  _ -> return False

chip :: IO (IORef Int) -> Ptr CInt -> IO (Ptr CInt)
chip ref ptr = ref >>= (\x -> (fmap fromIntegral (peek ptr)) >>= (writeIORef x) >> (return (plusPtr ptr 1)))

peel :: IO (IORef [Int]) -> IO (IORef Int) -> Ptr CInt -> IO (Ptr CInt)
peel ref len ptr = ref >>= (\x -> len >>= readIORef >>= (\y -> (fmap (map fromIntegral) (peekArray y ptr)) >>= (writeIORef x) >> (return (plusPtr ptr y))))

cook :: (IO (IORef a) -> IO (IORef b) -> Ptr CInt -> IO (Ptr CInt)) -> IO (IORef [a]) -> IO (IORef a) -> IO (IORef [b]) -> Ptr CInt -> IO (Ptr CInt)
cook fun ref tmp len ptr = (cookH len) >>= (\x -> fold' (cookF fun ref tmp) x (return ptr))

cookF :: (IO (IORef a) -> IO (IORef b) -> Ptr CInt -> IO (Ptr CInt)) -> IO (IORef [a]) -> IO (IORef a) -> IO (IORef b) -> IO (Ptr CInt) -> IO (Ptr CInt)
cookF fun ref tmp len ptr = ptr >>= (\x -> cookG ref tmp (fun tmp len x))

cookG :: IO (IORef [a]) -> IO (IORef a) -> IO (Ptr CInt) -> IO (Ptr CInt)
cookG ref tmp ptr = ref >>= (\x -> (readIORef x) >>= (\y -> tmp >>= readIORef >>= (\z -> (writeIORef x (z:y)) >> ptr)))

cookH :: IO (IORef [b]) -> IO [IO (IORef b)]
cookH len = len >>= readIORef >>= (\x -> return (map newIORef x))

onion :: IO (IORef [[Int]]) -> IO (IORef [Int]) -> Ptr CInt -> IO (Ptr CInt)
onion ref len ptr = let tmp = newIORef [] in cook peel ref tmp len ptr

patch :: IO (IORef [[[Int]]]) -> IO (IORef [[Int]]) -> Ptr CInt -> IO (Ptr CInt)
patch ref len ptr = let tmp  = newIORef [[]] in cook onion ref tmp len ptr

--num-places,[num-boundaries],[[boundary]],num-done,num-todo,[place-todo]--
readSideband :: IO Sideband
readSideband = let
 places = newIORef (1::Int)
 boundaries = newIORef []
 boundary = newIORef [[]]
 dones = newIORef (1::Int)
 todos = newIORef (1::Int)
 todo = newIORef []
 in (sidebandC 0) >>=
 (chip places) >>=
 (peel boundaries places) >>=
 (onion boundary boundaries) >>=
 (chip dones) >>=
 (chip todos) >>=
 (peel todo todos) >>
 boundary >>= readIORef >>= (\x ->
 dones >>= readIORef >>= (\y ->
 todo >>= readIORef >>= (\z ->
 return (map2 Boundary x, Boundary y, z))))

-- (num-places,[num-boundaries],[num-regions],[[boundary]],[[region]],
--  [[first-halfspace-size]],[[second-halfspace-size]],[[[first-halfspace-region]]],[[[second-halfspace-region]]],
--  [embeded-region-size],[[embeded-region]])
readGeneric :: IO Generic
readGeneric = let
 places = newIORef (1::Int)
 boundaries = newIORef []
 regions = newIORef []
 boundary = newIORef [[]]
 region = newIORef [[]]
 firsts = newIORef [[]]
 seconds = newIORef [[]]
 first = newIORef [[[]]]
 second = newIORef [[[]]]
 embeds = newIORef []
 embed = newIORef [[]]
 in (genericC 0) >>=
 (chip places) >>=
 (peel boundaries places) >>=
 (peel regions places) >>=
 (onion boundary boundaries) >>=
 (onion region regions) >>=
 (onion firsts boundaries) >>=
 (onion seconds boundaries) >>=
 (patch first firsts) >>=
 (patch second seconds) >>=
 (peel embeds places) >>=
 (onion embed embeds) >>
 boundary >>= ((fmap (map2 Boundary)) . readIORef) >>= (\w ->
 first >>= ((fmap (map3 Region)) . readIORef) >>= (\x ->
 second >>= ((fmap (map3 Region)) . readIORef) >>= (\y ->
 embed >>= ((fmap (map2 Region)) . readIORef) >>= (\z -> let
 in return (zip (zipWith3 (zipWith3 readGenericF) w x y) z)))))

readGenericF :: Boundary -> [Region] -> [Region] -> (Boundary,[[Region]])
readGenericF a b c = (a,[b,c])

paste :: Int -> Ptr CInt -> IO (Ptr CInt)
paste = undefined

cover :: [Int] -> Ptr CInt -> IO (Ptr CInt)
cover = undefined

layer :: [[Int]] -> Ptr CInt -> IO (Ptr CInt)
layer = undefined

hoard :: [[[Int]]] -> Ptr CInt -> IO (Ptr CInt)
hoard = undefined

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

--type Sideband = ([[Boundary]],Boundary,[Int])
--num-places,[num-boundaries],[[boundary]],num-done,num-todo,[place-todo]--
writeSideband :: Sideband -> IO (Ptr CInt)
writeSideband (a,b,c) = let
 places = length a
 boundaries = map length a
 boundary = map2 (\(Boundary x) -> x) a
 dones = (\(Boundary x) -> x) b
 todos = length c
 size = 1 + places + (writeGenericF a) + 2 + todos
 in (sidebandC (fromIntegral size)) >>=
 (paste places) >>=
 (cover boundaries) >>=
 (layer boundary) >>=
 (paste dones) >>=
 (paste todos) >>=
 (cover c)

