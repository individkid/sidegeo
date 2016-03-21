module SideGeo.Lambda where

import qualified Data.List
import qualified Data.Set
import qualified Data.Map
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix

type Set = Data.Set.Set
type Map = Data.Map.Map
type Scalar = Double
type Matrix = HMatrix.Matrix Scalar
type Vector = HMatrix.Vector Scalar

class (Ord a, Enum a) => Holes a where
 zero :: a

bang :: Maybe a -> String -> a
bang Nothing b = error b
bang (Just a) _ = a

strip :: Maybe (Maybe a) -> Maybe a
strip Nothing = Nothing
strip (Just a) = a

listMap :: (a -> b) -> [a] -> [b]
listMap = Data.List.map

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter = Data.List.filter

listOptMap :: Eq b => (a -> Maybe b) -> [a] -> [b]
listOptMap f a = listMap g (listFilter h (listMap f a)) where
 g b = bang b "oops"
 h b = b /= Nothing

listFold1 :: (a -> a -> a) -> [a] -> a
listFold1 = Data.List.foldl1

listFold2 :: (a -> b -> b) -> [a] -> b -> b
listFold2 f a b = Data.List.foldl g b a where g b' a' = f a' b'

listAny :: (a -> Bool) -> [a] -> Bool
listAny f a = Data.List.any f a

listZip :: [a] -> [b] -> [(a,b)]
listZip = Data.List.zip

listUnzip :: [(a,b)] -> ([a],[b])
listUnzip = Data.List.unzip

listFromSize :: (Int -> a) -> Int -> [a]
listFromSize f n = listMap f [0..n]

listFromSize2 :: (Int -> Int -> a) -> Int -> Int -> [[a]]
listFromSize2 f n m = listFromSize (f0 m) n where
 f0 m' n' = listFromSize (f n') m'

setSize :: Ord a => Set a -> Int
setSize = Data.Set.size

setNonempty :: Ord a => Set a -> Bool
setNonempty a = (setSize a) /= 0

member :: Ord a => Set a -> a -> Bool
member a b = Data.Set.member b a

setEmpty :: Ord a => Set a
setEmpty = Data.Set.empty

single :: Ord a => a -> Set a
single = Data.Set.singleton

maybeChoose :: Ord a => Set a -> Maybe a
maybeChoose a
 | (setSize a) == 0 = Nothing
 | otherwise = Just (Data.Set.elemAt 0 a)

choose :: Ord a => Set a -> a
choose a = bang (maybeChoose a) "set empty"

maybeChoose2 :: Ord a => Set a -> (Maybe a,Maybe a)
maybeChoose2 a
 | (setSize a) == 0 = (Nothing,Nothing)
 | (setSize a) == 1 = (Just (Data.Set.elemAt 0 a),Nothing)
 | otherwise = (Just (Data.Set.elemAt 0 a),Just (Data.Set.elemAt 1 a))

choose2 :: Ord a => Set a -> (a,a)
choose2 a = (bang a0 "set empty", bang a1 "set singleton") where (a0,a1) = maybeChoose2 a

maybeChooseRemove :: Ord a => Set a -> (Maybe a,Set a)
maybeChooseRemove a
 | (setSize a) == 0 = (Nothing,a)
 | otherwise = (Just h,t) where
 h = choose a
 t = remove a h

chooseRemove :: Ord a => Set a -> (a,Set a)
chooseRemove a = (bang a0 "set empty", a1) where (a0,a1) = maybeChooseRemove a

setToList :: Ord a => Set a -> [a]
setToList = Data.Set.toAscList

setFromList :: Ord a => [a] -> Set a
setFromList = Data.Set.fromList

setZip :: Ord a => Ord b => Set a -> Set b -> Set (a,b)
setZip a b = setFromList (listZip (setToList a) (setToList b))

setUnzip :: Ord a => Ord b => Set (a,b) -> (Set a, Set b)
setUnzip a = (setFromList b, setFromList c) where
 (b,c) = listUnzip (setToList a)

holes :: Holes a => Set a -> Int -> Set a
holes _ 0 = setFromList []
holes a b = differ (setFromList [zero..c]) a where
 c = f [zero..] (b-1)
 f [] _ = zero
 f (c':d') b'
  | member a c' = f d' b'
  | b' == 0 = c'
  | otherwise = f d' (b'-1)

hole :: Holes a => Set a -> a
hole a = choose (holes a 1)

setSets :: Ord a => Set a -> Set Int -> Set (Set a)
setSets a b
 | member b (setSize a) = union (single a) (f a b)
 | otherwise = f a b where
 f a' b' = unions (setMap (g a') (setFilter ((>) (setSize a')) b'))
 g a' b'
  | b' == 1 = setMap single a'
  | ((setSize a') - 1) > b' = union (unions (setMap (i0 a' b') a')) (unions (setMap (j0 a' b') a'))
  | otherwise = union (unions (setMap (i0 a' b') a')) (setMap (remove a') a')
 i0 a' b' = i1 a' b'
 i1 a' b' c' = setMap (k c') (g (remove a' c') (b'-1))
 j0 a' b' = j1 a' b'
 j1 a' b' c' = g (remove a' c') b'
 k c' d' = insert d' c'

selects :: Ord a => Set a -> Set (a, Set a)
selects as = setMap f as where
 f a = (a, remove as a)

permutes :: Ord a => Set a -> Set [a]
permutes a = unions (setMap f (selects a)) where
 f (a',b') = setMap (g a') (permutes b')
 g a' b' = a':b'

setMaps :: Ord a => Ord b => Set a -> Set b -> Set (Map a b)
setMaps as bs = setMap f (permutes bs) where
 al = setToList as
 f bl = mapFromList (listZip al bl)

setAll :: Ord a => (a -> Bool) -> Set a -> Bool
setAll f a = Data.List.all f (setToList a)

setAny :: Ord a => (a -> Bool) -> Set a -> Bool
setAny f a = Data.List.any f (setToList a)

setMap :: Ord a => Ord b => (a -> b) -> Set a -> Set b
setMap = Data.Set.map

setMap2 :: Ord a => Ord b => Ord c => (a -> b -> c) -> Set a -> Set b -> Set c
setMap2 f a b = unions (setMap g a) where
 g a' = setMap (f a') b

setOptMap :: Ord a => Ord b => (a -> Maybe b) -> Set a -> Set b
setOptMap f a = setFromList (listOptMap f (setToList a))

setConnect :: Ord a => (a -> Set a) -> a -> Set a
setConnect f a = setConnect2 f (single a)

setConnect2 :: Ord a => (a -> Set a) -> Set a -> Set a
setConnect2 f a = g a setEmpty where
 g todo rslt = if (setSize todo) == 0 then rslt else g todo0 rslt0 where
  a0 = choose todo
  rslt0 = insert rslt a0
  todo0 = differ (union todo (f a0)) rslt0

setMaybeFind :: Ord a => (a -> Bool) -> Set a -> Maybe a
setMaybeFind f a = Data.List.find f (setToList a)

setFind :: Ord a => (a -> Bool) -> Set a -> a
setFind f a = bang (setMaybeFind f a) "set wrong"

setFilter :: Ord a => (a -> Bool) -> Set a -> Set a
setFilter = Data.Set.filter

setFold1 :: Ord a => (a -> a -> a) -> Set a -> a
setFold1 f a = listFold1 f (setToList a)

setFold2 :: Ord a => (a -> b -> b) -> Set a -> b -> b
setFold2 f a b = Data.List.foldl' g b (setToList a) where g b' a' = f a' b'

setFoldBackElse1 :: Ord a => (a -> a -> b) -> (b -> a) -> (a -> b) -> b -> Set a -> b
setFoldBackElse1 f g h b a
 | (setSize a) == 0 = b
 | (setSize a) == 1 = h (choose a)
 | (setSize a) == 2 = b0
 | otherwise = setFoldBackElse1 f g h b a2 where
 (a0,a1) = choose2 a
 b0 = f a0 a1
 a2 = insert (forceRemove (forceRemove a a0) a1) (g b0)

setFoldBackElse2 :: Ord a => (a -> b -> c) -> (c -> b) -> (b -> c) -> Set a -> b -> c
setFoldBackElse2 f g h a b
 | (setSize a) == 0 = h b
 | (setSize a) == 1 = f a0 b
 | otherwise = setFoldBackElse2 f g h a1 b1 where
 a0 = choose a
 a1 = remove a a0
 b1 = g (f a0 b)

forceInsert :: Ord a => Set a -> a -> Set a
forceInsert a b = Data.Set.insert b a

insert :: Ord a => Set a -> a -> Set a
insert a b
 | member a b = error "already member"
 | otherwise = forceInsert a b

inserts :: Ord a => Set a -> Set a -> Set a
inserts a b
 | (intersect a b) /= setEmpty = error "not disjoint"
 | otherwise = union a b

forceRemove :: Ord a => Set a -> a -> Set a
forceRemove a b = Data.Set.delete b a

remove :: Ord a => Set a -> a -> Set a
remove a b
 | not (member a b) = error "not member"
 | otherwise = forceRemove a b

removes :: Ord a => Set a -> Set a -> Set a
removes a b
 | (intersect a b) /= b = error "not proper"
 | otherwise = differ a b

insertRemove :: Ord a => Set a -> a -> Set a
insertRemove a b = if member a b then remove a b else insert a b

-- aka forceInserts
union :: Ord a => Set a -> Set a -> Set a
union = Data.Set.union

unions :: Ord a => Set (Set a) -> Set a
unions a = listFold1 union (setToList a)

-- aka nothing because why intersect sets known to be proper or disjoint
intersect :: Ord a => Set a -> Set a -> Set a
intersect = Data.Set.intersection

intersects :: Ord a => Set (Set a) -> Set a
intersects a = listFold1 intersect (setToList a)

-- aka forceRemoves
differ :: Ord a => Set a -> Set a -> Set a
differ = Data.Set.difference

-- aka insertRemoves
symmetric :: Ord a => Set a -> Set a -> Set a
symmetric a b = union (differ a b) (differ b a)

mapSize :: Map a b -> Int
mapSize = Data.Map.size

mapNonempty :: Map a b -> Bool
mapNonempty a = (mapSize a) /= 0

maybeSub :: Ord a => Map a b -> a -> Maybe b
maybeSub m a = Data.Map.lookup a m

maybeSub2 :: Ord a => Ord b => Map a (Map b c) -> a -> b -> Maybe c
maybeSub2 a b c = f (maybeSub a b) where
 f Nothing = Nothing
 f (Just d) = maybeSub d c

sub :: Ord a => Map a b -> a -> b
sub a b = f (maybeSub a b) where
 f Nothing = error "not found"
 f (Just c) = c

sub2 :: Ord a => Ord b => Map a (Map b c) -> a -> b -> c
sub2 a b c = sub (f (maybeSub a b)) c where
 f Nothing = error "not findable"
 f (Just d) = d

sub3 :: Ord a => Ord b => Ord c => Map a (Map b (Map c d)) -> a -> b -> c -> d
sub3 t a b c = sub (f (maybeSub2 t a b)) c where
 f Nothing = error "no find"
 f (Just e) = e

keysSet :: Map a b -> Set a
keysSet = Data.Map.keysSet

valsSet :: Ord a => Ord b => Map a b -> Set b
valsSet a
 | (mapSize a) == 0 = error "range undefined"
 | otherwise = setMap (sub a) (keysSet a)

keysSet2 :: Ord a => Ord b => Ord c => Map a (Map b c) -> Set b
keysSet2 a = unions (setMap keysSet (valsSet a))

valsSet2 :: Ord a => Ord b => Ord c => Map a (Map b c) -> Set c
valsSet2 a = unions (setMap valsSet (valsSet a))

mapEmpty :: Ord a => Map a b
mapEmpty = Data.Map.empty

fromSet :: (a -> b) -> Set a -> Map a b
fromSet = Data.Map.fromSet

fromSet2 :: (a -> b -> c) -> Set a -> Set b -> Map a (Map b c)
fromSet2 f a b = fromSet g a where g a' = fromSet (f a') b

fromOptSet :: (a -> Maybe b) -> Set a -> Map a b
fromOptSet f a = Data.Map.map g (Data.Map.filter h (fromSet f a)) where
 g a' = bang a' "fromOptSet error"
 h Nothing = False
 h (Just _) = True

fromOptSet2 :: (a -> b -> Maybe c) -> Set a -> Set b -> Map a (Map b c)
fromOptSet2 f a b = Data.Map.filter h (fromSet g a) where
 g a' = fromOptSet (f a') b
 h a' = not (Data.Map.null a')

fromSize :: (Int -> a) -> Int -> Map Int a
fromSize f a = mapFromList (listZip [0..(a-1)] (listMap f [0..(a-1)]))

fromSize2 :: (Int -> Int -> a) -> Int -> Int -> Map Int (Map Int a)
fromSize2 f a b = fromSize (g b) a where
 g b' a' = fromSize (f a') b'

fromSize3 :: (Int -> Int -> Int -> a) -> Int -> Int -> Int -> Map Int (Map Int (Map Int a))
fromSize3 f a b c = fromSize (g b c) a where
 g a' b' c' = fromSize2 (f a') b' c'

mapUnzip :: Ord a => Map a (b,c) -> (Map a b, Map a c)
mapUnzip a = (valsMap f a, valsMap g a) where
 f (b,_) = b; g (_,c) = c

fromKeysVals :: Ord a => Ord b => Set a -> Set b -> Map a b
fromKeysVals a b = mapFromSet (setZip a b)

fromVals :: Ord a => Set a -> Map Int a
fromVals a = fromKeysVals (setFromList [0..((setSize a)-1)]) a

fromKeys :: Ord a => Set a -> Map a Int
fromKeys a = fromKeysVals a (setFromList [0..((setSize a)-1)])

mapFromList :: Ord a => [(a,b)] -> Map a b
mapFromList = Data.Map.fromList

mapToList :: Ord a => Map a b -> [(a,b)]
mapToList = Data.Map.toList

mapFromSet :: Ord a => Ord b => Set (a,b) -> Map a b
mapFromSet = mapFromList . setToList

mapToSet :: Ord a => Ord b => Map a b -> Set (a,b)
mapToSet = setFromList . mapToList

inverse :: Ord a => Ord b => Map a b -> Map b a
inverse a = mapFromList (listMap f (mapToList a)) where
 f (a',b') = (b',a')

adverse :: Ord a => Ord b => Map a b -> Map b (Set a)
adverse a = listFold2 f (setToList (keysSet a)) (fromSet g (valsSet a)) where
 f b c = let d = sub a b in replace c (d, insert (sub c d) b)
 g _ = setEmpty

image :: Ord a => Ord b => Map a b -> Set a -> Set b
image a b = setMap (sub a) b

mapMap :: Ord a => Ord c => ((a,b) -> (c,d)) -> Map a b -> Map c d
mapMap f a = mapFromList (listMap f (mapToList a))

mapFilter :: Ord a => ((a,b) -> Bool) -> Map a b -> Map a b
mapFilter f a = mapFromList (listFilter f (mapToList a))

valsMap :: (b -> c) -> Map a b -> Map a c
valsMap = Data.Map.map

keysMap :: Ord a => Ord b => (a -> b) -> Map a c -> Map b c
keysMap f a = mapMap g a where
 g (a',b') = (f a', b')

extend :: Ord a => Map a b -> (a,b) -> Map a b
extend a (b,c) = Data.Map.insert b c a

restrict :: Ord a => Map a b -> Set a -> Map a b
restrict a b = fromSet (sub a) b

replace :: Ord a => Map a b -> (a,b) -> Map a b
replace a (b,c) = extend (restrict a (remove (keysSet a) b)) (b,c)

extends :: Ord a => Map a b -> Map a b -> Map a b
extends a b = mapFromList ((mapToList a) ++ (mapToList b))

matFromLists :: [[Scalar]] -> Matrix
matFromLists = HMatrix.fromLists

matToLists :: Matrix -> [[Scalar]]
matToLists = HMatrix.toLists

vecToList :: Vector -> [Scalar]
vecToList = HMatrix.toList

vecSize :: Vector -> Int
vecSize = HMatrix.size

vecToMap :: Vector -> Map Int Scalar
vecToMap a = mapFromList (listZip [0..((vecSize a)-1)] (vecToList a))

vecFromMap :: Map Int Scalar -> Vector
vecFromMap a = vecFromList (listMap (sub a) (setToList (keysSet a)))

vecFromList :: [Scalar] -> Vector
vecFromList = HMatrix.fromList

matTimes :: Matrix -> Matrix -> Matrix
matTimes = HMatrix.mul

matApply :: Matrix -> Vector -> Vector
matApply = HMatrix.app

matScale :: Matrix -> Scalar -> Matrix
matScale a b = HMatrix.scale b a

vecDot :: Vector -> Vector -> Scalar
vecDot = HMatrix.dot

vecScale :: Vector -> Scalar -> Vector
vecScale a b = HMatrix.scale b a

vecDist :: Vector -> Vector -> Scalar
vecDist a b = sqrt (vecDot a b)

matRow :: Matrix -> Int -> Vector
matRow = (HMatrix.!)

matCol :: Matrix -> Int -> Vector
matCol a b = (HMatrix.!) (matTr a) b

matTr :: Matrix -> Matrix
matTr = HMatrix.tr

matQr :: Matrix -> (Matrix,Matrix)
matQr = HMatrix.qr

matSolve :: Matrix -> Vector -> Vector
matSolve a b = let
 c = HMatrix.asColumn b
 d = HMatrix.linearSolve a c
 e = bang d "cannot solve" in
 HMatrix.flatten e

vecSolve :: Int -> Map Int (Map Int Vector) -> Set (Int, Vector) -> Vector
vecSolve a b c = let
 scalar = valsMap (valsMap vecToMap) b
 (direct,d) = mapUnzip (fromVals c)
 given = valsMap vecToMap d
 numer = fromSize3 i (a-1) (a-1) (a-1)
 i plane point dimen
  | base == dimen = (sub3 scalar base point dimen) + (sub2 given plane point)
  | otherwise = sub3 scalar base point dimen where
  base = sub direct plane
 len = fromSize2 j (a-1) (a-2)
 j plane point =
  vecDist (vecFromMap (sub2 numer plane point)) (vecFromMap (sub2 numer plane (point+1)))
 denom = fromSize2 k (a-1) (a-2)
 k plane point =
  listFold2 k2 (setToList (valsSet (mapFilter (k1 point) (sub len plane)))) (sub2 len plane point)
 k1 point (a',_) = a' < point
 k2 a' b' = a' * b'
 lists = listFromSize2 f (a-1) (a-1)
 f row col
  | col < a && row0 == col = -1.0
  | col < a && row0 /= col = 0.0
  | col >= a && row1 == col1 = coef
  | col >= a && row1 /= col1 = 0.0
  | otherwise = 0.0 where
  row0 = mod row a
  row1 = div row a
  col1 = div (col-a) (a-1)
  plane = row1
  point = mod (col-a) (a-1)
  dimen = row0
  coef = ((sub3 numer plane (point+1) dimen) - (sub3 numer plane point dimen)) / (sub2 denom plane point)
 list = listFromSize g (a-1)
 g row =
  negate (sub3 numer plane point dimen) where
  plane = div row a
  point = 0
  dimen = mod row a
 in matSolve (matFromLists lists) (vecFromList list)

vecSum :: Set Vector -> Vector
vecSum a = setFold1 f a where
 f a' b' = vecFromList (listMap g (listZip (vecToList a') (vecToList b')))
 g (a',b') = a' + b'

