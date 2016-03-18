module Space (
 Boundary,Region,Sidedness,Color,Space,
 side,bounds,regs,sides,color,rename,
 empty,order,system,simplex,
 subspace,superspace,spaces,overlaps) where

import qualified Data.List
import qualified Data.Set
import qualified Data.Map
import qualified Numeric.LinearAlgebra

linear :: Int -> Int -> Int
linear 0 n = 1
linear m 0 = 1
linear m n = (linear (m-1) n) + (linear (m-1) (n-1))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*(factorial (n-1))

ratio :: Int -> Int -> Float
ratio m n = p/q where
 x = factorial n
 y = linear m n
 z = m^n
 p = fromIntegral (x*y)
 q = fromIntegral z

bang :: Maybe a -> String -> a
bang Nothing b = error b
bang (Just a) b = a

strip :: Maybe (Maybe a) -> Maybe a
strip Nothing = Nothing
strip (Just a) = a

listMap :: (a -> b) -> [a] -> [b]
listMap = Data.List.map

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter = Data.List.filter

listOptMap :: Eq b => (a -> Maybe b) -> [a] -> [b]
listOptMap f a = listMap g (listFilter h (listMap f a)) where
 g (Just b) = b
 h b = b /= Nothing

listFold1 :: (a -> a -> a) -> [a] -> a
listFold1 = Data.List.foldl1

listFold2 :: (a -> b -> b) -> [a] -> b -> b
listFold2 f a b = Data.List.foldl g b a where g b a = f a b

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
 f0 m n = listFromSize (f n) m

type Set = Data.Set.Set

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

class (Ord a, Enum a) => Holes a where
 zero :: a

holes :: Holes a => Set a -> Int -> Set a
holes a 0 = setFromList []
holes a b = differ (setFromList [zero..c]) a where
 c = f [zero..] (b-1)
 f (c:d) b
  | member a c = f d b
  | b == 0 = c
  | otherwise = f d (b-1)

hole :: Holes a => Set a -> a
hole a = choose (holes a 1)

setSets :: Ord a => Set a -> Set Int -> Set (Set a)
setSets a b
 | member b (setSize a) = union (single a) (f a b)
 | otherwise = f a b where
 f :: Ord a => Set a -> Set Int -> Set (Set a)
 f a b = unions (setMap (g a) (setFilter ((>) (setSize a)) b))
 g :: Ord a => Set a -> Int -> Set (Set a)
 g a b
  | b == 1 = setMap single a
  | ((setSize a) - 1) > b = union (unions (setMap (h0' a b) a)) (unions (setMap (h1' a b) a))
  | otherwise = union (unions (setMap (h0' a b) a)) (setMap (remove a) a)
 h0' :: Ord a => Set a -> Int -> (a -> Set (Set a))
 h0' a b = h0 a b
 h0 :: Ord a => Set a -> Int -> a -> Set (Set a)
 h0 a b c = setMap (i c) (g (remove a c) (b-1))
 h1' :: Ord a => Set a -> Int -> (a -> Set (Set a))
 h1' a b = h1 a b
 h1 :: Ord a => Set a -> Int -> a -> Set (Set a)
 h1 a b c = g (remove a c) b
 i :: Ord a => a -> Set a -> Set a
 i c d = insert d c

selects :: Ord a => Set a -> Set (a, Set a)
selects as = setMap f as where
 f a = (a, remove as a)

permutes :: Ord a => Set a -> Set [a]
permutes as = unions (setMap f (selects as)) where
 f (a,as) = setMap (g a) (permutes as)
 g a al = a:al

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
 g a = setMap (f a) b

setOptMap :: Ord a => Ord b => (a -> Maybe b) -> Set a -> Set b
setOptMap f a = setFromList (listOptMap f (setToList a))

setConnect :: Ord a => (a -> Set a) -> a -> Set a
setConnect f a = setConnect2 f (single a)

setConnect2 :: Ord a => (a -> Set a) -> Set a -> Set a
setConnect2 f a = g a setEmpty where
 g todo rslt = if (setSize todo) == 0 then rslt else g todo0 rslt0 where
  a = choose todo
  rslt0 = insert rslt a
  todo0 = differ (union todo (f a)) rslt0

setMaybeFind :: Ord a => (a -> Bool) -> Set a -> Maybe a
setMaybeFind f a = Data.List.find f (setToList a)

setFind :: Ord a => (a -> Bool) -> Set a -> a
setFind f a = bang (setMaybeFind f a) "set wrong"

setFilter :: Ord a => (a -> Bool) -> Set a -> Set a
setFilter = Data.Set.filter

setFold1 :: Ord a => (a -> a -> a) -> Set a -> a
setFold1 f a = listFold1 f (setToList a)

setFold2 :: Ord a => (a -> b -> b) -> Set a -> b -> b
setFold2 f a b = Data.List.foldl' g b (setToList a) where g b a = f a b

setFoldBackElse1 :: Ord a => (a -> a -> b) -> (b -> a) -> (a -> b) -> b -> Set a -> b
setFoldBackElse1 f g h b a
 | (setSize a) == 0 = b
 | (setSize a) == 1 = h (choose a)
 | (setSize a) == 2 = b0
 | otherwise = setFoldBackElse1 f g h b a' where
 (a0,a1) = choose2 a
 b0 = f a0 a1
 a' = insert (forceRemove (forceRemove a a0) a1) (g b0)

setFoldBackElse2 :: Ord a => (a -> b -> c) -> (c -> b) -> (b -> c) -> Set a -> b -> c
setFoldBackElse2 f g h a b
 | (setSize a) == 0 = h b
 | (setSize a) == 1 = f a0 b
 | otherwise = setFoldBackElse2 f g h a' b' where
 a0 = choose a
 a' = remove a a0
 b' = g (f a0 b)

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

type Map = Data.Map.Map

mapSize :: Map k a -> Int
mapSize = Data.Map.size

mapNonempty :: Map k a -> Bool
mapNonempty a = (mapSize a) /= 0

maybeSub :: Ord k => Map k a -> k -> Maybe a
maybeSub m a = Data.Map.lookup a m

maybeSub2 :: Ord k1 => Ord k2 => Map k1 (Map k2 a) -> k1 -> k2 -> Maybe a
maybeSub2 t a b = f (maybeSub t a) where
 f Nothing = Nothing
 f (Just m) = maybeSub m b

sub :: Ord k => Map k a -> k -> a
sub m a = f (maybeSub m a) where
 f Nothing = error "not found"
 f (Just b) = b

sub2 :: Ord k1 => Ord k2 => Map k1 (Map k2 a) -> k1 -> k2 -> a
sub2 t a b = sub (f (maybeSub t a)) b where
 f Nothing = error "not findable"
 f (Just m) = m

sub3 :: Ord k1 => Ord k2 => Ord k3 => Map k1 (Map k2 (Map k3 a)) -> k1 -> k2 -> k3 -> a
sub3 t a b c = sub (f (maybeSub2 t a b)) c where
 f Nothing = error "no find"
 f (Just m) = m

keysSet :: Map k a -> Set k
keysSet = Data.Map.keysSet

valsSet :: Ord a => Ord b => Map a b -> Set b
valsSet a
 | (mapSize a) == 0 = error "range undefined"
 | otherwise = setMap (sub a) (keysSet a)

keysSet2 :: Ord a => Ord b => Ord c => Map a (Map b c) -> Set b
keysSet2 a = unions (setMap keysSet (valsSet a))

valsSet2 :: Ord a => Ord b => Ord c => Map a (Map b c) -> Set c
valsSet2 a = unions (setMap valsSet (valsSet a))

mapEmpty :: Ord k => Map k a
mapEmpty = Data.Map.empty

fromSet :: (k -> a) -> Set k -> Map k a
fromSet = Data.Map.fromSet

fromSet2 :: (k1 -> k2 -> a) -> Set k1 -> Set k2 -> Map k1 (Map k2 a)
fromSet2 f a b = fromSet g a where g a = fromSet (f a) b

fromOptSet :: (k -> Maybe a) -> Set k -> Map k a
fromOptSet f a = Data.Map.map g (Data.Map.filter h (fromSet f a)) where
 g a = bang a "fromOptSet error"
 h Nothing = False
 h (Just a) = True

fromOptSet2 :: (a -> b -> Maybe c) -> Set a -> Set b -> Map a (Map b c)
fromOptSet2 f a b = Data.Map.filter h (fromSet g a) where
 g a = fromOptSet (f a) b
 h a = not (Data.Map.null a)

fromSize :: (Int -> a) -> Int -> Map Int a
fromSize f a = mapFromList (listZip [0..(a-1)] (listMap f [0..(a-1)]))

fromSize2 :: (Int -> Int -> a) -> Int -> Int -> Map Int (Map Int a)
fromSize2 f a0 a1 = fromSize (f0 a1) a0 where
 f0 a1 a0 = fromSize (f a0) a1

fromSize3 :: (Int -> Int -> Int -> a) -> Int -> Int -> Int -> Map Int (Map Int (Map Int a))
fromSize3 f a0 a1 a2 = fromSize (f0 a1 a2) a0 where
 f0 a1 a2 a0 = fromSize2 (f a0) a1 a2

mapUnzip :: Ord a => Map a (b,c) -> (Map a b, Map a c)
mapUnzip a = (valsMap f0 a, valsMap f1 a) where
 f0 (b,c) = b; f1 (b,c) = c

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
 f (a,b) = (b,a)

adverse :: Ord a => Ord b => Map a b -> Map b (Set a)
adverse a = listFold2 f (setToList (keysSet a)) (fromSet g (valsSet a)) where
 f b c = let d = sub a b in replace c (d, insert (sub c d) b)
 g b = setEmpty

image :: Ord a => Ord b => Map a b -> Set a -> Set b
image a b = setMap (sub a) b

mapMap :: Ord a => Ord c => ((a,b) -> (c,d)) -> Map a b -> Map c d
mapMap f a = mapFromList (listMap f (mapToList a))

mapFilter :: Ord a => ((a,b) -> Bool) -> Map a b -> Map a b
mapFilter f a = mapFromList (listFilter f (mapToList a))

valsMap :: (a -> b) -> Map k a -> Map k b
valsMap = Data.Map.map

keysMap :: Ord a => Ord b => (a -> b) -> Map a v -> Map b v
keysMap f a = mapMap g a where
 g (a,b) = (f a, b)

extend :: Ord a => Map a b -> (a,b) -> Map a b
extend a (b,c) = Data.Map.insert b c a

restrict :: Ord a => Map a b -> Set a -> Map a b
restrict a b = fromSet (sub a) b

replace :: Ord a => Map a b -> (a,b) -> Map a b
replace a (b,c) = extend (restrict a (remove (keysSet a) b)) (b,c)

extends :: Ord a => Map a b -> Map a b -> Map a b
extends a b = mapFromList ((mapToList a) ++ (mapToList b))

type Scalar = Double
type Matrix = Numeric.LinearAlgebra.Matrix Scalar
type Vector = Numeric.LinearAlgebra.Vector Scalar

matFromLists :: [[Scalar]] -> Matrix
matFromLists = Numeric.LinearAlgebra.fromLists

matToLists :: Matrix -> [[Scalar]]
matToLists = Numeric.LinearAlgebra.toLists

vecToList :: Vector -> [Scalar]
vecToList = Numeric.LinearAlgebra.toList

vecSize :: Vector -> Int
vecSize = Numeric.LinearAlgebra.size

vecToMap :: Vector -> Map Int Scalar
vecToMap a = mapFromList (listZip [0..((vecSize a)-1)] (vecToList a))

vecFromMap :: Map Int Scalar -> Vector
vecFromMap a = vecFromList (listMap (sub a) (setToList (keysSet a)))

vecFromList :: [Scalar] -> Vector
vecFromList = Numeric.LinearAlgebra.fromList

matTimes :: Matrix -> Matrix -> Matrix
matTimes = (Numeric.LinearAlgebra.<>)

matApply :: Matrix -> Vector -> Vector
matApply = (Numeric.LinearAlgebra.#>)

matScale :: Matrix -> Scalar -> Matrix
matScale a b = Numeric.LinearAlgebra.scale b a

vecDot :: Vector -> Vector -> Scalar
vecDot = (Numeric.LinearAlgebra.<.>)

vecScale :: Vector -> Scalar -> Vector
vecScale a b = Numeric.LinearAlgebra.scale b a

vecDist :: Vector -> Vector -> Scalar
vecDist a b = sqrt (vecDot a b)

matRow :: Matrix -> Int -> Vector
matRow = (Numeric.LinearAlgebra.!)

matCol :: Matrix -> Int -> Vector
matCol a b = (Numeric.LinearAlgebra.!) (matTr a) b

matTr :: Matrix -> Matrix
matTr = Numeric.LinearAlgebra.tr

matQr :: Matrix -> (Matrix,Matrix)
matQr = Numeric.LinearAlgebra.qr

matSolve :: Matrix -> Vector -> Vector
matSolve a b = let
 c = Numeric.LinearAlgebra.asColumn b
 d = Numeric.LinearAlgebra.linearSolve a c
 e = bang d "cannot solve" in
 Numeric.LinearAlgebra.flatten e

vecSolve :: Int -> Map Int (Map Int Vector) -> Set (Int, Vector) -> Vector
vecSolve a b c = let
 scalar = valsMap (valsMap vecToMap) b
 (direct,d) = mapUnzip (fromVals c)
 given = valsMap vecToMap d
 numer = fromSize3 i (a-1) (a-1) (a-1)
 i plane point dimen
  | base == dimen = (sub3 scalar base point dimen) + (sub2 given plane point)
  | base /= dimen = sub3 scalar base point dimen where
  base = sub direct plane
 length = fromSize2 j (a-1) (a-2)
 j plane point =
  vecDist (vecFromMap (sub2 numer plane point)) (vecFromMap (sub2 numer plane (point+1)))
 denom = fromSize2 k (a-1) (a-2)
 k plane point =
  listFold2 k2 (setToList (valsSet (mapFilter (k1 point) (sub length plane)))) (sub2 length plane point)
 k1 point (a,b) =
  a < point
 k2 a b =
  a * b
 lists = listFromSize2 f (a-1) (a-1)
 f row col
  | col < a && row0 == col = -1.0
  | col < a && row0 /= col = 0.0
  | col >= a && row1 == col1 = coef
  | col >= a && row1 /= col1 = 0.0 where
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
 f a b = vecFromList (listMap g (listZip (vecToList a) (vecToList b)))
 g (a,b) = a + b

-- undefined
data Boundary = Boundary Int deriving (Show, Eq, Ord)
data Region = Region Int deriving (Show, Eq, Ord)
data Sidedness = Sidedness Int deriving (Show, Eq, Ord)
data Vertex = Vertex Int deriving (Show, Eq, Ord)
data Color = Color Int deriving (Show, Eq, Ord)

instance Enum Boundary where
 succ (Boundary x) = Boundary (succ x); pred (Boundary x) = Boundary (pred x)
 toEnum x = Boundary x; fromEnum (Boundary x) = x
instance Enum Region where
 succ (Region x) = Region (succ x); pred (Region x) = Region (pred x)
 toEnum x = Region x; fromEnum (Region x) = x
instance Enum Sidedness where
 succ (Sidedness x) = Sidedness (succ x); pred (Sidedness x) = Sidedness (pred x)
 toEnum x = Sidedness x; fromEnum (Sidedness x) = x
instance Enum Vertex where
 succ (Vertex x) = Vertex (succ x); pred (Vertex x) = Vertex (pred x)
 toEnum x = Vertex x; fromEnum (Vertex x) = x
instance Enum Color where
 succ (Color x) = Color (succ x); pred (Color x) = Color (pred x)
 toEnum x = Color x; fromEnum (Color x) = x

instance Holes Boundary where zero = Boundary 0
instance Holes Region where zero = Region 0
instance Holes Sidedness where zero = Sidedness 0
instance Holes Vertex where zero = Vertex 0
instance Holes Color where zero = Color 0

-- collections of undefined
type Boundaries = Set Boundary
type Regions = Set Region
type Sidednesses = Set Sidedness
type Vertices = Set Vertex
type Colors = Set Color
type Direction = Map Boundary Sidedness
type Directions = Set Direction
type Rainbow = Map Color Directions
type Polytope = Set Rainbow
type Reflection = Map Sidedness Sidedness
type Symmetry = Map Boundary (Boundary, Reflection)
type Symmetries = Set Symmetry

-- representations
type Side = Map Boundary (Map Region Sidedness) -- st
type Dual = Map Sidedness (Map Region Boundaries) -- dt
type Duali = Map Sidedness (Map Boundaries Region) -- di
type Duals = Directions -- ds
type Half = Map Sidedness (Map Boundary Regions) -- ht
type Halfi = Map Sidedness (Map Regions Boundary) -- hi
type Bounds = Boundaries -- bs
type Regs = Regions -- rs
type Sides = Sidednesses -- ss
type Neighbor = Map Boundary (Map Region Region) -- nt
type Attached = Map Sidedness (Map Boundary Regions) -- at
type Flat = Map Boundary Regions -- am
type Shell = Map Sidedness (Map Region Boundaries) -- bt
type Cage = Map Region Boundaries -- bm
type Disk = Map Sidedness (Map Region Regions) -- rt
type Blot = Map Region Regions -- rm
type Vert = Map Boundaries Vertex -- vm
type Verti = Map Vertex Boundaries -- vi
type Verts = Vertices -- vs
type Pencil = Map Vertex Regions -- pm
type Corner = Map Region Vertices -- qm
type Inside = Regions -- is
type Outside = Regions -- os
type Signs = Vertices -- gs
type Signb = Map Vertex Boundaries -- gb
type Signr = Map Vertex Rainbow -- gr
type Tope = Map Region Color -- tm
type Topei = Map Color Regions -- ti
type Topes = Colors -- ts
type Topez = Polytope -- tz
type Plane = Map Boundary Vector -- cb
type Coplane = Map Vertex Vector -- cv
type Basis = Map Int (Map Int Vector) -- ci

-- TODO autogenerate Type[0-9] Param[0-9] param[0-9]_param[0-9] param[0-9]_type[0-9] param[0-9]_type param[0-9]type_param[0-9] type[0-9]_param[0-9] type[0-9]undef_param[0-9] type[0-9]type_param[0-9] Tag Rep type0_rep reps_type[1-9] conversions type_space type[0-9]_space space_param[0-9] space_type

-- return values
data Side0 = Side0 Side deriving (Show, Eq, Ord) -- st
data Dual0 = Dual0 Dual deriving (Show, Eq, Ord) -- dt
data Duali0 = Duali0 Duali deriving (Show, Eq, Ord) -- di
data Duals0 = Duals0 Duals deriving (Show, Eq, Ord) -- ds
data Half0 = Half0 Half deriving (Show, Eq, Ord) -- ht
data Halfi0 = Halfi0 Halfi deriving (Show, Eq, Ord) -- hi
data Bounds0 = Bounds0 Bounds deriving (Show, Eq, Ord) -- bs
data Regs0 = Regs0 Regs deriving (Show, Eq, Ord) -- rs
data Sides0 = Sides0 Sides deriving (Show, Eq, Ord) -- ss
data Neighbor0 = Neighbor0 Neighbor deriving (Show, Eq, Ord) -- nt
data Attached0 = Attached0 Attached deriving (Show, Eq, Ord) -- at
data Flat0 = Flat0 Flat deriving (Show, Eq, Ord) -- am
data Shell0 = Shell0 Shell deriving (Show, Eq, Ord) -- bt
data Cage0 = Cage0 Cage deriving (Show, Eq, Ord) -- bm
data Disk0 = Disk0 Disk deriving (Show, Eq, Ord) -- rt
data Blot0 = Blot0 Blot deriving (Show, Eq, Ord) -- rm
data Vert0 = Vert0 Vert deriving (Show, Eq, Ord) -- vm
data Verti0 = Verti0 Verti deriving (Show, Eq, Ord) -- vi
data Verts0 = Verts0 Verts deriving (Show, Eq, Ord) -- vs
data Pencil0 = Pencil0 Pencil deriving (Show, Eq, Ord) -- pm
data Corner0 = Corner0 Corner deriving (Show, Eq, Ord) -- qm
data Inside0 = Inside0 Inside deriving (Show, Eq, Ord) -- is
data Outside0 = Outside0 Outside deriving (Show, Eq, Ord) -- os
data Signs0 = Signs0 Signs deriving (Show, Eq, Ord) -- gs
data Signb0 = Signb0 Signb deriving (Show, Eq, Ord) -- gb
data Signr0 = Signr0 Signr deriving (Show, Eq, Ord) -- gr
data Tope0 = Tope0 Tope deriving (Show, Eq, Ord) -- tm
data Topei0 = Topei0 Topei deriving (Show, Eq, Ord) -- ti
data Topes0 = Topes0 Topes deriving (Show, Eq, Ord) -- ts
data Topez0 = Topez0 Topez deriving (Show, Eq, Ord) -- tz
data Plane0 = Plane0 Plane deriving (Show, Eq, Ord) -- cb
data Coplane0 = Coplane0 Coplane deriving (Show, Eq, Ord) -- cv
data Basis0 = Basis0 Basis deriving (Show, Eq, Ord) -- ci

-- parameters for inducers and generators
data Side1 = Side1 Dual Sides deriving (Show, Eq, Ord)
 -- dt ss
data Side2 = Side2 Half Sides deriving (Show, Eq, Ord)
 -- ht ss
data Side3 = Side3 Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt bs rs ss
data Side4 = Side4 Half Bounds Regs Sides deriving (Show, Eq, Ord)
 -- ht bs rs ss
data Dual1 = Dual1 Side Bounds Sides deriving (Show, Eq, Ord)
 -- st bs ss
data Dual2 = Dual2 Side Bounds Regs Sides deriving (Show, Eq, Ord)
 -- st bs rs ss
data Dual3 = Dual3 Duali Sides deriving (Show, Eq, Ord)
 -- dt ss
data Duali1 = Duali1 Dual Sides deriving (Show, Eq, Ord)
 -- dt ss
data Duals1 = Duals1 Side Bounds Regs deriving (Show, Eq, Ord)
 -- st bs rs
data Half1 = Half1 Side Regs Sides deriving (Show, Eq, Ord)
 -- st rs ss
data Half2 = Half2 Side Bounds Regs Sides deriving (Show, Eq, Ord)
 -- st bs rs ss
data Half3 = Half3 Halfi Sides deriving (Show, Eq, Ord)
 -- hi ss
data Halfi1 = Halfi1 Half Sides deriving (Show, Eq, Ord)
 -- ht ss
data Neighbor1 = Neighbor1 Dual Duali Sides deriving (Show, Eq, Ord)
 -- dt di ss
data Neighbor2 = Neighbor2 Dual Duali Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt di bs rs ss
data Attached1 = Attached1 Side Regs Neighbor deriving (Show, Eq, Ord)
 -- st rs nt
data Attached2 = Attached2 Side Bounds Regs Sides Neighbor deriving (Show, Eq, Ord)
 -- st bs rs ss nt
data Flat1 = Flat1 Sides Attached deriving (Show, Eq, Ord)
 -- ss at
data Flat2 = Flat2 Bounds Sides Attached deriving (Show, Eq, Ord)
 -- bs ss at
data Shell1 = Shell1 Side Bounds Neighbor deriving (Show, Eq, Ord)
 -- st bs nt
data Shell2 = Shell2 Side Bounds Regs Sides Neighbor deriving (Show, Eq, Ord)
 -- st bs rs ss nt
data Cage1 = Cage1 Sides Shell deriving (Show, Eq, Ord)
 -- ss bt
data Cage2 = Cage2 Regs Sides Shell deriving (Show, Eq, Ord)
 -- rs ss bt
data Disk1 = Disk1 Neighbor Shell deriving (Show, Eq, Ord)
 -- nt bt
data Disk2 = Disk2 Regs Sides Neighbor Shell deriving (Show, Eq, Ord)
 -- rs ss nt bt
data Blot1 = Blot1 Sides Disk deriving (Show, Eq, Ord)
 -- ss rt
data Blot2 = Blot2 Regs Sides Disk deriving (Show, Eq, Ord)
 -- rs ss rt
data Vert1 = Vert1 Dual Duali Half Regs Sides Flat Cage deriving (Show, Eq, Ord)
 -- dt di ht rs ss am bm
data Verti1 = Verti1 Vert deriving (Show, Eq, Ord)
 -- vm
data Verts1 = Verts1 Vert deriving (Show, Eq, Ord)
 -- vm
data Verts2 = Verts2 Verti deriving (Show, Eq, Ord)
 -- vi
data Pencil1 = Pencil1 Dual Duali Sides Flat Verti Verts deriving (Show, Eq, Ord)
 -- dt di ss am vi vs
data Pencil2 = Pencil2 Dual Duali Sides Flat deriving (Show, Eq, Ord)
 -- dt di ss am
data Corner1 = Corner1 Regs Cage Vert deriving (Show, Eq, Ord)
 -- rs bm vm
data Inside1 = Inside1 Dual Duali Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt di bs rs ss
data Outside1 = Outside1 Regs Inside deriving (Show, Eq, Ord)
 -- rs is
data Signs1 = Signs1 Dual Duali Half Sides Flat Verti Verts Tope deriving (Show, Eq, Ord)
 -- dt di ht ss am vi vs tm
data Signb1 = Signb1 Neighbor Verti Pencil Tope deriving (Show, Eq, Ord)
 -- nt vi pm tm
data Signb2 = Signb2 Neighbor Verti Pencil Signs Tope deriving (Show, Eq, Ord)
 -- nt vi pm gs tm
data Signr1 = Signr1 Side Sides Pencil Signb Topei Topes deriving (Show, Eq, Ord)
 -- st ss pm gb ti ts
data Signr2 = Signr2 Side Sides Pencil Signs Signb Topei Topes deriving (Show, Eq, Ord)
 -- st ss pm gs gb ti ts
data Tope1 = Tope1 Regs deriving (Show, Eq, Ord)
 -- rs

-- parameters for deducers and constructors
data Take0 = Take0 Dual Bounds Sides deriving (Show, Eq, Ord)
 -- dt bs ss
data Take1 = Take1 Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt bs rs ss
data Polycil0 = Polycil0 Dual Duali Half Sides Flat deriving (Show, Eq, Ord)
 -- dt di ht ss am
data Subspace0 = Subspace0 Dual Sides deriving (Show, Eq, Ord)
 -- dt ss
data Surspace0 = Surspace0 Side Bounds Regs deriving (Show, Eq, Ord)
 -- st bs rs
data Surspace1 = Surspace1 Side Bounds deriving (Show, Eq, Ord)
 -- st bs
data Section0 = Section0 Side Bounds Sides Attached deriving (Show, Eq, Ord)
 -- st bs ss at
data Subsection0 = Subsection0 Side Dual Bounds Sides Attached deriving (Show, Eq, Ord)
 -- st dt bs ss at
data Subsection1 = Subsection1 Dual Sides deriving (Show, Eq, Ord)
 -- dt ss
data Supersection0 = Supersection0 Side Dual Bounds Regs Sides Blot deriving (Show, Eq, Ord)
 -- st dt bs rs ss rm
data Supersection1 = Supersection1 Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt bs rs ss
data Supersection2 = Supersection2 Side Dual Bounds Regs Sides Attached Blot deriving (Show, Eq, Ord)
 -- st dt bs rs ss at rm
data Supersection3 = Supersection3 Side Dual Bounds Regs Sides Blot Boundary deriving (Show, Eq, Ord)
 -- st dt bs rs ss rm b
data Supersection4 = Supersection4 Dual Sides Boundary deriving (Show, Eq, Ord)
 -- dt ss b
data Superspace0 = Superspace0 Dual Bounds Sides deriving (Show, Eq, Ord)
 -- dt bs ss
data Migrate0 = Migrate0 Side Bounds Regs Sides Cage deriving (Show, Eq, Ord)
 -- st bs rs ss bm
data Cospace0 = Cospace0 Side Dual Bounds Regs Sides Attached Blot Verti Corner deriving (Show, Eq, Ord)
 -- st dt bs rs ss at rm vi qm
data Cospace1 = Cospace1 Side Bounds Regs Corner deriving (Show, Eq, Ord)
 -- st bs rs qm
data Cospace2 = Cospace2 Dual Sides deriving (Show, Eq, Ord)
 -- dt ss
data Cospace3 = Cospace3 Side Dual Bounds Regs Sides Cage deriving (Show, Eq, Ord)
 -- st dt bs rs ss bm
data Cospace4 = Cospace4 Dual Bounds Regs Sides Cage deriving (Show, Eq, Ord)
 -- dt bs rs ss bm
data Cospace5 = Cospace5 Dual Bounds Sides Inside deriving (Show, Eq, Ord)
 -- dt bs ss is
data Cospace6 = Cospace6 Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt bs rs ss
data Spaces0 = Spaces0 Side Dual Bounds Regs Sides Attached Blot Verti Corner deriving (Show, Eq, Ord)
 -- st dt bs rs ss at rm vi qm
data Spaces1 = Spaces1 Dual Regs Sides Verts deriving (Show, Eq, Ord)
 -- dt rs ss vs
data Polytope0 = Polytope0 Signr deriving (Show, Eq, Ord)
 -- gr
data Polytope1 = Polytope1 Dual Bounds Sides Topes Topez deriving (Show, Eq, Ord)
 -- dt bs ss ts tz
data Polytope2 = Polytope2 Side Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- st dt bs rs ss
data Polytope3 = Polytope3 Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt bs rs ss
data Polytope4 = Polytope4 Side Dual Bounds Sides Attached Blot deriving (Show, Eq, Ord)
 -- st dt bs ss at rm
data Overlaps0 = Overlaps0 Side Dual Duali Half Bounds Regs Sides Neighbor Flat Verti Verts Pencil
 deriving (Show, Eq, Ord) -- st dt di ht bs rs ss nt am vi vs pm
data Overlaps1 = Overlaps1 Dual Bounds Sides Inside deriving (Show, Eq, Ord)
 -- dt bs ss is
data Sample0 = Sample0 Side Sides Vert Pencil Basis deriving (Show, Eq, Ord)
 -- st ss vm pm ci
data Sample1 = Sample1 Bounds Vert Verti Verts Plane Coplane deriving (Show, Eq, Ord)
 -- bs vm vi vs cb cv
data Sample2 = Sample2 Duali Cage deriving (Show, Eq, Ord)
 -- di bm

-- inducers
side1_s :: Side1 -> Boundary -> Region -> Sidedness
side1_s (Side1 dt ss) b r = setFind f ss where
 f s = member (sub2 dt s r) b
side2_s :: Side2 -> Boundary -> Region -> Sidedness
side2_s (Side2 ht ss) b r = setFind f ss where
 f s = member (sub2 ht s b) r
dual1_bs :: Dual1 -> Sidedness -> Region -> Boundaries
dual1_bs (Dual1 st bs ss) s r = setFilter f bs where
 f b = (sub2 st b r) == s
half1_rs :: Half1 -> Sidedness -> Boundary -> Regions
half1_rs (Half1 st rs ss) s b = setFilter f rs where
 f r = (sub2 st b r) == s
neighbor1_r :: Neighbor1 -> Boundary -> Region -> Maybe Region
neighbor1_r (Neighbor1 dt di ss) b r = maybeSub2 di s (insertRemove (sub2 dt s r) b) where
 s = choose ss
attached1_rs :: Attached1 -> Sidedness -> Boundary -> Regions
attached1_rs (Attached1 st rs nt) s b = setFilter f rs where
 f r = (maybeSub2 nt b r) /= Nothing && (sub2 st b r) == s
flat1_rs :: Flat1 -> Boundary -> Regions
flat1_rs (Flat1 ss at) b = unions (setMap f ss) where
 f s = sub2 at s b
shell1_bs :: Shell1 -> Sidedness -> Region -> Boundaries
shell1_bs (Shell1 st bs nt) s r = setFilter f bs where
 f b = (maybeSub2 nt b r) /= Nothing && (sub2 st b r) == s
cage1_bs :: Cage1 -> Region -> Boundaries
cage1_bs (Cage1 ss bt) r = unions (setMap f ss) where
 f s = sub2 bt s r
disk1_rs :: Disk1 -> Sidedness -> Region -> Regions
disk1_rs (Disk1 nt bt) s r = setMap f (sub2 bt s r) where
 f b = sub2 nt b r
blot1_rs :: Blot1 -> Region -> Regions
blot1_rs (Blot1 ss rt) r = unions (setMap f ss) where
 f s = sub2 rt s r
signs1_gs :: Signs1 -> Vertices
signs1_gs (Signs1 dt di ht ss am vi vs tm) = setFilter f vs where
 f v = let bs = sub vi v in setAll g (setSets bs (single ((setSize bs)-1)))
 g bs = setAny h (setMaps bs ss)
 h m = let p = polycil p0 m; q = (image tm p) in (setSize q) == 1
 p0 = Polycil0 dt di ht ss am
signb1_gb :: Signb1 -> Vertex -> Boundaries
signb1_gb (Signb1 nt vi pm tm) v = setFilter f (sub vi v) where
 p = sub pm v
 f b = setAny (g b) p
 g b r = h r (maybeSub2 nt b r)
 h r0 Nothing = False
 h r0 (Just r1) = (sub tm r0) /= (sub tm r1)
signr1_gr :: Signr1 -> Vertex -> Rainbow
signr1_gr (Signr1 st ss pm gb ti ts) v = fromSet f ts where
 bs = sub gb v -- significant boundaries
 rs = sub pm v -- pencil regions
 ms = setMaps bs ss -- significant directions
 -- significant directions with any pencil region of given color
 f c = setFilter (g c) ms
 -- whether any pencil region in given significant direction is of given color
 g c m = setAny (member (sub ti c)) (setFilter (h m) rs)
 -- whether given pencil region is in given significant direction
 h m r = setAll (i m r) bs
 -- whether given sidedness is as indicated by given significant direction
 i m r b = (sub m b) == (sub2 st b r)

-- generators
side3_side0 (Side3 dt bs rs ss) = Side0 (fromSet2 (side1_s st1) bs rs) where
 st1 = Side1 dt ss
side4_side0 (Side4 ht bs rs ss) = Side0 (fromSet2 (side2_s st2) bs rs) where
 st2 = Side2 ht ss
dual2_dual0 (Dual2 st bs rs ss) = Dual0 (fromSet2 (dual1_bs dt1) ss rs) where
 dt1 = Dual1 st bs ss
dual3_dual0 (Dual3 di ss) = Dual0 (fromSet f ss) where
 f s = inverse (sub di s)
duali1_duali0 (Duali1 dt ss) = Duali0 (fromSet f ss) where
 f s = inverse (sub dt s)
duals1_duals0 (Duals1 st bs rs) = Duals0 (setMap f rs) where
 f r = fromSet (g r) bs; g r b = sub2 st b r
half2_half0 (Half2 st bs rs ss) = Half0 (fromSet2 (half1_rs ht1) ss bs) where
 ht1 = Half1 st rs ss
half3_half0 (Half3 hi ss) = Half0 (fromSet f ss) where
 f s = inverse (sub hi s)
halfi1_halfi0 (Halfi1 ht ss) = Halfi0 (fromSet f ss) where
 f s = inverse (sub ht s)
side0_bounds0 (Side0 st) = Bounds0 (keysSet st) where
 -- nothing
dual0_bounds0 (Dual0 dt) = Bounds0 (unions (valsSet2 dt)) where
 -- nothing
duali0_bounds0 (Duali0 di) = Bounds0 (unions (keysSet2 di)) where
 -- nothing
half0_bounds0 (Half0 ht) = Bounds0 (keysSet2 ht) where
 -- nothing
halfi0_bounds0 (Halfi0 hi) = Bounds0 (valsSet2 hi) where
 -- nothing
side0_regs0 (Side0 st) = Regs0 (keysSet2 st) where
 -- nothing
dual0_regs0 (Dual0 dt) = Regs0 (keysSet2 dt) where
 -- nothing
duali0_regs0 (Duali0 di) = Regs0 (valsSet2 di) where
 -- nothing
half0_regs0 (Half0 ht) = Regs0 (unions (valsSet2 ht)) where
 -- nothing
halfi0_regs0 (Halfi0 hi) = Regs0 (unions (keysSet2 hi)) where
 -- nothing
side0_sides0 (Side0 st) = Sides0 (valsSet2 st) where
 -- nothing
dual0_sides0 (Dual0 dt) = Sides0 (keysSet dt) where
 -- nothing
duali0_sides0 (Duali0 di) = Sides0 (keysSet di) where
 -- nothing
half0_sides0 (Half0 ht) = Sides0 (keysSet ht) where
 -- nothing
halfi0_sides0 (Halfi0 hi) = Sides0 (keysSet hi) where
 -- nothing
neighbor2_neighbor0 (Neighbor2 dt di bs rs ss) = Neighbor0 (fromOptSet2 (neighbor1_r nt1) bs rs) where
 nt1 = Neighbor1 dt di ss
attached2_attached0 (Attached2 st bs rs ss nt) = Attached0 (fromSet2 (attached1_rs at1) ss bs) where
 at1 = Attached1 st rs nt
flat2_flat0 (Flat2 bs ss at) = Flat0 (fromSet (flat1_rs am1) bs) where
 am1 = Flat1 ss at
shell2_shell0 (Shell2 st bs rs ss nt) = Shell0 (fromSet2 (shell1_bs bt1) ss rs) where
 bt1 = Shell1 st bs nt
cage2_cage0 (Cage2 rs ss bt) = Cage0 (fromSet (cage1_bs bm1) rs) where
 bm1 = Cage1 ss bt
disk2_disk0 (Disk2 rs ss nt bt) = Disk0 (fromSet2 (disk1_rs rt1) ss rs) where
 rt1 = Disk1 nt bt
blot2_blot0 (Blot2 rs ss rt) = Blot0 (fromSet (blot1_rs rm1) rs) where
 rm1 = Blot1 ss rt
vert1_vert0 (Vert1 dt di ht rs ss am bm) = Vert0 (valsMap Vertex (fromKeys s)) where
 s = unions (setMap f (setMap (sub bm) rs))
 f s = setFilter g (setSets s (setFromList [1..((setSize s)-1)]))
 g bs = setAll h (setMaps bs ss)
 f4 = Polycil0 dt di ht ss am
 h m = (setSize (polycil f4 m)) == 1
verti1_verti0 (Verti1 vm) = Verti0 (inverse vm) where
 -- nothing
verts1_verts0 (Verts1 vm) = Verts0 (valsSet vm) where
 -- nothing
verts2_verts0 (Verts2 vi) = Verts0 (keysSet vi) where
 -- nothing
pencil1_pencil0 (Pencil1 dt di ss am vi vs) = Pencil0 (fromSet f vs) where
 f2 = Pencil2 dt di ss am; f v = pencil f2 (sub vi v)
corner1_corner0 (Corner1 rs bm vm) = Corner0 (fromSet f rs) where
 f r = let bs = sub bm r in setOptMap (maybeSub vm) (setSets bs (setFromList [1..((setSize bs)-1)]))
inside1_inside0 (Inside1 dt di bs rs ss) = Inside0 (setFilter f rs) where
 n1 = (Neighbor1 dt di ss); f r = (colleague n1 bs r) == Nothing
outside1_outside0 (Outside1 rs is) = Outside0 (differ rs is) where
 -- nothing
signs1_signs0 (Signs1 dt di ht ss am vi vs tm) = Signs0 (signs1_gs s) where
 s = Signs1 dt di ht ss am vi vs tm
signb2_signb0 (Signb2 nt vi pm gs tm) = Signb0 (fromSet (signb1_gb s) gs) where
 s = Signb1 nt vi pm tm
signr2_signr0 (Signr2 st ss pm gs gb ti ts) = Signr0 (fromSet (signr1_gr s) gs) where
 s = Signr1 st ss pm gb ti ts
tope1_tope0 (Tope1 rs) = Tope0 (fromSet f rs) where
 f r = zero
tope0_topei0 (Tope0 tm) = Topei0 (adverse tm) where
 -- nothing
tope0_topes0 (Tope0 tm) = Topes0 (valsSet tm) where
 -- nothing
topez0_topes0 (Topez0 tz) = Topes0 (unions (setMap keysSet tz)) where
 -- nothing

-- selectors
section0_side0 (Section0 st bs ss at) = Side0 st
subsection0_subspace0 (Subsection0 st dt bs ss at) = Subspace0 dt ss
subsection0_section0 (Subsection0 st dt bs ss at) = Section0 st bs ss at
subsection0_bs (Subsection0 st dt bs ss at) = bs
subsection0_side0 (Subsection0 st dt bs ss at) = Side0 st
subsection1_subspace0 (Subsection1 dt ss) = Subspace0 dt ss
supersection2_subsection0 (Supersection2 st dt bs rs ss at rm) = Subsection0 st dt bs ss at
supersection2_supersection0 (Supersection2 st dt bs rs ss at rm) = Supersection0 st dt bs rs ss rm
supersection3_subsection1 (Supersection3 st dt bs rs ss rm b) = Subsection1 dt ss
supersection3_supersection0 (Supersection3 st dt bs rs ss rm b) = Supersection0 st dt bs rs ss rm
supersection3_supersection1b (Supersection3 st dt bs rs ss rm b) = (Supersection1 dt bs rs ss, b)
supersection4_subsection1b (Supersection4 dt ss b) = (Subsection1 dt ss, b)
migrate0_side0 (Migrate0 st bs rs ss bm) = Side0 st
spaces0_side0 (Spaces0 st dt bs rs ss at rm vi qm) = Side0 st
spaces0_bs (Spaces0 st dt bs rs ss at rm vi qm) = bs
spaces0_cospace0 (Spaces0 st dt bs rs ss at rm vi qm) = Cospace0 st dt bs rs ss at rm vi qm
spaces0_supersection0 (Spaces0 st dt bs rs ss at rm vi qm) = Supersection0 st dt bs rs ss rm
spaces0_cospace1 (Spaces0 st dt bs rs ss at rm vi qm) = Cospace1 st bs rs qm
spaces1_cospace2 (Spaces1 dt rs ss vs) = Cospace2 dt ss
spaces1_vs (Spaces1 dt rs ss vs) = vs
spaces1_rs (Spaces1 dt rs ss vs) = rs
polytope0_signr0 (Polytope0 gr) = Signr0 gr
polytope1_sides0 (Polytope1 dt bs ss ts tz) = Sides0 ss
polytope1_superspace0 (Polytope1 dt bs ss ts tz) = Superspace0 dt bs ss
polytope1_ts (Polytope1 dt bs ss ts tz) = ts
polytope1_tz (Polytope1 dt bs ss ts tz) = tz
polytope2_st (Polytope2 st dt bs rs ss) = st
polytope2_rs (Polytope2 st dt bs rs ss) = rs
polytope2_take1 (Polytope2 st dt bs rs ss) = Take1 dt bs rs ss
polytope3_take1 (Polytope3 dt bs rs ss) = Take1 dt bs rs ss
polytope3_rs (Polytope3 dt bs rs ss) = rs
polytope4_section0 (Polytope4 st dt bs ss at rm) = Section0 st bs ss at
polytope4_take0 (Polytope4 st dt bs ss at rm) = Take0 dt bs ss
polytope4_at (Polytope4 st dt bs ss at rm) = at
polytope4_rm (Polytope4 st dt bs ss at rm) = rm
overlaps0_subspace0 (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) = Subspace0 dt ss
overlaps0_take1 (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) = Take1 dt bs rs ss
overlaps0tm_signs1 (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) tm = Signs1 dt di ht ss am vi vs tm
overlaps0gt_signb2 (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) gs tm = Signb2 nt vi pm gs tm
overlaps0gt_signr2 (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) gs gb ti ts = Signr2 st ss pm gs gb ti ts
overlaps0_bs (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) = bs
overlaps0_rs (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) = rs
overlaps1_take0 (Overlaps1 dt bs ss is) = Take0 dt bs ss
overlaps1_is (Overlaps1 dt bs ss is) = is

-- converters
duali0_subsection0 (Duali0 di) = Subsection0 st dt bs ss at where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
duali0_subsection1 (Duali0 di) = Subsection1 dt ss where
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
duali0_polytope2 (Duali0 di) = Polytope2 st dt bs rs ss where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
duali0_overlaps1 (Duali0 di) = Overlaps1 dt bs ss is where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Inside0 is) = inside1_inside0 (Inside1 dt di bs rs ss)
side0_overlaps0 (Side0 st) = Overlaps0 st dt di ht bs rs ss nt am vi vs pm where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Half0 ht) = half2_half0 (Half2 st bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
 (Flat0 am) = flat2_flat0 (Flat2 bs ss at)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
 (Vert0 vm) = vert1_vert0 (Vert1 dt di ht rs ss am bm)
 (Verti0 vi) = verti1_verti0 (Verti1 vm)
 (Verts0 vs) = verts1_verts0 (Verts1 vm)
 (Pencil0 pm) = pencil1_pencil0 (Pencil1 dt di ss am vi vs)
side0_subsection1 (Side0 st) = Subsection1 dt ss where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
side0_supersection0 (Side0 st) = Supersection0 st dt bs rs ss rm where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
side0_supersection1 (Side0 st) = Supersection1 dt bs rs ss where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
side0_migrate0 (Side0 st) = Migrate0 st bs rs ss bm where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
side0_cospace3 (Side0 st) = (Cospace3 st dt bs rs ss bm) where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
side0_cospace6 (Side0 st) = (Cospace6 dt bs rs ss) where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
side0_duals1 (Side0 st) = (Duals1 st bs rs) where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
side0_spaces0 (Side0 st) = Spaces0 st dt bs rs ss at rm vi qm where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Half0 ht) = half2_half0 (Half2 st bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
 (Flat0 am) = flat2_flat0 (Flat2 bs ss at)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
 (Vert0 vm) = vert1_vert0 (Vert1 dt di ht rs ss am bm)
 (Verti0 vi) = verti1_verti0 (Verti1 vm)
 (Corner0 qm) = corner1_corner0 (Corner1 rs bm vm)
 (Inside0 is) = inside1_inside0 (Inside1 dt di bs rs ss)
 (Outside0 os) = outside1_outside0 (Outside1 rs is)
side0tz_polytope1 (Side0 st) tz = Polytope1 dt bs ss ts tz where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Topes0 ts) = topez0_topes0 (Topez0 tz)
dual0_polytope4 (Dual0 dt) = Polytope4 st dt bs ss at rm where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
dual0_supersection0 (Dual0 dt) = Supersection0 st dt bs rs ss rm where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
dual0_supersection1 (Dual0 dt) = Supersection1 dt bs rs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
dual0_spaces1 (Dual0 dt) = Spaces1 dt rs ss vs where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Half0 ht) = half2_half0 (Half2 st bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
 (Flat0 am) = flat2_flat0 (Flat2 bs ss at)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
 (Vert0 vm) = vert1_vert0 (Vert1 dt di ht rs ss am bm)
 (Verts0 vs) = verts1_verts0 (Verts1 vm)
dual0_polytope3 (Dual0 dt) = Polytope3 dt bs rs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
side0_section0 (Side0 st) = Section0 st bs ss at where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
side0b_supersection3 (Side0 st, b) = Supersection3 st dt bs rs ss rm b where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
side0b_supersection4 (Side0 st, b) = Supersection4 dt ss b where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
side0_supersection2 (Side0 st) = Supersection2 st dt bs rs ss at rm where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
duali0_section0 (Duali0 di) = Section0 st bs ss at where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
duali0_supersection2 (Duali0 di) = Supersection2 st dt bs rs ss at rm where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss) 
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
duali0_supersection3 (Duali0 di,b) = Supersection3 st dt bs rs ss rm b where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss) 
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
duali0_supersection4 (Duali0 di,b) = Supersection4 dt ss b where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
duali0_cospace5 (Duali0 di) = (Cospace5 dt bs ss is) where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Inside0 is) = inside1_inside0 (Inside1 dt di bs rs ss)
side0_superspace0 (Side0 st) = Superspace0 dt bs ss where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
dual0_superspace0 (Dual0 dt) = Superspace0 dt bs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
dual0_surspace0 (Dual0 dt) = Surspace0 st bs rs where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
dual0_take1 (Dual0 dt) = Take1 dt bs rs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)

-- unconverter, for rare case only
subsection1_side0 (Subsection1 dt ss) = Side0 st where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)

-- deducers
polyant :: Half0 -> Direction -> Regions
polyant (Half0 ht) b2s = intersects (setMap f (keysSet b2s)) where
 f b = sub2 ht (sub b2s b) b
colleague :: Neighbor1 -> Boundaries -> Region -> Maybe Region
colleague (Neighbor1 dt di ss) bs r = maybeSub2 di s (symmetric bs (sub2 dt s r)) where
 s = choose ss
pencil :: Pencil2 -> Boundaries -> Regions
pencil (Pencil2 dt di ss am) bs = setFilter f (intersects (setMap (sub am) bs)) where
 n1 = Neighbor1 dt di ss
 f r = (colleague n1 bs r) /= Nothing
polycil :: Polycil0 -> Direction -> Regions
polycil (Polycil0 dt di ht ss am) b2s = intersect rs0 rs1 where
 rs0 = (polyant (Half0 ht) b2s)
 rs1 = (pencil (Pencil2 dt di ss am) (keysSet b2s))
connect :: Blot0 -> Regions -> Maybe Region -> Regions
connect (Blot0 rm) rs Nothing = setEmpty
connect (Blot0 rm) rs (Just r) = setConnect f r where
 f r = intersect (sub rm r) rs
retake :: Take0 -> Take1 -> Regions -> Regions -- take Regions from Take0 to Take1
retake (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) rs0 = unions (setMap f rs0) where
 s = choose (intersect ss0 ss1)
 bs = intersect bs0 bs1
 f r0 = setFilter (g r0) rs1
 g r0 r1 = (intersect bs (sub2 dt0 s r0)) == (intersect bs (sub2 dt1 s r1))

-- nontrivial deducers
-- to find typographic minimum, call equispace on space with itself, and find minimum after rename
equispace :: Duals0 -> Duals0 -> Symmetries
equispace (Duals0 ds0) (Duals0 ds1) = f bs0 bs1 mapEmpty setEmpty setEmpty where
 bs0 = unions (setMap keysSet ds0)
 ss0 = unions (setMap valsSet ds0)
 bs1 = unions (setMap keysSet ds1)
 ss1 = unions (setMap valsSet ds1)
 sml = setToList (setMaps ss0 ss1)
 f :: Boundaries -> Boundaries -> Map Boundary (Boundary, Map Sidedness Sidedness) ->
  Boundaries -> Boundaries -> Set (Map Boundary (Boundary, Map Sidedness Sidedness))
 f bs0 bs1 m ks vs
  | ds2 /= ds4 = setEmpty
  | (setSize bs1) == 0 = single m
  | otherwise = listFold1 union [g b0 b1 sm | b0 <- bl0, b1 <- bl1, sm <- sml] where
  bl0 = setToList bs0
  bl1 = setToList bs1
  ds2 = setMap (h vs) ds0
  ds3 = setMap (h ks) ds1
  ds4 = setMap (mapMap i) ds3
  g :: Boundary -> Boundary -> Map Sidedness Sidedness -> Set (Map Boundary (Boundary, Map Sidedness Sidedness))
  g b0 b1 sm = f (remove bs0 b0) (remove bs1 b1) (extend m (b1,(b0,sm))) (insert ks b1) (insert vs b0)
  h :: Boundaries -> Map Boundary Sidedness -> Map Boundary Sidedness
  h bs sm = restrict sm bs
  i :: (Boundary,Sidedness) -> (Boundary,Sidedness)
  i (b0,s0) = let (b1,sm) = sub m b0 in (b1, sub sm s0)
-- return whether polytopes are equivalent
-- to find typographic minimum, call equitope on polytope with itself, and find minimum after rename
equitope :: Topez0 -> Topez0 -> Symmetries
equitope (Topez0 p0) (Topez0 p1) = f bs0 bs1 mapEmpty setEmpty setEmpty where
 bs0 = unions (setMap keysSet (unions (unions (setMap valsSet p0))))
 ss0 = unions (setMap valsSet (unions (unions (setMap valsSet p0))))
 bs1 = unions (setMap keysSet (unions (unions (setMap valsSet p1))))
 ss1 = unions (setMap valsSet (unions (unions (setMap valsSet p1))))
 sml = setToList (setMaps ss0 ss1)
 f :: Boundaries -> Boundaries -> Map Boundary (Boundary, Map Sidedness Sidedness) ->
  Boundaries -> Boundaries -> Set (Map Boundary (Boundary, Map Sidedness Sidedness))
 f bs0 bs1 m ks vs
  | p3 /= p6 = setEmpty
  | (setSize bs1) == 0 = single m
  | otherwise = listFold1 union [g b0 b1 sm | b0 <- bl0, b1 <- bl1, sm <- sml] where
  bl0 = setToList bs0
  bl1 = setToList bs1
  p2 :: Set (Map Color (Set (Map Boundary Sidedness)))
  p2 = setMap (valsMap (setMap (h vs))) p0
  p3 = setMap (valsMap (setFilter mapNonempty)) p2
  p4 = setMap (valsMap (setMap (h ks))) p1
  p5 = setMap (valsMap (setFilter mapNonempty)) p4
  p6 = setMap (valsMap (setMap (mapMap i))) p5
  g :: Boundary -> Boundary -> Map Sidedness Sidedness -> Set (Map Boundary (Boundary, Map Sidedness Sidedness))
  g b0 b1 sm = f (remove bs0 b0) (remove bs1 b1) (extend m (b1,(b0,sm))) (insert ks b1) (insert vs b0)
  h bs sm = restrict sm bs
  i (b0,s0) = let (b1,sm) = sub m b0 in (b1, sub sm s0)

-- nontrivial converters
-- polytope classifies regions as polytope
polytope :: Polytope0 -> Topez0
polytope s = Topez0 (valsSet gr) where
 (Signr0 gr) = polytope0_signr0 s
-- embed returns space and embedding for polytope
-- the Polytope1 argument is for recursion
-- at top level, pass in empty space for Polytope1
embed :: Polytope1 -> (Dual0,Topei0)
embed s = if (setSize tz) == 0 then
 (empty1 (polytope1_sides0 s), Topei0 mapEmpty) else
 (s5, Topei0 (fromSet f0 ts)) where
 ts :: Colors
 ts = polytope1_ts s
 tz :: Set (Map Color (Set (Map Boundary Sidedness)))
 tz = polytope1_tz s
 f0 :: Color -> Regions
 f0 c = setConnect2 (f (sub exclude c)) (sub include c)
 f :: Regions -> Region -> Regions
 f rs r = differ (sub rm r) rs
 s1 = setMap g tz
 g :: Map Color (Set (Map Boundary Sidedness)) -> Dual0
 g a = let
  ms = unions (valsSet a)
  ss = unions (setMap valsSet ms)
  rs = setFromList [zero..(Region ((setSize ms)-1))] in
  vertex rs ss ms
 -- surspace of indicated regions from vertex pencil
 vertex :: Regions -> Sides -> Directions -> Dual0
 vertex rs ss ds = Dual0 (fromSet2 f ss rs) where
  t = fromKeysVals rs ds
  f s r = let m = (sub t r) in setFilter (g m s) (keysSet m)
  g m s b = (sub m b) == s
 s2 = setMap dual0_superspace0 s1
 s3 = polytope1_superspace0 s
 s4 = forceInsert s2 s3
 s5 = superspace2 s4
 s6 = dual0_polytope4 s5
 ms :: Set (Map Boundary Sidedness)
 ms = unions (unions (setMap valsSet tz))
 bs = unions (setMap keysSet ms)
 ss = unions (setMap valsSet ms)
 rm = polytope4_rm s6
 a0 = polytope4_section0 s6
 a1 = polytope4_take0 s6
 a2 = polytope4_at s6
 (include,exclude) = (setFold2 extends incl mapEmpty, setFold2 extends excl mapEmpty)
 (incl,excl) = setUnzip (setMap2 h bs ss)
 h :: Boundary -> Sidedness -> (Map Color Regions, Map Color Regions)
 h b s = let
  a3 = section1 a0 b
  a4 = side0tz_polytope1 a3 (i b s)
  (dt, Topei0 ti) = embed a4 -- embedding in side of boundary
  a5 = dual0_polytope3 dt
  a6 = polytope3_take1 a5
  a7 = polytope3_rs a5
  incl = valsMap (retake a1 a6) ti
  excl = valsMap ((retake a1 a6) . (differ a7)) ti
  ude = sub2 a2 s b in
  (valsMap (intersect ude) incl , valsMap (intersect ude) excl)
 i :: Boundary -> Sidedness -> Set (Map Color (Set (Map Boundary Sidedness)))
 i b s = setMap (j b s) tz
 j :: Boundary -> Sidedness -> Map Color (Set (Map Boundary Sidedness)) ->
  Map Color (Set (Map Boundary Sidedness))
 j b s m = valsMap (k b s) m
 k :: Boundary -> Sidedness -> Set (Map Boundary Sidedness) -> Set (Map Boundary Sidedness)
 k b s m = setFilter k0 (setMap (k1 b s) (setFilter (k2 b s) m))
 k0 :: Map Boundary Sidedness -> Bool
 k0 m = (mapSize m) /= 0
 k1 :: Boundary -> Sidedness -> Map Boundary Sidedness -> Map Boundary Sidedness
 k1 b s m = restrict m (remove (keysSet m) b)
 k2 :: Boundary -> Sidedness -> Map Boundary Sidedness -> Bool
 k2 b s m = (maybeSub m b) == (Just s)

-- trivial constructors
base1 :: Int -> Sides0
base1 n = Sides0 (setFromList [zero..(Sidedness (n-1))])
empty1 :: Sides0 -> Dual0
empty1 (Sides0 ss) = Dual0 (fromSet2 f ss rs) where
 f s r = setEmpty
 rs = single zero
order1 :: Sides0 -> Int -> Side0
order1 (Sides0 ss) bn = Side0 (fromSet2 f bs rs) where
 (inside,outside) = choose2 ss
 bs = setFromList [zero..(Boundary (bn-1))]
 rs = setFromList [zero..(Region bn)]
 f (Boundary b) (Region r) = if r <= b then inside else outside
system1 :: Sides0 -> Int -> Dual0
system1 (Sides0 ss) bn = system2 (Sides0 ss) (setFromList [zero..(Boundary (bn-1))])
system2 :: Sides0 -> Boundaries -> Dual0
system2 ss bs = superspace2 (setMap f bs) where
 dual0 = empty1 ss
 s0 = dual0_supersection0 dual0
 s1 = dual0_supersection1 dual0
 f b = side0_superspace0 (supersection1 s0 s1 b)
simplex1 :: Sides0 -> Int -> Side0
simplex1 sides0 bn = surspace1 (Surspace0 st bs rs) (choose rs) where
 Surspace0 st bs rs = dual0_surspace0 (system1 sides0 bn)

-- constructors
-- subspace regions are same wrt remaining boundaries
subspace1 :: Subspace0 -> Boundary -> Duali0
subspace1 (Subspace0 dt ss) b = Duali0 (fromSet f ss) where
 f s = mapMap g (sub dt s)
 g (k,v) = (remove v b,k)
subspace2 :: Subspace0 -> Boundaries -> Duali0
subspace2 (Subspace0 dt ss) bs = Duali0 (fromSet f ss) where
 f s = mapMap g (sub dt s)
 g (k,v) = (intersect bs v,k)
-- surspace is section of given regions
surspace1 :: Surspace0 -> Region -> Side0
surspace1 (Surspace0 st bs rs) r = Side0 (fromSet2 f bs rs0) where
 f b r = sub2 st b r
 rs0 = remove rs r
surspace2 :: Surspace1 -> Regions -> Side0
surspace2 (Surspace1 st bs) rs = Side0 (fromSet2 f bs rs) where
 f b r = sub2 st b r
-- section regions are isomorhpic to regions attached on some side
-- fails if boundary not in space
section1 :: Section0 -> Boundary -> Side0
section1 (Section0 st bs ss at) b = Side0 (fromSet f (remove bs b)) where
 rs = sub2 at (choose ss) b
 f b = fromSet (sub2 st b) rs
section2 :: Section0 -> Boundaries -> Side0
section2 s bs = setFoldBackElse2 f side0_section0 section0_side0 bs s where
 f b s = section1 s b
-- choose boundary and find subspaces and section
-- recurse on subspaces to find subspace of result
-- recurse on section and subspace of result for section in result
-- add section to subspace of result for result
subsection1 :: Subsection0 -> Subsection1 -> Subsection1 -> Side0
subsection1 s0 s1 s2
 | b3 == Nothing = subsection0_side0 s0
 | otherwise = let
 (Just b) = b3
 s3 = duali0_subsection0 (subspace2 (subsection0_subspace0 s0) bs3)
 s4 = subspace2 (subsection1_subspace0 s1) bs3
 s5 = subspace2 (subsection1_subspace0 s2) bs3
 s6 = section1 (subsection0_section0 s0) b
 s7 = subsection1 s3 (duali0_subsection1 s4) (duali0_subsection1 s5)
 s8 = subsection1 s3 (side0_subsection1 s6) (side0_subsection1 s7) in
 supersection1 (side0_supersection0 s7) (side0_supersection1 s8) b where
 (b3,bs3) = maybeChooseRemove (subsection0_bs s0)
subsection2 :: Subsection0 -> Set Subsection1 -> Side0
subsection2 s0 s1 = setFoldBackElse1 f side0_subsection1 subsection1_side0 s2 s1 where
 f = subsection1 s0
 s2 = subsection0_side0 s0
-- create new regions isomorphic to section
-- find regions on one side of isomorphic regions
-- wrt new boundary new regions are opposite isomorphic
-- wrt new boundary regions on the one side are same
-- wrt old boundaries new regions are same as isomorphic
-- wrt old boundaries old regions are unchanged
supersection1 :: Supersection0 -> Supersection1 -> Boundary -> Side0
supersection1 (Supersection0 st0 dt0 bs0 rs0 ss0 rm0) (Supersection1 dt1 bs1 rs1 ss1) b0 =
 Side0 (fromSet2 f boundaries regions) where
 (inside,outside) = choose2 ss0
 figure0 = retake (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) rs1
 ground = removes rs0 figure0
 figure1 = holes rs0 (setSize figure0)
 ground0 = connect (Blot0 rm0) ground (maybeChoose ground)
 boundaries = insert bs0 b0
 regions = inserts rs0 figure1
 map = fromKeysVals figure1 figure0
 f b r
  | b == b0 && (member ground0 r || member figure0 r) = inside
  | b == b0 = outside
  | member figure1 r = sub2 st0 b (sub map r)
  | otherwise = sub2 st0 b r
-- supersection (universe for supersection, and universe for subsection)
--              (universe for supersection, and arg for subsection)
--          Set (arg for supersection, and arg for subsection)
supersection2 :: Supersection2 -> Supersection3 -> Set Supersection4 -> Side0
supersection2 s0 s1 s2
 | (setSize s2) == 0 = rslt
 | otherwise = let
 sub0 = supersection2_subsection0 s0
 sub1 = supersection3_subsection1 s1
 sup0 = supersection3_supersection0 s1
 (sup1,sup2) = chooseRemove (setMap f s2)
 f s2 = let
  (sub2,b2) = supersection4_subsection1b s2
  sup1 = side0_supersection1 (subsection1 sub0 sub1 sub2) in
  (supersection1 sup0 sup1 b0, b2)
 s0' = side0_supersection2 rslt
 s1' = side0b_supersection3 sup1
 s2' = setMap side0b_supersection4 sup2 in
 supersection2 s0' s1' s2' where
 sup0 = supersection2_supersection0 s0
 (sup1,b0) = (supersection3_supersection1b s1)
 rslt = supersection1 sup0 sup1 b0
-- see http://www.sidegeo.blogspot.com/ for proof that result is linear if args are linear
superspace1 :: Superspace0 -> Superspace0 -> Dual0
superspace1 (Superspace0 dt0 bs0 ss0) (Superspace0 dt1 bs1 ss1)
 | shared == bs0 = Dual0 dt1
 | shared == bs1 = Dual0 dt0
 | otherwise = let
 bound0 = choose (removes bs0 shared)
 bounds0 = insert shared bound0
 sub0 = subspace2 (Subspace0 dt0 ss0) bounds0
 sect0 = section1 (duali0_section0 sub0) bound0
 bound1 = choose (removes bs1 shared)
 bounds1 = insert shared bound1
 sub1 = subspace2 (Subspace0 dt1 ss1) bounds1
 sect1 = section1 (duali0_section0 sub1) bound1
 sub2 = subspace2 (Subspace0 dt0 ss0) shared
 -- sub2 = subspace2 (Subspace0 dt1 ss1) shared -- bz equal?
 arg0 = duali0_supersection2 sub2
 arg1 = side0b_supersection3 (sect0,bound0)
 arg2 = single (side0b_supersection4 (sect1,bound1))
 sect2 = supersection2 arg0 arg1 arg2
 space0 = superspace1 (Superspace0 dt0 bs0 ss0) (side0_superspace0 sect2) in
 superspace1 (Superspace0 dt1 bs1 ss1) (dual0_superspace0 space0) where
 shared = intersect bs0 bs1
superspace2 :: Set Superspace0 -> Dual0
superspace2 a = setFoldBackElse1 superspace1 dual0_superspace0 f g a where
 f (Superspace0 dt bs ss) = Dual0 dt
 g = Dual0 (fromSet2 h setEmpty setEmpty)
 h s b = setEmpty
-- replace region by same except reversed wrt cage boundaries
migrate1 :: Migrate0 -> Region -> Side0
migrate1 s0@(Migrate0 st0 bs0 rs0 ss0 bm0) r = Side0 (fromSet2 f bs0 rs0) where
 (inside,outside) = choose2 ss0
 m = mapFromList [(inside,outside),(outside,inside)]
 bs = sub bm0 r
 f b r = let s = sub2 st0 b r in if member bs b then sub m s else s
-- replace regions by same except reversed wrt cage boundaries
migrate2 :: Migrate0 -> Regions -> Side0
migrate2 s0 rs = setFoldBackElse2 f side0_migrate0 migrate0_side0 rs s0 where
 f r s0 = migrate1 s0 r

-- Find cospace by finding supersection of section by boundary with each section connected by migration.
-- Migrate a section by finding region blocks to migrate with.
-- Find region blocks by trying take from each region of each vertex space from sectioned space.
-- A set of regions is a block if it's cage union is just the vertex boundaries.
cospace1 :: Cospace0 -> Map Vertex Boundary -> Dual0
cospace1 s0@(Cospace0 st0 dt0 bs0 rs0 ss0 at0 rm0 vi0 qm0) m =
 Dual0 (fromSet2 f6 ss rs) where
 -- choose boundary, find section and parallel by it
 chosen = choose bs0; extra = hole bs0
 subspace = duali0_supersection2 (subspace1 (Subspace0 dt0 ss0) chosen)
 section = side0b_supersection3 ((section1 (Section0 st0 bs0 ss0 at0) chosen), chosen)
 Supersection3 st1 dt1 bs1 rs1 ss1 rm1 b1 = section
 parallel = single (Supersection4 dt1 ss1 extra)
 -- find supersection of section and parallel
 super = side0_section0 (supersection2 subspace section parallel)
 -- use section by parallel as start space for setConnect by migrations
 sections = setConnect f (section1 super extra)
 -- to find migrations, find migratable region sets
 -- f takes section in s0 to migrated sections
 f :: Side0 -> Set Side0
 f (Side0 st1) = let
  (Cospace3 st1 dt1 bs1 rs1 ss1 bm1) = side0_cospace3 (Side0 st1)
  rs = retake (Take0 dt1 bs1 ss1) (Take1 dt0 bs0 rs0 ss0) rs1
  vs = unions (setMap (sub qm0) rs)
  rz = setOptMap (g (Cospace4 dt1 bs1 rs1 ss1 bm1)) vs
  in setMap (migrate2 (Migrate0 st1 bs1 rs1 ss1 bm1)) rz
 -- a section-region-set is migratable if its block is the boundaries of a vertex
 -- a block of a set of regions is the union of their cages
 -- if take of inside of subspace in s1 is block in s1, return it
 -- Vertex from s0 to Regions in s1
 g :: Cospace4 -> Vertex -> Maybe Regions
 g (Cospace4 dt1 bs1 rs1 ss1 bm1) v = let
  -- find boundaries through vertex in s0
  bs = sub vi0 v
  -- find subspace in s1 by boundaries through vertex
  Cospace5 dt2 bs2 ss2 is2 = duali0_cospace5 (subspace2 (Subspace0 dt1 ss1) bs)
  -- take inside of subspace to s1 to potential result
  rslt = retake (Take0 dt2 bs2 ss2) (Take1 dt1 bs1 rs1 ss1) is2
  -- find union of cages of potential result
  cond = unions (setMap (sub bm1) rslt)
  -- if union is boundaries then just taken else nothing
  in if bs == cond then Just rslt else Nothing
 -- now find coregions for sections
 -- h returns coboundaries separated by section
 h :: Side0 -> Set Boundaries
 h (Side0 st1) = let
  -- take the regions in the given section to s0
  Cospace6 dt1 bs1 rs1 ss1 = side0_cospace6 (Side0 st1)
  figure = retake (Take0 dt1 bs1 ss1) (Take1 dt0 bs0 rs0 ss0) rs1
  -- find the complement of the regions in s0
  ground = differ rs0 figure
  -- find one connected region set
  above = connect (Blot0 rm0) ground (maybeChoose ground)
  -- find other connected region set
  below = differ ground above
  -- apply qm0 to each to get two vertex sets
  verts0 = unions (setMap (sub qm0) above)
  verts1 = unions (setMap (sub qm0) below)
  -- apply m to each for two boundary sets
  bounds0 = setOptMap (maybeSub m) verts0
  bounds1 = setOptMap (maybeSub m) verts1
  -- returns doubleton of the two sets
  in union (single bounds0) (single bounds1)
 bz = (setMap h sections)
 bs = unions (unions bz)
 ss = holes setEmpty (setSize (choose bz))
 (inside,outside) = choose2 ss
 only = union (single bs) (single setEmpty)
 internal = remove bz only
 cage = fromSet f1 internal
 f1 :: Set Boundaries -> Boundaries
 f1 a = setFilter (f2 a) bs
 f2 :: Set Boundaries -> Boundary -> Bool
 f2 a b = member bz (setMap (f3 b) a)
 f3 :: Boundary -> Boundaries -> Boundaries
 f3 b bs = insertRemove bs b
 center = choose internal -- does not work if s0 is vertex space
 connected = setConnect f4 (center,choose2 center)
 f4 :: (Set Boundaries,(Boundaries,Boundaries)) -> Set (Set Boundaries,(Boundaries,Boundaries))
 f4 (bz,(is,os)) = if member internal bz then setMap (f5 bz is os) (sub cage bz) else setEmpty
 f5 :: Set Boundaries -> Boundaries -> Boundaries -> Boundary -> (Set Boundaries,(Boundaries,Boundaries))
 f5 bz is os b = (setMap (f3 b) bz,(insertRemove is b,insertRemove os b))
 rs = holes setEmpty (setSize connected)
 regions = fromKeysVals rs connected
 f6 :: Sidedness -> Region -> Boundaries
 f6 s r = let (bz,(is,os)) = (sub regions r) in if s == inside then is else os
-- cospace2 finds section space of space s0 from r in the cospace s1
-- this is the surspace of the regions that have corners in both vertex sets mapped from dual in cospace
cospace2 :: Cospace1 -> Cospace2 -> Map Boundary Vertex -> Region -> Side0
cospace2 (Cospace1 st0 bs0 rs0 qm0) (Cospace2 dt1 ss1) m r =
 surspace2 (Surspace1 st0 bs0) rs where
 (inside,outside) = choose2 ss1
 i = setMap (sub m) (sub2 dt1 inside r)
 o = setMap (sub m) (sub2 dt1 outside r)
 rs = setFilter f rs0
 f r = let a = intersect c i; b = intersect c o; c = sub qm0 r in
  ((setSize a) /= 0) && ((setSize b) /= 0)

-- all spaces with boundary added
spaces1 :: Spaces0 -> Boundary -> Set Side0
spaces1 s0 b = setMap choose (valsSet (adverse (fromSet f s2))) where
 f :: Side0 -> Duals
 f st = setFold2 (g ds) m ds where
  (Duals0 ds) = ds0
  ds0 = duals1_duals0 (side0_duals1 st)
  m = equispace ds0 ds0
 g :: Duals -> Map Boundary (Boundary, Map Sidedness Sidedness) -> Duals -> Duals
 g ds m ds0 = let ds1 = setMap (mapMap (i m)) ds in if ds0 < ds1 then ds0 else ds1
 s2 = setMap h rs1
 h :: Region -> Side0
 h r = supersection1 sup0 (side0_supersection1 (cospace2 co1 co2 b2v r)) b
 i :: Map Boundary (Boundary, Map Sidedness Sidedness) -> (Boundary,Sidedness) -> (Boundary,Sidedness)
 i m (b0,s0) = let (b1,sm) = sub m b0 in (b1, sub sm s0)
 dt1 = cospace1 (spaces0_cospace0 s0) v2b
 s1 = dual0_spaces1 dt1
 sup0 = spaces0_supersection0 s0
 co1 = spaces0_cospace1 s0
 co2 = spaces1_cospace2 s1
 vs1 = spaces1_vs s1
 rs1 = spaces1_rs s1
 v2b = fromKeysVals vs1 (holes setEmpty (setSize vs1))
 b2v = inverse v2b
-- all spaces of given dimension and number of boundaries
spaces2 :: Int -> Int -> Set Side0
spaces2 dn bn = setFoldBackElse2 f g h extra (single spaces0) where
 f :: Boundary -> Set Spaces0 -> Set Side0
 f b s = unions (setMap (f0 b) s)
 f0 :: Boundary -> Spaces0 -> Set Side0
 f0 b s = spaces1 s b
 g :: Set Side0 -> Set Spaces0
 g s = setMap side0_spaces0 s
 h :: Set Spaces0 -> Set Side0
 h s = setMap spaces0_side0 s
 extra = holes bs0 (bn-dn+1)
 rep0 = simplex1 (base1 2) dn
 spaces0 = side0_spaces0 rep0
 bs0 = spaces0_bs spaces0

-- all overlaps embedded in space
overlaps1 :: Overlaps0 -> Color -> Color -> Set Topez0
overlaps1 s0 c0 c1 = valsSet (fromSet f rz) where
 f :: Regions -> Topez0
 f rs = setFold2 (g p) (equitope p p) p where
  p = polytope s1
  tm = fromSet f0 rs
  (Topei0 ti) = tope0_topei0 (Tope0 tm)
  (Topes0 ts) = tope0_topes0 (Tope0 tm)
  s1 = Polytope0 gr
  (Signs0 gs) = signs1_signs0 (overlaps0tm_signs1 s0 tm)
  (Signb0 gb) = signb2_signb0 (overlaps0gt_signb2 s0 gs tm)
  (Signr0 gr) = signr2_signr0 (overlaps0gt_signr2 s0 gs gb ti ts)
 f0 :: Region -> Color
 f0 r = if member rs r then c1 else c0
 g :: Topez0 -> Map Boundary (Boundary, Map Sidedness Sidedness) -> Topez0 -> Topez0
 g (Topez0 p) m (Topez0 p0) = let
  p1 = setMap (valsMap (setMap (mapMap (h m)))) p in
  if p0 < p1 then Topez0 p0 else Topez0 p1
 h :: Map Boundary (Boundary, Map Sidedness Sidedness) -> (Boundary,Sidedness) -> (Boundary,Sidedness)
 h m (b,s) = let (b',s') = sub m b in (b', sub s' s)
 s2 = overlaps0_subspace0 s0
 s3 = overlaps0_take1 s0
 bs = overlaps0_bs s0
 rs = overlaps0_rs s0
 bz = setSets bs (single (quot (setSize bs) 2))
 rz = setMap i bz
 i :: Boundaries -> Regions
 i bs0 = let
  bs1 = differ bs bs0
  o0 = duali0_overlaps1 (subspace2 s2 bs0)
  o1 = duali0_overlaps1 (subspace2 s2 bs1)
  rs0 = retake (overlaps1_take0 o0) s3 (overlaps1_is o0)
  rs1 = retake (overlaps1_take0 o1) s3 (overlaps1_is o1) in
  union rs0 rs1
-- all overlaps of given dimension
overlaps2 :: Int -> Color -> Color -> Set Topez0
overlaps2 dn c0 c1 = unions (setMap (f c0 c1) b) where
 f c0 c1 s = overlaps1 s c0 c1
 a = spaces2 dn (dn + 1)
 b = setMap side0_overlaps0 a

-- extend planes and coplanes by boundary
-- sample1 :: space -> subspace -> cosubspace ->
-- cosubspasce-boundary to subspace-vertex -> suspace-vertex to cosubspace-boundary ->
-- dimension -> space-boundary-not-in-subspace ->
-- (extended-subspace-planes, extended-subspace-vertices)
sample1 :: Sample0 -> Sample1 -> Sample2 -> Map Boundary Vertex -> Map Vertex Boundary ->
 Int -> Boundary -> (Plane0,Coplane0)
sample1 (Sample0 st0 ss0 vm0 pm0 ci0) (Sample1 bs1 vm1 vi1 vs1 cb1 cv1) (Sample2 di2 bm2) b2v v2b dn b0 =
 (Plane0 cb0, Coplane0 cv0) where
 s = choose ss0
 -- assume given boundary is from an inside coregion
 -- that is it is horizontal wrt the partial order
 -- that is it does not divide the outside region with same sidedness wrt all boundaries
 cb0 = extend cb1 (b0, vecScale (vecSum cs1) (1.0/(fromIntegral (setSize cs1))))
 cs1 = setMap (sub cv1) vs1
 vs1 = setMap (sub b2v) (sub bm2 r2)
 r2 = sub2 di2 s bs2
 -- assume coboundaries have some partial order
 -- that is an outside region has same sidedness wrt all boundaries
 bs2 = setMap (sub v2b) (setFilter f vs1)
 f v1 = s == (sub2 st0 b0 (g v1))
 g v1 = choose (sub pm0 (sub vm0 (sub vi1 v1)))
 cv0 = setFold2 h (setMap i (setSets bs1 (single (dn-1)))) cv1
 h bs0 cv0 = extend cv0 ((sub vm1 bs0), (vecSolve dn ci0 (j bs0)))
 i bs0 = insert bs0 b0
 j bs0 = setMap k bs0
 k b0 = (0, sub cb0 b0)
-- In general sample, first find cospace of given.
-- Choose outside coregion.
-- Extend given by section determined by chosen coregion.
-- Extend cospace by vertices on extension.
-- Choose next outside coregion by rejecting those with extension coboundaries in their cage.
-- Repeat until given is extended by an outside simplex.
-- Choose a simplex of planes for the outside simplex.
-- Fold with special sample on each given boundary. Return planes except the outside simplex.

-- In special classify, recurse to general to classify intersections with given plane.
-- Extend given space by section returned by recursion.
-- If dimension is zero, return single region separating the boundaries.
-- In general classify, fold special on each given plane.

data Tag =
 SideTag |
 DualTag |
 DualiTag |
 DualsTag |
 HalfTag |
 HalfiTag |
 BoundsTag |
 RegsTag |
 SidesTag |
 NeighborTag |
 AttachedTag |
 FlatTag |
 ShellTag |
 CageTag |
 DiskTag |
 BlotTag |
 VertTag |
 VertiTag |
 VertsTag |
 PencilTag |
 CornerTag |
 SignTag |
 InsideTag |
 OutsideTag |
 SignsTag |
 SignbTag |
 SignrTag |
 TopeTag |
 TopeiTag |
 TopesTag |
 TopezTag
 deriving (Show, Eq, Ord)

data Rep =
 SideRep Side |
 DualRep Dual |
 DualiRep Duali |
 DualsRep Duals |
 HalfRep Half |
 HalfiRep Halfi |
 BoundsRep Bounds |
 RegsRep Regs |
 SidesRep Sides |
 NeighborRep Neighbor |
 AttachedRep Attached |
 FlatRep Flat |
 ShellRep Shell |
 CageRep Cage |
 DiskRep Disk |
 BlotRep Blot |
 VertRep Vert |
 VertiRep Verti |
 VertsRep Verts |
 PencilRep Pencil |
 CornerRep Corner |
 InsideRep Inside |
 OutsideRep Outside |
 SignsRep Signs |
 SignbRep Signb |
 SignrRep Signr |
 TopeRep Tope |
 TopeiRep Topei |
 TopesRep Topes |
 TopezRep Topez
 deriving (Show, Eq, Ord)

side0_rep (Side0 st) = SideRep st
dual0_rep (Dual0 dt) = DualRep dt
duali0_rep (Duali0 di) = DualiRep di
duals0_rep (Duals0 ds) = DualsRep ds
half0_rep (Half0 ht) = HalfRep ht
halfi0_rep (Halfi0 hi) = HalfiRep hi
bounds0_rep (Bounds0 bs) = BoundsRep bs
regs0_rep (Regs0 rs) = RegsRep rs
sides0_rep (Sides0 ss) = SidesRep ss
neighbor0_rep (Neighbor0 nt) = NeighborRep nt
attached0_rep (Attached0 at) = AttachedRep at
flat0_rep (Flat0 am) = FlatRep am
shell0_rep (Shell0 bt) = ShellRep bt
cage0_rep (Cage0 bm) = CageRep bm
disk0_rep (Disk0 rt) = DiskRep rt
blot0_rep (Blot0 rm) = BlotRep rm
vert0_rep (Vert0 vm) = VertRep vm
verti0_rep (Verti0 vi) = VertiRep vi
verts0_rep (Verts0 vs) = VertsRep vs
pencil0_rep (Pencil0 pm) = PencilRep pm
corner0_rep (Corner0 qm) = CornerRep qm
inside0_rep (Inside0 is) = InsideRep is
outside0_rep (Outside0 os) = OutsideRep os
signs0_rep (Signs0 gs) = SignsRep gs
signb0_rep (Signb0 gb) = SignbRep gb
signr0_rep (Signr0 gr) = SignrRep gr
tope0_rep (Tope0 tm) = TopeRep tm
topei0_rep (Topei0 ti) = TopeiRep ti
topes0_rep (Topes0 ts) = TopesRep ts

reps_side0 [SideRep st] = Side0 st
reps_side3 [DualRep dt, BoundsRep bs, RegsRep rs, SidesRep ss] = Side3 dt bs rs ss
reps_side4 [HalfRep ht, BoundsRep bs, RegsRep rs, SidesRep ss] = Side4 ht bs rs ss
reps_dual0 [DualRep dt] = Dual0 dt
reps_dual2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss] = Dual2 st bs rs ss
reps_dual3 [DualiRep di, SidesRep ss] = Dual3 di ss
reps_duali0 [DualiRep di] = Duali0 di
reps_duali1 [DualRep dt, SidesRep ss] = Duali1 dt ss
reps_duals1 [SideRep st, BoundsRep bs, RegsRep rs] = Duals1 st bs rs
reps_half0 [HalfRep ht] = Half0 ht
reps_half2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss] = Half2 st bs rs ss
reps_half3 [HalfiRep hi, SidesRep ss] = Half3 hi ss
reps_halfi0 [HalfiRep hi] = Halfi0 hi
reps_halfi1 [HalfRep ht, SidesRep ss] = Halfi1 ht ss
reps_neighbor2 [DualRep dt, DualiRep di, BoundsRep bs, RegsRep rs, SidesRep ss] =
 Neighbor2 dt di bs rs ss
reps_attached2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss, NeighborRep nt] =
 Attached2 st bs rs ss nt
reps_flat2 [BoundsRep bs, SidesRep ss, AttachedRep at] = Flat2 bs ss at
reps_shell2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss, NeighborRep nt] =
 Shell2 st bs rs ss nt
reps_cage2 [RegsRep rs, SidesRep ss, ShellRep bt] = Cage2 rs ss bt
reps_disk2 [RegsRep rs, SidesRep ss, NeighborRep nt, ShellRep bt] = Disk2 rs ss nt bt
reps_blot2 [RegsRep rs, SidesRep ss, DiskRep rt] = Blot2 rs ss rt
reps_vert1 [DualRep dt, DualiRep di, HalfRep ht, RegsRep rs, SidesRep ss,
 FlatRep am, CageRep bm] = Vert1 dt di ht rs ss am bm
reps_verti1 [VertRep vm] = Verti1 vm
reps_verts1 [VertRep vm] = Verts1 vm
reps_verts2 [VertiRep vi] = Verts2 vi
reps_pencil1 [DualRep dt, DualiRep di, SidesRep ss, FlatRep am, VertiRep vi, VertsRep vs] =
 Pencil1 dt di ss am vi vs
reps_corner1 [RegsRep rs, CageRep bm, VertRep vm] = Corner1 rs bm vm
reps_inside1 [DualRep dt, DualiRep di, BoundsRep bs, RegsRep rs, SidesRep ss] = Inside1 dt di bs rs ss
reps_outside1 [RegsRep rs, InsideRep is] = Outside1 rs is
reps_signs1 [DualRep dt, DualiRep di, HalfRep ht, SidesRep ss, FlatRep am,
 VertiRep vi, VertsRep vs, TopeRep tm] = Signs1 dt di ht ss am vi vs tm
reps_signb2 [NeighborRep nt, VertiRep vi, PencilRep pm, SignsRep gs, TopeRep tm] = Signb2 nt vi pm gs tm
reps_signr2 [SideRep st, SidesRep ss, PencilRep pm, SignsRep gs, SignbRep gb, TopeiRep ti, TopesRep ts] =
 Signr2 st ss pm gs gb ti ts
reps_tope0 [TopeRep tm] = Tope0 tm
reps_tope1 [RegsRep rs] = Tope1 rs

-- conversions is list of tuples of space to space converter, tags converted from, tags converted to
conversions :: Map Tag [([Rep] -> Rep, [Tag])]
conversions = mapFromList [
 (SideTag, [
  (side0_rep.side3_side0.reps_side3,
   [DualTag,BoundsTag,RegsTag,SidesTag]),
  (side0_rep.side4_side0.reps_side4,
  [HalfTag,BoundsTag,RegsTag,SidesTag])]),
 (DualTag, [
  (dual0_rep.dual2_dual0.reps_dual2,
   [SideTag,BoundsTag,RegsTag,SidesTag]),
  (dual0_rep.dual3_dual0.reps_dual3,
   [DualiTag,SidesTag])]),
 (DualiTag, [
  (duali0_rep.duali1_duali0.reps_duali1,
   [DualTag,SidesTag])]),
 (DualsTag, [
  (duals0_rep.duals1_duals0.reps_duals1,
   [SideTag,BoundsTag,RegsTag])]),
 (HalfTag, [
  (half0_rep.half2_half0.reps_half2,
   [SideTag, BoundsTag, RegsTag, SidesTag]),
  (half0_rep.half3_half0.reps_half3,
   [HalfiTag, SidesTag])]),
 (HalfiTag, [
  (halfi0_rep.halfi1_halfi0.reps_halfi1,
   [HalfTag, SidesTag])]),
 (BoundsTag, [
  (bounds0_rep.side0_bounds0.reps_side0,
   [SideTag]),
  (bounds0_rep.dual0_bounds0.reps_dual0,
   [DualTag]),
  (bounds0_rep.duali0_bounds0.reps_duali0,
   [DualiTag]),
  (bounds0_rep.half0_bounds0.reps_half0,
   [HalfTag]),
  (bounds0_rep.halfi0_bounds0.reps_halfi0,
   [HalfiTag])]),
 (RegsTag, [
  (regs0_rep.side0_regs0.reps_side0,
   [SideTag]),
  (regs0_rep.dual0_regs0.reps_dual0,
   [DualTag]),
  (regs0_rep.duali0_regs0.reps_duali0,
   [DualiTag]),
  (regs0_rep.half0_regs0.reps_half0,
   [HalfTag]),
  (regs0_rep.halfi0_regs0.reps_halfi0,
   [HalfiTag])]),
 (SidesTag, [
  (sides0_rep.side0_sides0.reps_side0,
   [SideTag]),
  (sides0_rep.dual0_sides0.reps_dual0,
   [DualTag]),
  (sides0_rep.duali0_sides0.reps_duali0,
   [DualiTag]),
  (sides0_rep.half0_sides0.reps_half0,
   [HalfTag]),
  (sides0_rep.halfi0_sides0.reps_halfi0,
   [HalfiTag])]),
 (NeighborTag, [
  (neighbor0_rep.neighbor2_neighbor0.reps_neighbor2,
   [DualTag, DualiTag, BoundsTag, RegsTag, SidesTag])]),
 (AttachedTag, [
  (attached0_rep.attached2_attached0.reps_attached2,
   [SideTag, BoundsTag, RegsTag, SidesTag, NeighborTag])]),
 (FlatTag, [
  (flat0_rep.flat2_flat0.reps_flat2,
   [BoundsTag, SidesTag, AttachedTag])]),
 (ShellTag, [
  (shell0_rep.shell2_shell0.reps_shell2,
   [SideTag, BoundsTag, RegsTag, SidesTag, NeighborTag])]),
 (CageTag, [
  (cage0_rep.cage2_cage0.reps_cage2,
   [RegsTag, SidesTag, ShellTag])]),
 (DiskTag, [
  (disk0_rep.disk2_disk0.reps_disk2,
   [RegsTag, SidesTag, NeighborTag, ShellTag])]),
 (BlotTag, [
  (blot0_rep.blot2_blot0.reps_blot2,
   [RegsTag, SidesTag, DiskTag])]),
 (VertTag, [
  (vert0_rep.vert1_vert0.reps_vert1,
   [DualTag, DualiTag, HalfTag, RegsTag, SidesTag, AttachedTag, CageTag])]),
 (VertiTag, [
  (verti0_rep.verti1_verti0.reps_verti1,
   [VertTag])]),
 (VertsTag, [
  (verts0_rep.verts1_verts0.reps_verts1,
   [VertTag])]),
 (VertsTag, [
  (verts0_rep.verts2_verts0.reps_verts2,
   [VertiTag])]),
 (PencilTag, [
  (pencil0_rep.pencil1_pencil0.reps_pencil1,
   [DualTag, DualiTag, SidesTag, FlatTag, VertiTag, VertsTag])]),
 (CornerTag, [
  (corner0_rep.corner1_corner0.reps_corner1,
   [RegsTag, CageTag, VertTag])]),
 (InsideTag, [
  (inside0_rep.inside1_inside0.reps_inside1,
   [DualTag, DualiTag, BoundsTag, RegsTag, SidesTag])]),
 (OutsideTag, [
  (outside0_rep.outside1_outside0.reps_outside1,
   [RegsTag, InsideTag])]),
 (SignsTag, [
  (signs0_rep.signs1_signs0.reps_signs1,
   [DualTag, DualiTag, HalfTag, SidesTag, FlatTag, VertiTag, VertsTag, TopeTag])]),
 (SignbTag, [
  (signb0_rep.signb2_signb0.reps_signb2,
   [NeighborTag, VertiTag, PencilTag, TopeTag])]),
 (SignrTag, [
  (signr0_rep.signr2_signr0.reps_signr2,
   [NeighborTag, VertiTag, PencilTag, TopeTag])]),
 (TopeTag, [
  (tope0_rep.tope1_tope0.reps_tope1,
   [RegsTag])]),
 (TopeiTag, [
  (topei0_rep.tope0_topei0.reps_tope0,
   [TopeTag])]),
 (TopesTag, [
  (topes0_rep.tope0_topes0.reps_tope0,
   [TopeTag])])]

type Space = Map Tag Rep

-- convert augments space and gets reps as image of tags
convert :: Space -> [Tag] -> ([Rep],Space)
convert s l = let
 (p,q) = f s l
 r = listMap (sub q) l in
 if p then (r,q) else error "cannot convert" where
 f :: Space -> [Tag] -> (Bool,Space)
 f s l = listFold2 g l (True,s)
 g :: Tag -> (Bool,Space) -> (Bool,Space)
 g t (False,s) = (False,s)
 g t (True,s) = h s t
 h :: Space -> Tag -> (Bool,Space)
 h s t = if (maybeSub s t) /= Nothing then (True,s) else
  listFold2 (i t) (sub conversions t) (False,s)
 i :: Tag -> ([Rep] -> Rep, [Tag]) -> (Bool,Space) -> (Bool,Space)
 i t (j,l) (True,s) = (True,s)
 i t (j,l) (False,s) = let
  (p,q) = f s l
  r = listMap (sub q) l
  e = extend q (t, j r) in
  if p then (True,e) else (False,q)
-- force removes dependents and inserts given
force :: Space -> Tag -> Rep -> Space
force s t r = extend (restrict s (setFromList (listOptMap f b))) (t,r) where
 b :: [(Tag, [([Rep] -> Rep, [Tag])])]
 b = mapToList conversions
 f :: (Tag, [([Rep] -> Rep, [Tag])]) -> Maybe Tag
 f (a,b) = if a == t || (listAny g b) then Nothing else Just a
 g (c,d) = listAny ((==) t) d

st_space st = mapFromList [(SideTag, SideRep st)]
dt_space dt = mapFromList [(DualTag, DualRep dt)]
di_space di = mapFromList [(DualiTag, DualiRep di)]
tz_space tz = mapFromList [(TopezTag, TopezRep tz)]
side0_space (Side0 st) = st_space st
dual0_space (Dual0 dt) = dt_space dt
duali0_space (Duali0 di) = di_space di
topez0_space (Topez0 tz) = tz_space tz

space_subspace0 s0 = (Subspace0 dt ss, s1) where
 ([DualRep dt, SidesRep ss], s1) =
  convert s0 [DualTag, SidesTag]
space_superspace2 s0 = (Superspace0 dt bs ss, s1) where
 ([DualRep dt, BoundsRep bs, SidesRep ss], s1) =
  convert s0 [DualTag, BoundsTag, SidesTag]
space_take0 s0 = (Take0 dt bs ss, s1) where
 ([DualRep dt, BoundsRep bs, SidesRep ss], s1) =
  convert s0 [DualTag, BoundsTag, SidesTag]
space_take1 s0 = (Take1 dt bs rs ss, s1) where
 ([DualRep dt, BoundsRep bs, RegsRep rs, SidesRep ss], s1) =
  convert s0 [DualTag, BoundsTag, RegsTag, SidesTag]
space_rs s0 = (rs,s1) where
 ([RegsRep rs], s1) = convert s0 [RegsTag]
space_tm s0 = (tm,s1) where
 ([TopeRep tm], s1) = convert s0 [TopeTag]

side :: Space -> Boundary -> Region -> (Sidedness,Space)
side s b r = (sub2 st b r, s) where
 ([SideRep st], s) = convert s [SideTag]
bounds :: Space -> ([Boundary],Space)
bounds s = (setToList bs, s) where
 ([BoundsRep bs], s) = convert s [BoundsTag]
regs :: Space -> ([Region],Space)
regs s = (setToList rs, s) where
 ([RegsRep rs], s) = convert s [RegsTag]
sides :: Space -> ([Sidedness],Space)
sides s = (setToList ss, s) where
 ([SidesRep ss], s) = convert s [SidesTag]
color :: Space -> Region -> (Color,Space)
color s r = (sub tm r, s) where
 ([TopeRep tm], s) = convert s [TopeTag]
rename :: Space -> Space -> [Region] -> ([Region],Space,Space)
rename s0 s1 a = (setToList rs,s0',s1') where
 rs = retake (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) (setFromList a)
 ([DualRep dt0, BoundsRep bs0, SidesRep ss0], s0') = convert s0 [DualTag,BoundsTag,SidesTag]
 ([DualRep dt1, BoundsRep bs1, RegsRep rs1, SidesRep ss1], s1') = convert s1 [DualTag,BoundsTag,RegsTag,SidesTag]

empty :: Space
empty = dt_space dt where
 Dual0 dt = empty1 (base1 2)
order :: Int -> Space
order n = st_space st where
 Side0 st = order1 (base1 2) n
system :: Int -> Space
system n = dt_space dt where
 Dual0 dt = system1 (base1 2) n
simplex :: Int -> Space
simplex n = st_space st where
 Side0 st = simplex1 (base1 2) n

subspace :: Space -> [Boundary] -> (Space,Space)
subspace s0 bs = (a4,s3) where
 (a0,s1) = space_subspace0 s0
 (tm,s2) = space_tm s1
 (take1,s3) = space_take1 s2
 a1 = duali0_space (subspace2 a0 (setFromList bs))
 (rs,a2) = space_rs a1
 (take0,a3) = space_take0 a2
 a4 = extend a3 (TopeTag, TopeRep (fromSet f rs))
 f r = sub tm (choose (retake take0 take1 (single r)))
superspace :: [Space] -> ([Color] -> Color) -> (Space,[Space])
superspace s0 f = (a4,s3) where
 (a0,s1) = listUnzip (listMap space_superspace2 s0)
 a1 = dual0_space (superspace2 (setFromList a0))
 (take1,s2) = listUnzip (listMap space_take1 s1)
 (tm,s3) = listUnzip (listMap space_tm s2)
 list = listZip take1 tm
 (rs,a2) = space_rs a1
 (take0,a3) = space_take0 a2
 a4 = extend a3 (TopeTag, TopeRep (fromSet g rs))
 g r = f (listMap (h r take0) list)
 h :: Region -> Take0 -> (Take1,Tope) -> Color
 h r take0 (take1,tm) = sub tm (choose (retake take0 take1 (single r)))
spaces :: Int -> Int -> [Space]
spaces dn bn = listMap f (setToList (spaces2 dn bn)) where
 f (Side0 st) = let
  ([InsideRep is, TopeRep tm, TopesRep ts], s) = convert (st_space st) [InsideTag,TopeTag,TopesTag]
  c1 = hole ts
  g (r,c0) = if member is r then (r,c1) else (r,c0) in
  force s TopeTag (TopeRep (mapMap g tm))
overlaps :: Int -> [Space]
overlaps dn = setToList (setMap topez0_space (overlaps2 dn c0 c1)) where
 c0 = zero
 c1 = hole (single c0)

{-
display :: Space -> (Friendly,Space)
poke :: Space -> Click -> ([Pierce],Space)
scratch :: Point -> Click -> Transform
apply :: Space -> [Color] -> Transform -> (Space,Space)
pierce :: Pierce -> (Facet,Point)
facet :: Facet -> (Region,Region,Color,Color)
add :: Space -> Facet -> (Space,Space)
subtract :: Space -> Facet -> (Space,Space)
refine :: Space -> Pierce -> (Space,Space)
-}
