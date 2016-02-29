-- TRY use lists instead of Set and Map
-- TRY rely on lazy evaluation instead of representations
module Space (
 Boundary,Region,Sidedness,Space,
 space_bs,space_rs,space_ss,
 space_st,space_bz,side,rename,
 empty,order,system,simplex,
 subspace,superspace,equivalent,
 polytope,spaces,overlaps,construct) where

import Prelude hiding (take,zip,unzip,map,fold,compare,length,concat,reverse)
import qualified Data.List
import qualified Data.Set
import qualified Data.Map

linear :: Int -> Int -> Int
linear 0 n = 1
linear m 0 = 1
linear m n = (linear (m-1) n) + (linear (m-1) (n-1))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*(factorial (n-1))

ratio :: Int -> Int -> Float
ratio m n = (fromIntegral ((factorial n)*(linear m n)))/(fromIntegral (m^n))

bang :: Maybe a -> String -> a
bang Nothing b = error b
bang (Just a) b = a

strip :: Maybe (Maybe a) -> Maybe a
strip Nothing = Nothing
strip (Just a) = a

listSize :: [a] -> Int
listSize = Data.List.length

map :: (a -> b) -> [a] -> [b]
map = Data.List.map

fold :: (a -> a -> a) -> [a] -> a
fold = Data.List.foldl1

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind = Data.List.find

maybeOptFind :: Eq a => [Maybe a] -> Maybe a
maybeOptFind a = strip (maybeFind f a) where
 f a = a /= Nothing

maybeCall :: (a -> b) -> Maybe a -> Maybe b
maybeCall f (Just a) = Just (f a)
maybeCall _ _ = Nothing

maybeCall2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeCall2 f (Just a) (Just b) = Just (f a b)
maybeCall2 _ _ _ = Nothing

zip :: [a] -> [b] -> [(a,b)]
zip = Data.List.zip

unzip :: [(a,b)] -> ([a],[b])
unzip = Data.List.unzip

concat :: [[a]] -> [a]
concat = Data.List.concat

reverse :: [a] -> [a]
reverse = Data.List.reverse

type Set = Data.Set.Set

setSize :: Ord a => Set a -> Int
setSize = Data.Set.size

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

maybeConsChoose :: Ord a => Set a -> (Maybe a,Set a)
maybeConsChoose a
 | (setSize a) == 0 = (Nothing,a)
 | otherwise = (Just h,t) where
 h = choose a
 t = remove a h

consChoose :: Ord a => Set a -> (a,Set a)
consChoose a = (bang a0 "set empty", a1) where (a0,a1) = maybeConsChoose a

setToList :: Ord a => Set a -> [a]
setToList = Data.Set.toAscList

setFromList :: Ord a => [a] -> Set a
setFromList = Data.Set.fromList

class (Ord a, Enum a) => Holes a where
 zero :: a

holes :: Holes a => Set a -> Int -> Set a
holes a 0 = setFromList []
holes a b = differ (setFromList [zero..max]) a where
 max = f [zero..] (b-1)
 f (ah:at) b
  | member a ah = f at b
  | b == 0 = ah
  | otherwise = f at (b-1)

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
 f bl = mapFromList (zip al bl)

setAll :: Ord a => (a -> Bool) -> Set a -> Bool
setAll f a = Data.List.all f (setToList a)

setAny :: Ord a => (a -> Bool) -> Set a -> Bool
setAny f a = Data.List.any f (setToList a)

setMap :: Ord a => Ord b => (a -> b) -> Set a -> Set b
setMap = Data.Set.map

setMap2 :: Ord a => Ord b => Ord c => (a -> b -> c) -> Set a -> Set b -> Set c
setMap2 f a b = unions (setMap g a) where
 g a = setMap (f a) b

setConnect :: Ord a => (a -> Set a) -> a -> Set a
setConnect f a = setConnect2 f (single a)

setConnect2 :: Ord a => (a -> Set a) -> Set a -> Set a
setConnect2 f a = g a setEmpty where
 g todo rslt = if (setSize todo) == 0 then rslt else g todo0 rslt0 where
  a = choose todo
  rslt0 = insert rslt a
  todo0 = differ (union todo (f a)) rslt0

setOptMap :: Ord a => Ord b => (a -> Maybe b) -> Set a -> Set b
setOptMap f a = setMap g (setFilter h (setMap f a)) where
 g (Just b) = b
 h b = b /= Nothing

setMaybeFind :: Ord a => (a -> Bool) -> Set a -> Maybe a
setMaybeFind f a = maybeFind f (setToList a)

setFind :: Ord a => (a -> Bool) -> Set a -> a
setFind f a = bang (setMaybeFind f a) "set wrong"

setFilter :: Ord a => (a -> Bool) -> Set a -> Set a
setFilter = Data.Set.filter

setFoldlBackElse :: Ord a => (a -> b -> c) -> (c -> b) -> (b -> c) -> Set a -> b -> c
setFoldlBackElse f g h a b
 | (setSize a) == 0 = h b
 | (setSize a) == 1 = f a0 b
 | otherwise = setFoldlBackElse f g h a' b' where
 a0 = choose a
 a' = remove a a0
 b' = g (f a0 b)

setFoldBackElse :: Ord a => (a -> a -> b) -> (b -> a) -> (a -> b) -> b -> Set a -> b
setFoldBackElse f g h b a
 | (setSize a) == 0 = b
 | (setSize a) == 1 = h (choose a)
 | (setSize a) == 2 = b0
 | otherwise = setFoldBackElse f g h b a' where
 (a0,a1) = choose2 a
 b0 = f a0 a1
 a' = insert (forceRemove2 a a0 a1) (g b0)

forceInsert :: Ord a => Set a -> a -> Set a
forceInsert a b = Data.Set.insert b a

insert :: Ord a => Set a -> a -> Set a
insert a b
 | member a b = error "already member"
 | otherwise = forceInsert a b

forceInsert2 :: Ord a => Set a -> a -> a -> Set a
forceInsert2 a b c = insert (insert a b) c

insert2 :: Ord a => Set a -> a -> a -> Set a
insert2 a b c
 | member a b || member a c = error "already member"
 | otherwise = forceInsert2 a b c

forceOptInsert :: Ord a => Set a -> Maybe a -> Set a
forceOptInsert a Nothing = a
forceOptInsert a (Just b) = forceInsert a b

optInsert :: Ord a => Set a -> Maybe a -> Set a
optInsert a Nothing = a
optInsert a (Just b) = insert a b

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

forceRemove2 :: Ord a => Set a -> a -> a -> Set a
forceRemove2 a b c = remove (remove a b) c

remove2 :: Ord a => Set a -> a -> a -> Set a
remove2 a b c
 | not (member a b) || not (member a c) = error "not member"
 | otherwise = forceRemove2 a b c

forceOptRemove :: Ord a => Set a -> Maybe a -> Set a
forceOptRemove a Nothing = a
forceOptRemove a (Just b) = forceRemove a b

optRemove :: Ord a => Set a -> Maybe a -> Set a
optRemove a Nothing = a
optRemove a (Just b) = remove a b

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
unions a = fold union (setToList a)

-- aka nothing because why intersect sets known to be proper or disjoint
intersect :: Ord a => Set a -> Set a -> Set a
intersect = Data.Set.intersection

intersects :: Ord a => Set (Set a) -> Set a
intersects a = fold intersect (setToList a)

-- aka forceRemoves
differ :: Ord a => Set a -> Set a -> Set a
differ = Data.Set.difference

-- aka insertRemoves
symmetric :: Ord a => Set a -> Set a -> Set a
symmetric a b = union (differ a b) (differ b a)

type Map = Data.Map.Map

mapSize :: Map k a -> Int
mapSize = Data.Map.size

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
sub3 t a b c = sub2 (f (maybeSub t a)) b c where
 f Nothing = error "not findable"
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

mapSingle :: Ord k => (k,a) -> Map k a
mapSingle a = Data.Map.fromList [a]

fromSet :: (k -> a) -> Set k -> Map k a
fromSet = Data.Map.fromSet

fromSet2 :: (k1 -> k2 -> a) -> Set k1 -> Set k2 -> Map k1 (Map k2 a)
fromSet2 f a b = fromSet g a where g a = fromSet (f a) b

fromSet3 :: (k1 -> k2 -> k3 -> a) -> Set k1 -> Set k2 -> Set k3 -> Map k1 (Map k2 (Map k3 a))
fromSet3 f a b c = fromSet g a where g a = fromSet2 (f a) b c

fromOptSet :: (k -> Maybe a) -> Set k -> Map k a
fromOptSet f a = Data.Map.map g (Data.Map.filter h (fromSet f a)) where
 g a = bang a "fromOptSet error"
 h Nothing = False
 h (Just a) = True

fromOptSet2 :: (a -> b -> Maybe c) -> Set a -> Set b -> Map a (Map b c)
fromOptSet2 f a b = Data.Map.filter h (fromSet g a) where
 g a = fromOptSet (f a) b
 h a = not (Data.Map.null a)

mapFromList :: Ord a => [(a,b)] -> Map a b
mapFromList = Data.Map.fromList

mapToList :: Ord a => Map a b -> [(a,b)]
mapToList = Data.Map.toList

inverse :: Ord b => Map a b -> Map b a
inverse a = mapFromList (map f (Data.Map.toList a)) where f (a,b) = (b,a)

image :: Ord a => Ord b => Map a b -> Set a -> Set b
image m a = setMap (sub m) a

mapMap :: Ord c => ((a,b) -> (c,d)) -> Map a b -> Map c d
mapMap f a = mapFromList (map f (Data.Map.toList a))

mapVals :: (a -> b) -> Map k a -> Map k b
mapVals = Data.Map.map

count :: Ord a => Set a -> Map a Int
count a = relate a (setFromList [0..((setSize a)-1)])

relate :: Ord a => Set a -> Set b -> Map a b
relate a b = fromSet f a where f k = Data.Set.elemAt (Data.Set.findIndex k a) b

extend :: Ord a => Map a b -> (a,b) -> Map a b
extend m (k,v) = Data.Map.insert k v m

restrict :: Ord a => Map a b -> Set a -> Map a b
restrict m a = fromSet (sub m) a

mapUnion :: Ord a => Map a b -> Map a b -> Map a b
mapUnion a b = mapFromList ((Data.Map.toList a)++(Data.Map.toList b))

parity :: Ord a => Map a a -> Bool
parity a = f (mapToList a) where
 f [] = False
 f ((k,v):t) = (f t) /= (g k ((k,v):t) False)
 g a ((k,v):t) b = if a == v then b else g a t (not b)

-- undefined
data Boundary = Boundary Int deriving (Show, Eq, Ord)
data Region = Region Int deriving (Show, Eq, Ord)
data Sidedness = Sidedness Int deriving (Show, Eq, Ord)
data Vertex = Vertex Int deriving (Show, Eq, Ord)

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

instance Holes Boundary where zero = Boundary 0
instance Holes Region where zero = Region 0
instance Holes Sidedness where zero = Sidedness 0
instance Holes Vertex where zero = Vertex 0

-- collections of undefined
type Boundaries = Set Boundary
type Regions = Set Region
type Sidednesses = Set Sidedness
type Vertices = Set Vertex
type Direction = Map Boundary Sidedness
type Directions = Set Direction
type Polytope = Set [Directions]

-- representations
type Side = Map Boundary (Map Region Sidedness) -- st
type Dual = Map Sidedness (Map Region Boundaries) -- dt
type Duali = Map Sidedness (Map Boundaries Region) -- di
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
type Hive = Set Boundaries -- bz

-- return values and parameters for inducers and generators
data Side0 = Side0 Side deriving (Show, Eq, Ord) -- st
data Side1 = Side1 Dual Sides deriving (Show, Eq, Ord) -- dt ss
data Side2 = Side2 Half Sides deriving (Show, Eq, Ord) -- ht ss
data Side3 = Side3 Dual Bounds Regs Sides deriving (Show, Eq, Ord) -- dt bs rs ss
data Side4 = Side4 Half Bounds Regs Sides deriving (Show, Eq, Ord) -- ht bs rs ss
data Dual0 = Dual0 Dual deriving (Show, Eq, Ord) -- dt
data Dual1 = Dual1 Side Bounds Sides deriving (Show, Eq, Ord) -- st bs ss
data Dual2 = Dual2 Side Bounds Regs Sides deriving (Show, Eq, Ord) -- st bs rs ss
data Dual3 = Dual3 Duali Sides deriving (Show, Eq, Ord) -- dt ss
data Dual4 = Dual4 Regs Sides Directions deriving (Show, Eq, Ord) -- rs ss ds
data Duali0 = Duali0 Duali deriving (Show, Eq, Ord) -- di
data Duali1 = Duali1 Dual Sides deriving (Show, Eq, Ord) -- dt ss
data Half0 = Half0 Half deriving (Show, Eq, Ord) -- ht
data Half1 = Half1 Side Regs Sides deriving (Show, Eq, Ord) -- st rs ss
data Half2 = Half2 Side Bounds Regs Sides deriving (Show, Eq, Ord) -- st bs rs ss
data Half3 = Half3 Halfi Sides deriving (Show, Eq, Ord) -- hi ss
data Halfi0 = Halfi0 Halfi deriving (Show, Eq, Ord) -- hi
data Halfi1 = Halfi1 Half Sides deriving (Show, Eq, Ord) -- ht ss
data Bounds0 = Bounds0 Bounds deriving (Show, Eq, Ord) -- bs
data Regs0 = Regs0 Regs deriving (Show, Eq, Ord) -- rs
data Sides0 = Sides0 Sides deriving (Show, Eq, Ord) -- ss
data Neighbor0 = Neighbor0 Neighbor deriving (Show, Eq, Ord) -- nt
data Neighbor1 = Neighbor1 Dual Duali Sides deriving (Show, Eq, Ord) -- dt di ss
data Neighbor2 = Neighbor2 Dual Duali Bounds Regs Sides deriving (Show, Eq, Ord) -- dt di bs rs ss
data Attached0 = Attached0 Attached deriving (Show, Eq, Ord) -- at
data Attached1 = Attached1 Side Regs Neighbor deriving (Show, Eq, Ord) -- st rs nt
data Attached2 = Attached2 Side Bounds Regs Sides Neighbor deriving (Show, Eq, Ord) -- st bs rs ss nt
data Flat0 = Flat0 Flat deriving (Show, Eq, Ord) -- am
data Flat1 = Flat1 Sides Attached deriving (Show, Eq, Ord) -- ss at
data Flat2 = Flat2 Bounds Sides Attached deriving (Show, Eq, Ord) -- bs ss at
data Shell0 = Shell0 Shell deriving (Show, Eq, Ord) -- bt
data Shell1 = Shell1 Side Bounds Neighbor deriving (Show, Eq, Ord) -- st bs nt
data Shell2 = Shell2 Side Bounds Regs Sides Neighbor deriving (Show, Eq, Ord) -- st bs rs ss nt
data Cage0 = Cage0 Cage deriving (Show, Eq, Ord) -- bm
data Cage1 = Cage1 Sides Shell deriving (Show, Eq, Ord) -- ss bt
data Cage2 = Cage2 Regs Sides Shell deriving (Show, Eq, Ord) -- rs ss bt
data Disk0 = Disk0 Disk deriving (Show, Eq, Ord) -- rt
data Disk1 = Disk1 Neighbor Shell deriving (Show, Eq, Ord) -- nt bt
data Disk2 = Disk2 Regs Sides Neighbor Shell deriving (Show, Eq, Ord) -- rs ss nt bt
data Blot0 = Blot0 Blot deriving (Show, Eq, Ord) -- rm
data Blot1 = Blot1 Sides Disk deriving (Show, Eq, Ord) -- ss rt
data Blot2 = Blot2 Regs Sides Disk deriving (Show, Eq, Ord) -- rs ss rt
data Vert0 = Vert0 Vert deriving (Show, Eq, Ord) -- vm
data Vert1 = Vert1 Dual Duali Half Regs Sides Flat Cage deriving (Show, Eq, Ord) -- dt di ht rs ss am bm
data Verti0 = Verti0 Verti deriving (Show, Eq, Ord) -- vi
data Verti1 = Verti1 Vert deriving (Show, Eq, Ord) -- vm
data Verts0 = Verts0 Verts deriving (Show, Eq, Ord) -- vs
data Verts1 = Verts1 Vert deriving (Show, Eq, Ord) -- vm
data Verts2 = Verts2 Verti deriving (Show, Eq, Ord) -- vi
data Pencil0 = Pencil0 Pencil deriving (Show, Eq, Ord) -- pm
data Pencil1 = Pencil1 Dual Duali Sides Flat Verti Verts deriving (Show, Eq, Ord) -- dt di ss am vi vs
data Pencil2 = Pencil2 Dual Duali Sides Flat deriving (Show, Eq, Ord) -- dt di ss am
data Corner0 = Corner0 Corner deriving (Show, Eq, Ord) -- qm
data Corner1 = Corner1 Regs Cage Vert deriving (Show, Eq, Ord) -- rs bm vm
data Inside0 = Inside0 Inside deriving (Show, Eq, Ord) -- is
data Inside1 = Inside1 Dual Duali Bounds Regs Sides deriving (Show, Eq, Ord) -- dt di bs rs ss
data Outside0 = Outside0 Outside deriving (Show, Eq, Ord) -- os
data Outside1 = Outside1 Regs Inside deriving (Show, Eq, Ord) -- rs is
data Hive0 = Hive0 Hive deriving (Show, Eq, Ord) -- bz
data Hive1 = Hive1 Cage Inside deriving (Show, Eq, Ord) -- bm is

-- parameters for deducers and constructors
data Take0 = Take0 Dual Bounds Sides deriving (Show, Eq, Ord)
 -- dt bs ss
data Take1 = Take1 Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt bs rs ss
data Signify0 = Signify0 Dual Duali Half Sides Flat Verti Verts deriving (Show, Eq, Ord)
 -- dt di ht ss am vi vs
data Signify1 = Signify1 Neighbor Verti Pencil deriving (Show, Eq, Ord)
 -- nt vi pm
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
data Equivalent0 = Equivalent0 Dual Bounds Sides deriving (Show, Eq, Ord)
 -- dt bs ss
data Equivalent1 = Equivalent1 Bounds Hive deriving (Show, Eq, Ord)
 -- bs bz
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
data Rotate0 = Rotate0 Side Bounds Sides Half Outside deriving (Show, Eq, Ord)
 -- st bs ss ht os
data Polytope0 = Polytope0 Dual Duali Half Bounds Sides Neighbor Flat Verti Verts Pencil deriving (Show, Eq, Ord)
 -- dt di ht bs ss nt am vi vs pm
data Polytope1 = Polytope1 Side Dual Half Bounds Sides Attached Blot deriving (Show, Eq, Ord)
 -- st dt ht bs ss at rm
data Polytope2 = Polytope2 Side Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- st dt bs rs ss
data Polytope3 = Polytope3 Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt bs rs ss

-- generators take several types to produce one type, and call only inducers and accessors.
-- converters take one type to produce several types, and call only generators.
-- selectors repackage types, and call nothing.
-- no intermediate result should be converted more than once.
-- arguments are only selected, and every field of an argument should be selected at least once.

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

-- generators
side3_side0 (Side3 dt bs rs ss) = Side0 (fromSet2 (side1_s st1) bs rs) where
 st1 = Side1 dt ss
side4_side0 (Side4 ht bs rs ss) = Side0 (fromSet2 (side2_s st2) bs rs) where
 st2 = Side2 ht ss
dual2_dual0 (Dual2 st bs rs ss) = Dual0 (fromSet2 (dual1_bs dt1) ss rs) where
 dt1 = Dual1 st bs ss
dual3_dual0 (Dual3 di ss) = Dual0 (fromSet f ss) where
 f s = inverse (sub di s)
dual4_dual0 (Dual4 rs ss ds) = Dual0 (fromSet2 f ss rs) where
 t = relate rs ds
 f s r = let m = (sub t r) in setFilter (g m s) (keysSet m)
 g m s b = (sub m b) == s
duali1_duali0 (Duali1 dt ss) = Duali0 (fromSet f ss) where
 f s = inverse (sub dt s)
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
vert1_vert0 (Vert1 dt di ht rs ss am bm) = Vert0 (mapVals Vertex (count s)) where
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
hive1_hive0 (Hive1 bm is) = Hive0 (image bm is) where
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
equivalent0_subspace0 (Equivalent0 dt bs ss) = Subspace0 dt ss
equivalent0_bs (Equivalent0 dt bs ss) = bs
migrate0_side0 (Migrate0 st bs rs ss bm) = Side0 st
spaces0_side0 (Spaces0 st dt bs rs ss at rm vi qm) = Side0 st
spaces0_bs (Spaces0 st dt bs rs ss at rm vi qm) = bs
spaces0_cospace0 (Spaces0 st dt bs rs ss at rm vi qm) = Cospace0 st dt bs rs ss at rm vi qm
spaces0_supersection0 (Spaces0 st dt bs rs ss at rm vi qm) = Supersection0 st dt bs rs ss rm
spaces0_cospace1 (Spaces0 st dt bs rs ss at rm vi qm) = Cospace1 st bs rs qm
spaces1_cospace2 (Spaces1 dt rs ss vs) = Cospace2 dt ss
spaces1_vs (Spaces1 dt rs ss vs) = vs
spaces1_rs (Spaces1 dt rs ss vs) = rs
rotate0_side0 (Rotate0 st bs ss ht os) = Side0 st
polytope0_signify0 (Polytope0 dt di ht bs ss nt am vi vs pm) = Signify0 dt di ht ss am vi vs
polytope0_signify1 (Polytope0 dt di ht bs ss nt am vi vs pm) = Signify1 nt vi pm
polytope0_subspace0 (Polytope0 dt di ht bs ss nt am vi vs pm) = Subspace0 dt ss
polytope0_take0 (Polytope0 dt di ht bs ss nt am vi vs pm) = Take0 dt bs ss
polytope1_superspace0 (Polytope1 st dt ht bs ss at rm) = Superspace0 dt bs ss
polytope1_section0 (Polytope1 st dt ht bs ss at rm) = Section0 st bs ss at
polytope1_take0 (Polytope1 st dt ht bs ss at rm) = Take0 dt bs ss
polytope1_ht (Polytope1 st dt ht bs ss at rm) = ht
polytope1_rm (Polytope1 st dt ht bs ss at rm) = rm
polytope2_st (Polytope2 st dt bs rs ss) = st
polytope2_rs (Polytope2 st dt bs rs ss) = rs
polytope2_take1 (Polytope2 st dt bs rs ss) = Take1 dt bs rs ss
polytope3_take1 (Polytope3 dt bs rs ss) = Take1 dt bs rs ss
polytope3_rs (Polytope3 dt bs rs ss) = rs

-- converter
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
side0_polytope1 (Side0 st) = Polytope1 st dt ht bs ss at rm where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Half0 ht) = half2_half0 (Half2 st bs rs ss)
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
duali0_hive0 (Duali0 di) = Hive0 bz where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
 (Inside0 is) = inside1_inside0 (Inside1 dt di bs rs ss)
 (Hive0 bz) = hive1_hive0 (Hive1 bm is)
duali0_equivalent1 (Duali0 di) = Equivalent1 bs bz where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
 (Inside0 is) = inside1_inside0 (Inside1 dt di bs rs ss)
 (Hive0 bz) = hive1_hive0 (Hive1 bm is)
side0_rotate0 (Side0 st) = Rotate0 st bs ss ht os where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Half0 ht) = half2_half0 (Half2 st bs rs ss)
 (Inside0 is) = inside1_inside0 (Inside1 dt di bs rs ss)
 (Outside0 os) = outside1_outside0 (Outside1 rs is)

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
take :: Take0 -> Take1 -> Regions -> Regions -- take Regions from Take0 to Take1
take (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) rs0 = unions (setMap f rs0) where
 s = choose (intersect ss0 ss1)
 bs = intersect bs0 bs1
 f r0 = setFilter (g r0) rs1
 g r0 r1 = (intersect bs (sub2 dt0 s r0)) == (intersect bs (sub2 dt1 s r1))
signifyv :: Signify0 -> Regions -> Vertices
signifyv (Signify0 dt di ht ss am vi vs) rs = setFilter f vs where
 f v = let bs = sub vi v in setAll g (setSets bs (single ((setSize bs)-1)))
 g bs = setAny h (setMaps bs ss)
 h m = let p = polycil p0 m; q = (intersect rs p) in (q == setEmpty) || (q == p)
 p0 = Polycil0 dt di ht ss am
signifyb :: Signify1 -> Regions -> Vertex -> Boundaries
signifyb (Signify1 nt vi pm) rs v = setFilter f (sub vi v) where
 p = sub pm v
 f b = setAny (g b) p
 g b r = h r (maybeSub2 nt b r)
 h r0 Nothing = False
 h r0 (Just r1) = (member rs r0) /= (member rs r1)
signifyr :: Signify1 -> Regions -> Vertex -> Regions
signifyr (Signify1 nt vi pm) rs v = setFilter f (sub pm v) where
 bs = sub vi v
 f r = setAny (g r) bs
 g r b = h r (maybeSub2 nt b r)
 h r0 Nothing = False
 h r0 (Just r1) = (member rs r0) /= (member rs r1)

-- canonical
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
section2 s bs = setFoldlBackElse f side0_section0 section0_side0 bs s where
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
 (b3,bs3) = maybeConsChoose (subsection0_bs s0)
subsection2 :: Subsection0 -> Set Subsection1 -> Side0
subsection2 s0 s1 = setFoldBackElse f side0_subsection1 subsection1_side0 s2 s1 where
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
 figure0 = take (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) rs1
 ground = removes rs0 figure0
 figure1 = holes rs0 (setSize figure0)
 ground0 = connect (Blot0 rm0) ground (maybeChoose ground)
 boundaries = insert bs0 b0
 regions = inserts rs0 figure1
 map = relate figure1 figure0
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
 (sup1,sup2) = consChoose (setMap f s2)
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
superspace2 a = setFoldBackElse superspace1 dual0_superspace0 f g a where
 f (Superspace0 dt bs ss) = Dual0 dt
 g = Dual0 (fromSet2 h setEmpty setEmpty)
 h s b = setEmpty
-- equivalent returns s1 with boundaries renamed such that result is subspace in s0
equivalent1 :: Equivalent0 -> Equivalent0 -> Maybe Duali0
equivalent1 s0 s1 = f bs0 bs1 mapEmpty setEmpty setEmpty where
 sub0 :: Subspace0
 sub0 = equivalent0_subspace0 s0
 bs0 :: Boundaries
 bs0 = equivalent0_bs s0
 sub1 :: Subspace0
 sub1 = equivalent0_subspace0 s1
 bs1 :: Boundaries
 bs1 = equivalent0_bs s1
 f :: Boundaries -> Boundaries -> Map Boundary Boundary -> Boundaries -> Boundaries -> Maybe Duali0
 f bs0 bs1 m ks vs
  | (duali0_hive0 s0) /= (duali0_hive0 s1) = Nothing
  | (setSize bs1) == 0 = Just s1
  | otherwise = maybeOptFind [f4 b0 b1 | b0 <- bl0, b1 <- bl1] where
  s0 = (subspace2 sub0 vs)
  s1 = (subspace2 sub1 ks)
  bl0 = setToList bs0
  bl1 = setToList bs1
  f4 b0 b1 = f (remove bs0 b0) (remove bs1 b1) (extend m (b1,b0)) (insert ks b1) (insert vs b0)
-- return whether polytopes are equivalent
equivalent2 :: Polytope -> Polytope -> Bool
equivalent2 p0 p1 = undefined
-- replace region by same except reversed wrt cage boundaries
migrate1 :: Migrate0 -> Region -> Side0
migrate1 s0@(Migrate0 st0 bs0 rs0 ss0 bm0) r = Side0 (fromSet2 f bs0 rs0) where
 (inside,outside) = choose2 ss0
 m = mapFromList [(inside,outside),(outside,inside)]
 bs = sub bm0 r
 f b r = let s = sub2 st0 b r in if member bs b then sub m s else s
-- replace regions by same except reversed wrt cage boundaries
migrate2 :: Migrate0 -> Regions -> Side0
migrate2 s0 rs = setFoldlBackElse f side0_migrate0 migrate0_side0 rs s0 where
 f r s0 = migrate1 s0 r
-- TODO instead find subspace of cospace of boundaries in range of given map
cospace1 :: Cospace0 -> Map Vertex Boundary -> Dual0
cospace1 s0@(Cospace0 st0 dt0 bs0 rs0 ss0 at0 rm0 vi0 qm0) m =
 Dual0 (fromSet2 f6 ss rs) where
 -- choose boundary, find section by it, and rotate space by it,
 chosen = choose bs0; extra = hole bs0
 subspace = duali0_supersection2 (subspace1 (Subspace0 dt0 ss0) chosen)
 section = side0b_supersection3 ((section1 (Section0 st0 bs0 ss0 at0) chosen), chosen)
 Supersection3 st1 dt1 bs1 rs1 ss1 rm1 b1 = section
 parallel = single (Supersection4 dt1 ss1 extra)
 -- find supersection of section wrt chosen and rotate wrt hole in subspace
 super = side0_section0 (supersection2 subspace section parallel)
 -- use section by hole as start space for setConnect by migrations
 sections = setConnect f (section1 super extra)
 -- to find migrations, find migratable region sets
 -- f takes section in s0 to migrated sections
 f :: Side0 -> Set Side0
 f (Side0 st1) = let
  (Cospace3 st1 dt1 bs1 rs1 ss1 bm1) = side0_cospace3 (Side0 st1)
  rs = take (Take0 dt1 bs1 ss1) (Take1 dt0 bs0 rs0 ss0) rs1
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
  rslt = take (Take0 dt2 bs2 ss2) (Take1 dt1 bs1 rs1 ss1) is2
  -- find union of cages of potential result
  cond = unions (setMap (sub bm1) rslt)
  -- if union is boundaries then just taken else nothing
  in if bs == cond then Just rslt else Nothing
 -- h returns coboundaries separated by section
 h :: Side0 -> Set Boundaries
 h (Side0 st1) = let
  -- take the regions in the given section to s0
  Cospace6 dt1 bs1 rs1 ss1 = side0_cospace6 (Side0 st1)
  figure = take (Take0 dt1 bs1 ss1) (Take1 dt0 bs0 rs0 ss0) rs1
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
  bounds0 = setMap (sub m) verts0
  bounds1 = setMap (sub m) verts1
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
 center = choose internal
 connected = setConnect f4 (center,choose2 center)
 f4 :: (Set Boundaries,(Boundaries,Boundaries)) -> Set (Set Boundaries,(Boundaries,Boundaries))
 f4 (bz,(is,os)) = if member internal bz then setMap (f5 bz is os) (sub cage bz) else setEmpty
 f5 :: Set Boundaries -> Boundaries -> Boundaries -> Boundary -> (Set Boundaries,(Boundaries,Boundaries))
 f5 bz is os b = (setMap (f3 b) bz,(insertRemove is b,insertRemove os b))
 rs = holes setEmpty (setSize connected)
 regions = relate rs connected
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
--
spaces1 :: Spaces0 -> Boundary -> Set Side0
spaces1 s0 b = setMap f rs1 where
 f r = supersection1 sup0 (side0_supersection1 (cospace2 co1 co2 b2v r)) b
 dt1 = cospace1 (spaces0_cospace0 s0) v2b
 s1 = dual0_spaces1 dt1
 sup0 = spaces0_supersection0 s0
 co1 = spaces0_cospace1 s0
 co2 = spaces1_cospace2 s1
 vs1 = spaces1_vs s1
 rs1 = spaces1_rs s1
 v2b = relate vs1 (holes setEmpty (setSize vs1))
 b2v = inverse v2b
--
spaces2 :: Int -> Int -> Set Side0
spaces2 dn bn = setFoldlBackElse f g h extra (single spaces0) where
 f b s = unions (setMap (f0 b) s)
 f0 b s = spaces1 s b
 g s = setMap side0_spaces0 s
 h s = setMap spaces0_side0 s
 extra = holes bs0 (bn-dn+1)
 rep0 = simplex1 (base1 2) dn
 spaces0 = side0_spaces0 rep0
 bs0 = spaces0_bs spaces0
--
rotate1 :: Rotate0 -> Boundary -> Side0
rotate1 (Rotate0 st0 bs0 ss0 ht0 os0) b = Side0 st where
 st = fromSet2 (sub2 st0) bs0 (intersect os0 (sub2 ht0 (choose ss0) b))
--
rotate2 :: Rotate0 -> Boundaries -> Side0
rotate2 s0 bs = setFoldlBackElse f side0_rotate0 rotate0_side0 bs s0 where
 f :: Boundary -> Rotate0 -> Side0
 f b s0 = rotate1 s0 b
-- polytope1 classifies regions as polytope
-- for each significant vertex, for each significant region, add direction from region's cage
polytope1 :: Polytope0 -> Regions -> Polytope
polytope1 s rs = setMap f (signifyv arg0 rs) where
 f :: Vertex -> [Set Direction]
 f v = g (signifyb arg1 rs v) (signifyr arg1 rs v)
 g :: Boundaries -> Regions -> [Set Direction]
 g bs rs = h (duali0_polytope2 (subspace2 arg2 bs)) bs rs
 h :: Polytope2 -> Boundaries -> Regions -> [Set Direction]
 h s1 bs rs = i (j (polytope2_st s1) bs) (take arg3 (polytope2_take1 s1) rs) (polytope2_rs s1)
 i :: (Region -> Direction) -> Regions -> Regions -> [Set Direction]
 i i0 rs0 rs1 = [setMap i0 rs0, setMap i0 (differ rs1 rs0)]
 j :: Side -> Boundaries -> Region -> Direction
 j st bs r = fromSet (k st r) bs
 k :: Side -> Region -> Boundary -> Sidedness
 k st r b = sub2 st b r
 arg0 = polytope0_signify0 s
 arg1 = polytope0_signify1 s
 arg2 = polytope0_subspace0 s
 arg3 = polytope0_take0 s
-- polytope2 returns space and embedding for polytope
polytope2 :: Polytope1 -> Polytope -> (Dual0,Regions)
polytope2 s dz = (s3, setConnect2 f include) where
 f r = differ (sub rm r) exclude
 rm = polytope1_rm s
 s1 = setMap g dz
 g :: [Directions] -> Dual0
 g a = let
  ds = unions (setFromList a)
  ss = unions (setMap valsSet ds)
  rs = setFromList [zero..(Region ((setSize ds)-1))] in
  dual4_dual0 (Dual4 rs ss ds)
 s2 = setMap dual0_superspace0 s1
 s3 = superspace2 (forceInsert s2 (polytope1_superspace0 s))
 ms = unions (unions (setMap setFromList dz))
 bs = unions (setMap keysSet ms)
 ss = unions (setMap valsSet ms)
 arg0 = polytope1_section0 s
 arg1 = polytope1_take0 s
 arg2 = polytope1_ht s
 clude = setMap2 h bs ss
 h :: Boundary -> Sidedness -> (Regions,Regions)
 h b s = let
  (dt,rs) = polytope2 arg4 (i b s)
  arg3 = section1 arg0 b
  arg4 = side0_polytope1 arg3
  arg5 = dual0_polytope3 dt
  arg6 = polytope3_take1 arg5
  arg7 = polytope3_rs arg5
  incl = take arg1 arg6 rs
  excl = take arg1 arg6 (differ arg7 rs)
  half = sub2 arg2 s b in
  (intersect incl half, intersect excl half)
 i :: Boundary -> Sidedness -> Polytope
 i b s = setFilter i0 (setMap (j b s) dz)
 i0 :: [Directions] -> Bool
 i0 m = (listSize m) /= 0
 j :: Boundary -> Sidedness -> [Directions] -> [Directions]
 j b s l = filter j0 (map (k b s) l)
 j0 :: Directions -> Bool
 j0 m = (setSize m) /= 0
 k :: Boundary -> Sidedness -> Directions -> Directions
 k b s m = setFilter k0 (setMap (k1 b s) (setFilter (k2 b s) m))
 k0 :: Direction -> Bool
 k0 m = (mapSize m) /= 0
 k1 :: Boundary -> Sidedness -> Direction -> Direction
 k1 b s m = restrict m (remove (keysSet m) b)
 k2 :: Boundary -> Sidedness -> Direction -> Bool
 k2 b s m = (maybeSub m b) == (Just s)
 (incl,excl) = unzip (setToList clude)
 (include,exclude) = (unions (setFromList incl), unions (setFromList excl))

data Tag =
 SideTag |
 DualTag |
 DualiTag |
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
 HiveTag
 deriving (Show, Eq, Ord)

data Rep =
 SideRep Side |
 DualRep Dual |
 DualiRep Duali |
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
 HiveRep Hive
 deriving (Show, Eq, Ord)

data Space = Space (Map Tag Rep)
 deriving (Show, Eq, Ord)

side0_rep (Side0 st) = SideRep st
dual0_rep (Dual0 dt) = DualRep dt
duali0_rep (Duali0 di) = DualiRep di
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
hive0_rep (Hive0 bz) = HiveRep bz

reps_side0 [SideRep st] = Side0 st
reps_side3 [DualRep dt, BoundsRep bs, RegsRep rs, SidesRep ss] = Side3 dt bs rs ss
reps_side4 [HalfRep ht, BoundsRep bs, RegsRep rs, SidesRep ss] = Side4 ht bs rs ss
reps_dual0 [DualRep dt] = Dual0 dt
reps_dual2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss] = Dual2 st bs rs ss
reps_dual3 [DualiRep di, SidesRep ss] = Dual3 di ss
reps_duali0 [DualiRep di] = Duali0 di
reps_duali1 [DualRep dt, SidesRep ss] = Duali1 dt ss
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
reps_hive1 [CageRep bm, InsideRep is] = Hive1 bm is

-- conversions is list of tuples of space to space converter, tags converted from, tags converted to
conversions :: [([Rep] -> Rep, [Tag], Tag)]
conversions = [
 (side0_rep.side3_side0.reps_side3, [DualTag,BoundsTag,RegsTag,SidesTag], SideTag),
 (side0_rep.side4_side0.reps_side4, [HalfTag,BoundsTag,RegsTag,SidesTag], SideTag),
 (dual0_rep.dual2_dual0.reps_dual2, [SideTag,BoundsTag,RegsTag,SidesTag], DualTag),
 (dual0_rep.dual3_dual0.reps_dual3, [DualiTag,SidesTag], DualTag),
 (duali0_rep.duali1_duali0.reps_duali1, [DualTag,SidesTag], DualiTag),
 (half0_rep.half2_half0.reps_half2, [SideTag, BoundsTag, RegsTag, SidesTag], HalfTag),
 (half0_rep.half3_half0.reps_half3, [HalfiTag, SidesTag], HalfTag),
 (halfi0_rep.halfi1_halfi0.reps_halfi1, [HalfTag, SidesTag], HalfiTag),
 (bounds0_rep.side0_bounds0.reps_side0, [SideTag], BoundsTag),
 (bounds0_rep.dual0_bounds0.reps_dual0, [DualTag], BoundsTag),
 (bounds0_rep.duali0_bounds0.reps_duali0, [DualiTag], BoundsTag),
 (bounds0_rep.half0_bounds0.reps_half0, [HalfTag], BoundsTag),
 (bounds0_rep.halfi0_bounds0.reps_halfi0, [HalfiTag], BoundsTag),
 (regs0_rep.side0_regs0.reps_side0, [SideTag], RegsTag),
 (regs0_rep.dual0_regs0.reps_dual0, [DualTag], RegsTag),
 (regs0_rep.duali0_regs0.reps_duali0, [DualiTag], RegsTag),
 (regs0_rep.half0_regs0.reps_half0, [HalfTag], RegsTag),
 (regs0_rep.halfi0_regs0.reps_halfi0, [HalfiTag], RegsTag),
 (sides0_rep.side0_sides0.reps_side0, [SideTag], SidesTag),
 (sides0_rep.dual0_sides0.reps_dual0, [DualTag], SidesTag),
 (sides0_rep.duali0_sides0.reps_duali0, [DualiTag], SidesTag),
 (sides0_rep.half0_sides0.reps_half0, [HalfTag], SidesTag),
 (sides0_rep.halfi0_sides0.reps_halfi0, [HalfiTag], SidesTag),
 (neighbor0_rep.neighbor2_neighbor0.reps_neighbor2, [DualTag, DualiTag, BoundsTag, RegsTag, SidesTag],
  NeighborTag),
 (attached0_rep.attached2_attached0.reps_attached2, [SideTag, BoundsTag, RegsTag, SidesTag, NeighborTag],
  AttachedTag),
 (flat0_rep.flat2_flat0.reps_flat2, [BoundsTag, SidesTag, AttachedTag], FlatTag),
 (shell0_rep.shell2_shell0.reps_shell2, [SideTag, BoundsTag, RegsTag, SidesTag, NeighborTag], ShellTag),
 (cage0_rep.cage2_cage0.reps_cage2, [RegsTag, SidesTag, ShellTag], CageTag),
 (disk0_rep.disk2_disk0.reps_disk2, [RegsTag, SidesTag, NeighborTag, ShellTag], DiskTag),
 (blot0_rep.blot2_blot0.reps_blot2, [RegsTag, SidesTag, DiskTag], BlotTag),
 (vert0_rep.vert1_vert0.reps_vert1, [DualTag, DualiTag, HalfTag, RegsTag, SidesTag, AttachedTag, CageTag],
  VertTag),
 (verti0_rep.verti1_verti0.reps_verti1, [VertTag], VertiTag),
 (verts0_rep.verts1_verts0.reps_verts1, [VertTag], VertsTag),
 (verts0_rep.verts2_verts0.reps_verts2, [VertiTag], VertsTag),
 (pencil0_rep.pencil1_pencil0.reps_pencil1, [DualTag, DualiTag, SidesTag, FlatTag, VertiTag, VertsTag],
  PencilTag),
 (corner0_rep.corner1_corner0.reps_corner1, [RegsTag, CageTag, VertTag], CornerTag),
 (inside0_rep.inside1_inside0.reps_inside1, [DualTag, DualiTag, BoundsTag, RegsTag, SidesTag], InsideTag),
 (outside0_rep.outside1_outside0.reps_outside1, [RegsTag, InsideTag], OutsideTag),
 (hive0_rep.hive1_hive0.reps_hive1, [CageTag, InsideTag], HiveTag)]

-- convert augments space and gets reps as image of tags
-- repeatedly so long as leftover tags is not empty
-- find first of conversions with satisfied from set and satisfying to set
convert :: Space -> [Tag] -> ([Rep],Space)
convert s t = (map (sub m) t, Space m) where
 t' = setFromList t
 Space m' = s
 d' = keysSet m'
 e' = differ t' d'
 (m, d, e) = f (m', d', e')
 -- f extends map domain and range by first of conversions that
 -- has all arguments in current domain and some results in current range
 f :: (Map Tag Rep, Set Tag, Set Tag) -> (Map Tag Rep, Set Tag, Set Tag)
 f (m, d, e) | (setSize e) == 0 = (m, d, e)
  | otherwise = f (g (maybeFind (h d e) conversions) m d e)
 -- g returns the extensions given the originals and a conversion function
 g :: Maybe ([Rep] -> Rep, [Tag], Tag) -> Map Tag Rep -> Set Tag -> Set Tag ->
  (Map Tag Rep, Set Tag, Set Tag)
 g Nothing m d e = error "cannot convert space"
 g (Just (i, j, k)) m d e = (m', d', e') where
  m' = extend m (k,k')
  k' = i (map (sub m) j)
  j' = setFromList j
  d' = union d j'
  e' = differ e j'
 -- h returns whether the given from conversions satisfies the given domain and range
 h :: Set Tag -> Set Tag -> ([Rep] -> Rep, [Tag], Tag) -> Bool
 h d e (i, j, k) = ((intersect d j') == j') && (member e k) where
  j' = setFromList j

reps_subspace0 [DualRep dt, SidesRep ss] = Subspace0 dt ss
reps_section0 [SideRep st, BoundsRep bs, SidesRep ss, AttachedRep at] = Section0 st bs ss at
reps_superspace0 [DualRep dt, BoundsRep bs, SidesRep ss] = Superspace0 dt bs ss

subspace0_tag = [DualTag, SidesTag]
section0_tag = [SideTag, BoundsTag, SidesTag, AttachedTag]
superspace0_tag = [DualTag, BoundsTag, SidesTag]

st_space :: Side -> Space
st_space st = Space (mapFromList [(SideTag, SideRep st)])
dt_space :: Dual -> Space
dt_space dt = Space (mapFromList [(DualTag, DualRep dt)])
di_space :: Duali -> Space
di_space di = Space (mapFromList [(DualiTag, DualiRep di)])

side :: Space -> Boundary -> Region -> Sidedness
side (Space m) b r = bang (maybeOptFind [
  maybeCall f0 (maybeSub m SideTag),
  maybeCall2 f1 (maybeSub m DualTag) (maybeSub m SidesTag),
  maybeCall2 f2 (maybeSub m HalfTag) (maybeSub m SidesTag)])
  "cannot induce; try converting with space_st" where
 f0 :: Rep -> Sidedness
 f0 (SideRep st) = sub2 st b r
 f1 :: Rep -> Rep -> Sidedness
 f1 (DualRep dt) (SidesRep ss) = side1_s (Side1 dt ss) b r
 f2 :: Rep -> Rep -> Sidedness
 f2 (HalfRep ht) (SidesRep ss) = side2_s (Side2 ht ss) b r

rename :: Space -> Space -> [Region] -> ([Region],Space,Space)
rename s0 s1 a = (setToList rs,s0',s1') where
 rs = take (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) (setFromList a)
 ([DualRep dt0, BoundsRep bs0, SidesRep ss0], s0') = convert s0 [DualTag,BoundsTag,SidesTag]
 ([DualRep dt1, BoundsRep bs1, RegsRep rs1, SidesRep ss1], s1') = convert s1 [DualTag,BoundsTag,RegsTag,SidesTag]

space_bs :: Space -> (Boundaries,Space)
space_bs s = (bs,s) where
 ([BoundsRep bs], s) = convert s [BoundsTag]
space_rs :: Space -> (Regions,Space)
space_rs s = (rs,s) where
 ([RegsRep rs], s) = convert s [RegsTag]
space_ss :: Space -> (Sidednesses,Space)
space_ss s = (ss,s) where
 ([SidesRep ss], s) = convert s [SidesTag]
space_st :: Space -> (Side,Space)
space_st s = (st,s) where
 ([SideRep st], s) = convert s [SideTag]
space_bz :: Space -> (Hive,Space)
space_bz s = (bz,s) where
 ([HiveRep bz], s) = convert s [HiveTag]

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
subspace s bs = (di_space di, s1) where
 (s0,s1) = convert s subspace0_tag
 Duali0 di = subspace2 (reps_subspace0 s0) (setFromList bs)
superspace :: [Space] -> (Space,[Space])
superspace s1 = (dt_space dt, b1) where
 (a1,b1) = unzip (map f s1)
 f s = convert s superspace0_tag
 Dual0 dt = superspace2 (setFromList (map reps_superspace0 a1))
equivalent :: [[Space]] -> [[Space]] -> ([[Space]],[[Space]],[[Space]])
equivalent = undefined
polytope :: Space -> [Region] -> (Space,[Region],Space)
polytope = undefined
spaces :: Int -> Int -> [Space]
spaces dn bn = map f (setToList (spaces2 dn bn)) where
 f (Side0 st) = st_space st
overlaps :: Int -> [Space]
overlaps = undefined
construct :: Space -> [[Boundary]] -> [[(Boundary,Sidedness)]] -> (Space,Space)
construct = undefined

{- TODO
sample :: Space -> Int -> Hostile -- planes for given space of given dimension
classify :: Hostile -> Space -- sidednesses of regions wrt planes
metric :: [Space -> Hostile-> Int] -> Space -> Hostile -> Int -> (Space, Hostile)
display :: Space -> [[Region]] -> Hostile -> Friendly
pierce :: Space -> [[Region]] -> Hostile -> Click -> ([[Region]], [Facet], [Poke])
transform :: Poke -> Scratch -> Mode -> Matrix
apply :: Friendly -> Matrix -> Friendly
select :: Space -> [[Region]] -> Hostile -> (Space, [[Region]], Hostile)
add :: Space -> [[Region]] -> Facet -> Friendly -> ([[Region]], Friendly)
subtract :: Space -> [[Region]] -> Facet -> Friendly -> ([[Region]], Friendly)
refine :: Space -> [[Region]] -> Facet -> Poke -> Hostile -> (Space, [[Region]], Hostile)
-}
