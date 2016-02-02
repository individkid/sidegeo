import qualified Data.List
import qualified Data.Set
import qualified Data.Map

linear :: Int -> Int -> Int
linear 0 _ = 1
linear _ 0 = 1
linear m n = (linear (m-1) n) + (linear (m-1) (n-1))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*(factorial (n-1))

ratio :: Int -> Int -> Float
ratio m n = (fromIntegral ((factorial n)*(linear m n)))/(fromIntegral (m^n))

bang :: Maybe a -> a
bang Nothing = error "value expected"
bang (Just a) = a

listMap :: (a -> b) -> [a] -> [b]
listMap = Data.List.map

listFoldl :: (a -> b -> b) -> [a] -> b -> b
listFoldl f a b = Data.List.foldl' g b a where g b a = f a b

listFoldr :: (a -> b -> b) -> [a] -> b -> b
listFoldr f a b = Data.List.foldr f b a

listFold :: (a -> a -> a) -> [a] -> a
listFold = Data.List.foldr1

maybeListFind :: (a -> Bool) -> [a] -> Maybe a
maybeListFind = Data.List.find

listFind :: (a -> Bool) -> [a] -> a
listFind f a = bang (maybeListFind f a)

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter = Data.List.filter

type Set = Data.Set.Set

size :: Set a -> Int
size = Data.Set.size

member :: Ord a => Set a -> a -> Bool
member a b = Data.Set.member b a

empty :: Ord a => Set a
empty = Data.Set.empty

single :: a -> Set a
single = Data.Set.singleton

subSingle :: Ord a => Maybe a -> Set a
subSingle Nothing = Data.Set.empty
subSingle (Just a) = Data.Set.singleton a

double :: Ord a => a -> a -> Set a
double a b = union (single a) (single b)

subDouble :: Ord a => Maybe a -> Maybe a -> Set a
subDouble a b = union (subSingle a) (subSingle b)

maybeChoose :: Set a -> Maybe a
maybeChoose a
 | (size a) == 0 = Nothing
 | otherwise = Just (Data.Set.findMin a)

choose :: Set a -> a
choose a = bang (maybeChoose a)

maybeChoose2 :: Set a -> (Maybe a,Maybe a)
maybeChoose2 a
 | (size a) == 0 = (Nothing,Nothing)
 | (size a) == 1 = (Just (Data.Set.elemAt 0 a),Nothing)
 | otherwise = (Just (Data.Set.elemAt 0 a),Just (Data.Set.elemAt 1 a))

choose2 :: Set a -> (a,a)
choose2 a = (a0,a1) where (Just a0,Just a1) = maybeChoose2 a

maybeSplitChoose :: Ord a => Set a -> (Maybe a,Set a)
maybeSplitChoose a
 | (size a) == 0 = (Nothing,a)
 | otherwise = (Just h,t) where h = choose a; t = remove a h

splitChoose :: Ord a => Set a -> (a,Set a)
splitChoose a = (a0,a1) where (Just a0,a1) = maybeSplitChoose a

toList :: Set a -> [a]
toList = Data.Set.toList

fromList :: Ord a => [a] -> Set a
fromList = Data.Set.fromList

class (Ord a, Enum a) => Holes a where
 zero :: a

holes :: Holes a => Set a -> Int -> Set a
holes _ 0 = fromList []
holes a b = differ (fromList [zero..max]) a where
 max = f [zero..] (b-1)
 f (ah:at) b
  | b == 0 = ah
  | member a ah = f at b
  | otherwise = f at (b-1)

setMap :: Ord b => (a -> b) -> Set a -> Set b
setMap = Data.Set.map

setSubMap :: Ord b => (a -> Maybe b) -> Set a -> Set b
setSubMap f a = setMap g (setFilter h (setMap f a)) where
 g a = bang a; h Nothing = False; h _ = True

maybeSetFind :: (a -> Bool) -> Set a -> Maybe a
maybeSetFind f a = maybeListFind f (toList a)

setFind :: (a -> Bool) -> Set a -> a
setFind f a = bang (maybeSetFind f a)

setFilter :: (a -> Bool) -> Set a -> Set a
setFilter = Data.Set.filter

setFoldlBackElse :: Ord a => (a -> b -> c) -> (c -> b) -> (b -> c) -> Set a -> b -> c
setFoldlBackElse f g h a b
 | (size a) == 0 = h b
 | (size a) == 1 = f x b
 | otherwise = setFoldlBackElse f g h a' b' where
 x = choose a; a' = remove a x; b' = g (f x b)

setFoldBackElse :: Ord a => (a -> a -> b) -> (b -> a) -> (a -> b) -> b -> Set a -> b
setFoldBackElse f g h b a
 | (size a) == 0 = b
 | (size a) == 1 = h (choose a)
 | (size a) == 2 = b'
 | otherwise = setFoldBackElse f g h b a' where
 a' = insert (remove2 a a0 a1) (g b')
 b' = f a0 a1
 (a0,a1) = choose2 a

insert :: Ord a => Set a -> a -> Set a
insert a b = Data.Set.insert b a

insert2 :: Ord a => Set a -> a -> a -> Set a
insert2 a b c = insert (insert a b) c

remove :: Ord a => Set a -> a -> Set a
remove a b = Data.Set.delete b a

remove2 :: Ord a => Set a -> a -> a -> Set a
remove2 a b c = remove (remove a b) c

subRemove :: Ord a => Set a -> Maybe a -> Set a
subRemove a Nothing = a
subRemove a (Just b) = remove a b

insertRemove :: Ord a => Set a -> a -> Set a
insertRemove a b = if member a b then remove a b else insert a b

union :: Ord a => Set a -> Set a -> Set a
union = Data.Set.union

unions :: Ord a => Set (Set a) -> Set a
unions a = listFold union (toList a)

intersect :: Ord a => Set a -> Set a -> Set a
intersect = Data.Set.intersection

intersects :: Ord a => Set (Set a) -> Set a
intersects a = listFold intersect (toList a)

differ :: Ord a => Set a -> Set a -> Set a
differ = Data.Set.difference

symmetric :: Ord a => Set a -> Set a -> Set a
symmetric a b = union (differ a b) (differ b a)

type Map = Data.Map.Map

sub :: Ord k => Map k a -> k -> a
sub = (Data.Map.!)

sub2 :: Ord k1 => Ord k2 => Map k1 (Map k2 a) -> k1 -> k2 -> a
sub2 t a b = sub (sub t a) b

try :: Ord k => Map k a -> k -> Maybe a
try t a = Data.Map.lookup a t

try2 :: Ord k1 => Ord k2 => Map k1 (Map k2 a) -> k1 -> k2 -> Maybe a
try2 t a b = f (try t a) where f Nothing = Nothing; f (Just s) = try s b

keysSet :: Map k a -> Set k
keysSet = Data.Map.keysSet

valsSet :: Ord a => Ord b => Map a b -> Set b
valsSet a = setMap (sub a) (keysSet a)

keysSet2 :: Ord a => Ord b => Ord c => Map a (Map b c) -> Set b
keysSet2 a = unions (setMap keysSet (valsSet a))

valsSet2 :: Ord a => Ord b => Ord c => Map a (Map b c) -> Set c
valsSet2 a = unions (setMap valsSet (valsSet a))

fromSet :: (k -> a) -> Set k -> Map k a
fromSet = Data.Map.fromSet

fromSet2 :: (k1 -> k2 -> a) -> Set k1 -> Set k2 -> Map k1 (Map k2 a)
fromSet2 f a b = fromSet g a where g a = fromSet (f a) b

fromSubSet :: (k -> Maybe a) -> Set k -> Map k a
fromSubSet f a = Data.Map.map g (Data.Map.filter h (fromSet f a)) where
 g a = bang a; h Nothing = False; h _ = True

fromSubSet2 :: (a -> b -> Maybe c) -> Set a -> Set b -> Map a (Map b c)
fromSubSet2 f a b = Data.Map.filter h (fromSet g a) where
 g a = fromSubSet (f a) b; h a = not (Data.Map.null a)

inverse :: Ord b => Map a b -> Map b a
inverse a = Data.Map.fromList (listMap f (Data.Map.toList a)) where f (a,b) = (b,a)

mapMap :: Ord c => ((a,b) -> (c,d)) -> Map a b -> Map c d
mapMap f a = Data.Map.fromList (listMap f (Data.Map.toList a))

relate :: Ord a => Set a -> Set b -> Map a b
relate a b = fromSet f a where f k = Data.Set.elemAt (Data.Set.findIndex k a) b

data Boundary = Boundary Int deriving (Show, Eq, Ord)
data Region = Region Int deriving (Show, Eq, Ord)
data Sidedness = Sidedness Int deriving (Show, Eq, Ord)
data Order = Order Int deriving (Show, Eq, Ord)
data Index = Index Int deriving (Show, Eq, Ord)
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
instance Enum Order where
 succ (Order x) = Order (succ x); pred (Order x) = Order (pred x)
 toEnum x = Order x; fromEnum (Order x) = x
instance Enum Index where
 succ (Index x) = Index (succ x); pred (Index x) = Index (pred x)
 toEnum x = Index x; fromEnum (Index x) = x
instance Enum Vertex where
 succ (Vertex x) = Vertex (succ x); pred (Vertex x) = Vertex (pred x)
 toEnum x = Vertex x; fromEnum (Vertex x) = x

instance Holes Boundary where zero = Boundary 0
instance Holes Region where zero = Region 0
instance Holes Sidedness where zero = Sidedness 0
instance Holes Order where zero = Order 0
instance Holes Index where zero = Index 0
instance Holes Vertex where zero = Vertex 0

type Boundaries = Set Boundary -- bs
type Regions = Set Region -- rs
type Sidednesses = Set Sidedness -- ss
type Vertices = Set Vertex -- cs

data Part = Part Region Boundary Region deriving (Show, Eq, Ord)

type Side = Map Boundary (Map Region Sidedness) -- st
type Dual = Map Sidedness (Map Region Boundaries) -- dt
type Duali = Map Sidedness (Map Boundaries Region) -- di
type Half = Map Sidedness (Map Boundary Regions) -- ht
type Halfi = Map Sidedness (Map Regions Boundary) -- hi
type Bounds = Boundaries -- bs
type Regs = Regions -- rs
type Sides = Sidednesses -- ss
type Length = Int -- bn
type Linear = Int -- rn
type Dimension = Int -- dn
type Neighbor = Map Boundary (Map Region Region) -- nt
type Attached = Map Sidedness (Map Boundary Regions) -- at
type Flat = Map Boundary Regions -- am
type Shell = Map Sidedness (Map Region Boundaries) -- bt
type Cage = Map Region Boundaries -- bm
type Disk = Map Sidedness (Map Region Regions) -- rt
type Blot = Map Region Regions -- rm
type Wave = Map Order (Map Index Part) -- pt
type RWave = Map Region (Map Order Index) -- ut
type BWave = Map Boundary (Map Order Index) -- qt
type NWave = Map Region (Map Order Index) -- vt
type Inside = Regions -- is
type Outside = Regions -- os
type Incidence = Map Vertex Boundaries -- vi
type Symbol = Map Boundaries Vertex -- vm
type Verts = Vertices -- cs
type Corners = Map Region Vertices -- cm
type Star = Map Vertex Regions -- ci
type Sign = Map Boundary (Map Vertex Sidedness) -- ct

data Side0 = Side0 Side deriving (Show, Eq, Ord) -- st
data Side1 = Side1 Dual Sides deriving (Show, Eq, Ord) -- dt ss
data Side2 = Side2 Half Sides deriving (Show, Eq, Ord) -- ht ss
data Side3 = Side3 Dual Bounds Regs Sides deriving (Show, Eq, Ord) -- dt bs rs ss
data Side4 = Side4 Half Bounds Regs Sides deriving (Show, Eq, Ord) -- ht bs rs ss
data Dual0 = Dual0 Dual deriving (Show, Eq, Ord) -- dt
data Dual1 = Dual1 Side Bounds Sides deriving (Show, Eq, Ord) -- st bs ss
data Dual2 = Dual2 Side Bounds Regs Sides deriving (Show, Eq, Ord) -- st bs rs ss
data Dual3 = Dual3 Duali Sides deriving (Show, Eq, Ord) -- dt ss
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
data Length0 = Length0 Length deriving (Show, Eq, Ord) -- bn
data Length1 = Length1 Linear Dimension deriving (Show, Eq, Ord) -- rn dn
data Linear0 = Linear0 Linear deriving (Show, Eq, Ord) -- rn
data Linear1 = Linear1 Length Dimension deriving (Show, Eq, Ord) -- bn dn
data Dimension0 = Dimension0 Dimension deriving (Show, Eq, Ord) -- dn
data Dimension1 = Dimension1 Length Linear deriving (Show, Eq, Ord) -- bn rn
data Neighbor0 = Neighbor0 Neighbor deriving (Show, Eq, Ord) -- nt
data Neighbor1 = Neighbor1 Dual Duali Sides deriving (Show, Eq, Ord) -- dt di ss
data Neighbor2 = Neighbor2 Dual Duali Bounds Regs Sides deriving (Show, Eq, Ord) -- dt di bs rs ss
data Attached0 = Attached0 Attached deriving (Show, Eq, Ord) -- at
data Attached1 = Attached1 Side Regs Neighbor deriving (Show, Eq, Ord) -- st rs nt
data Attached2 = Attached2 Side Bounds Regs Sides Neighbor deriving (Show, Eq, Ord) -- st bs rs ss nt
data Flat0 = Flat0 Flat deriving (Show, Eq, Ord) -- am
data Flat1 = Flat1 Sides Attached deriving (Show, Eq, Ord) -- ss at
data Flat2 = Flat2 Bounds Sides Attached deriving (Show, Eq, Ord) -- bs ss at
data Flat3 = Flat3 Dual Duali Sides Flat deriving (Show, Eq, Ord) -- dt di ss am
data Flat4 = Flat4 Dual Duali Half Sides Flat deriving (Show, Eq, Ord) -- dt di ht ss am
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
data Take0 = Take0 Dual Bounds Sides deriving (Show, Eq, Ord) -- dt bs ss
data Take1 = Take1 Dual Bounds Regs Sides deriving (Show, Eq, Ord) -- dt bs rs ss
data Subspace0 = Subspace0 Dual Sides deriving (Show, Eq, Ord) -- dt ss
data Surspace0 = Surspace0 Side Bounds Regs deriving (Show, Eq, Ord) -- st bs rs
data Surspace1 = Surspace1 Side Bounds deriving (Show, Eq, Ord) -- st bs
data Section0 = Section0 Side Bounds Sides Attached deriving (Show, Eq, Ord) -- st bs ss at
data Supersection0 = Supersection0 Side Dual Bounds Regs Sides Blot deriving (Show, Eq, Ord)
 -- st dt bs rs ss rm
data Supersection1 = Supersection1 Dual Bounds Regs Sides deriving (Show, Eq, Ord) -- dt bs rs ss
data Subsection0 = Subsection0 Side Dual Bounds Sides Attached deriving (Show, Eq, Ord)
 -- st dt bs ss at
data Subsection1 = Subsection1 Dual Sides deriving (Show, Eq, Ord) -- dt ss
data Supersection2 = Supersection2 Side Dual Bounds Regs Sides Attached Blot deriving (Show, Eq, Ord)
 -- st dt bs rs ss at rm
data Supersection3 = Supersection3 Side Dual Bounds Regs Sides Blot Boundary deriving (Show, Eq, Ord)
 -- st dt bs rs ss rm b
data Supersection4 = Supersection4 Dual Sides Boundary deriving (Show, Eq, Ord) -- dt ss b
data Superspace = Superspace Dual Bounds Sides deriving (Show, Eq, Ord) -- dt bs ss

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
neighbor1_r (Neighbor1 dt di ss) b r = try2 di s (insertRemove (sub2 dt s r) b) where
 s = choose ss
attached1_rs :: Attached1 -> Sidedness -> Boundary -> Regions
attached1_rs (Attached1 st rs nt) s b = setFilter f rs where
 f r = (try2 nt b r) /= Nothing && (sub2 st b r) == s
flat1_rs :: Flat1 -> Boundary -> Regions
flat1_rs (Flat1 ss at) b = unions (setMap f ss) where
 f s = sub2 at s b
shell1_bs :: Shell1 -> Sidedness -> Region -> Boundaries
shell1_bs (Shell1 st bs nt) s r = setFilter f bs where
 f b = (try2 nt b r) /= Nothing && (sub2 st b r) == s
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
bounds0_length0 (Bounds0 bs) = Length0 (size bs) where
 -- nothing
length1_length0 (Length1 rn dn) = Length0 (listFind f [0..]) where
 f bn = rn == (linear bn dn)
regs0_linear0 (Regs0 rs) = Linear0 (size rs) where
 -- nothing
linear1_linear0 (Linear1 bn dn) = Linear0 (linear bn dn) where
 -- nothing
dimension1_dimension0 (Dimension1 bn rn) = Dimension0 (listFind f [0..]) where
 f dn = rn == (linear bn dn)
neighbor2_neighbor0 (Neighbor2 dt di bs rs ss) = Neighbor0 (fromSubSet2 (neighbor1_r nt1) bs rs) where
 nt1 = Neighbor1 dt di ss
attached2_attached0 (Attached2 st bs rs ss nt) = Attached0 (fromSet2 (attached1_rs at1) ss bs) where
 at1 = Attached1 st rs nt
flat2_flat0 (Flat2 bs ss at) = Flat0 (fromSet (flat1_rs rm1) bs) where
 rm1 = Flat1 ss at
shell2_shell0 (Shell2 st bs rs ss nt) = Shell0 (fromSet2 (shell1_bs bt1) ss rs) where
 bt1 = Shell1 st bs nt
cage2_cage0 (Cage2 rs ss bt) = Cage0 (fromSet (cage1_bs bm1) rs) where
 bm1 = Cage1 ss bt
disk2_disk0 (Disk2 rs ss nt bt) = Disk0 (fromSet2 (disk1_rs rt1) ss rs) where
 rt1 = Disk1 nt bt
blot2_blot0 (Blot2 rs ss rt) = Blot0 (fromSet (blot1_rs rm1) rs) where
 rm1 = Blot1 ss rt

-- selectors
section0_side0 (Section0 st _ _ _) = Side0 st where
 -- nothing
subsection0_subspace0 (Subsection0 _ dt _ ss _) = Subspace0 dt ss where
 -- nothing
subsection0_section0 (Subsection0 st _ bs ss at) = Section0 st bs ss at where
 -- nothing
subsection0_bs (Subsection0 _ _ bs _ _) = bs where
 -- nothing
subsection0_side0 (Subsection0 st _ _ _ _) = Side0 st where
 -- nothing
subsection1_subspace0 (Subsection1 dt ss) = Subspace0 dt ss where
 -- nothing
supersection2_subsection0 (Supersection2 st dt bs rs ss at rm) = Subsection0 st dt bs ss at where
 -- nothing
supersection2_supersection0 (Supersection2 st dt bs rs ss at rm) = Supersection0 st dt bs rs ss rm where
 -- nothing
supersection3_subsection1 (Supersection3 st dt bs rs ss rm b) = Subsection1 dt ss where
 -- nothing
supersection3_supersection0 (Supersection3 st dt bs rs ss rm b) = Supersection0 st dt bs rs ss rm where
 -- nothing
supersection3_supersection1b (Supersection3 st dt bs rs ss rm b) = (Supersection1 dt bs rs ss, b) where
 -- nothing
supersection4_subsection1b (Supersection4 dt ss b) = (Subsection1 dt ss, b) where
 -- nothing

-- unconverter
subsection1_side0 (Subsection1 dt ss) = Side0 st where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)

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
side0_superspace (Side0 st) = Superspace dt bs ss where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
dual0_superspace (Dual0 dt) = Superspace dt bs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
dual0_surspace0 (Dual0 dt) = Surspace0 st bs rs where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)

-- deducers
polyant :: Half0 -> Map Boundary Sidedness -> Regions
polyant (Half0 ht) b2s = intersects (setMap f (keysSet b2s)) where
 f b = sub2 ht (sub b2s b) b
colleague :: Neighbor1 -> Boundaries -> Region -> Maybe Region
colleague (Neighbor1 dt di ss) bs r = try2 di s (symmetric bs (sub2 dt s r)) where
 s = choose ss
pencil :: Flat3 -> Boundaries -> Regions
pencil (Flat3 dt di ss am) bs = setFilter f (intersects (setMap (sub am) bs)) where
 n1 = Neighbor1 dt di ss; f r = (colleague n1 bs r) /= Nothing
polycil :: Flat4 -> Map Boundary Sidedness -> Regions
polycil (Flat4 dt di ht ss am) b2s = intersect rs0 rs1 where
 rs0 = (polyant (Half0 ht) b2s); rs1 = (pencil (Flat3 dt di ss am) (keysSet b2s))
connect :: Blot0 -> Regions -> Maybe Region -> Regions
connect (Blot0 rm) rs r = f r (subRemove rs r) (fromList []) (fromList []) where
 f Nothing _ _ rslt = rslt
 f (Just r) pool todo rslt = f (maybeChoose todo0) pool0 todo0 rslt0 where
  blot = intersect (sub rm r) pool
  pool0 = differ pool blot
  todo0 = remove (union todo blot) r
  rslt0 = insert rslt r
spaceTake :: Take0 -> Take1 -> Regions -> Regions -- take Regions from Take0 to Take1
spaceTake (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) rs0 = unions (setMap f rs0) where
 s = choose (intersect ss0 ss1)
 bs = intersect bs0 bs1
 f r0 = setFilter (g r0) rs1
 g r0 r1 = (intersect bs (sub2 dt0 s r0)) == (intersect bs (sub2 dt1 s r1))
-- vsignal significant vertices of polytope given by regions 
-- vsignal :: Space -> Regions -> Vertices
-- vsignal = undefined
-- bsignal significant boundaries wrt vertex
-- bsignal :: Space -> Regions -> Vertex -> Boundaries
-- bsignal = undefined
-- rsignal significant regions wrt vertex
-- rsignal :: Space -> Regions -> Vertex -> Regions
-- rsignal = undefined

-- constructors
-- subspace regions are same wrt remaining boundaries
subspace0 :: Subspace0 -> Boundary -> Duali0
subspace0 (Subspace0 dt ss) b = Duali0 (fromSet f ss) where
 f s = mapMap g (sub dt s)
 g (k,v) = (remove v b,k)
subspace :: Subspace0 -> Boundaries -> Duali0
subspace (Subspace0 dt ss) bs = Duali0 (fromSet f ss) where
 f s = mapMap g (sub dt s)
 g (k,v) = (intersect bs v,k)
-- surspace is section of given regions
surspace0 :: Surspace0 -> Region -> Side0
surspace0 (Surspace0 st bs rs) r = Side0 (fromSet2 f bs (remove rs r)) where
 f b r = sub2 st b r
surspace :: Surspace1 -> Regions -> Side0
surspace (Surspace1 st bs) rs = Side0 (fromSet2 f bs rs) where
 f b r = sub2 st b r
-- section regions are isomorhpic to regions attached on some side
-- fails if boundary not in space
section1 :: Section0 -> Boundary -> Side0
section1 (Section0 st bs ss at) b = Side0 (fromSet f (remove bs b)) where
 rs = sub2 at (choose ss) b
 f b = fromSet (sub2 st b) rs
section :: Section0 -> Boundaries -> Side0
section s bs = setFoldlBackElse f g h bs s where
 f b s = section1 s b
 g = side0_section0
 h = section0_side0
-- choose boundary and find subspaces and section
-- recurse on subspaces to find subspace of result
-- recurse on section and subspace of result for section in result
-- add section to subspace of result for result
subsection1 :: Subsection0 -> Subsection1 -> Subsection1 -> Side0
subsection1 s0 s1 s2
 | b3 == Nothing = subsection0_side0 s0
 | otherwise = let
 (Just b) = b3
 s3 = duali0_subsection0 (subspace (subsection0_subspace0 s0) bs3)
 s4 = subspace (subsection1_subspace0 s1) bs3
 s5 = subspace (subsection1_subspace0 s2) bs3
 s6 = section1 (subsection0_section0 s0) b
 s7 = subsection1 s3 (duali0_subsection1 s4) (duali0_subsection1 s5)
 s8 = subsection1 s3 (side0_subsection1 s6) (side0_subsection1 s7) in
 supersection1 (side0_supersection0 s7) (side0_supersection1 s8) b where
 (b3,bs3) = maybeSplitChoose (subsection0_bs s0)
subsection :: Subsection0 -> Set Subsection1 -> Side0
subsection s0 s1 = setFoldBackElse f g h e s1 where
 f = (subsection1 s0)
 g = side0_subsection1
 h = subsection1_side0
 e = (subsection0_side0 s0)
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
 figure0 = spaceTake (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) rs1
 ground = differ rs0 figure0
 figure1 = holes rs0 (size figure0)
 ground0 = connect (Blot0 rm0) ground (maybeChoose ground)
 boundaries = insert bs0 b0
 regions = union rs0 figure1
 map = relate figure1 figure0
 f b r
  | b == b0 && (member ground0 r || member figure0 r) = inside
  | b == b0 = outside
  | member figure1 r = sub2 st0 b (sub map r)
  | otherwise = sub2 st0 b r
-- supersection (universe for supersection, and universe for subsection)
--              (universe for supersection, and arg for subsection)
--          Set (arg for supersection, and arg for subsection)
supersection :: Supersection2 -> Supersection3 -> Set Supersection4 -> Side0
supersection s0 s1 s2
 | (size s2) == 0 = rslt
 | otherwise = let
 sub0 = supersection2_subsection0 s0
 sub1 = supersection3_subsection1 s1
 sup0 = supersection3_supersection0 s1
 (sup1,sup2) = splitChoose (setMap f s2)
 f s2 = let
  (sub2,b2) = supersection4_subsection1b s2
  sup1 = side0_supersection1 (subsection1 sub0 sub1 sub2) in
  (supersection1 sup0 sup1 b0, b2)
 s0' = side0_supersection2 rslt
 s1' = side0b_supersection3 sup1
 s2' = setMap side0b_supersection4 sup2 in
 supersection s0' s1' s2' where
 sup0 = supersection2_supersection0 s0
 (sup1,b0) = (supersection3_supersection1b s1)
 rslt = supersection1 sup0 sup1 b0
-- see http://www.sidegeo.blogspot.com/ for result linear if args are linear
superspace1 :: Superspace -> Superspace -> Dual0
superspace1 (Superspace dt0 bs0 ss0) (Superspace dt1 bs1 ss1)
 | shared == bs0 = Dual0 dt1
 | shared == bs1 = Dual0 dt0
 | otherwise = let
 bound0 = choose (differ bs0 shared)
 bounds0 = insert shared bound0
 sub0 = subspace (Subspace0 dt0 ss0) bounds0
 sect0 = section1 (duali0_section0 sub0) bound0
 bound1 = choose (differ bs1 shared)
 bounds1 = insert shared bound1
 sub1 = subspace (Subspace0 dt1 ss1) bounds1
 sect1 = section1 (duali0_section0 sub1) bound1
 sub2 = subspace (Subspace0 dt0 ss0) shared
 arg0 = duali0_supersection2 sub2
 arg1 = side0b_supersection3 (sect0,bound0)
 arg2 = single (side0b_supersection4 (sect1,bound1))
 sect2 = supersection arg0 arg1 arg2
 space0 = superspace1 (Superspace dt0 bs0 ss0) (side0_superspace sect2) in
 superspace1 (Superspace dt1 bs1 ss1) (dual0_superspace space0) where
 shared = intersect bs0 bs1
superspace :: Set Superspace -> Dual0
superspace a = setFoldBackElse superspace1 dual0_superspace f g a where
 f (Superspace dt bs ss) = Dual0 dt; g = Dual0 (fromSet2 h empty empty); h s b = empty
emptyspace :: Sides0 -> Dual0
emptyspace (Sides0 ss) = Dual0 (fromSet2 f ss (single zero)) where
 f s r = empty
orderspace :: Sides0 -> Length0 -> Side0
orderspace (Sides0 ss) (Length0 bn) = Side0 (fromSet2 f bs rs) where
 (inside,outside) = choose2 ss
 bs = fromList [zero..(Boundary (bn-1))]
 rs = fromList [zero..(Region bn)]
 f (Boundary b) (Region r) = if r <= b then inside else outside
powerspace :: Sides0 -> Length0 -> Dual0
powerspace (Sides0 ss) (Length0 bn) = superspace (fromList (listMap f bs)) where
 bs = [zero..(Boundary (bn-1))]
 dual0 = emptyspace (Sides0 ss)
 empty0 = dual0_supersection0 dual0
 empty1 = dual0_supersection1 dual0
 f b = side0_superspace (supersection1 empty0 empty1 b)
simplespace :: Sides0 -> Length0 -> Side0
simplespace sides0 length0 = surspace0 (Surspace0 st bs rs) (choose rs) where
 Surspace0 st bs rs = dual0_surspace0 (powerspace sides0 length0)
