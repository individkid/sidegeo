module SideGeo.Kernel where

import SideGeo.Container
import SideGeo.Lambda
import SideGeo.Types
import SideGeo.Implicit1
import SideGeo.Deduce
import SideGeo.Induce
import SideGeo.Implicit2
import SideGeo.Equivalent

linear :: Int -> Int -> Int
linear 0 _ = 1
linear _ 0 = 1
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

-- trivial constructors
base1 :: Int -> Sides0
base1 n = Sides0 (setFromList [zero..(Sidedness (n-1))])
empty1 :: Sides0 -> Dual0
empty1 (Sides0 ss) = Dual0 (fromSet2 f ss rs) where
 f _ _ = setEmpty
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
 f b' r' = sub2 st b' r'
 rs0 = remove rs r
surspace2 :: Surspace1 -> Regions -> Side0
surspace2 (Surspace1 st bs) rs = Side0 (fromSet2 f bs rs) where
 f b r = sub2 st b r
-- section regions are isomorhpic to regions attached on some side
-- fails if boundary not in space
section1 :: Section0 -> Boundary -> Side0
section1 (Section0 st bs ss at) b = Side0 (fromSet f (remove bs b)) where
 rs = sub2 at (choose ss) b
 f b' = fromSet (sub2 st b') rs
section2 :: Section0 -> Boundaries -> Side0
section2 s bs = setFoldBackElse2 f side0_section0 section0_side0 bs s where
 f b' s' = section1 s' b'
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
 relate = fromKeysVals figure1 figure0
 f b r
  | b == b0 && (member ground0 r || member figure0 r) = inside
  | b == b0 = outside
  | member figure1 r = sub2 st0 b (sub relate r)
  | otherwise = sub2 st0 b r
-- supersection (universe for supersection, and universe for subsection)
--              (universe for supersection, and arg for subsection)
--          Set (arg for supersection, and arg for subsection)
supersection2 :: Supersection2 -> Supersection3 -> Set Supersection4 -> Side0
supersection2 s0 s1 s2
 | (setSize s2) == 0 = s5
 | otherwise = let
 s0sub0 = supersection2_subsection0 s0
 s0sub1 = supersection3_subsection1 s1
 ((s3,b3),s4) = chooseRemove (setMap f s2)
 f s2' = let
  (s2sub1,b2) = supersection4_subsection1_b s2'
  s3sup1 = side0_supersection1 (subsection1 s0sub0 s0sub1 s2sub1) in
  (supersection1 s0sup0 s3sup1 b3, b2)
 s5sup2 = side0_supersection2 s5
 s3sup3 = side0_b_supersection3 s3 b3
 s4sup4 = setMap g s4
 g (s4',b4') = side0_b_supersection4 s4' b4' in
 supersection2 s5sup2 s3sup3 s4sup4 where
 s0sup0 = supersection2_supersection0 s0
 (s1sup1,b1) = supersection3_supersection1_b s1
 s5 = supersection1 s0sup0 s1sup1 b1
-- see http://www.sidegeo.blogspot.com/ for proof that result is linear if args are linear
superspace1 :: Superspace0 -> Superspace0 -> Dual0
superspace1 (Superspace0 dt0 bs0 ss0) (Superspace0 dt1 bs1 ss1)
 | shared == bs0 = Dual0 dt1
 | shared == bs1 = Dual0 dt0
 | otherwise = let
 bound0 = choose (removes bs0 shared)
 bounds0 = insert shared bound0
 subs0 = subspace2 (Subspace0 dt0 ss0) bounds0
 sect0 = section1 (duali0_section0 subs0) bound0
 bound1 = choose (removes bs1 shared)
 bounds1 = insert shared bound1
 subs1 = subspace2 (Subspace0 dt1 ss1) bounds1
 sect1 = section1 (duali0_section0 subs1) bound1
 subs2 = subspace2 (Subspace0 dt0 ss0) shared
 -- subs2 = subspace2 (Subspace0 dt1 ss1) shared -- bz equal?
 arg0 = duali0_supersection2 subs2
 arg1 = side0_b_supersection3 sect0 bound0
 arg2 = single (side0_b_supersection4 sect1 bound1)
 sect2 = supersection2 arg0 arg1 arg2
 space0 = superspace1 (Superspace0 dt0 bs0 ss0) (side0_superspace0 sect2) in
 superspace1 (Superspace0 dt1 bs1 ss1) (dual0_superspace0 space0) where
 shared = intersect bs0 bs1
superspace2 :: Set Superspace0 -> Dual0
superspace2 a = setFoldBackElse1 superspace1 dual0_superspace0 f g a where
 f (Superspace0 dt _ _) = Dual0 dt
 g = Dual0 (fromSet2 h setEmpty setEmpty)
 h _ _ = setEmpty
-- replace region by same except reversed wrt cage boundaries
migrate1 :: Migrate0 -> Region -> Side0
migrate1 (Migrate0 st0 bs0 rs0 ss0 bm0) r = Side0 (fromSet2 f bs0 rs0) where
 (inside,outside) = choose2 ss0
 m = mapFromList [(inside,outside),(outside,inside)]
 bs = sub bm0 r
 f b' r' = let s = sub2 st0 b' r' in if member bs b' then sub m s else s
-- replace regions by same except reversed wrt cage boundaries
migrate2 :: Migrate0 -> Regions -> Side0
migrate2 s0 rs = setFoldBackElse2 f side0_migrate0 migrate0_side0 rs s0 where
 f r' s0' = migrate1 s0' r'

-- Find cospace by finding supersection
--  of section by boundary
--  with each section connected by migration.
-- Migrate a section by finding region blocks to migrate with.
-- Find region blocks by trying take
--  from each region of each vertex space from sectioned space.
-- A set of regions is a block if it's cage union is just the vertex boundaries.
cospace1 :: Cospace0 -> Map Vertex Boundary -> Dual0
cospace1 (Cospace0 st0 dt0 bs0 rs0 ss0 at0 rm0 vi0 qm0) m =
 Dual0 (fromSet2 f6 ss rs) where
 -- choose boundary, find section and parallel by it
 chosen = choose bs0; extra = hole bs0
 subspace = duali0_supersection2 (subspace1 (Subspace0 dt0 ss0) chosen)
 section = side0_b_supersection3 (section1 (Section0 st0 bs0 ss0 at0) chosen) chosen
 Supersection3 _ dt1 _ _ ss1 _ _ = section
 parallel = single (Supersection4 dt1 ss1 extra)
 -- find supersection of section and parallel
 super = side0_section0 (supersection2 subspace section parallel)
 -- use section by parallel as start space for setConnect by migrations
 sections = setConnect f (section1 super extra)
 -- to find migrations, find migratable region sets
 -- f takes section in s0 to migrated sections
 f :: Side0 -> Set Side0
 f (Side0 st1') = let
  (Cospace3 _ dt1' bs1' rs1' ss1' bm1') = side0_cospace3 (Side0 st1')
  rs' = retake (Take0 dt1' bs1' ss1') (Take1 dt0 bs0 rs0 ss0) rs1'
  vs' = unions (setMap (sub qm0) rs')
  rz' = setOptMap (g (Cospace4 dt1' bs1' rs1' ss1' bm1')) vs'
  in setMap (migrate2 (Migrate0 st1' bs1' rs1' ss1' bm1')) rz'
 -- a section-region-set is migratable if its block is the boundaries of a vertex
 -- a block of a set of regions is the union of their cages
 -- if take of inside of subspace in s1 is block in s1, return it
 -- Vertex from s0 to Regions in s1
 g :: Cospace4 -> Vertex -> Maybe Regions
 g (Cospace4 dt1' bs1' rs1' ss1' bm1') v' = let
  -- find boundaries through vertex in s0
  bs' = sub vi0 v'
  -- find subspace in s1 by boundaries through vertex
  Cospace5 dt2' bs2' ss2' is2' = duali0_cospace5 (subspace2 (Subspace0 dt1' ss1') bs')
  -- take inside of subspace to s1 to potential result
  rslt = retake (Take0 dt2' bs2' ss2') (Take1 dt1' bs1' rs1' ss1') is2'
  -- find union of cages of potential result
  cond = unions (setMap (sub bm1') rslt)
  -- if union is boundaries then just taken else nothing
  in if bs' == cond then Just rslt else Nothing
 -- now find coregions for sections
 -- h returns coboundaries separated by section
 h :: Side0 -> Set Boundaries
 h (Side0 st1') = let
  -- take the regions in the given section to s0
  Cospace6 dt1' bs1' rs1' ss1' = side0_cospace6 (Side0 st1')
  figure = retake (Take0 dt1' bs1' ss1') (Take1 dt0 bs0 rs0 ss0) rs1'
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
 (inside,_) = choose2 ss
 only = union (single bs) (single setEmpty)
 internal = remove bz only
 cage = fromSet f1 internal
 f1 :: Set Boundaries -> Boundaries
 f1 a = setFilter (f2 a) bs
 f2 :: Set Boundaries -> Boundary -> Bool
 f2 a b = member bz (setMap (f3 b) a)
 f3 :: Boundary -> Boundaries -> Boundaries
 f3 b' bs' = insertRemove bs' b'
 center = choose internal -- does not work if s0 is vertex space
 connected = setConnect f4 (center,choose2 center)
 f4 :: (Set Boundaries, (Boundaries, Boundaries)) ->
  Set (Set Boundaries, (Boundaries, Boundaries))
 f4 (bz',(is',os')) = if member internal bz' then
  setMap (f5 bz' is' os') (sub cage bz') else setEmpty
 f5 :: Set Boundaries -> Boundaries -> Boundaries -> Boundary ->
  (Set Boundaries, (Boundaries, Boundaries))
 f5 bz' is' os' b' = (setMap (f3 b') bz', (insertRemove is' b' ,insertRemove os' b'))
 rs = holes setEmpty (setSize connected)
 regions = fromKeysVals rs connected
 f6 :: Sidedness -> Region -> Boundaries
 f6 s' r' = let (_,(is',os')) = (sub regions r') in if s' == inside then is' else os'
-- cospace2 finds section space of space s0 from r in the cospace s1
-- this is the surspace of the regions
-- that have corners in both vertex sets mapped from dual in cospace
cospace2 :: Cospace1 -> Cospace2 -> Map Boundary Vertex -> Region -> Side0
cospace2 (Cospace1 st0 bs0 rs0 qm0) (Cospace2 dt1 ss1) m r =
 surspace2 (Surspace1 st0 bs0) rs where
 (inside,outside) = choose2 ss1
 i = setMap (sub m) (sub2 dt1 inside r)
 o = setMap (sub m) (sub2 dt1 outside r)
 rs = setFilter f rs0
 f r' = let a = intersect c i; b = intersect c o; c = sub qm0 r' in
  ((setSize a) /= 0) && ((setSize b) /= 0)

-- all spaces with boundary added
spaces1 :: Spaces0 -> Boundary -> Set Side0
spaces1 s0 b = setMap choose (valsSet (adverse (fromSet f s2))) where
 f :: Side0 -> Duals
 f st = setFold2 (g ds) m ds where
  (Duals0 ds) = ds0
  ds0 = duals1_duals0 (side0_duals1 st)
  m = equispace ds0 ds0
 g :: Duals -> Symmetry -> Duals -> Duals
 g ds m ds0 = let ds1 = setMap (mapMap (i m)) ds in if ds0 < ds1 then ds0 else ds1
 s2 = setMap h rs1
 h :: Region -> Side0
 h r = supersection1 sup0 (side0_supersection1 (cospace2 co1 co2 b2v r)) b
 i :: Symmetry -> (Boundary,Sidedness) -> (Boundary,Sidedness)
 i m' (b',s') = let (b1,sm) = sub m' b' in (b1, sub sm s')
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

-- extend planes and coplanes by boundary
-- sample1 :: space -> subspace -> cosubspace ->
-- cosubspasce-boundary to subspace-vertex -> suspace-vertex to cosubspace-boundary ->
-- dimension -> space-boundary-not-in-subspace ->
-- (extended-subspace-planes, extended-subspace-vertices)
sample1 :: Sample0 -> Sample1 -> Sample2 -> Map Boundary Vertex -> Map Vertex Boundary ->
 Int -> Boundary -> (Plane0,Coplane0)
sample1
 (Sample0 st0 ss0 vm0 pm0 ci0)
 (Sample1 bs1 vm1 vi1 cb1 cv1)
 (Sample2 di2 bm2) b2v v2b dn b0 =
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
 h bs0' cv0' = extend cv0' ((sub vm1 bs0'), (vecSolve dn ci0 (j bs0')))
 i bs0 = insert bs0 b0
 j bs0 = setMap k bs0
 k b0' = (0, sub cb0 b0')
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

