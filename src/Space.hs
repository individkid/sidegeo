module Space (
 Boundary,Region,Sidedness,Color,Space,
 side,bounds,regs,sides,color,rename,
 empty,order,system,simplex,
 subspace,superspace,spaces,overlaps) where

import Lambda
import Types
import Manual
import Automatic

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
  a4 = side0_tz_polytope1 a3 (i b s)
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
 ((sup1,b1),sup2) = chooseRemove (setMap f s2)
 f s2 = let
  (sub2,b2) = supersection4_subsection1_b s2
  sup1 = side0_supersection1 (subsection1 sub0 sub1 sub2) in
  (supersection1 sup0 sup1 b0, b2)
 s0' = side0_supersection2 rslt
 s1' = side0_b_supersection3 sup1 b1
 s2' = setMap g sup2
 g (s2,b2) = side0_b_supersection4 s2 b2 in
 supersection2 s0' s1' s2' where
 sup0 = supersection2_supersection0 s0
 (sup1,b0) = supersection3_supersection1_b s1
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
 arg1 = side0_b_supersection3 sect0 bound0
 arg2 = single (side0_b_supersection4 sect1 bound1)
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
 section = side0_b_supersection3 (section1 (Section0 st0 bs0 ss0 at0) chosen) chosen
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
  (Signs0 gs) = signs1_signs0 (ovelaps0_tm_signs1 s0 tm)
  (Signb0 gb) = signb2_signb0 (overlaps0_gs_tm_signb2 s0 gs tm)
  (Signr0 gr) = signr2_signr0 (overlaps0_gs_gb_ti_ts_signr2 s0 gs gb ti ts)
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
