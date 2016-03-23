module SideGeo.Polytope where

import SideGeo.Container
import SideGeo.Lambda
import SideGeo.Types
import SideGeo.Implicit1
import SideGeo.Induce
import SideGeo.Deduce
import SideGeo.Implicit2
import SideGeo.Equivalent
import SideGeo.Kernel

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
 tz :: Polytope
 tz = polytope1_tz s
 f0 :: Color -> Regions
 f0 c = setConnect2 (f (sub exclude c)) (sub include c)
 f :: Regions -> Region -> Regions
 f rs r = differ (sub rm r) rs
 s1 = setMap g tz
 g :: Rainbow -> Dual0
 g a = let
  ms' = unions (valsSet a)
  ss' = unions (setMap valsSet ms')
  rs' = setFromList [zero..(Region ((setSize ms')-1))] in
  g0 rs' ss' ms'
 -- surspace of indicated regions from vertex pencil
 g0 :: Regions -> Sides -> Directions -> Dual0
 g0 rs' ss' ds' = Dual0 (fromSet2 f' ss' rs') where
  t' = fromKeysVals rs' ds'
  f' s' r' = let m' = (sub t' r') in setFilter (g' m' s') (keysSet m')
  g' m' s' b' = (sub m' b') == s'
 s2 = setMap dual0_superspace0 s1
 s3 = polytope1_superspace0 s
 s4 = forceInsert s2 s3
 s5 = superspace2 s4
 s6 = dual0_polytope4 s5
 ms :: Directions
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
 h b' s' = let
  a3 = section1 a0 b'
  a4 = side0_tz_polytope1 a3 (i b' s')
  (dt', Topei0 ti') = embed a4 -- embedding in side of boundary
  a5 = dual0_polytope3 dt'
  a6 = polytope3_take1 a5
  a7 = polytope3_rs a5
  incl' = valsMap (retake a1 a6) ti'
  excl' = valsMap ((retake a1 a6) . (differ a7)) ti'
  ude' = sub2 a2 s' b' in
  (valsMap (intersect ude') incl' , valsMap (intersect ude') excl')
 i :: Boundary -> Sidedness -> Polytope
 i b' s' = setMap (j b' s') tz
 j :: Boundary -> Sidedness -> Rainbow -> Rainbow
 j b' s' m' = valsMap (k b' s') m'
 k :: Boundary -> Sidedness -> Directions -> Directions
 k b' s' m' = setFilter k0 (setMap (k1 b') (setFilter (k2 b' s') m'))
 k0 :: Direction -> Bool
 k0 m' = (mapSize m') /= 0
 k1 :: Boundary -> Direction -> Direction
 k1 b' m' = restrict m' (remove (keysSet m') b')
 k2 :: Boundary -> Sidedness -> Direction -> Bool
 k2 b' s' m' = (maybeSub m' b') == (Just s')

-- all overlaps embedded in space
overlaps1 :: Overlaps0 -> Color -> Color -> Set Topez0
overlaps1 s0 c0 c1 = valsSet (fromSet f rz) where
 f :: Regions -> Topez0
 f rs' = setFold2 (g p) (equitope p p) p where
  p = polytope s1
  tm = fromSet f0 rs'
  (Topei0 ti) = tope0_topei0 (Tope0 tm)
  (Topes0 ts) = tope0_topes0 (Tope0 tm)
  s1 = Polytope0 gr
  (Signs0 gs) = signs1_signs0 (ovelaps0_tm_signs1 s0 tm)
  (Signb0 gb) = signb2_signb0 (overlaps0_gs_tm_signb2 s0 gs tm)
  (Signr0 gr) = signr2_signr0 (overlaps0_gs_gb_ti_ts_signr2 s0 gs gb ti ts)
 f0 :: Region -> Color
 f0 r = if member rs r then c1 else c0
 g :: Topez0 -> Symmetry -> Topez0 -> Topez0
 g (Topez0 p) m (Topez0 p0) = let
  p1 = setMap (valsMap (setMap (mapMap (h m)))) p in
  if p0 < p1 then Topez0 p0 else Topez0 p1
 h :: Symmetry -> (Boundary,Sidedness) -> (Boundary,Sidedness)
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
 f c0' c1' s' = overlaps1 s' c0' c1'
 a = spaces2 dn (dn + 1)
 b = setMap side0_overlaps0 a
