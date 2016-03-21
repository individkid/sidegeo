module SideGeo.Induce where

import SideGeo.Lambda
import SideGeo.Types
import SideGeo.Imply1
import SideGeo.Deduce

-- inducers
side1_s :: Side1 -> Boundary -> Region -> Sidedness
side1_s (Side1 dt ss) b r = setFind f ss where
 f s = member (sub2 dt s r) b
side2_s :: Side2 -> Boundary -> Region -> Sidedness
side2_s (Side2 ht ss) b r = setFind f ss where
 f s = member (sub2 ht s b) r
dual1_bs :: Dual1 -> Sidedness -> Region -> Boundaries
dual1_bs (Dual1 st bs _) s r = setFilter f bs where
 f b = (sub2 st b r) == s
half1_rs :: Half1 -> Sidedness -> Boundary -> Regions
half1_rs (Half1 st rs _) s b = setFilter f rs where
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
 h _ Nothing = False
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
side3_side0 :: Side3 -> Side0
side3_side0 (Side3 dt bs rs ss) = Side0 (fromSet2 (side1_s st1) bs rs) where
 st1 = Side1 dt ss
side4_side0 :: Side4 -> Side0
side4_side0 (Side4 ht bs rs ss) = Side0 (fromSet2 (side2_s st2) bs rs) where
 st2 = Side2 ht ss
dual2_dual0 :: Dual2 -> Dual0
dual2_dual0 (Dual2 st bs rs ss) = Dual0 (fromSet2 (dual1_bs dt1) ss rs) where
 dt1 = Dual1 st bs ss
dual3_dual0 :: Dual3 -> Dual0
dual3_dual0 (Dual3 di ss) = Dual0 (fromSet f ss) where
 f s = inverse (sub di s)
duali1_duali0 :: Duali1 -> Duali0
duali1_duali0 (Duali1 dt ss) = Duali0 (fromSet f ss) where
 f s = inverse (sub dt s)
duals1_duals0 :: Duals1 -> Duals0
duals1_duals0 (Duals1 st bs rs) = Duals0 (setMap f rs) where
 f r = fromSet (g r) bs; g r b = sub2 st b r
half2_half0 :: Half2 -> Half0
half2_half0 (Half2 st bs rs ss) = Half0 (fromSet2 (half1_rs ht1) ss bs) where
 ht1 = Half1 st rs ss
half3_half0 :: Half3 -> Half0
half3_half0 (Half3 hi ss) = Half0 (fromSet f ss) where
 f s = inverse (sub hi s)
halfi1_halfi0 :: Halfi1 -> Halfi0
halfi1_halfi0 (Halfi1 ht ss) = Halfi0 (fromSet f ss) where
 f s = inverse (sub ht s)
side0_bounds0 :: Side0 -> Bounds0
side0_bounds0 (Side0 st) = Bounds0 (keysSet st) where
 -- nothing
dual0_bounds0 :: Dual0 -> Bounds0
dual0_bounds0 (Dual0 dt) = Bounds0 (unions (valsSet2 dt)) where
 -- nothing
duali0_bounds0 :: Duali0 -> Bounds0
duali0_bounds0 (Duali0 di) = Bounds0 (unions (keysSet2 di)) where
 -- nothing
half0_bounds0 :: Half0 -> Bounds0
half0_bounds0 (Half0 ht) = Bounds0 (keysSet2 ht) where
 -- nothing
halfi0_bounds0 :: Halfi0 -> Bounds0
halfi0_bounds0 (Halfi0 hi) = Bounds0 (valsSet2 hi) where
 -- nothing
side0_regs0 :: Side0 -> Regs0
side0_regs0 (Side0 st) = Regs0 (keysSet2 st) where
 -- nothing
dual0_regs0 :: Dual0 -> Regs0
dual0_regs0 (Dual0 dt) = Regs0 (keysSet2 dt) where
 -- nothing
duali0_regs0 :: Duali0 -> Regs0
duali0_regs0 (Duali0 di) = Regs0 (valsSet2 di) where
 -- nothing
half0_regs0 :: Half0 -> Regs0
half0_regs0 (Half0 ht) = Regs0 (unions (valsSet2 ht)) where
 -- nothing
halfi0_regs0 :: Halfi0 -> Regs0
halfi0_regs0 (Halfi0 hi) = Regs0 (unions (keysSet2 hi)) where
 -- nothing
side0_sides0 :: Side0 -> Sides0
side0_sides0 (Side0 st) = Sides0 (valsSet2 st) where
 -- nothing
dual0_sides0 :: Dual0 -> Sides0
dual0_sides0 (Dual0 dt) = Sides0 (keysSet dt) where
 -- nothing
duali0_sides0 :: Duali0 -> Sides0
duali0_sides0 (Duali0 di) = Sides0 (keysSet di) where
 -- nothing
half0_sides0 :: Half0 -> Sides0
half0_sides0 (Half0 ht) = Sides0 (keysSet ht) where
 -- nothing
halfi0_sides0 :: Halfi0 -> Sides0
halfi0_sides0 (Halfi0 hi) = Sides0 (keysSet hi) where
 -- nothing
neighbor2_neighbor0 :: Neighbor2 -> Neighbor0
neighbor2_neighbor0 (Neighbor2 dt di bs rs ss) = Neighbor0 (fromOptSet2 (neighbor1_r nt1) bs rs) where
 nt1 = Neighbor1 dt di ss
attached2_attached0 :: Attached2 -> Attached0
attached2_attached0 (Attached2 st bs rs ss nt) = Attached0 (fromSet2 (attached1_rs at1) ss bs) where
 at1 = Attached1 st rs nt
flat2_flat0 :: Flat2 -> Flat0
flat2_flat0 (Flat2 bs ss at) = Flat0 (fromSet (flat1_rs am1) bs) where
 am1 = Flat1 ss at
shell2_shell0 :: Shell2 -> Shell0
shell2_shell0 (Shell2 st bs rs ss nt) = Shell0 (fromSet2 (shell1_bs bt1) ss rs) where
 bt1 = Shell1 st bs nt
cage2_cage0 :: Cage2 -> Cage0
cage2_cage0 (Cage2 rs ss bt) = Cage0 (fromSet (cage1_bs bm1) rs) where
 bm1 = Cage1 ss bt
disk2_disk0 :: Disk2 -> Disk0
disk2_disk0 (Disk2 rs ss nt bt) = Disk0 (fromSet2 (disk1_rs rt1) ss rs) where
 rt1 = Disk1 nt bt
blot2_blot0 :: Blot2 -> Blot0
blot2_blot0 (Blot2 rs ss rt) = Blot0 (fromSet (blot1_rs rm1) rs) where
 rm1 = Blot1 ss rt
vert1_vert0 :: Vert1 -> Vert0
vert1_vert0 (Vert1 dt di ht rs ss am bm) = Vert0 (valsMap Vertex (fromKeys s)) where
 s = unions (setMap f (setMap (sub bm) rs))
 f s' = setFilter g (setSets s' (setFromList [1..((setSize s')-1)]))
 g bs = setAll h (setMaps bs ss)
 f4 = Polycil0 dt di ht ss am
 h m = (setSize (polycil f4 m)) == 1
verti1_verti0 :: Verti1 -> Verti0
verti1_verti0 (Verti1 vm) = Verti0 (inverse vm) where
 -- nothing
verts1_verts0 :: Verts1 -> Verts0
verts1_verts0 (Verts1 vm) = Verts0 (valsSet vm) where
 -- nothing
verts2_verts0 :: Verts2 -> Verts0
verts2_verts0 (Verts2 vi) = Verts0 (keysSet vi) where
 -- nothing
pencil1_pencil0 :: Pencil1 -> Pencil0
pencil1_pencil0 (Pencil1 dt di ss am vi vs) = Pencil0 (fromSet f vs) where
 f2 = Pencil2 dt di ss am; f v = pencil f2 (sub vi v)
corner1_corner0 :: Corner1 -> Corner0
corner1_corner0 (Corner1 rs bm vm) = Corner0 (fromSet f rs) where
 f r = let bs = sub bm r in setOptMap (maybeSub vm) (setSets bs (setFromList [1..((setSize bs)-1)]))
inside1_inside0 :: Inside1 -> Inside0
inside1_inside0 (Inside1 dt di bs rs ss) = Inside0 (setFilter f rs) where
 n1 = (Neighbor1 dt di ss); f r = (colleague n1 bs r) == Nothing
outside1_outside0 :: Outside1 -> Outside0
outside1_outside0 (Outside1 rs is) = Outside0 (differ rs is) where
 -- nothing
signs1_signs0 :: Signs1 -> Signs0
signs1_signs0 (Signs1 dt di ht ss am vi vs tm) = Signs0 (signs1_gs s) where
 s = Signs1 dt di ht ss am vi vs tm
signb2_signb0 :: Signb2 -> Signb0
signb2_signb0 (Signb2 nt vi pm gs tm) = Signb0 (fromSet (signb1_gb s) gs) where
 s = Signb1 nt vi pm tm
signr2_signr0 :: Signr2 -> Signr0
signr2_signr0 (Signr2 st ss pm gs gb ti ts) = Signr0 (fromSet (signr1_gr s) gs) where
 s = Signr1 st ss pm gb ti ts
tope1_tope0 :: Tope1 -> Tope0
tope1_tope0 (Tope1 rs) = Tope0 (fromSet f rs) where
 f _ = zero
tope0_topei0 :: Tope0 -> Topei0
tope0_topei0 (Tope0 tm) = Topei0 (adverse tm) where
 -- nothing
tope0_topes0 :: Tope0 -> Topes0
tope0_topes0 (Tope0 tm) = Topes0 (valsSet tm) where
 -- nothing
topez0_topes0 :: Topez0 -> Topes0
topez0_topes0 (Topez0 tz) = Topes0 (unions (setMap keysSet tz)) where
 -- nothing

