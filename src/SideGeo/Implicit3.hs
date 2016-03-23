module SideGeo.Implicit3 where

import SideGeo.Types
import SideGeo.Implicit1
import SideGeo.Induce

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

side0_rep :: Side0 -> Rep
side0_rep (Side0 st) = SideRep st where
 -- nothing
dual0_rep :: Dual0 -> Rep
dual0_rep (Dual0 dt) = DualRep dt where
 -- nothing
duali0_rep :: Duali0 -> Rep
duali0_rep (Duali0 di) = DualiRep di where
 -- nothing
duals0_rep :: Duals0 -> Rep
duals0_rep (Duals0 ds) = DualsRep ds where
 -- nothing
half0_rep :: Half0 -> Rep
half0_rep (Half0 ht) = HalfRep ht where
 -- nothing
halfi0_rep :: Halfi0 -> Rep
halfi0_rep (Halfi0 hi) = HalfiRep hi where
 -- nothing
bounds0_rep :: Bounds0 -> Rep
bounds0_rep (Bounds0 bs) = BoundsRep bs where
 -- nothing
regs0_rep :: Regs0 -> Rep
regs0_rep (Regs0 rs) = RegsRep rs where
 -- nothing
sides0_rep :: Sides0 -> Rep
sides0_rep (Sides0 ss) = SidesRep ss where
 -- nothing
neighbor0_rep :: Neighbor0 -> Rep
neighbor0_rep (Neighbor0 nt) = NeighborRep nt where
 -- nothing
attached0_rep :: Attached0 -> Rep
attached0_rep (Attached0 at) = AttachedRep at where
 -- nothing
flat0_rep :: Flat0 -> Rep
flat0_rep (Flat0 am) = FlatRep am where
 -- nothing
shell0_rep :: Shell0 -> Rep
shell0_rep (Shell0 bt) = ShellRep bt where
 -- nothing
cage0_rep :: Cage0 -> Rep
cage0_rep (Cage0 bm) = CageRep bm where
 -- nothing
disk0_rep :: Disk0 -> Rep
disk0_rep (Disk0 rt) = DiskRep rt where
 -- nothing
blot0_rep :: Blot0 -> Rep
blot0_rep (Blot0 rm) = BlotRep rm where
 -- nothing
vert0_rep :: Vert0 -> Rep
vert0_rep (Vert0 vm) = VertRep vm where
 -- nothing
verti0_rep :: Verti0 -> Rep
verti0_rep (Verti0 vi) = VertiRep vi where
 -- nothing
verts0_rep :: Verts0 -> Rep
verts0_rep (Verts0 vs) = VertsRep vs where
 -- nothing
pencil0_rep :: Pencil0 -> Rep
pencil0_rep (Pencil0 pm) = PencilRep pm where
 -- nothing
corner0_rep :: Corner0 -> Rep
corner0_rep (Corner0 qm) = CornerRep qm where
 -- nothing
inside0_rep :: Inside0 -> Rep
inside0_rep (Inside0 is) = InsideRep is where
 -- nothing
outside0_rep :: Outside0 -> Rep
outside0_rep (Outside0 os) = OutsideRep os where
 -- nothing
signs0_rep :: Signs0 -> Rep
signs0_rep (Signs0 gs) = SignsRep gs where
 -- nothing
signb0_rep :: Signb0 -> Rep
signb0_rep (Signb0 gb) = SignbRep gb where
 -- nothing
signr0_rep :: Signr0 -> Rep
signr0_rep (Signr0 gr) = SignrRep gr where
 -- nothing
tope0_rep :: Tope0 -> Rep
tope0_rep (Tope0 tm) = TopeRep tm where
 -- nothing
topei0_rep :: Topei0 -> Rep
topei0_rep (Topei0 ti) = TopeiRep ti where
 -- nothing
topes0_rep :: Topes0 -> Rep
topes0_rep (Topes0 ts) = TopesRep ts where
 -- nothing

reps_side0 :: [Rep] -> Side0
reps_side0 [SideRep st] = Side0 st where
reps_side0 _ = error "oops"
 -- nothing
reps_side3 :: [Rep] -> Side3
reps_side3 [DualRep dt, BoundsRep bs, RegsRep rs, SidesRep ss] = Side3 dt bs rs ss where
reps_side3 _ = error "oops"
 -- nothing
reps_side4 :: [Rep] -> Side4
reps_side4 [HalfRep ht, BoundsRep bs, RegsRep rs, SidesRep ss] = Side4 ht bs rs ss where
reps_side4 _ = error "oops"
 -- nothing
reps_dual0 :: [Rep] -> Dual0
reps_dual0 [DualRep dt] = Dual0 dt where
reps_dual0 _ = error "oops"
 -- nothing
reps_dual2 :: [Rep] -> Dual2
reps_dual2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss] = Dual2 st bs rs ss where
reps_dual2 _ = error "oops"
 -- nothing
reps_dual3 :: [Rep] -> Dual3
reps_dual3 [DualiRep di, SidesRep ss] = Dual3 di ss where
reps_dual3 _ = error "oops"
 -- nothing
reps_duali0 :: [Rep] -> Duali0
reps_duali0 [DualiRep di] = Duali0 di where
reps_duali0 _ = error "oops"
 -- nothing
reps_duali1 :: [Rep] -> Duali1
reps_duali1 [DualRep dt, SidesRep ss] = Duali1 dt ss where
reps_duali1 _ = error "oops"
 -- nothing
reps_duals1 :: [Rep] -> Duals1
reps_duals1 [SideRep st, BoundsRep bs, RegsRep rs] = Duals1 st bs rs where
reps_duals1 _ = error "oops"
 -- nothing
reps_half0 :: [Rep] -> Half0
reps_half0 [HalfRep ht] = Half0 ht where
reps_half0 _ = error "oops"
 -- nothing
reps_half2 :: [Rep] -> Half2
reps_half2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss] = Half2 st bs rs ss where
reps_half2 _ = error "oops"
 -- nothing
reps_half3 :: [Rep] -> Half3
reps_half3 [HalfiRep hi, SidesRep ss] = Half3 hi ss where
reps_half3 _ = error "oops"
 -- nothing
reps_halfi0 :: [Rep] -> Halfi0
reps_halfi0 [HalfiRep hi] = Halfi0 hi where
reps_halfi0 _ = error "oops"
 -- nothing
reps_halfi1 :: [Rep] -> Halfi1
reps_halfi1 [HalfRep ht, SidesRep ss] = Halfi1 ht ss where
reps_halfi1 _ = error "oops"
 -- nothing
reps_neighbor2 :: [Rep] -> Neighbor2
reps_neighbor2 [DualRep dt, DualiRep di, BoundsRep bs, RegsRep rs, SidesRep ss] = Neighbor2 dt di bs rs ss where
reps_neighbor2 _ = error "oops"
 -- nothing
reps_attached2 :: [Rep] -> Attached2
reps_attached2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss, NeighborRep nt] = Attached2 st bs rs ss nt where
reps_attached2 _ = error "oops"
 -- nothing
reps_flat2 :: [Rep] -> Flat2
reps_flat2 [BoundsRep bs, SidesRep ss, AttachedRep at] = Flat2 bs ss at where
reps_flat2 _ = error "oops"
 -- nothing
reps_shell2 :: [Rep] -> Shell2
reps_shell2 [SideRep st, BoundsRep bs, RegsRep rs, SidesRep ss, NeighborRep nt] = Shell2 st bs rs ss nt where
reps_shell2 _ = error "oops"
 -- nothing
reps_cage2 :: [Rep] -> Cage2
reps_cage2 [RegsRep rs, SidesRep ss, ShellRep bt] = Cage2 rs ss bt where
reps_cage2 _ = error "oops"
 -- nothing
reps_disk2 :: [Rep] -> Disk2
reps_disk2 [RegsRep rs, SidesRep ss, NeighborRep nt, ShellRep bt] = Disk2 rs ss nt bt where
reps_disk2 _ = error "oops"
 -- nothing
reps_blot2 :: [Rep] -> Blot2
reps_blot2 [RegsRep rs, SidesRep ss, DiskRep rt] = Blot2 rs ss rt where
reps_blot2 _ = error "oops"
 -- nothing
reps_vert1 :: [Rep] -> Vert1
reps_vert1 [DualRep dt, DualiRep di, HalfRep ht, RegsRep rs, SidesRep ss, FlatRep am, CageRep bm] = Vert1 dt di ht rs ss am bm where
reps_vert1 _ = error "oops"
 -- nothing
reps_verti1 :: [Rep] -> Verti1
reps_verti1 [VertRep vm] = Verti1 vm where
reps_verti1 _ = error "oops"
 -- nothing
reps_verts1 :: [Rep] -> Verts1
reps_verts1 [VertRep vm] = Verts1 vm where
reps_verts1 _ = error "oops"
 -- nothing
reps_verts2 :: [Rep] -> Verts2
reps_verts2 [VertiRep vi] = Verts2 vi where
reps_verts2 _ = error "oops"
 -- nothing
reps_pencil1 :: [Rep] -> Pencil1
reps_pencil1 [DualRep dt, DualiRep di, SidesRep ss, FlatRep am, VertiRep vi, VertsRep vs] = Pencil1 dt di ss am vi vs where
reps_pencil1 _ = error "oops"
 -- nothing
reps_corner1 :: [Rep] -> Corner1
reps_corner1 [RegsRep rs, CageRep bm, VertRep vm] = Corner1 rs bm vm where
reps_corner1 _ = error "oops"
 -- nothing
reps_inside1 :: [Rep] -> Inside1
reps_inside1 [DualRep dt, DualiRep di, BoundsRep bs, RegsRep rs, SidesRep ss] = Inside1 dt di bs rs ss where
reps_inside1 _ = error "oops"
 -- nothing
reps_outside1 :: [Rep] -> Outside1
reps_outside1 [RegsRep rs, InsideRep is] = Outside1 rs is where
reps_outside1 _ = error "oops"
 -- nothing
reps_signs1 :: [Rep] -> Signs1
reps_signs1 [DualRep dt, DualiRep di, HalfRep ht, SidesRep ss, FlatRep am, VertiRep vi, VertsRep vs, TopeRep tm] = Signs1 dt di ht ss am vi vs tm where
reps_signs1 _ = error "oops"
 -- nothing
reps_signb2 :: [Rep] -> Signb2
reps_signb2 [NeighborRep nt, VertiRep vi, PencilRep pm, SignsRep gs, TopeRep tm] = Signb2 nt vi pm gs tm where
reps_signb2 _ = error "oops"
 -- nothing
reps_signr2 :: [Rep] -> Signr2
reps_signr2 [SideRep st, SidesRep ss, PencilRep pm, SignsRep gs, SignbRep gb, TopeiRep ti, TopesRep ts] = Signr2 st ss pm gs gb ti ts where
reps_signr2 _ = error "oops"
 -- nothing
reps_tope0 :: [Rep] -> Tope0
reps_tope0 [TopeRep tm] = Tope0 tm where
reps_tope0 _ = error "oops"
 -- nothing
reps_tope1 :: [Rep] -> Tope1
reps_tope1 [RegsRep rs] = Tope1 rs where
reps_tope1 _ = error "oops"
 -- nothing

-- conversions is list of tuples of space to space converter, tags converted from, tags converted to
conversions :: [(Tag, [([Rep] -> Rep, [Tag])])]
conversions = [
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
