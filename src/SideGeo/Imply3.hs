module SideGeo.Imply3 where

import SideGeo.Types
import SideGeo.Imply1
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
