module SideGeo.Implicit2 where

import SideGeo.Types
import SideGeo.Implicit1
import SideGeo.Induce

-- parameters for constructors
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
data Sample1 = Sample1 Bounds Vert Verti Plane Coplane deriving (Show, Eq, Ord)
 -- bs vm vi vs cb cv
data Sample2 = Sample2 Duali Cage deriving (Show, Eq, Ord)
 -- di bm

-- selectors
section0_side0 :: Section0 -> Side0
section0_side0 (Section0 st _ _ _) = Side0 st where
 -- nothing
subsection0_subspace0 :: Subsection0 -> Subspace0
subsection0_subspace0 (Subsection0 _ dt _ ss _) = Subspace0 dt ss where
 -- nothing
subsection0_section0 :: Subsection0 -> Section0
subsection0_section0 (Subsection0 st _ bs ss at) = Section0 st bs ss at where
 -- nothing
subsection0_bs :: Subsection0 -> Bounds
subsection0_bs (Subsection0 _ _ bs _ _) = bs where
 -- nothing
subsection0_side0 :: Subsection0 -> Side0
subsection0_side0 (Subsection0 st _ _ _ _) = Side0 st where
 -- nothing
subsection1_subspace0 :: Subsection1 -> Subspace0
subsection1_subspace0 (Subsection1 dt ss) = Subspace0 dt ss where
 -- nothing
supersection2_subsection0 :: Supersection2 -> Subsection0
supersection2_subsection0 (Supersection2 st dt bs _ ss at _) = Subsection0 st dt bs ss at where
 -- nothing
supersection2_supersection0 :: Supersection2 -> Supersection0
supersection2_supersection0 (Supersection2 st dt bs rs ss _ rm) = Supersection0 st dt bs rs ss rm where
 -- nothing
supersection3_subsection1 :: Supersection3 -> Subsection1
supersection3_subsection1 (Supersection3 _ dt _ _ ss _ _) = Subsection1 dt ss where
 -- nothing
supersection3_supersection0 :: Supersection3 -> Supersection0
supersection3_supersection0 (Supersection3 st dt bs rs ss rm _) = Supersection0 st dt bs rs ss rm where
 -- nothing
supersection3_supersection1_b :: Supersection3 -> (Supersection1, Boundary)
supersection3_supersection1_b (Supersection3 _ dt bs rs ss _ b) = (Supersection1 dt bs rs ss, b) where
 -- nothing
supersection4_subsection1_b :: Supersection4 -> (Subsection1, Boundary)
supersection4_subsection1_b (Supersection4 dt ss b) = (Subsection1 dt ss, b) where
 -- nothing
migrate0_side0 :: Migrate0 -> Side0
migrate0_side0 (Migrate0 st _ _ _ _) = Side0 st where
 -- nothing
spaces0_side0 :: Spaces0 -> Side0
spaces0_side0 (Spaces0 st _ _ _ _ _ _ _ _) = Side0 st where
 -- nothing
spaces0_bs :: Spaces0 -> Bounds
spaces0_bs (Spaces0 _ _ bs _ _ _ _ _ _) = bs where
 -- nothing
spaces0_cospace0 :: Spaces0 -> Cospace0
spaces0_cospace0 (Spaces0 st dt bs rs ss at rm vi qm) = Cospace0 st dt bs rs ss at rm vi qm where
 -- nothing
spaces0_supersection0 :: Spaces0 -> Supersection0
spaces0_supersection0 (Spaces0 st dt bs rs ss _ rm _ _) = Supersection0 st dt bs rs ss rm where
 -- nothing
spaces0_cospace1 :: Spaces0 -> Cospace1
spaces0_cospace1 (Spaces0 st _ bs rs _ _ _ _ qm) = Cospace1 st bs rs qm where
 -- nothing
spaces1_cospace2 :: Spaces1 -> Cospace2
spaces1_cospace2 (Spaces1 dt _ ss _) = Cospace2 dt ss where
 -- nothing
spaces1_vs :: Spaces1 -> Verts
spaces1_vs (Spaces1 _ _ _ vs) = vs where
 -- nothing
spaces1_rs :: Spaces1 -> Regs
spaces1_rs (Spaces1 _ rs _ _) = rs where
 -- nothing
polytope0_signr0 :: Polytope0 -> Signr0
polytope0_signr0 (Polytope0 gr) = Signr0 gr where
 -- nothing
polytope1_sides0 :: Polytope1 -> Sides0
polytope1_sides0 (Polytope1 _ _ ss _ _) = Sides0 ss where
 -- nothing
polytope1_superspace0 :: Polytope1 -> Superspace0
polytope1_superspace0 (Polytope1 dt bs ss _ _) = Superspace0 dt bs ss where
 -- nothing
polytope1_ts :: Polytope1 -> Topes
polytope1_ts (Polytope1 _ _ _ ts _) = ts where
 -- nothing
polytope1_tz :: Polytope1 -> Topez
polytope1_tz (Polytope1 _ _ _ _ tz) = tz where
 -- nothing
polytope2_st :: Polytope2 -> Side
polytope2_st (Polytope2 st _ _ _ _) = st where
 -- nothing
polytope2_rs :: Polytope2 -> Regs
polytope2_rs (Polytope2 _ _ _ rs _) = rs where
 -- nothing
polytope2_take1 :: Polytope2 -> Take1
polytope2_take1 (Polytope2 _ dt bs rs ss) = Take1 dt bs rs ss where
 -- nothing
polytope3_take1 :: Polytope3 -> Take1
polytope3_take1 (Polytope3 dt bs rs ss) = Take1 dt bs rs ss where
 -- nothing
polytope3_rs :: Polytope3 -> Regs
polytope3_rs (Polytope3 _ _ rs _) = rs where
 -- nothing
polytope4_section0 :: Polytope4 -> Section0
polytope4_section0 (Polytope4 st _ bs ss at _) = Section0 st bs ss at where
 -- nothing
polytope4_take0 :: Polytope4 -> Take0
polytope4_take0 (Polytope4 _ dt bs ss _ _) = Take0 dt bs ss where
 -- nothing
polytope4_at :: Polytope4 -> Attached
polytope4_at (Polytope4 _ _ _ _ at _) = at where
 -- nothing
polytope4_rm :: Polytope4 -> Blot
polytope4_rm (Polytope4 _ _ _ _ _ rm) = rm where
 -- nothing
overlaps0_subspace0 :: Overlaps0 -> Subspace0
overlaps0_subspace0 (Overlaps0 _ dt _ _ _ _ ss _ _ _ _ _) = Subspace0 dt ss where
 -- nothing
overlaps0_take1 :: Overlaps0 -> Take1
overlaps0_take1 (Overlaps0 _ dt _ _ bs rs ss _ _ _ _ _) = Take1 dt bs rs ss where
 -- nothing
ovelaps0_tm_signs1 :: Overlaps0 -> Tope -> Signs1
ovelaps0_tm_signs1 (Overlaps0 _ dt di ht _ _ ss _ am vi vs _) tm = Signs1 dt di ht ss am vi vs tm where
 -- nothing
overlaps0_gs_tm_signb2 :: Overlaps0 -> Signs -> Tope -> Signb2
overlaps0_gs_tm_signb2 (Overlaps0 _ _ _ _ _ _ _ nt _ vi _ pm) gs tm = Signb2 nt vi pm gs tm where
 -- nothing
overlaps0_gs_gb_ti_ts_signr2 :: Overlaps0 -> Signs -> Signb -> Topei -> Topes -> Signr2
overlaps0_gs_gb_ti_ts_signr2 (Overlaps0 st _ _ _ _ _ ss _ _ _ _ pm) gs gb ti ts = Signr2 st ss pm gs gb ti ts where
 -- nothing
overlaps0_bs :: Overlaps0 -> Bounds
overlaps0_bs (Overlaps0 _ _ _ _ bs _ _ _ _ _ _ _) = bs where
 -- nothing
overlaps0_rs :: Overlaps0 -> Regs
overlaps0_rs (Overlaps0 _ _ _ _ _ rs _ _ _ _ _ _) = rs where
 -- nothing
overlaps1_take0 :: Overlaps1 -> Take0
overlaps1_take0 (Overlaps1 dt bs ss _) = Take0 dt bs ss where
 -- nothing
overlaps1_is :: Overlaps1 -> Inside
overlaps1_is (Overlaps1 _ _ _ is) = is where
 -- nothing

-- converters
duali0_subsection0 :: Duali0 -> Subsection0
duali0_subsection0 (Duali0 di) = Subsection0 st dt bs ss at where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
duali0_subsection1 :: Duali0 -> Subsection1
duali0_subsection1 (Duali0 di) = Subsection1 dt ss where
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
duali0_polytope2 :: Duali0 -> Polytope2
duali0_polytope2 (Duali0 di) = Polytope2 st dt bs rs ss where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
duali0_overlaps1 :: Duali0 -> Overlaps1
duali0_overlaps1 (Duali0 di) = Overlaps1 dt bs ss is where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Inside0 is) = inside1_inside0 (Inside1 dt di bs rs ss)
side0_overlaps0 :: Side0 -> Overlaps0
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
side0_subsection1 :: Side0 -> Subsection1
side0_subsection1 (Side0 st) = Subsection1 dt ss where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
side0_supersection0 :: Side0 -> Supersection0
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
side0_supersection1 :: Side0 -> Supersection1
side0_supersection1 (Side0 st) = Supersection1 dt bs rs ss where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
side0_migrate0 :: Side0 -> Migrate0
side0_migrate0 (Side0 st) = Migrate0 st bs rs ss bm where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
side0_cospace3 :: Side0 -> Cospace3
side0_cospace3 (Side0 st) = Cospace3 st dt bs rs ss bm where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Cage0 bm) = cage2_cage0 (Cage2 rs ss bt)
side0_cospace6 :: Side0 -> Cospace6
side0_cospace6 (Side0 st) = Cospace6 dt bs rs ss where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
side0_duals1 :: Side0 -> Duals1
side0_duals1 (Side0 st) = Duals1 st bs rs where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
side0_spaces0 :: Side0 -> Spaces0
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
side0_tz_polytope1 :: Side0 -> Topez -> Polytope1
side0_tz_polytope1 (Side0 st) tz = Polytope1 dt bs ss ts tz where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Topes0 ts) = topez0_topes0 (Topez0 tz)
dual0_polytope4 :: Dual0 -> Polytope4
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
dual0_supersection0 :: Dual0 -> Supersection0
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
dual0_supersection1 :: Dual0 -> Supersection1
dual0_supersection1 (Dual0 dt) = Supersection1 dt bs rs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
dual0_spaces1 :: Dual0 -> Spaces1
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
dual0_polytope3 :: Dual0 -> Polytope3
dual0_polytope3 (Dual0 dt) = Polytope3 dt bs rs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
side0_section0 :: Side0 -> Section0
side0_section0 (Side0 st) = Section0 st bs ss at where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
side0_b_supersection3 :: Side0 -> Boundary -> Supersection3
side0_b_supersection3 (Side0 st) b = Supersection3 st dt bs rs ss rm b where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
 (Duali0 di) = duali1_duali0 (Duali1 dt ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Shell0 bt) = shell2_shell0 (Shell2 st bs rs ss nt)
 (Disk0 rt) = disk2_disk0 (Disk2 rs ss nt bt)
 (Blot0 rm) = blot2_blot0 (Blot2 rs ss rt)
side0_b_supersection4 :: Side0 -> Boundary -> Supersection4
side0_b_supersection4 (Side0 st) b = Supersection4 dt ss b where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
side0_supersection2 :: Side0 -> Supersection2
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
duali0_section0 :: Duali0 -> Section0
duali0_section0 (Duali0 di) = Section0 st bs ss at where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
 (Neighbor0 nt) = neighbor2_neighbor0 (Neighbor2 dt di bs rs ss)
 (Attached0 at) = attached2_attached0 (Attached2 st bs rs ss nt)
duali0_supersection2 :: Duali0 -> Supersection2
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
duali0_cospace5 :: Duali0 -> Cospace5
duali0_cospace5 (Duali0 di) = Cospace5 dt bs ss is where
 (Bounds0 bs) = duali0_bounds0 (Duali0 di)
 (Regs0 rs) = duali0_regs0 (Duali0 di)
 (Sides0 ss) = duali0_sides0 (Duali0 di)
 (Dual0 dt) = dual3_dual0 (Dual3 di ss)
 (Inside0 is) = inside1_inside0 (Inside1 dt di bs rs ss)
side0_superspace0 :: Side0 -> Superspace0
side0_superspace0 (Side0 st) = Superspace0 dt bs ss where
 (Bounds0 bs) = side0_bounds0 (Side0 st)
 (Regs0 rs) = side0_regs0 (Side0 st)
 (Sides0 ss) = side0_sides0 (Side0 st)
 (Dual0 dt) = dual2_dual0 (Dual2 st bs rs ss)
dual0_superspace0 :: Dual0 -> Superspace0
dual0_superspace0 (Dual0 dt) = Superspace0 dt bs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
dual0_surspace0 :: Dual0 -> Surspace0
dual0_surspace0 (Dual0 dt) = Surspace0 st bs rs where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)
dual0_take1 :: Dual0 -> Take1
dual0_take1 (Dual0 dt) = Take1 dt bs rs ss where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Sides0 ss) = dual0_sides0 (Dual0 dt)

-- unconverter, for rare case only
subsection1_side0 :: Subsection1 -> Side0
subsection1_side0 (Subsection1 dt ss) = Side0 st where
 (Bounds0 bs) = dual0_bounds0 (Dual0 dt)
 (Regs0 rs) = dual0_regs0 (Dual0 dt)
 (Side0 st) = side3_side0 (Side3 dt bs rs ss)

