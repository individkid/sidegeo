module Automatic where

import Types

-- return values
data Side0 = Side0 Side deriving (Show, Eq, Ord)
 -- st
data Dual0 = Dual0 Dual deriving (Show, Eq, Ord)
 -- dt
data Duali0 = Duali0 Duali deriving (Show, Eq, Ord)
 -- di
data Duals0 = Duals0 Duals deriving (Show, Eq, Ord)
 -- ds
data Half0 = Half0 Half deriving (Show, Eq, Ord)
 -- ht
data Halfi0 = Halfi0 Halfi deriving (Show, Eq, Ord)
 -- hi
data Bounds0 = Bounds0 Bounds deriving (Show, Eq, Ord)
 -- bs
data Regs0 = Regs0 Regs deriving (Show, Eq, Ord)
 -- rs
data Sides0 = Sides0 Sides deriving (Show, Eq, Ord)
 -- ss
data Neighbor0 = Neighbor0 Neighbor deriving (Show, Eq, Ord)
 -- nt
data Attached0 = Attached0 Attached deriving (Show, Eq, Ord)
 -- at
data Flat0 = Flat0 Flat deriving (Show, Eq, Ord)
 -- am
data Shell0 = Shell0 Shell deriving (Show, Eq, Ord)
 -- bt
data Cage0 = Cage0 Cage deriving (Show, Eq, Ord)
 -- bm
data Disk0 = Disk0 Disk deriving (Show, Eq, Ord)
 -- rt
data Blot0 = Blot0 Blot deriving (Show, Eq, Ord)
 -- rm
data Vert0 = Vert0 Vert deriving (Show, Eq, Ord)
 -- vm
data Verti0 = Verti0 Verti deriving (Show, Eq, Ord)
 -- vi
data Verts0 = Verts0 Verts deriving (Show, Eq, Ord)
 -- vs
data Pencil0 = Pencil0 Pencil deriving (Show, Eq, Ord)
 -- pm
data Corner0 = Corner0 Corner deriving (Show, Eq, Ord)
 -- qm
data Inside0 = Inside0 Inside deriving (Show, Eq, Ord)
 -- is
data Outside0 = Outside0 Outside deriving (Show, Eq, Ord)
 -- os
data Signs0 = Signs0 Signs deriving (Show, Eq, Ord)
 -- gs
data Signb0 = Signb0 Signb deriving (Show, Eq, Ord)
 -- gb
data Signr0 = Signr0 Signr deriving (Show, Eq, Ord)
 -- gr
data Tope0 = Tope0 Tope deriving (Show, Eq, Ord)
 -- tm
data Topei0 = Topei0 Topei deriving (Show, Eq, Ord)
 -- ti
data Topes0 = Topes0 Topes deriving (Show, Eq, Ord)
 -- ts
data Topez0 = Topez0 Topez deriving (Show, Eq, Ord)
 -- tz
data Plane0 = Plane0 Plane deriving (Show, Eq, Ord)
 -- cb
data Coplane0 = Coplane0 Coplane deriving (Show, Eq, Ord)
 -- cv
data Basis0 = Basis0 Basis deriving (Show, Eq, Ord)
 -- ci

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
supersection3_supersection1_b (Supersection3 st dt bs rs ss rm b) = (Supersection1 dt bs rs ss, b)
supersection4_subsection1_b (Supersection4 dt ss b) = (Subsection1 dt ss, b)
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
ovelaps0_tm_signs1 (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) tm = Signs1 dt di ht ss am vi vs tm
overlaps0_gs_tm_signb2 (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) gs tm = Signb2 nt vi pm gs tm
overlaps0_gs_gb_ti_ts_signr2 (Overlaps0 st dt di ht bs rs ss nt am vi vs pm) gs gb ti ts = Signr2 st ss pm gs gb ti ts
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
side0_tz_polytope1 (Side0 st) tz = Polytope1 dt bs ss ts tz where
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
side0_b_supersection4 (Side0 st) b = Supersection4 dt ss b where
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

