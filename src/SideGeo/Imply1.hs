module SideGeo.Imply1 where

import SideGeo.Types

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

-- parameters for deducers
data Take0 = Take0 Dual Bounds Sides deriving (Show, Eq, Ord)
 -- dt bs ss
data Take1 = Take1 Dual Bounds Regs Sides deriving (Show, Eq, Ord)
 -- dt bs rs ss
data Polycil0 = Polycil0 Dual Duali Half Sides Flat deriving (Show, Eq, Ord)
 -- dt di ht ss am

