module SideGeo.Imply4 where

import SideGeo.Convert

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

