module SideGeo.Imply4 where

import SideGeo.Types
import SideGeo.Imply1
import SideGeo.Imply2
import SideGeo.Imply3
import SideGeo.Convert

st_space :: Side -> Space
st_space st = space SideTag (SideRep st) where
 -- nothing
dt_space :: Dual -> Space
dt_space dt = space DualTag (DualRep dt) where
 -- nothing
di_space :: Duali -> Space
di_space di = space DualiTag (DualiRep di) where
 -- nothing
tz_space :: Topez -> Space
tz_space tz = space TopezTag (TopezRep tz) where
 -- nothing
side0_space :: Side0 -> Space
side0_space (Side0 st) = st_space st where
 -- nothing
dual0_space :: Dual0 -> Space
dual0_space (Dual0 dt) = dt_space dt where
 -- nothing
duali0_space :: Duali0 -> Space
duali0_space (Duali0 di) = di_space di where
 -- nothing
topez0_space :: Topez0 -> Space
topez0_space (Topez0 tz) = tz_space tz where
 -- nothing

space_subspace0 :: Space -> (Subspace0,Space)
space_subspace0 s0 = (Subspace0 dt ss, s1) where
 ([DualRep dt, SidesRep ss], s1) =
  convert s0 [DualTag, SidesTag]
space_superspace2 :: Space -> (Superspace0,Space)
space_superspace2 s0 = (Superspace0 dt bs ss, s1) where
 ([DualRep dt, BoundsRep bs, SidesRep ss], s1) =
  convert s0 [DualTag, BoundsTag, SidesTag]
space_take0 :: Space -> (Take0,Space)
space_take0 s0 = (Take0 dt bs ss, s1) where
 ([DualRep dt, BoundsRep bs, SidesRep ss], s1) =
  convert s0 [DualTag, BoundsTag, SidesTag]
space_take1 :: Space -> (Take1,Space)
space_take1 s0 = (Take1 dt bs rs ss, s1) where
 ([DualRep dt, BoundsRep bs, RegsRep rs, SidesRep ss], s1) =
  convert s0 [DualTag, BoundsTag, RegsTag, SidesTag]
space_rs :: Space -> (Regs,Space)
space_rs s0 = (rs,s1) where
 ([RegsRep rs], s1) = convert s0 [RegsTag]
space_tm :: Space -> (Tope,Space)
space_tm s0 = (tm,s1) where
 ([TopeRep tm], s1) = convert s0 [TopeTag]

