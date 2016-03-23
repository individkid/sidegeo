module SideGeo.Space (
 Boundary,Region,Sidedness,Color,Space,
 side,bounds,regs,sides,color,rename,
 empty,order,system,simplex,
 subspace,superspace,spaces,overlaps) where

import SideGeo.Container
import SideGeo.Lambda
import SideGeo.Types
import SideGeo.Implicit1
import SideGeo.Deduce
import SideGeo.Kernel
import SideGeo.Polytope
import SideGeo.Implicit3
import SideGeo.Convert
import SideGeo.Implicit4

side :: Space -> Boundary -> Region -> (Sidedness,Space)
side s' b' r' = (sub2 st b' r', s) where
 ([SideRep st], s) = convert s' [SideTag]
bounds :: Space -> ([Boundary],Space)
bounds s' = (setToList bs, s) where
 ([BoundsRep bs], s) = convert s' [BoundsTag]
regs :: Space -> ([Region],Space)
regs s' = (setToList rs, s) where
 ([RegsRep rs], s) = convert s' [RegsTag]
sides :: Space -> ([Sidedness],Space)
sides s' = (setToList ss, s) where
 ([SidesRep ss], s) = convert s' [SidesTag]
color :: Space -> Region -> (Color,Space)
color s' r' = (sub tm r', s) where
 ([TopeRep tm], s) = convert s' [TopeTag]
rename :: Space -> Space -> [Region] -> ([Region],Space,Space)
rename s0' s1' a' = (setToList rs,s0,s1) where
 rs = retake (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) (setFromList a')
 ([DualRep dt0, BoundsRep bs0, SidesRep ss0], s0) = convert s0' [DualTag,BoundsTag,SidesTag]
 ([DualRep dt1, BoundsRep bs1, RegsRep rs1, SidesRep ss1], s1) = convert s1' [DualTag,BoundsTag,RegsTag,SidesTag]

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
 h r' take0' (take1',tm') = sub tm' (choose (retake take0' take1' (single r')))
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
