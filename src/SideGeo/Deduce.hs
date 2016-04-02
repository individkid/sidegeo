module SideGeo.Deduce where

import SideGeo.Lambda
import SideGeo.Types
import SideGeo.Implicit1

-- deducers
polyant :: Half0 -> Direction -> Regions
polyant (Half0 ht) b2s = intersects (setMap f (keysSet b2s)) where
 f b = sub2 ht (sub b2s b) b
colleague :: Neighbor1 -> Boundaries -> Region -> Maybe Region
colleague (Neighbor1 dt di ss) bs r = maybeSub2 di s (symmetric bs (sub2 dt s r)) where
 s = choose ss
pencil :: Pencil2 -> Boundaries -> Regions
pencil (Pencil2 dt di ss am) bs = setFilter f (intersects (setMap (sub am) bs)) where
 n1 = Neighbor1 dt di ss
 f r = (colleague n1 bs r) /= Nothing
polycil :: Polycil0 -> Direction -> Regions
polycil (Polycil0 dt di ht ss am) b2s = intersect rs0 rs1 where
 rs0 = (polyant (Half0 ht) b2s)
 rs1 = (pencil (Pencil2 dt di ss am) (keysSet b2s))
connect :: Blot0 -> Regions -> Maybe Region -> Regions
connect (Blot0 _) _ Nothing = setEmpty
connect (Blot0 rm) rs (Just r) = setConnect f r where
 f r' = intersect (sub rm r') rs
retake :: Take0 -> Take1 -> Regions -> Regions -- take Regions from take0 to take1
retake (Take0 dt0 bs0 ss0) (Take1 dt1 bs1 rs1 ss1) rs0 = unions (setMap f rs0) where
 s = choose (intersect ss0 ss1)
 bs = intersect bs0 bs1
 f r0 = setFilter (g r0) rs1
 g r0 r1 = (intersect bs (sub2 dt0 s r0)) == (intersect bs (sub2 dt1 s r1))
