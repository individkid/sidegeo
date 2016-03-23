module SideGeo.Equivalent where

import SideGeo.Lambda
import SideGeo.Types
import SideGeo.Implicit1

-- nontrivial deducers
-- to find typographic minimum, call equispace on space with itself, and find minimum after rename
equispace :: Duals0 -> Duals0 -> Symmetries
equispace (Duals0 ds0) (Duals0 ds1) = f bs0 bs1 mapEmpty setEmpty setEmpty where
 bs0 = unions (setMap keysSet ds0)
 ss0 = unions (setMap valsSet ds0)
 bs1 = unions (setMap keysSet ds1)
 ss1 = unions (setMap valsSet ds1)
 sml = setToList (setMaps ss0 ss1)
 f :: Boundaries -> Boundaries -> Symmetry -> Boundaries -> Boundaries -> Symmetries
 f bs0' bs1' m ks vs
  | ds2 /= ds4 = setEmpty
  | (setSize bs1) == 0 = single m
  | otherwise = listFold1 union [g b0 b1 sm | b0 <- bl0, b1 <- bl1, sm <- sml] where
  bl0 = setToList bs0'
  bl1 = setToList bs1'
  ds2 = setMap (h vs) ds0
  ds3 = setMap (h ks) ds1
  ds4 = setMap (mapMap i) ds3
  g :: Boundary -> Boundary -> Reflection -> Symmetries
  g b0 b1 sm = f (remove bs0 b0) (remove bs1 b1) (extend m (b1,(b0,sm))) (insert ks b1) (insert vs b0)
  h :: Boundaries -> Direction -> Direction
  h bs sm = restrict sm bs
  i :: (Boundary,Sidedness) -> (Boundary,Sidedness)
  i (b0,s0) = let (b1,sm) = sub m b0 in (b1, sub sm s0)
-- return whether polytopes are equivalent
-- to find typographic minimum, call equitope on polytope with itself, and find minimum after rename
equitope :: Topez0 -> Topez0 -> Symmetries
equitope (Topez0 p0) (Topez0 p1) = f bs0 bs1 mapEmpty setEmpty setEmpty where
 bs0 = unions (setMap keysSet (unions (unions (setMap valsSet p0))))
 ss0 = unions (setMap valsSet (unions (unions (setMap valsSet p0))))
 bs1 = unions (setMap keysSet (unions (unions (setMap valsSet p1))))
 ss1 = unions (setMap valsSet (unions (unions (setMap valsSet p1))))
 sml = setToList (setMaps ss0 ss1)
 f :: Boundaries -> Boundaries -> Symmetry -> Boundaries -> Boundaries -> Symmetries
 f bs0' bs1' m ks vs
  | p3 /= p6 = setEmpty
  | (setSize bs1) == 0 = single m
  | otherwise = listFold1 union [g b0 b1 sm | b0 <- bl0, b1 <- bl1, sm <- sml] where
  bl0 = setToList bs0'
  bl1 = setToList bs1'
  p2 :: Polytope
  p2 = setMap (valsMap (setMap (h vs))) p0
  p3 = setMap (valsMap (setFilter mapNonempty)) p2
  p4 = setMap (valsMap (setMap (h ks))) p1
  p5 = setMap (valsMap (setFilter mapNonempty)) p4
  p6 = setMap (valsMap (setMap (mapMap i))) p5
  g :: Boundary -> Boundary -> Reflection -> Symmetries
  g b0 b1 sm = f (remove bs0 b0) (remove bs1 b1) (extend m (b1,(b0,sm))) (insert ks b1) (insert vs b0)
  h bs sm = restrict sm bs
  i (b0,s0) = let (b1,sm) = sub m b0 in (b1, sub sm s0)
