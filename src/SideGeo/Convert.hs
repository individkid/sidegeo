module SideGeo.Convert where

import SideGeo.Container
import SideGeo.Lambda
import SideGeo.Implicit3

type Space = Map Tag Rep

-- convert augments space and gets reps as image of tags
convert :: Space -> [Tag] -> ([Rep],Space)
convert s l = let
 (p,q) = f s l
 r = listMap (sub q) l in
 if p then (r,q) else error "cannot convert" where
 m = mapFromList conversions
 f :: Space -> [Tag] -> (Bool,Space)
 f s' l' = listFold2 g l' (True,s')
 g :: Tag -> (Bool,Space) -> (Bool,Space)
 g _ (False,s') = (False,s')
 g t' (True,s') = h s' t'
 h :: Space -> Tag -> (Bool,Space)
 h s' t' = if (maybeSub s' t') /= Nothing then (True,s') else
  listFold2 (i t') (sub m t') (False,s')
 i :: Tag -> ([Rep] -> Rep, [Tag]) -> (Bool,Space) -> (Bool,Space)
 i _ (_,_) (True,s') = (True,s')
 i t' (j',l') (False,s') = let
  (p,q) = f s' l'
  r = listMap (sub q) l'
  e = extend q (t', j' r) in
  if p then (True,e) else (False,q)
-- force removes dependents and inserts given
force :: Space -> Tag -> Rep -> Space
force s t r = extend (restrict s (setFromList (listOptMap f conversions))) (t,r) where
 f :: (Tag, [([Rep] -> Rep, [Tag])]) -> Maybe Tag
 f (a,b) = if a == t || (listAny g b) then Nothing else Just a
 g (_,d) = listAny ((==) t) d
-- space makes single representation
space :: Tag -> Rep -> Space
space t r = mapFromList [(t,r)]
