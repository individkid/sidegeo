module Implicit.Implicit where

import Text.Regex.Posix
import Data.List
import Debug.Trace

merge :: [String] -> [String] -> [String] -> [String]
merge super sub0 sub1 =
 f super sub0 sub1 where
 f :: [String] -> [String] -> [String] -> [String]
 f (a:super') (b:sub0') (c:sub1')
  | a == b && a == c = a:(f super' sub0' sub1')
  | a == b && a /= c = a:(f super' sub0' (c:sub1'))
  | a /= b && a == c = a:(f super' (b:sub0') sub1')
  | a /= b && a /= c = f super' (b:sub0') (c:sub1')
  | otherwise = error "merge impossible"
 f (a:super') (b:sub0') []
  | a == b = a:(f super' sub0' [])
  | a /= b = f super' (b:sub0') []
  | otherwise = error "merge impossible"
 f (a:super') [] (c:sub1')
  | a == c = a:(f super' [] sub1')
  | a /= c = f super' [] (c:sub1')
  | otherwise = error "merge impossible"
 f _ [] [] = []
 f [] _ _ = error "merge empty super"

unmerge :: [String] -> [String] -> [String] -> [String]
unmerge super sub0 sub1 =
 f super sub0 sub1 where
 f :: [String] -> [String] -> [String] -> [String]
 f (a:super') (b:sub0') (c:sub1')
  | a == b && a == c = f super' sub0' sub1'
  | a == b && a /= c = a:(f super' sub0' (c:sub1'))
  | a /= b && a == c = f super' (b:sub0') sub1'
  | a /= b && a /= c = f super' (b:sub0') (c:sub1')
  | otherwise = error "unmerge impossible"
 f (a:super') (b:sub0') []
  | a == b = a:(f super' sub0' [])
  | a /= b = f super' (b:sub0') []
  | otherwise = error "unmerge impossible"
 f (a:super') [] (c:sub1')
  | a == c = f super' [] sub1'
  | a /= c = f super' [] (c:sub1')
  | otherwise = error "unmerge impossible"
 f _ [] [] = []
 f [] _ _ = error "unmerge super empty"

shortest :: [[String]] -> [String]
shortest [] = error "no shortest"
shortest (a:b) = f 0 a b where
 f _ c [] = c
 f n c (d:e) = let m = length d in
  if m < n then f m d e else f n c e

allin :: [String] -> [String] -> Bool
allin (a0:b0) (a1:b1)
 | a0 == a1 = allin b0 b1
 | a0 /= a1 = allin b0 (a1:b1)
 | otherwise = error "allin impossible"
allin _ [] = True
allin [] _ = False

isin :: [String] -> String -> Bool
isin (a:b) c
 | a == c = True
 | a /= c = isin b c
 | otherwise = error "oops1"
isin [] _ = False

anyin :: [String] -> [String] -> [String] -> Bool
anyin (a:allarg) (b:super) (c:subs)
 | a == b && a == c = anyin allarg super subs
 | a == b && a /= c = anyin allarg super (c:subs)
 | a /= b && a == c = False
 | a /= b && a /= c = anyin allarg (b:super) (c:subs)
 | otherwise = error "oops2"
anyin _ [] (_:_) = False
anyin _ _ [] = True
anyin [] _ _ = error "anyin allarg empty"

ofin :: [(String,[String])] -> String -> [String]
ofin [] _ = []
ofin ((a,b):c) d = if a == d then b else ofin c d

plan :: [String] -> [(String,[String])] -> [String] -> [String] -> [(String,[String])] ->
 [String] -> [(String,[String])] -> Maybe [(String,[String])]
plan _ [] _ _ _ (c:todo) _ = error ("plan empty original and nonempty todo"++(show (c:todo)))
plan _ _ [] _ _ (c:todo) _ = error ("plan empty given and nonempty todo"++(show (c:todo)))
plan _ _ _ [] _ (c:todo) _ = error ("plan empty allarg and nonempty todo"++(show (c:todo)))
plan _ _ _ _ [] (c:todo) _ = error ("plan empty convert and nonempty todo"++(show (c:todo)))
plan _ _ _ _ _ [] rslt = Just rslt -- nothing left todo
plan allarg original given (a:tailarg) ((b,args):convert) (c:todo) rslt
 | a /= b && a /= c =
  trace ("plan1;given:"++(show given)++";head:"++(show a)++";convert:"++(show b)++";todo:"++(show c))
  (plan allarg original given tailarg ((b,args):convert) (c:todo) rslt)
 | trivial && a /= b && a == c =
  trace ("plan2;given:"++(show given)++";head:"++(show a)++";convert:"++(show b)++";todo:"++(show c))
  (plan allarg original given tailarg ((b,args):convert) todo rslt)
 | (not trivial) && a /= b && a == c =
  trace ("plan3;given:"++(show given)++";head:"++(show a)++";convert:"++(show b)++";todo:"++(show c))
  (error "plan todo without convert")
 | a == b && a /= c = 
  trace ("plan4;given:"++(show given)++";head:"++(show a)++";convert:"++(show b)++";todo:"++(show c))
  (plan allarg original given tailarg convert (c:todo) rslt)
 | trivial && a == b && a == c =
  trace ("plan5;given:"++(show given)++";head:"++(show a)++";convert:"++(show b)++";todo:"++(show c))
  (plan allarg original given tailarg convert todo rslt)
 | (not trivial) && circular && a == b && a == c =
  trace ("plan6;given:"++(show given)++";head:"++(show a)++";convert:"++(show b)++";todo"++(show c))
  Nothing
 | (not trivial) && (not circular) && a == b && a == c = let
  todo' = merge allarg todo args
  rslt' = (b,args):rslt in
  trace ("plan7;given:"++(show given)++";head:"++(show a)++";convert:"++(show (b,args))++";todo:"++(show c))
  (plan allarg original given allarg original todo' rslt')
 | otherwise = error "plan impossible" where
 trivial = isin given a
 circular = (find (intree rslt b) args) /= Nothing

intree :: [(String,[String])] -> String -> String -> Bool
intree a b c = let
 args = ofin a c in
 -- trace ("intree;a:"++(show a)++";b:"++(show b)++";c:"++(show c))
 ((isin args b) || ((find (intree a b) args) /= Nothing))

named :: String -> [(String,String)]
named a = map f b where
 b = allsuch "^(type|data) [A-Z][a-z]* = .* -- [a-z][a-z]?$" a
 f c = (middle "[A-Z][a-z]*" c, suffix " = .* -- " c)

explicit :: String -> [(String,[String])]
explicit a = map f b where
 b = allsuch "[^A-Za-z][A-Z][a-z]*[0-9]( +[a-z][a-z]?[0-9]?)+" a
 f c = (c0,c2) where
  c0 = middle "[A-Z][a-z][a-z]*[0-9]" c
  c1 = suffix "[A-Z][a-z][a-z]*[0-9]" c
  c2 = allsuch "[a-z][a-z]?" c1

manual :: String -> [(String,String)]
manual a = map f b where
 b = allsuch "^[a-z]+[0-9]_[a-z]+[0-9]" a
 f c = (prefix "_" c, suffix "_" c)

required :: String -> [(String,[String],String,[String])]
required a = map f b where
 b = allsuch "[a-z]+[0-9](_[a-z][a-z]?)*_[a-z]+[0-9]?(_[a-z][a-z]?)*" a
 f c = if c' then (c0,c1,c2,c3) else (d0,d1,d2,d3) where
  c' = anysuch "[a-z]+[0-9].*_[a-z]+[0-9]" c
  c0' = middle "[a-z]+[0-9](_[a-z][a-z]?)*_" c
  c1' = allsuch "_[a-z][a-z]?" c0'
  c2' = suffix "[a-z]+[0-9](_[a-z][a-z]?)*_" c
  c3' = allsuch "_[a-z][a-z]?" c2'
  c0 = middle "[a-z]+[0-9]" c0'
  c1 = map (suffix "_") c1'
  c2 = middle "[a-z]+[0-9]" c2'
  c3 = map (suffix "_") c3'
  d0' = middle ".*_" c
  d1' = allsuch "_[a-z][a-z]?" d0'
  d0 = middle "[a-z]+[0-9]" d0'
  d1 = map (suffix "_") d1'
  d2 = suffix ".*_" c
  d3 = []

suffix :: String -> String -> String
suffix pat a = f (a =~ pat) where
 f :: (String,String,String) -> String
 f (_,_,a1) = a1

prefix :: String -> String -> String
prefix pat a = f (a =~ pat) where
 f :: (String,String,String) -> String
 f (a1,_,_) = a1

middle :: String -> String -> String
middle pat a = f (a =~ pat) where
 f :: (String,String,String) -> String
 f (_,a1,_) = a1

allsuch :: String -> String -> [String]
allsuch pat a = f (a =~ pat) where
 f :: (String,String,String) -> [String]
 f (_,"",_) = []
 f (_,a1,a2) = a1:(f (a2 =~ pat))

anysuch :: String -> String -> Bool
anysuch pat a = f (a =~ pat) where
 f :: (String,String,String) -> Bool
 f (_,a1,_) = a1 /= ""
