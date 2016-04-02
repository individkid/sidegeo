module Implicit.Implicit where

import Text.Regex.Posix
import Debug.Trace

-- get identifiers and function parts

named :: String -> [(String,String)]
named a = map f b where
 b = allsuch "^(type|data) [A-Z][a-z]* = .* -- [a-z][a-z]?$" a
 f c = (middle "[A-Z][a-z]*" c, suffix " = .* -- " c)

explicit :: String -> [(String,[String])]
explicit a = map f b where
 b = allsuch "[^A-Za-z][A-Z][a-z]*[0-9]( +[a-z_][a-z]?[0-9]?)+" a
 f c = (c0,c2) where
  c0 = middle "[A-Z][a-z][a-z]*[0-9]" c
  c1 = suffix "[A-Z][a-z][a-z]*[0-9]" c
  c2 = allsuch "[a-z_][a-z]?" c1

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

-- complete constructor args necessary for selectors
-- combine constructors into super-constructors
-- compose constructors with selectors for more sub-constructors
-- repeat from combine until no work done

compose :: [(String,[String])] -> [(String,[String])] -> [(String,[String])]
compose [] _ = []
compose _ [] = []
compose ((a0,a1):as) ((b0,b1):bs)
 | a0 == b0 = let f a b = (b,a) in (map (f a1) b1) ++ (compose as bs)
 | a0 < b0 = compose as ((b0,b1):bs)
 | a0 > b0 = compose ((a0,a1):as) bs
 | otherwise = error "compose impossible"

combine :: [String] -> [(String,[String])] -> [(String,[String])]
combine _ [] = []
combine _ [a] = [a]
combine a ((b0,b1):(c0,c1):d)
 | b0 == c0 = combine a ((b0, (merge a b1 c1)) : d)
 | b0 /= c0 = (b0,b1) : (combine a ((c0,c1):d))
 | otherwise = error "combine impossible"

comsort1 :: [(String,[String])] -> [(String,[String])]
comsort1 [] = []
comsort1 [a] = [a]
comsort1 a = let (b,c) = complit1 a in comerge1 (comsort1 b) (comsort1 c)

comsort2 :: [String] -> [String]
comsort2 [] = []
comsort2 [a] = [a]
comsort2 a = let (b,c) = complit2 a in comerge2 (comsort2 b) (comsort2 c)

complit1 :: [(String,[String])] -> ([(String,[String])],[(String,[String])])
complit1 [] = ([],[])
complit1 [a] = ([a],[])
complit1 (a0:a1:as) = let (b0,b1) = complit1 as in (a0:b0,a1:b1)

complit2 :: [String] -> ([String],[String])
complit2 [] = ([],[])
complit2 [a] = ([a],[])
complit2 (a0:a1:as) = let (b0,b1) = complit2 as in (a0:b0,a1:b1)

comerge1 :: [(String,[String])] -> [(String,[String])] -> [(String,[String])]
comerge1 [] b = b
comerge1 a [] = a
comerge1 (a:as) (b:bs)
 | a <= b = a : (comerge1 as (b:bs))
 | a > b = b : (comerge1 (a:as) bs)
 | otherwise = error "comerge1 impossible"

comerge2 :: [String] -> [String] -> [String]
comerge2 [] b = b
comerge2 a [] = a
comerge2 (a:as) (b:bs)
 | a <= b = a : (comerge2 as (b:bs))
 | a > b = b : (comerge2 (a:as) bs)
 | otherwise = error "comerge2 impossible"

-- find which one-arg converters used in multi-arg converters

choices :: [(String,[String])] -> [(String,[String])] -> [[(String,[String])]]
choices [] [] = []
choices [] [a] = [[a]]
choices [] ((a0,a1):(b0,b1):c)
 | a0 == b0 = choices [(a0,a1),(b0,b1)] c
 | a0 /= b0 = map ((:)(a0,a1)) (choices [] ((b0,b1):c))
 | otherwise = error "choices impossible"
choices ((a0,a1):b) [] = let f a = [a] in map f ((a0,a1):b)
choices ((a0,a1):b) ((c0,c1):d)
 | a0 == c0 = choices ((c0,c1):(a0,a1):b) d
 | a0 /= c0 = let f a = map ((:)a) (choices [] ((c0,c1):d)) in concat (map f ((a0,a1):b))
 | otherwise = error "choices impossible"

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
  trace ("plan7;given:"++(show given)++";head:"++(show a)++";convert:"++(show b)++";todo:"++(show c))
  (plan allarg original given allarg original todo' rslt')
 | otherwise = error "plan impossible" where
 trivial = isin given a
 circular = any (intree rslt b) args

shortest :: [Maybe [(String,[String])]] -> [(String,[String])]
shortest [] = error "no shortest"
shortest (Nothing:b) = shortest b
shortest ((Just a):b) = f (length a) a b where
 f _ c [] = c
 f n c (Nothing:e) = f n c e
 f n c ((Just d):e) = let m = length d in
  if m < n then f m d e else f n c e

isin :: [String] -> String -> Bool
isin (a:b) c
 | a == c = True
 | a /= c = isin b c
 | otherwise = error "oops1"
isin [] _ = False

ofin :: [(String,[String])] -> String -> [String]
ofin [] _ = []
ofin ((a,b):c) d = if a == d then b else ofin c d

intree :: [(String,[String])] -> String -> String -> Bool
intree a b c = let
 args = ofin a c in
 -- trace ("intree;a:"++(show a)++";b:"++(show b)++";c:"++(show c))
 ((isin args b) || (any (intree a b) args))

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

-- produce single-arg constructors
-- produce multi-arg constrctors for single-arg converters
-- produce parameter constructors for converters
-- produce selectors
-- produce converters
-- produce tag rep declarations converters table
-- produce space converters
