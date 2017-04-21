module Main where

main :: IO()
main = mapM_ (\x -> putStrLn (show x)) matrix

matrix :: [[String]]
matrix = reduce given

given :: [[String]]
given = [
  ["a","b","0","0","0","0","1","0","0","A"],
  ["c","d","0","0","0","0","0","1","0","B"],
  ["e","f","0","0","0","0","0","0","1","C"],
  ["0","0","g","h","0","0","1","0","0","D"],
  ["0","0","i","j","0","0","0","1","0","E"],
  ["0","0","k","l","0","0","0","0","1","F"],
  ["0","0","0","0","m","n","1","0","0","G"],
  ["0","0","0","0","o","p","0","1","0","H"],
  ["0","0","0","0","q","r","0","0","1","I"]
 ]

-- [0,0,1] [a,b,c] [g,h,i]
-- [0,1,0]*[d,e,f]=[d,e,f]
-- [1,0,0] [g,h,i] [a,b,c]

-- [0,0,1] [A] [C]
-- [0,1,0]*[B]=[B]
-- [1,0,0] [C] [A]

-- a*p + b*q + 1*x + 0*y + 0*z = A
-- c*p + d*q + 0*x + 1*y + 0*z = B
-- e*p + f*q + 0*x + 0*y + 1*z = C

-- x = (-a)*p + (-b)*q + A
-- y = (-c)*p + (-d)*q + B
-- z = (-e)*p + (-f)*q + C

-- [(x0,y0,z0),(x1,y1,z1),(x2,y2,z2)]
-- x = (x1-x0)*p + (x2-x0)*q + x0
-- y = (y1-y0)*p + (y2-y0)*q + y0
-- z = (z1-z0)*p + (z2-z0)*q + z0
-- a = x0-x1; b = x0-x2; A = x0
-- c = y0-y1; d = y0-y2; B = y0
-- e = z0-z1; f = z0-z2; C = z0

reduce :: [[String]] -> [[String]]
reduce (a:b)
 | (head a) == "0" = reduce (b ++ [a])
 | (length b) == 2 = (a:b)
 | otherwise = reduce (map (zeroOutHead a) b)
reduce _ = undefined

zeroOutHead :: [String] -> [String] -> [String]
zeroOutHead (_:b) ("0":d) = let
 prod :: String -> String
 prod = multiply "0"
 pairs :: [(String,String)]
 pairs = zip d (map prod b)
 in map add pairs
zeroOutHead (a:b) (c:d) = let
 prod :: String -> String
 prod = multiply (concat ["(-",c,"/",a,")"])
 pairs :: [(String,String)]
 pairs = zip d (map prod b)
 in map add pairs
zeroOutHead _ _ = undefined

multiply :: String -> String -> String
multiply _ "0" = "0"
multiply "0" _ = "0"
multiply a "1" = a
multiply "1" a = a
multiply a b = concat ["(",a,"*",b,")"]

add :: (String,String) -> String
add ("0",a) = a
add (a,"0") = a
add (a,b) = concat ["(",a,"+",b,")"]
