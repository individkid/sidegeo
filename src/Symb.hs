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
