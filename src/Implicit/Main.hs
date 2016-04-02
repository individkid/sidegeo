module Main where

import Implicit.Implicit

allarg :: [String]
allarg = ["st","dt","ht","bs","rs","ss"]

choice :: [(String,[String])]
choice = [("st",["dt","bs","rs","ss"]),
          ("dt",["ht","bs","rs","ss"]),
          ("ht",["st","bs","rs","ss"]),
          ("bs",["ht"]),
          ("rs",["ht"]),
          ("ss",["ht"])]

main :: IO ()
main = do
 -- putStr ("\n"++(show (plan allarg choice ["ht"] allarg choice ["st"] []))++"\n\n")
 -- container <- (readFile "src/SideGeo/Container.hs")
 -- lambda <- (readFile "src/SideGeo/Lambda.hs")
 types <- (readFile "src/SideGeo/Types.hs")
 deduce <- (readFile "src/SideGeo/Deduce.hs")
 induce <- (readFile "src/SideGeo/Induce.hs")
 -- equivalent <- (readFile "src/SideGeo/Equivalent.hs")
 kernel <- (readFile "src/SideGeo/Kernel.hs")
 polytope <- (readFile "src/SideGeo/Polytope.hs")
 -- convert <- (readFile "src/SideGeo/Convert.hs")
 -- space <- (readFile "src/SideGeo/Space.hs")

 putStr "\n"
 putStr "named Types.hs\n"
 putStr (show (concat (map named (lines types))))
 putStr "\n"

 putStr "\n"
 putStr "explicit Deduce.hs\n"
 putStr (show (concat (map explicit (lines deduce))))
 putStr "\n"
 putStr "explicit Induce.hs\n"
 putStr (show (concat (map explicit (lines induce))))
 putStr "\n"
 putStr "explicit Kernel.hs\n"
 putStr (show (concat (map explicit (lines kernel))))
 putStr "\n"
 putStr "explicit Polytope.hs\n"
 putStr (show (concat (map explicit (lines polytope))))
 putStr "\n"

 putStr "\n"
 putStr "manual Induce.hs\n"
 putStr (show (concat (map manual (lines induce))))
 putStr "\n"

 putStr "\n"
 putStr "required Kernel.hs\n"
 putStr (show (concat (map required (lines kernel))))
 putStr "\n"
 putStr "required Polytope.hs\n"
 putStr (show (concat (map required (lines polytope))))
 putStr "\n"

 putStr "\n"
