module Main where

import Implicit.Implicit

allconv :: [(String,[String])]
allconv = [("st",["dt","bs","rs","ss"]),("st",["ht","bs","rs","ss"]),
           ("dt",["st","bs","rs","ss"]),("ht",["st","bs","rs","ss"]),
           ("bs",["st"]),("bs",["dt"]),("bs",["ht"]),
           ("rs",["st"]),("rs",["dt"]),("rs",["ht"]),
           ("ss",["st"]),("ss",["dt"]),("ss",["ht"])]

allarg :: [String]
allarg = ["st","dt","ht","xt","bs","rs","ss"]

choice :: [(String,[String])]
choice = [("st",["dt","bs","rs","ss"]),
          ("dt",["ht","bs","rs","ss"]),
          ("ht",["st","bs","rs","ss"]),
          ("bs",["ht"]),
          ("rs",["ht"]),
          ("ss",["ht"])]

main :: IO ()
main = putStr ("\n"++(show (plan allarg choice ["ht"] allarg choice ["st"] []))++"\n\n")

{-
main :: IO ()
main = do
 contents <- getContents
 putStr "named"
 putStr (show (concat (map named (lines contents))))
 putStr "\n"
 putStr "explicit"
 putStr (show (concat (map explicit (lines contents))))
 putStr "\n"
 putStr "manual"
 putStr (show (concat (map manual (lines contents))))
 putStr "\n"
 putStr "required"
 putStr (show (concat (map required (lines contents))))
 putStr "\n"
-}
