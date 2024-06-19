module Main where

import BinaryTree

main :: IO ()

testTree =
  Node 5 "Root" (Node 3 "left" Null Null) (Node 10 "right" (Node 6 "Lefty" Null Null) (Node 14 "Righty" Null Null))

newtree = insert 11 "test" testTree

main = print ( newtree )
