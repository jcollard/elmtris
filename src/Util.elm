module Util where

-- Creates an infinite list of the specified element
repeat : a -> [a]
repeat a = a::(repeat a)

replicate : Int -> a -> [a]
replicate n a =
  if n <= 0 then [] else a::(replicate (n-1) a)