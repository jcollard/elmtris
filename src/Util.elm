module Util where

-- Creates an infinite list of the specified element
repeat : a -> [a]
repeat a = a::(repeat a)

