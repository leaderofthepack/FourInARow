------------------------------------------------------------------------
--
-- Richard Bird: Introduction to Functional Programming
--
-- Chapter 9.7: Game Trees
--
------------------------------------------------------------------------

module GameTree where

data GameTree a = Node a [GameTree a]
                  deriving Show

gt1 = Node 0 [Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 [] ]]]
gt2 = Node 0 [Node 1 [Node 2 []]]

size (Node x ts) = 1 + sum (map size ts)

depth (Node x [])     = 0
depth (Node x (t:ts)) = 1 + maximum' 0 (map depth (t:ts))

maximum' m []     = m
maximum' m (x:xs) = if m > x then
                       maximum' m xs
                    else
                       maximum' x xs

minimum' m []     = m
minimum' m (x:xs) = if m < x then
                       minimum' m xs
                    else
                       minimum' x xs

prune 0 (Node x ts) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ts) = Node x (map (prune (n-1)) ts) 

minimax (Node x [])     = x
minimax (Node x (t:ts)) = -minimum' 0 (map minimax (t:ts))




