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

-- | The function 'size' returns the number of nodes in a 'GameTree'
size :: Num b => GameTree a -> b 
size (Node x ts) = 1 + sum (map size ts)

-- | The function 'depth' returns the depth of the 'GameTree'
depth :: (Ord b, Num b) => GameTree a -> b
depth (Node x [])     = 0
depth (Node x (t:ts)) = 1 + maximum' 0 (map depth (t:ts))

-- | The function 'maximum'' returns the maximum of a list, if this is larger than a default value.
maximum' :: Ord a => a -> [a] -> a
maximum' m []     = m
maximum' m (x:xs) = if m > x then
                       maximum' m xs
                    else
                       maximum' x xs
-- | The function 'minimum'' returns the maximum of a list, if this is smaller than a default value.
minimum' :: Ord a => a -> [a] -> a
minimum' m []     = m
minimum' m (x:xs) = if m < x then
                       minimum' m xs
                    else
                       minimum' x xs

-- | The function 'prune' prunes a 'GameTree' to a given level.
prune :: (Num a, Eq a) => a -> GameTree b -> GameTree b
prune 0 (Node x ts) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ts) = Node x (map (prune (n-1)) ts) 

minimax :: (Ord a, Num a) => GameTree a -> a
minimax (Node x [])     = x
minimax (Node x (t:ts)) = -minimum' 0 (map minimax (t:ts))




