data Tree a = Empty 
              | Node a (Tree a) (Tree a)
              deriving (Show)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty           = Node x Empty Empty
insert x tree@(Node y left right) 
      | comparison == LT = Node y (insert x left) right
      | comparison == GT = Node y left (insert x right)
      | otherwise        = tree
      where comparison = compare x y

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty           = False
member x (Node y left right) 
      | comparison == LT = member x left
      | comparison == GT = member x right
      | otherwise        = True
      where comparison = compare x y

memberFromBook :: (Ord a) => a -> Tree a -> Bool
memberFromBook x Empty               = False
memberFromBook x (Node y left right) 
  | (x < y)   = memberFromBook x left
  | (x > y)   = memberFromBook x right
  | otherwise = True

-- #hacphi
-- re-write to use `case` for the comparison
-- Danny explained that we can check for <= only

member' :: (Ord a) => a -> Tree a -> Bool
member' _ Empty               = False
member' x (Node y left right) = case (compare x y) of 
                                     LT -> member' x left
                                     GT -> member' x right
                                     EQ -> True
-- Exercise 2.2
-- worst case of `member` is 2*d comparions.
-- re-write member to take no more than d+1 comparisons by 
-- keeping track of a candidate element that might be equal 
-- to the query element (say, the last element for which     
-- < returned false or <= returned true) and checking for 
-- equality only at the bottom of the Tree

type NumberComparisons = Int

memberShort :: (Ord a) => a -> Tree a -> (Bool, NumberComparisons)
memberShort x tree = go x tree Nothing 0
  where
    go x Empty cand comps               = case cand of Nothing  -> (False, comps)
                                                       (Just y) -> (x == y, comps)
    go x (Node y left right) cand comps = if (x <= y) then (go x left (Just y) (comps+1)) 
                                          else (go x right cand (comps+1))

-- Exercise 2.3 Inserting an existing element into a binary search tree copies the entire search
-- path plan even though the copied nodes are indistinguisable from the originals. Rewrite insert
-- using Exceptions to avoid this copying. Establish only one handler per insertion rather than one handler
-- per iteration.
insertExc :: (Ord a) => a -> Tree a -> Tree a
insertExc x Empty               = Node x Empty Empty
insertExc x (Node y left right) = case x `compare` y of
                                   GT -> insertExc x right
                                   LT -> insertExc x left
                                   EQ -> error "already present in tree!"

-- Exercise 2.4 Combine the ideas of the previous two exercises to obtain a version of 
-- insert that performs no unnecessary copying and uses no more than `d+1` comparisons
insertShort :: (Ord a) => a -> Tree a -> Tree a
insertShort x tree = go x tree Nothing
  where
    go x Empty cand               = case cand of Nothing  -> (Node x Empty Empty)
                                                 (Just y) -> if (x == y) then (error "!") else (Node x Empty Empty)
    go x (Node y left right) cand = if (x <= y) then (Node y (go x left (Just y)) right)
                                                else (Node y left (go x right cand))

-- Exercise 2.5
-- Part a: make a `complete a Int` function that creates a tree of 
-- depth Int, putting a in every leaf of the tree.
-- TODO: does it run in O(d) time? 
complete :: a -> Integer -> Maybe (Tree a)
complete x depth 
 | depth < 0  = Nothing
 | otherwise  = Just $ complete' depth
                        where complete' d 
                                | d == 0    = Empty
                                | otherwise = let copiedTree = complete' (d-1) 
                                              in Node x copiedTree copiedTree

-- Part B: Re-visit once discuss `complete`