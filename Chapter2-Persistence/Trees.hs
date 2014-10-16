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

-- Exercise 2.2
-- worst case of `member` is 2*d comparions.
-- re-write member to take no more than d+1 comparisons by 
-- keeping track of a candidate element that might be equal 
-- to the query element (say, the last element for which     
-- < returned false or <= returned true) and checking for 
-- equality only at the bottom of the Tree

-- TODO: am I doing that by re-using `comparison`?

-- Exercise 2.5
-- make a `complete a Int` function that creates a tree of 
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