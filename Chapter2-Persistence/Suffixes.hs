-- Exercise 2.1
-- suffixes [1,2,3,4] == [[1,2,3,4], [2,3,4], [3,4], [4], []]
suffixes :: [a] -> [[a]]
suffixes []         = [] : []
suffixes xxs@(_:xs) = xxs : suffixes xs