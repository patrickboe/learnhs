maximum :: Ord a => [a] -> a
maximum = foldl1 max

minimum :: Ord a => [a] -> a
minimum = foldr1 min
