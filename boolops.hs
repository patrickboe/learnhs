rec_scanr :: (a -> b -> b) -> b -> [a] -> [b]
rec_scanr f i [] = [i]
rec_scanr f i (x : xs) = (f x (head rem)) : rem
  where rem = (rec_scanr f i xs)
