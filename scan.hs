myscanr :: (a -> b -> b) -> b -> [a] -> [b]
myscanr f i = foldr (\j (x:xs) -> (f j x) : xs) [i]
