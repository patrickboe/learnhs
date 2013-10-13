myscanr :: (a -> b -> b) -> b -> [a] -> [b]
myscanr f p = foldr (\q xss@(x : xs) -> (f q x) : xss) [p]

myscanl :: (a -> b -> a) -> a -> [b] -> [a]
myscanl f p = reverse . (foldl (\xss@(x : xs) q -> (f x q) : xss) [p])
