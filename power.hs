power x y
  | y == 0 = 1
  | y < 0 = (power x (y + 1)) / x
  | otherwise = x * (power x (y - 1))
