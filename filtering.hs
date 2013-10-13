doubleOfFirstForEvenSeconds = (map doubleFirst) . (filter evenSecond)
  where
  evenSecond (x,y) = (y `mod` 2) == 0
  doubleFirst (x,y) = 2 * x

