factlist :: Int-> [Int]
factlist x = take x (scanl (*) 1 [2..])
