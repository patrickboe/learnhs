sequenceIO :: [IO a] -> IO [a]
sequenceIO = ioIter []
  where
  ioIter :: [a] -> [IO a] -> IO [a]
  ioIter rs [] =
    return rs
  ioIter rs (a : as) =
    do
      r <- a
      ioIter (r:rs) as

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f = mIter [] f
  where
  mIter :: [b] -> (a -> IO b) -> [a] -> IO [b]
  mIter rs _ [] =
    return rs
  mIter rs f (a : as) =
    do
      r <- f a
      mIter (r:rs) f as
