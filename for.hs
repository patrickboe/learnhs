 for :: a -> (a->Bool) -> (a->a) -> (a-> IO ()) -> IO ()
 for i p f job = do
  if (p i)
    then do
      job i
      for (f i) p f job
    else
      return ()
