main = do
  putStrLn "What's your name?"
  n <- getLine
  case n of
    "Koen"                              -> putStrLn "Debugging Haskell is fun."
    x | elem x ["Simon","John","Phil"]  -> putStrLn "Haskell is a great language."
    _                                   -> do
                                          putStrLn "You are not on the list."
                                          main
