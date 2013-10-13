areaMessage base height = "The area of the triangle is " ++ (show ((read base) * (read height) / 2))

main = do
  putStrLn "The base?"
  base <- getLine
  putStrLn "The height?"
  height <- getLine
  putStrLn (areaMessage base height)
