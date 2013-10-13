import Control.Monad
-- | Consume a given character in the input, and return the character we 
--   just consumed, paired with rest of the string. We use a do-block  so that
--   if the pattern match fails at any point, fail of the Maybe monad (i.e.
--   Nothing) is returned.
char :: Char -> String -> Maybe (Char, String)
char c s = do
  let (c':s') = s
  if c == c' then Just (c, s') else Nothing

hexChar s = foldl1 mplus (map ($s) parsers)
              where parsers = (map char (['0'..'9'] ++ ['a'..'f']))
