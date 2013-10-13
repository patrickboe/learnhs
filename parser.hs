{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Identity (Identity)
import System.Environment (getArgs)
import Text.Parsec hiding (spaces)

symbol :: Stream s m Char => ParsecT s u m Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany1 space

parseExpr :: Stream s m Char => ParsecT s u m LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

nonquote :: Stream s m Char => ParsecT s u m Char
nonquote = do c <- noneOf "\""
              return case c of
                       '\' -> char '"'
                       otherwise -> return c

parseString :: Stream s m Char => ParsecT s u m LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Stream s m Char => ParsecT s u m LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                           "#t" -> Bool True
                           "#f" -> Bool False
                           otherwise -> Atom atom

parseNumber :: Stream s m Char => ParsecT s u m LispVal
parseNumber = many1 digit >>= (return . Number . read)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
