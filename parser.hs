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

escapedChar :: Stream s m Char => ParsecT s u m Char
escapedChar = do e <- oneOf "nrt\\\""
                 return $ case e of
                            '\\' -> '\\'
                            'n'  -> '\n'
                            'r'  -> '\r'
                            't'  -> '\t'
                            '"'  -> '"'

stringChar :: Stream s m Char => ParsecT s u m Char
stringChar = do c <- noneOf "\""
                if c == '\\'
                  then escapedChar
                  else return c

parseString :: Stream s m Char => ParsecT s u m LispVal
parseString = do char '"'
                 x <- many stringChar
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
