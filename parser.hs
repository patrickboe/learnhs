{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Identity (Identity)
import System.Environment (getArgs)
import Text.Parsec hiding (spaces)
import Numeric
import Data.Char
import Data.List

symbol :: Stream s m Char => ParsecT s u m Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany1 space

parseExpr :: Stream s m Char => ParsecT s u m LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseDecimal

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right x -> "Found value : " ++ show x

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show (Atom s) = show s
  show (List l) = "(" ++ (intercalate " , " (map show l)) ++ ")"
  show (DottedList xs e) = "(" ++ (intercalate " , " (map show xs)) ++ " . " ++ (show e) ++ ")"
  show (Number n) = show n
  show (String s) = s
  show (Bool b) = show b

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

parsePrefixedVal :: Stream s m Char => ParsecT s u m LispVal
parsePrefixedVal = do prefix <- oneOf "tfbodx"
                      if prefix `elem` "tf"
                        then return $ Bool (prefix=='t')
                        else parseRadix $ case prefix of
                                            'd' -> readDec
                                            'b' -> readBin
                                            'x' -> readHex
                                            'o' -> readOct

parseAtom :: Stream s m Char => ParsecT s u m LispVal
parseAtom = do first <- letter <|> symbol
               case first of
                 '#' -> parsePrefixedVal
                 otherwise -> do
                   rest <- many (letter <|> digit <|> symbol)
                   return $ Atom (first : rest)

parseBool :: Stream s m Char => ParsecT s u m LispVal
parseBool = liftM (Bool . (== 't')) $ oneOf "tf"

parseDecimal :: Stream s m Char => ParsecT s u m LispVal
parseDecimal = parseRadix readDec

parseRadix :: Stream s m Char => ReadS Integer -> ParsecT s u m LispVal
parseRadix readf = liftM (Number . fst . head . readf) $ many1 digit

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
