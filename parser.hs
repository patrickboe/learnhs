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
parseExpr = parseNumber
        <|> parseString
        <|> parseAtom

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

parseAtom :: Stream s m Char => ParsecT s u m LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                           "#t" -> Bool True
                           "#f" -> Bool False
                           otherwise -> Atom atom

parseNumber :: Stream s m Char => ParsecT s u m LispVal
parseNumber = parseDecimal <|> parseRadix

parseDecimal :: Stream s m Char => ParsecT s u m LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseRadix :: Stream s m Char => ParsecT s u m LispVal
parseRadix = do char '#'
                c <- oneOf "bodx"
                ns <- many1 digit
                let reader = fst . head . (case c of
                                             'd' -> readDec
                                             'b' -> readBin
                                             'x' -> readHex
                                             'o' -> readOct)
                return $ Number $ reader ns

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
