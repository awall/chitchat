--
-- After some quick comparisons, settled on using Megaparsec for parsing.
-- See https://github.com/mrkkrp/megaparsec#comparison-with-other-solutions for comparison of alternatives.
--
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module ChitChat.Ast
  ( commandP
  ) where

import Control.Monad.State
import Data.Char
import Data.IORef
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data WordT = WordT
data IntT = IntT
data StringT = StringT
data FunT = FunT
data FunCallT = FunCallT

data BlockT = BlockT
data ArgT = ArgT

data Ast a where
  AstWord :: String -> Ast WordT
  AstInt :: Int -> Ast IntT  
  AstString :: String -> Ast StringT
  AstFun :: [Ast WordT] -> Ast BlockT -> Ast FunT
  AstFunCall :: Ast WordT -> [Ast ArgT] -> Ast FunCallT  

  AstWordArg :: Ast WordT -> Ast ArgT
  AstIntArg :: Ast IntT -> Ast ArgT
  AstStringArg :: Ast StringT -> Ast ArgT
  AstFunArg :: Ast FunT -> Ast ArgT
  AstBlockArg :: Ast BlockT -> Ast ArgT

  AstBlock :: [Ast FunCallT] -> Ast BlockT  

type Parser = Parsec Void String

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

digit :: Parser Char
digit = oneOf "0123456789"

munch :: Parser ()
munch = void $ many $ oneOf " \t"

lfmunch :: Parser ()
lfmunch = void $ many $ oneOf " \t\n"

wordCharP :: Parser Char
wordCharP = noneOf "[]#\"\t\n"

wordP :: Parser (Ast WordT)
wordP = AstWord <$> many1 wordCharP <* munch

intP :: Parser (Ast IntT)
intP =  AstInt <$> read <$> many1 digit <* munch

stringCharP :: Parser Char
stringCharP = noneOf "\"\n"

stringP :: Parser (Ast StringT)
stringP = AstString <$> between (char '\"') (char '\"') (many stringCharP) <* munch

funP :: Parser (Ast FunT)
funP = do
  char '#'
  words <- many wordP
  block <- blockP
  return $ AstFun words block

argP :: Parser (Ast ArgT)
argP = 
  stringArgP <|> intArgP <|> funArgP <|> wordArgP <|> blockArgP
  where 
    stringArgP = AstStringArg <$> stringP
    intArgP = AstIntArg <$> intP
    funArgP = AstFunArg <$> funP
    wordArgP = AstWordArg <$> wordP
    blockArgP = AstBlockArg <$> blockP

blockP :: Parser (Ast BlockT)
blockP = AstBlock <$> between (char '[' <* lfmunch) (char ']') funCallsP

funCallP :: Parser (Ast FunCallT)
funCallP = do
  word <- wordP
  args <- many argP
  return $ AstFunCall word args

funCallsP :: Parser [Ast FunCallT]
funCallsP = sepEndBy1 funCallP (char '\n' >> lfmunch)

commandP :: Parser (Ast FunCallT)
commandP = funCallP <* lfmunch <* eof

