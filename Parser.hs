module Parser where

import Control.Monad
import ParseTree
import qualified Lexer as L
import Text.ParserCombinators.Parsec

parseCon :: String -> Either ParseError Program
parseCon input = parse translate "" input

translate :: Parser Program
translate =  do L.whiteSpace
                prgm <- fmap Program (many def)
                eof
                return prgm
         <?> "program"

term :: Parser Term
term =  fmap Variable identifier
    <|> fmap Number L.natural
    <|> do L.symbol "\\"
           args <- L.parens $ identifier `sepBy` (char ',')
           L.symbol "->"
           body <- many1 term
           return (Lambda args body)
    <?> "term"

def :: Parser Def
def =  do L.reserved "def"
          name <- identifier
          L.symbol "\"=\""
          expr <- term
          return (Def name expr)
   <?> "def"

identifier :: Parser Identifier
identifier =  fmap Identifier L.identifier
          <?> "identifier"
