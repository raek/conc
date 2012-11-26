module Lexer where

import Control.Monad
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Token (makeTokenParser)

whiteSpace = P.whiteSpace lexer
symbol     = P.symbol     lexer
natural    = P.natural    lexer
identifier = P.identifier lexer
reserved   = P.reserved   lexer
parens     = P.parens     lexer
lexer      = makeTokenParser $ haskellDef
             { P.reservedNames = ["def"]
             , P.identLetter   = P.identLetter haskellDef <|> char '?' <|> char '-'
             }