module ParseTree where

newtype Identifier = Identifier String
  deriving (Show)

data Program = Program [Def]
  deriving (Show)

data Def     = Def Identifier Term
  deriving (Show)

data Term    = Variable   Identifier
             | Number     Integer
             | Lambda     [Identifier] [Term]
  deriving (Show)
