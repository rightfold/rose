module Language.Rose.AST where

data ModuleDecl = ModuleDecl String [Decl] deriving (Show)

data Decl
  = LetDecl String TermExpr
  deriving (Show)

data TermExpr
  = NameTermExpr String
  | ApplyTermExpr TermExpr TermExpr
  deriving (Show)
