module Language.Rose.AST where

type File = [Decl]

data Decl
  = NamespaceDecl [String]
  | UsingDecl [String]
  deriving (Show)
