module Language.Rose.AST where

type File = [Decl]

type NamespaceName = [String]

data QualifiedName = QualifiedName (Maybe NamespaceName) String deriving (Show)

data Decl
  = NamespaceDecl NamespaceName
  | UsingDecl NamespaceName
  | ClassDecl String (Maybe QualifiedName) [QualifiedName] [ClassMemberDecl]
  | ModuleDecl String [ClassMemberDecl]
  deriving (Show)

data ClassMemberDecl
  = FnClassMemberDecl Bool String [(String, TypeExpr)] TypeExpr Expr
  deriving (Show)

data TypeExpr
  = NameTypeExpr QualifiedName
  | VoidTypeExpr
  deriving (Show)

data Expr
  = NameExpr QualifiedName
  | CallExpr Expr [Expr]
  | InstanceMethodExpr Expr String
  | InstanceVariableExpr Expr String
  | StaticMethodExpr QualifiedName String
  | StaticVariableExpr QualifiedName String
  deriving (Show)
