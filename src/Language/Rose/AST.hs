module Language.Rose.AST where

type File = [Decl]

type NamespaceName = [String]

data QualifiedName = QualifiedName (Maybe NamespaceName) String deriving (Show)

data TypeParam = TypeParam Variance String deriving (Show)

data Variance = Covariant | Contravariant | Invariant deriving (Show)

data SFV = Static | Final | Virtual deriving (Show)

data Decl
  = NamespaceDecl NamespaceName
  | UsingDecl NamespaceName
  | ClassDecl String [TypeParam] (Maybe QualifiedName) [QualifiedName] [ClassMemberDecl]
  | ModuleDecl String [ClassMemberDecl]
  deriving (Show)

data ClassMemberDecl
  = CtorClassMemberDecl [(Bool, String, TypeExpr)]
  | FnClassMemberDecl SFV String [TypeParam] [(String, TypeExpr)] TypeExpr Expr
  deriving (Show)

data TypeExpr
  = NameTypeExpr QualifiedName
  | VoidTypeExpr
  | FnTypeExpr [TypeExpr] TypeExpr
  | AppliedTypeExpr TypeExpr [TypeExpr]
  | NullableTypeExpr TypeExpr
  deriving (Show)

data Expr
  = NameExpr QualifiedName
  | CallExpr Expr [Expr]
  | NewExpr TypeExpr [Expr]
  | CastExpr TypeExpr Expr
  | LambdaExpr [String] Expr
  | LetExpr String Expr Expr
  | IfExpr Expr Expr Expr
  | UncheckedExpr Expr
  | InstanceMethodExpr Expr String
  | InstanceVariableExpr Expr String
  | StaticMethodExpr QualifiedName String
  | StaticVariableExpr QualifiedName String
  deriving (Show)
