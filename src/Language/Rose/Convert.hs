module Language.Rose.Convert
( convert
) where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Map as Map
import Language.Rose.AST

data ValueSymbol

data TypeSymbol

data Env = Env (Map String ValueSymbol) (Map String TypeSymbol)

convert :: File -> String
convert file = convert' (Env Map.empty Map.empty) file

convert' :: Env -> File -> String
convert' env = (>>= convertDecl env)

convertDecl :: Env -> Decl -> String
convertDecl env (NamespaceDecl n) = "namespace " ++ convertNamespaceName env n ++ ";\n"
convertDecl env (UsingDecl n) = "using " ++ convertNamespaceName env n ++ ";\n"
convertDecl env (ClassDecl n _ _ ds) =
  "final class " ++ n ++ " {\n" ++ (ds >>= convertClassMemberDecl env) ++ "}\n"

convertClassMemberDecl :: Env -> ClassMemberDecl -> String
convertClassMemberDecl env (FnClassMemberDecl n _ rt _) =
  "public final function " ++ n ++ "(): " ++ convertTypeExpr env rt ++ " {\n}\n"

convertTypeExpr :: Env -> TypeExpr -> String
convertTypeExpr env (NameTypeExpr n) = convertQualifiedName env n
convertTypeExpr _   VoidTypeExpr     = "void"

convertNamespaceName :: Env -> NamespaceName -> String
convertNamespaceName _ name = intercalate "\\" name

convertQualifiedName :: Env -> QualifiedName -> String
convertQualifiedName _ (QualifiedName (Just []) n) = "\\" ++ n
convertQualifiedName e (QualifiedName (Just ns) n) = convertNamespaceName e ns ++ "\\" ++ n
convertQualifiedName _ (QualifiedName Nothing   n) = n
