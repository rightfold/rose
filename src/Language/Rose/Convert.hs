module Language.Rose.Convert
( convert
) where

import Data.List (intercalate)
import Data.Map (Map)
import Language.Rose.AST

import qualified Data.Map as Map

data ValueSymbol
  = VariableValueSymbol

data TypeSymbol

data Env = Env { vsyms :: Map String ValueSymbol
               , tsyms :: Map String TypeSymbol
               }

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
convertClassMemberDecl env (FnClassMemberDecl n ps rt b) =
  "public final function " ++ n
  ++ "(" ++ intercalate "," (map (\(p, t) -> convertTypeExpr env t ++ " $" ++ p) ps)
  ++ "): " ++ convertTypeExpr env rt -- TODO: parameters
  ++ " {\n" ++ convertExprS bEnv (\e -> "return " ++ e ++ ";\n") b ++ "}\n"
  where bEnv = foldl (\e (p, _) -> e { vsyms = Map.insert p VariableValueSymbol (vsyms e) })
                     env ps

convertTypeExpr :: Env -> TypeExpr -> String
convertTypeExpr env (NameTypeExpr n) = convertQualifiedName env n
convertTypeExpr _   VoidTypeExpr     = "void"

convertExprS :: Env -> (String -> String) -> Expr -> String
convertExprS env result e = result (convertExprE env e)

convertExprE :: Env -> Expr -> String
convertExprE env (NameExpr q@(QualifiedName Nothing n)) =
  case Map.lookup n (vsyms env) of
    Just VariableValueSymbol -> "$" ++ n
    _ -> convertQualifiedName env q
convertExprE env (NameExpr n) = convertQualifiedName env n

convertNamespaceName :: Env -> NamespaceName -> String
convertNamespaceName _ name = intercalate "\\" name

convertQualifiedName :: Env -> QualifiedName -> String
convertQualifiedName _ (QualifiedName (Just []) n) = "\\" ++ n
convertQualifiedName e (QualifiedName (Just ns) n) = convertNamespaceName e ns ++ "\\" ++ n
convertQualifiedName _ (QualifiedName Nothing   n) = n
