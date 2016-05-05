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
convert file = convert' (Env (Map.singleton "this" VariableValueSymbol) Map.empty) file

convert' :: Env -> File -> String
convert' env = (>>= convertDecl env)

convertDecl :: Env -> Decl -> String
convertDecl env (NamespaceDecl n) = "namespace " ++ convertNamespaceName env n ++ ";\n"
convertDecl env (UsingDecl n) = "using " ++ convertNamespaceName env n ++ ";\n"
convertDecl env (ClassDecl n _ _ ds) =
  "final class " ++ n ++ " {\n" ++ (ds >>= convertClassMemberDecl env) ++ "}\n"
convertDecl env (ModuleDecl n ds) =
  convertDecl env (ClassDecl n Nothing [] (map makeStatic ds))
  where makeStatic (FnClassMemberDecl _ n ps rt b) =
          FnClassMemberDecl True n ps rt b

convertClassMemberDecl :: Env -> ClassMemberDecl -> String
convertClassMemberDecl env (FnClassMemberDecl static n ps rt b) =
  "public " ++ (if static then "static" else "final") ++ " function " ++ n
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
convertExprE env (CallExpr c as) =
  "call_user_func(" ++ convertExprE env c ++ (as >>= (", " ++) . convertExprE env) ++ ")"
convertExprE env (InstanceMethodExpr e n) =
  "inst_meth(" ++ convertExprE env e ++ ", '" ++ n ++ "')"
convertExprE env (StaticMethodExpr c n) =
  "class_meth(" ++ convertQualifiedName env c ++ "::class, '" ++ n ++ "')"

convertNamespaceName :: Env -> NamespaceName -> String
convertNamespaceName _ name = intercalate "\\" name

convertQualifiedName :: Env -> QualifiedName -> String
convertQualifiedName _ (QualifiedName (Just []) n) = "\\" ++ n
convertQualifiedName e (QualifiedName (Just ns) n) = convertNamespaceName e ns ++ "\\" ++ n
convertQualifiedName _ (QualifiedName Nothing   n) = n
