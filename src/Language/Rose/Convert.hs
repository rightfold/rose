module Language.Rose.Convert
( convert
) where

import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import Language.Rose.AST

import qualified Data.Map as Map

data ValueSymbol
  = VariableValueSymbol
  deriving (Eq)

data TypeSymbol

data Env = Env { vsyms :: Map String ValueSymbol
               , tsyms :: Map String TypeSymbol
               }

convert :: File -> String
convert file = convert' (Env (Map.singleton "this" VariableValueSymbol) Map.empty) file

convert' :: Env -> File -> String
convert' env = (>>= convertDecl env)

convertTypeParamList :: Env -> [TypeParam] -> String
convertTypeParamList _ [] = ""
convertTypeParamList _ ts = "<" ++ intercalate ", " (map go ts) ++ ">"
  where go (TypeParam Covariant     n) = "+" ++ n
        go (TypeParam Contravariant n) = "-" ++ n
        go (TypeParam Invariant     n) = n

convertSFV :: SFV -> String
convertSFV Static  = "static"
convertSFV Final   = "final"
convertSFV Virtual = "virtual"

convertDecl :: Env -> Decl -> String
convertDecl env (NamespaceDecl n) = "namespace " ++ convertNamespaceName env n ++ ";\n"
convertDecl env (UsingDecl n) = "using " ++ convertNamespaceName env n ++ ";\n"
convertDecl env (ClassDecl n ts _ _ ds) =
  "final class " ++ n ++ convertTypeParamList env ts
   ++ " {\n" ++ (ds >>= convertClassMemberDecl env) ++ "}\n"
convertDecl env (ModuleDecl n ds) =
  convertDecl env (ClassDecl n [] Nothing [] (map makeStatic ds))
  where makeStatic (FnClassMemberDecl _ n ts ps rt b) =
          FnClassMemberDecl Static n ts ps rt b

convertClassMemberDecl :: Env -> ClassMemberDecl -> String
convertClassMemberDecl env (CtorClassMemberDecl ps) =
  "public function __construct"
  ++ "(" ++ intercalate ", " (map param ps) ++ ") { }\n"
  where param (True, n, t)  = "private " ++ convertTypeExpr env t ++ " $" ++ n
        param (False, n, t) = convertTypeExpr env t ++ " $" ++ n
convertClassMemberDecl env (FnClassMemberDecl sfv n ts ps rt b) =
  "public " ++ convertSFV sfv ++ " function " ++ n
  ++ convertTypeParamList env ts
  ++ "(" ++ intercalate "," (map (\(p, t) -> convertTypeExpr env t ++ " $" ++ p) ps)
  ++ "): " ++ convertTypeExpr env rt -- TODO: parameters
  ++ " {\n" ++ convertExprS bEnv (\e -> "return " ++ e ++ ";\n") b ++ "}\n"
  where bEnv = foldl (\e (p, _) -> e { vsyms = Map.insert p VariableValueSymbol (vsyms e) })
                     env ps

convertTypeExpr :: Env -> TypeExpr -> String
convertTypeExpr env (NameTypeExpr n)       = convertQualifiedName env n
convertTypeExpr _   VoidTypeExpr           = "void"
convertTypeExpr env (FnTypeExpr ps r)      =
  "(function"
  ++ "(" ++ intercalate ", " (map (convertTypeExpr env) ps)
  ++ "): " ++ convertTypeExpr env r ++ ")"
convertTypeExpr env (AppliedTypeExpr p ts) =
  convertTypeExpr env p
  ++ "<" ++ intercalate ", " (map (convertTypeExpr env) ts) ++ ">"

convertExprS :: Env -> (String -> String) -> Expr -> String
convertExprS env result (LetExpr n v b) =
  convertExprS env (\e -> "$" ++ n ++ " = " ++ e ++ ";\n") v
  ++ convertExprS bEnv result b
  where bEnv = env { vsyms = Map.insert n VariableValueSymbol (vsyms env) }
convertExprS env result e = result (convertExprE env e)

convertExprE :: Env -> Expr -> String
convertExprE env (NameExpr q@(QualifiedName Nothing n)) =
  case Map.lookup n (vsyms env) of
    Just VariableValueSymbol -> "$" ++ n
    _ -> convertQualifiedName env q
convertExprE env (NameExpr n) = convertQualifiedName env n
convertExprE env (CallExpr c as) =
  "call_user_func(" ++ convertExprE env c ++ (as >>= (", " ++) . convertExprE env) ++ ")"
convertExprE env (NewExpr c as) =
  "new " ++ convertTypeExpr env c
  ++ "(" ++ intercalate ", " (map (convertExprE env) as) ++ ")"
convertExprE env (LambdaExpr ps b) =
  "function"
  ++ "(" ++ intercalate ", " (map ("$" ++) ps) ++ ")"
  ++ (if null frees then "" else " use(" ++ intercalate ", " (map ("$" ++) frees) ++ ")")
  ++ " {\n" ++ convertExprS bEnv (\e -> "return " ++ e ++ ";\n") b ++ "}"
  where bEnv = foldl (\e p -> e { vsyms = Map.insert p VariableValueSymbol (vsyms e) })
                     env ps
        frees = Map.toList (vsyms env)
                & filter ((== VariableValueSymbol) . snd)
                & map fst
                & filter (/= "this")
                & filter (not . (`elem` ps))
convertExprE env (InstanceMethodExpr e n) =
  "inst_meth(" ++ convertExprE env e ++ ", '" ++ n ++ "')"
convertExprE env (InstanceVariableExpr e n) =
  convertExprE env e ++ "->" ++ n
convertExprE env (StaticMethodExpr c n) =
  "class_meth(" ++ convertQualifiedName env c ++ "::class, '" ++ n ++ "')"

convertNamespaceName :: Env -> NamespaceName -> String
convertNamespaceName _ name = intercalate "\\" name

convertQualifiedName :: Env -> QualifiedName -> String
convertQualifiedName _ (QualifiedName (Just []) n) = "\\" ++ n
convertQualifiedName e (QualifiedName (Just ns) n) = convertNamespaceName e ns ++ "\\" ++ n
convertQualifiedName _ (QualifiedName Nothing   n) = n
