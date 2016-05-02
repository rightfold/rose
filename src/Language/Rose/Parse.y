{
module Language.Rose.Parse
( parse
) where

import Language.Rose.AST
import Language.Rose.Lex (Token(..))
}

%name parse
%tokentype { Token }
%error { error . show }

%token
  class               { Class }
  end                 { End }
  fn                  { Fn }
  is                  { Is }
  namespace           { Namespace }
  using               { Using }
  void                { Void }

  identifier          { Identifier $$ }

  ':'                 { Colon }
  '('                 { ParenLeft }
  ')'                 { ParenRight }
  ';'                 { Semicolon }
  '~'                 { Tilde }

%%

File :           { [] }
     | Decl File { $1 : $2 }



Decl : NamespaceDecl { $1 }
     | UsingDecl     { $1 }
     | ClassDecl     { $1 }

NamespaceDecl : namespace NamespaceName ';' { NamespaceDecl $2 }

UsingDecl : using NamespaceName ';' { UsingDecl $2 }

ClassDecl : class identifier is ClassMemberDecls end ';' { ClassDecl $2 Nothing [] $4 }



ClassMemberDecl : FnClassMemberDecl { $1 }

FnClassMemberDecl : fn identifier ValueParamList ':' TypeExpr is Expr ';'
                      { FnClassMemberDecl $2 $3 $5 $7 }



TypeExpr : NameTypeExpr { $1 }
         | VoidTypeExpr { $1 }

NameTypeExpr : QualifiedName { NameTypeExpr $1 }

VoidTypeExpr : void { VoidTypeExpr }



Expr : NameExpr { $1 }

NameExpr : identifier { NameExpr $1 }



ClassMemberDecls :                                  { [] }
                 | ClassMemberDecl ClassMemberDecls { $1 : $2 }

ValueParamList : '(' ')' { [] }

NamespaceName : identifier                   { [$1] }
              | identifier '~' NamespaceName { $1 : $3 }

QualifiedName : identifier        { QualifiedName Nothing $1 }
              | NamespaceName     { QualifiedName (Just (init $1)) (last $1) }
              | '~' NamespaceName { QualifiedName (Just (init $2)) (last $2) }
