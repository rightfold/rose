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
  module              { Module }
  namespace           { Namespace }
  using               { Using }
  void                { Void }

  identifier          { Identifier $$ }

  ':'                 { Colon }
  ','                 { Comma }
  '('                 { ParenLeft }
  ')'                 { ParenRight }
  '.'                 { Period }
  ';'                 { Semicolon }
  '~'                 { Tilde }

%%

File :           { [] }
     | Decl File { $1 : $2 }



Decl : NamespaceDecl { $1 }
     | UsingDecl     { $1 }
     | ClassDecl     { $1 }
     | ModuleDecl    { $1 }

NamespaceDecl : namespace NamespaceName ';' { NamespaceDecl $2 }

UsingDecl : using NamespaceName ';' { UsingDecl $2 }

ClassDecl : class identifier is ClassMemberDecls end ';' { ClassDecl $2 Nothing [] $4 }

ModuleDecl : module identifier is ClassMemberDecls end ';' { ModuleDecl $2 $4 }



ClassMemberDecl : FnClassMemberDecl { $1 }

FnClassMemberDecl : fn identifier ValueParamList ':' TypeExpr is Expr ';'
                      { FnClassMemberDecl False $2 $3 $5 $7 }



TypeExpr : NameTypeExpr { $1 }
         | VoidTypeExpr { $1 }

NameTypeExpr : QualifiedName { NameTypeExpr $1 }

VoidTypeExpr : void { VoidTypeExpr }



Expr : CallExpr { $1 }

CallExpr : PrimExpr { $1 }
         | CallExpr ValueArgList { CallExpr $1 $2 }
         | CallExpr '.' identifier { InstanceMethodExpr $1 $3 }

PrimExpr : NameExpr         { $1 }
         | StaticMethodExpr { $1 }

NameExpr : QualifiedName { NameExpr $1 }

StaticMethodExpr : QualifiedName ':' identifier { StaticMethodExpr $1 $3 }



ClassMemberDecls :                                  { [] }
                 | ClassMemberDecl ClassMemberDecls { $1 : $2 }

ValueArgList : '(' ValueArgs ')' { $2 }

ValueArgs :                        { [] }
          | ValueArg               { [$1] }
          | ValueArg ',' ValueArgs { $1 : $3 }

ValueArg : Expr { $1 }

ValueParamList : '(' ValueParams ')' { $2 }

ValueParams :                            { [] }
            | ValueParam                 { [$1] }
            | ValueParam ',' ValueParams { $1 : $3 }

ValueParam : identifier ':' TypeExpr { ($1, $3) }

NamespaceName : identifier                   { [$1] }
              | identifier '~' NamespaceName { $1 : $3 }

QualifiedName : NamespaceName
                  { QualifiedName (if null (init $1) then Nothing else Just (init $1))
                                  (last $1) }
              | '~' NamespaceName { QualifiedName (Just (init $2)) (last $2) }
