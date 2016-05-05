{
module Language.Rose.Parse
( parse
) where

import Language.Rose.AST
import qualified Language.Rose.AST as AST
import Language.Rose.Lex (Token(..))
import qualified Language.Rose.Lex as Lex
}

%name parse
%tokentype { Token }
%error { error . show }

%token
  namespace           { Namespace }
  unchecked           { Unchecked }
  virtual             { Lex.Virtual }
  module              { Module }
  static              { Lex.Static }
  class               { Class }
  field               { Field }
  using               { Using }
  cast                { Cast }
  else                { Else }
  then                { Then }
  void                { Void }
  end                 { End }
  let                 { Let }
  new                 { New }
  fn                  { Fn }
  if                  { If }
  in                  { In }
  is                  { Is }

  identifier          { Identifier $$ }

  '=>'                { ThickArrow }
  '->'                { ThinArrow }
  '['                 { BracketLeft }
  ']'                 { BracketRight }
  ':'                 { Colon }
  ','                 { Comma }
  '='                 { Equals }
  '('                 { ParenLeft }
  ')'                 { ParenRight }
  '.'                 { Period }
  '+'                 { Plus }
  '?'                 { QuestionMark }
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

ClassDecl : class identifier TypeParamList is ClassMemberDecls end ';'
              { ClassDecl $2 $3 Nothing [] $5 }

ModuleDecl : module identifier is ClassMemberDecls end ';' { ModuleDecl $2 $4 }



ClassMemberDecl : CtorClassMemberDecl { $1 }
                | FnClassMemberDecl   { $1 }

CtorClassMemberDecl : new '(' CtorParams ')' ';'
                        { CtorClassMemberDecl $3 }

FnClassMemberDecl : SFV fn identifier TypeParamList ValueParamList ':' TypeExpr is Expr ';'
                      { FnClassMemberDecl $1 $3 $4 $5 $7 $9 }



TypeExpr : AppliedTypeExpr  { $1 }
         | NameTypeExpr     { $1 }
         | VoidTypeExpr     { $1 }
         | FnTypeExpr       { $1 }
         | NullableTypeExpr { $1 }

AppliedTypeExpr : NameTypeExpr '[' TypeExprs ']' { AppliedTypeExpr $1 $3 }

NameTypeExpr : QualifiedName { NameTypeExpr $1 }

VoidTypeExpr : void { VoidTypeExpr }

FnTypeExpr : fn '(' TypeExprs ')' '=>' TypeExpr { FnTypeExpr $3 $6 }

NullableTypeExpr : '?' TypeExpr { NullableTypeExpr $2 }



Expr : LambdaExpr { $1 }

LambdaExpr : fn '(' Identifiers ')' '=>' Expr { LambdaExpr $3 $6 }
           | let identifier '=' Expr in Expr { LetExpr $2 $4 $6 }
           | if Expr then Expr else Expr { IfExpr $2 $4 $6 }
           | EqExpr { $1 }

EqExpr : CallExpr '=' CallExpr { BinaryExpr "===" $1 $3 }
       | CallExpr              { $1 }

CallExpr : PrimExpr { $1 }
         | CallExpr ValueArgList { CallExpr $1 $2 }
         | CallExpr '.' identifier { InstanceMethodExpr $1 $3 }
         | CallExpr '->' identifier { InstanceVariableExpr $1 $3 }

PrimExpr : NameExpr         { $1 }
         | StaticMethodExpr { $1 }
         | NewExpr          { $1 }
         | CastExpr         { $1 }
         | UncheckedExpr    { $1 }

NameExpr : QualifiedName { NameExpr $1 }

StaticMethodExpr : QualifiedName ':' identifier { StaticMethodExpr $1 $3 }

NewExpr : new TypeExpr ValueArgList { NewExpr $2 $3 }

CastExpr : cast '[' TypeExpr ']' '(' Expr ')' { CastExpr $3 $6 }

UncheckedExpr : unchecked '(' Expr ')' { UncheckedExpr $3 }



Identifiers :                            { [] }
            | identifier                 { [$1] }
            | identifier ',' Identifiers { $1 : $3 }

SFV : static  { AST.Static }
    |         { Final }
    | virtual { AST.Virtual }

TypeParamList :                    { [] }
              | '[' TypeParams ']' { $2 }

TypeParams : TypeParam                { [$1] }
           | TypeParam ',' TypeParams { $1 : $3 }

TypeParam : Variance identifier { TypeParam $1 $2 }

Variance : '+' { Covariant }
         |     { Invariant }

ClassMemberDecls :                                  { [] }
                 | ClassMemberDecl ClassMemberDecls { $1 : $2 }

ValueArgList : '(' ValueArgs ')' { $2 }

ValueArgs :                        { [] }
          | ValueArg               { [$1] }
          | ValueArg ',' ValueArgs { $1 : $3 }

ValueArg : Expr { $1 }

CtorParams :                          { [] }
           | CtorParam                { [$1] }
           | CtorParam ',' CtorParams { $1 : $3 }

CtorParam : identifier ':' TypeExpr       { (False, $1, $3) }
          | field identifier ':' TypeExpr { (True, $2, $4) }

ValueParamList : '(' ValueParams ')' { $2 }

ValueParams :                            { [] }
            | ValueParam                 { [$1] }
            | ValueParam ',' ValueParams { $1 : $3 }

ValueParam : identifier ':' TypeExpr { ($1, $3) }

TypeExprs :                        { [] }
          | TypeExpr               { [$1] }
          | TypeExpr ',' TypeExprs { $1 : $3 }

NamespaceName : identifier                   { [$1] }
              | identifier '~' NamespaceName { $1 : $3 }

QualifiedName : NamespaceName
                  { QualifiedName (if null (init $1) then Nothing else Just (init $1))
                                  (last $1) }
              | '~' NamespaceName { QualifiedName (Just (init $2)) (last $2) }
