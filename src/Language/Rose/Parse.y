{
module Language.Rose.Parse
( parse
) where

import Language.Rose.AST (ModuleDecl(ModuleDecl))
import Language.Rose.Lex (Token(..))
}

%name parse
%tokentype { Token }
%error { error . show }

%token
  module              { Module }

  identifier          { Identifier $$ }

  '{'                 { LeftBrace }
  '}'                 { RightBrace }
  '='                 { Equals }
  ';'                 { Semicolon }

%%

ModuleDecl : module identifier '{' '}' { ModuleDecl $2 [] }
