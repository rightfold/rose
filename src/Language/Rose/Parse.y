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
  is                  { Is }
  namespace           { Namespace }
  using               { Using }

  identifier          { Identifier $$ }

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

ClassDecl : class identifier is end ';' { ClassDecl $2 Nothing [] [] }

NamespaceName : identifier                   { [$1] }
              | identifier '~' NamespaceName { $1 : $3 }
