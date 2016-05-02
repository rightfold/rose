{
module Language.Rose.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;

  namespace           { const Namespace }
  using               { const Using }

  [a-zA-Z]+           { Identifier }

  \;                  { const Semicolon }
  \~                  { const Tilde }

{
data Token
  = Namespace
  | Using

  | Identifier String

  | Semicolon
  | Tilde
  deriving (Eq, Show)
}
