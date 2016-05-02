{
module Language.Rose.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;

  class               { const Class }
  end                 { const End }
  is                  { const Is }
  namespace           { const Namespace }
  using               { const Using }

  [a-zA-Z]+           { Identifier }

  \;                  { const Semicolon }
  \~                  { const Tilde }

{
data Token
  = Class
  | End
  | Is
  | Namespace
  | Using

  | Identifier String

  | Semicolon
  | Tilde
  deriving (Eq, Show)
}
