{
module Language.Rose.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;

  class               { const Class }
  end                 { const End }
  fn                  { const Fn }
  is                  { const Is }
  namespace           { const Namespace }
  using               { const Using }
  void                { const Void }

  [a-zA-Z]+           { Identifier }

  \:                  { const Colon }
  \(                  { const ParenLeft }
  \)                  { const ParenRight }
  \;                  { const Semicolon }
  \~                  { const Tilde }

{
data Token
  = Class
  | End
  | Fn
  | Is
  | Namespace
  | Using
  | Void

  | Identifier String

  | Colon
  | ParenLeft
  | ParenRight
  | Semicolon
  | Tilde
  deriving (Eq, Show)
}
