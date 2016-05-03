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
  module              { const Module }
  namespace           { const Namespace }
  using               { const Using }
  void                { const Void }

  [a-zA-Z]+           { Identifier }

  \:                  { const Colon }
  \,                  { const Comma }
  \(                  { const ParenLeft }
  \)                  { const ParenRight }
  \.                  { const Period }
  \;                  { const Semicolon }
  \~                  { const Tilde }

{
data Token
  = Class
  | End
  | Fn
  | Is
  | Module
  | Namespace
  | Using
  | Void

  | Identifier String

  | Colon
  | Comma
  | ParenLeft
  | ParenRight
  | Period
  | Semicolon
  | Tilde
  deriving (Eq, Show)
}
