{
module Language.Rose.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;

  namespace           { const Namespace }
  module              { const Module }
  class               { const Class }
  using               { const Using }
  void                { const Void }
  end                 { const End }
  fn                  { const Fn }
  is                  { const Is }

  [a-zA-Z]+           { Identifier }

  \=\>                { const ThickArrow }
  \-\>                { const ThinArrow }
  \[                  { const BracketLeft }
  \]                  { const BracketRight }
  \:                  { const Colon }
  \,                  { const Comma }
  \=                  { const Equals }
  \(                  { const ParenLeft }
  \)                  { const ParenRight }
  \.                  { const Period }
  \+                  { const Plus }
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

  | ThickArrow
  | ThinArrow
  | BracketLeft
  | BracketRight
  | Colon
  | Comma
  | Equals
  | ParenLeft
  | ParenRight
  | Period
  | Plus
  | Semicolon
  | Tilde
  deriving (Eq, Show)
}
