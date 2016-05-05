{
module Language.Rose.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;
  \-\-.*              ;

  namespace           { const Namespace }
  virtual             { const Virtual }
  module              { const Module }
  static              { const Static }
  class               { const Class }
  field               { const Field }
  using               { const Using }
  void                { const Void }
  end                 { const End }
  let                 { const Let }
  new                 { const New }
  fn                  { const Fn }
  is                  { const Is }
  in                  { const In }

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
  = Namespace
  | Virtual
  | Module
  | Static
  | Class
  | Field
  | Using
  | Void
  | End
  | Let
  | New
  | Fn
  | Is
  | In

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
