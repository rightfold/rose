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
  cast                { const Cast }
  else                { const Else }
  then                { const Then }
  void                { const Void }
  end                 { const End }
  let                 { const Let }
  new                 { const New }
  fn                  { const Fn }
  if                  { const If }
  is                  { const Is }
  in                  { const In }

  [a-zA-Z_]+          { Identifier }

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
  \?                  { const QuestionMark }
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
  | Cast
  | Else
  | Then
  | Void
  | End
  | Let
  | New
  | Fn
  | If
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
  | QuestionMark
  | Semicolon
  | Tilde
  deriving (Eq, Show)
}
