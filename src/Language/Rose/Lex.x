{
module Language.Rose.Lex where
}

%wrapper "basic"

tokens :-
  $white              ;

  module              { const Module }

  [a-zA-Z]+           { Identifier }

  \{                  { const LeftBrace }
  \}                  { const RightBrace }
  \=                  { const Equals }
  \;                  { const Semicolon }

{
data Token
  = Module

  | Identifier String

  | LeftBrace
  | RightBrace
  | Equals
  | Semicolon
  deriving (Eq, Show)
}
