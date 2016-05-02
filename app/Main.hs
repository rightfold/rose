module Main where

import Language.Rose.Lex (alexScanTokens)
import Language.Rose.Parse (parse)

main :: IO ()
main = print . parse . alexScanTokens $ code
  where code = "namespace Hello~World~Test;\n\
               \using FH~IO;\n\
               \using Hello~World~Icle;\n\
               \using PDO;\n\
               \class IO is\n\
               \  fn f(): void is\n\
               \    x;\n\
               \end;\n"
