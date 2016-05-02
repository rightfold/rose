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
               \  fn f(x: bool): void is\n\
               \    x;\n\
               \  fn f(x: int): T is\n\
               \    x;\n\
               \  fn f(x: string): N~T is\n\
               \    x;\n\
               \  fn f(x: mixed): ~N~T is\n\
               \    x;\n\
               \  fn f(x: float): ~T is\n\
               \    x;\n\
               \end;\n"
