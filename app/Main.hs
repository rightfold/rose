module Main where

import Language.Rose.Lex (alexScanTokens)
import Language.Rose.Parse (parse)

main :: IO ()
main = print . parse . alexScanTokens $ code
  where code = "module Main { }"
