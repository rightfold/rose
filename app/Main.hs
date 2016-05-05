module Main where

import Language.Rose.Convert (convert)
import Language.Rose.Lex (alexScanTokens)
import Language.Rose.Parse (parse)

main :: IO ()
main = interact $ convert . parse . alexScanTokens
