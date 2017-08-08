{-
 - ----------------------------------------------------------------------------
 - "THE BEER-WARE LICENSE" (Revision 42):
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you
 - can do whatever you want with this stuff. If we meet some day, and you
 - think this stuff is worth it, you can buy me a beer in return. Gregor Kopf
 - ----------------------------------------------------------------------------
 -}

import Text.Udoc.Document
import Text.JSON
import Text.Udoc.DocumentParser
import Text.Parsec.Prim
import Data.Functor.Identity

handleSpecialCommand name args =
   case name of
         "tableOfContents" -> return $ ItemMetaTag ([("type", "tableOfContents")])
         _ -> fail $ "Unknown command " ++ name

main = do input <- getContents
          case parseDocument (defaultParserState { parserStateFlavor = [BlockQuotes, FencedCodeBlocks] }) handleSpecialCommand input of
             Left err -> putStrLn $ show err
             Right (parsed, _) -> putStr $ encode parsed
