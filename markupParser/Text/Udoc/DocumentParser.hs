{-
 - ----------------------------------------------------------------------------
 - "THE BEER-WARE LICENSE" (Revision 42):
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you
 - can do whatever you want with this stuff. If we meet some day, and you
 - think this stuff is worth it, you can buy me a beer in return Gregor Kopf
 - ----------------------------------------------------------------------------
 -}

module Text.Udoc.DocumentParser where

import Text.ParserCombinators.Parsec hiding (State, try, spaces)
import Text.Parsec.Prim (parserZero, runParserT, ParsecT, try)
import Control.Monad
import Control.Monad.State
import Text.JSON
import Data.Either
import Data.Either.Utils
import Text.Udoc.Document
import Data.Maybe
import Data.Functor.Identity
import Text.Parsec.Indent
import Text.Parsec.Pos
import Control.Applicative hiding ((<|>), many, optional)

-- Indentation sensitive parser
type IParse r = ParsecT String () (State SourcePos) r
-- The type for HandleSpecialCommand: Takes a name, arguments and returns
-- a new parser 
type HSP = String -> [(String, String)] -> IParse DocumentItem

myParse f p src = fst $ flip runState (f $ initialPos "") $ runParserT p () "" src
parseDocument hsp input = forceEither $ myParse id (documentItems hsp) input

spaces = skipMany $ oneOf " \t"
whiteSpace = skipMany $ oneOf " \t\n"

-- Top level parser. It will parse headings or paragraphs until
-- eof is found.
documentItems :: HSP -> IParse [DocumentItem]
documentItems hsp = do whiteSpace
                       items <- many ( (try (heading hsp)) <|> paragraph hsp)
                       whiteSpace
                       eof
                       return items

quotedContent =
   between (char '"') (char '"') (many1 (wordChar <|> char ' '))

commandParameter = do whiteSpace
                      name <- many1 alphaNum
                      spaces
                      char '='
                      spaces
                      value <- quotedContent <|> (many1 $ noneOf "\n ,\t\"[]")
                      whiteSpace
                      return (name, value)

-- A command surrounded by square brackets. This is the default
-- form.
command = do 
   whiteSpace
   commandName <- many1 (alphaNum <|> char '/')
   whiteSpace
   params <- commandParameter `sepBy` (char ',')
   whiteSpace
   return (commandName, params)

squareBrCommand = between (char '[') (char ']') (do ret <- command; spaces; return ret)

-- Some text surrounded by curly brackets. This will internally be
-- represented as a command called "_s" (for source code)
inlineSource = do (char '{' >> return ())
                  return ("_s", [])

{-
comment = do (string "/*" >> return ())
             (many $ noneOf "" >> return ())
             (string "*/" >> return ())
-}

-- Helper functions for working with association lists
getMandatory keys aList = mapM ((flip lookup) aList) keys
mandRename mandKeyName aList =
   zip <$> (Just $ map snd mandKeyName) <*> (getMandatory (map fst mandKeyName) aList)

optRename optKeyName aList =
   let values = map ((flip lookup) aList) (map fst optKeyName)
       getJust ((Just x, Just y)) = Just (x,y)
       getJust (Nothing, _) = Nothing
       getJust (_, Nothing) = Nothing
   in
   catMaybes $ map getJust (zip (map (Just . snd) optKeyName) values)

getArgumentsOrFail mandArgs optArgs aList f =
   let mand = mandRename mandArgs aList
       opt  = optRename optArgs aList
   in
   if (isNothing mand) 
      then fail $ "Missing argument. Epecting: " ++ (show $ map fst mandArgs) ++ " but only got " ++ (show $ map fst aList)
      else do return $ f (fromJust mand) opt

createMetaTag t mprops oprops = ItemMetaTag $ [("type", t)] ++ mprops ++ oprops
handleMetaTag t mand opt aList = getArgumentsOrFail mand opt aList (createMetaTag t)

handleLab = handleMetaTag "label" [("name", "name")] []
handleRef = handleMetaTag "ref" [("label", "label")] []
handleImgRef = handleMetaTag "imgref" [("label", "label")] []
createMetaContainer t props content = 
   ItemDocumentContainer $ DocumentMetaContainer ([("type", t)] ++ props) content

-- A command is either inlineSource or a regular command surrounded
-- by square brackets.
extendedCommand' = inlineSource <|> squareBrCommand
extendedCommand hsp = do
   (name, args) <- (spaces >> extendedCommand')
   result <- handleExtendedCommand name args hsp
   return result

-- Expects a command with a given name
extendedCommandName name = try (do (name', args) <- extendedCommand'
                                   if name == name' 
                                     then do return ()
                                     else do fail ""
                               ) <?> "the tag [" ++ name ++ "]"

removeTrailingNewline items =
   init items ++ nl (last items)
   where nl (ItemWord w) = [ItemWord $ stripLastNewline w]
         nl x = [x]

stripLastNewline s
   | last s == '\n' = init s
   | otherwise = s

-- Handle an extended command. This is called once a command
-- has been found. It's responsible for returning the appropriate
-- data structure for the parse tree.
handleExtendedCommand name args handleSpecialCommand =
   case name of
      "b"     -> do skipEmptyLines
                    bold <- containerContentsUntil (extendedCommandName "/b") handleSpecialCommand
                    return $ ItemDocumentContainer $ DocumentBoldFace bold
      "br"    -> do return $ ItemLinebreak
      "meta"  -> do return $ ItemMetaTag args
      "pb"    -> do return $ ItemMetaTag [("type", "pagebreak")]
      "image" -> do return $ ItemImage (Image (fromJust (lookup "path"    args))
                                              (fromJust (lookup "caption" args))
                                              (fromJust (lookup "label"   args))
                                       )
      "table" -> do skipEmptyLines
                    rows <- manyTill (do r <- tableRow handleSpecialCommand; skipMany1 $ char '\n'; return r) 
                                     (extendedCommandName "/table")
                    return $ ItemDocumentContainer $ DocumentTable rows
      "_s"    -> do source <- manyTill inlineVerbatimContent (char '}' >> spaces)
                    return $ ItemDocumentContainer $ DocumentMetaContainer ([("type", "inlineSource")]) source
      "source" -> do skipEmptyLines
                     source <- manyTill (verbatimContent) (extendedCommandName "/source")
                     return $ ItemDocumentContainer $ DocumentMetaContainer ([("type", "source")]) (removeTrailingNewline source)
      "label" -> handleLab args
      "ref"   -> handleRef args
      "imgref" -> handleImgRef args

      -- If we end up here, we can at least check if the
      -- identified command is some special-purpose
      -- command
      _       -> handleSpecialCommand name args


-- Simply parse a paragraph.
paragraph = paragraphWithout parserZero

-- Parse a paragraph. This Paragraph could contain a list of words, lists or
-- special commands. However, this parser will fail if parser x succeeds.
paragraphWithout :: IParse () -> HSP -> IParse DocumentItem
paragraphWithout x hsp = do 
   lines <- many1 $ (  notFollowedBy x >> 
                       (
                             (do x <- try $ line hsp; optional newline; return x)
                         <|> (do x <- try $ extendedCommand hsp; optional newline; return [x])
                         <|> (do x <- try $ uList hsp; return [x])
                         <|> (do x <- try $ oList hsp; return [x])
                       )
                    )
   skipMany $ try emptyline
   return $ ItemDocumentContainer $ DocumentParagraph $ concat lines

-- Valid characters for words
escaped x = (string ("\\"++[x])) >> return x
wordChar =     (try $ escaped '[')
           <|> (try $ escaped ']')
           <|> (try $ escaped '|')
           <|> (try $ escaped '{')
           <|> (try $ escaped '}')
           <|> (try $ escaped '"')
           <|> (try $ escaped '\\')
           <|> noneOf " \t\n[]|{}\""
           <?> "a valid word character"

word = do result <- many1 wordChar
          spaces
          return $ ItemWord result

-- The contents of a table row, i.e. some text separated by the pipe character.
-- XXX: was: many word
cellContent hsp = do
   x <- many $ containerContentOneLine hsp
   return $ x

tableRow hsp = do 
   row <- (cellContent hsp) `sepBy` ((skipMany $ char ' ') >> char '|' >> (skipMany $ char ' '))
   return $ DocumentTableRow (row)

-- Contents of a source code section. Note that spaces, newlines and
-- tabs will be left as they are. 
verbatimContent = try verbatimBold <|> verbatimText

-- Helper for bold text in verbatim content
verbatimBold = do
   string "[b]"
   bold <- manyTill verbatimText (extendedCommandName "/b")
   return $ ItemDocumentContainer $ DocumentBoldFace (bold)

-- Contents of the verbatim container. That might be everything, but
-- we handle [b] and [/b], in order to allow for bold parts in source
-- code.
verbatimText = do 
   result <- many1 (wordChar 
                    <|> oneOf "\n\t |{}\"]"
                    <|> ( (notFollowedBy $ string "[/source]") >>
                          (notFollowedBy $ string "[b]") >>
                          (notFollowedBy $ string "[/b]") >>
                          (char '[')
                        )
                    )
   return $ ItemWord result

-- Contents of the inline verbatim container - may basically be everything
-- besides { or }, which need to be escaped.
inlineVerbatimContent = do 
   result <- many1 (wordChar <|> oneOf "\n\t |\"[]")
   return $ ItemWord result

-- One line of text. It will not match lists or headings.

beginRegularLine =
   (notFollowedBy $ listItemBegin) >>
   (notFollowedBy $ headingBegin)

line hsp = 
   spaces >> beginRegularLine >> (many1 word)

-- An empty line
emptyline :: IParse ()
emptyline = do spaces
               (char '\n' >> return ())
               return ()

skipEmptyLines :: IParse ()
skipEmptyLines = skipMany (try emptyline)

-- This basically matches anything but paragraphs and headings
-- It will parse one item, but it will fail on newline
containerContentOneLine :: HSP -> IParse DocumentItem
containerContentOneLine hsp =
          (try $ extendedCommand hsp)
      <|> (try $ uList hsp)
      <|> (try $ oList hsp)
      <|> (beginRegularLine >> word)

-- If the last item in l is a Paragraph, then remove this paragraph
-- and replace it by its plain contents.
removeLastPara [] = []
removeLastPara l = 
   let rev = reverse l
       fst = removePara $ head rev
   in
   reverse ((reverse fst) ++ (tail rev))

-- If the supplied DocumentItem is a DocumentParagraph, then just
-- return the Paragraph contents. Otherwise simply return the DocumentItem
-- that has been provided.
removePara :: DocumentItem -> [DocumentItem]
removePara (ItemDocumentContainer (DocumentParagraph l)) = l
removePara x = [x]

-- Read container content until parser x succeeds. This will generally
-- return a list of DocumentItems, potentially containing Paragraphs.
-- If the last item in the list is a Paragraph, it will be replaced by
-- its contents.
containerContentsUntil x hsp = do
   contents <- manyTill (containerContentWithout x hsp) (x)
   return $ removeLastPara contents

containerContentWithout x hsp = notFollowedBy x >> paragraphWithout x hsp

containerContentBlock hsp = do
   x <- many1 (containerContentOneLine hsp)
   spaces
   optional newline
   spaces
   return x

-- A plain list (much like LaTeX itemize)
-- The syntax is simple:
-- * foo
-- * bar
--   * bla
-- * foo
uListItemBegin = do
      i <- many $ char ' '
      char '*'
      j <- many $ char ' '
      return $ ((length (i++j)) + 1)

uListItemBody hsp ind = do
      content <- block (containerContentBlock hsp)
      return $ (UListItem ind, (concat content))

uListItem hsp =
   do ind <- uListItemBegin
      uListItemBody hsp ind

uList hsp = do
   checkIndent
   lookAhead (uListItemBegin)
   result <- block $ uListItem hsp
   return $ ItemDocumentContainer (DocumentUList result)

-- A numbered list (much like LaTeX enumerate)
-- The syntax is:
-- 1. A
-- 1.1. B
-- 1.2) C
-- 2.) D
oListItemBegin = do
   i <- many $ char ' '
   number <- dottedNumber
   trailer <- ( (try $ string ".)") <|> string "." <|> string ")" <?> "a number in an enumerated list")
   spaces
   return ((length i), number)

oListItem hsp =
   do (ind, num) <- oListItemBegin
      content <- block $ (containerContentBlock hsp)
      return $ (OListItem ind num, (concat content))

oList hsp =
   do checkIndent
      lookAhead oListItemBegin
      items <- block $ oListItem hsp
      return $ ItemDocumentContainer (DocumentOList items)

-- A number like 1, 1.2 or 1.2.3.4
dottedNumber = do num <- many1 digit
                  rest <- try (char '.' >> dottedNumber) <|> return ""
                  if length rest == 0 then
                     return num
                   else
                      return (num ++ "." ++ rest)

listItemBegin = ((try uListItemBegin) >> return ()) <|> (oListItemBegin >> return ())
headingBegin = do
   spaces
   level <- (many1 $ char '#')
   spaces
   return $ length level

-- A heading. Currently, we do not do our indentation check and allow only
-- headings with one line.
heading :: HSP -> IParse DocumentItem
heading hsp = do level <- headingBegin
                 items <- block $ do
                              l <- line hsp
                              optional newline
                              spaces
                              return l
                 skipEmptyLines
                 return $ ItemDocumentContainer (DocumentHeading (Heading level Nothing) (concat items))
