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

-- | Indentation sensitive parser
type IParse r = ParsecT String () (State SourcePos) r

-- | The type for HandleSpecialCommand: Takes a name, arguments and returns
-- a new parser 
type HSP = String -> [(String, String)] -> IParse DocumentItem

-- | A command is an entity in a document that will cause the backend renderer
-- to perform a special operation. Commands in this markup language take the
-- general form [command argument=value, argument2=value2]. Therefore, a Command
-- is a tuple consisting of the command name and the command arguments.
type Command = (String, [(String, String)])

-- | Parse a udoc document and return the document items.
parseDocument :: HSP -> String -> [DocumentItem]
parseDocument hsp input = 
   forceEither $ myParse id (documentItems hsp) input
   where myParse f p src = fst $ flip runState (f $ initialPos "") $ 
                                      runParserT p () "" src

-- | Skips zero or more space or tab characters.
spaces :: IParse ()
spaces = skipMany $ oneOf " \t"

-- | Skipts zero of more space, tab and newline characters.
whiteSpace :: IParse ()
whiteSpace = skipMany $ oneOf " \t\n"

-- | Top level parser. It will parse headings or paragraphs until
-- eof is found.
documentItems :: HSP -> IParse [DocumentItem]
documentItems hsp = do whiteSpace
                       items <- many ( (try (heading hsp)) <|> paragraph hsp)
                       whiteSpace
                       eof
                       return items

-- | A string between two double quotes.
quotedContent :: IParse String
quotedContent =
   between (char '"') (char '"') (many1 (wordChar <|> char ' '))

-- | A command parameter. This is a string of the form: key = value. The
-- value may be quoted.
commandParameter :: IParse (String, String)
commandParameter = do whiteSpace
                      name <- many1 alphaNum
                      spaces
                      char '='
                      spaces
                      value <- quotedContent <|> (many1 $ noneOf "\n ,\t\"[]")
                      whiteSpace
                      return (name, value)

-- | The inner part of a command. This matches a string of the form:
-- command key=value.
command :: IParse Command
command = do 
   whiteSpace
   commandName <- many1 (alphaNum <|> char '/')
   whiteSpace
   params <- commandParameter `sepBy` (char ',')
   whiteSpace
   return (commandName, params)


-- | A command surrounded by square brackets. 
squareBrCommand :: IParse Command
squareBrCommand = 
   between (char '[') (char ']') (do ret <- command; spaces; return ret)

-- | Some text surrounded by curly brackets. This will internally be
-- represented as a command called "_s" (for source code)
inlineSource :: IParse Command
inlineSource = do (char '{' >> return ())
                  return ("_s", [])

-- | Looks up a list of keys from an association list. If all keys are found,
-- returns Just [values]. Otherwise, returns nothing.
getMandatory ::   (Eq a) => 
                  [a] -- ^ Keys to be looked up
               -> [(a, b)] -- ^ An association list
               -> Maybe [b] -- ^ The result
getMandatory keys aList = mapM ((flip lookup) aList) keys

-- | Looks up a list of keys from an association list. If at least one of the
-- keys are not found, this runction returns Nothing. Otherwise, it will return
-- Just a new association list. The keys in this new association list will
-- however not be the original keys that were looked up. Instead, the lookup
-- keys will be renamed.
mandRename ::   (Eq a) => 
                [(a, b)] -- ^ A pair: (lookup key, new key)
             -> [(a, c)] -- ^ An association list
             -> Maybe [(b, c)] -- ^ The result with the lookup keys renamed
mandRename mandKeyName aList =
   zip <$> (Just $ map snd mandKeyName) <*> 
           (getMandatory (map fst mandKeyName) aList)

-- | Looks up a list of keys from an association list and builds a new
-- association list, holding the found values. The keys in the new list are
-- obtained by renaming the old keys.
optRename ::    (Eq a) => 
                [(a, b)] -- ^ A pair: (lookup key, new key)
             -> [(a, c)] -- ^ An association list
             -> [(b, c)] -- ^ The result with the lookup keys renamed
optRename optKeyName aList =
   let values = map ((flip lookup) aList) (map fst optKeyName)
       -- getJust (Just 3, Just 3) = Just (3, 4)
       -- getJust (Nothing, Just 2) = Nothing
       getJust x = ((,)) <$> fst x <*> snd x
   in
   catMaybes $ map getJust (zip (map (Just . snd) optKeyName) values)

getArgumentsOrFail ::    (Monad m, Eq a, Show a) => 
                         [(a, b)] -- ^ A renaming pair for the mandatory args
                      -> [(a, b)] -- ^ A renaming pair for the optional args
                      -> [(a, c)] -- ^ An association list
                      -> ([(b,c)] -> [(b,c)] -> d) -- ^ A function for handling
                                          -- the resulting lists of mandatory
                                          -- and optional arguments
                      -> m d -- ^ The result
getArgumentsOrFail mandArgs optArgs aList f =
   let mand = mandRename mandArgs aList
       opt  = optRename optArgs aList
   in
   case mand of
      Nothing  -> fail $ "Missing argument. Epecting: " ++ 
                        (show $ map fst mandArgs) ++ 
                        " but only got " ++ (show $ map fst aList)
      Just m'  -> return $ f m' opt

-- | Creates an ItemMetaTag from the following data: a tag name, the list
-- of mandatory properties and the list of optional properties.
createMetaTag ::    String -- ^ The name of the tag type
                 -> [(String, String)] -- ^ The mandatory properties
                 -> [(String, String)] -- ^ The optional properties
                 -> DocumentItem -- ^ The resulting document item
createMetaTag t mprops oprops = ItemMetaTag $ [("type", t)] ++ mprops ++ oprops

-- | Tries to create a meta tag specified by is name. To create the tag, this
-- function will lookup all mandatory and optional arguments from the argument
-- list that has been supplied. If this worked out, it will return an
-- ItemMetaTag.
handleMetaTag ::    (Monad m, Show a, Eq a) => 
                    String -- ^ The name of the tag type
                 -> [(a, String)] -- ^ Mandatory tag arguments
                 -> [(a, String)] -- ^ Optional tag arguments
                 -> [(a, String)] -- ^ The parsed tag arguments
                 -> m DocumentItem -- ^ The resulting item
handleMetaTag t mand opt aList = 
   getArgumentsOrFail mand opt aList (createMetaTag t)

-- | Checks that all required arguments are present and then creates the
-- meta tag with the type label.
handleLab :: [(String, String)] -> IParse DocumentItem
handleLab = handleMetaTag "label" [("name", "name")] []

-- | Checks that all required arguments are present and then creates the
-- meta tag with the type ref.
handleRef :: [(String, String)] -> IParse DocumentItem
handleRef = handleMetaTag "ref" [("label", "label")] []

-- | Checks that all required arguments are present and then creates the
-- meta tag with the type imgref.
handleImgRef :: [(String, String)] -> IParse DocumentItem
handleImgRef = handleMetaTag "imgref" [("label", "label")] []

-- | Checks that all required arguments are present and then creates the
-- meta tag with the type tblref.
handleTblRef :: [(String, String)] -> IParse DocumentItem
handleTblRef = handleMetaTag "tblref" [("label", "label")] []

createMetaContainer t props content = 
   ItemDocumentContainer $ DocumentMetaContainer ([("type", t)] ++ props) content

-- | A command is either inlineSource or a regular command surrounded
-- by square brackets.
extendedCommand' :: IParse Command
extendedCommand' = inlineSource <|> squareBrCommand

-- | This parses a command and handles it accordingly.
extendedCommand :: HSP -> IParse DocumentItem
extendedCommand hsp = do
   (name, args) <- (spaces >> extendedCommand')
   result <- handleExtendedCommand name args hsp
   return result

-- | This works as some sort of lookahead: it expects a given command. However,
-- it will not consume any input.
extendedCommandName :: String -> IParse ()
extendedCommandName name = try (do (name', args) <- extendedCommand'
                                   if name == name' 
                                     then do return ()
                                     else do fail ""
                               ) <?> "the tag [" ++ name ++ "]"

-- | If the last item in a list of items is a word, this function will remove
-- the last trailing newline of this word.
removeTrailingNewline :: [DocumentItem] -> [DocumentItem]
removeTrailingNewline items =
   init items ++ nl (last items)
   where nl (ItemWord w) = [ItemWord $ stripLastNewline w]
         nl x = [x]

-- | This function removes the last trailing newline character from a string.
stripLastNewline :: String -> String
stripLastNewline s
   | last s == '\n' = init s
   | otherwise = s

-- | Handle an extended command. This is called once a command
-- has been found. It's responsible for returning the appropriate
-- data structure for the parse tree.
handleExtendedCommand ::    String -- ^ The command name
                         -> [(String, String)] -- ^ The command's arguments
                         -> HSP -- ^ The supported special commands
                         -> IParse DocumentItem 
handleExtendedCommand name args handleSpecialCommand =
   case name of
      "b"      -> do skipEmptyLines
                     bold <- containerContentsUntil (extendedCommandName "/b") 
                                                    handleSpecialCommand
                     return $ ItemDocumentContainer $ DocumentBoldFace bold
      "br"     -> return $ ItemLinebreak
      "meta"   -> return $ ItemMetaTag args
      "pb"     -> return $ ItemMetaTag [("type", "pagebreak")]
      "image"  -> do if (isNothing(lookup "label" args) || isNothing(lookup "path" args) || isNothing(lookup "caption" args))
                        then fail "Image tag needs label caption and path"
                        else return $ ItemImage (Image (fromJust (lookup "path"    args))
                                                 (fromJust (lookup "caption" args))
                                                 (fromJust (lookup "label"   args))
                                                )
      "table"  -> do let mCL = ((,)) <$> lookup "caption" args <*> lookup "label" args
                     let style = fromMaybe "head_top" $ lookup "style" args
                     skipEmptyLines
                     rows <- manyTill (do r <- tableRow handleSpecialCommand; skipMany1 $ char '\n'; return r) 
                                      (extendedCommandName "/table")
                     return $ ItemDocumentContainer $ DocumentTable style mCL rows
      "_s"     -> do source <- manyTill inlineVerbatimContent (char '}' >> spaces)
                     return $ ItemDocumentContainer $ DocumentMetaContainer ([("type", "inlineSource")]) source
      "source" -> do skipEmptyLines
                     source <- manyTill (verbatimContent "[/source]") (extendedCommandName "/source")
                     return $ ItemDocumentContainer $ DocumentMetaContainer ([("type", "source")]) (removeTrailingNewline source)
      "label"  -> handleLab args
      "ref"    -> handleRef args
      "imgref" -> handleImgRef args
      "tblref" -> handleTblRef args
      -- If we end up here, we can at least check if the
      -- identified command is some special-purpose
      -- command
      _         -> handleSpecialCommand name args


-- | Simply parse a paragraph.
paragraph :: HSP -> IParse DocumentItem
paragraph = paragraphWithout parserZero

-- | Parse a paragraph. This Paragraph could contain a list of words or lists or
-- special commands. However, this parser will fail if parser x succeeds.
paragraphWithout ::    IParse () -- ^ The parser that may not succeed
                    -> HSP -- ^ The supported special commands
                    -> IParse DocumentItem
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

-- | Parses escaped character sequences (such as \[) and returns the according
-- character.
escaped :: Char -> IParse Char
escaped x = (string ("\\"++[x])) >> return x

-- | Parses one character in a regular word of the text.
wordChar :: IParse Char
wordChar =     (try $ escaped '[')
           <|> (try $ escaped ']')
           <|> (try $ escaped '|')
           <|> (try $ escaped '{')
           <|> (try $ escaped '}')
           <|> (try $ escaped '"')
           <|> (try $ escaped '\\')
           <|> noneOf " \t\n[]|{}\""
           <?> "a valid word character"

-- | Parses a whole word in the text of the document.
word :: IParse DocumentItem
word = do result <- many1 wordChar
          spaces
          return $ ItemWord result

-- | The contents of a table row, i.e. some text separated by the pipe 
-- character.
cellContent :: HSP -> IParse [DocumentItem]
cellContent hsp = do
   x <- many $ containerContentOneLine hsp
   return $ x

-- | A row of a table
tableRow :: HSP -> IParse DocumentContainer
tableRow hsp = do 
   row <- (cellContent hsp) `sepBy` pipe
   return $ DocumentTableRow (row)
   where pipe = ((skipMany $ char ' ') >> char '|' >> (skipMany $ char ' '))

-- | Contents of a source code section. Note that spaces, newlines and
-- tabs will be left as they are. However, the bold tag is supported!
verbatimContent :: String -> IParse DocumentItem
verbatimContent closingTag = 
   try (verbatimBold closingTag) <|> (verbatimText closingTag)

-- | Helper for bold text in verbatim content
verbatimBold ::   String -- ^ The tag that will close the verbatim section.
                         -- could be [/code] for example.
               -> IParse DocumentItem
verbatimBold closingTag = do
   string "[b]"
   bold <- manyTill (verbatimText closingTag) (extendedCommandName "/b")
   return $ ItemDocumentContainer $ DocumentBoldFace (bold)

-- | Contents of the verbatim container. That might be everything, but
-- we handle [b] and [/b], in order to allow for bold parts in source
-- code.
verbatimText :: String -> IParse DocumentItem
verbatimText closingTag = do 
   result <- many1 (wordChar 
                    <|> oneOf "\n\t |{}\"]"
                    <|> ( (notFollowedBy $ string closingTag) >>
                          (notFollowedBy $ string "[b]") >>
                          (notFollowedBy $ string "[/b]") >>
                          (char '[')
                        )
                    )
   return $ ItemWord result

-- | Contents of the inline verbatim container - may basically be everything
-- besides { or }, which need to be escaped.
inlineVerbatimContent :: IParse DocumentItem
inlineVerbatimContent = do 
   result <- many1 (wordChar <|> oneOf "\n\t |\"[]")
   return $ ItemWord result

-- | The start of the regular line of text. This is only a lookahead, which
-- will not match lists or headings.
beginRegularLine :: IParse ()
beginRegularLine =
   (notFollowedBy $ listItemBegin) >>
   (notFollowedBy $ headingBegin)

-- | One line of text
line :: HSP -> IParse [DocumentItem]
line hsp = 
   spaces >> beginRegularLine >> (many1 word)

-- | An empty line of text
emptyline :: IParse ()
emptyline = do spaces
               (char '\n' >> return ())
               return ()

-- | Skipts all empty lines of text
skipEmptyLines :: IParse ()
skipEmptyLines = skipMany (try emptyline)

-- | This basically matches anything but paragraphs and headings
-- It will parse one item, but it will fail on newline
containerContentOneLine :: HSP -> IParse DocumentItem
containerContentOneLine hsp =
          (try $ extendedCommand hsp)
      <|> (try $ uList hsp)
      <|> (try $ oList hsp)
      <|> (beginRegularLine >> word)

-- | If the last item in l is a Paragraph, then remove this paragraph
-- and replace it by its plain contents.
removeLastPara :: [DocumentItem] -> [DocumentItem]
removeLastPara [] = []
removeLastPara l = 
   let rev = reverse l
       fst = removePara $ head rev
   in
   reverse ((reverse fst) ++ (tail rev))

-- | If the supplied DocumentItem is a DocumentParagraph, then just
-- return the Paragraph contents. Otherwise simply return the DocumentItem
-- that has been provided.
removePara :: DocumentItem -> [DocumentItem]
removePara (ItemDocumentContainer (DocumentParagraph l)) = l
removePara x = [x]

-- | Read container content until parser x succeeds. This will generally
-- return a list of DocumentItems, potentially containing Paragraphs.
-- If the last item in the list is a Paragraph, it will be replaced by
-- its contents.
containerContentsUntil ::    IParse() -- ^ The parser that may not succeed
                          -> HSP -- ^ The supported special commands
                          -> IParse [DocumentItem]
containerContentsUntil x hsp = do
   contents <- manyTill (containerContentWithout x hsp) (x)
   return $ removeLastPara contents

containerContentWithout x hsp = notFollowedBy x >> paragraphWithout x hsp

containerContentBlock hsp = do
   x <- many1 $ do
      l <- containerContentOneLine hsp
      spaces
      return l
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
