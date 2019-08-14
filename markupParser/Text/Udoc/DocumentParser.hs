{-
 - ----------------------------------------------------------------------------
 - "THE BEER-WARE LICENSE" (Revision 42):
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you
 - can do whatever you want with this stuff. If we meet some day, and you
 - think this stuff is worth it, you can buy me a beer in return. Gregor Kopf
 - ----------------------------------------------------------------------------
 -}

{-|
Module      : Text.Udoc.DocumentParser
Description : Udoc document parser
Copyright   : (c) Gregor Kopf, 2012
License     : BEER-WARE LICENSE (Revision 42)
Maintainer  : code@gregorkopf.de
Stability   : experimental

This module contains the implementation of the udoc parser.
-}

{-# LANGUAGE FlexibleContexts #-}

module Text.Udoc.DocumentParser where

import           Text.ParserCombinators.Parsec hiding (State, try, spaces)
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Error as PE
import           Text.Parsec.Prim (parserZero, runParserT, ParsecT, try)
import qualified Text.Parsec.Prim as PP
import           Control.Monad
import           Control.Monad.State
import           Text.JSON
import           Text.Udoc.Document
import           Data.Either
import           Data.Char
import           Data.Maybe
import           Data.Functor.Identity
import           Text.Parsec.Indent
import           Text.Parsec.Pos
import           Control.Applicative hiding ((<|>), many, optional)
import           Text.Read
import           Data.List

data SyntaxOption = SkipNewlinesAfterUlist
                  | SkipNewlinesAfterImage
                  | BacktickSource
                  | SkipNewlinesAfterSourceOrQuoteBlock
                  | BlockQuotes
                  | FencedCodeBlocks
                  | NewStyleOlist
                  deriving (Eq, Read, Show)

type SyntaxFlavor = [SyntaxOption]

data ParserState = ParserState {
    parserStateFlavor               :: SyntaxFlavor
  , parserStateLastInlineOpeningTag :: Char
}

-- | A parser state without any syntax options set
defaultParserState :: ParserState
defaultParserState = ParserState [] '{'

-- | Indentation sensitive parser
type IParse r = ParsecT String ParserState (State SourcePos) r

-- | The type for HandleSpecialCommand: Takes a name, arguments and returns
-- a new parser 
type HSP = String -> [(String, String)] -> IParse DocumentItem

-- | Returns whether or not a parsing option is set.
isOptionSet :: SyntaxOption -> IParse Bool
isOptionSet opt = (elem opt . parserStateFlavor) <$> P.getState

-- | Executes a parser if any only if a configuration option is set.
whenOptionSet :: SyntaxOption -> IParse () -> IParse ()
whenOptionSet opt act = do
    s <- isOptionSet opt
    when s $ act

-- | A command is an entity in a document that will cause the backend renderer
-- to perform a special operation. Commands in this markup language take the
-- general form [command argument=value, argument2=value2]. Therefore, a Command
-- is a tuple consisting of the command name and the command arguments.
type Command = (String, [(String, String)])

-- | Parse a udoc document and return the document items.
parseDocument :: ParserState -> HSP -> String -> Either ParseError ([DocumentItem], ParserState)
parseDocument initialState hsp input = 
   myParse id parser input
   where myParse f p src = fst
                           $ flip runState (f $ initialPos "")
                           $ runParserT p initialState "" src
         parser = do items <- documentItems hsp
                     s     <- P.getState
                     return (items, s)

-- | Parse an inline udoc document and return the document items (e.g. to
-- | parse table cells). In case of an error use the given initial source
-- | position as error position.
-- |
-- | TODO: As soon as its possible to determine the initial position of
-- |       each table cell it would be nice to combine the initial
-- |       position and the error position to determine the real
-- |       position.
parseInlineDocument :: ParserState -> SourcePos -> HSP -> String -> Either ParseError ([DocumentItem], ParserState)
parseInlineDocument initialState currentPosition handleSpecialCommand source =
  fixPosition $ fst $ runState runParser (initialPos fileName)
  where
    fileName :: String
    fileName = sourceName currentPosition

    runParser :: State SourcePos (Either ParseError ([DocumentItem], ParserState))
    runParser = runParserT parser initialState fileName source

    parser :: IParse ([DocumentItem], ParserState)
    parser = do
      itemHandler <- documentItems handleSpecialCommand
      currentState <- P.getState
      return (itemHandler, currentState)

    fixPosition :: Either ParseError ([DocumentItem], ParserState) -> Either ParseError ([DocumentItem], ParserState)
    fixPosition result = case result of
      Left error -> Left $ PE.setErrorPos (updateErrorPos currentPosition (PE.errorPos error)) error
      right -> right

    -- | Use the position of the last parsed parent document character as new
    -- | error position.
    updateErrorPos :: SourcePos -> SourcePos -> SourcePos
    updateErrorPos initialPos errorPos =
      setSourceColumn (setSourceLine errorPos $ (sourceLine initialPos)) (sourceColumn initialPos)


-- | Skips zero or more space or tab characters.
spaces :: IParse ()
spaces = skipMany $ oneOf " \t"

-- | Skipts zero of more space, tab and newline characters.
whiteSpace :: IParse ()
whiteSpace = skipMany $ oneOf " \t\n"

-- | Top level parser. It will parse headings or paragraphs until
-- eof is found.
documentItems :: HSP -> IParse [DocumentItem]
documentItems hsp = do
    whiteSpace
    -- Before we start our paragraph based parsing, let's see if the
    -- document starts with one or more meta tags. If so, we just
    -- include them separately, i.e., without a surrounding paragraph.
    docHead <- many $ tryCommand isMetaTag
    whiteSpace
    items   <- many $     (try $ heading hsp) 
                      <|> (try $ optionIf BlockQuotes $ blockQuote) 
                      <|> (try $ optionIf FencedCodeBlocks $ fencedCodeBlock)
                      <|> paragraph hsp
    whiteSpace
    eof
    return $ docHead ++ items
    where tryCommand pred = try $ do
             cmd <- extendedCommand hsp
             optional newline
             if pred cmd
                then return cmd
                else fail "Did not find expected command"
          isMetaTag (ItemMetaTag _) = True
          isMetaTag _               = False
          optionIf opt a = do
             s <- isOptionSet opt
             if s
                then a
                else fail ""

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
inlineSource = do backticks <- isOptionSet BacktickSource
                  let chars = if backticks then "{`" else "{"
                  opener <- oneOf chars
                  PP.modifyState $ \x -> x { parserStateLastInlineOpeningTag = opener }
                  return ("_s", [])

-- | Some text surrounded by double quotes. This will internally be
-- represented as a command called "_q" (for quote)
inlineQuote :: IParse Command
inlineQuote = char '"' >> return ("_q", [])

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

-- | A combination of mandRename and optRename. This function tries to extract
-- all required and optional arguments from a list. The obtained arguments will
-- be renamed and then a function will be invoked on both resulting argument
-- lists. The result of the function will be returned.
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

{-| Lookup a required tag attribute that may not exist. -}
mLookup :: (Show a, Monad m, Eq a) => a -> [(a , b)] -> String -> m b
mLookup k al error_message = maybe (fail error_message) return $ lookup k al

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

-- | Handles the "setpos" tag, which is used internally to signal
-- the parser that the current 'SourcePos' changed.
handleSetPos :: [(String, String)] -> IParse DocumentItem
handleSetPos args = do
    ifHave "filename" args $ \filename ->
       modifySourcePos $ (flip setSourceName) filename
    ifHave "line" args $ \linenumber -> 
       modifySourcePos $ (flip setSourceLine) (read linenumber)
    ifHave "column" args $ \column ->
       modifySourcePos $ (flip setSourceColumn) (read column)
    handleMetaTag "setpos" [] [("filename", "filename"), ("line", "line"), ("column", "column")] args
    where ifHave x l a = case lookup x l of
                            Just v -> a v
                            Nothing -> return ()
          modifySourcePos f = getPosition >>= (setPosition . f)

-- | Handles the flavor command, which can be used to activate markup
-- extensions.
handleFlavor :: [(String, String)] -> IParse DocumentItem
handleFlavor args = do
    tag <- handleMetaTag "flavor" [("name", "name")] [] args
    case (lookup "name" args >>= readMaybe) of
       Nothing -> fail $ "Cannot parse udoc extension name " ++ (show $ lookup "name" args)
       Just e  -> PP.modifyState $ \x -> x { parserStateFlavor = (e:parserStateFlavor x) }
    return tag

-- | Creates an ItemDocumentMetaContainer from the container type, its
-- properties and its content.
createMetaContainer ::    String -- ^ The container type
                       -> [(String, String)] -- ^ The container properties
                       -> [DocumentItem] -- ^ The container content
                       -> DocumentItem -- ^ The resulting item
createMetaContainer t props content = 
   ItemDocumentContainer $ DocumentMetaContainer ([("type", t)] ++ props) content

-- | A command is either inlineSource or a regular command surrounded
-- by square brackets.
extendedCommand' :: IParse Command
extendedCommand' = inlineSource <|> inlineQuote <|> squareBrCommand

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
removeTrailingNewline [] = []
removeTrailingNewline items =
   init items ++ nl (last items)
   where nl (ItemWord w) = [ItemWord $ dropWhileEnd isSpace w]
         nl x = [x]

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

      "image"  -> do label   <- mLookup "label"   args "Missing label in image tag"
                     caption <- mLookup "caption" args "Missing caption in image tag"
                     path    <- mLookup "path"    args "Missing path in image tag"
                     eatSpaces <- isOptionSet SkipNewlinesAfterImage 
                     when eatSpaces skipEmptyLines 
                     return $ ItemImage $ Image path caption label

      "inlineImage" -> do let vOffset = fromMaybe "0" $ lookup "vOffset" args
                          path <- mLookup "path" args "Missing path in inlineImage tag"
                          return $ ItemMetaTag (
                            [ ("type", "inlineImage")
                            , ("path", path)
                            , ("vOffset", vOffset)
                            ])

      "table"  -> do let mCL = ((,)) <$> lookup "caption" args <*> lookup "label" args
                     let style = fromMaybe "head_top" $ lookup "style" args
                     let mWidths = sequence $ map readMaybe $ map (filter (/= ' ')) $ split ',' $ fromMaybe "" $ lookup "widths" args
                     widths <- case mWidths of
                                     Nothing -> fail "Cannot parse table widths argument."
                                     Just w -> return w
                     skipEmptyLines

                     -- We need the position of the first row  as `table` is
                     -- going to parse the whole table at once.
                     currentPosition <- P.getPosition
                     currentState <- P.getState

                     rows <- table

                     -- We simply apply our document parser to each table
                     -- cell.
                     rows <- forM rows $ \row -> do
                         cells <- forM row $ \cell -> do
                            case parseInlineDocument currentState currentPosition handleSpecialCommand cell of
                               Left err -> error $ "Error while parsing the table which is located at " ++ (show currentPosition) ++ ".\n\n" ++ (show err)
                               Right (inner, newS) -> do P.setState newS
                                                         return $ concat $ map stripOuterParagraph inner
                         return $ DocumentTableRow cells
                     return $ ItemDocumentContainer $ DocumentTable style mCL widths rows
      "_s"     -> do x <- parserStateLastInlineOpeningTag <$> P.getState
                     let chr = case x of
                                  '{' -> '}'
                                  '`' -> '`'
                                  x   -> x
                     source <- manyTill inlineVerbatimContent (char chr >> spaces)
                     return $ ItemDocumentContainer $ DocumentMetaContainer ([("type", "inlineSource")]) source
      "_q"     -> do text <- manyTill inlineQuotedContent (char '"' >> spaces)
                     return $ ItemDocumentContainer $ DocumentMetaContainer ([("type", "inlineQuote")]) text
      "source" -> do let language = fromMaybe "" $ lookup "language" args
                     skipEmptyLines
                     source <- manyTill (verbatimContent "[/source]") (extendedCommandName "/source")
                     eatSpaces <- isOptionSet SkipNewlinesAfterSourceOrQuoteBlock
                     when eatSpaces skipEmptyLines
                     return $ ItemDocumentContainer $ DocumentMetaContainer ([("type", "source"), ("language", language)]) (removeTrailingNewline source)
      "label"  -> handleLab args
      "ref"    -> handleRef args
      "imgref" -> handleImgRef args
      "tblref" -> handleTblRef args
      "setpos" -> handleSetPos args
      "quote"  -> do optional newline
                     content <- manyTill (verbatimContent "[/quote]") (extendedCommandName "/quote")
                     eatSpaces <- isOptionSet SkipNewlinesAfterSourceOrQuoteBlock
                     when eatSpaces skipEmptyLines
                     return $ ItemDocumentContainer $ DocumentMetaContainer ([("type", "blockquote")]) (removeTrailingNewline content)
      "flavor" -> handleFlavor args
 
      -- If we end up here, we can at least check if the
      -- identified command is some special-purpose
      -- command
      _         -> handleSpecialCommand name args
   where stripOuterParagraph :: DocumentItem -> [DocumentItem]
         stripOuterParagraph (ItemDocumentContainer (DocumentParagraph x)) = x
         stripOuterParagraph x = [x]
         split :: Eq a => a -> [a] -> [[a]]
         split d [] = []
         split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- | Parses one line from a table. Be aware, it really parses one line, not
-- one row.
oneTableLine :: IParse [String]
oneTableLine = 
   (many $ (notFollowedBy (extendedCommandName "/table") >> noneOf "|\n")) `sepBy` ((optional $ char ' ') >> (char '|') >> (optional $ char ' '))

-- | Utility for stopping parsing when a table ends
endOr :: a -> IParse a -> IParse a
endOr val action = try $ ((extendedCommandName "/table") >> return val)
                   <|> action

-- | Actual table parsing function. Takes a flag that tells whether or not the
-- table has to be parsed in "multi-line mode" (i.e., whether horizontal row
-- delimiters are necessary or not) and a current table structure. Returns a
-- new table structure.
table' :: Bool -> [[String]] -> IParse [[String]]
table' ml table = do
   skipEmptyLines
   endOr table $ do
      lookAhead anyToken
      l <- oneTableLine
      let (newTable, newMl) = tableAppend ml l table
      table' newMl newTable

-- | Correctly appends a row to a table, depending on wheter the table is
-- to be parsed in multi-line mode or not.
tableAppend :: Bool -> [String] -> [[String]] -> ([[String]], Bool)
tableAppend ml l table
   | (not ml) && isDelimiter l && null table = (table, True)
   | (not ml) && isDelimiter l = ([foldl (zipWith (\a b -> a ++ b ++ "\n")) emptyRow table] ++ [take (length $ table !! 0) emptyRow], True)
   | ml && isDelimiter l = (table ++ [take (length $ table !! 0) emptyRow], True)
   | not ml = (table ++ [l], False)
   | ml && length table > 0 = ((init table) ++ [zipWith (\a b -> a ++ b ++ "\n") (last table) l], True)
   | otherwise = (table, ml) -- Should not happen
   where isDelimiter (l:[]) = all ((||) <$> (=='-') <*> (=='+')) l
         isDelimiter _      = False
         emptyRow = repeat ""

-- | Convenience wrapper around our table parsing function
table :: IParse [[String]]
table = do 
   t <- table' False []
   return $ takeWhile (not . all (=="")) t

-- | Simply parse a paragraph.
paragraph :: HSP -> IParse DocumentItem
paragraph hsp = do
    bq <- isOptionSet BlockQuotes 
    fc <- isOptionSet FencedCodeBlocks
    let without = [(bq, blockQuoteBegin), (fc, fencedCodeBlockBegin)]
    let withouts = foldl (\p (flag, parser) -> if flag then (p <|> (try parser)) else p) parserZero without
    paragraphWithout withouts hsp

-- | Parse a paragraph. This Paragraph could contain a list of words or lists or
-- special commands. However, this parser will fail if parser x succeeds.
paragraphWithout ::    IParse () -- ^ The parser that may not succeed
                    -> HSP -- ^ The supported special commands
                    -> IParse DocumentItem
paragraphWithout x hsp = do 
   lines <- many1 (  notFollowedBy x >> 
                     (
                           (do x <- try $ line hsp; optional newline; return x)
                       <|> (do x <- try $ extendedCommand hsp; optional newline; return [x])
                       <|> (do x <- try $ uList hsp; return [x])
                       <|> (do x <- try $ oList hsp; return [x])
                     )
                  ) <?> "a properly indented paragraph, command or list"
   skipEmptyLines
   return $ ItemDocumentContainer $ DocumentParagraph $ concat lines

-- | Parses escaped character sequences (such as \[) and returns the according
-- character.
escaped :: Char -> IParse Char
escaped x = (string ("\\"++[x])) >> return x

-- | Parses one character in a regular word of the text.
wordChar :: IParse Char
wordChar = do
    bts <- isOptionSet BacktickSource 
    fcb <- isOptionSet FencedCodeBlocks
    let additional = if bts || fcb
                       then "`"
                       else ""
    doWordChar additional 
    where doWordChar additional =
                (try $ escaped '[')
            <|> (try $ escaped ']')
            <|> (try $ escaped '|')
            <|> (try $ escaped '{')
            <|> (try $ escaped '}')
            <|> (try $ escaped '"')
            <|> (try $ escaped '\\')
            <|> (try $ escaped '`')
            <|> (noneOf $ " \t\n[]|{}\"" ++ additional)
            <?> "a valid word character"
   
-- | Parses a whole word in the text of the document.
word :: IParse DocumentItem
word = do result <- many1 wordChar
          spaces
          return $ ItemWord result

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
   result <- many1 (
                      (
                         (
                              (try $ lookAhead $ string "\\[b]")
                              <|> (try $ lookAhead $ string "\\[/b]")
                              <|> (try $ lookAhead $ string $ "\\" ++ closingTag)
                         ) >>
                         (escaped '[')
                      )
                      <|>
                      (
                         (notFollowedBy $ string closingTag) >>
                         (notFollowedBy $ string "[b]") >>
                         (notFollowedBy $ string "[/b]") >>
                         (anyChar)
                      )
                   )

   return $ ItemWord result

-- | Contents of the inline verbatim container - may basically be everything
-- besides { or }, which need to be escaped.
inlineVerbatimContent :: IParse DocumentItem
inlineVerbatimContent = do 
   result <- many1 (wordChar <|> oneOf "\n\t |\"[]")
   return $ ItemWord result

-- | Contents of the inline quoted text container - may basically be everything
-- besides ", which needs to be escaped.
inlineQuotedContent :: IParse DocumentItem
inlineQuotedContent = do 
    result <- word
    spaces
    skipMany $ char '\n'
    spaces
    return result

-- | The start of the regular line of text. This is only a lookahead, which
-- will not match lists or headings (or block quotes if this feature is
-- enabled)
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
-- It will parse multiple words or one other item, but it will 
-- fail on newline.
containerContentOneLine :: HSP -> IParse [DocumentItem]
containerContentOneLine hsp =
          (try $ (:[]) <$> extendedCommand hsp)
      <|> (try $ (:[]) <$> uList hsp)
      <|> (try $ (:[]) <$> oList hsp)
      <|> (beginRegularLine >> many1 word)

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

-- | The contents of a container; may not contain anything matched by some
-- parser x.
containerContentWithout ::   IParse () -- ^ Forbidden content
                          -> HSP  -- ^ Supported special commands
                          -> IParse DocumentItem
containerContentWithout x hsp = notFollowedBy x >> paragraphWithout x hsp

-- | The contents of a container
containerContentBlock :: HSP -> IParse [DocumentItem]
containerContentBlock hsp = do
   x <- many1 $ do
      l <- containerContentOneLine hsp
      spaces
      return l
   spaces
   optional newline
   spaces
   return $ concat x

-- | The begin of an item in an un-ordered list. Returns the indentation level.
uListItemBegin :: IParse Int
uListItemBegin = do
      i <- many $ char ' '
      char '*'
      j <- many $ char ' '
      return $ ((length (i++j)) + 1)

-- | The body of an item in an un-ordered list
uListItemBody :: HSP -> Int -> IParse (UListItem, [DocumentItem])
uListItemBody hsp ind = do
      content <- block (containerContentBlock hsp)
      return $ (UListItem ind, (concat content))

-- | One item in an un-ordered list
uListItem :: HSP -> IParse (UListItem, [DocumentItem])
uListItem hsp =
   do ind <- uListItemBegin
      uListItemBody hsp ind

-- | Am un-ordered (bullet point) list
uList :: HSP -> IParse DocumentItem
uList hsp = do
   checkIndent
   lookAhead (uListItemBegin)
   result <- block $ uListItem hsp
   eatSpaces <- isOptionSet SkipNewlinesAfterUlist
   when eatSpaces $ skipEmptyLines
   return $ ItemDocumentContainer (DocumentUList result)

-- | The begin of an item in an ordered list. Returns the indentation level
-- and the item's number.
oListItemBegin :: IParse (Int, String)
oListItemBegin = do
   newStyle <- isOptionSet NewStyleOlist
   if newStyle
      then newOlistItemBegin
      else oldOlistItemBegin

-- | The new version of ordered lists start with dotted numbers followed
-- by either a dot, the sequence .) or the ) character.
newOlistItemBegin :: IParse (Int, String)
newOlistItemBegin = do
   i <- many $ char ' '
   string "+"
   spaces
   return ((length i), "0")

-- | The old version of ordered lists start with dotted numbers followed
-- by either a dot, the sequence .) or the ) character.
oldOlistItemBegin :: IParse (Int, String)
oldOlistItemBegin = do
   i <- many $ char ' '
   number <- dottedNumber
   trailer <- ( (try $ string ".)") <|> string "." <|> string ")" <?> "a number in an enumerated list")
   spaces
   return ((length i), number)

-- | One item in an ordered list
oListItem :: HSP -> IParse (OListItem, [DocumentItem])
oListItem hsp =
   do (ind, num) <- oListItemBegin
      content <- block $ (containerContentBlock hsp)
      return $ (OListItem ind num, (concat content))

-- | An ordered list. 
oList :: HSP -> IParse DocumentItem
oList hsp =
   do checkIndent
      lookAhead oListItemBegin
      items <- block $ oListItem hsp
      return $ ItemDocumentContainer (DocumentOList items)

-- | A number like 1, 1.2 or 1.2.3.4
dottedNumber :: IParse String
dottedNumber = do num <- many1 digit
                  rest <- try (char '.' >> dottedNumber) <|> return ""
                  if length rest == 0 then
                     return num
                   else
                      return (num ++ "." ++ rest)

-- | The begin of a list item: either the begin of an UListItem or the
-- begin of an OListItem.
listItemBegin :: IParse ()
listItemBegin = 
   ((try uListItemBegin) >> return ()) <|> (oListItemBegin >> return ())

-- | The begin of a heading: a number of # characters. Returns the heading
-- level.
headingBegin :: IParse Int
headingBegin = do
   spaces
   level <- (many1 $ char '#')
   spaces
   return $ length level

-- | A heading
heading :: HSP -> IParse DocumentItem
heading hsp = do level <- headingBegin
                 items <- block $ do
                              l <- line hsp
                              optional newline
                              spaces
                              return l
                 skipEmptyLines
                 return $ ItemDocumentContainer (DocumentHeading (Heading level Nothing) (concat items))

-- | The start of a block quote
blockQuoteBegin :: IParse ()
blockQuoteBegin = do
    spaces
    char '>'
    spaces

-- | A block quote
blockQuote :: IParse DocumentItem
blockQuote = do
    lines <- many1 $ do
       blockQuoteBegin
       thisLine <- many $ noneOf "\n"
       optional newline
       return thisLine
    skipEmptyLines
    return $ ItemDocumentContainer $ DocumentMetaContainer [("type","blockquote")] [ItemWord $ intercalate "\n" lines]

-- | The start of a fenced code block
fencedCodeBlockBegin :: IParse ()
fencedCodeBlockBegin = do
    string "```"
    return ()

-- | A fenced code block
fencedCodeBlock :: IParse DocumentItem
fencedCodeBlock = do
    fencedCodeBlockBegin
    language <- many $ noneOf "\n"
    newline
    lines <- many $ do
       notFollowedBy $ string "```"
       thisLine <- many $ noneOf "\n"
       newline
       return thisLine
    string "```"
    newline
    skipEmptyLines
    return $ ItemDocumentContainer $ DocumentMetaContainer [("type", "source"), ("language", language)] [ItemWord $ intercalate "\n" lines]
