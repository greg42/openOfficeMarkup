{-
 - ----------------------------------------------------------------------------
 - "THE BEER-WARE LICENSE" (Revision 42):
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you
 - can do whatever you want with this stuff. If we meet some day, and you
 - think this stuff is worth it, you can buy me a beer in return Gregor Kopf
 -
 - ----------------------------------------------------------------------------
 -}

module Text.Udoc.Document 
   (DocumentItem(..), OListItem(..), UListItem(..), Heading(..),
    DocumentContainer(..), DocumentImage(..),
    computeHeadingNumbers, generateToc,
    filterItems)

where

import Text.JSON
import Control.Monad.State
import Data.Maybe
import Control.Applicative

mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

------------------------ Data Definitions -----------------------------

{-| A container for other document items. A container is something that has
    a content. You can think of a container as something similar to an
    environment in LaTeX. -}
data DocumentContainer =   DocumentHeading   Heading [DocumentItem]
                         | DocumentBoldFace  [DocumentItem]
                         | DocumentParagraph [DocumentItem]
                         | DocumentOList     [(OListItem, [DocumentItem])]
                         | DocumentUList     [(UListItem, [DocumentItem])]
                         | DocumentTableRow  [[DocumentItem]]
                         | DocumentTable     String (Maybe (String, String)) 
                                             [DocumentContainer]
                         | DocumentMetaContainer [(String,String)] [DocumentItem]
                         deriving(Show)

showJSON' (Just x) = showJSON x
showJSON' Nothing = JSNull

instance JSON DocumentContainer where
   showJSON (DocumentHeading heading items) = 
      makeObj [  ("type", showJSON "DocumentHeading")
               , ("DocumentHeading", showJSON heading)
               , ("content", showJSON items)
              ]
   showJSON (DocumentBoldFace items) = 
      makeObj [  ("type", showJSON "DocumentBoldFace")
               , ("content", showJSON items)
              ]

   showJSON (DocumentParagraph items) = 
      makeObj [  ("type", showJSON "DocumentParagraph")
               , ("content", showJSON items)
              ]

   showJSON (DocumentOList items) = 
      makeObj [  ("type", showJSON "DocumentOList")
               , ("content", showJSON items)
              ]

   showJSON (DocumentUList items) = 
      makeObj [  ("type", showJSON "DocumentUList")
               , ("content", showJSON items)
              ]

   showJSON (DocumentTable style mCL items) = 
      makeObj [  ("type", showJSON "DocumentTable")
               , ("content", showJSON items)
               , ("caption", showJSON' $ fst <$> mCL)
               , ("label", showJSON' $ snd <$> mCL)
               , ("style", showJSON style)
              ]

   showJSON (DocumentTableRow items) = 
      makeObj [  ("type", showJSON "DocumentTableRow")
               , ("content", showJSON items)
              ]

   showJSON (DocumentMetaContainer properties items) = 
      makeObj [  ("type", showJSON "DocumentMetaContainer")
               , ("properties", showJSON $ toJSObject properties)
               , ("content", showJSON items)
              ]

   readJSON (JSObject obj) = let
     jsonObjAssoc = fromJSObject obj
     getOne  x    = (mLookup x jsonObjAssoc) >>= readJSON
     getOne' x    = case (lookup x jsonObjAssoc) of
                      Just (JSNull) -> return Nothing
                      Just (x)      -> Just <$> readJSON x
                      Nothing       -> return Nothing
    in do
      objType <- getOne "type"
      case objType of
         "DocumentHeading"        -> DocumentHeading   <$> (getOne objType) <*> 
                                                           getOne "content"
         "DocumentBoldFace"       -> DocumentBoldFace  <$> getOne "content"
         "DocumentParagraph"      -> DocumentParagraph <$> getOne "content"
         "DocumentOList"          -> DocumentOList     <$> getOne "content"
         "DocumentUList"          -> DocumentUList     <$> getOne "content"
         "DocumentTable"          -> do thing   <- getOne "content"
                                        caption <- getOne' "caption"
                                        label   <- getOne' "label"
                                        style   <- getOne "style"
                                        return $ DocumentTable 
                                                   style 
                                                   (((,)) <$> caption <*> label) 
                                                   thing
         "DocumentTableRow"       -> DocumentTableRow  <$> getOne "content"
         "DocumentMetaContainer"  -> DocumentMetaContainer <$> 
                                             (fromJSObject <$> 
                                                 getOne "properties") <*> 
                                             getOne "content"
         _                        -> fail $ "Unknown object type: " ++ 
                                                show objType
   readJSON x = fail $ "Cannot decode JSON item: " ++ show x


{-| A DocumentItem can be anything that is part of a document. This includes
    simple entities such as words or images, but also containers such as tables
    or paragraphs. -}
data DocumentItem =   ItemWord String
                    | ItemDocumentContainer DocumentContainer
                    | ItemImage DocumentImage
                    | ItemLinebreak
                    | ItemMetaTag [(String, String)]
                    deriving(Show)

instance JSON DocumentItem where
   showJSON (ItemWord x) = 
       makeObj [ ("type", showJSON "ItemWord"), ("ItemWord", showJSON x) ]

   showJSON (ItemDocumentContainer c) = 
       makeObj [  ("type", showJSON "ItemDocumentContainer")
                , ("ItemDocumentContainer", showJSON c) 
               ]

   showJSON (ItemMetaTag t) = 
      makeObj [  ("type", showJSON "ItemMetaTag") 
               , ("ItemMetaTag", showJSON $ toJSObject t) 
              ]

   showJSON (ItemLinebreak) = makeObj [ ("type", showJSON "ItemLinebreak") ]

   showJSON (ItemImage i) = 
      makeObj [  ("type", showJSON "ItemImage")
               , ("ItemImage", showJSON $ i) 
              ]

   readJSON (JSObject obj) = let
     jsonObjAssoc = fromJSObject obj
     getOne  x = (mLookup x jsonObjAssoc) >>= readJSON
    in do
      objType <- getOne "type"
      case objType of
         "ItemWord"               -> ItemWord <$> getOne objType
         "ItemDocumentContainer"  -> ItemDocumentContainer <$> getOne objType
         "ItemMetaTag"            -> ItemMetaTag <$> (fromJSObject <$> 
                                                      getOne objType)
         "ItemLinebreak"          -> return ItemLinebreak
         "ItemImage"              -> ItemImage <$> getOne objType
         _                        -> fail $ "Unknown object type: " ++ 
                                            show objType
   readJSON x = fail $ "Cannot decode JSON item: " ++ show x

{-| An item in an ordered list -}
data OListItem = OListItem {   oListItemIndent  :: Int -- ^ The indentation level
                             , oListItemNumber  :: String -- ^ The number of the
                                                          -- item. This is
                                                          -- something like
                                                          -- 1.2.3.
                           } deriving(Show)

instance JSON OListItem where
   showJSON oli = makeObj [   ("indent", showJSON $ oListItemIndent oli)
                            , ("number", showJSON $ oListItemNumber oli)
                          ]

   readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
    in do
      indent <- mLookup "indent" jsonObjAssoc >>= readJSON
      number <- mLookup "number" jsonObjAssoc >>= readJSON
      return $ OListItem indent number

{-| An item in an un-ordered (bullet point) list -}
data UListItem = UListItem { uListItemIndent  :: Int -- ^ The item's indent
                           } deriving(Show)

instance JSON UListItem where
   showJSON uli = makeObj [ ("indent", showJSON $ uListItemIndent uli) ]

   readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
    in do
      indent <- mLookup "indent" jsonObjAssoc >>= readJSON
      return $ UListItem indent 

{-| A heading -}
data Heading = Heading {   headingLevel :: Int -- ^ The heading level
                         , headingComputedNumber :: Maybe [Int]
                           -- ^ A heading can have a computed heading number,
                           -- such as 1.2.3. This is useful for generating
                           -- a table of contents or for referencing sections
                           -- in a document.
                       } deriving(Show)

instance JSON Heading where
   showJSON hd  = makeObj [  ("level", showJSON $ headingLevel hd)
                           , ("computedNumber", showJSON $ 
                                                  headingComputedNumber hd)
                          ]

   readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
    in do
      level  <- mLookup "level"  jsonObjAssoc >>= readJSON
      cn     <- mLookup "computedNumber"  jsonObjAssoc >>= readJSON
      return $ Heading level cn
   readJSON x = fail $ "Cannot decode JSON item: " ++ show x

{-| An image in a document -}
data DocumentImage = Image {
                        imageFilename :: String -- ^ The file name of the image
                      , imageCaption  :: String -- ^ The image's caption
                      , imageLabel    :: String -- ^ A label for referencing the
                                                -- image
                     } deriving(Show)

instance JSON DocumentImage where
   showJSON (Image filename caption label) =
      makeObj [  ("imageFilename", showJSON filename)
               , ("imageCaption" , showJSON caption)
               , ("imageLabel"   , showJSON label)
              ]

   readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
    in do
      filename <- mLookup "imageFilename" jsonObjAssoc >>= readJSON
      caption  <- mLookup "imageCaption"  jsonObjAssoc >>= readJSON
      label    <- mLookup "imageLabel"    jsonObjAssoc >>= readJSON
      return $ Image filename caption label

------------------------ Compute the heading level --------------------

listOfSize :: Int -> [Int]
listOfSize n = take n (repeat 0)

type NestedStack = [[Int]]

incrementLastNumber :: NestedStack -> NestedStack
incrementLastNumber (top:rest) = ((init top)++[(last top)+1]):rest

addElementOfLevel :: Int -> NestedStack -> NestedStack
addElementOfLevel n stack = ((head stack) ++ (listOfSize (n - (length $ head stack)))):stack

nestingLevel :: Int -> State NestedStack [Int]
nestingLevel level = do
   curStack <- get
   case compare level (length $ head curStack) of
      EQ -> do put $ incrementLastNumber curStack
      GT -> do put $ addElementOfLevel level curStack
      LT -> let tmpStack = dropWhile ( (>level) . length) curStack in do
                if (length $ head tmpStack) < level
                   then put $ addElementOfLevel level tmpStack
                   else put $ incrementLastNumber tmpStack
   stack <- get
   return $ head stack

computeHeadingNumbers' [] = do return []
computeHeadingNumbers' ( (ItemDocumentContainer (DocumentHeading (Heading level _) content) ):rest) = do
   computedNumber <- nestingLevel level
   rest' <- computeHeadingNumbers' rest
   return $ ItemDocumentContainer (DocumentHeading (Heading level (Just computedNumber)) content):rest'

computeHeadingNumbers' (x:rest) = do   rest' <- computeHeadingNumbers' rest
                                       return $ x:rest'

{-| Computes the heading numbers and returns a new document where all heading
    have a headingComputedNumber. -}
computeHeadingNumbers :: [DocumentItem] -> [DocumentItem]
computeHeadingNumbers x = evalState (computeHeadingNumbers' x) [[-1]]

------------------------- Generating a TOC ----------------------------

headingIndent l = length l + 1

strJoin delim [] = ""
strJoin delim (head:rest) = let end = strJoin delim rest in
                            if end /= "" then
                               head ++ delim ++ end
                            else
                               head

headingNumber level = strJoin "." (map (show . (+1)) level)

headingToToc (ItemDocumentContainer (DocumentHeading (Heading _ cn) headline)) = let 
                                indent   = headingIndent `fmap` cn
                                num      = headingNumber `fmap` cn
                            in
                            if not (isJust indent) || not (isJust num) then
                               Nothing
                            else
                               Just $ (OListItem (fromJust indent) (fromJust num), headline)

headingToToc _ = Nothing

{-| Generates a table of contents for a document. It returns the table of
    contents as a DocumentOList. -}
generateToc :: [DocumentItem] -> DocumentItem
generateToc document = ItemDocumentContainer $
                         DocumentOList $ map fromJust (filter isJust (map headingToToc document))

-------------------------- Filter out certain items -------------------

filterHelper func item =
   if func item
      then [item]
      else []

filterContainer func (DocumentHeading _ content) =
   filterItems func content

filterContainer func (DocumentBoldFace content) =
   filterItems func content

filterContainer func (DocumentParagraph content) =
   filterItems func content

filterContainer func (DocumentOList content) =
   concat $ map ( (filterItems func) . snd) content
 
filterContainer func (DocumentUList content) =
   concat $ map ( (filterItems func) . snd) content

filterContainer func (DocumentTableRow content) =
   filterItems func (concat content)

filterContainer func (DocumentTable _ _ content) =
   filterItems func (map ItemDocumentContainer content)

filterContainer func (DocumentMetaContainer _ content) =
   filterItems func content

{-| FilterItems will recursively walk through the whole
 - document depth-first, returning a list of elements where
 - func(element) == True -}
filterItems ::   (DocumentItem -> Bool) -- ^ The filter function
              -> [DocumentItem] -- ^ The document
              -> [DocumentItem] -- ^ The resulting document
filterItems _ [] = []
filterItems func (item:items) =
   case item of
      ItemDocumentContainer dc -> filterHelper func item ++
                                  filterContainer func dc ++
                                  filterItems func items
      _ -> filterHelper func item ++
           filterItems func items
