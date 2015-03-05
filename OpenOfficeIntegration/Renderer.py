# -*- coding: utf-8 -*-

#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# <code@gregorkopf.de> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a beer in return Gregor Kopf
# ----------------------------------------------------------------------------
#

import uno
import json
import platform
import time
from com.sun.star.text.ControlCharacter import PARAGRAPH_BREAK
from com.sun.star.text.TextContentAnchorType import AS_CHARACTER
from com.sun.star.awt import Size
from com.sun.star.text.ReferenceFieldPart import CHAPTER, CATEGORY_AND_NUMBER
from com.sun.star.text.ReferenceFieldSource import BOOKMARK,REFERENCE_MARK,SEQUENCE_FIELD
from com.sun.star.style.NumberingType import ARABIC
from com.sun.star.text.SetVariableType import SEQUENCE

from com.sun.star.lang import XMain
from com.sun.star.awt.FontWeight import BOLD, NORMAL
from com.sun.star.beans import PropertyValue
from com.sun.star.beans.PropertyState import DIRECT_VALUE
from com.sun.star.lang import Locale

import traceback
import os
import sys
currentDir = os.path.dirname(traceback.extract_stack()[-1][0])
sys.path.append(currentDir)

if sys.version >= '3':
   unicode = str

class Renderer(object):
   def __init__(self):
      self.i18n = {}
      self.custom_i18n = {}
      self.currentListLevel = 0
      self.afterRendering = []
      self.CurrentHeading = (0, '')
      self.findingList = []
      self.Images = {}
      self.Tables = {}
      self.knownImageRefs = []
      self.knownTableRefs = []
      self._hookRender = None
      self.languageStrings = {}
      self._lastItem = None

   def init(self, document, cursor):
      self._cursor = cursor
      self._document = document
      self._realDocument = document

   def handleCustomMetaContainer(self, properties, content):
      raise NotImplementedError

   def changeLanguage(self, lang):
      if lang in self.custom_i18n.keys():
         myi18n = self.custom_i18n[lang]
      else:
         raise RuntimeError('Unsupported language: %s' % lang)
      for key in list(self.i18n.keys()):
         del self.i18n[key]
      for key in myi18n:
         self.i18n[key] = myi18n[key]

      if lang in self.languageStrings:
         language, country = self.languageStrings[lang]
         loc = Locale()
         loc.Language = language
         loc.Country = country
         self._document.CharLocale = loc

   def renderWord(self, string, lookAhead = None):
      self.insertString(string)
      if lookAhead:
         if type(lookAhead) == dict and 'type' in lookAhead and\
            lookAhead['type'] == 'ItemWord':
            self.insertString(' ')
         elif type(lookAhead) == str or type(lookAhead) == unicode:
            self.insertString(' ')

   def renderHeading(self, heading, items):
      level = heading["level"]
      self.insertHeading(level, items)

   def renderBoldFace(self, items):
      if self.needSpace():
         self.insertString(' ')
      self.insertBoldFace(items)
      self.smartSpace()

   def renderParagraph(self, items):
      self.insertParagraph(items)

   def renderLinebreak(self):
      self.insertString("\n")

   def renderMetaContainer(self, content, properties):
      if not 'type' in properties:
         return
      if properties['type'] == 'source':
         self.insertSourceCode(content)
      if properties['type'] == 'inlineSource':
         self.insertInlineSourceCode(content)
      elif self.handleCustomMetaContainer(properties, content):
         return
      else:
         # Ignore unknown meta container types..
         return

   def renderTable(self, table, caption, label):
      normTable = [x['content'] for x in table]
      self.insertTable(normTable, caption, label)

   def renderUListItem(self, uli, content):
      self.insertUListItem(content)

   def renderOListItem(self, oli, content, nl = None):
      self.insertOListItem(content, nl)

   def renderUlist(self, ulist):
      oldName = self._cursor.ParaStyleName
      for uli, content in ulist:
         self.currentListLevel += 1
         self.renderUListItem(uli, content)
         self.currentListLevel -= 1

      if self.currentListLevel == 0:
         self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
         self._cursor.ParaStyleName = oldName

   def renderOlist(self, olist):
      nl = 1
      oldName = self._cursor.ParaStyleName
      for oli, content in olist:
         self.currentListLevel += 1
         self.renderOListItem(oli, content, nl)
         nl = None
         self.currentListLevel -= 1

      if self.currentListLevel == 0:
         self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
         self._cursor.ParaStyleName = oldName

   def renderContainer(self, container):
      containerType = container['type']
      if containerType == 'DocumentHeading':
         self.renderHeading(container['DocumentHeading'], 
                            container['content'])
      elif containerType == 'DocumentBoldFace':
         self.renderBoldFace(container['content'])
      elif containerType == 'DocumentParagraph':
         self.renderParagraph(container['content'])
      elif containerType == 'DocumentTable':
         self.renderTable(container['content'], container['caption'], container['label'])
      elif containerType == 'DocumentMetaContainer':
         self.renderMetaContainer(container['content'],
                             container['properties'])
      elif containerType == 'DocumentUList':
         self.renderUlist(container['content'])
      elif containerType == 'DocumentOList':
         self.renderOlist(container['content'])
      else:
         raise RuntimeError('Unsupported container type: %s' % containerType)

   def renderImage(self, img):
      self.insertImage(img['imageFilename'], img['imageCaption'],
                       img['imageLabel'])

   def handleCustomMetaTag(self, tag):
      raise NotImplementedError

   def renderMetaTag(self, tag):
      if 'language' in tag:
         lang = tag['language']
         self.changeLanguage(lang)
      if not 'type' in tag:
         self.handleCustomMetaTag(tag)
      elif tag['type'] == 'tableOfContents':
         self.insertTableOfContents()
      elif tag['type'] == 'label':
         self.insertReferenceMark(tag['name'])
      elif tag['type'] == 'ref':
         self.insertReference(tag['label'])
      elif tag['type'] == 'imgref':
         self.insertImageReference(tag['label'])
      elif tag['type'] == 'tblref':
         self.insertTableReference(tag['label'])
      elif tag['type'] == 'pagebreak':
         self.insertPageBreak()
      elif tag['type'] == 'footnote':
         self.insertFootnote(tag['text'])
      elif tag['type'] == 'inlineImage':
         self.insertInlineImage(tag['path'], tag['vOffset'])
      else:
         self.handleCustomMetaTag(tag)

   def _isWord(self, item):
      return self._getWord(item) != None

   def _getWord(self, item):
      if type(item) == str or type(item) == unicode:
         return item
      if type(item) == dict:
         if item['type'] == 'ItemWord':
            return item['ItemWord']
      return None

   def smartSpace(self):
      punctation = ('.', ',', ';', ':', '!', '?', '(', ')', '[', ']')
      def startsWithPunctation(x):
         for p in punctation:
            if x.startswith(p):
               return True
         return False
      def fun(item):
         if self._isWord(item) and not startsWithPunctation(self._getWord(item)):
            self.insertString(' ')
         return True
      self._hookRender = fun

   def needSpace(self):
       w = self._getWord(self._lastItem)
       if w != None:
           if w.endswith('('):
               return False
           else:
               return True

   def render(self, item, lookAhead = None):
      if self._hookRender:
         hr = self._hookRender
         self._hookRender = None
         if not hr(item):
            return
      if type(item) == str or type(item) == unicode:
         self.renderWord(item, lookAhead)
      elif type(item) == list or type(item) == tuple:
         for index, itm in enumerate(item):
            if self._hookRender:
               hr = self._hookRender
               self._hookRender = None
               if not hr(itm):
                  return
            if index+1 < len(item):
               self.render(itm, item[index+1])
            else:
               self.render(itm, None)
      elif type(item) == dict:
         itemType = item['type']
         if itemType == 'ItemWord':
            self.renderWord(item['ItemWord'], lookAhead)
         elif itemType == 'ItemDocumentContainer':
            self.renderContainer(item['ItemDocumentContainer'])
         elif itemType == 'ItemLinebreak':
            self.renderLinebreak()
         elif itemType == 'ItemMetaTag':
            self.renderMetaTag(item['ItemMetaTag'])
         elif itemType == 'ItemImage':
            self.renderImage(item['ItemImage'])
         else:
            raise RuntimeError('Cannot render item type: %s' % itemType)
      else:
         raise RuntimeError("Cannot render %s (type: %s)" % ((str(item),
                                                        str(type(item)))))
      self._lastItem = item

   # The main rendering function. Will be called from the "compiler"
   # chain.
   def renderJson(self, encodedDoc):
      self.render(json.loads(encodedDoc))
      for fun in self.afterRendering:
         fun(self)
 
   def doAfterRendering(self, f):
      self.afterRendering.append(f)

   def createUnoService(self, serviceName):
     sm = uno.getComponentContext().ServiceManager
     return sm.createInstanceWithContext(serviceName, uno.getComponentContext())

   def setParStyle(self, text, parStyle):
      enum = text.createEnumeration()
      while enum.hasMoreElements():
         par = enum.nextElement()
         if par.supportsService("com.sun.star.text.Paragraph"):
            par.ParaStyleName = parStyle

   def changeParaStyle(self, newStyle):
      old = self._cursor.ParaStyleName
      self._cursor.ParaStyleName = newStyle
      return old

   def changeCharStyle(self, newStyle):
      old = self._cursor.CharStyleName
      self._cursor.CharStyleName = newStyle
      return old

   def optimalTableWidth(self, table):
      self._document.getCurrentController().select(table)
      vc = self._document.getCurrentController().getViewCursor()
      vc.gotoEnd(True)
      vc.gotoEnd(True)
      provider = self._document.getCurrentController().Frame
      dispatcher = self.createUnoService("com.sun.star.frame.DispatchHelper")
      dispatcher.executeDispatch(provider, ".uno:SetOptimalColumnWidth", "", 0, ())
      vc.goDown(1, False)

      table.RelativeWidth = 100

   def guessImageSize(self, image):
      # Maximum width and height in 1/100 mm
      maxWidth = 150 * 100  # Template maximum width in millimeters is 150.
                            # Let's save some margin.
      maxHeight = 195 * 100 # Template maximum height in millimeters is 215.

      size = image.Size
      if size.Height == 0 or size.Width == 0:
         size.Height = image.SizePixel.Height * 2540.0 * TwipsPerPixelY() / 1440
         size.Width  = image.SizePixel.Width  * 2540.0 * TwipsPerPixelX() / 1440
      if size.Width == 0 or size.Height == 0:
         return None
      if size.Width > maxWidth:
         size.Height = size.Height * maxWidth / size.Width
         size.Width = maxWidth
      if size.Height > maxHeight:
         size.Width = size.Width * maxHeight / size.Height
         size.Height = maxHeight
      return size
   
   def getOrCreateSequenceFieldMaster(self, name):
      masters = self._document.getTextFieldMasters()
      fName = "com.sun.star.text.FieldMaster.SetExpression." + name
      if not masters.hasByName(fName):
         masterField = self._realDocument.createInstance("com.sun.star.text.FieldMaster.SetExpression")
         masterField.Name = name
         masterField.SubType = SEQUENCE
      else:
         masterField = masters.getByName(fName)
      return masterField

   def createSequenceField(self, name):
      field = self._realDocument.createInstance("com.sun.star.text.TextField.SetExpression")
      field.NumberingType = ARABIC
      field.attachTextFieldMaster(self.getOrCreateSequenceFieldMaster(name))
      field.Content = name + " + 1"
      return field

   # Provide sizes in 1/100 mm if you need to.
   def _insertImage(self, path, inline = False, width = None, 
                    height = None, scale = None, vOffset = None):

      if platform.platform().lower().startswith('win'):
         url = 'file:///' + os.path.realpath(path).replace(':', '|')
      else:
         url = 'file://' + os.path.realpath(path)

      # This is pure crap. OOo.. WTF is wrong with you..?
      # We'll first insert the image into the internal bitmap table
      bitmaps = self._realDocument.createInstance('com.sun.star.drawing.BitmapTable')
      try:
         internalUrl = bitmaps.getByName(url)
      except:
         bitmaps.insertByName(url, url) 
      internalUrl = bitmaps.getByName(url)

      # Now insert the image, *NOT* using the source from the internal
      # bitmap table, but instead using the external URL.
      graph = self._realDocument.createInstance("com.sun.star.text.TextGraphicObject")
      if inline:
         graph.AnchorType = AS_CHARACTER
      self._document.Text.insertTextContent(self._cursor, graph, False)

      graph.GraphicURL = url

      # XXX Yes, I know. It's super hacky.
      time.sleep(0.1)

      # Now we can correctly determine the image size
      if not width and not height and not scale:
         size = self.guessImageSize(graph)
         if size:
            gsize = graph.Size
            gsize.Height = size.Height
            gsize.Width = size.Width
            graph.Size = gsize
      elif not scale:
         gsize = graph.Size
         oldW, oldH = gsize.Width, gsize.Height
         if width:
            gsize.Width = width
            if not height:
               gsize.Height = (width/oldW) * oldH
         if height:
            gsize.Height = height
            if not width:
               gsize.Width = int ( (float(height)/oldH) * oldW)
         graph.Size = gsize
      else:
         gsize = graph.Size
         w, h = gsize.Width, gsize.Height
         gsize.Width = int(w * float(scale))
         gsize.Height = int(h * float(scale))
         graph.Size = gsize

      # Finally, we can use the internal URL, so that the image will
      # actually be embedded into the ODT. Like.. WTF?
      graph.GraphicURL = internalUrl
   
      if vOffset and vOffset != 0:
         graph.VertOrient = 0
         graph.VertOrientPosition = -graph.Size.Height + float(vOffset)

      return graph

   def insertInlineImage(self, path, vOffset):
      self.insertString(' ')
      self._insertImage(path, inline = True, scale = 0.15, vOffset = vOffset)
      self.insertString(' ')

   def insertImage(self, path, caption, labelName):
      CAPTION_TITLE=self.i18n['figure']

      self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
      self._insertImage(path)

      oldStyle = self.changeParaStyle(self.STYLE_FIGURE_CAPTION)
      field = self.createSequenceField(CAPTION_TITLE)
      self.insertString(CAPTION_TITLE + ' ')
      self._document.Text.insertTextContent(self._cursor, field, False)
      self.insertString(' - ')
      self.insertString(caption)

      self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
      self.changeParaStyle(oldStyle)

      # Remember the number of the current image, so that we'll later
      # be able to reference it properly.
      cnt = len(self.Images)
      self.Images[labelName] = cnt

   def insertUListItem(self, content):
      self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
      self._cursor.ParaStyleName = self.STYLE_LIST_1 # % global_currentListLevel
      self._cursor.NumberingLevel = self.currentListLevel - 1
      self.render(content)

   def insertOListItem(self, content, startVal = None):
      self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
      self._cursor.ParaStyleName = self.STYLE_NUMBERING_1 # % global_currentListLevel
      self._cursor.NumberingLevel = self.currentListLevel - 1
      if startVal != None:
         self._cursor.NumberingStartValue = startVal
      self.render(content)

   def insertString(self, text):
      self._document.Text.insertString(self._cursor, text, False)

   def insertFootnote(self, content):
      fn = self._realDocument.createInstance("com.sun.star.text.Footnote")
      self._document.Text.insertTextContent(self._cursor, fn, False)
      fn.insertString(fn.createTextCursor(), content, False)
      self.smartSpace()

   def insertBookmark(self, name):
      bm = self._realDocument.createInstance("com.sun.star.text.Bookmark")
      bm.Name = name
      self._document.Text.insertTextContent(self._cursor, bm, False)

   def insertReferenceMark(self, name):
      bm = self._realDocument.createInstance("com.sun.star.text.ReferenceMark")
      bm.Name = name
      self._document.Text.insertTextContent(self._cursor, bm, False)

   def insertReference(self, name):
      field = self._realDocument.createInstance("com.sun.star.text.textfield.GetReference")
      field.ReferenceFieldPart = CHAPTER
      field.ReferenceFieldSource = REFERENCE_MARK
      field.SourceName = name
      if self.needSpace():
         self.insertString(" ")
      self._document.Text.insertTextContent(self._cursor, field, False)
      self._realDocument.getTextFields().refresh()
      self._realDocument.refresh()
      self.smartSpace()

   def insertImageReference(self, name):
      # Fix for the nasty double reference problem.
      # Normally, it would insert two bookmarks with the
      # same name, which will cause tons of trouble.
      # This way, bookmark names will stay unique.
      refName = "_do_img_ref_" + name
      while refName in self.knownImageRefs:
         refName = 'X' + refName
      self.knownImageRefs.append(refName)
      self.insertBookmark(refName)
      self.smartSpace()

      space = self.needSpace()
      def do_insert_imageref(self):
         where = self._document.getBookmarks().getByName(refName).getAnchor()
         oldCur = self._cursor
         self._cursor = where
         field = self._realDocument.createInstance("com.sun.star.text.textfield.GetReference")
         field.ReferenceFieldPart = CATEGORY_AND_NUMBER
         field.ReferenceFieldSource = SEQUENCE_FIELD
         field.SourceName = self.i18n['figure']
         if not name in self.Images:
            raise RuntimeError('Unknown image %s' % name)
         field.SequenceNumber = self.Images[name] 
         # Inserting the space and the field in reverse content, because the
         # "cursor" is not going to be updated on these operations.
         self._document.Text.insertTextContent(self._cursor, field, False)
         # Also, insert a space before the reference
         if space:
            self.insertString(" ")
         self._document.getTextFields().refresh()
         self._document.refresh()
         self._cursor = oldCur

      self.doAfterRendering(do_insert_imageref)

   def insertTableReference(self, name):
      refName = "_do_tbl_ref_" + name
      while refName in self.knownTableRefs:
         refName = 'X' + refName
      self.knownTableRefs.append(refName)
      self.insertBookmark(refName)
      self.smartSpace()

      space = self.needSpace()
      def do_insert_tableref(self):
         where = self._document.getBookmarks().getByName(refName).getAnchor()
         oldCur = self._cursor
         self._cursor = where
         field = self._realDocument.createInstance("com.sun.star.text.textfield.GetReference")
         field.ReferenceFieldPart = CATEGORY_AND_NUMBER
         field.ReferenceFieldSource = SEQUENCE_FIELD
         field.SourceName = self.i18n['table']
         if not name in self.Tables:
            raise RuntimeError('Unknown table %s' % name)
         field.SequenceNumber = self.Tables[name] 
         # Inserting the space and the field in reverse content, because the
         # "cursor" is not going to be updated on these operations.
         self._document.Text.insertTextContent(self._cursor, field, False)
         # Also, insert a space before the reference
         if space:
            self.insertString(" ")
         self._document.getTextFields().refresh()
         self._document.refresh()
         self._cursor = oldCur

      self.doAfterRendering(do_insert_tableref)


   def insertTableOfContents(self):
      index = self._realDocument.createInstance("com.sun.star.text.ContentIndex")
      index.CreateFromOutline = True
      self._document.Text.insertTextContent(self._cursor, index, False)
      def updateToc(self):
         index.update()
      self.doAfterRendering(updateToc)

   def insertPageBreak(self):
      self._cursor.PageDescName = self._cursor.PageStyleName

   def insertTable(self, tableContent, caption, labelName):
      numRows = len(tableContent)
      numCols = max(list(map(len, tableContent)))
   
      text = self._document.Text
      table = self._realDocument.createInstance("com.sun.star.text.TextTable")
      table.initialize(numRows, numCols)
      table.RepeatHeadline = True
      text.insertTextContent(self._cursor, table, 0)
   
      for numRow, row in enumerate(tableContent):
         for numCol, cellContent in enumerate(row):
            cell = table.getCellByPosition(numCol, numRow)
            if numRow == 0:
               cell.BackColor = self.STYLE_TABLE_HEADING_BACKGROUND
               self.setParStyle(cell, self.STYLE_TABLE_HEADER)
            else:
               self.setParStyle(cell, self.STYLE_TABLE_CONTENT)
   
            tmpCur = cell.Text.createTextCursor()
            tmpCur.gotoEnd(False)
            oldCur = self._cursor
            oldDoc = self._document
            self._cursor = tmpCur
            self._realDocument = self._document
            self._document = cell

            self.render(cellContent)

            self._document = oldDoc
            self._cursor = oldCur
   
      self.optimalTableWidth(table)

      if caption != None and labelName != None:
         CAPTION_TITLE=self.i18n['table']
         oldStyle = self.changeParaStyle(self.STYLE_TABLE_CAPTION)
         field = self.createSequenceField(CAPTION_TITLE)
         self.insertString(CAPTION_TITLE + ' ')
         self._document.Text.insertTextContent(self._cursor, field, False)
         self.insertString(' - ')
         self.insertString(caption)

         self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
         self.changeParaStyle(oldStyle)

         # Remember the number of the current table so that later we'll
         # be able to reference it properly.
         cnt = len(self.Tables)
         self.Tables[labelName] = cnt

   def insertHeading(self, headingLevel, headingText):
      if headingLevel > 4:
         raise RuntimeError('Illegal heading level!')

      self.render(headingText)
      self._cursor.ParaStyleName = self.STYLE_PARAM_HEADING % headingLevel
      headingNumber = self._cursor.ListLabelString

      self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
      self._cursor.ParaStyleName = self.STYLE_STANDARD_TEXT

      self.CurrentHeading = (headingLevel, headingText, headingNumber)

   def insertBoldFace(self, text):
      self._cursor.CharWeight = BOLD
      self.render(text)
      self._cursor.setPropertyToDefault("CharWeight")

   def insertSourceCode(self, text):
      self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
      oldStyle = self.changeParaStyle(self.STYLE_SOURCE_CODE)
   
      self.render(text)
   
      self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)
      self.changeParaStyle(oldStyle)

   def insertInlineSourceCode(self, text):
      old = self.changeCharStyle(self.STYLE_INLINE_SOURCE_CODE)
      if self.needSpace():
         self.insertString(' ')
      self.render(text)
      self.smartSpace()
      # Thanks, joern.
      #self._cursor.CharStyleName = "Default Style"
      self._cursor.setPropertyToDefault("CharStyleName")
      #self.changeCharStyle(old)

   def insertParagraph(self, text):
      self.render(text)
      self._document.Text.insertControlCharacter(self._cursor, PARAGRAPH_BREAK, False)

class VanillaRenderer(Renderer):
   def __init__(self):
      Renderer.__init__(self)
      self.STYLE_STANDARD_TEXT       = "Text body"
      self.STYLE_FIGURE_CAPTION      = "Text body"
      self.STYLE_TABLE_CAPTION       = "Text body"
      self.STYLE_LIST_1              = "List 1"
      self.STYLE_NUMBERING_1         = "Numbering 1"
      self.STYLE_TITLE               = "Title"
      self.STYLE_SUBTITLE            = "Subtitle"
      self.STYLE_TABLE_HEADER        = "Table Heading"
      self.STYLE_TABLE_HEADER_LEFT   = "Table Heading"
      self.STYLE_TABLE_CONTENT       = "Table Contents"
      self.STYLE_PARAM_HEADING       = "Heading %d"
      self.STYLE_TERMS               = "Text body"
      self.STYLE_SOURCE_CODE         = "Source Code"
      self.STYLE_INLINE_SOURCE_CODE  = "Source Code"
      self.STYLE_TABLE_HEADING_BACKGROUND = 11111111
      self.custom_i18n['en'] = {
         'figure': 'Figure',
         'table' : 'Table',
      }
      self.languageStrings['en'] = ("en", "US")

   def handleCustomMetaTag(self, tag):
      pass

   def handleCustomMetaContainer(self, properties, content):
      pass
