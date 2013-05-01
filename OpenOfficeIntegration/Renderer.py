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
      self.knownImageRefs = []
      self._hookRender = None

   def handleCustomMetaContainer(self, properties, document, cursor, content):
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

   def renderWord(self, document, cursor, string, lookAhead = None):
      self.insertString(document, cursor, string)
      if lookAhead:
         if type(lookAhead) == dict and 'type' in lookAhead and\
            lookAhead['type'] == 'ItemWord':
            self.insertString(document, cursor, ' ')
         elif type(lookAhead) == str or type(lookAhead) == unicode:
            self.insertString(document, cursor, ' ')

   def renderHeading(self, document, cursor, heading, items):
      level = heading["level"]
      self.insertHeading(document, cursor, level, items)

   def renderBoldFace(self, document, cursor, items):
      self.insertString(document, cursor, ' ')
      self.insertBoldFace(document, cursor, items)
      self.smartSpace(document, cursor)

   def renderParagraph(self, document, cursor, items):
      self.insertParagraph(document, cursor, items)

   def renderLinebreak(self, document, cursor):
      self.insertString(document, cursor, "\n")

   def renderMetaContainer(self, document, cursor, content, properties):
      if not 'type' in properties:
         return
      if properties['type'] == 'source':
         self.insertSourceCode(document, cursor, content)
      if properties['type'] == 'inlineSource':
         self.insertInlineSourceCode(document, cursor, content)
      elif self.handleCustomMetaContainer(properties, document, cursor, content):
         return
      else:
         # Ignore unknown meta container types..
         return

   def renderTable(self, document, cursor, table):
      normTable = [x['content'] for x in table]
      self.insertTable(document, cursor, normTable)

   def renderUListItem(self, document, cursor, uli, content):
      self.insertUListItem(document, cursor, content)

   def renderOListItem(self, document, cursor, oli, content, nl = None):
      self.insertOListItem(document, cursor, content, nl)

   def renderUlist(self, document, cursor, ulist):
      oldName = cursor.ParaStyleName
      for uli, content in ulist:
         self.currentListLevel += 1
         self.renderUListItem(document, cursor, uli, content)
         self.currentListLevel -= 1

      if self.currentListLevel == 0:
         document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
         cursor.ParaStyleName = oldName

   def renderOlist(self, document, cursor, olist):
      nl = 1
      oldName = cursor.ParaStyleName
      for oli, content in olist:
         self.currentListLevel += 1
         self.renderOListItem(document, cursor, oli, content, nl)
         nl = None
         self.currentListLevel -= 1

      if self.currentListLevel == 0:
         document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
         cursor.ParaStyleName = oldName

   def renderContainer(self, document, cursor, container):
      containerType = container['type']
      if containerType == 'DocumentHeading':
         self.renderHeading(document, cursor, container['DocumentHeading'], 
                            container['content'])
      elif containerType == 'DocumentBoldFace':
         self.renderBoldFace(document, cursor, container['content'])
      elif containerType == 'DocumentParagraph':
         self.renderParagraph(document, cursor, container['content'])
      elif containerType == 'DocumentTable':
         self.renderTable(document, cursor, container['content'])
      elif containerType == 'DocumentMetaContainer':
         self.renderMetaContainer(document, cursor, container['content'],
                             container['properties'])
      elif containerType == 'DocumentUList':
         self.renderUlist(document, cursor, container['content'])
      elif containerType == 'DocumentOList':
         self.renderOlist(document, cursor, container['content'])
      else:
         raise RuntimeError('Unsupported container type: %s' % containerType)

   def renderImage(self, document, cursor, img):
      self.insertImage(document, cursor, img['imageFilename'], img['imageCaption'],
                       img['imageLabel'])

   def handleCustomMetaTag(self, document, cursor, tag):
      raise NotImplementedError

   def renderMetaTag(self, document, cursor, tag):
      if 'language' in tag:
         lang = tag['language']
         self.changeLanguage(lang)
      if not 'type' in tag:
         self.handleCustomMetaTag(document, cursor, tag)
      elif tag['type'] == 'tableOfContents':
         self.insertTableOfContents(document, cursor)
      elif tag['type'] == 'label':
         self.insertReferenceMark(document, cursor, tag['name'])
      elif tag['type'] == 'ref':
         self.insertReference(document, cursor, tag['label'])
      elif tag['type'] == 'imgref':
         self.insertImageReference(document, cursor, tag['label'])
      elif tag['type'] == 'pagebreak':
         self.insertPageBreak(document, cursor)
      elif tag['type'] == 'footnote':
         self.insertFootnote(document, cursor, tag['text'])
      elif tag['type'] == 'inlineImage':
         self.insertInlineImage(document, cursor, tag['path'], tag['vOffset'])
      else:
         self.handleCustomMetaTag(document, cursor, tag)

   def _isWord(self, item):
      return self._getWord(item) != None

   def _getWord(self, item):
      if type(item) == str or type(item) == unicode:
         return item
      if type(item) == dict:
         if item['type'] == 'ItemWord':
            return item['ItemWord']
      return None

   def smartSpace(self, document, cursor):
      punctation = ('.', ',', ';', ':', '!', '?', '(', ')', '[', ']', '{', '}')
      def startsWithPunctation(x):
         for p in punctation:
            if x.startswith(p):
               return True
         return False
      def fun(document, cursor, item):
         if self._isWord(item) and not startsWithPunctation(self._getWord(item)):
            self.insertString(document, cursor, ' ')
         return True
      self._hookRender = fun

   def render(self, document, cursor, item, lookAhead = None):
      if self._hookRender:
         hr = self._hookRender
         self._hookRender = None
         if not hr(document, cursor, item):
            return
      if type(item) == str or type(item) == unicode:
         self.renderWord(document, cursor, item, lookAhead)
      elif type(item) == list or type(item) == tuple:
         for index, itm in enumerate(item):
            if self._hookRender:
               hr = self._hookRender
               self._hookRender = None
               if not hr(document, cursor, itm):
                  return
            if index+1 < len(item):
               self.render(document, cursor, itm, item[index+1])
            else:
               self.render(document, cursor, itm, None)
      elif type(item) == dict:
         itemType = item['type']
         if itemType == 'ItemWord':
            self.renderWord(document, cursor, item['ItemWord'], lookAhead)
         elif itemType == 'ItemDocumentContainer':
            self.renderContainer(document, cursor, item['ItemDocumentContainer'])
         elif itemType == 'ItemLinebreak':
            self.renderLinebreak(document, cursor)
         elif itemType == 'ItemMetaTag':
            self.renderMetaTag(document, cursor, item['ItemMetaTag'])
         elif itemType == 'ItemImage':
            self.renderImage(document, cursor, item['ItemImage'])
         else:
            raise RuntimeError('Cannot render item type: %s' % itemType)
      else:
         raise RuntimeError("Cannot render %s (type: %s)" % ((str(item),
                                                        str(type(item)))))

   # The main rendering function. Will be called from the "compiler"
   # chain.
   def renderJson(self, document, cursor, encodedDoc):
      self.render(document, cursor, json.loads(encodedDoc))
      for fun in self.afterRendering:
         fun(document, cursor)
 
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

   def changeParaStyle(self, cursor, newStyle):
      old = cursor.ParaStyleName
      cursor.ParaStyleName = newStyle
      return old

   def optimalTableWidth(self, document, table):
      document.getCurrentController().select(table)
      vc = document.getCurrentController().getViewCursor()
      vc.gotoEnd(True)
      vc.gotoEnd(True)
      provider = document.getCurrentController().Frame
      dispatcher = self.createUnoService("com.sun.star.frame.DispatchHelper")
      dispatcher.executeDispatch(provider, ".uno:SetOptimalColumnWidth", "", 0, ())
      vc.goDown(1, False)

      table.RelativeWidth = 100

   def guessImageSize(self, image):
      # Maximum width and height in 1/100 mm
      maxWidth = 120 * 100  # Template maximum width in millimeters is 150.
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
   
   def getOrCreateSequenceFieldMaster(self, document, name):
      masters = document.getTextFieldMasters()
      fName = "com.sun.star.text.FieldMaster.SetExpression." + name
      if not masters.hasByName(fName):
         masterField = document.createInstance("com.sun.star.text.FieldMaster.SetExpression")
         masterField.Name = name
         masterField.SubType = SEQUENCE
      else:
         masterField = masters.getByName(fName)
      return masterField

   def createSequenceField(self, document, name):
      field = document.createInstance("com.sun.star.text.TextField.SetExpression")
      field.NumberingType = ARABIC
      field.attachTextFieldMaster(self.getOrCreateSequenceFieldMaster(document, name))
      field.Content = name + " + 1"
      return field

   # Provide sizes in 1/100 mm if you need to.
   def _insertImage(self, document, cursor, path, inline = False, width = None, 
                    height = None, scale = None, vOffset = None):

      if platform.platform().lower().startswith('win'):
         url = 'file:///' + os.path.realpath(path).replace(':', '|')
      else:
         url = 'file://' + os.path.realpath(path)

      # This is pure crap. OOo.. WTF is wrong with you..?
      # We'll first insert the image into the internal bitmap table
      bitmaps = document.createInstance('com.sun.star.drawing.BitmapTable')
      try:
         internalUrl = bitmaps.getByName(url)
      except:
         bitmaps.insertByName(url, url) 
      internalUrl = bitmaps.getByName(url)

      # Now insert the image, *NOT* using the source from the internal
      # bitmap table, but instead using the external URL.
      graph = document.createInstance("com.sun.star.text.TextGraphicObject")
      if inline:
         graph.AnchorType = AS_CHARACTER
      document.Text.insertTextContent(cursor, graph, False)

      graph.GraphicURL = url

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

   def insertInlineImage(self, document, cursor, path, vOffset):
      self.insertString(document, cursor, ' ')
      self._insertImage(document, cursor, path, inline = True, scale = 0.15, vOffset = vOffset)
      self.insertString(document, cursor, ' ')

   def insertImage(self, document, cursor, path, caption, labelName):
      CAPTION_TITLE=self.i18n['figure']

      document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
      self._insertImage(document, cursor, path)

      oldStyle = self.changeParaStyle(cursor, self.STYLE_FIGURE_CAPTION)
      field = self.createSequenceField(document, CAPTION_TITLE)
      self.insertString(document, cursor, CAPTION_TITLE + ' ')
      document.Text.insertTextContent(cursor, field, False)
      self.insertString(document, cursor, ' - ')
      self.insertString(document, cursor, caption)

      document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
      self.changeParaStyle(cursor, oldStyle)

      # Remember the number of the current image, so that we'll later
      # be able to reference it properly.
      cnt = len(self.Images)
      self.Images[labelName] = cnt

   def insertUListItem(self, document, cursor, content):
      document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
      cursor.ParaStyleName = self.STYLE_LIST_1 # % global_currentListLevel
      cursor.NumberingLevel = self.currentListLevel - 1
      self.render(document, cursor, content)

   def insertOListItem(self, document, cursor, content, startVal = None):
      document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
      cursor.ParaStyleName = self.STYLE_NUMBERING_1 # % global_currentListLevel
      cursor.NumberingLevel = self.currentListLevel - 1
      if startVal != None:
         cursor.NumberingStartValue = startVal
      self.render(document, cursor, content)

   def insertString(self, document, cursor, text):
      document.Text.insertString(cursor, text, False)

   def insertFootnote(self, document, cursor, content):
      fn = document.createInstance("com.sun.star.text.Footnote")
      document.Text.insertTextContent(cursor, fn, False)
      fn.insertString(fn.createTextCursor(), content, False)
      self.smartSpace(document, cursor)

   def insertBookmark(self, document, cursor, name):
      bm = document.createInstance("com.sun.star.text.Bookmark")
      bm.Name = name
      document.Text.insertTextContent(cursor, bm, False)

   def insertReferenceMark(self, document, cursor, name):
      bm = document.createInstance("com.sun.star.text.ReferenceMark")
      bm.Name = name
      document.Text.insertTextContent(cursor, bm, False)

   def insertReference(self, document, cursor, name):
      field = document.createInstance("com.sun.star.text.textfield.GetReference")
      field.ReferenceFieldPart = CHAPTER
      field.ReferenceFieldSource = REFERENCE_MARK
      field.SourceName = name
      self.insertString(document, cursor, " ")
      document.Text.insertTextContent(cursor, field, False)
      document.getTextFields().refresh()
      document.refresh()
      self.smartSpace(document, cursor)

   def insertImageReference(self, document, cursor, name):
      # Fix for the nasty double reference problem.
      # Normally, it would insert two bookmarks with the
      # same name, which will cause tons of trouble.
      # This way, bookmark names will stay unique.
      refName = "_do_img_ref_" + name
      while refName in self.knownImageRefs:
         refName = 'X' + refName
      self.knownImageRefs.append(refName)
      self.insertBookmark(document, cursor, refName)

      def do_insert_imageref(document, cursor):
         where = document.getBookmarks().getByName(refName).getAnchor()
         cursor = where
         field = document.createInstance("com.sun.star.text.textfield.GetReference")
         field.ReferenceFieldPart = CATEGORY_AND_NUMBER
         field.ReferenceFieldSource = SEQUENCE_FIELD
         field.SourceName = self.i18n['figure']
         if not name in self.Images:
            raise RuntimeError('Unknown image %s' % name)
         field.SequenceNumber = self.Images[name] 
         # Inserting the space and the field in reverse content, because the
         # "cursor" is not going to be updated on these operations.
         self.smartSpace(document, cursor)
         document.Text.insertTextContent(cursor, field, False)
         # Also, insert a space before the reference
         self.insertString(document, cursor, " ")
         document.getTextFields().refresh()
         document.refresh()

      self.doAfterRendering(do_insert_imageref)

   def insertTableOfContents(self, document, cursor):
      index = document.createInstance("com.sun.star.text.ContentIndex")
      index.CreateFromOutline = True
      document.Text.insertTextContent(cursor, index, False)
      def updateToc(document, cursor):
         index.update()
      self.doAfterRendering(updateToc)

   def insertPageBreak(self, document, cursor):
      cursor.PageDescName = cursor.PageStyleName

   def insertTable(self, document, cursor, tableContent):
      numRows = len(tableContent)
      numCols = max(list(map(len, tableContent)))
   
      text = document.Text
      table = document.createInstance("com.sun.star.text.TextTable")
      table.initialize(numRows, numCols)
      text.insertTextContent(cursor, table, 0)
   
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
            self.render(cell, tmpCur, cellContent)
   
      self.optimalTableWidth(document, table)

   def insertHeading(self, document, cursor,
                     headingLevel, headingText):
      if headingLevel > 4:
         raise RuntimeError('Illegal heading level!')

      self.render(document, cursor, headingText)
      cursor.ParaStyleName = self.STYLE_PARAM_HEADING % headingLevel
      headingNumber = cursor.ListLabelString

      document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
      cursor.ParaStyleName = self.STYLE_STANDARD_TEXT

      self.CurrentHeading = (headingLevel, headingText, headingNumber)

   def insertBoldFace(self, document, cursor,
                      text):
      cursor.CharWeight = BOLD
      self.render(document, cursor, text)
      cursor.setPropertyToDefault("CharWeight")

   def insertSourceCode(self, document, cursor,
                        text):
      document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
      oldStyle = self.changeParaStyle(cursor, self.STYLE_SOURCE_CODE)
   
      self.render(document, cursor, text)
   
      document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
      self.changeParaStyle(cursor, oldStyle)

   def insertInlineSourceCode(self, document, cursor,
                              text):
      cursor.CharStyleName = self.STYLE_INLINE_SOURCE_CODE
      self.insertString(document, cursor, ' ')
      self.render(document, cursor, text)
      self.smartSpace(document, cursor)
      # cursor.CharStyleName = "Default"
      # Thanks, joern.
      cursor.setPropertyToDefault("CharStyleName")

   def insertParagraph(self, document, cursor,
                       text):
      self.render(document, cursor, text)
      document.Text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)

class VanillaRenderer(Renderer):
   def __init__(self):
      Renderer.__init__(self)
      self.STYLE_STANDARD_TEXT       = "Text body"
      self.STYLE_FIGURE_CAPTION      = "Text body"
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
      }
      self.changeLanguage('en')

   def handleCustomMetaTag(self, document, cursor, tag):
      pass

   def handleCustomMetaContainer(self, properties, document, cursor, content):
      pass
