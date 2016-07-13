# -*- coding: utf-8 -*-

#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# <code@gregorkopf.de> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a beer in return. Gregor Kopf
# ----------------------------------------------------------------------------
#

import sys
import time

if sys.platform == 'darwin':
  sys.path.append('/Applications/NeoOffice.app/Contents/MacOS')
  # Thanks, Nathan
  sys.path.append('/Applications/LibreOffice.app/Contents/MacOS')
 
from myOfficehelper import OpenOfficeInstance
import uno

# a UNO struct later needed to create a document
import os
# Yeah, I know. But this works. Hackers gonna hack..
import traceback
currentDir = os.path.dirname(traceback.extract_stack()[-1][0])
sys.path.append(currentDir)
from com.sun.star.util import XCloseListener
import unohelper

class myCloseListener(unohelper.Base, XCloseListener):
    def __init__(self, caller):
        self.caller = caller
        self._alive = True

    def notifyClosing(self, eventObject):
        self._alive = False

    def queryClosing(self, eventObject, owner):
        self._alive = False

class OpenOfficeBackend(object):
   def __init__(self):
      self._instance = None
      self._doc = None

   def getOOoInstance(self, headless):
      # Just convert the document?
      if not self._instance:
         self._instance = OpenOfficeInstance(headless = True)
      self._headless = headless
      ctx = self._instance.getContext() 
      self._smgr = ctx.ServiceManager
      self._desktop = self._smgr.createInstanceWithContext('com.sun.star.frame.Desktop', ctx)
   
   def _convertToURL(self, pathname):
       """Convert a Windows or Linux pathname into an OOo URL."""
       if len(pathname) > 1:
           if pathname[1:2] == ":":
               pathname = "/" + pathname[0] + "|" + pathname[2:]
       pathname = pathname.replace("\\", "/")
       pathname = "file://" + pathname
       return pathname 

   def _isTextDoc(self, path):
      return path.endswith('.ott')

   def loadTemplate(self, templatePath):
      url = self._convertToURL(os.path.realpath(templatePath))
      args = (uno.createUnoStruct('com.sun.star.beans.PropertyValue'),)
      args[0].Name  = 'Hidden'
      args[0].Value = True
      if self._headless:
         self._doc = self._desktop.loadComponentFromURL(url, '_blank', 0, args)
      else:
         self._doc = self._desktop.loadComponentFromURL(url, '_blank', 0, ())
      
      if self._isTextDoc(templatePath):
         self._textCursor = self._doc.Text.createTextCursor()
      else:
         self._textCursor = None
      self._listener = myCloseListener(self._doc)
      self._doc.addCloseListener(self._listener)

   def renderWith(self, renderer, content, *args, **kw):
      renderer.init(self._doc, self._textCursor)
      renderer.renderJson(content, *args, **kw)

   def saveAs(self, outFile):
      if outFile.upper().endswith('.PDF'):
         args = (uno.createUnoStruct('com.sun.star.beans.PropertyValue'),)
         args[0].Name  = 'FilterName'
         args[0].Value = 'writer_pdf_Export'
      elif outFile.upper().endswith('.ODT'):
         args = ()
      elif outFile.upper().endswith('.OTS'):
         args = ()
      elif outFile.upper().endswith('.DOCX'):
         args = ()
      else:
         raise RuntimeError('Unknown output file format.')
      self._doc.storeToURL(self._convertToURL(os.path.realpath(outFile)), args)

   def close(self):
      if self._doc:
         self._doc.dispose()
      else:
         return
      # Wait for the document to be closed
      while self._listener._alive:
         time.sleep(0.1)
      self._instance.close()
