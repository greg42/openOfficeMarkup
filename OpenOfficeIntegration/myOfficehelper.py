# -*- coding: utf-8 -*-

#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# <code@gregorkopf.de> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a beer in return Gregor Kopf
# ----------------------------------------------------------------------------
#

import os
import signal
from sys import platform
from time import sleep

import uno
from com.sun.star.connection import NoConnectException

class OpenOfficeInstance(object):
   def __init__(self, headless = False):
      if "UNO_PATH" in os.environ:
         office = os.environ["UNO_PATH"]
      else:
         if platform.startswith("win"):
            # XXX
            office = ""
         else:
            # Lets hope that works..
            office = '/usr/bin'
      office = os.path.join(office, "soffice")
      if platform.startswith("win"):
          office += ".exe"

      self._pidfile = "/tmp/recurity_renderer_OOinstance" #XXX Windows compat needed
      xLocalContext = uno.getComponentContext()
      self._resolver = xLocalContext.ServiceManager.createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", xLocalContext)
      self._socket = "host=127.0.0.1,port=2002,tcpNoDelay=1"

      args = ["--invisible", "--nologo", "--nodefault", "--norestore", "--nofirststartwizard"]
      if headless:
         args.append("--headless")

      if platform.startswith("win"):
         cmdArray = ['"' + office + '"']
      else:
         cmdArray = [office]
      cmdArray += args + ["--accept=socket,%s;urp;" % self._socket]
      if( not os.path.isfile(self._pidfile)):
         self.pid = os.spawnv(os.P_NOWAIT, office, cmdArray)
         f = open(self._pidfile,"w")
         f.write(str(self.pid))

   def close(self):
     if(self.pid):
        os.kill(self.pid, signal.SIGTERM)
        os.remove(self._pidfile)

   def getContext(self):
      context = None
      for _ in range(20):
          try:
              context = self._resolver.resolve("uno:socket,host=localhost,port=2002;urp;StarOffice.ComponentContext")
              break
          except NoConnectException:
              sleep(0.5)

      if not context:
         raise RuntimeError("Cannot connect to soffice.", None)

      return context
