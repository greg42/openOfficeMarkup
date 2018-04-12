#!/usr/bin/env python

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
import os

from Renderer.Renderer import VanillaRenderer
from OOoBackend import OpenOfficeBackend

if len(sys.argv) < 4:
   sys.stderr.write('Usage: %s parsedDocument path_to_template [output_file]' % sys.argv[0])
   sys.exit(2)

content = open(sys.argv[1]).read()

renderer = VanillaRenderer()

backend = OpenOfficeBackend()
backend.getOOoInstance(headless = len(sys.argv) > 4)
backend.loadTemplate((os.path.realpath(sys.argv[2])))
backend.renderWith(renderer, content)

if len(sys.argv) > 3:
   outFile = sys.argv[3]
   backend.saveAs(outFile)
   backend.close()
