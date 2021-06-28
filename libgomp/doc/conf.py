# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../..//doc')

from baseconf import *

project = 'GNU Offloading and Multi Processing Runtime Library'
copyright = '2006-%s Free Software Foundation, Inc.' % YEAR
authors = 'GCC Developer Community'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', 'libgomp.tex', project, authors, 'manual'),
]

texinfo_documents = [
  ('index', 'libgomp', project, authors, None, None, None, True)
]

tags.add('libgomp')
