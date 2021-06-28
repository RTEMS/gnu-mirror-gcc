# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../..//doc')

from baseconf import *

project = 'GNU libiberty'
copyright = '2001-%s Free Software Foundation, Inc.' % YEAR
authors = 'Phil Edwards et al.'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', 'libiberty.tex', project, authors, 'manual'),
]


texinfo_documents = [
  ('index', 'libiberty', project, authors, None, None, None, True)
]

tags.add('libiberty')
