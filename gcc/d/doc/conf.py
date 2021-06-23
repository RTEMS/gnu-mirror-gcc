# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../../..//doc')

from baseconf import *

project = 'The GNU D Compiler'
copyright = '2006-%s Free Software Foundation, Inc.' % YEAR
authors = 'David Friedman, Iain Buclaw'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', 'gdc.tex', project, authors, 'manual'),
]

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('invoking-gdc', 'gdc', project, [authors], 1),
]

texinfo_documents = [
  ('index', 'gdc', project, authors, None, None, None, True)
]

tags.add('gdc')
