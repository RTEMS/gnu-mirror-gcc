# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../../..//doc')

from baseconf import *

project = 'libgccjit'
copyright = '1987-%s Free Software Foundation, Inc.' % YEAR
authors = 'David Malcolm'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', 'libgccjit.tex', project, authors, 'manual'),
]

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('index', 'libgccjit', project, [authors], 1),
]

texinfo_documents = [
  ('index', 'libgccjit', project, authors, None, None, None, True)
]

tags.add('libgccjit')
