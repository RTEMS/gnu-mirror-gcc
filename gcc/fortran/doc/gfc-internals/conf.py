# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../../../..//doc')

from baseconf import *

project = 'GNU Fortran Internals'
copyright = '2007-%s Free Software Foundation, Inc.' % YEAR
authors = 'The gfortran team'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', 'gfc-internals.tex', project, authors, 'manual'),
]

texinfo_documents = [
  ('index', 'gfc-internals', project, authors, None, None, None, True)
]

tags.add('gfc-internals')

if gcc_DEVPHASE == 'experimental':
    tags.add('development')
