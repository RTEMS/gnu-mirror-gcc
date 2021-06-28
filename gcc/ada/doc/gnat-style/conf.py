# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../share')

from adabaseconf import *

project = 'GNAT Coding Style: A Guide for GNAT Developers'
authors = 'AdaCore'

set_latex_elements(project)

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', 'gnat-style.tex', project, authors, 'manual'),
]

texinfo_documents = [
  ('index', 'gnat-style', project, authors, None, None, None, True)
]

tags.add(get_gnat_build_type())
