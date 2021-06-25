# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../share')

from adabaseconf import *

project = "GNAT User's Guide for Native Platforms"
authors = 'AdaCore'

set_latex_elements(project)

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', 'gnat_ugn.tex', project, authors, 'manual'),
]

texinfo_documents = [
  ('index', 'gnat_ugn', project, authors, None, None, None, True)
]

tags.add(get_gnat_build_type())
