..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _building_documentation:

Building Documentation
^^^^^^^^^^^^^^^^^^^^^^

The main GCC documentation uses `Sphinx`_ to generate pretty documentation
from `reStructuredText`_ format.
These are input for Manual pages format (:command:`make man`),
Info format (:command:`make info`), HTML format (:command:`make html`),
PDF format (:command:`make pdf`) and EPUB format (:command:`make epub`).

.. _Sphinx: http://www.sphinx-doc.org/
.. _reStructuredText: http://docutils.sourceforge.net/rst.html

Sphinx Install
==============

The ReST markups currently used by the documentation files are meant to be
built with ``Sphinx`` version |needs_sphinx| or higher.

Most distributions are shipped with Sphinx, but its toolchain is fragile,
and it is not uncommon that upgrading it or some other Python packages
on your machine would cause the documentation build to break.

A way to avoid that is to use a different version than the one shipped
with your distributions. In order to do so, it is recommended to install
Sphinx inside a virtual environment, using ``virtualenv-3``
or ``virtualenv``, depending on how your distribution packaged Python 3.

The HTML documentation uses RTD theme. Depending on the Sphinx version,
it should be installed separately, with ``pip install sphinx_rtd_theme``.

The PDF documentation needs `python3-Sphinx-latex` that installs the
corresponding XeLaTeX package. Moreover, `texlive-tex-gyre` texlive package
is needed.

Writing Documentation
=====================

Adding new documentation can be as simple as:

1. Add a new ``.rst`` file somewhere under a ``doc`` subfolder.
2. Refer to it from the Sphinx main `TOC tree`_ in a ``index.rst`` file.

.. _TOC tree: http://www.sphinx-doc.org/en/stable/markup/toctree.html

This is usually good enough for simple documentation (like the one you're
reading right now), but for larger documents it may be advisable to create a
subdirectory (or use an existing one).

See the documentation for `Sphinx`_ and `reStructuredText`_ on what you can do
with them. In particular, the Sphinx `reStructuredText Primer`_ is a good place
to get started with reStructuredText. There are also some `Sphinx specific
markup constructs`_.

.. _reStructuredText Primer: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx specific markup constructs: http://www.sphinx-doc.org/en/stable/markup/index.html

Specific guidelines for the GCC documentation
---------------------------------------------

Here are some specific guidelines for the GCC documentation:

* Please stick to this order of heading adornments:

  1. ``=`` with overline for document title::

       ==============
       Document title
       ==============

  2. ``-`` for chapters::

       Chapters
       --------

  3. ``*`` for sections::

       Section
       -------

  4. ``^`` for subsections::

       Subsection
       ^^^^^^^^^^

  5. ``^`` for subsubsections::

       Subsubsection
       ~~~~~~~~~~~~~

  Although RST doesn't mandate a specific order ("Rather than imposing a fixed
  number and order of section title adornment styles, the order enforced will be
  the order as encountered."), having the higher levels the same overall makes
  it easier to follow the documents.

* For inserting fixed width text blocks (for code examples, use case
  examples, etc.), use ``::`` for anything that doesn't really benefit
  from syntax highlighting, especially short snippets. Use
  ``.. code-block:: <language>`` for longer code blocks that benefit
  from highlighting. For a short snippet of code embedded in the text, use \`\`.

* GCC defines the following GCC-specific directives:

  * ``gcc-attr`` - GCC attributes that can be used for function, type, variable attributes
  * ``gcc-param`` - a GCC parameter directive, e.g. ``.. gcc-param: inline-unit-growth``

.. _miscellaneous-docs:

Miscellaneous Documentation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to the formal documentation that is installed by GCC,
there are several other text files in the :samp:`gcc` subdirectory
with miscellaneous documentation:

:samp:`ABOUT-GCC-NLS`
  Notes on GCC's Native Language Support.  FIXME: this should be part of
  this manual rather than a separate file.

:samp:`ABOUT-NLS`
  Notes on the Free Translation Project.

:samp:`COPYING`

  The GNU General Public License, Versions 2 and 3.

:samp:`COPYING.LIB` :samp:`COPYING3.LIB`
  The GNU Lesser General Public License, Versions 2.1 and 3.

:samp:`*ChangeLog*` :samp:`*/ChangeLog*`
  Change log files for various parts of GCC.

:samp:`LANGUAGES`

  Details of a few changes to the GCC front-end interface.  FIXME: the
  information in this file should be part of general documentation of
  the front-end interface in this manual.

:samp:`ONEWS`

  Information about new features in old versions of GCC.  (For recent
  versions, the information is on the GCC web site.)

:samp:`README.Portability`
  Information about portability issues when writing code in GCC.  FIXME:
  why isn't this part of this manual or of the GCC Coding Conventions?

  FIXME: document such files in subdirectories, at least :samp:`config`,
  :samp:`c`, :samp:`cp`, :samp:`objc`, :samp:`testsuite`.