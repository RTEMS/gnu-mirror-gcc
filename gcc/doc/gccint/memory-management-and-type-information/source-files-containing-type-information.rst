..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _files:

Source Files Containing Type Information
****************************************

.. index:: generated files

.. index:: files, generated

Whenever you add ``GTY`` markers to a source file that previously
had none, or create a new source file containing ``GTY`` markers,
there are three things you need to do:

* You need to add the file to the list of source files the type
  machinery scans.  There are four cases:

  * For a back-end file, this is usually done
    automatically; if not, you should add it to ``target_gtfiles`` in
    the appropriate port's entries in :samp:`config.gcc`.

  * For files shared by all front ends, add the filename to the
    ``GTFILES`` variable in :samp:`Makefile.in`.

  * For files that are part of one front end, add the filename to the
    ``gtfiles`` variable defined in the appropriate
    :samp:`config-lang.in`.
    Headers should appear before non-headers in this list.

  * For files that are part of some but not all front ends, add the
    filename to the ``gtfiles`` variable of *all* the front ends
    that use it.

* If the file was a header file, you'll need to check that it's included
  in the right place to be visible to the generated files.  For a back-end
  header file, this should be done automatically.  For a front-end header
  file, it needs to be included by the same file that includes
  :samp:`gtype-{lang}.h`.  For other header files, it needs to be
  included in :samp:`gtype-desc.c`, which is a generated file, so add it to
  ``ifiles`` in ``open_base_file`` in :samp:`gengtype.c`.

  For source files that aren't header files, the machinery will generate a
  header file that should be included in the source file you just changed.
  The file will be called :samp:`gt-{path}.h` where :samp:`{path}` is the
  pathname relative to the :samp:`gcc` directory with slashes replaced by
  -, so for example the header file to be included in
  :samp:`cp/parser.c` is called :samp:`gt-cp-parser.c`.  The
  generated header file should be included after everything else in the
  source file.  Don't forget to mention this file as a dependency in the
  :samp:`Makefile`!

For language frontends, there is another file that needs to be included
somewhere.  It will be called :samp:`gtype-{lang}.h`, where
:samp:`{lang}` is the name of the subdirectory the language is contained in.

Plugins can add additional root tables.  Run the ``gengtype``
utility in plugin mode as ``gengtype -P pluginout.h source-dirfile-listplugin*.c`` with your plugin files
:samp:`{plugin*.c}` using ``GTY`` to generate the :samp:`{pluginout.h}` file.
The GCC build tree is needed to be present in that mode.