..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _code-generation:

Code Generation
***************

.. index:: options, code generation

In addition to the many :command:`gcc` options controlling code generation,
:command:`gdc` has several options specific to itself.

``-H``

  .. index:: -H

  Generates D interface files for all modules being compiled.  The compiler
  determines the output file based on the name of the input file, removes
  any directory components and suffix, and applies the :samp:`.di` suffix.

:samp:`-Hd {dir}`

  .. index:: -Hd

  Same as :option:`-H`, but writes interface files to directory :samp:`{dir}`.
  This option can be used with :option:`-Hf `:samp:`{file}` to independently set the
  output file and directory path.

:samp:`-Hf {file}`

  .. index:: -Hf

  Same as :option:`-H` but writes interface files to :samp:`{file}`.  This option can
  be used with :option:`-Hd` :samp:`{dir}` to independently set the output file and
  directory path.

``-M``

  .. index:: -M

  Output the module dependencies of all source files being compiled in a
  format suitable for :command:`make`.  The compiler outputs one
  :command:`make` rule containing the object file name for that source file,
  a colon, and the names of all imported files.

``-MM``

  .. index:: -MM

  Like :option:`-M` but does not mention imported modules from the D standard
  library package directories.

:samp:`-MF {file}`

  .. index:: -MF

  When used with :option:`-M` or :option:`-MM`, specifies a :samp:`{file}` to write
  the dependencies to.  When used with the driver options :option:`-MD` or
  :option:`-MMD`, :option:`-MF` overrides the default dependency output file.

``-MG``

  .. index:: -MG

  This option is for compatibility with :command:`gcc`, and is ignored by the
  compiler.

``-MP``

  .. index:: -MP

  Outputs a phony target for each dependency other than the modules being
  compiled, causing each to depend on nothing.

:samp:`-MT {target}`

  .. index:: -MT

  Change the :samp:`{target}` of the rule emitted by dependency generation
  to be exactly the string you specify.  If you want multiple targets,
  you can specify them as a single argument to :option:`-MT`, or use
  multiple :option:`-MT` options.

:samp:`-MQ {target}`

  .. index:: -MQ

  Same as :option:`-MT`, but it quotes any characters which are special to
  :command:`make`.

``-MD``

  .. index:: -MD

  This option is equivalent to :option:`-M -MF` :samp:`{file}`.  The driver
  determines :samp:`{file}` by removing any directory components and suffix
  from the input file, and then adding a :samp:`.deps` suffix.

``-MMD``

  .. index:: -MMD

  Like :option:`-MD` but does not mention imported modules from the D standard
  library package directories.

``-X``

  .. index:: -X

  Output information describing the contents of all source files being
  compiled in JSON format to a file.  The driver determines :samp:`{file}` by
  removing any directory components and suffix from the input file, and then
  adding a :samp:`.json` suffix.

:samp:`-Xf {file}`

  .. index:: -Xf

  Same as :option:`-X`, but writes all JSON contents to the specified
  :samp:`{file}`.

``-fdoc``

  .. index:: -fdoc

  Generates ``Ddoc`` documentation and writes it to a file.  The compiler
  determines :samp:`{file}` by removing any directory components and suffix
  from the input file, and then adding a :samp:`.html` suffix.

:samp:`-fdoc-dir={dir}`

  .. index:: -fdoc-dir

  Same as :option:`-fdoc`, but writes documentation to directory :samp:`{dir}`.
  This option can be used with :option:`-fdoc-file`:samp:`={file}` to
  independently set the output file and directory path.

:samp:`-fdoc-file={file}`

  .. index:: -fdoc-file

  Same as :option:`-fdoc`, but writes documentation to :samp:`{file}`.  This
  option can be used with :option:`-fdoc-dir`:samp:`={dir}` to independently
  set the output file and directory path.

:samp:`-fdoc-inc={file}`

  .. index:: -fdoc-inc

  Specify :samp:`{file}` as a :samp:`{Ddoc}` macro file to be read.  Multiple
  :option:`-fdoc-inc` options can be used, and files are read and processed
  in the same order.