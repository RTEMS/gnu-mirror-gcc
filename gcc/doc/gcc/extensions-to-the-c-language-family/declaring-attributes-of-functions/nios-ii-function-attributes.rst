..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _nios-ii-function-attributes:

Nios II Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Nios II back end:

.. option:: target (options)

  .. index:: target function attribute

  As discussed in Common Function Attributes, this attribute
  allows specification of target-specific compilation options.

  When compiling for Nios II, the following options are allowed:

  :samp:`custom-{insn}={N}` :samp:`no-custom-{insn}`

    .. index:: target("custom-insn=N") function attribute, Nios II

    .. index:: target("no-custom-insn") function attribute, Nios II

    Each :samp:`custom-{insn}={N}` attribute locally enables use of a
    custom instruction with encoding :samp:`{N}` when generating code that uses
    :samp:`{insn}`.  Similarly, :samp:`no-custom-{insn}` locally inhibits use of
    the custom instruction :samp:`{insn}`.
    These target attributes correspond to the
    :option:`-mcustom-`:samp:`{insn}` = :samp:`{N}` and :option:`-mno-custom-`:samp:`{insn}`
    command-line options, and support the same set of :samp:`{insn}` keywords.
    See :ref:`nios-ii-options`, for more information.

  :samp:`custom-fpu-cfg={name}`

    .. index:: target("custom-fpu-cfg=name") function attribute, Nios II

    This attribute corresponds to the :option:`-mcustom-fpu-cfg`:samp:`={name}`
    command-line option, to select a predefined set of custom instructions
    named :samp:`{name}`.
    See :ref:`nios-ii-options`, for more information.