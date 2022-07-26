..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _openmp-context-selectors:

OpenMP Context Selectors
************************

``vendor`` is always ``gnu``. References are to the GCC manual.

.. list-table::
   :header-rows: 1

   * - ``arch``
     - ``kind``
     - ``isa``

   * - ``intel_mic``, ``x86``, ``x86_64``, ``i386``, ``i486``, ``i586``, ``i686``, ``ia32``
     - ``host``
     - See ``-m...`` flags in :ref:`gcc:x86-options` (without ``-m``)
   * - ``amdgcn``, ``gcn``
     - ``gpu``
     - See ``-march=`` in :ref:`gcc:amd-gcn-options`
   * - ``nvptx``
     - ``gpu``
     - See ``-misa=`` in :ref:`gcc:nvidia-ptx-options`

.. -
   The libgomp ABI
   -