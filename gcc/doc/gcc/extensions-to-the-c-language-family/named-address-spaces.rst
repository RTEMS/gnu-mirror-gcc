..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _named-address-spaces:

Named Address Spaces
********************

.. index:: Named Address Spaces

As an extension, GNU C supports named address spaces as
defined in the N1275 draft of ISO/IEC DTR 18037.  Support for named
address spaces in GCC will evolve as the draft technical report
changes.  Calling conventions for any target might also change.  At
present, only the AVR, M32C, RL78, and x86 targets support
address spaces other than the generic address space.

Address space identifiers may be used exactly like any other C type
qualifier (e.g., ``const`` or ``volatile`` ).  See the N1275
document for more details.

.. _avr-named-address-spaces:

AVR Named Address Spaces
^^^^^^^^^^^^^^^^^^^^^^^^

On the AVR target, there are several address spaces that can be used
in order to put read-only data into the flash memory and access that
data by means of the special instructions ``LPM`` or ``ELPM``
needed to read from flash.

Devices belonging to ``avrtiny`` and ``avrxmega3`` can access
flash memory by means of ``LD*`` instructions because the flash
memory is mapped into the RAM address space.  There is *no need*
for language extensions like ``__flash`` or attribute
AVR Variable Attributes``progmem``.
The default linker description files for these devices cater for that
feature and ``.rodata`` stays in flash: The compiler just generates
``LD*`` instructions, and the linker script adds core specific
offsets to all ``.rodata`` symbols: ``0x4000`` in the case of
``avrtiny`` and ``0x8000`` in the case of ``avrxmega3``.
See AVR Options for a list of respective devices.

For devices not in ``avrtiny`` or ``avrxmega3``,
any data including read-only data is located in RAM (the generic
address space) because flash memory is not visible in the RAM address
space.  In order to locate read-only data in flash memory *and*
to generate the right instructions to access this data without
using (inline) assembler code, special address spaces are needed.

``__flash``

  .. index:: __flash AVR Named Address Spaces

  The ``__flash`` qualifier locates data in the
  ``.progmem.data`` section. Data is read using the ``LPM``
  instruction. Pointers to this address space are 16 bits wide.

``__flash1`` ``__flash2`` ``__flash3`` ``__flash4`` ``__flash5``

  .. index:: __flash1 AVR Named Address Spaces

  .. index:: __flash2 AVR Named Address Spaces

  .. index:: __flash3 AVR Named Address Spaces

  .. index:: __flash4 AVR Named Address Spaces

  .. index:: __flash5 AVR Named Address Spaces

  These are 16-bit address spaces locating data in section
  ``.progmemN.data`` where :samp:`{N}` refers to
  address space ``__flashN``.
  The compiler sets the ``RAMPZ`` segment register appropriately
  before reading data by means of the ``ELPM`` instruction.

``__memx``

  .. index:: __memx AVR Named Address Spaces

  This is a 24-bit address space that linearizes flash and RAM:
  If the high bit of the address is set, data is read from
  RAM using the lower two bytes as RAM address.
  If the high bit of the address is clear, data is read from flash
  with ``RAMPZ`` set according to the high byte of the address.
  See :ref:`avr-built-in-functions`.

  Objects in this address space are located in ``.progmemx.data``.

  Example

.. code-block:: c++

  char my_read (const __flash char ** p)
  {
      /* p is a pointer to RAM that points to a pointer to flash.
         The first indirection of p reads that flash pointer
         from RAM and the second indirection reads a char from this
         flash address.  */

      return **p;
  }

  /* Locate array[] in flash memory */
  const __flash int array[] = { 3, 5, 7, 11, 13, 17, 19 };

  int i = 1;

  int main (void)
  {
     /* Return 17 by reading from flash memory */
     return array[array[i]];
  }

For each named address space supported by avr-gcc there is an equally
named but uppercase built-in macro defined.
The purpose is to facilitate testing if respective address space
support is available or not:

.. code-block:: c++

  #ifdef __FLASH
  const __flash int var = 1;

  int read_var (void)
  {
      return var;
  }
  #else
  #include <avr/pgmspace.h> /* From AVR-LibC */

  const int var PROGMEM = 1;

  int read_var (void)
  {
      return (int) pgm_read_word (&var);
  }
  #endif /* __FLASH */

Notice that attribute AVR Variable Attributes``progmem``
locates data in flash but
accesses to these data read from generic address space, i.e.
from RAM,
so that you need special accessors like ``pgm_read_byte``
from `AVR-LibC <http://nongnu.org/avr-libc/user-manual/>`_
together with attribute ``progmem``.

Limitations and caveats

* Reading across the 64 KiB section boundary of
  the ``__flash`` or ``__flashN`` address spaces
  shows undefined behavior. The only address space that
  supports reading across the 64 KiB flash segment boundaries is
  ``__memx``.

* If you use one of the ``__flashN`` address spaces
  you must arrange your linker script to locate the
  ``.progmemN.data`` sections according to your needs.

* Any data or pointers to the non-generic address spaces must
  be qualified as ``const``, i.e. as read-only data.
  This still applies if the data in one of these address
  spaces like software version number or calibration lookup table are intended to
  be changed after load time by, say, a boot loader. In this case
  the right qualification is ``const`` ``volatile`` so that the compiler
  must not optimize away known values or insert them
  as immediates into operands of instructions.

* The following code initializes a variable ``pfoo``
  located in static storage with a 24-bit address:

  .. code-block:: c++

    extern const __memx char foo;
    const __memx void *pfoo = &foo;

* On the reduced Tiny devices like ATtiny40, no address spaces are supported.
  Just use vanilla C / C++ code without overhead as outlined above.
  Attribute ``progmem`` is supported but works differently,
  see AVR Variable Attributes.

M32C Named Address Spaces
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: __far M32C Named Address Spaces

On the M32C target, with the R8C and M16C CPU variants, variables
qualified with ``__far`` are accessed using 32-bit addresses in
order to access memory beyond the first 64 Ki bytes.  If
``__far`` is used with the M32CM or M32C CPU variants, it has no
effect.

RL78 Named Address Spaces
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: __far RL78 Named Address Spaces

On the RL78 target, variables qualified with ``__far`` are accessed
with 32-bit pointers (20-bit addresses) rather than the default 16-bit
addresses.  Non-far variables are assumed to appear in the topmost
64 KiB of the address space.

x86 Named Address Spaces
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: x86 named address spaces

On the x86 target, variables may be declared as being relative
to the ``%fs`` or ``%gs`` segments.

``__seg_fs`` ``__seg_gs``

  .. index:: __seg_fs x86 named address space

  .. index:: __seg_gs x86 named address space

  The object is accessed with the respective segment override prefix.

  The respective segment base must be set via some method specific to
  the operating system.  Rather than require an expensive system call
  to retrieve the segment base, these address spaces are not considered
  to be subspaces of the generic (flat) address space.  This means that
  explicit casts are required to convert pointers between these address
  spaces and the generic address space.  In practice the application
  should cast to ``uintptr_t`` and apply the segment base offset
  that it installed previously.

  The preprocessor symbols ``__SEG_FS`` and ``__SEG_GS`` are
  defined when these address spaces are supported.