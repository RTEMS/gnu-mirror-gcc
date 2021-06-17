..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _variable-attributes:

Specifying Attributes of Variables
**********************************

.. index:: attribute of variables

.. index:: variable attributes

The keyword ``__attribute__`` allows you to specify special properties
of variables, function parameters, or structure, union, and, in C++, class
members.  This ``__attribute__`` keyword is followed by an attribute
specification enclosed in double parentheses.  Some attributes are currently
defined generically for variables.  Other attributes are defined for
variables on particular target systems.  Other attributes are available
for functions (see :ref:`function-attributes`), labels (see :ref:`label-attributes`),
enumerators (see :ref:`enumerator-attributes`), statements
(see :ref:`statement-attributes`), and for types (see :ref:`type-attributes`).
Other front ends might define more attributes
(see :ref:`Extensions to the C++ Language <c++-extensions>`).

See :ref:`attribute-syntax`, for details of the exact syntax for using
attributes.

.. toctree::
  :maxdepth: 2


.. _common-variable-attributes:

Common Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

The following attributes are supported on most targets.

.. option:: alias ("target")

  .. index:: alias variable attribute

  The ``alias`` variable attribute causes the declaration to be emitted
  as an alias for another symbol known as an :dfn:`alias target`.  Except
  for top-level qualifiers the alias target must have the same type as
  the alias.  For instance, the following

  .. code-block:: c++

    int var_target;
    extern int __attribute__ ((alias ("var_target"))) var_alias;

  defines ``var_alias`` to be an alias for the ``var_target`` variable.

  It is an error if the alias target is not defined in the same translation
  unit as the alias.

  Note that in the absence of the attribute GCC assumes that distinct
  declarations with external linkage denote distinct objects.  Using both
  the alias and the alias target to access the same object is undefined
  in a translation unit without a declaration of the alias with the attribute.

  This attribute requires assembler and object file support, and may not be
  available on all targets.

  .. index:: aligned variable attribute

.. option:: aligned

  The ``aligned`` attribute specifies a minimum alignment for the variable
  or structure field, measured in bytes.  When specified, :samp:`{alignment}` must
  be an integer constant power of 2.  Specifying no :samp:`{alignment}` argument
  implies the maximum alignment for the target, which is often, but by no
  means always, 8 or 16 bytes.

  For example, the declaration:

  .. code-block:: c++

    int x __attribute__ ((aligned (16))) = 0;

  causes the compiler to allocate the global variable ``x`` on a
  16-byte boundary.  On a 68040, this could be used in conjunction with
  an ``asm`` expression to access the ``move16`` instruction which
  requires 16-byte aligned operands.

  You can also specify the alignment of structure fields.  For example, to
  create a double-word aligned ``int`` pair, you could write:

  .. code-block:: c++

    struct foo { int x[2] __attribute__ ((aligned (8))); };

  This is an alternative to creating a union with a ``double`` member,
  which forces the union to be double-word aligned.

  As in the preceding examples, you can explicitly specify the alignment
  (in bytes) that you wish the compiler to use for a given variable or
  structure field.  Alternatively, you can leave out the alignment factor
  and just ask the compiler to align a variable or field to the
  default alignment for the target architecture you are compiling for.
  The default alignment is sufficient for all scalar types, but may not be
  enough for all vector types on a target that supports vector operations.
  The default alignment is fixed for a particular target ABI.

  GCC also provides a target specific macro ``__BIGGEST_ALIGNMENT__``,
  which is the largest alignment ever used for any data type on the
  target machine you are compiling for.  For example, you could write:

  .. code-block:: c++

    short array[3] __attribute__ ((aligned (__BIGGEST_ALIGNMENT__)));

  The compiler automatically sets the alignment for the declared
  variable or field to ``__BIGGEST_ALIGNMENT__``.  Doing this can
  often make copy operations more efficient, because the compiler can
  use whatever instructions copy the biggest chunks of memory when
  performing copies to or from the variables or fields that you have
  aligned this way.  Note that the value of ``__BIGGEST_ALIGNMENT__``
  may change depending on command-line options.

  When used on a struct, or struct member, the ``aligned`` attribute can
  only increase the alignment; in order to decrease it, the ``packed``
  attribute must be specified as well.  When used as part of a typedef, the
  ``aligned`` attribute can both increase and decrease alignment, and
  specifying the ``packed`` attribute generates a warning.

  Note that the effectiveness of ``aligned`` attributes for static
  variables may be limited by inherent limitations in the system linker
  and/or object file format.  On some systems, the linker is
  only able to arrange for variables to be aligned up to a certain maximum
  alignment.  (For some linkers, the maximum supported alignment may
  be very very small.)  If your linker is only able to align variables
  up to a maximum of 8-byte alignment, then specifying ``aligned(16)``
  in an ``__attribute__`` still only provides you with 8-byte
  alignment.  See your linker documentation for further information.

  Stack variables are not affected by linker restrictions; GCC can properly
  align them on any target.

  The ``aligned`` attribute can also be used for functions
  (see :ref:`common-function-attributes`.)

  .. index:: warn_if_not_aligned variable attribute

.. option:: warn_if_not_aligned (alignment)

  This attribute specifies a threshold for the structure field, measured
  in bytes.  If the structure field is aligned below the threshold, a
  warning will be issued.  For example, the declaration:

  .. code-block:: c++

    struct foo
    {
      int i1;
      int i2;
      unsigned long long x __attribute__ ((warn_if_not_aligned (16)));
    };

  causes the compiler to issue an warning on ``struct foo``, like
  :samp:`warning: alignment 8 of 'struct foo' is less than 16`.
  The compiler also issues a warning, like :samp:`warning: 'x' offset
  8 in 'struct foo' isn't aligned to 16`, when the structure field has
  the misaligned offset:

  .. code-block:: c++

    struct __attribute__ ((aligned (16))) foo
    {
      int i1;
      int i2;
      unsigned long long x __attribute__ ((warn_if_not_aligned (16)));
    };

  This warning can be disabled by :option:`-Wno-if-not-aligned`.
  The ``warn_if_not_aligned`` attribute can also be used for types
  (see :ref:`common-type-attributes`.)

.. option:: alloc_size (position)

  .. index:: alloc_size variable attribute

  The ``alloc_size`` variable attribute may be applied to the declaration
  of a pointer to a function that returns a pointer and takes at least one
  argument of an integer type.  It indicates that the returned pointer points
  to an object whose size is given by the function argument at :samp:`{position-1}`,
  or by the product of the arguments at :samp:`{position-1}` and :samp:`{position-2}`.
  Meaningful sizes are positive values less than ``PTRDIFF_MAX``.  Other
  sizes are disagnosed when detected.  GCC uses this information to improve
  the results of ``__builtin_object_size``.

  For instance, the following declarations

  .. code-block:: c++

    typedef __attribute__ ((alloc_size (1, 2))) void*
      (*calloc_ptr) (size_t, size_t);
    typedef __attribute__ ((alloc_size (1))) void*
      (*malloc_ptr) (size_t);

  specify that ``calloc_ptr`` is a pointer of a function that, like
  the standard C function ``calloc``, returns an object whose size
  is given by the product of arguments 1 and 2, and similarly, that
  ``malloc_ptr``, like the standard C function ``malloc``,
  returns an object whose size is given by argument 1 to the function.

.. option:: cleanup (cleanup_function)

  .. index:: cleanup variable attribute

  The ``cleanup`` attribute runs a function when the variable goes
  out of scope.  This attribute can only be applied to auto function
  scope variables; it may not be applied to parameters or variables
  with static storage duration.  The function must take one parameter,
  a pointer to a type compatible with the variable.  The return value
  of the function (if any) is ignored.

  If :option:`-fexceptions` is enabled, then :samp:`{cleanup_function}`
  is run during the stack unwinding that happens during the
  processing of the exception.  Note that the ``cleanup`` attribute
  does not allow the exception to be caught, only to perform an action.
  It is undefined what happens if :samp:`{cleanup_function}` does not
  return normally.

.. option:: common, nocommon

  .. index:: common variable attribute

  .. index:: nocommon variable attribute

  The ``common`` attribute requests GCC to place a variable in
  'common' storage.  The ``nocommon`` attribute requests the
  opposite---to allocate space for it directly.

  These attributes override the default chosen by the
  :option:`-fno-common` and :option:`-fcommon` flags respectively.

.. option:: copy

  .. index:: copy variable attribute

  The ``copy`` attribute applies the set of attributes with which
  :samp:`{variable}` has been declared to the declaration of the variable
  to which the attribute is applied.  The attribute is designed for
  libraries that define aliases that are expected to specify the same
  set of attributes as the aliased symbols.  The ``copy`` attribute
  can be used with variables, functions or types.  However, the kind
  of symbol to which the attribute is applied (either varible or
  function) must match the kind of symbol to which the argument refers.
  The ``copy`` attribute copies only syntactic and semantic attributes
  but not attributes that affect a symbol's linkage or visibility such as
  ``alias``, ``visibility``, or ``weak``.  The ``deprecated``
  attribute is also not copied.  See :ref:`common-function-attributes`.
  See :ref:`common-type-attributes`.

.. option:: deprecated

  .. index:: deprecated variable attribute

  The ``deprecated`` attribute results in a warning if the variable
  is used anywhere in the source file.  This is useful when identifying
  variables that are expected to be removed in a future version of a
  program.  The warning also includes the location of the declaration
  of the deprecated variable, to enable users to easily find further
  information about why the variable is deprecated, or what they should
  do instead.  Note that the warning only occurs for uses:

  .. code-block:: c++

    extern int old_var __attribute__ ((deprecated));
    extern int old_var;
    int new_fn () { return old_var; }

  results in a warning on line 3 but not line 2.  The optional :samp:`{msg}`
  argument, which must be a string, is printed in the warning if
  present.

  The ``deprecated`` attribute can also be used for functions and
  types (see :ref:`common-function-attributes`,
  see :ref:`common-type-attributes`).

  The message attached to the attribute is affected by the setting of
  the :option:`-fmessage-length` option.

.. option:: mode (mode)

  .. index:: mode variable attribute

  This attribute specifies the data type for the declaration---whichever
  type corresponds to the mode :samp:`{mode}`.  This in effect lets you
  request an integer or floating-point type according to its width.

  See :ref:`machine-modes`,
  for a list of the possible keywords for :samp:`{mode}`.
  You may also specify a mode of ``byte`` or ``__byte__`` to
  indicate the mode corresponding to a one-byte integer, ``word`` or
  ``__word__`` for the mode of a one-word integer, and ``pointer``
  or ``__pointer__`` for the mode used to represent pointers.

.. option:: nonstring

  .. index:: nonstring variable attribute

  The ``nonstring`` variable attribute specifies that an object or member
  declaration with type array of ``char``, ``signed char``, or
  ``unsigned char``, or pointer to such a type is intended to store
  character arrays that do not necessarily contain a terminating ``NUL``.
  This is useful in detecting uses of such arrays or pointers with functions
  that expect ``NUL`` -terminated strings, and to avoid warnings when such
  an array or pointer is used as an argument to a bounded string manipulation
  function such as ``strncpy``.  For example, without the attribute, GCC
  will issue a warning for the ``strncpy`` call below because it may
  truncate the copy without appending the terminating ``NUL`` character.
  Using the attribute makes it possible to suppress the warning.  However,
  when the array is declared with the attribute the call to ``strlen`` is
  diagnosed because when the array doesn't contain a ``NUL`` -terminated
  string the call is undefined.  To copy, compare, of search non-string
  character arrays use the ``memcpy``, ``memcmp``, ``memchr``,
  and other functions that operate on arrays of bytes.  In addition,
  calling ``strnlen`` and ``strndup`` with such arrays is safe
  provided a suitable bound is specified, and not diagnosed.

  .. code-block:: c++

    struct Data
    {
      char name [32] __attribute__ ((nonstring));
    };

    int f (struct Data *pd, const char *s)
    {
      strncpy (pd->name, s, sizeof pd->name);
      ...
      return strlen (pd->name);   // unsafe, gets a warning
    }

.. option:: packed

  .. index:: packed variable attribute

  The ``packed`` attribute specifies that a structure member should have
  the smallest possible alignment---one bit for a bit-field and one byte
  otherwise, unless a larger value is specified with the ``aligned``
  attribute.  The attribute does not apply to non-member objects.

  For example in the structure below, the member array ``x`` is packed
  so that it immediately follows ``a`` with no intervening padding:

  .. code-block:: c++

    struct foo
    {
      char a;
      int x[2] __attribute__ ((packed));
    };

  *Note:* The 4.1, 4.2 and 4.3 series of GCC ignore the
  ``packed`` attribute on bit-fields of type ``char``.  This has
  been fixed in GCC 4.4 but the change can lead to differences in the
  structure layout.  See the documentation of
  :option:`-Wpacked-bitfield-compat` for more information.

.. option:: section ("section-name")

  .. index:: section variable attribute

  Normally, the compiler places the objects it generates in sections like
  ``data`` and ``bss``.  Sometimes, however, you need additional sections,
  or you need certain particular variables to appear in special sections,
  for example to map to special hardware.  The ``section``
  attribute specifies that a variable (or function) lives in a particular
  section.  For example, this small program uses several specific section names:

  .. code-block:: c++

    struct duart a __attribute__ ((section ("DUART_A"))) = { 0 };
    struct duart b __attribute__ ((section ("DUART_B"))) = { 0 };
    char stack[10000] __attribute__ ((section ("STACK"))) = { 0 };
    int init_data __attribute__ ((section ("INITDATA")));

    main()
    {
      /* Initialize stack pointer */
      init_sp (stack + sizeof (stack));

      /* Initialize initialized data */
      memcpy (&init_data, &data, &edata - &data);

      /* Turn on the serial ports */
      init_duart (&a);
      init_duart (&b);
    }

  Use the ``section`` attribute with
  *global* variables and not *local* variables,
  as shown in the example.

  You may use the ``section`` attribute with initialized or
  uninitialized global variables but the linker requires
  each object be defined once, with the exception that uninitialized
  variables tentatively go in the ``common`` (or ``bss`` ) section
  and can be multiply 'defined'.  Using the ``section`` attribute
  changes what section the variable goes into and may cause the
  linker to issue an error if an uninitialized variable has multiple
  definitions.  You can force a variable to be initialized with the
  :option:`-fno-common` flag or the ``nocommon`` attribute.

  Some file formats do not support arbitrary sections so the ``section``
  attribute is not available on all platforms.
  If you need to map the entire contents of a module to a particular
  section, consider using the facilities of the linker instead.

.. option:: tls_model ("tls_model")

  .. index:: tls_model variable attribute

  The ``tls_model`` attribute sets thread-local storage model
  (see :ref:`thread-local`) of a particular ``__thread`` variable,
  overriding :option:`-ftls-model` = command-line switch on a per-variable
  basis.
  The :samp:`{tls_model}` argument should be one of ``global-dynamic``,
  ``local-dynamic``, ``initial-exec`` or ``local-exec``.

  Not all targets support this attribute.

.. option:: unused

  .. index:: unused variable attribute

  This attribute, attached to a variable or structure field, means that
  the variable or field is meant to be possibly unused.  GCC does not
  produce a warning for this variable or field.

.. option:: used

  .. index:: used variable attribute

  This attribute, attached to a variable with static storage, means that
  the variable must be emitted even if it appears that the variable is not
  referenced.

  When applied to a static data member of a C++ class template, the
  attribute also means that the member is instantiated if the
  class itself is instantiated.

.. option:: retain

  .. index:: retain variable attribute

  For ELF targets that support the GNU or FreeBSD OSABIs, this attribute
  will save the variable from linker garbage collection.  To support
  this behavior, variables that have not been placed in specific sections
  (e.g. by the ``section`` attribute, or the ``-fdata-sections`` option),
  will be placed in new, unique sections.

  This additional functionality requires Binutils version 2.36 or later.

.. option:: vector_size (bytes)

  .. index:: vector_size variable attribute

  This attribute specifies the vector size for the type of the declared
  variable, measured in bytes.  The type to which it applies is known as
  the :dfn:`base type`.  The :samp:`{bytes}` argument must be a positive
  power-of-two multiple of the base type size.  For example, the declaration:

  .. code-block:: c++

    int foo __attribute__ ((vector_size (16)));

  causes the compiler to set the mode for ``foo``, to be 16 bytes,
  divided into ``int`` sized units.  Assuming a 32-bit ``int``,
  ``foo`` 's type is a vector of four units of four bytes each, and
  the corresponding mode of ``foo`` is ``V4SI``.
  See :ref:`vector-extensions`, for details of manipulating vector variables.

  This attribute is only applicable to integral and floating scalars,
  although arrays, pointers, and function return values are allowed in
  conjunction with this construct.

  Aggregates with this attribute are invalid, even if they are of the same
  size as a corresponding scalar.  For example, the declaration:

  .. code-block:: c++

    struct S { int a; };
    struct S  __attribute__ ((vector_size (16))) foo;

  is invalid even if the size of the structure is the same as the size of
  the ``int``.

.. option:: visibility ("visibility_type")

  .. index:: visibility variable attribute

  This attribute affects the linkage of the declaration to which it is attached.
  The ``visibility`` attribute is described in
  Common Function Attributes.

.. option:: weak

  .. index:: weak variable attribute

  The ``weak`` attribute is described in
  Common Function Attributes.

.. option:: noinit

  .. index:: noinit variable attribute

  Any data with the ``noinit`` attribute will not be initialized by
  the C runtime startup code, or the program loader.  Not initializing
  data in this way can reduce program startup times.

  This attribute is specific to ELF targets and relies on the linker
  script to place sections with the ``.noinit`` prefix in the right
  location.

.. option:: persistent

  .. index:: persistent variable attribute

  Any data with the ``persistent`` attribute will not be initialized by
  the C runtime startup code, but will be initialized by the program
  loader.  This enables the value of the variable to :samp:`persist`
  between processor resets.

  This attribute is specific to ELF targets and relies on the linker
  script to place the sections with the ``.persistent`` prefix in the
  right location.  Specifically, some type of non-volatile, writeable
  memory is required.

.. option:: objc_nullability (nullability kind) (Objective-C and Objective-C++ only)

  .. index:: objc_nullability variable attribute

  This attribute applies to pointer variables only.  It allows marking the
  pointer with one of four possible values describing the conditions under
  which the pointer might have a ``nil`` value. In most cases, the
  attribute is intended to be an internal representation for property and
  method nullability (specified by language keywords); it is not recommended
  to use it directly.

  When :samp:`{nullability kind}` is ``"unspecified"`` or ``0``, nothing is
  known about the conditions in which the pointer might be ``nil``. Making
  this state specific serves to avoid false positives in diagnostics.

  When :samp:`{nullability kind}` is ``"nonnull"`` or ``1``, the pointer has
  no meaning if it is ``nil`` and thus the compiler is free to emit
  diagnostics if it can be determined that the value will be ``nil``.

  When :samp:`{nullability kind}` is ``"nullable"`` or ``2``, the pointer might
  be ``nil`` and carry meaning as such.

  When :samp:`{nullability kind}` is ``"resettable"`` or ``3`` (used only in
  the context of property attribute lists) this describes the case in which a
  property setter may take the value ``nil`` (which perhaps causes the
  property to be reset in some manner to a default) but for which the property
  getter will never validly return ``nil``.

.. _arc-variable-attributes:

ARC Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^

.. option:: aux

  .. index:: aux variable attribute, ARC

  The ``aux`` attribute is used to directly access the ARC's
  auxiliary register space from C.  The auxilirary register number is
  given via attribute argument.

.. _avr-variable-attributes:

AVR Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^

.. option:: progmem

  .. index:: progmem variable attribute, AVR

  The ``progmem`` attribute is used on the AVR to place read-only
  data in the non-volatile program memory (flash). The ``progmem``
  attribute accomplishes this by putting respective variables into a
  section whose name starts with ``.progmem``.

  This attribute works similar to the ``section`` attribute
  but adds additional checking.

  * Ordinary AVR cores with 32 general purpose registers:
    ``progmem`` affects the location
    of the data but not how this data is accessed.
    In order to read data located with the ``progmem`` attribute
    (inline) assembler must be used.

    .. code-block:: c++

      /* Use custom macros from http://nongnu.org/avr-libc/user-manual/AVR-LibC */
      #include <avr/pgmspace.h>

      /* Locate var in flash memory */
      const int var[2] PROGMEM = { 1, 2 };

      int read_var (int i)
      {
          /* Access var[] by accessor macro from avr/pgmspace.h */
          return (int) pgm_read_word (& var[i]);
      }

    AVR is a Harvard architecture processor and data and read-only data
    normally resides in the data memory (RAM).

    See also the AVR Named Address Spaces section for
    an alternate way to locate and access data in flash memory.

  * AVR cores with flash memory visible in the RAM address range:
    On such devices, there is no need for attribute ``progmem`` or
    AVR Named Address Spaces``__flash`` qualifier at all.
    Just use standard C / C++.  The compiler will generate ``LD*``
    instructions.  As flash memory is visible in the RAM address range,
    and the default linker script does *not* locate ``.rodata`` in
    RAM, no special features are needed in order not to waste RAM for
    read-only data or to read from flash.  You might even get slightly better
    performance by
    avoiding ``progmem`` and ``__flash``.  This applies to devices from
    families ``avrtiny`` and ``avrxmega3``, see AVR Options for
    an overview.

  * Reduced AVR Tiny cores like ATtiny40:
    The compiler adds ``0x4000``
    to the addresses of objects and declarations in ``progmem`` and locates
    the objects in flash memory, namely in section ``.progmem.data``.
    The offset is needed because the flash memory is visible in the RAM
    address space starting at address ``0x4000``.

    Data in ``progmem`` can be accessed by means of ordinary C code,
    no special functions or macros are needed.

    .. code-block:: c++

      /* var is located in flash memory */
      extern const int var[2] __attribute__((progmem));

      int read_var (int i)
      {
          return var[i];
      }

    Please notice that on these devices, there is no need for ``progmem``
    at all.

.. option:: io

  .. index:: io variable attribute, AVR

  Variables with the ``io`` attribute are used to address
  memory-mapped peripherals in the io address range.
  If an address is specified, the variable
  is assigned that address, and the value is interpreted as an
  address in the data address space.
  Example:

  .. code-block:: c++

    volatile int porta __attribute__((io (0x22)));

  The address specified in the address in the data address range.

  Otherwise, the variable it is not assigned an address, but the
  compiler will still use in/out instructions where applicable,
  assuming some other module assigns an address in the io address range.
  Example:

  .. code-block:: c++

    extern volatile int porta __attribute__((io));

.. option:: io_low

  .. index:: io_low variable attribute, AVR

  This is like the ``io`` attribute, but additionally it informs the
  compiler that the object lies in the lower half of the I/O area,
  allowing the use of ``cbi``, ``sbi``, ``sbic`` and ``sbis``
  instructions.

.. option:: address

  .. index:: address variable attribute, AVR

  Variables with the ``address`` attribute are used to address
  memory-mapped peripherals that may lie outside the io address range.

  .. code-block:: c++

    volatile int porta __attribute__((address (0x600)));

.. option:: absdata

  .. index:: absdata variable attribute, AVR

  Variables in static storage and with the ``absdata`` attribute can
  be accessed by the ``LDS`` and ``STS`` instructions which take
  absolute addresses.

  * This attribute is only supported for the reduced AVR Tiny core
    like ATtiny40.

  * You must make sure that respective data is located in the
    address range ``0x40``... ``0xbf`` accessible by
    ``LDS`` and ``STS``.  One way to achieve this as an
    appropriate linker description file.

  * If the location does not fit the address range of ``LDS``
    and ``STS``, there is currently (Binutils 2.26) just an unspecific
    warning like

    ``module.c:(.text+0x1c): warning: internal error: out of range error``

  See also the :option:`-mabsdata` AVR Optionscommand-line option.

.. _blackfin-variable-attributes:

Blackfin Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Three attributes are currently defined for the Blackfin.

.. option:: l1_data

  .. index:: l1_data variable attribute, Blackfin

  .. index:: l1_data_A variable attribute, Blackfin

  .. index:: l1_data_B variable attribute, Blackfin

  Use these attributes on the Blackfin to place the variable into L1 Data SRAM.
  Variables with ``l1_data`` attribute are put into the specific section
  named ``.l1.data``. Those with ``l1_data_A`` attribute are put into
  the specific section named ``.l1.data.A``. Those with ``l1_data_B``
  attribute are put into the specific section named ``.l1.data.B``.

.. option:: l2

  .. index:: l2 variable attribute, Blackfin

  Use this attribute on the Blackfin to place the variable into L2 SRAM.
  Variables with ``l2`` attribute are put into the specific section
  named ``.l2.data``.

.. _h8-300-variable-attributes:

H8/300 Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These variable attributes are available for H8/300 targets:

.. option:: eightbit_data

  .. index:: eightbit_data variable attribute, H8/300

  .. index:: eight-bit data on the H8/300, H8/300H, and H8S

  Use this attribute on the H8/300, H8/300H, and H8S to indicate that the specified
  variable should be placed into the eight-bit data section.
  The compiler generates more efficient code for certain operations
  on data in the eight-bit data area.  Note the eight-bit data area is limited to
  256 bytes of data.

  You must use GAS and GLD from GNU binutils version 2.7 or later for
  this attribute to work correctly.

.. option:: tiny_data

  .. index:: tiny_data variable attribute, H8/300

  .. index:: tiny data section on the H8/300H and H8S

  Use this attribute on the H8/300H and H8S to indicate that the specified
  variable should be placed into the tiny data section.
  The compiler generates more efficient code for loads and stores
  on data in the tiny data section.  Note the tiny data area is limited to
  slightly under 32KB of data.

.. _ia-64-variable-attributes:

IA-64 Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

The IA-64 back end supports the following variable attribute:

.. option:: model (model-name)

  .. index:: model variable attribute, IA-64

  On IA-64, use this attribute to set the addressability of an object.
  At present, the only supported identifier for :samp:`{model-name}` is
  ``small``, indicating addressability via 'small' (22-bit)
  addresses (so that their addresses can be loaded with the ``addl``
  instruction).  Caveat: such addressing is by definition not position
  independent and hence this attribute must not be used for objects
  defined by shared libraries.

.. _m32r-d-variable-attributes:

M32R/D Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

One attribute is currently defined for the M32R/D.

.. option:: model (model-name)

  .. index:: model-name variable attribute, M32R/D

  .. index:: variable addressability on the M32R/D

  Use this attribute on the M32R/D to set the addressability of an object.
  The identifier :samp:`{model-name}` is one of ``small``, ``medium``,
  or ``large``, representing each of the code models.

  Small model objects live in the lower 16MB of memory (so that their
  addresses can be loaded with the ``ld24`` instruction).

  Medium and large model objects may live anywhere in the 32-bit address space
  (the compiler generates ``seth/add3`` instructions to load their
  addresses).

.. _mep-variable-attributes:

MeP Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^

The MeP target has a number of addressing modes and busses.  The
``near`` space spans the standard memory space's first 16 megabytes
(24 bits).  The ``far`` space spans the entire 32-bit memory space.
The ``based`` space is a 128-byte region in the memory space that
is addressed relative to the ``$tp`` register.  The ``tiny``
space is a 65536-byte region relative to the ``$gp`` register.  In
addition to these memory regions, the MeP target has a separate 16-bit
control bus which is specified with ``cb`` attributes.

.. option:: based

  .. index:: based variable attribute, MeP

  Any variable with the ``based`` attribute is assigned to the
  ``.based`` section, and is accessed with relative to the
  ``$tp`` register.

.. option:: tiny

  .. index:: tiny variable attribute, MeP

  Likewise, the ``tiny`` attribute assigned variables to the
  ``.tiny`` section, relative to the ``$gp`` register.

.. option:: near

  .. index:: near variable attribute, MeP

  Variables with the ``near`` attribute are assumed to have addresses
  that fit in a 24-bit addressing mode.  This is the default for large
  variables ( ``-mtiny=4`` is the default) but this attribute can
  override ``-mtiny=`` for small variables, or override ``-ml``.

.. option:: far

  .. index:: far variable attribute, MeP

  Variables with the ``far`` attribute are addressed using a full
  32-bit address.  Since this covers the entire memory space, this
  allows modules to make no assumptions about where variables might be
  stored.

.. option:: io

  Variables with the ``io`` attribute are used to address
  memory-mapped peripherals.  If an address is specified, the variable
  is assigned that address, else it is not assigned an address (it is
  assumed some other module assigns an address).  Example:

  .. code-block:: c++

    int timer_count __attribute__((io(0x123)));

.. option:: cb

  .. index:: cb variable attribute, MeP

  Variables with the ``cb`` attribute are used to access the control
  bus, using special instructions.  ``addr`` indicates the control bus
  address.  Example:

  .. code-block:: c++

    int cpu_clock __attribute__((cb(0x123)));

.. _microsoft-windows-variable-attributes:

Microsoft Windows Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can use these attributes on Microsoft Windows targets.
x86 Variable Attributes for additional Windows compatibility
attributes available on all x86 targets.

.. option:: dllimport

  .. index:: dllimport variable attribute

  .. index:: dllexport variable attribute

  The ``dllimport`` and ``dllexport`` attributes are described in
  Microsoft Windows Function Attributes.

.. option:: selectany

  .. index:: selectany variable attribute

  The ``selectany`` attribute causes an initialized global variable to
  have link-once semantics.  When multiple definitions of the variable are
  encountered by the linker, the first is selected and the remainder are
  discarded.  Following usage by the Microsoft compiler, the linker is told
  *not* to warn about size or content differences of the multiple
  definitions.

  Although the primary usage of this attribute is for POD types, the
  attribute can also be applied to global C++ objects that are initialized
  by a constructor.  In this case, the static initialization and destruction
  code for the object is emitted in each translation defining the object,
  but the calls to the constructor and destructor are protected by a
  link-once guard variable.

  The ``selectany`` attribute is only available on Microsoft Windows
  targets.  You can use ``__declspec (selectany)`` as a synonym for
  ``__attribute__ ((selectany))`` for compatibility with other
  compilers.

.. option:: shared

  .. index:: shared variable attribute

  On Microsoft Windows, in addition to putting variable definitions in a named
  section, the section can also be shared among all running copies of an
  executable or DLL.  For example, this small program defines shared data
  by putting it in a named section ``shared`` and marking the section
  shareable:

  .. code-block:: c++

    int foo __attribute__((section ("shared"), shared)) = 0;

    int
    main()
    {
      /* Read and write foo.  All running
         copies see the same value.  */
      return 0;
    }

  You may only use the ``shared`` attribute along with ``section``
  attribute with a fully-initialized global definition because of the way
  linkers work.  See ``section`` attribute for more information.

  The ``shared`` attribute is only available on Microsoft Windows.

.. _msp430-variable-attributes:

MSP430 Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. option:: upper

  .. index:: upper variable attribute, MSP430

  .. index:: either variable attribute, MSP430

  These attributes are the same as the MSP430 function attributes of the
  same name (see :ref:`msp430-function-attributes`).

.. option:: lower

  .. index:: lower variable attribute, MSP430

  This option behaves mostly the same as the MSP430 function attribute of the
  same name (see :ref:`msp430-function-attributes`), but it has some additional
  functionality.

  If :option:`-mdata-region` ={ ``upper,either,none`` } has been passed, or
  the ``section`` attribute is applied to a variable, the compiler will
  generate 430X instructions to handle it.  This is because the compiler has
  to assume that the variable could get placed in the upper memory region
  (above address 0xFFFF).  Marking the variable with the ``lower`` attribute
  informs the compiler that the variable will be placed in lower memory so it
  is safe to use 430 instructions to handle it.

  In the case of the ``section`` attribute, the section name given
  will be used, and the ``.lower`` prefix will not be added.

.. _nvidia-ptx-variable-attributes:

Nvidia PTX Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These variable attributes are supported by the Nvidia PTX back end:

.. option:: shared

  .. index:: shared attribute, Nvidia PTX

  Use this attribute to place a variable in the ``.shared`` memory space.
  This memory space is private to each cooperative thread array; only threads
  within one thread block refer to the same instance of the variable.
  The runtime does not initialize variables in this memory space.

.. _powerpc-variable-attributes:

PowerPC Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Three attributes currently are defined for PowerPC configurations:
``altivec``, ``ms_struct`` and ``gcc_struct``.

.. index:: ms_struct variable attribute, PowerPC

.. index:: gcc_struct variable attribute, PowerPC

For full documentation of the struct attributes please see the
documentation in x86 Variable Attributes.

.. index:: altivec variable attribute, PowerPC

For documentation of ``altivec`` attribute please see the
documentation in PowerPC Type Attributes.

.. _rl78-variable-attributes:

RL78 Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: saddr variable attribute, RL78

The RL78 back end supports the ``saddr`` variable attribute.  This
specifies placement of the corresponding variable in the SADDR area,
which can be accessed more efficiently than the default memory region.

.. _v850-variable-attributes:

V850 Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These variable attributes are supported by the V850 back end:

.. option:: sda

  .. index:: sda variable attribute, V850

  Use this attribute to explicitly place a variable in the small data area,
  which can hold up to 64 kilobytes.

.. option:: tda

  .. index:: tda variable attribute, V850

  Use this attribute to explicitly place a variable in the tiny data area,
  which can hold up to 256 bytes in total.

.. option:: zda

  .. index:: zda variable attribute, V850

  Use this attribute to explicitly place a variable in the first 32 kilobytes
  of memory.

.. _x86-variable-attributes:

x86 Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^

Two attributes are currently defined for x86 configurations:
``ms_struct`` and ``gcc_struct``.

.. option:: ms_struct

  .. index:: ms_struct variable attribute, x86

  .. index:: gcc_struct variable attribute, x86

  If ``packed`` is used on a structure, or if bit-fields are used,
  it may be that the Microsoft ABI lays out the structure differently
  than the way GCC normally does.  Particularly when moving packed
  data between functions compiled with GCC and the native Microsoft compiler
  (either via function call or as data in a file), it may be necessary to access
  either format.

  The ``ms_struct`` and ``gcc_struct`` attributes correspond
  to the :option:`-mms-bitfields` and :option:`-mno-ms-bitfields`
  command-line options, respectively;
  see x86 Options, for details of how structure layout is affected.
  See :ref:`x86-type-attributes`, for information about the corresponding
  attributes on types.

.. _xstormy16-variable-attributes:

Xstormy16 Variable Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

One attribute is currently defined for xstormy16 configurations:
``below100``.

.. option:: below100

  .. index:: below100 variable attribute, Xstormy16

  If a variable has the ``below100`` attribute ( ``BELOW100`` is
  allowed also), GCC places the variable in the first 0x100 bytes of
  memory and use special opcodes to access it.  Such variables are
  placed in either the ``.bss_below100`` section or the
  ``.data_below100`` section.