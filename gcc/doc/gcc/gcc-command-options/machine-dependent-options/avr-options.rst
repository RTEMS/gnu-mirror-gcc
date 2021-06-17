..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _avr-options:

AVR Options
^^^^^^^^^^^

.. index:: AVR Options

These options are defined for AVR implementations:

.. option:: -mmcu=mcu

  Specify Atmel AVR instruction set architectures (ISA) or MCU type.

  The default for this option is :samp:`avr2`.

  GCC supports the following AVR devices and ISAs:

  .. This file is generated automatically using
     gcc/config/avr/gen-avr-mmcu-texi.c from:
      c	 gcc/config/avr/avr-arch.h
      c	 gcc/config/avr/avr-devices.c
      c	 gcc/config/avr/avr-mcus.def
     Please do not edit manually.

  ``avr2``
    'Classic' devices with up to 8 KiB of program memory.

    :samp:`{mcu}` = ``attiny22``, ``attiny26``, ``at90s2313``, ``at90s2323``, ``at90s2333``, ``at90s2343``, ``at90s4414``, ``at90s4433``, ``at90s4434``, ``at90c8534``, ``at90s8515``, ``at90s8535``.

  ``avr25``
    'Classic' devices with up to 8 KiB of program memory and with the ``MOVW`` instruction.

    :samp:`{mcu}` = ``attiny13``, ``attiny13a``, ``attiny24``, ``attiny24a``, ``attiny25``, ``attiny261``, ``attiny261a``, ``attiny2313``, ``attiny2313a``, ``attiny43u``, ``attiny44``, ``attiny44a``, ``attiny45``, ``attiny48``, ``attiny441``, ``attiny461``, ``attiny461a``, ``attiny4313``, ``attiny84``, ``attiny84a``, ``attiny85``, ``attiny87``, ``attiny88``, ``attiny828``, ``attiny841``, ``attiny861``, ``attiny861a``, ``ata5272``, ``ata6616c``, ``at86rf401``.

  ``avr3``
    'Classic' devices with 16 KiB up to 64 KiB of program memory.

    :samp:`{mcu}` = ``at76c711``, ``at43usb355``.

  ``avr31``
    'Classic' devices with 128 KiB of program memory.

    :samp:`{mcu}` = ``atmega103``, ``at43usb320``.

  ``avr35``
    'Classic' devices with 16 KiB up to 64 KiB of program memory and with the ``MOVW`` instruction.

    :samp:`{mcu}` = ``attiny167``, ``attiny1634``, ``atmega8u2``, ``atmega16u2``, ``atmega32u2``, ``ata5505``, ``ata6617c``, ``ata664251``, ``at90usb82``, ``at90usb162``.

  ``avr4``
    'Enhanced' devices with up to 8 KiB of program memory.

    :samp:`{mcu}` = ``atmega48``, ``atmega48a``, ``atmega48p``, ``atmega48pa``, ``atmega48pb``, ``atmega8``, ``atmega8a``, ``atmega8hva``, ``atmega88``, ``atmega88a``, ``atmega88p``, ``atmega88pa``, ``atmega88pb``, ``atmega8515``, ``atmega8535``, ``ata6285``, ``ata6286``, ``ata6289``, ``ata6612c``, ``at90pwm1``, ``at90pwm2``, ``at90pwm2b``, ``at90pwm3``, ``at90pwm3b``, ``at90pwm81``.

  ``avr5``
    'Enhanced' devices with 16 KiB up to 64 KiB of program memory.

    :samp:`{mcu}` = ``atmega16``, ``atmega16a``, ``atmega16hva``, ``atmega16hva2``, ``atmega16hvb``, ``atmega16hvbrevb``, ``atmega16m1``, ``atmega16u4``, ``atmega161``, ``atmega162``, ``atmega163``, ``atmega164a``, ``atmega164p``, ``atmega164pa``, ``atmega165``, ``atmega165a``, ``atmega165p``, ``atmega165pa``, ``atmega168``, ``atmega168a``, ``atmega168p``, ``atmega168pa``, ``atmega168pb``, ``atmega169``, ``atmega169a``, ``atmega169p``, ``atmega169pa``, ``atmega32``, ``atmega32a``, ``atmega32c1``, ``atmega32hvb``, ``atmega32hvbrevb``, ``atmega32m1``, ``atmega32u4``, ``atmega32u6``, ``atmega323``, ``atmega324a``, ``atmega324p``, ``atmega324pa``, ``atmega325``, ``atmega325a``, ``atmega325p``, ``atmega325pa``, ``atmega328``, ``atmega328p``, ``atmega328pb``, ``atmega329``, ``atmega329a``, ``atmega329p``, ``atmega329pa``, ``atmega3250``, ``atmega3250a``, ``atmega3250p``, ``atmega3250pa``, ``atmega3290``, ``atmega3290a``, ``atmega3290p``, ``atmega3290pa``, ``atmega406``, ``atmega64``, ``atmega64a``, ``atmega64c1``, ``atmega64hve``, ``atmega64hve2``, ``atmega64m1``, ``atmega64rfr2``, ``atmega640``, ``atmega644``, ``atmega644a``, ``atmega644p``, ``atmega644pa``, ``atmega644rfr2``, ``atmega645``, ``atmega645a``, ``atmega645p``, ``atmega649``, ``atmega649a``, ``atmega649p``, ``atmega6450``, ``atmega6450a``, ``atmega6450p``, ``atmega6490``, ``atmega6490a``, ``atmega6490p``, ``ata5795``, ``ata5790``, ``ata5790n``, ``ata5791``, ``ata6613c``, ``ata6614q``, ``ata5782``, ``ata5831``, ``ata8210``, ``ata8510``, ``ata5702m322``, ``at90pwm161``, ``at90pwm216``, ``at90pwm316``, ``at90can32``, ``at90can64``, ``at90scr100``, ``at90usb646``, ``at90usb647``, ``at94k``, ``m3000``.

  ``avr51``
    'Enhanced' devices with 128 KiB of program memory.

    :samp:`{mcu}` = ``atmega128``, ``atmega128a``, ``atmega128rfa1``, ``atmega128rfr2``, ``atmega1280``, ``atmega1281``, ``atmega1284``, ``atmega1284p``, ``atmega1284rfr2``, ``at90can128``, ``at90usb1286``, ``at90usb1287``.

  ``avr6``
    'Enhanced' devices with 3-byte PC, i.e. with more than 128 KiB of program memory.

    :samp:`{mcu}` = ``atmega256rfr2``, ``atmega2560``, ``atmega2561``, ``atmega2564rfr2``.

  ``avrxmega2``
    'XMEGA' devices with more than 8 KiB and up to 64 KiB of program memory.

    :samp:`{mcu}` = ``atxmega8e5``, ``atxmega16a4``, ``atxmega16a4u``, ``atxmega16c4``, ``atxmega16d4``, ``atxmega16e5``, ``atxmega32a4``, ``atxmega32a4u``, ``atxmega32c3``, ``atxmega32c4``, ``atxmega32d3``, ``atxmega32d4``, ``atxmega32e5``.

  ``avrxmega3``
    'XMEGA' devices with up to 64 KiB of combined program memory and RAM, and with program memory visible in the RAM address space.

    :samp:`{mcu}` = ``attiny202``, ``attiny204``, ``attiny212``, ``attiny214``, ``attiny402``, ``attiny404``, ``attiny406``, ``attiny412``, ``attiny414``, ``attiny416``, ``attiny417``, ``attiny804``, ``attiny806``, ``attiny807``, ``attiny814``, ``attiny816``, ``attiny817``, ``attiny1604``, ``attiny1606``, ``attiny1607``, ``attiny1614``, ``attiny1616``, ``attiny1617``, ``attiny3214``, ``attiny3216``, ``attiny3217``, ``atmega808``, ``atmega809``, ``atmega1608``, ``atmega1609``, ``atmega3208``, ``atmega3209``, ``atmega4808``, ``atmega4809``.

  ``avrxmega4``
    'XMEGA' devices with more than 64 KiB and up to 128 KiB of program memory.

    :samp:`{mcu}` = ``atxmega64a3``, ``atxmega64a3u``, ``atxmega64a4u``, ``atxmega64b1``, ``atxmega64b3``, ``atxmega64c3``, ``atxmega64d3``, ``atxmega64d4``.

  ``avrxmega5``
    'XMEGA' devices with more than 64 KiB and up to 128 KiB of program memory and more than 64 KiB of RAM.

    :samp:`{mcu}` = ``atxmega64a1``, ``atxmega64a1u``.

  ``avrxmega6``
    'XMEGA' devices with more than 128 KiB of program memory.

    :samp:`{mcu}` = ``atxmega128a3``, ``atxmega128a3u``, ``atxmega128b1``, ``atxmega128b3``, ``atxmega128c3``, ``atxmega128d3``, ``atxmega128d4``, ``atxmega192a3``, ``atxmega192a3u``, ``atxmega192c3``, ``atxmega192d3``, ``atxmega256a3``, ``atxmega256a3b``, ``atxmega256a3bu``, ``atxmega256a3u``, ``atxmega256c3``, ``atxmega256d3``, ``atxmega384c3``, ``atxmega384d3``.

  ``avrxmega7``
    'XMEGA' devices with more than 128 KiB of program memory and more than 64 KiB of RAM.

    :samp:`{mcu}` = ``atxmega128a1``, ``atxmega128a1u``, ``atxmega128a4u``.

  ``avrtiny``
    'TINY' Tiny core devices with 512 B up to 4 KiB of program memory.

    :samp:`{mcu}` = ``attiny4``, ``attiny5``, ``attiny9``, ``attiny10``, ``attiny20``, ``attiny40``.

  ``avr1``
    This ISA is implemented by the minimal AVR core and supported for assembler only.

    :samp:`{mcu}` = ``attiny11``, ``attiny12``, ``attiny15``, ``attiny28``, ``at90s1200``.

.. option:: -mabsdata

  Assume that all data in static storage can be accessed by LDS / STS
  instructions.  This option has only an effect on reduced Tiny devices like
  ATtiny40.  See also the ``absdata``
  AVR Variable Attributesvariable attribute.

.. option:: -maccumulate-args

  Accumulate outgoing function arguments and acquire/release the needed
  stack space for outgoing function arguments once in function
  prologue/epilogue.  Without this option, outgoing arguments are pushed
  before calling a function and popped afterwards.

  Popping the arguments after the function call can be expensive on
  AVR so that accumulating the stack space might lead to smaller
  executables because arguments need not be removed from the
  stack after such a function call.

  This option can lead to reduced code size for functions that perform
  several calls to functions that get their arguments on the stack like
  calls to printf-like functions.

.. option:: -mbranch-cost=cost

  Set the branch costs for conditional branch instructions to
  :samp:`{cost}`.  Reasonable values for :samp:`{cost}` are small, non-negative
  integers. The default branch cost is 0.

.. option:: -mcall-prologues

  Functions prologues/epilogues are expanded as calls to appropriate
  subroutines.  Code size is smaller.

.. option:: -mdouble=bits

  Set the size (in bits) of the ``double`` or ``long double`` type,
  respectively.  Possible values for :samp:`{bits}` are 32 and 64.
  Whether or not a specific value for :samp:`{bits}` is allowed depends on
  the ``--with-double=`` and ``--with-long-double=``
  `configure options <https://gcc.gnu.org/install/configure.html#avr>`_,
  and the same applies for the default values of the options.

.. option:: -mgas-isr-prologues

  Interrupt service routines (ISRs) may use the ``__gcc_isr`` pseudo
  instruction supported by GNU Binutils.
  If this option is on, the feature can still be disabled for individual
  ISRs by means of the AVR Function Attributes``no_gccisr``
  function attribute.  This feature is activated per default
  if optimization is on (but not with :option:`-Og`, see :ref:`optimize-options`),
  and if GNU Binutils support `PR21683 <https://sourceware.org/PR21683>`_.

.. option:: -mint8

  Assume ``int`` to be 8-bit integer.  This affects the sizes of all types: a
  ``char`` is 1 byte, an ``int`` is 1 byte, a ``long`` is 2 bytes,
  and ``long long`` is 4 bytes.  Please note that this option does not
  conform to the C standards, but it results in smaller code
  size.

.. option:: -mmain-is-OS_task

  Do not save registers in ``main``.  The effect is the same like
  attaching attribute AVR Function Attributes``OS_task``
  to ``main``. It is activated per default if optimization is on.

.. option:: -mn-flash=num

  Assume that the flash memory has a size of
  :samp:`{num}` times 64 KiB.

.. option:: -mno-interrupts

  Generated code is not compatible with hardware interrupts.
  Code size is smaller.

.. option:: -mrelax

  Try to replace ``CALL`` resp. ``JMP`` instruction by the shorter
  ``RCALL`` resp. ``RJMP`` instruction if applicable.
  Setting :option:`-mrelax` just adds the :option:`--mlink-relax` option to
  the assembler's command line and the :option:`--relax` option to the
  linker's command line.

  Jump relaxing is performed by the linker because jump offsets are not
  known before code is located. Therefore, the assembler code generated by the
  compiler is the same, but the instructions in the executable may
  differ from instructions in the assembler code.

  Relaxing must be turned on if linker stubs are needed, see the
  section on ``EIND`` and linker stubs below.

.. option:: -mrmw

  Assume that the device supports the Read-Modify-Write
  instructions ``XCH``, ``LAC``, ``LAS`` and ``LAT``.

.. option:: -mshort-calls

  Assume that ``RJMP`` and ``RCALL`` can target the whole
  program memory.

  This option is used internally for multilib selection.  It is
  not an optimization option, and you don't need to set it by hand.

.. option:: -msp8

  Treat the stack pointer register as an 8-bit register,
  i.e. assume the high byte of the stack pointer is zero.
  In general, you don't need to set this option by hand.

  This option is used internally by the compiler to select and
  build multilibs for architectures ``avr2`` and ``avr25``.
  These architectures mix devices with and without ``SPH``.
  For any setting other than :option:`-mmcu`:samp:`=avr2` or :option:`-mmcu`:samp:`=avr25`
  the compiler driver adds or removes this option from the compiler
  proper's command line, because the compiler then knows if the device
  or architecture has an 8-bit stack pointer and thus no ``SPH``
  register or not.

.. option:: -mstrict-X

  Use address register ``X`` in a way proposed by the hardware.  This means
  that ``X`` is only used in indirect, post-increment or
  pre-decrement addressing.

  Without this option, the ``X`` register may be used in the same way
  as ``Y`` or ``Z`` which then is emulated by additional
  instructions.
  For example, loading a value with ``X+const`` addressing with a
  small non-negative ``const < 64`` to a register :samp:`{Rn}` is
  performed as

  .. code-block:: c++

    adiw r26, const   ; X += const
    ld   Rn, X        ; Rn = *X
    sbiw r26, const   ; X -= const

.. option:: -mtiny-stack

  Only change the lower 8 bits of the stack pointer.

.. option:: -mfract-convert-truncate

  Allow to use truncation instead of rounding towards zero for fractional fixed-point types.

.. option:: -nodevicelib

  Don't link against AVR-LibC's device specific library ``lib<mcu>.a``.

.. option:: -nodevicespecs

  Don't add :option:`-specs`:samp:`=device-specs/specs-{mcu}` to the compiler driver's
  command line.  The user takes responsibility for supplying the sub-processes
  like compiler proper, assembler and linker with appropriate command line
  options.  This means that the user has to supply her private device specs
  file by means of :option:`-specs`:samp:`={path-to-specs-file}`.  There is no
  more need for option :option:`-mmcu`:samp:`={mcu}`.

  This option can also serve as a replacement for the older way of
  specifying custom device-specs files that needed :option:`-B` :samp:`{some-path}` to point to a directory
  which contains a folder named ``device-specs`` which contains a specs file named
  ``specs-mcu``, where :samp:`{mcu}` was specified by :option:`-mmcu`:samp:`={mcu}`.

.. option:: -Waddr-space-convert

  Warn about conversions between address spaces in the case where the
  resulting address space is not contained in the incoming address space.

.. option:: -Wno-addr-space-convert

  Default option value for :option:`-Waddr-space-convert`.

.. option:: -Wmisspelled-isr

  Warn if the ISR is misspelled, i.e. without __vector prefix.
  Enabled by default.

.. option:: -Wno-misspelled-isr

  Default option value for :option:`-Wmisspelled-isr`.

EIND and Devices with More Than 128 Ki Bytes of Flash
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: EIND

Pointers in the implementation are 16 bits wide.
The address of a function or label is represented as word address so
that indirect jumps and calls can target any code address in the
range of 64 Ki words.

In order to facilitate indirect jump on devices with more than 128 Ki
bytes of program memory space, there is a special function register called
``EIND`` that serves as most significant part of the target address
when ``EICALL`` or ``EIJMP`` instructions are used.

Indirect jumps and calls on these devices are handled as follows by
the compiler and are subject to some limitations:

* The compiler never sets ``EIND``.

* The compiler uses ``EIND`` implicitly in ``EICALL`` / ``EIJMP``
  instructions or might read ``EIND`` directly in order to emulate an
  indirect call/jump by means of a ``RET`` instruction.

* The compiler assumes that ``EIND`` never changes during the startup
  code or during the application. In particular, ``EIND`` is not
  saved/restored in function or interrupt service routine
  prologue/epilogue.

* For indirect calls to functions and computed goto, the linker
  generates *stubs*. Stubs are jump pads sometimes also called
  *trampolines*. Thus, the indirect call/jump jumps to such a stub.
  The stub contains a direct jump to the desired address.

* Linker relaxation must be turned on so that the linker generates
  the stubs correctly in all situations. See the compiler option
  :option:`-mrelax` and the linker option :option:`--relax`.
  There are corner cases where the linker is supposed to generate stubs
  but aborts without relaxation and without a helpful error message.

* The default linker script is arranged for code with ``EIND = 0``.
  If code is supposed to work for a setup with ``EIND != 0``, a custom
  linker script has to be used in order to place the sections whose
  name start with ``.trampolines`` into the segment where ``EIND``
  points to.

* The startup code from libgcc never sets ``EIND``.
  Notice that startup code is a blend of code from libgcc and AVR-LibC.
  For the impact of AVR-LibC on ``EIND``, see the
  `AVR-LibC user manual <http://nongnu.org/avr-libc/user-manual/>`_.

* It is legitimate for user-specific startup code to set up ``EIND``
  early, for example by means of initialization code located in
  section ``.init3``. Such code runs prior to general startup code
  that initializes RAM and calls constructors, but after the bit
  of startup code from AVR-LibC that sets ``EIND`` to the segment
  where the vector table is located.

  .. code-block::

    #include <avr/io.h>

    static void
    __attribute__((section(".init3"),naked,used,no_instrument_function))
    init3_set_eind (void)
    {
      __asm volatile ("ldi r24,pm_hh8(__trampolines_start)\n\t"
                      "out %i0,r24" :: "n" (&EIND) : "r24","memory");
    }

  The ``__trampolines_start`` symbol is defined in the linker script.

* Stubs are generated automatically by the linker if
  the following two conditions are met:

  * The address of a label is taken by means of the ``gs`` modifier
    (short for *generate stubs*) like so:

    .. code-block:: c++

      LDI r24, lo8(gs(func))
      LDI r25, hi8(gs(func))

  * The final location of that label is in a code segment
    *outside* the segment where the stubs are located.

* The compiler emits such ``gs`` modifiers for code labels in the
  following situations:

  * Taking address of a function or code label.

  * Computed goto.

  * If prologue-save function is used, see :option:`-mcall-prologues`
    command-line option.

  * Switch/case dispatch tables. If you do not want such dispatch
    tables you can specify the :option:`-fno-jump-tables` command-line option.

  * C and C++ constructors/destructors called during startup/shutdown.

  * If the tools hit a ``gs()`` modifier explained above.

* Jumping to non-symbolic addresses like so is *not* supported:

  .. code-block:: c++

    int main (void)
    {
        /* Call function at word address 0x2 */
        return ((int(*)(void)) 0x2)();
    }

  Instead, a stub has to be set up, i.e. the function has to be called
  through a symbol ( ``func_4`` in the example):

  .. code-block:: c++

    int main (void)
    {
        extern int func_4 (void);

        /* Call function at byte address 0x4 */
        return func_4();
    }

  and the application be linked with :option:`-Wl,--defsym,func_4`:samp:`=0x4`.
  Alternatively, ``func_4`` can be defined in the linker script.

Handling of the RAMPD, RAMPX, RAMPY and RAMPZ Special Function Registers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: RAMPD

.. index:: RAMPX

.. index:: RAMPY

.. index:: RAMPZ

Some AVR devices support memories larger than the 64 KiB range
that can be accessed with 16-bit pointers.  To access memory locations
outside this 64 KiB range, the content of a ``RAMP``
register is used as high part of the address:
The ``X``, ``Y``, ``Z`` address register is concatenated
with the ``RAMPX``, ``RAMPY``, ``RAMPZ`` special function
register, respectively, to get a wide address. Similarly,
``RAMPD`` is used together with direct addressing.

* The startup code initializes the ``RAMP`` special function
  registers with zero.

* If a AVR Named Address Spacesnamed address space other than
  generic or ``__flash`` is used, then ``RAMPZ`` is set
  as needed before the operation.

* If the device supports RAM larger than 64 KiB and the compiler
  needs to change ``RAMPZ`` to accomplish an operation, ``RAMPZ``
  is reset to zero after the operation.

* If the device comes with a specific ``RAMP`` register, the ISR
  prologue/epilogue saves/restores that SFR and initializes it with
  zero in case the ISR code might (implicitly) use it.

* RAM larger than 64 KiB is not supported by GCC for AVR targets.
  If you use inline assembler to read from locations outside the
  16-bit address range and change one of the ``RAMP`` registers,
  you must reset it to zero after the access.

AVR Built-in Macros
~~~~~~~~~~~~~~~~~~~

GCC defines several built-in macros so that the user code can test
for the presence or absence of features.  Almost any of the following
built-in macros are deduced from device capabilities and thus
triggered by the :option:`-mmcu` = command-line option.

For even more AVR-specific built-in macros see
AVR Named Address Spaces and AVR Built-in Functions.

``__AVR_ARCH__``
  Build-in macro that resolves to a decimal number that identifies the
  architecture and depends on the :option:`-mmcu`:samp:`={mcu}` option.
  Possible values are:

  ``2``, ``25``, ``3``, ``31``, ``35``,
  ``4``, ``5``, ``51``, ``6``

  for :samp:`{mcu}` = ``avr2``, ``avr25``, ``avr3``, ``avr31``,
  ``avr35``, ``avr4``, ``avr5``, ``avr51``, ``avr6``,

  respectively and

  ``100``,
  ``102``, ``103``, ``104``,
  ``105``, ``106``, ``107``

  for :samp:`{mcu}` = ``avrtiny``,
  ``avrxmega2``, ``avrxmega3``, ``avrxmega4``,
  ``avrxmega5``, ``avrxmega6``, ``avrxmega7``, respectively.
  If :samp:`{mcu}` specifies a device, this built-in macro is set
  accordingly. For example, with :option:`-mmcu`:samp:`=atmega8` the macro is
  defined to ``4``.

:samp:`__AVR_{Device}__`
  Setting :option:`-mmcu`:samp:`={device}` defines this built-in macro which reflects
  the device's name. For example, :option:`-mmcu`:samp:`=atmega8` defines the
  built-in macro ``__AVR_ATmega8__``, :option:`-mmcu`:samp:`=attiny261a` defines
  ``__AVR_ATtiny261A__``, etc.

  The built-in macros' names follow
  the scheme ``__AVR_Device__`` where :samp:`{Device}` is
  the device name as from the AVR user manual. The difference between
  :samp:`{Device}` in the built-in macro and :samp:`{device}` in
  :option:`-mmcu`:samp:`={device}` is that the latter is always lowercase.

  If :samp:`{device}` is not a device but only a core architecture like
  :samp:`avr51`, this macro is not defined.

``__AVR_DEVICE_NAME__``
  Setting :option:`-mmcu`:samp:`={device}` defines this built-in macro to
  the device's name. For example, with :option:`-mmcu`:samp:`=atmega8` the macro
  is defined to ``atmega8``.

  If :samp:`{device}` is not a device but only a core architecture like
  :samp:`avr51`, this macro is not defined.

``__AVR_XMEGA__``
  The device / architecture belongs to the XMEGA family of devices.

``__AVR_HAVE_ELPM__``
  The device has the ``ELPM`` instruction.

``__AVR_HAVE_ELPMX__``
  The device has the ``ELPM Rn,Z`` and ``ELPM
  Rn,Z+`` instructions.

``__AVR_HAVE_MOVW__``
  The device has the ``MOVW`` instruction to perform 16-bit
  register-register moves.

``__AVR_HAVE_LPMX__``
  The device has the ``LPM Rn,Z`` and
  ``LPM Rn,Z+`` instructions.

``__AVR_HAVE_MUL__``
  The device has a hardware multiplier.

``__AVR_HAVE_JMP_CALL__``
  The device has the ``JMP`` and ``CALL`` instructions.
  This is the case for devices with more than 8 KiB of program
  memory.

``__AVR_HAVE_EIJMP_EICALL__`` ``__AVR_3_BYTE_PC__``
  The device has the ``EIJMP`` and ``EICALL`` instructions.
  This is the case for devices with more than 128 KiB of program memory.
  This also means that the program counter
  (PC) is 3 bytes wide.

``__AVR_2_BYTE_PC__``
  The program counter (PC) is 2 bytes wide. This is the case for devices
  with up to 128 KiB of program memory.

``__AVR_HAVE_8BIT_SP__`` ``__AVR_HAVE_16BIT_SP__``
  The stack pointer (SP) register is treated as 8-bit respectively
  16-bit register by the compiler.
  The definition of these macros is affected by :option:`-mtiny-stack`.

``__AVR_HAVE_SPH__`` ``__AVR_SP8__``
  The device has the SPH (high part of stack pointer) special function
  register or has an 8-bit stack pointer, respectively.
  The definition of these macros is affected by :option:`-mmcu` = and
  in the cases of :option:`-mmcu`:samp:`=avr2` and :option:`-mmcu`:samp:`=avr25` also
  by :option:`-msp8`.

``__AVR_HAVE_RAMPD__`` ``__AVR_HAVE_RAMPX__`` ``__AVR_HAVE_RAMPY__`` ``__AVR_HAVE_RAMPZ__``
  The device has the ``RAMPD``, ``RAMPX``, ``RAMPY``,
  ``RAMPZ`` special function register, respectively.

``__NO_INTERRUPTS__``
  This macro reflects the :option:`-mno-interrupts` command-line option.

``__AVR_ERRATA_SKIP__`` ``__AVR_ERRATA_SKIP_JMP_CALL__``
  Some AVR devices (AT90S8515, ATmega103) must not skip 32-bit
  instructions because of a hardware erratum.  Skip instructions are
  ``SBRS``, ``SBRC``, ``SBIS``, ``SBIC`` and ``CPSE``.
  The second macro is only defined if ``__AVR_HAVE_JMP_CALL__`` is also
  set.

``__AVR_ISA_RMW__``
  The device has Read-Modify-Write instructions (XCH, LAC, LAS and LAT).

:samp:`__AVR_SFR_OFFSET__={offset}`
  Instructions that can address I/O special function registers directly
  like ``IN``, ``OUT``, ``SBI``, etc. may use a different
  address as if addressed by an instruction to access RAM like ``LD``
  or ``STS``. This offset depends on the device architecture and has
  to be subtracted from the RAM address in order to get the
  respective I/O address.

``__AVR_SHORT_CALLS__``
  The :option:`-mshort-calls` command line option is set.

:samp:`__AVR_PM_BASE_ADDRESS__={addr}`
  Some devices support reading from flash memory by means of ``LD*``
  instructions.  The flash memory is seen in the data address space
  at an offset of ``__AVR_PM_BASE_ADDRESS__``.  If this macro
  is not defined, this feature is not available.  If defined,
  the address space is linear and there is no need to put
  ``.rodata`` into RAM.  This is handled by the default linker
  description file, and is currently available for
  ``avrtiny`` and ``avrxmega3``.  Even more convenient,
  there is no need to use address spaces like ``__flash`` or
  features like attribute ``progmem`` and ``pgm_read_*``.

``__WITH_AVRLIBC__``
  The compiler is configured to be used together with AVR-Libc.
  See the :option:`--with-avrlibc` configure option.

``__HAVE_DOUBLE_MULTILIB__``
  Defined if :option:`-mdouble` = acts as a multilib option.

``__HAVE_DOUBLE32__`` ``__HAVE_DOUBLE64__``
  Defined if the compiler supports 32-bit double resp. 64-bit double.
  The actual layout is specified by option :option:`-mdouble` =.

``__DEFAULT_DOUBLE__``
  The size in bits of ``double`` if :option:`-mdouble` = is not set.
  To test the layout of ``double`` in a program, use the built-in
  macro ``__SIZEOF_DOUBLE__``.

``__HAVE_LONG_DOUBLE32__`` ``__HAVE_LONG_DOUBLE64__`` ``__HAVE_LONG_DOUBLE_MULTILIB__`` ``__DEFAULT_LONG_DOUBLE__``
  Same as above, but for ``long double`` instead of ``double``.

``__WITH_DOUBLE_COMPARISON__``
  Reflects the ``--with-double-comparison={tristate|bool|libf7}``
  `configure option <https://gcc.gnu.org/install/configure.html#avr>`_
  and is defined to ``2`` or ``3``.

``__WITH_LIBF7_LIBGCC__`` ``__WITH_LIBF7_MATH__`` ``__WITH_LIBF7_MATH_SYMBOLS__``
  Reflects the ``--with-libf7={libgcc|math|math-symbols}``
  `configure option <https://gcc.gnu.org/install/configure.html#avr>`_.