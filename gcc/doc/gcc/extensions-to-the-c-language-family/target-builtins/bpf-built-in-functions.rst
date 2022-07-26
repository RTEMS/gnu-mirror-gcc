..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _bpf-built-in-functions:

BPF Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

The following built-in functions are available for eBPF targets.

.. function:: unsigned long long __builtin_bpf_load_byte (unsigned long long offset)

  Load a byte from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. function:: unsigned long long __builtin_bpf_load_half (unsigned long long offset)

  Load 16-bits from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. function:: unsigned long long __builtin_bpf_load_word (unsigned long long offset)

  Load 32-bits from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. function:: void * __builtin_preserve_access_index (expr)

  BPF Compile Once-Run Everywhere (CO-RE) support. Instruct GCC to generate CO-RE relocation records for any accesses to aggregate data structures (struct, union, array types) in :samp:`{expr}`. This builtin is otherwise transparent, the return value is whatever :samp:`{expr}` evaluates to. It is also overloaded: :samp:`{expr}` may be of any type (not necessarily a pointer), the return type is the same. Has no effect if ``-mco-re`` is not in effect (either specified or implied).