..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _add-options:

Features for dg-add-options
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The supported values of :samp:`{feature}` for directive ``dg-add-options``
are:

``arm_fp``
  ``__ARM_FP`` definition.  Only ARM targets support this feature, and only then
  in certain modes; see the arm_fp_okarm_fp_ok effective target
  keyword.

``arm_fp_dp``
  ``__ARM_FP`` definition with double-precision support.  Only ARM
  targets support this feature, and only then in certain modes; see the
  arm_fp_dp_okarm_fp_dp_ok effective target keyword.

``arm_neon``
  NEON support.  Only ARM targets support this feature, and only then
  in certain modes; see the arm_neon_okarm_neon_ok effective target
  keyword.

``arm_fp16``
  VFP half-precision floating point support.  This does not select the
  FP16 format; for that, use arm_fp16_ieeearm_fp16_ieee or
  arm_fp16_alternativearm_fp16_alternative instead.  This
  feature is only supported by ARM targets and then only in certain
  modes; see the arm_fp16_okarm_fp16_ok effective target
  keyword.

``arm_fp16_ieee``
  ARM IEEE 754-2008 format VFP half-precision floating point support.
  This feature is only supported by ARM targets and then only in certain
  modes; see the arm_fp16_okarm_fp16_ok effective target
  keyword.

``arm_fp16_alternative``
  ARM Alternative format VFP half-precision floating point support.
  This feature is only supported by ARM targets and then only in certain
  modes; see the arm_fp16_okarm_fp16_ok effective target
  keyword.

``arm_neon_fp16``
  NEON and half-precision floating point support.  Only ARM targets
  support this feature, and only then in certain modes; see
  the arm_neon_fp16_okarm_neon_fp16_ok effective target keyword.

``arm_vfp3``
  arm vfp3 floating point support; see
  the arm_vfp3_okarm_vfp3_ok effective target keyword.

``arm_arch_v8a_hard``
  Add options for ARMv8-A and the hard-float variant of the AAPCS,
  if this is supported by the compiler; see the
  arm_arch_v8a_hard_okarm_arch_v8a_hard_ok effective target keyword.

``arm_v8_1a_neon``
  Add options for ARMv8.1-A with Adv.SIMD support, if this is supported
  by the target; see the arm_v8_1a_neon_okarm_v8_1a_neon_ok
  effective target keyword.

``arm_v8_2a_fp16_scalar``
  Add options for ARMv8.2-A with scalar FP16 support, if this is
  supported by the target; see the
  arm_v8_2a_fp16_scalar_okarm_v8_2a_fp16_scalar_ok effective
  target keyword.

``arm_v8_2a_fp16_neon``
  Add options for ARMv8.2-A with Adv.SIMD FP16 support, if this is
  supported by the target; see the
  arm_v8_2a_fp16_neon_okarm_v8_2a_fp16_neon_ok effective target
  keyword.

``arm_v8_2a_dotprod_neon``
  Add options for ARMv8.2-A with Adv.SIMD Dot Product support, if this is
  supported by the target; see the
  arm_v8_2a_dotprod_neon_ok effective target keyword.

``arm_fp16fml_neon``
  Add options to enable generation of the ``VFMAL`` and ``VFMSL``
  instructions, if this is supported by the target; see the
  arm_fp16fml_neon_ok effective target keyword.

``arm_dsp``
  Add options for ARM DSP intrinsics support, if this is supported by
  the target; see the arm_dsp_okarm_dsp_ok effective target
  keyword.

``bind_pic_locally``
  Add the target-specific flags needed to enable functions to bind
  locally when using pic/PIC passes in the testsuite.

:samp:`float{n}`
  Add the target-specific flags needed to use the ``_Floatn`` type.

:samp:`float{n}x`
  Add the target-specific flags needed to use the ``_Floatnx`` type.

``ieee``
  Add the target-specific flags needed to enable full IEEE
  compliance mode.

``mips16_attribute``
  ``mips16`` function attributes.
  Only MIPS targets support this feature, and only then in certain modes.

``stack_size``
  Add the flags needed to define macro STACK_SIZE and set it to the stack size
  limit associated with the stack_size_et``stack_size`` effective
  target.

``sqrt_insn``
  Add the target-specific flags needed to enable hardware square root
  instructions, if any.

``tls``
  Add the target-specific flags needed to use thread-local storage.