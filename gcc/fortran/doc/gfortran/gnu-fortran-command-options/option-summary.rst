..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _option-summary:

Option summary
**************

Options
^^^^^^^

Here is a summary of all the options specific to GNU Fortran, grouped
by type.  Explanations are in the following sections.

*Fortran Language Options*
  See :ref:`fortran-dialect-options`.

  :option:`-fall-intrinsics` :option:`-fallow-argument-mismatch` :option:`-fallow-invalid-boz` |gol|
  :option:`-fbackslash` :option:`-fcray-pointer` :option:`-fd-lines-as-code` :option:`-fd-lines-as-comments` |gol|
  :option:`-fdec` :option:`-fdec-char-conversions` :option:`-fdec-structure` :option:`-fdec-intrinsic-ints` |gol|
  :option:`-fdec-static` :option:`-fdec-math` :option:`-fdec-include` :option:`-fdec-format-defaults` |gol|
  :option:`-fdec-blank-format-item` :option:`-fdefault-double-8` :option:`-fdefault-integer-8` |gol|
  :option:`-fdefault-real-8` :option:`-fdefault-real-10` :option:`-fdefault-real-16` :option:`-fdollar-ok` |gol|
  :option:`-ffixed-line-length-n` :option:`-ffixed-line-length-none` :option:`-fpad-source` |gol|
  :option:`-ffree-form` :option:`-ffree-line-length-n` :option:`-ffree-line-length-none` |gol|
  :option:`-fimplicit-none` :option:`-finteger-4-integer-8` :option:`-fmax-identifier-length` |gol|
  :option:`-fmodule-private` :option:`-ffixed-form` :option:`-fno-range-check` :option:`-fopenacc` :option:`-fopenmp` |gol|
  :option:`-freal-4-real-10` :option:`-freal-4-real-16` :option:`-freal-4-real-8` :option:`-freal-8-real-10` |gol|
  :option:`-freal-8-real-16` :option:`-freal-8-real-4` :option:`-std`:samp:`={std}` :option:`-ftest-forall-temp`

*Preprocessing Options*
  See :ref:`preprocessing-options`.

  :option:`-A-`:samp:`{question}`:samp:`[={answer}]` |gol|
  :option:`-A`:samp:`{question}`:samp:`={answer}` :option:`-C` :option:`-CC` :option:`-D`:samp:`{macro}`:samp:`[={defn}]` |gol|
  :option:`-H` :option:`-P` |gol|
  :option:`-U`:samp:`{macro}` :option:`-cpp` :option:`-dD` :option:`-dI` :option:`-dM` :option:`-dN` :option:`-dU` :option:`-fworking-directory` |gol|
  :option:`-imultilib` :samp:`{dir}` |gol|
  :option:`-iprefix` :samp:`{file}` :option:`-iquote` :option:`-isysroot` :samp:`{dir}` :option:`-isystem` :samp:`{dir}` :option:`-nocpp` |gol|
  :option:`-nostdinc` |gol|
  :option:`-undef`

*Error and Warning Options*
  See :ref:`error-and-warning-options`.

  :option:`-Waliasing` :option:`-Wall` :option:`-Wampersand` :option:`-Warray-bounds` |gol|
  :option:`-Wc-binding-type` :option:`-Wcharacter-truncation` :option:`-Wconversion` |gol|
  :option:`-Wdo-subscript` :option:`-Wfunction-elimination` :option:`-Wimplicit-interface` |gol|
  :option:`-Wimplicit-procedure` :option:`-Wintrinsic-shadow` :option:`-Wuse-without-only` |gol|
  :option:`-Wintrinsics-std` :option:`-Wline-truncation` :option:`-Wno-align-commons` |gol|
  :option:`-Wno-overwrite-recursive` :option:`-Wno-tabs` :option:`-Wreal-q-constant` :option:`-Wsurprising` |gol|
  :option:`-Wunderflow` :option:`-Wunused-parameter` :option:`-Wrealloc-lhs` :option:`-Wrealloc-lhs-all` |gol|
  :option:`-Wfrontend-loop-interchange` :option:`-Wtarget-lifetime` :option:`-fmax-errors`:samp:`={n}` |gol|
  :option:`-fsyntax-only` :option:`-pedantic` |gol|
  :option:`-pedantic-errors`

*Debugging Options*
  See :ref:`debugging-options`.

  :option:`-fbacktrace` :option:`-fdump-fortran-optimized` :option:`-fdump-fortran-original` |gol|
  :option:`-fdebug-aux-vars` :option:`-fdump-fortran-global` :option:`-fdump-parse-tree` :option:`-ffpe-trap`:samp:`={list}` |gol|
  :option:`-ffpe-summary`:samp:`={list}`

*Directory Options*
  See :ref:`directory-options`.

  :option:`-I`:samp:`{dir}`  :option:`-J`:samp:`{dir}` :option:`-fintrinsic-modules-path` :samp:`{dir}`

*Link Options*
  See :ref:`link-options`.

  :option:`-static-libgfortran`

*Runtime Options*
  See :ref:`runtime-options`.

  :option:`-fconvert`:samp:`={conversion}` :option:`-fmax-subrecord-length`:samp:`={length}` |gol|
  :option:`-frecord-marker`:samp:`={length}` :option:`-fsign-zero`

*Interoperability Options*
  See :ref:`interoperability-options`.

  :option:`-fc-prototypes` :option:`-fc-prototypes-external`

*Code Generation Options*
  See :ref:`code-gen-options`.

  :option:`-faggressive-function-elimination` :option:`-fblas-matmul-limit`:samp:`={n}` |gol|
  :option:`-fbounds-check` :option:`-ftail-call-workaround` :option:`-ftail-call-workaround`:samp:`={n}` |gol|
  :option:`-fcheck-array-temporaries` |gol|
  :option:`-fcheck`:samp:`={<all|array-temps|bits|bounds|do|mem|pointer|recursion>}` |gol|
  :option:`-fcoarray`:samp:`={<none|single|lib>}` :option:`-fexternal-blas` :option:`-ff2c` |gol|
  :option:`-ffrontend-loop-interchange` :option:`-ffrontend-optimize` |gol|
  :option:`-finit-character`:samp:`={n}` :option:`-finit-integer`:samp:`={n}` :option:`-finit-local-zero` |gol|
  :option:`-finit-derived` :option:`-finit-logical`:samp:`={<true|false>}` |gol|
  :option:`-finit-real`:samp:`={<zero|inf|-inf|nan|snan>}` |gol|
  :option:`-finline-matmul-limit`:samp:`={n}` |gol|
  :option:`-finline-arg-packing` :option:`-fmax-array-constructor`:samp:`={n}` |gol|
  :option:`-fmax-stack-var-size`:samp:`={n}` :option:`-fno-align-commons` :option:`-fno-automatic` |gol|
  :option:`-fno-protect-parens` :option:`-fno-underscoring` :option:`-fsecond-underscore` |gol|
  :option:`-fpack-derived` :option:`-frealloc-lhs` :option:`-frecursive` :option:`-frepack-arrays` |gol|
  :option:`-fshort-enums` :option:`-fstack-arrays`