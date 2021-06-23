..
  Copyright 1988-2021 Free Software Foundation, Inc.
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

  :option:`-fall-intrinsics` :option:`-fallow-argument-mismatch` :option:`-fallow-invalid-boz`
  :option:`-fbackslash` :option:`-fcray-pointer` :option:`-fd-lines-as-code` :option:`-fd-lines-as-comments`
  :option:`-fdec` :option:`-fdec-char-conversions` :option:`-fdec-structure` :option:`-fdec-intrinsic-ints`
  :option:`-fdec-static` :option:`-fdec-math` :option:`-fdec-include` :option:`-fdec-format-defaults`
  :option:`-fdec-blank-format-item` :option:`-fdefault-double-8` :option:`-fdefault-integer-8`
  :option:`-fdefault-real-8` :option:`-fdefault-real-10` :option:`-fdefault-real-16` :option:`-fdollar-ok`
  :option:`-ffixed-line-length-`:samp:`{n}` :option:`-ffixed-line-length-none` :option:`-fpad-source`
  :option:`-ffree-form` :option:`-ffree-line-length-`:samp:`{n}` :option:`-ffree-line-length-none`
  :option:`-fimplicit-none` :option:`-finteger-4-integer-8` :option:`-fmax-identifier-length`
  :option:`-fmodule-private` :option:`-ffixed-form` :option:`-fno-range-check` :option:`-fopenacc` :option:`-fopenmp`
  :option:`-freal-4-real-10` :option:`-freal-4-real-16` :option:`-freal-4-real-8` :option:`-freal-8-real-10`
  :option:`-freal-8-real-16` :option:`-freal-8-real-4` :option:`-std`:samp:`={std}` :option:`-ftest-forall-temp`

*Preprocessing Options*
  See :ref:`Enable and customize preprocessing <preprocessing-options>`.

  :option:`-A-`:samp:`{question}`:samp:`[={answer}]`
  :option:`-A`:samp:`{question}`:samp:`={answer}` :option:`-C` :option:`-CC` :option:`-D`:samp:`{macro}`:samp:`[={defn}]`
  :option:`-H` :option:`-P`
  :option:`-U`:samp:`{macro}` :option:`-cpp` :option:`-dD` :option:`-dI` :option:`-dM` :option:`-dN` :option:`-dU` :option:`-fworking-directory`
  :option:`-imultilib` :samp:`{dir}`
  :option:`-iprefix` :samp:`{file}` :option:`-iquote` :option:`-isysroot` :samp:`{dir}` :option:`-isystem` :samp:`{dir}` :option:`-nocpp`
  :option:`-nostdinc`
  :option:`-undef`

*Error and Warning Options*
  See :ref:`Options to request or suppress errors
  and warnings <error-and-warning-options>`.

  :option:`-Waliasing` :option:`-Wall` :option:`-Wampersand` :option:`-Warray-bounds`
  :option:`-Wc-binding-type` :option:`-Wcharacter-truncation` :option:`-Wconversion`
  :option:`-Wdo-subscript` :option:`-Wfunction-elimination` :option:`-Wimplicit-interface`
  :option:`-Wimplicit-procedure` :option:`-Wintrinsic-shadow` :option:`-Wuse-without-only`
  :option:`-Wintrinsics-std` :option:`-Wline-truncation` :option:`-Wno-align-commons`
  :option:`-Wno-overwrite-recursive` :option:`-Wno-tabs` :option:`-Wreal-q-constant` :option:`-Wsurprising`
  :option:`-Wunderflow` :option:`-Wunused-parameter` :option:`-Wrealloc-lhs` :option:`-Wrealloc-lhs-all`
  :option:`-Wfrontend-loop-interchange` :option:`-Wtarget-lifetime` :option:`-fmax-errors`:samp:`={n}`
  :option:`-fsyntax-only` :option:`-pedantic`
  :option:`-pedantic-errors`

*Debugging Options*
  See :ref:`Options for debugging your program or GNU Fortran <debugging-options>`.

  :option:`-fbacktrace` :option:`-fdump-fortran-optimized` :option:`-fdump-fortran-original`
  :option:`-fdebug-aux-vars` :option:`-fdump-fortran-global` :option:`-fdump-parse-tree` :option:`-ffpe-trap`:samp:`={list}`
  :option:`-ffpe-summary`:samp:`={list}`

*Directory Options*
  See :ref:`Options for directory search <directory-options>`.

  :option:`-I`:samp:`{dir}`  :option:`-J`:samp:`{dir}` :option:`-fintrinsic-modules-path` :samp:`{dir}`

*Link Options*
  See :ref:`Options for influencing the linking step <link-options>`.

  :option:`-static-libgfortran`

*Runtime Options*
  See :ref:`Options for influencing runtime behavior <runtime-options>`.

  :option:`-fconvert`:samp:`={conversion}` :option:`-fmax-subrecord-length`:samp:`={length}`
  :option:`-frecord-marker`:samp:`={length}` :option:`-fsign-zero`

*Interoperability Options*
  See :ref:`Options for interoperability <interoperability-options>`.

  :option:`-fc-prototypes` :option:`-fc-prototypes-external`

*Code Generation Options*
  See :ref:`Options for code generation conventions <code-gen-options>`.

  :option:`-faggressive-function-elimination` :option:`-fblas-matmul-limit`:samp:`={n}`
  :option:`-fbounds-check` :option:`-ftail-call-workaround` :option:`-ftail-call-workaround`:samp:`={n}`
  :option:`-fcheck-array-temporaries`
  :option:`-fcheck`:samp:`={<all|array-temps|bits|bounds|do|mem|pointer|recursion>}`
  :option:`-fcoarray`:samp:`={<none|single|lib>}` :option:`-fexternal-blas` :option:`-ff2c`
  :option:`-ffrontend-loop-interchange` :option:`-ffrontend-optimize`
  :option:`-finit-character`:samp:`={n}` :option:`-finit-integer`:samp:`={n}` :option:`-finit-local-zero`
  :option:`-finit-derived` :option:`-finit-logical`:samp:`={<true|false>}`
  :option:`-finit-real`:samp:`={<zero|inf|-inf|nan|snan>}`
  :option:`-finline-matmul-limit`:samp:`={n}`
  :option:`-finline-arg-packing` :option:`-fmax-array-constructor`:samp:`={n}`
  :option:`-fmax-stack-var-size`:samp:`={n}` :option:`-fno-align-commons` :option:`-fno-automatic`
  :option:`-fno-protect-parens` :option:`-fno-underscoring` :option:`-fsecond-underscore`
  :option:`-fpack-derived` :option:`-frealloc-lhs` :option:`-frecursive` :option:`-frepack-arrays`
  :option:`-fshort-enums` :option:`-fstack-arrays`