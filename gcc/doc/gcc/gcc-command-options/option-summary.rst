..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _option-summary:

Option Summary
**************

Here is a summary of all the options, grouped by type.  Explanations are
in the following sections.

*Overall Options*
  See :ref:`Options Controlling the Kind of Output <overall-options>`.

  :option:`-c`  :option:`-S`  :option:`-E`  :option:`-o` :samp:`{file}`
  :option:`-dumpbase` :samp:`{dumpbase}`  :option:`-dumpbase-ext` :samp:`{auxdropsuf}`
  :option:`-dumpdir` :samp:`{dumppfx}`  :option:`-x` :samp:`{language}`
  :option:`-v`  :option:`-###`  :option:`--help`:samp:`[={class}[,...]]`  :option:`--target-help`  :option:`--version`
  :option:`-pass-exit-codes`  :option:`-pipe`  :option:`-specs`:samp:`={file}`  :option:`-wrapper`:samp:`@{file}`
  :option:`-ffile-prefix-map`:samp:`={old}={new}`
  :option:`-fplugin`:samp:`={file}`  :option:`-fplugin-arg-`:samp:`{name}={arg}`
  :option:`-fdump-ada-spec`:samp:`[-{slim}]` :option:`-fada-spec-parent`:samp:`={unit}`  :option:`-fdump-go-spec`:samp:`={file}`

*C Language Options*
  See :ref:`Options Controlling C Dialect <c-dialect-options>`.

  :option:`-ansi`  :option:`-std`:samp:`={standard}`  :option:`-fgnu89-inline`
  :option:`-fpermitted-flt-eval-methods`:samp:`={standard}`
  :option:`-aux-info` :samp:`{filename}`  :option:`-fallow-parameterless-variadic-functions`
  :option:`-fno-asm`  :option:`-fno-builtin`  :option:`-fno-builtin-`:samp:`{function}`  :option:`-fgimple`
  :option:`-fhosted`  :option:`-ffreestanding`
  :option:`-fopenacc`  :option:`-fopenacc-dim`:samp:`={geom}`
  :option:`-fopenmp`  :option:`-fopenmp-simd`
  :option:`-fms-extensions`  :option:`-fplan9-extensions`  :option:`-fsso-struct`:samp:`={endianness}`
  :option:`-fallow-single-precision`  :option:`-fcond-mismatch`  :option:`-flax-vector-conversions`
  :option:`-fsigned-bitfields`  :option:`-fsigned-char`
  :option:`-funsigned-bitfields`  :option:`-funsigned-char`

*C++ Language Options*
  See :ref:`Options Controlling C++ Dialect <c++-dialect-options>`.

  :option:`-fabi-version`:samp:`={n}`  :option:`-fno-access-control`
  :option:`-faligned-new`:samp:`={n}`  :option:`-fargs-in-order`:samp:`={n}`  :option:`-fchar8_t`  :option:`-fcheck-new`
  :option:`-fconstexpr-depth`:samp:`={n}`  :option:`-fconstexpr-cache-depth`:samp:`={n}`
  :option:`-fconstexpr-loop-limit`:samp:`={n}`  :option:`-fconstexpr-ops-limit`:samp:`={n}`
  :option:`-fno-elide-constructors`
  :option:`-fno-enforce-eh-specs`
  :option:`-fno-gnu-keywords`
  :option:`-fno-implicit-templates`
  :option:`-fno-implicit-inline-templates`
  :option:`-fno-implement-inlines`
  :option:`-fmodule-header` :samp:`[={kind}]` :option:`-fmodule-only` :option:`-fmodules-ts`
  :option:`-fmodule-implicit-inline`
  :option:`-fno-module-lazy`
  :option:`-fmodule-mapper`:samp:`={specification}`
  :option:`-fmodule-version-ignore`
  :option:`-fms-extensions`
  :option:`-fnew-inheriting-ctors`
  :option:`-fnew-ttp-matching`
  :option:`-fno-nonansi-builtins`  :option:`-fnothrow-opt`  :option:`-fno-operator-names`
  :option:`-fno-optional-diags`  :option:`-fpermissive`
  :option:`-fno-pretty-templates`
  :option:`-fno-rtti`  :option:`-fsized-deallocation`
  :option:`-ftemplate-backtrace-limit`:samp:`={n}`
  :option:`-ftemplate-depth`:samp:`={n}`
  :option:`-fno-threadsafe-statics`  :option:`-fuse-cxa-atexit`
  :option:`-fno-weak`  :option:`-nostdinc++`
  :option:`-fvisibility-inlines-hidden`
  :option:`-fvisibility-ms-compat`
  :option:`-fext-numeric-literals`
  :option:`-flang-info-include-translate`:samp:`=[{header}]`
  :option:`-flang-info-include-translate-not`
  :option:`-flang-info-module-cmi`:samp:`[={module}]`
  :option:`-stdlib`:samp:`={libstdc++,libc++}`
  :option:`-Wabi-tag`  :option:`-Wcatch-value`  :option:`-Wcatch-value`:samp:`={n}`
  :option:`-Wno-class-conversion`  :option:`-Wclass-memaccess`
  :option:`-Wcomma-subscript`  :option:`-Wconditionally-supported`
  :option:`-Wno-conversion-null`  :option:`-Wctad-maybe-unsupported`
  :option:`-Wctor-dtor-privacy`  :option:`-Wno-delete-incomplete`
  :option:`-Wdelete-non-virtual-dtor`  :option:`-Wdeprecated-copy` :option:`-Wdeprecated-copy-dtor`
  :option:`-Wno-deprecated-enum-enum-conversion` :option:`-Wno-deprecated-enum-float-conversion`
  :option:`-Weffc++`  :option:`-Wno-exceptions` :option:`-Wextra-semi`  :option:`-Wno-inaccessible-base`
  :option:`-Wno-inherited-variadic-ctor`  :option:`-Wno-init-list-lifetime`
  :option:`-Winvalid-imported-macros`
  :option:`-Wno-invalid-offsetof`  :option:`-Wno-literal-suffix`
  :option:`-Wmismatched-new-delete` :option:`-Wmismatched-tags`
  :option:`-Wmultiple-inheritance`  :option:`-Wnamespaces`  :option:`-Wnarrowing`
  :option:`-Wnoexcept`  :option:`-Wnoexcept-type`  :option:`-Wnon-virtual-dtor`
  :option:`-Wpessimizing-move`  :option:`-Wno-placement-new`  :option:`-Wplacement-new`:samp:`={n}`
  :option:`-Wrange-loop-construct` :option:`-Wredundant-move` :option:`-Wredundant-tags`
  :option:`-Wreorder`  :option:`-Wregister`
  :option:`-Wstrict-null-sentinel`  :option:`-Wno-subobject-linkage`  :option:`-Wtemplates`
  :option:`-Wno-non-template-friend`  :option:`-Wold-style-cast`
  :option:`-Woverloaded-virtual`  :option:`-Wno-pmf-conversions` :option:`-Wsign-promo`
  :option:`-Wsized-deallocation`  :option:`-Wsuggest-final-methods`
  :option:`-Wsuggest-final-types`  :option:`-Wsuggest-override`
  :option:`-Wno-terminate`  :option:`-Wuseless-cast`  :option:`-Wno-vexing-parse`
  :option:`-Wvirtual-inheritance`
  :option:`-Wno-virtual-move-assign`  :option:`-Wvolatile`  :option:`-Wzero-as-null-pointer-constant`

*Objective-C and Objective-C++ Language Options*
  See :ref:`Options Controlling
  Objective-C and Objective-C++ Dialects <objective-c-and-objective-c++-dialect-options>`.

  :option:`-fconstant-string-class`:samp:`={class-name}`
  :option:`-fgnu-runtime`  :option:`-fnext-runtime`
  :option:`-fno-nil-receivers`
  :option:`-fobjc-abi-version`:samp:`={n}`
  :option:`-fobjc-call-cxx-cdtors`
  :option:`-fobjc-direct-dispatch`
  :option:`-fobjc-exceptions`
  :option:`-fobjc-gc`
  :option:`-fobjc-nilcheck`
  :option:`-fobjc-std`:samp:`=objc1`
  :option:`-fno-local-ivars`
  :option:`-fivar-visibility`:samp:`=[public|protected|private|package]`
  :option:`-freplace-objc-classes`
  :option:`-fzero-link`
  :option:`-gen-decls`
  :option:`-Wassign-intercept`  :option:`-Wno-property-assign-default`
  :option:`-Wno-protocol` :option:`-Wobjc-root-class` :option:`-Wselector`
  :option:`-Wstrict-selector-match`
  :option:`-Wundeclared-selector`

*Diagnostic Message Formatting Options*
  See :ref:`Options to Control Diagnostic Messages Formatting <diagnostic-message-formatting-options>`.

  :option:`-fmessage-length`:samp:`={n}`
  :option:`-fdiagnostics-plain-output`
  :option:`-fdiagnostics-show-location`:samp:`=[once|every-line]`
  :option:`-fdiagnostics-color`:samp:`=[auto|never|always]`
  :option:`-fdiagnostics-urls`:samp:`=[auto|never|always]`
  :option:`-fdiagnostics-format`:samp:`=[text|json]`
  :option:`-fno-diagnostics-show-option`  :option:`-fno-diagnostics-show-caret`
  :option:`-fno-diagnostics-show-labels`  :option:`-fno-diagnostics-show-line-numbers`
  :option:`-fno-diagnostics-show-cwe`
  :option:`-fdiagnostics-minimum-margin-width`:samp:`={width}`
  :option:`-fdiagnostics-parseable-fixits`  :option:`-fdiagnostics-generate-patch`
  :option:`-fdiagnostics-show-template-tree`  :option:`-fno-elide-type`
  :option:`-fdiagnostics-path-format`:samp:`=[none|separate-events|inline-events]`
  :option:`-fdiagnostics-show-path-depths`
  :option:`-fno-show-column`
  :option:`-fdiagnostics-column-unit`:samp:`=[display|byte]`
  :option:`-fdiagnostics-column-origin`:samp:`={origin}`

*Warning Options*
  See :ref:`Options to Request or Suppress Warnings <warning-options>`.

  :option:`-fsyntax-only`  :option:`-fmax-errors`:samp:`={n}`  :option:`-Wpedantic`
  :option:`-pedantic-errors`
  :option:`-w`  :option:`-Wextra`  :option:`-Wall`  :option:`-Wabi`:samp:`={n}`
  :option:`-Waddress`  :option:`-Wno-address-of-packed-member`  :option:`-Waggregate-return`
  :option:`-Walloc-size-larger-than`:samp:`={byte-size}`  :option:`-Walloc-zero`
  :option:`-Walloca`  :option:`-Walloca-larger-than`:samp:`={byte-size}`
  :option:`-Wno-aggressive-loop-optimizations`
  :option:`-Warith-conversion`
  :option:`-Warray-bounds`  :option:`-Warray-bounds`:samp:`={n}`
  :option:`-Wno-attributes`  :option:`-Wattribute-alias`:samp:`={n}` :option:`-Wno-attribute-alias`
  :option:`-Wno-attribute-warning`  :option:`-Wbool-compare`  :option:`-Wbool-operation`
  :option:`-Wno-builtin-declaration-mismatch`
  :option:`-Wno-builtin-macro-redefined`  :option:`-Wc90-c99-compat`  :option:`-Wc99-c11-compat`
  :option:`-Wc11-c2x-compat`
  :option:`-Wc++-compat`  :option:`-Wc++11-compat`  :option:`-Wc++14-compat`  :option:`-Wc++17-compat`
  :option:`-Wc++20-compat`
  :option:`-Wno-c++11-extensions`  :option:`-Wno-c++14-extensions` :option:`-Wno-c++17-extensions`
  :option:`-Wno-c++20-extensions`  :option:`-Wno-c++23-extensions`
  :option:`-Wcast-align`  :option:`-Wcast-align`:samp:`=strict`  :option:`-Wcast-function-type`  :option:`-Wcast-qual`
  :option:`-Wchar-subscripts`
  :option:`-Wclobbered`  :option:`-Wcomment`
  :option:`-Wconversion`  :option:`-Wno-coverage-mismatch`  :option:`-Wno-cpp`
  :option:`-Wdangling-else`  :option:`-Wdate-time`
  :option:`-Wno-deprecated`  :option:`-Wno-deprecated-declarations`  :option:`-Wno-designated-init`
  :option:`-Wdisabled-optimization`
  :option:`-Wno-discarded-array-qualifiers`  :option:`-Wno-discarded-qualifiers`
  :option:`-Wno-div-by-zero`  :option:`-Wdouble-promotion`
  :option:`-Wduplicated-branches`  :option:`-Wduplicated-cond`
  :option:`-Wempty-body`  :option:`-Wno-endif-labels`  :option:`-Wenum-compare`  :option:`-Wenum-conversion`
  :option:`-Werror`  :option:`-Werror`:samp:`=*`  :option:`-Wexpansion-to-defined`  :option:`-Wfatal-errors`
  :option:`-Wfloat-conversion`  :option:`-Wfloat-equal`  :option:`-Wformat`  :option:`-Wformat`:samp:`=2`
  :option:`-Wno-format-contains-nul`  :option:`-Wno-format-extra-args`
  :option:`-Wformat-nonliteral`  :option:`-Wformat-overflow`:samp:`={n}`
  :option:`-Wformat-security`  :option:`-Wformat-signedness`  :option:`-Wformat-truncation`:samp:`={n}`
  :option:`-Wformat-y2k`  :option:`-Wframe-address`
  :option:`-Wframe-larger-than`:samp:`={byte-size}`  :option:`-Wno-free-nonheap-object`
  :option:`-Wno-if-not-aligned`  :option:`-Wno-ignored-attributes`
  :option:`-Wignored-qualifiers`  :option:`-Wno-incompatible-pointer-types`
  :option:`-Wimplicit`  :option:`-Wimplicit-fallthrough`  :option:`-Wimplicit-fallthrough`:samp:`={n}`
  :option:`-Wno-implicit-function-declaration`  :option:`-Wno-implicit-int`
  :option:`-Winit-self`  :option:`-Winline`  :option:`-Wno-int-conversion`  :option:`-Wint-in-bool-context`
  :option:`-Wno-int-to-pointer-cast`  :option:`-Wno-invalid-memory-model`
  :option:`-Winvalid-pch`  :option:`-Wjump-misses-init`  :option:`-Wlarger-than`:samp:`={byte-size}`
  :option:`-Wlogical-not-parentheses`  :option:`-Wlogical-op`  :option:`-Wlong-long`
  :option:`-Wno-lto-type-mismatch` :option:`-Wmain`  :option:`-Wmaybe-uninitialized`
  :option:`-Wmemset-elt-size`  :option:`-Wmemset-transposed-args`
  :option:`-Wmisleading-indentation`  :option:`-Wmissing-attributes`  :option:`-Wmissing-braces`
  :option:`-Wmissing-field-initializers`  :option:`-Wmissing-format-attribute`
  :option:`-Wmissing-include-dirs`  :option:`-Wmissing-noreturn`  :option:`-Wno-missing-profile`
  :option:`-Wno-multichar`  :option:`-Wmultistatement-macros`  :option:`-Wnonnull`  :option:`-Wnonnull-compare`
  :option:`-Wnormalized`:samp:`=[none|id|nfc|nfkc]`
  :option:`-Wnull-dereference`  :option:`-Wno-odr`
  :option:`-Wopenacc-parallelism`
  :option:`-Wopenmp-simd`
  :option:`-Wno-overflow`  :option:`-Woverlength-strings`  :option:`-Wno-override-init-side-effects`
  :option:`-Wpacked`  :option:`-Wno-packed-bitfield-compat`  :option:`-Wpacked-not-aligned`  :option:`-Wpadded`
  :option:`-Wparentheses`  :option:`-Wno-pedantic-ms-format`
  :option:`-Wpointer-arith`  :option:`-Wno-pointer-compare`  :option:`-Wno-pointer-to-int-cast`
  :option:`-Wno-pragmas`  :option:`-Wno-prio-ctor-dtor`  :option:`-Wredundant-decls`
  :option:`-Wrestrict`  :option:`-Wno-return-local-addr`  :option:`-Wreturn-type`
  :option:`-Wno-scalar-storage-order`  :option:`-Wsequence-point`
  :option:`-Wshadow`  :option:`-Wshadow`:samp:`=global`  :option:`-Wshadow`:samp:`=local`  :option:`-Wshadow`:samp:`=compatible-local`
  :option:`-Wno-shadow-ivar`
  :option:`-Wno-shift-count-negative`  :option:`-Wno-shift-count-overflow`  :option:`-Wshift-negative-value`
  :option:`-Wno-shift-overflow`  :option:`-Wshift-overflow`:samp:`={n}`
  :option:`-Wsign-compare`  :option:`-Wsign-conversion`
  :option:`-Wno-sizeof-array-argument`
  :option:`-Wsizeof-array-div`
  :option:`-Wsizeof-pointer-div`  :option:`-Wsizeof-pointer-memaccess`
  :option:`-Wstack-protector`  :option:`-Wstack-usage`:samp:`={byte-size}`  :option:`-Wstrict-aliasing`
  :option:`-Wstrict-aliasing`:samp:`=n`  :option:`-Wstrict-overflow`  :option:`-Wstrict-overflow`:samp:`={n}`
  :option:`-Wstring-compare`
  :option:`-Wno-stringop-overflow` :option:`-Wno-stringop-overread`
  :option:`-Wno-stringop-truncation`
  :option:`-Wsuggest-attribute`:samp:`=[pure|const|noreturn|format|malloc]`
  :option:`-Wswitch`  :option:`-Wno-switch-bool`  :option:`-Wswitch-default`  :option:`-Wswitch-enum`
  :option:`-Wno-switch-outside-range`  :option:`-Wno-switch-unreachable`  :option:`-Wsync-nand`
  :option:`-Wsystem-headers`  :option:`-Wtautological-compare`  :option:`-Wtrampolines`  :option:`-Wtrigraphs`
  :option:`-Wtsan` :option:`-Wtype-limits`  :option:`-Wundef`
  :option:`-Wuninitialized`  :option:`-Wunknown-pragmas`
  :option:`-Wunsuffixed-float-constants`  :option:`-Wunused`
  :option:`-Wunused-but-set-parameter`  :option:`-Wunused-but-set-variable`
  :option:`-Wunused-const-variable`  :option:`-Wunused-const-variable`:samp:`={n}`
  :option:`-Wunused-function`  :option:`-Wunused-label`  :option:`-Wunused-local-typedefs`
  :option:`-Wunused-macros`
  :option:`-Wunused-parameter`  :option:`-Wno-unused-result`
  :option:`-Wunused-value`  :option:`-Wunused-variable`
  :option:`-Wno-varargs`  :option:`-Wvariadic-macros`
  :option:`-Wvector-operation-performance`
  :option:`-Wvla`  :option:`-Wvla-larger-than`:samp:`={byte-size}`  :option:`-Wno-vla-larger-than`
  :option:`-Wvolatile-register-var`  :option:`-Wwrite-strings`
  :option:`-Wzero-length-bounds`

*Static Analyzer Options*
  :option:`-fanalyzer`
  :option:`-fanalyzer-call-summaries`
  :option:`-fanalyzer-checker`:samp:`={name}`
  :option:`-fno-analyzer-feasibility`
  :option:`-fanalyzer-fine-grained`
  :option:`-fanalyzer-state-merge`
  :option:`-fanalyzer-state-purge`
  :option:`-fanalyzer-transitivity`
  :option:`-fanalyzer-verbose-edges`
  :option:`-fanalyzer-verbose-state-changes`
  :option:`-fanalyzer-verbosity`:samp:`={level}`
  :option:`-fdump-analyzer`
  :option:`-fdump-analyzer-stderr`
  :option:`-fdump-analyzer-callgraph`
  :option:`-fdump-analyzer-exploded-graph`
  :option:`-fdump-analyzer-exploded-nodes`
  :option:`-fdump-analyzer-exploded-nodes-2`
  :option:`-fdump-analyzer-exploded-nodes-3`
  :option:`-fdump-analyzer-feasibility`
  :option:`-fdump-analyzer-json`
  :option:`-fdump-analyzer-state-purge`
  :option:`-fdump-analyzer-supergraph`
  :option:`-Wno-analyzer-double-fclose`
  :option:`-Wno-analyzer-double-free`
  :option:`-Wno-analyzer-exposure-through-output-file`
  :option:`-Wno-analyzer-file-leak`
  :option:`-Wno-analyzer-free-of-non-heap`
  :option:`-Wno-analyzer-malloc-leak`
  :option:`-Wno-analyzer-mismatching-deallocation`
  :option:`-Wno-analyzer-null-argument`
  :option:`-Wno-analyzer-null-dereference`
  :option:`-Wno-analyzer-possible-null-argument`
  :option:`-Wno-analyzer-possible-null-dereference`
  :option:`-Wno-analyzer-shift-count-negative`
  :option:`-Wno-analyzer-shift-count-overflow`
  :option:`-Wno-analyzer-stale-setjmp-buffer`
  :option:`-Wno-analyzer-tainted-array-index`
  :option:`-Wanalyzer-too-complex`
  :option:`-Wno-analyzer-unsafe-call-within-signal-handler`
  :option:`-Wno-analyzer-use-after-free`
  :option:`-Wno-analyzer-use-of-pointer-in-stale-stack-frame`
  :option:`-Wno-analyzer-use-of-uninitialized-value`
  :option:`-Wno-analyzer-write-to-const`
  :option:`-Wno-analyzer-write-to-string-literal`

*C and Objective-C-only Warning Options*
  :option:`-Wbad-function-cast`  :option:`-Wmissing-declarations`
  :option:`-Wmissing-parameter-type`  :option:`-Wmissing-prototypes`  :option:`-Wnested-externs`
  :option:`-Wold-style-declaration`  :option:`-Wold-style-definition`
  :option:`-Wstrict-prototypes`  :option:`-Wtraditional`  :option:`-Wtraditional-conversion`
  :option:`-Wdeclaration-after-statement`  :option:`-Wpointer-sign`

*Debugging Options*
  See :ref:`Options for Debugging Your Program <debugging-options>`.

  :option:`-g`  :option:`-g`:samp:`{level}`  :option:`-gdwarf`  :option:`-gdwarf-`:samp:`{version}`
  :option:`-ggdb`  :option:`-grecord-gcc-switches`  :option:`-gno-record-gcc-switches`
  :option:`-gstabs`  :option:`-gstabs+`  :option:`-gstrict-dwarf`  :option:`-gno-strict-dwarf`
  :option:`-gas-loc-support`  :option:`-gno-as-loc-support`
  :option:`-gas-locview-support`  :option:`-gno-as-locview-support`
  :option:`-gcolumn-info`  :option:`-gno-column-info`  :option:`-gdwarf32`  :option:`-gdwarf64`
  :option:`-gstatement-frontiers`  :option:`-gno-statement-frontiers`
  :option:`-gvariable-location-views`  :option:`-gno-variable-location-views`
  :option:`-ginternal-reset-location-views`  :option:`-gno-internal-reset-location-views`
  :option:`-ginline-points`  :option:`-gno-inline-points`
  :option:`-gvms`  :option:`-gxcoff`  :option:`-gxcoff+`  :option:`-gz`:samp:`[={type}]`
  :option:`-gsplit-dwarf`  :option:`-gdescribe-dies`  :option:`-gno-describe-dies`
  :option:`-fdebug-prefix-map`:samp:`={old}={new}`  :option:`-fdebug-types-section`
  :option:`-fno-eliminate-unused-debug-types`
  :option:`-femit-struct-debug-baseonly`  :option:`-femit-struct-debug-reduced`
  :option:`-femit-struct-debug-detailed`:samp:`[={spec-list}]`
  :option:`-fno-eliminate-unused-debug-symbols`  :option:`-femit-class-debug-always`
  :option:`-fno-merge-debug-strings`  :option:`-fno-dwarf2-cfi-asm`
  :option:`-fvar-tracking`  :option:`-fvar-tracking-assignments`

*Optimization Options*
  See :ref:`Options that Control Optimization <optimize-options>`.

  :option:`-faggressive-loop-optimizations`
  :option:`-falign-functions`:samp:`[={n}[{m}:[{n2}[:{m2}]]]]`
  :option:`-falign-jumps`:samp:`[={n}[{m}:[{n2}[:{m2}]]]]`
  :option:`-falign-labels`:samp:`[={n}[{m}:[{n2}[:{m2}]]]]`
  :option:`-falign-loops`:samp:`[={n}[{m}:[{n2}[:{m2}]]]]`
  :option:`-fno-allocation-dce` :option:`-fallow-store-data-races`
  :option:`-fassociative-math`  :option:`-fauto-profile`  :option:`-fauto-profile`:samp:`[={path}]`
  :option:`-fauto-inc-dec`  :option:`-fbranch-probabilities`
  :option:`-fcaller-saves`
  :option:`-fcombine-stack-adjustments`  :option:`-fconserve-stack`
  :option:`-fcompare-elim`  :option:`-fcprop-registers`  :option:`-fcrossjumping`
  :option:`-fcse-follow-jumps`  :option:`-fcse-skip-blocks`  :option:`-fcx-fortran-rules`
  :option:`-fcx-limited-range`
  :option:`-fdata-sections`  :option:`-fdce`  :option:`-fdelayed-branch`
  :option:`-fdelete-null-pointer-checks`  :option:`-fdevirtualize`  :option:`-fdevirtualize-speculatively`
  :option:`-fdevirtualize-at-ltrans`  :option:`-fdse`
  :option:`-fearly-inlining`  :option:`-fipa-sra`  :option:`-fexpensive-optimizations`  :option:`-ffat-lto-objects`
  :option:`-ffast-math`  :option:`-ffinite-math-only`  :option:`-ffloat-store`  :option:`-fexcess-precision`:samp:`={style}`
  :option:`-ffinite-loops`
  :option:`-fforward-propagate`  :option:`-ffp-contract`:samp:`={style}`  :option:`-ffunction-sections`
  :option:`-fgcse`  :option:`-fgcse-after-reload`  :option:`-fgcse-las`  :option:`-fgcse-lm`  :option:`-fgraphite-identity`
  :option:`-fgcse-sm`  :option:`-fhoist-adjacent-loads`  :option:`-fif-conversion`
  :option:`-fif-conversion2`  :option:`-findirect-inlining`
  :option:`-finline-functions`  :option:`-finline-functions-called-once`  :option:`-finline-limit`:samp:`={n}`
  :option:`-finline-small-functions` :option:`-fipa-modref` :option:`-fipa-cp`  :option:`-fipa-cp-clone`
  :option:`-fipa-bit-cp`  :option:`-fipa-vrp`  :option:`-fipa-pta`  :option:`-fipa-profile`  :option:`-fipa-pure-const`
  :option:`-fipa-reference`  :option:`-fipa-reference-addressable`
  :option:`-fipa-stack-alignment`  :option:`-fipa-icf`  :option:`-fira-algorithm`:samp:`={algorithm}`
  :option:`-flive-patching`:samp:`={level}`
  :option:`-fira-region`:samp:`={region}`  :option:`-fira-hoist-pressure`
  :option:`-fira-loop-pressure`  :option:`-fno-ira-share-save-slots`
  :option:`-fno-ira-share-spill-slots`
  :option:`-fisolate-erroneous-paths-dereference`  :option:`-fisolate-erroneous-paths-attribute`
  :option:`-fivopts`  :option:`-fkeep-inline-functions`  :option:`-fkeep-static-functions`
  :option:`-fkeep-static-consts`  :option:`-flimit-function-alignment`  :option:`-flive-range-shrinkage`
  :option:`-floop-block`  :option:`-floop-interchange`  :option:`-floop-strip-mine`
  :option:`-floop-unroll-and-jam`  :option:`-floop-nest-optimize`
  :option:`-floop-parallelize-all`  :option:`-flra-remat`  :option:`-flto`  :option:`-flto-compression-level`
  :option:`-flto-partition`:samp:`={alg}`  :option:`-fmerge-all-constants`
  :option:`-fmerge-constants`  :option:`-fmodulo-sched`  :option:`-fmodulo-sched-allow-regmoves`
  :option:`-fmove-loop-invariants`  :option:`-fno-branch-count-reg`
  :option:`-fno-defer-pop`  :option:`-fno-fp-int-builtin-inexact`  :option:`-fno-function-cse`
  :option:`-fno-guess-branch-probability`  :option:`-fno-inline`  :option:`-fno-math-errno`  :option:`-fno-peephole`
  :option:`-fno-peephole2`  :option:`-fno-printf-return-value`  :option:`-fno-sched-interblock`
  :option:`-fno-sched-spec`  :option:`-fno-signed-zeros`
  :option:`-fno-toplevel-reorder`  :option:`-fno-trapping-math`  :option:`-fno-zero-initialized-in-bss`
  :option:`-fomit-frame-pointer`  :option:`-foptimize-sibling-calls`
  :option:`-fpartial-inlining`  :option:`-fpeel-loops`  :option:`-fpredictive-commoning`
  :option:`-fprefetch-loop-arrays`
  :option:`-fprofile-correction`
  :option:`-fprofile-use`  :option:`-fprofile-use`:samp:`={path}` :option:`-fprofile-partial-training`
  :option:`-fprofile-values` :option:`-fprofile-reorder-functions`
  :option:`-freciprocal-math`  :option:`-free`  :option:`-frename-registers`  :option:`-freorder-blocks`
  :option:`-freorder-blocks-algorithm`:samp:`={algorithm}`
  :option:`-freorder-blocks-and-partition`  :option:`-freorder-functions`
  :option:`-frerun-cse-after-loop`  :option:`-freschedule-modulo-scheduled-loops`
  :option:`-frounding-math`  :option:`-fsave-optimization-record`
  :option:`-fsched2-use-superblocks`  :option:`-fsched-pressure`
  :option:`-fsched-spec-load`  :option:`-fsched-spec-load-dangerous`
  :option:`-fsched-stalled-insns-dep`:samp:`[={n}]`  :option:`-fsched-stalled-insns`:samp:`[={n}]`
  :option:`-fsched-group-heuristic`  :option:`-fsched-critical-path-heuristic`
  :option:`-fsched-spec-insn-heuristic`  :option:`-fsched-rank-heuristic`
  :option:`-fsched-last-insn-heuristic`  :option:`-fsched-dep-count-heuristic`
  :option:`-fschedule-fusion`
  :option:`-fschedule-insns`  :option:`-fschedule-insns2`  :option:`-fsection-anchors`
  :option:`-fselective-scheduling`  :option:`-fselective-scheduling2`
  :option:`-fsel-sched-pipelining`  :option:`-fsel-sched-pipelining-outer-loops`
  :option:`-fsemantic-interposition`  :option:`-fshrink-wrap`  :option:`-fshrink-wrap-separate`
  :option:`-fsignaling-nans`
  :option:`-fsingle-precision-constant`  :option:`-fsplit-ivs-in-unroller`  :option:`-fsplit-loops`
  :option:`-fsplit-paths`
  :option:`-fsplit-wide-types`  :option:`-fsplit-wide-types-early`  :option:`-fssa-backprop`  :option:`-fssa-phiopt`
  :option:`-fstdarg-opt`  :option:`-fstore-merging`  :option:`-fstrict-aliasing`
  :option:`-fthread-jumps`  :option:`-ftracer`  :option:`-ftree-bit-ccp`
  :option:`-ftree-builtin-call-dce`  :option:`-ftree-ccp`  :option:`-ftree-ch`
  :option:`-ftree-coalesce-vars`  :option:`-ftree-copy-prop`  :option:`-ftree-dce`  :option:`-ftree-dominator-opts`
  :option:`-ftree-dse`  :option:`-ftree-forwprop`  :option:`-ftree-fre`  :option:`-fcode-hoisting`
  :option:`-ftree-loop-if-convert`  :option:`-ftree-loop-im`
  :option:`-ftree-phiprop`  :option:`-ftree-loop-distribution`  :option:`-ftree-loop-distribute-patterns`
  :option:`-ftree-loop-ivcanon`  :option:`-ftree-loop-linear`  :option:`-ftree-loop-optimize`
  :option:`-ftree-loop-vectorize`
  :option:`-ftree-parallelize-loops`:samp:`={n}`  :option:`-ftree-pre`  :option:`-ftree-partial-pre`  :option:`-ftree-pta`
  :option:`-ftree-reassoc`  :option:`-ftree-scev-cprop`  :option:`-ftree-sink`  :option:`-ftree-slsr`  :option:`-ftree-sra`
  :option:`-ftree-switch-conversion`  :option:`-ftree-tail-merge`
  :option:`-ftree-ter`  :option:`-ftree-vectorize`  :option:`-ftree-vrp`  :option:`-funconstrained-commons`
  :option:`-funit-at-a-time`  :option:`-funroll-all-loops`  :option:`-funroll-loops`
  :option:`-funsafe-math-optimizations`  :option:`-funswitch-loops`
  :option:`-fipa-ra`  :option:`-fvariable-expansion-in-unroller`  :option:`-fvect-cost-model`  :option:`-fvpt`
  :option:`-fweb`  :option:`-fwhole-program`  :option:`-fwpa`  :option:`-fuse-linker-plugin` :option:`-fzero-call-used-regs`
  :option:`--param` :samp:`{name}={value}`
  :option:`-O`  :option:`-O0`  :option:`-O1`  :option:`-O2`  :option:`-O3`  :option:`-Os`  :option:`-Ofast`  :option:`-Og`

*Program Instrumentation Options*
  See :ref:`Program Instrumentation Options <instrumentation-options>`.

  :option:`-p`  :option:`-pg`  :option:`-fprofile-arcs`  :option:`--coverage`  :option:`-ftest-coverage`
  :option:`-fprofile-abs-path`
  :option:`-fprofile-dir`:samp:`={path}`  :option:`-fprofile-generate`  :option:`-fprofile-generate`:samp:`={path}`
  :option:`-fprofile-info-section`  :option:`-fprofile-info-section`:samp:`={name}`
  :option:`-fprofile-note`:samp:`={path}` :option:`-fprofile-prefix-path`:samp:`={path}`
  :option:`-fprofile-update`:samp:`={method}` :option:`-fprofile-filter-files`:samp:`={regex}`
  :option:`-fprofile-exclude-files`:samp:`={regex}`
  :option:`-fprofile-reproducible`:samp:`=[multithreaded|parallel-runs|serial`
  :option:`-fsanitize`:samp:`={style}`  :option:`-fsanitize-recover`  :option:`-fsanitize-recover`:samp:`={style}`
  :option:`-fasan-shadow-offset`:samp:`={number}`  :option:`-fsanitize-sections`:samp:`={s1}, {s2},...`
  :option:`-fsanitize-undefined-trap-on-error`  :option:`-fbounds-check`
  :option:`-fcf-protection`:samp:`=[full|branch|return|none|check]`
  :option:`-fstack-protector`  :option:`-fstack-protector-all`  :option:`-fstack-protector-strong`
  :option:`-fstack-protector-explicit`  :option:`-fstack-check`
  :option:`-fstack-limit-register`:samp:`={reg}`  :option:`-fstack-limit-symbol`:samp:`={sym}`
  :option:`-fno-stack-limit`  :option:`-fsplit-stack`
  :option:`-fvtable-verify`:samp:`=[std|preinit|none]`
  :option:`-fvtv-counts`  :option:`-fvtv-debug`
  :option:`-finstrument-functions`
  :option:`-finstrument-functions-exclude-function-list`:samp:`={sym}, {sym},...`
  :option:`-finstrument-functions-exclude-file-list`:samp:`={file}, {file},...`

*Preprocessor Options*
  See :ref:`Options Controlling the Preprocessor <preprocessor-options>`.

  :option:`-A`:samp:`{question}={answer}`
  :option:`-A-`:samp:`{question}[={answer}]`
  :option:`-C`  :option:`-CC`  :option:`-D`:samp:`{macro}[={defn}]`
  :option:`-dD`  :option:`-dI`  :option:`-dM`  :option:`-dN`  :option:`-dU`
  :option:`-fdebug-cpp`  :option:`-fdirectives-only`  :option:`-fdollars-in-identifiers`
  :option:`-fexec-charset`:samp:`={charset}`  :option:`-fextended-identifiers`
  :option:`-finput-charset`:samp:`={charset}`  :option:`-flarge-source-files`
  :option:`-fmacro-prefix-map`:samp:`={old}={new}` :option:`-fmax-include-depth`:samp:`={depth}`
  :option:`-fno-canonical-system-headers`  :option:`-fpch-deps`  :option:`-fpch-preprocess`
  :option:`-fpreprocessed`  :option:`-ftabstop`:samp:`={width}`  :option:`-ftrack-macro-expansion`
  :option:`-fwide-exec-charset`:samp:`={charset}`  :option:`-fworking-directory`
  :option:`-H`  :option:`-imacros` :samp:`{file}`  :option:`-include` :samp:`{file}`
  :option:`-M`  :option:`-MD`  :option:`-MF`  :option:`-MG`  :option:`-MM`  :option:`-MMD`  :option:`-MP`  :option:`-MQ`  :option:`-MT` :option:`-Mno-modules`
  :option:`-no-integrated-cpp`  :option:`-P`  :option:`-pthread`  :option:`-remap`
  :option:`-traditional`  :option:`-traditional-cpp`  :option:`-trigraphs`
  :option:`-U`:samp:`{macro}`  :option:`-undef`
  :option:`-Wp,`:samp:`{option}`  :option:`-Xpreprocessor` :samp:`{option}`

*Assembler Options*
  See :ref:`Passing Options to the Assembler <assembler-options>`.

  :option:`-Wa,`:samp:`{option}`  :option:`-Xassembler` :samp:`{option}`

*Linker Options*
  See :ref:`Options for Linking <link-options>`.

  :samp:`{object-file-name}`  :option:`-fuse-ld`:samp:`={linker}`  :option:`-l`:samp:`{library}`
  :option:`-nostartfiles`  :option:`-nodefaultlibs`  :option:`-nolibc`  :option:`-nostdlib`
  :option:`-e` :samp:`{entry}`  :option:`--entry`:samp:`={entry}`
  :option:`-pie`  :option:`-pthread`  :option:`-r`  :option:`-rdynamic`
  :option:`-s`  :option:`-static`  :option:`-static-pie`  :option:`-static-libgcc`  :option:`-static-libstdc++`
  :option:`-static-libasan`  :option:`-static-libtsan`  :option:`-static-liblsan`  :option:`-static-libubsan`
  :option:`-shared`  :option:`-shared-libgcc`  :option:`-symbolic`
  :option:`-T` :samp:`{script}`  :option:`-Wl,`:samp:`{option}`  :option:`-Xlinker` :samp:`{option}`
  :option:`-u` :samp:`{symbol}`  :option:`-z` :samp:`{keyword}`

*Directory Options*
  See :ref:`Options for Directory Search <directory-options>`.

  :option:`-B`:samp:`{prefix}`  :option:`-I`:samp:`{dir}`  :option:`-I-`
  :option:`-idirafter` :samp:`{dir}`
  :option:`-imacros` :samp:`{file}`  :option:`-imultilib` :samp:`{dir}`
  :option:`-iplugindir`:samp:`={dir}`  :option:`-iprefix` :samp:`{file}`
  :option:`-iquote` :samp:`{dir}`  :option:`-isysroot` :samp:`{dir}`  :option:`-isystem` :samp:`{dir}`
  :option:`-iwithprefix` :samp:`{dir}`  :option:`-iwithprefixbefore` :samp:`{dir}`
  :option:`-L`:samp:`{dir}`  :option:`-no-canonical-prefixes`  :option:`--no-sysroot-suffix`
  :option:`-nostdinc`  :option:`-nostdinc++`  :option:`--sysroot`:samp:`={dir}`

*Code Generation Options*
  See :ref:`Options for Code Generation Conventions <code-gen-options>`.

  :option:`-fcall-saved-`:samp:`{reg}`  :option:`-fcall-used-`:samp:`{reg}`
  :option:`-ffixed-`:samp:`{reg}`  :option:`-fexceptions`
  :option:`-fnon-call-exceptions`  :option:`-fdelete-dead-exceptions`  :option:`-funwind-tables`
  :option:`-fasynchronous-unwind-tables`
  :option:`-fno-gnu-unique`
  :option:`-finhibit-size-directive`  :option:`-fcommon`  :option:`-fno-ident`
  :option:`-fpcc-struct-return`  :option:`-fpic`  :option:`-fPIC`  :option:`-fpie`  :option:`-fPIE`  :option:`-fno-plt`
  :option:`-fno-jump-tables` :option:`-fno-bit-tests`
  :option:`-frecord-gcc-switches`
  :option:`-freg-struct-return`  :option:`-fshort-enums`  :option:`-fshort-wchar`
  :option:`-fverbose-asm`  :option:`-fpack-struct`:samp:`[={n}]`
  :option:`-fleading-underscore`  :option:`-ftls-model`:samp:`={model}`
  :option:`-fstack-reuse`:samp:`={reuse_level}`
  :option:`-ftrampolines`  :option:`-ftrapv`  :option:`-fwrapv`
  :option:`-fvisibility`:samp:`=[default|internal|hidden|protected]`
  :option:`-fstrict-volatile-bitfields`  :option:`-fsync-libcalls`

*Developer Options*
  See :ref:`GCC Developer Options <developer-options>`.

  :option:`-d`:samp:`{letters}`  :option:`-dumpspecs`  :option:`-dumpmachine`  :option:`-dumpversion`
  :option:`-dumpfullversion`  :option:`-fcallgraph-info`:samp:`[=su,da]`
  :option:`-fchecking`  :option:`-fchecking`:samp:`={n}`
  :option:`-fdbg-cnt-list`   :option:`-fdbg-cnt`:samp:`={counter-value-list}`
  :option:`-fdisable-ipa-`:samp:`{pass_name}`
  :option:`-fdisable-rtl-`:samp:`{pass_name}`
  :option:`-fdisable-rtl-`:samp:`{pass-name}={range-list}`
  :option:`-fdisable-tree-`:samp:`{pass_name}`
  :option:`-fdisable-tree-`:samp:`{pass-name}={range-list}`
  :option:`-fdump-debug`  :option:`-fdump-earlydebug`
  :option:`-fdump-noaddr`  :option:`-fdump-unnumbered`  :option:`-fdump-unnumbered-links`
  :option:`-fdump-final-insns`:samp:`[={file}]`
  :option:`-fdump-ipa-all`  :option:`-fdump-ipa-cgraph`  :option:`-fdump-ipa-inline`
  :option:`-fdump-lang-all`
  :option:`-fdump-lang-`:samp:`{switch}`
  :option:`-fdump-lang-`:samp:`{switch}-{options}`
  :option:`-fdump-lang-`:samp:`{switch}-{options}={filename}`
  :option:`-fdump-passes`
  :option:`-fdump-rtl-`:samp:`{pass}`  :option:`-fdump-rtl-`:samp:`{pass}={filename}`
  :option:`-fdump-statistics`
  :option:`-fdump-tree-all`
  :option:`-fdump-tree-`:samp:`{switch}`
  :option:`-fdump-tree-`:samp:`{switch}-{options}`
  :option:`-fdump-tree-`:samp:`{switch}-{options}={filename}`
  :option:`-fcompare-debug`:samp:`[={opts}]`  :option:`-fcompare-debug-second`
  :option:`-fenable-`:samp:`{kind}-{pass}`
  :option:`-fenable-`:samp:`{kind}-{pass}={range-list}`
  :option:`-fira-verbose`:samp:`={n}`
  :option:`-flto-report`  :option:`-flto-report-wpa`  :option:`-fmem-report-wpa`
  :option:`-fmem-report`  :option:`-fpre-ipa-mem-report`  :option:`-fpost-ipa-mem-report`
  :option:`-fopt-info`  :option:`-fopt-info-`:samp:`{options}:[={file}]`
  :option:`-fprofile-report`
  :option:`-frandom-seed`:samp:`={string}`  :option:`-fsched-verbose`:samp:`={n}`
  :option:`-fsel-sched-verbose`  :option:`-fsel-sched-dump-cfg`  :option:`-fsel-sched-pipelining-verbose`
  :option:`-fstats`  :option:`-fstack-usage`  :option:`-ftime-report`  :option:`-ftime-report-details`
  :option:`-fvar-tracking-assignments-toggle`  :option:`-gtoggle`
  :option:`-print-file-name`:samp:`={library}`  :option:`-print-libgcc-file-name`
  :option:`-print-multi-directory`  :option:`-print-multi-lib`  :option:`-print-multi-os-directory`
  :option:`-print-prog-name`:samp:`={program}`  :option:`-print-search-dirs`  :option:`-Q`
  :option:`-print-sysroot`  :option:`-print-sysroot-headers-suffix`
  :option:`-save-temps`  :option:`-save-temps`:samp:`=cwd`  :option:`-save-temps`:samp:`=obj`  :option:`-time`:samp:`[={file}]`

*Machine-Dependent Options*
  See :ref:`Machine-Dependent Options <submodel-options>`.

  .. This list is ordered alphanumerically by subsection name.

  .. Try and put the significant identifier (CPU or system) first,

  .. so users have a clue at guessing where the ones they want will be.

  *AArch64 Options*

  :option:`-mabi`:samp:`={name}`  :option:`-mbig-endian`  :option:`-mlittle-endian`
  :option:`-mgeneral-regs-only`
  :option:`-mcmodel`:samp:`=tiny`  :option:`-mcmodel`:samp:`=small`  :option:`-mcmodel`:samp:`=large`
  :option:`-mstrict-align`  :option:`-mno-strict-align`
  :option:`-momit-leaf-frame-pointer`
  :option:`-mtls-dialect`:samp:`=desc`  :option:`-mtls-dialect`:samp:`=traditional`
  :option:`-mtls-size`:samp:`={size}`
  :option:`-mfix-cortex-a53-835769`  :option:`-mfix-cortex-a53-843419`
  :option:`-mlow-precision-recip-sqrt`  :option:`-mlow-precision-sqrt`  :option:`-mlow-precision-div`
  :option:`-mpc-relative-literal-loads`
  :option:`-msign-return-address`:samp:`={scope}`
  :option:`-mbranch-protection`:samp:`={none}|{standard}|{pac-ret}[+{leaf}+{b-key}|{bti}`
  :option:`-mharden-sls`:samp:`={opts}`
  :option:`-march`:samp:`={name}`  :option:`-mcpu`:samp:`={name}`  :option:`-mtune`:samp:`={name}`
  :option:`-moverride`:samp:`={string}`  :option:`-mverbose-cost-dump`
  :option:`-mstack-protector-guard`:samp:`={guard}` :option:`-mstack-protector-guard-reg`:samp:`={sysreg}`
  :option:`-mstack-protector-guard-offset`:samp:`={offset}` :option:`-mtrack-speculation`
  :option:`-moutline-atomics`

  *Adapteva Epiphany Options*

  :option:`-mhalf-reg-file`  :option:`-mprefer-short-insn-regs`
  :option:`-mbranch-cost`:samp:`={num}`  :option:`-mcmove`  :option:`-mnops`:samp:`={num}`  :option:`-msoft-cmpsf`
  :option:`-msplit-lohi`  :option:`-mpost-inc`  :option:`-mpost-modify`  :option:`-mstack-offset`:samp:`={num}`
  :option:`-mround-nearest`  :option:`-mlong-calls`  :option:`-mshort-calls`  :option:`-msmall16`
  :option:`-mfp-mode`:samp:`={mode}`  :option:`-mvect-double`  :option:`-max-vect-align`:samp:`={num}`
  :option:`-msplit-vecmove-early`  :option:`-m1reg-`:samp:`{reg}`

  *AMD GCN Options*

  :option:`-march`:samp:`={gpu}` :option:`-mtune`:samp:`={gpu}` :option:`-mstack-size`:samp:`={bytes}`

  *ARC Options*

  :option:`-mbarrel-shifter`  :option:`-mjli-always`
  :option:`-mcpu`:samp:`={cpu}`  :option:`-mA6`  :option:`-mARC600`  :option:`-mA7`  :option:`-mARC700`
  :option:`-mdpfp`  :option:`-mdpfp-compact`  :option:`-mdpfp-fast`  :option:`-mno-dpfp-lrsr`
  :option:`-mea`  :option:`-mno-mpy`  :option:`-mmul32x16`  :option:`-mmul64`  :option:`-matomic`
  :option:`-mnorm`  :option:`-mspfp`  :option:`-mspfp-compact`  :option:`-mspfp-fast`  :option:`-msimd`  :option:`-msoft-float`  :option:`-mswap`
  :option:`-mcrc`  :option:`-mdsp-packa`  :option:`-mdvbf`  :option:`-mlock`  :option:`-mmac-d16`  :option:`-mmac-24`  :option:`-mrtsc`  :option:`-mswape`
  :option:`-mtelephony`  :option:`-mxy`  :option:`-misize`  :option:`-mannotate-align`  :option:`-marclinux`  :option:`-marclinux_prof`
  :option:`-mlong-calls`  :option:`-mmedium-calls`  :option:`-msdata`  :option:`-mirq-ctrl-saved`
  :option:`-mrgf-banked-regs`  :option:`-mlpc-width`:samp:`={width}`  :option:`-G` :samp:`{num}`
  :option:`-mvolatile-cache`  :option:`-mtp-regno`:samp:`={regno}`
  :option:`-malign-call`  :option:`-mauto-modify-reg`  :option:`-mbbit-peephole`  :option:`-mno-brcc`
  :option:`-mcase-vector-pcrel`  :option:`-mcompact-casesi`  :option:`-mno-cond-exec`  :option:`-mearly-cbranchsi`
  :option:`-mexpand-adddi`  :option:`-mindexed-loads`  :option:`-mlra`  :option:`-mlra-priority-none`
  :option:`-mlra-priority-compact` mlra :option:`-priority-noncompact`  :option:`-mmillicode`
  :option:`-mmixed-code`  :option:`-mq-class`  :option:`-mRcq`  :option:`-mRcw`  :option:`-msize-level`:samp:`={level}`
  :option:`-mtune`:samp:`={cpu}`  :option:`-mmultcost`:samp:`={num}`  :option:`-mcode-density-frame`
  :option:`-munalign-prob-threshold`:samp:`={probability}`  :option:`-mmpy-option`:samp:`={multo}`
  :option:`-mdiv-rem`  :option:`-mcode-density`  :option:`-mll64`  :option:`-mfpu`:samp:`={fpu}`  :option:`-mrf16`  :option:`-mbranch-index`

  *ARM Options*

  :option:`-mapcs-frame`  :option:`-mno-apcs-frame`
  :option:`-mabi`:samp:`={name}`
  :option:`-mapcs-stack-check`  :option:`-mno-apcs-stack-check`
  :option:`-mapcs-reentrant`  :option:`-mno-apcs-reentrant`
  :option:`-mgeneral-regs-only`
  :option:`-msched-prolog`  :option:`-mno-sched-prolog`
  :option:`-mlittle-endian`  :option:`-mbig-endian`
  :option:`-mbe8`  :option:`-mbe32`
  :option:`-mfloat-abi`:samp:`={name}`
  :option:`-mfp16-format`:samp:`={name}`
  :option:`-mthumb-interwork`  :option:`-mno-thumb-interwork`
  :option:`-mcpu`:samp:`={name}`  :option:`-march`:samp:`={name}`  :option:`-mfpu`:samp:`={name}`
  :option:`-mtune`:samp:`={name}`  :option:`-mprint-tune-info`
  :option:`-mstructure-size-boundary`:samp:`={n}`
  :option:`-mabort-on-noreturn`
  :option:`-mlong-calls`  :option:`-mno-long-calls`
  :option:`-msingle-pic-base`  :option:`-mno-single-pic-base`
  :option:`-mpic-register`:samp:`={reg}`
  :option:`-mnop-fun-dllimport`
  :option:`-mpoke-function-name`
  :option:`-mthumb`  :option:`-marm`  :option:`-mflip-thumb`
  :option:`-mtpcs-frame`  :option:`-mtpcs-leaf-frame`
  :option:`-mcaller-super-interworking`  :option:`-mcallee-super-interworking`
  :option:`-mtp`:samp:`={name}`  :option:`-mtls-dialect`:samp:`={dialect}`
  :option:`-mword-relocations`
  :option:`-mfix-cortex-m3-ldrd`
  :option:`-munaligned-access`
  :option:`-mneon-for-64bits`
  :option:`-mslow-flash-data`
  :option:`-masm-syntax-unified`
  :option:`-mrestrict-it`
  :option:`-mverbose-cost-dump`
  :option:`-mpure-code`
  :option:`-mcmse`
  :option:`-mfdpic`

  *AVR Options*

  :option:`-mmcu`:samp:`={mcu}`  :option:`-mabsdata`  :option:`-maccumulate-args`
  :option:`-mbranch-cost`:samp:`={cost}`
  :option:`-mcall-prologues`  :option:`-mgas-isr-prologues`  :option:`-mint8`
  :option:`-mdouble`:samp:`={bits}` :option:`-mlong-double`:samp:`={bits}`
  :option:`-mn_flash`:samp:`={size}`  :option:`-mno-interrupts`
  :option:`-mmain-is-OS_task`  :option:`-mrelax`  :option:`-mrmw`  :option:`-mstrict-X`  :option:`-mtiny-stack`
  :option:`-mfract-convert-truncate`
  :option:`-mshort-calls`  :option:`-nodevicelib`  :option:`-nodevicespecs`
  :option:`-Waddr-space-convert`  :option:`-Wmisspelled-isr`

  *Blackfin Options*

  :option:`-mcpu`:samp:`={cpu}[-{sirevision}]`
  :option:`-msim`  :option:`-momit-leaf-frame-pointer`  :option:`-mno-omit-leaf-frame-pointer`
  :option:`-mspecld-anomaly`  :option:`-mno-specld-anomaly`  :option:`-mcsync-anomaly`  :option:`-mno-csync-anomaly`
  :option:`-mlow-64k`  :option:`-mno-low64k`  :option:`-mstack-check-l1`  :option:`-mid-shared-library`
  :option:`-mno-id-shared-library`  :option:`-mshared-library-id`:samp:`={n}`
  :option:`-mleaf-id-shared-library`  :option:`-mno-leaf-id-shared-library`
  :option:`-msep-data`  :option:`-mno-sep-data`  :option:`-mlong-calls`  :option:`-mno-long-calls`
  :option:`-mfast-fp`  :option:`-minline-plt`  :option:`-mmulticore`  :option:`-mcorea`  :option:`-mcoreb`  :option:`-msdram`
  :option:`-micplb`

  *C6X Options*

  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-march`:samp:`={cpu}`
  :option:`-msim`  :option:`-msdata`:samp:`={sdata-type}`

  *CRIS Options*

  :option:`-mcpu`:samp:`={cpu}`  :option:`-march`:samp:`={cpu}`  :option:`-mtune`:samp:`={cpu}`
  :option:`-mmax-stack-frame`:samp:`={n}`  :option:`-melinux-stacksize`:samp:`={n}`
  :option:`-metrax4`  :option:`-metrax100`  :option:`-mpdebug`  :option:`-mcc-init`  :option:`-mno-side-effects`
  :option:`-mstack-align`  :option:`-mdata-align`  :option:`-mconst-align`
  :option:`-m32-bit`  :option:`-m16-bit`  :option:`-m8-bit`  :option:`-mno-prologue-epilogue`  :option:`-mno-gotplt`
  :option:`-melf`  :option:`-maout`  :option:`-melinux`  :option:`-mlinux`  :option:`-sim`  :option:`-sim2`
  :option:`-mmul-bug-workaround`  :option:`-mno-mul-bug-workaround`

  *CR16 Options*

  :option:`-mmac`
  :option:`-mcr16cplus`  :option:`-mcr16c`
  :option:`-msim`  :option:`-mint32`  :option:`-mbit-ops`
  :option:`-mdata-model`:samp:`={model}`

  *C-SKY Options*

  :option:`-march`:samp:`={arch}`  :option:`-mcpu`:samp:`={cpu}`
  :option:`-mbig-endian`  :option:`-EB`  :option:`-mlittle-endian`  :option:`-EL`
  :option:`-mhard-float`  :option:`-msoft-float`  :option:`-mfpu`:samp:`={fpu}`  :option:`-mdouble-float`  :option:`-mfdivdu`
  :option:`-mfloat-abi`:samp:`={name}`
  :option:`-melrw`  :option:`-mistack`  :option:`-mmp`  :option:`-mcp`  :option:`-mcache`  :option:`-msecurity`  :option:`-mtrust`
  :option:`-mdsp`  :option:`-medsp`  :option:`-mvdsp`
  :option:`-mdiv`  :option:`-msmart`  :option:`-mhigh-registers`  :option:`-manchor`
  :option:`-mpushpop`  :option:`-mmultiple-stld`  :option:`-mconstpool`  :option:`-mstack-size`  :option:`-mccrt`
  :option:`-mbranch-cost`:samp:`={n}`  :option:`-mcse-cc`  :option:`-msched-prolog` :option:`-msim`

  *Darwin Options*

  :option:`-all_load`  :option:`-allowable_client`  :option:`-arch`  :option:`-arch_errors_fatal`
  :option:`-arch_only`  :option:`-bind_at_load`  :option:`-bundle`  :option:`-bundle_loader`
  :option:`-client_name`  :option:`-compatibility_version`  :option:`-current_version`
  :option:`-dead_strip`
  :option:`-dependency-file`  :option:`-dylib_file`  :option:`-dylinker_install_name`
  :option:`-dynamic`  :option:`-dynamiclib`  :option:`-exported_symbols_list`
  :option:`-filelist`  :option:`-flat_namespace`  :option:`-force_cpusubtype_ALL`
  :option:`-force_flat_namespace`  :option:`-headerpad_max_install_names`
  :option:`-iframework`
  :option:`-image_base`  :option:`-init`  :option:`-install_name`  :option:`-keep_private_externs`
  :option:`-multi_module`  :option:`-multiply_defined`  :option:`-multiply_defined_unused`
  :option:`-noall_load`   :option:`-no_dead_strip_inits_and_terms`
  :option:`-nofixprebinding`  :option:`-nomultidefs`  :option:`-noprebind`  :option:`-noseglinkedit`
  :option:`-pagezero_size`  :option:`-prebind`  :option:`-prebind_all_twolevel_modules`
  :option:`-private_bundle`  :option:`-read_only_relocs`  :option:`-sectalign`
  :option:`-sectobjectsymbols`  :option:`-whyload`  :option:`-seg1addr`
  :option:`-sectcreate`  :option:`-sectobjectsymbols`  :option:`-sectorder`
  :option:`-segaddr`  :option:`-segs_read_only_addr`  :option:`-segs_read_write_addr`
  :option:`-seg_addr_table`  :option:`-seg_addr_table_filename`  :option:`-seglinkedit`
  :option:`-segprot`  :option:`-segs_read_only_addr`  :option:`-segs_read_write_addr`
  :option:`-single_module`  :option:`-static`  :option:`-sub_library`  :option:`-sub_umbrella`
  :option:`-twolevel_namespace`  :option:`-umbrella`  :option:`-undefined`
  :option:`-unexported_symbols_list`  :option:`-weak_reference_mismatches`
  :option:`-whatsloaded`  :option:`-F`  :option:`-gused`  :option:`-gfull`  :option:`-mmacosx-version-min`:samp:`={version}`
  :option:`-mkernel`  :option:`-mone-byte-bool`

  *DEC Alpha Options*

  :option:`-mno-fp-regs`  :option:`-msoft-float`
  :option:`-mieee`  :option:`-mieee-with-inexact`  :option:`-mieee-conformant`
  :option:`-mfp-trap-mode`:samp:`={mode}`  :option:`-mfp-rounding-mode`:samp:`={mode}`
  :option:`-mtrap-precision`:samp:`={mode}`  :option:`-mbuild-constants`
  :option:`-mcpu`:samp:`={cpu-type}`  :option:`-mtune`:samp:`={cpu-type}`
  :option:`-mbwx`  :option:`-mmax`  :option:`-mfix`  :option:`-mcix`
  :option:`-mfloat-vax`  :option:`-mfloat-ieee`
  :option:`-mexplicit-relocs`  :option:`-msmall-data`  :option:`-mlarge-data`
  :option:`-msmall-text`  :option:`-mlarge-text`
  :option:`-mmemory-latency`:samp:`={time}`

  *eBPF Options*

  :option:`-mbig-endian` :option:`-mlittle-endian` :option:`-mkernel`:samp:`={version}`
  :option:`-mframe-limit`:samp:`={bytes}` :option:`-mxbpf`

  *FR30 Options*

  :option:`-msmall-model`  :option:`-mno-lsim`

  *FT32 Options*

  :option:`-msim`  :option:`-mlra`  :option:`-mnodiv`  :option:`-mft32b`  :option:`-mcompress`  :option:`-mnopm`

  *FRV Options*

  :option:`-mgpr-32`  :option:`-mgpr-64`  :option:`-mfpr-32`  :option:`-mfpr-64`
  :option:`-mhard-float`  :option:`-msoft-float`
  :option:`-malloc-cc`  :option:`-mfixed-cc`  :option:`-mdword`  :option:`-mno-dword`
  :option:`-mdouble`  :option:`-mno-double`
  :option:`-mmedia`  :option:`-mno-media`  :option:`-mmuladd`  :option:`-mno-muladd`
  :option:`-mfdpic`  :option:`-minline-plt`  :option:`-mgprel-ro`  :option:`-multilib-library-pic`
  :option:`-mlinked-fp`  :option:`-mlong-calls`  :option:`-malign-labels`
  :option:`-mlibrary-pic`  :option:`-macc-4`  :option:`-macc-8`
  :option:`-mpack`  :option:`-mno-pack`  :option:`-mno-eflags`  :option:`-mcond-move`  :option:`-mno-cond-move`
  :option:`-moptimize-membar`  :option:`-mno-optimize-membar`
  :option:`-mscc`  :option:`-mno-scc`  :option:`-mcond-exec`  :option:`-mno-cond-exec`
  :option:`-mvliw-branch`  :option:`-mno-vliw-branch`
  :option:`-mmulti-cond-exec`  :option:`-mno-multi-cond-exec`  :option:`-mnested-cond-exec`
  :option:`-mno-nested-cond-exec`  :option:`-mtomcat-stats`
  :option:`-mTLS`  :option:`-mtls`
  :option:`-mcpu`:samp:`={cpu}`

  *GNU/Linux Options*

  :option:`-mglibc`  :option:`-muclibc`  :option:`-mmusl`  :option:`-mbionic`  :option:`-mandroid`
  :option:`-tno-android-cc`  :option:`-tno-android-ld`

  *H8/300 Options*

  :option:`-mrelax`  :option:`-mh`  :option:`-ms`  :option:`-mn`  :option:`-mexr`  :option:`-mno-exr`  :option:`-mint32`  :option:`-malign-300`

  *HPPA Options*

  :option:`-march`:samp:`={architecture-type}`
  :option:`-mcaller-copies`  :option:`-mdisable-fpregs`  :option:`-mdisable-indexing`
  :option:`-mfast-indirect-calls`  :option:`-mgas`  :option:`-mgnu-ld`   :option:`-mhp-ld`
  :option:`-mfixed-range`:samp:`={register-range}`
  :option:`-mjump-in-delay`  :option:`-mlinker-opt`  :option:`-mlong-calls`
  :option:`-mlong-load-store`  :option:`-mno-disable-fpregs`
  :option:`-mno-disable-indexing`  :option:`-mno-fast-indirect-calls`  :option:`-mno-gas`
  :option:`-mno-jump-in-delay`  :option:`-mno-long-load-store`
  :option:`-mno-portable-runtime`  :option:`-mno-soft-float`
  :option:`-mno-space-regs`  :option:`-msoft-float`  :option:`-mpa-risc-1-0`
  :option:`-mpa-risc-1-1`  :option:`-mpa-risc-2-0`  :option:`-mportable-runtime`
  :option:`-mschedule`:samp:`={cpu-type}`  :option:`-mspace-regs`  :option:`-msio`  :option:`-mwsio`
  :option:`-munix`:samp:`={unix-std}`  :option:`-nolibdld`  :option:`-static`  :option:`-threads`

  *IA-64 Options*

  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-mgnu-as`  :option:`-mgnu-ld`  :option:`-mno-pic`
  :option:`-mvolatile-asm-stop`  :option:`-mregister-names`  :option:`-msdata`  :option:`-mno-sdata`
  :option:`-mconstant-gp`  :option:`-mauto-pic`  :option:`-mfused-madd`
  :option:`-minline-float-divide-min-latency`
  :option:`-minline-float-divide-max-throughput`
  :option:`-mno-inline-float-divide`
  :option:`-minline-int-divide-min-latency`
  :option:`-minline-int-divide-max-throughput`
  :option:`-mno-inline-int-divide`
  :option:`-minline-sqrt-min-latency`  :option:`-minline-sqrt-max-throughput`
  :option:`-mno-inline-sqrt`
  :option:`-mdwarf2-asm`  :option:`-mearly-stop-bits`
  :option:`-mfixed-range`:samp:`={register-range}`  :option:`-mtls-size`:samp:`={tls-size}`
  :option:`-mtune`:samp:`={cpu-type}`  :option:`-milp32`  :option:`-mlp64`
  :option:`-msched-br-data-spec`  :option:`-msched-ar-data-spec`  :option:`-msched-control-spec`
  :option:`-msched-br-in-data-spec`  :option:`-msched-ar-in-data-spec`  :option:`-msched-in-control-spec`
  :option:`-msched-spec-ldc`  :option:`-msched-spec-control-ldc`
  :option:`-msched-prefer-non-data-spec-insns`  :option:`-msched-prefer-non-control-spec-insns`
  :option:`-msched-stop-bits-after-every-cycle`  :option:`-msched-count-spec-in-critical-path`
  :option:`-msel-sched-dont-check-control-spec`  :option:`-msched-fp-mem-deps-zero-cost`
  :option:`-msched-max-memory-insns-hard-limit`  :option:`-msched-max-memory-insns`:samp:`={max-insns}`

  *LM32 Options*

  :option:`-mbarrel-shift-enabled`  :option:`-mdivide-enabled`  :option:`-mmultiply-enabled`
  :option:`-msign-extend-enabled`  :option:`-muser-enabled`

  *M32R/D Options*

  :option:`-m32r2`  :option:`-m32rx`  :option:`-m32r`
  :option:`-mdebug`
  :option:`-malign-loops`  :option:`-mno-align-loops`
  :option:`-missue-rate`:samp:`={number}`
  :option:`-mbranch-cost`:samp:`={number}`
  :option:`-mmodel`:samp:`={code-size-model-type}`
  :option:`-msdata`:samp:`={sdata-type}`
  :option:`-mno-flush-func`  :option:`-mflush-func`:samp:`={name}`
  :option:`-mno-flush-trap`  :option:`-mflush-trap`:samp:`={number}`
  :option:`-G` :samp:`{num}`

  *M32C Options*

  :option:`-mcpu`:samp:`={cpu}`  :option:`-msim`  :option:`-memregs`:samp:`={number}`

  *M680x0 Options*

  :option:`-march`:samp:`={arch}`  :option:`-mcpu`:samp:`={cpu}`  :option:`-mtune`:samp:`={tune}`
  :option:`-m68000`  :option:`-m68020`  :option:`-m68020-40`  :option:`-m68020-60`  :option:`-m68030`  :option:`-m68040`
  :option:`-m68060`  :option:`-mcpu32`  :option:`-m5200`  :option:`-m5206e`  :option:`-m528x`  :option:`-m5307`  :option:`-m5407`
  :option:`-mcfv4e`  :option:`-mbitfield`  :option:`-mno-bitfield`  :option:`-mc68000`  :option:`-mc68020`
  :option:`-mnobitfield`  :option:`-mrtd`  :option:`-mno-rtd`  :option:`-mdiv`  :option:`-mno-div`  :option:`-mshort`
  :option:`-mno-short`  :option:`-mhard-float`  :option:`-m68881`  :option:`-msoft-float`  :option:`-mpcrel`
  :option:`-malign-int`  :option:`-mstrict-align`  :option:`-msep-data`  :option:`-mno-sep-data`
  :option:`-mshared-library-id`:samp:`=n`  :option:`-mid-shared-library`  :option:`-mno-id-shared-library`
  :option:`-mxgot`  :option:`-mno-xgot`  :option:`-mlong-jump-table-offsets`

  *MCore Options*

  :option:`-mhardlit`  :option:`-mno-hardlit`  :option:`-mdiv`  :option:`-mno-div`  :option:`-mrelax-immediates`
  :option:`-mno-relax-immediates`  :option:`-mwide-bitfields`  :option:`-mno-wide-bitfields`
  :option:`-m4byte-functions`  :option:`-mno-4byte-functions`  :option:`-mcallgraph-data`
  :option:`-mno-callgraph-data`  :option:`-mslow-bytes`  :option:`-mno-slow-bytes`  :option:`-mno-lsim`
  :option:`-mlittle-endian`  :option:`-mbig-endian`  :option:`-m210`  :option:`-m340`  :option:`-mstack-increment`

  *MeP Options*

  :option:`-mabsdiff`  :option:`-mall-opts`  :option:`-maverage`  :option:`-mbased`:samp:`={n}`  :option:`-mbitops`
  :option:`-mc`:samp:`={n}`  :option:`-mclip`  :option:`-mconfig`:samp:`={name}`  :option:`-mcop`  :option:`-mcop32`  :option:`-mcop64`  :option:`-mivc2`
  :option:`-mdc`  :option:`-mdiv`  :option:`-meb`  :option:`-mel`  :option:`-mio-volatile`  :option:`-ml`  :option:`-mleadz`  :option:`-mm`  :option:`-mminmax`
  :option:`-mmult`  :option:`-mno-opts`  :option:`-mrepeat`  :option:`-ms`  :option:`-msatur`  :option:`-msdram`  :option:`-msim`  :option:`-msimnovec`  :option:`-mtf`
  :option:`-mtiny`:samp:`={n}`

  *MicroBlaze Options*

  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-msmall-divides`  :option:`-mcpu`:samp:`={cpu}`
  :option:`-mmemcpy`  :option:`-mxl-soft-mul`  :option:`-mxl-soft-div`  :option:`-mxl-barrel-shift`
  :option:`-mxl-pattern-compare`  :option:`-mxl-stack-check`  :option:`-mxl-gp-opt`  :option:`-mno-clearbss`
  :option:`-mxl-multiply-high`  :option:`-mxl-float-convert`  :option:`-mxl-float-sqrt`
  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-mxl-reorder`  :option:`-mxl-mode-`:samp:`{app-model}`
  :option:`-mpic-data-is-text-relative`

  *MIPS Options*

  :option:`-EL`  :option:`-EB`  :option:`-march`:samp:`={arch}`  :option:`-mtune`:samp:`={arch}`
  :option:`-mips1`  :option:`-mips2`  :option:`-mips3`  :option:`-mips4`  :option:`-mips32`  :option:`-mips32r2`  :option:`-mips32r3`  :option:`-mips32r5`
  :option:`-mips32r6`  :option:`-mips64`  :option:`-mips64r2`  :option:`-mips64r3`  :option:`-mips64r5`  :option:`-mips64r6`
  :option:`-mips16`  :option:`-mno-mips16`  :option:`-mflip-mips16`
  :option:`-minterlink-compressed`  :option:`-mno-interlink-compressed`
  :option:`-minterlink-mips16`  :option:`-mno-interlink-mips16`
  :option:`-mabi`:samp:`={abi}`  :option:`-mabicalls`  :option:`-mno-abicalls`
  :option:`-mshared`  :option:`-mno-shared`  :option:`-mplt`  :option:`-mno-plt`  :option:`-mxgot`  :option:`-mno-xgot`
  :option:`-mgp32`  :option:`-mgp64`  :option:`-mfp32`  :option:`-mfpxx`  :option:`-mfp64`  :option:`-mhard-float`  :option:`-msoft-float`
  :option:`-mno-float`  :option:`-msingle-float`  :option:`-mdouble-float`
  :option:`-modd-spreg`  :option:`-mno-odd-spreg`
  :option:`-mabs`:samp:`={mode}`  :option:`-mnan`:samp:`={encoding}`
  :option:`-mdsp`  :option:`-mno-dsp`  :option:`-mdspr2`  :option:`-mno-dspr2`
  :option:`-mmcu`  :option:`-mmno-mcu`
  :option:`-meva`  :option:`-mno-eva`
  :option:`-mvirt`  :option:`-mno-virt`
  :option:`-mxpa`  :option:`-mno-xpa`
  :option:`-mcrc`  :option:`-mno-crc`
  :option:`-mginv`  :option:`-mno-ginv`
  :option:`-mmicromips`  :option:`-mno-micromips`
  :option:`-mmsa`  :option:`-mno-msa`
  :option:`-mloongson-mmi`  :option:`-mno-loongson-mmi`
  :option:`-mloongson-ext`  :option:`-mno-loongson-ext`
  :option:`-mloongson-ext2`  :option:`-mno-loongson-ext2`
  :option:`-mfpu`:samp:`={fpu-type}`
  :option:`-msmartmips`  :option:`-mno-smartmips`
  :option:`-mpaired-single`  :option:`-mno-paired-single`  :option:`-mdmx`  :option:`-mno-mdmx`
  :option:`-mips3d`  :option:`-mno-mips3d`  :option:`-mmt`  :option:`-mno-mt`  :option:`-mllsc`  :option:`-mno-llsc`
  :option:`-mlong64`  :option:`-mlong32`  :option:`-msym32`  :option:`-mno-sym32`
  :option:`-G`:samp:`{num}`  :option:`-mlocal-sdata`  :option:`-mno-local-sdata`
  :option:`-mextern-sdata`  :option:`-mno-extern-sdata`  :option:`-mgpopt`  :option:`-mno-gopt`
  :option:`-membedded-data`  :option:`-mno-embedded-data`
  :option:`-muninit-const-in-rodata`  :option:`-mno-uninit-const-in-rodata`
  :option:`-mcode-readable`:samp:`={setting}`
  :option:`-msplit-addresses`  :option:`-mno-split-addresses`
  :option:`-mexplicit-relocs`  :option:`-mno-explicit-relocs`
  :option:`-mcheck-zero-division`  :option:`-mno-check-zero-division`
  :option:`-mdivide-traps`  :option:`-mdivide-breaks`
  :option:`-mload-store-pairs`  :option:`-mno-load-store-pairs`
  :option:`-mmemcpy`  :option:`-mno-memcpy`  :option:`-mlong-calls`  :option:`-mno-long-calls`
  :option:`-mmad`  :option:`-mno-mad`  :option:`-mimadd`  :option:`-mno-imadd`  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-nocpp`
  :option:`-mfix-24k`  :option:`-mno-fix-24k`
  :option:`-mfix-r4000`  :option:`-mno-fix-r4000`  :option:`-mfix-r4400`  :option:`-mno-fix-r4400`
  :option:`-mfix-r5900`  :option:`-mno-fix-r5900`
  :option:`-mfix-r10000`  :option:`-mno-fix-r10000`  :option:`-mfix-rm7000`  :option:`-mno-fix-rm7000`
  :option:`-mfix-vr4120`  :option:`-mno-fix-vr4120`
  :option:`-mfix-vr4130`  :option:`-mno-fix-vr4130`  :option:`-mfix-sb1`  :option:`-mno-fix-sb1`
  :option:`-mflush-func`:samp:`={func}`  :option:`-mno-flush-func`
  :option:`-mbranch-cost`:samp:`={num}`  :option:`-mbranch-likely`  :option:`-mno-branch-likely`
  :option:`-mcompact-branches`:samp:`={policy}`
  :option:`-mfp-exceptions`  :option:`-mno-fp-exceptions`
  :option:`-mvr4130-align`  :option:`-mno-vr4130-align`  :option:`-msynci`  :option:`-mno-synci`
  :option:`-mlxc1-sxc1`  :option:`-mno-lxc1-sxc1`  :option:`-mmadd4`  :option:`-mno-madd4`
  :option:`-mrelax-pic-calls`  :option:`-mno-relax-pic-calls`  :option:`-mmcount-ra-address`
  :option:`-mframe-header-opt`  :option:`-mno-frame-header-opt`

  *MMIX Options*

  :option:`-mlibfuncs`  :option:`-mno-libfuncs`  :option:`-mepsilon`  :option:`-mno-epsilon`  :option:`-mabi`:samp:`=gnu`
  :option:`-mabi`:samp:`=mmixware`  :option:`-mzero-extend`  :option:`-mknuthdiv`  :option:`-mtoplevel-symbols`
  :option:`-melf`  :option:`-mbranch-predict`  :option:`-mno-branch-predict`  :option:`-mbase-addresses`
  :option:`-mno-base-addresses`  :option:`-msingle-exit`  :option:`-mno-single-exit`

  *MN10300 Options*

  :option:`-mmult-bug`  :option:`-mno-mult-bug`
  :option:`-mno-am33`  :option:`-mam33`  :option:`-mam33-2`  :option:`-mam34`
  :option:`-mtune`:samp:`={cpu-type}`
  :option:`-mreturn-pointer-on-d0`
  :option:`-mno-crt0`  :option:`-mrelax`  :option:`-mliw`  :option:`-msetlb`

  *Moxie Options*

  :option:`-meb`  :option:`-mel`  :option:`-mmul.x`  :option:`-mno-crt0`

  *MSP430 Options*

  :option:`-msim`  :option:`-masm-hex`  :option:`-mmcu` =  :option:`-mcpu` =  :option:`-mlarge`  :option:`-msmall`  :option:`-mrelax`
  :option:`-mwarn-mcu`
  :option:`-mcode-region` :option:`-mdata-region`
  :option:`-msilicon-errata` :option:`-msilicon-errata-warn`
  :option:`-mhwmult` :option:`-minrt`  :option:`-mtiny-printf`  :option:`-mmax-inline-shift`

  *NDS32 Options*

  :option:`-mbig-endian`  :option:`-mlittle-endian`
  :option:`-mreduced-regs`  :option:`-mfull-regs`
  :option:`-mcmov`  :option:`-mno-cmov`
  :option:`-mext-perf`  :option:`-mno-ext-perf`
  :option:`-mext-perf2`  :option:`-mno-ext-perf2`
  :option:`-mext-string`  :option:`-mno-ext-string`
  :option:`-mv3push`  :option:`-mno-v3push`
  :option:`-m16bit`  :option:`-mno-16bit`
  :option:`-misr-vector-size`:samp:`={num}`
  :option:`-mcache-block-size`:samp:`={num}`
  :option:`-march`:samp:`={arch}`
  :option:`-mcmodel`:samp:`={code-model}`
  :option:`-mctor-dtor`  :option:`-mrelax`

  *Nios II Options*

  :option:`-G` :samp:`{num}`  :option:`-mgpopt`:samp:`={option}`  :option:`-mgpopt`  :option:`-mno-gpopt`
  :option:`-mgprel-sec`:samp:`={regexp}`  :option:`-mr0rel-sec`:samp:`={regexp}`
  :option:`-mel`  :option:`-meb`
  :option:`-mno-bypass-cache`  :option:`-mbypass-cache`
  :option:`-mno-cache-volatile`  :option:`-mcache-volatile`
  :option:`-mno-fast-sw-div`  :option:`-mfast-sw-div`
  :option:`-mhw-mul`  :option:`-mno-hw-mul`  :option:`-mhw-mulx`  :option:`-mno-hw-mulx`  :option:`-mno-hw-div`  :option:`-mhw-div`
  :option:`-mcustom-`:samp:`{insn}={N}`  :option:`-mno-custom-`:samp:`{insn}`
  :option:`-mcustom-fpu-cfg`:samp:`={name}`
  :option:`-mhal`  :option:`-msmallc`  :option:`-msys-crt0`:samp:`={name}`  :option:`-msys-lib`:samp:`={name}`
  :option:`-march`:samp:`={arch}`  :option:`-mbmx`  :option:`-mno-bmx`  :option:`-mcdx`  :option:`-mno-cdx`

  *Nvidia PTX Options*

  :option:`-m64`  :option:`-mmainkernel`  :option:`-moptimize`

  *OpenRISC Options*

  :option:`-mboard`:samp:`={name}`  :option:`-mnewlib`  :option:`-mhard-mul`  :option:`-mhard-div`
  :option:`-msoft-mul`  :option:`-msoft-div`
  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-mdouble-float` :option:`-munordered-float`
  :option:`-mcmov`  :option:`-mror`  :option:`-mrori`  :option:`-msext`  :option:`-msfimm`  :option:`-mshftimm`

  *PDP-11 Options*

  :option:`-mfpu`  :option:`-msoft-float`  :option:`-mac0`  :option:`-mno-ac0`  :option:`-m40`  :option:`-m45`  :option:`-m10`
  :option:`-mint32`  :option:`-mno-int16`  :option:`-mint16`  :option:`-mno-int32`
  :option:`-msplit`  :option:`-munix-asm`  :option:`-mdec-asm`  :option:`-mgnu-asm`  :option:`-mlra`

  *picoChip Options*

  :option:`-mae`:samp:`={ae_type}`  :option:`-mvliw-lookahead`:samp:`={N}`
  :option:`-msymbol-as-address`  :option:`-mno-inefficient-warnings`

  *PowerPC Options*
  See RS/6000 and PowerPC Options.

  *PRU Options*

  :option:`-mmcu`:samp:`={mcu}`  :option:`-minrt`  :option:`-mno-relax`  :option:`-mloop`
  :option:`-mabi`:samp:`={variant}`

  *RISC-V Options*

  :option:`-mbranch-cost`:samp:`={N-instruction}`
  :option:`-mplt`  :option:`-mno-plt`
  :option:`-mabi`:samp:`={ABI-string}`
  :option:`-mfdiv`  :option:`-mno-fdiv`
  :option:`-mdiv`  :option:`-mno-div`
  :option:`-march`:samp:`={ISA-string}`
  :option:`-mtune`:samp:`={processor-string}`
  :option:`-mpreferred-stack-boundary`:samp:`={num}`
  :option:`-msmall-data-limit`:samp:`={N-bytes}`
  :option:`-msave-restore`  :option:`-mno-save-restore`
  :option:`-mshorten-memrefs`  :option:`-mno-shorten-memrefs`
  :option:`-mstrict-align`  :option:`-mno-strict-align`
  :option:`-mcmodel`:samp:`=medlow`  :option:`-mcmodel`:samp:`=medany`
  :option:`-mexplicit-relocs`  :option:`-mno-explicit-relocs`
  :option:`-mrelax`  :option:`-mno-relax`
  :option:`-mriscv-attribute`  :option:`-mmo-riscv-attribute`
  :option:`-malign-data`:samp:`={type}`
  :option:`-mbig-endian`  :option:`-mlittle-endian`
  :option:`-mstack-protector-guard`:samp:`={guard}` :option:`-mstack-protector-guard-reg`:samp:`={reg}`
  :option:`-mstack-protector-guard-offset`:samp:`={offset}`

  *RL78 Options*

  :option:`-msim`  :option:`-mmul`:samp:`=none`  :option:`-mmul`:samp:`=g13`  :option:`-mmul`:samp:`=g14`  :option:`-mallregs`
  :option:`-mcpu`:samp:`=g10`  :option:`-mcpu`:samp:`=g13`  :option:`-mcpu`:samp:`=g14`  :option:`-mg10`  :option:`-mg13`  :option:`-mg14`
  :option:`-m64bit-doubles`  :option:`-m32bit-doubles`  :option:`-msave-mduc-in-interrupts`

  *RS/6000 and PowerPC Options*

  :option:`-mcpu`:samp:`={cpu-type}`
  :option:`-mtune`:samp:`={cpu-type}`
  :option:`-mcmodel`:samp:`={code-model}`
  :option:`-mpowerpc64`
  :option:`-maltivec`  :option:`-mno-altivec`
  :option:`-mpowerpc-gpopt`  :option:`-mno-powerpc-gpopt`
  :option:`-mpowerpc-gfxopt`  :option:`-mno-powerpc-gfxopt`
  :option:`-mmfcrf`  :option:`-mno-mfcrf`  :option:`-mpopcntb`  :option:`-mno-popcntb`  :option:`-mpopcntd`  :option:`-mno-popcntd`
  :option:`-mfprnd`  :option:`-mno-fprnd`
  :option:`-mcmpb`  :option:`-mno-cmpb`  :option:`-mhard-dfp`  :option:`-mno-hard-dfp`
  :option:`-mfull-toc`   :option:`-mminimal-toc`  :option:`-mno-fp-in-toc`  :option:`-mno-sum-in-toc`
  :option:`-m64`  :option:`-m32`  :option:`-mxl-compat`  :option:`-mno-xl-compat`  :option:`-mpe`
  :option:`-malign-power`  :option:`-malign-natural`
  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-mmultiple`  :option:`-mno-multiple`
  :option:`-mupdate`  :option:`-mno-update`
  :option:`-mavoid-indexed-addresses`  :option:`-mno-avoid-indexed-addresses`
  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-mbit-align`  :option:`-mno-bit-align`
  :option:`-mstrict-align`  :option:`-mno-strict-align`  :option:`-mrelocatable`
  :option:`-mno-relocatable`  :option:`-mrelocatable-lib`  :option:`-mno-relocatable-lib`
  :option:`-mtoc`  :option:`-mno-toc`  :option:`-mlittle`  :option:`-mlittle-endian`  :option:`-mbig`  :option:`-mbig-endian`
  :option:`-mdynamic-no-pic`  :option:`-mswdiv`  :option:`-msingle-pic-base`
  :option:`-mprioritize-restricted-insns`:samp:`={priority}`
  :option:`-msched-costly-dep`:samp:`={dependence_type}`
  :option:`-minsert-sched-nops`:samp:`={scheme}`
  :option:`-mcall-aixdesc`  :option:`-mcall-eabi`  :option:`-mcall-freebsd`
  :option:`-mcall-linux`  :option:`-mcall-netbsd`  :option:`-mcall-openbsd`
  :option:`-mcall-sysv`  :option:`-mcall-sysv-eabi`  :option:`-mcall-sysv-noeabi`
  :option:`-mtraceback`:samp:`={traceback_type}`
  :option:`-maix-struct-return`  :option:`-msvr4-struct-return`
  :option:`-mabi`:samp:`={abi-type}`  :option:`-msecure-plt`  :option:`-mbss-plt`
  :option:`-mlongcall`  :option:`-mno-longcall`  :option:`-mpltseq`  :option:`-mno-pltseq`
  :option:`-mblock-move-inline-limit`:samp:`={num}`
  :option:`-mblock-compare-inline-limit`:samp:`={num}`
  :option:`-mblock-compare-inline-loop-limit`:samp:`={num}`
  :option:`-mno-block-ops-unaligned-vsx`
  :option:`-mstring-compare-inline-limit`:samp:`={num}`
  :option:`-misel`  :option:`-mno-isel`
  :option:`-mvrsave`  :option:`-mno-vrsave`
  :option:`-mmulhw`  :option:`-mno-mulhw`
  :option:`-mdlmzb`  :option:`-mno-dlmzb`
  :option:`-mprototype`  :option:`-mno-prototype`
  :option:`-msim`  :option:`-mmvme`  :option:`-mads`  :option:`-myellowknife`  :option:`-memb`  :option:`-msdata`
  :option:`-msdata`:samp:`={opt}`  :option:`-mreadonly-in-sdata`  :option:`-mvxworks`  :option:`-G` :samp:`{num}`
  :option:`-mrecip`  :option:`-mrecip`:samp:`={opt}`  :option:`-mno-recip`  :option:`-mrecip-precision`
  :option:`-mno-recip-precision`
  :option:`-mveclibabi`:samp:`={type}`  :option:`-mfriz`  :option:`-mno-friz`
  :option:`-mpointers-to-nested-functions`  :option:`-mno-pointers-to-nested-functions`
  :option:`-msave-toc-indirect`  :option:`-mno-save-toc-indirect`
  :option:`-mpower8-fusion`  :option:`-mno-mpower8-fusion`  :option:`-mpower8-vector`  :option:`-mno-power8-vector`
  :option:`-mcrypto`  :option:`-mno-crypto`  :option:`-mhtm`  :option:`-mno-htm`
  :option:`-mquad-memory`  :option:`-mno-quad-memory`
  :option:`-mquad-memory-atomic`  :option:`-mno-quad-memory-atomic`
  :option:`-mcompat-align-parm`  :option:`-mno-compat-align-parm`
  :option:`-mfloat128`  :option:`-mno-float128`  :option:`-mfloat128-hardware`  :option:`-mno-float128-hardware`
  :option:`-mgnu-attribute`  :option:`-mno-gnu-attribute`
  :option:`-mstack-protector-guard`:samp:`={guard}` :option:`-mstack-protector-guard-reg`:samp:`={reg}`
  :option:`-mstack-protector-guard-offset`:samp:`={offset}` :option:`-mprefixed` :option:`-mno-prefixed`
  :option:`-mpcrel` :option:`-mno-pcrel` :option:`-mmma` :option:`-mno-mmma` :option:`-mrop-protect` :option:`-mno-rop-protect`
  :option:`-mprivileged` :option:`-mno-privileged`

  *RX Options*

  :option:`-m64bit-doubles`  :option:`-m32bit-doubles`  :option:`-fpu`  :option:`-nofpu`
  :option:`-mcpu`
  :option:`-mbig-endian-data`  :option:`-mlittle-endian-data`
  :option:`-msmall-data`
  :option:`-msim`  :option:`-mno-sim`
  :option:`-mas100-syntax`  :option:`-mno-as100-syntax`
  :option:`-mrelax`
  :option:`-mmax-constant-size`
  :option:`-mint-register`
  :option:`-mpid`
  :option:`-mallow-string-insns`  :option:`-mno-allow-string-insns`
  :option:`-mjsr`
  :option:`-mno-warn-multiple-fast-interrupts`
  :option:`-msave-acc-in-interrupts`

  *S/390 and zSeries Options*

  :option:`-mtune`:samp:`={cpu-type}`  :option:`-march`:samp:`={cpu-type}`
  :option:`-mhard-float`  :option:`-msoft-float`  :option:`-mhard-dfp`  :option:`-mno-hard-dfp`
  :option:`-mlong-double-64`  :option:`-mlong-double-128`
  :option:`-mbackchain`  :option:`-mno-backchain`  :option:`-mpacked-stack`  :option:`-mno-packed-stack`
  :option:`-msmall-exec`  :option:`-mno-small-exec`  :option:`-mmvcle`  :option:`-mno-mvcle`
  :option:`-m64`  :option:`-m31`  :option:`-mdebug`  :option:`-mno-debug`  :option:`-mesa`  :option:`-mzarch`
  :option:`-mhtm`  :option:`-mvx`  :option:`-mzvector`
  :option:`-mtpf-trace`  :option:`-mno-tpf-trace`  :option:`-mtpf-trace-skip`  :option:`-mno-tpf-trace-skip`
  :option:`-mfused-madd`  :option:`-mno-fused-madd`
  :option:`-mwarn-framesize`  :option:`-mwarn-dynamicstack`  :option:`-mstack-size`  :option:`-mstack-guard`
  :option:`-mhotpatch`:samp:`={halfwords},{halfwords}`

  *Score Options*

  :option:`-meb`  :option:`-mel`
  :option:`-mnhwloop`
  :option:`-muls`
  :option:`-mmac`
  :option:`-mscore5`  :option:`-mscore5u`  :option:`-mscore7`  :option:`-mscore7d`

  *SH Options*

  :option:`-m1`  :option:`-m2`  :option:`-m2e`
  :option:`-m2a-nofpu`  :option:`-m2a-single-only`  :option:`-m2a-single`  :option:`-m2a`
  :option:`-m3`  :option:`-m3e`
  :option:`-m4-nofpu`  :option:`-m4-single-only`  :option:`-m4-single`  :option:`-m4`
  :option:`-m4a-nofpu`  :option:`-m4a-single-only`  :option:`-m4a-single`  :option:`-m4a`  :option:`-m4al`
  :option:`-mb`  :option:`-ml`  :option:`-mdalign`  :option:`-mrelax`
  :option:`-mbigtable`  :option:`-mfmovd`  :option:`-mrenesas`  :option:`-mno-renesas`  :option:`-mnomacsave`
  :option:`-mieee`  :option:`-mno-ieee`  :option:`-mbitops`  :option:`-misize`  :option:`-minline-ic_invalidate`  :option:`-mpadstruct`
  :option:`-mprefergot`  :option:`-musermode`  :option:`-multcost`:samp:`={number}`  :option:`-mdiv`:samp:`={strategy}`
  :option:`-mdivsi3_libfunc`:samp:`={name}`  :option:`-mfixed-range`:samp:`={register-range}`
  :option:`-maccumulate-outgoing-args`
  :option:`-matomic-model`:samp:`={atomic-model}`
  :option:`-mbranch-cost`:samp:`={num}`  :option:`-mzdcbranch`  :option:`-mno-zdcbranch`
  :option:`-mcbranch-force-delay-slot`
  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-mfsca`  :option:`-mno-fsca`  :option:`-mfsrra`  :option:`-mno-fsrra`
  :option:`-mpretend-cmove`  :option:`-mtas`

  *Solaris 2 Options*

  :option:`-mclear-hwcap`  :option:`-mno-clear-hwcap`  :option:`-mimpure-text`  :option:`-mno-impure-text`
  :option:`-pthreads`

  *SPARC Options*

  :option:`-mcpu`:samp:`={cpu-type}`
  :option:`-mtune`:samp:`={cpu-type}`
  :option:`-mcmodel`:samp:`={code-model}`
  :option:`-mmemory-model`:samp:`={mem-model}`
  :option:`-m32`  :option:`-m64`  :option:`-mapp-regs`  :option:`-mno-app-regs`
  :option:`-mfaster-structs`  :option:`-mno-faster-structs`  :option:`-mflat`  :option:`-mno-flat`
  :option:`-mfpu`  :option:`-mno-fpu`  :option:`-mhard-float`  :option:`-msoft-float`
  :option:`-mhard-quad-float`  :option:`-msoft-quad-float`
  :option:`-mstack-bias`  :option:`-mno-stack-bias`
  :option:`-mstd-struct-return`  :option:`-mno-std-struct-return`
  :option:`-munaligned-doubles`  :option:`-mno-unaligned-doubles`
  :option:`-muser-mode`  :option:`-mno-user-mode`
  :option:`-mv8plus`  :option:`-mno-v8plus`  :option:`-mvis`  :option:`-mno-vis`
  :option:`-mvis2`  :option:`-mno-vis2`  :option:`-mvis3`  :option:`-mno-vis3`
  :option:`-mvis4`  :option:`-mno-vis4`  :option:`-mvis4b`  :option:`-mno-vis4b`
  :option:`-mcbcond`  :option:`-mno-cbcond`  :option:`-mfmaf`  :option:`-mno-fmaf`  :option:`-mfsmuld`  :option:`-mno-fsmuld`
  :option:`-mpopc`  :option:`-mno-popc`  :option:`-msubxc`  :option:`-mno-subxc`
  :option:`-mfix-at697f`  :option:`-mfix-ut699`  :option:`-mfix-ut700`  :option:`-mfix-gr712rc`
  :option:`-mlra`  :option:`-mno-lra`

  *System V Options*

  :option:`-Qy`  :option:`-Qn`  :option:`-YP,`:samp:`{paths}`  :option:`-Ym,`:samp:`{dir}`

  *TILE-Gx Options*

  :option:`-mcpu`:samp:`=CPU`  :option:`-m32`  :option:`-m64`  :option:`-mbig-endian`  :option:`-mlittle-endian`
  :option:`-mcmodel`:samp:`={code-model}`

  *TILEPro Options*

  :option:`-mcpu`:samp:`={cpu}`  :option:`-m32`

  *V850 Options*

  :option:`-mlong-calls`  :option:`-mno-long-calls`  :option:`-mep`  :option:`-mno-ep`
  :option:`-mprolog-function`  :option:`-mno-prolog-function`  :option:`-mspace`
  :option:`-mtda`:samp:`={n}`  :option:`-msda`:samp:`={n}`  :option:`-mzda`:samp:`={n}`
  :option:`-mapp-regs`  :option:`-mno-app-regs`
  :option:`-mdisable-callt`  :option:`-mno-disable-callt`
  :option:`-mv850e2v3`  :option:`-mv850e2`  :option:`-mv850e1`  :option:`-mv850es`
  :option:`-mv850e`  :option:`-mv850`  :option:`-mv850e3v5`
  :option:`-mloop`
  :option:`-mrelax`
  :option:`-mlong-jumps`
  :option:`-msoft-float`
  :option:`-mhard-float`
  :option:`-mgcc-abi`
  :option:`-mrh850-abi`
  :option:`-mbig-switch`

  *VAX Options*

  :option:`-mg`  :option:`-mgnu`  :option:`-munix`

  *Visium Options*

  :option:`-mdebug`  :option:`-msim`  :option:`-mfpu`  :option:`-mno-fpu`  :option:`-mhard-float`  :option:`-msoft-float`
  :option:`-mcpu`:samp:`={cpu-type}`  :option:`-mtune`:samp:`={cpu-type}`  :option:`-msv-mode`  :option:`-muser-mode`

  *VMS Options*

  :option:`-mvms-return-codes`  :option:`-mdebug-main`:samp:`={prefix}`  :option:`-mmalloc64`
  :option:`-mpointer-size`:samp:`={size}`

  *VxWorks Options*

  :option:`-mrtp`  :option:`-non-static`  :option:`-Bstatic`  :option:`-Bdynamic`
  :option:`-Xbind-lazy`  :option:`-Xbind-now`

  *x86 Options*

  :option:`-mtune`:samp:`={cpu-type}`  :option:`-march`:samp:`={cpu-type}`
  :option:`-mtune-ctrl`:samp:`={feature-list}`  :option:`-mdump-tune-features`  :option:`-mno-default`
  :option:`-mfpmath`:samp:`={unit}`
  :option:`-masm`:samp:`={dialect}`  :option:`-mno-fancy-math-387`
  :option:`-mno-fp-ret-in-387`  :option:`-m80387`  :option:`-mhard-float`  :option:`-msoft-float`
  :option:`-mno-wide-multiply`  :option:`-mrtd`  :option:`-malign-double`
  :option:`-mpreferred-stack-boundary`:samp:`={num}`
  :option:`-mincoming-stack-boundary`:samp:`={num}`
  :option:`-mcld`  :option:`-mcx16`  :option:`-msahf`  :option:`-mmovbe`  :option:`-mcrc32` :option:`-mmwait`
  :option:`-mrecip`  :option:`-mrecip`:samp:`={opt}`
  :option:`-mvzeroupper`  :option:`-mprefer-avx128`  :option:`-mprefer-vector-width`:samp:`={opt}`
  :option:`-mmmx`  :option:`-msse`  :option:`-msse2`  :option:`-msse3`  :option:`-mssse3`  :option:`-msse4.1`  :option:`-msse4.2`  :option:`-msse4`  :option:`-mavx`
  :option:`-mavx2`  :option:`-mavx512f`  :option:`-mavx512pf`  :option:`-mavx512er`  :option:`-mavx512cd`  :option:`-mavx512vl`
  :option:`-mavx512bw`  :option:`-mavx512dq`  :option:`-mavx512ifma`  :option:`-mavx512vbmi`  :option:`-msha`  :option:`-maes`
  :option:`-mpclmul`  :option:`-mfsgsbase`  :option:`-mrdrnd`  :option:`-mf16c`  :option:`-mfma`  :option:`-mpconfig`  :option:`-mwbnoinvd`
  :option:`-mptwrite`  :option:`-mprefetchwt1`  :option:`-mclflushopt`  :option:`-mclwb`  :option:`-mxsavec`  :option:`-mxsaves`
  :option:`-msse4a`  :option:`-m3dnow`  :option:`-m3dnowa`  :option:`-mpopcnt`  :option:`-mabm`  :option:`-mbmi`  :option:`-mtbm`  :option:`-mfma4`  :option:`-mxop`
  :option:`-madx`  :option:`-mlzcnt`  :option:`-mbmi2`  :option:`-mfxsr`  :option:`-mxsave`  :option:`-mxsaveopt`  :option:`-mrtm`  :option:`-mhle`  :option:`-mlwp`
  :option:`-mmwaitx`  :option:`-mclzero`  :option:`-mpku`  :option:`-mthreads`  :option:`-mgfni`  :option:`-mvaes`  :option:`-mwaitpkg`
  :option:`-mshstk` :option:`-mmanual-endbr` :option:`-mforce-indirect-call`  :option:`-mavx512vbmi2` :option:`-mavx512bf16` :option:`-menqcmd`
  :option:`-mvpclmulqdq`  :option:`-mavx512bitalg`  :option:`-mmovdiri`  :option:`-mmovdir64b`  :option:`-mavx512vpopcntdq`
  :option:`-mavx5124fmaps`  :option:`-mavx512vnni`  :option:`-mavx5124vnniw`  :option:`-mprfchw`  :option:`-mrdpid`
  :option:`-mrdseed`  :option:`-msgx` :option:`-mavx512vp2intersect` :option:`-mserialize` :option:`-mtsxldtrk`
  :option:`-mamx-tile`  :option:`-mamx-int8`  :option:`-mamx-bf16` :option:`-muintr` :option:`-mhreset` :option:`-mavxvnni`
  :option:`-mcldemote`  :option:`-mms-bitfields`  :option:`-mno-align-stringops`  :option:`-minline-all-stringops`
  :option:`-minline-stringops-dynamically`  :option:`-mstringop-strategy`:samp:`={alg}`
  :option:`-mkl` :option:`-mwidekl`
  :option:`-mmemcpy-strategy`:samp:`={strategy}`  :option:`-mmemset-strategy`:samp:`={strategy}`
  :option:`-mpush-args`  :option:`-maccumulate-outgoing-args`  :option:`-m128bit-long-double`
  :option:`-m96bit-long-double`  :option:`-mlong-double-64`  :option:`-mlong-double-80`  :option:`-mlong-double-128`
  :option:`-mregparm`:samp:`={num}`  :option:`-msseregparm`
  :option:`-mveclibabi`:samp:`={type}`  :option:`-mvect8-ret-in-mem`
  :option:`-mpc32`  :option:`-mpc64`  :option:`-mpc80`  :option:`-mstackrealign`
  :option:`-momit-leaf-frame-pointer`  :option:`-mno-red-zone`  :option:`-mno-tls-direct-seg-refs`
  :option:`-mcmodel`:samp:`={code-model}`  :option:`-mabi`:samp:`={name}`  :option:`-maddress-mode`:samp:`={mode}`
  :option:`-m32`  :option:`-m64`  :option:`-mx32`  :option:`-m16`  :option:`-miamcu`  :option:`-mlarge-data-threshold`:samp:`={num}`
  :option:`-msse2avx`  :option:`-mfentry`  :option:`-mrecord-mcount`  :option:`-mnop-mcount`  :option:`-m8bit-idiv`
  :option:`-minstrument-return`:samp:`={type}` :option:`-mfentry-name`:samp:`={name}` :option:`-mfentry-section`:samp:`={name}`
  :option:`-mavx256-split-unaligned-load`  :option:`-mavx256-split-unaligned-store`
  :option:`-malign-data`:samp:`={type}`  :option:`-mstack-protector-guard`:samp:`={guard}`
  :option:`-mstack-protector-guard-reg`:samp:`={reg}`
  :option:`-mstack-protector-guard-offset`:samp:`={offset}`
  :option:`-mstack-protector-guard-symbol`:samp:`={symbol}`
  :option:`-mgeneral-regs-only`  :option:`-mcall-ms2sysv-xlogues`
  :option:`-mindirect-branch`:samp:`={choice}`  :option:`-mfunction-return`:samp:`={choice}`
  :option:`-mindirect-branch-register` :option:`-mneeded`

  *x86 Windows Options*

  :option:`-mconsole`  :option:`-mcygwin`  :option:`-mno-cygwin`  :option:`-mdll`
  :option:`-mnop-fun-dllimport`  :option:`-mthread`
  :option:`-municode`  :option:`-mwin32`  :option:`-mwindows`  :option:`-fno-set-stack-executable`

  *Xstormy16 Options*

  :option:`-msim`

  *Xtensa Options*

  :option:`-mconst16`  :option:`-mno-const16`
  :option:`-mfused-madd`  :option:`-mno-fused-madd`
  :option:`-mforce-no-pic`
  :option:`-mserialize-volatile`  :option:`-mno-serialize-volatile`
  :option:`-mtext-section-literals`  :option:`-mno-text-section-literals`
  :option:`-mauto-litpools`  :option:`-mno-auto-litpools`
  :option:`-mtarget-align`  :option:`-mno-target-align`
  :option:`-mlongcalls`  :option:`-mno-longcalls`
  :option:`-mabi`:samp:`={abi-type}`

  *zSeries Options*
  See S/390 and zSeries Options.