/* This header file defines a set of macros to be used in the construction
   of parameter passing and/or va_arg code gen tests during the
   pre-processing stage.  It is included inside abitest.h.

   The following macros are defined here:

     LAST_ARG
     ARG
     DOTS
     ANON
     LAST_ANON
     PTR
     PTR_ANON
     LAST_ANONPTR

  These macros are given different definitions depending on which one of
  the following macros is defined.

    AARCH64_MACRO_DEF_CHECK_INCOMING_ARGS
    AARCH64_MACRO_DEF_GEN_PARAM_TYPE_LIST
    AARCH64_MACRO_DEF_GEN_ARGUMENT_LIST
    AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST
    AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST_WITH_IDENT
    AARCH64_VARIADIC_MACRO_DEF_ASSIGN_LOCAL_VARS_WITH_ARGS
    AARCH64_VARIADIC_MACRO_DEF_GEN_ARGUMENT_LIST

  Do not define more than one of the above macros.  */


/* AARCH64_MACRO_DEF_CHECK_INCOMING_ARGS
   Define macros to check the incoming arguments.  */

#ifdef AARCH64_MACRO_DEF_CHECK_INCOMING_ARGS

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#undef PTR
#undef PTR_ANON
#undef LAST_ANONPTR
#undef ANON_PROMOTED

/* Generate memcmp to check if the incoming args have the expected values.  */
#define LAST_ARG_NONFLAT(type, val, offset, layout, ...)		\
{									\
  type __x = val;							\
  DUMP_ARG(type,val);							\
  if (validate_memory (&__x, stack + offset, sizeof (type), layout) != 0) \
    abort();								\
}
#define LAST_ARG(type,val,offset,...) LAST_ARG_NONFLAT (type, val, offset, \
							flat,__VA_ARGS__)
#define ARG_NONFLAT(type,val,offset,layout,...) LAST_ARG_NONFLAT (type, val, \
								  offset, \
								  layout, \
								  __VA_ARGS__)
#define ARG(type,val,offset,...) LAST_ARG_NONFLAT(type, val, offset, \
						  flat, __VA_ARGS__)

/* For AAPCS64-cap, we specialise the anonymous argument macros to check that
   the arguments are passed as expected. The user simply needs to provide the
   offset into the Anonymous Argument Memory Area at which they expect to find
   the given argument.

   Note that we don't override the macros for AAPCS64_TEST_STDARG tests: for
   those tests, the __builtin_va_{start,arg,end} functions are used to extract
   the variadic arguments and pass them to a non-variadic function, where we
   simply check that the arguments end up in the expected registers for such a
   function. Hence, we don't need to extract the incoming variadic arguments
   manually.  */
#if defined(__CHERI_PURE_CAPABILITY__) && !defined(AAPCS64_TEST_STDARG)

#define LAST_ANON(type,val,ignored,offset,...)	  \
{						  \
  type __x = val;				  \
  void *ptr = *(void **)(stack + C9) + offset;	  \
  if (sizeof (type) > 16 || _Alignof (type) > 16) \
    ptr = *(void **)ptr;			  \
  if (memcmp (&__x, ptr, sizeof (type)) != 0)	  \
    abort ();					  \
}

/* For AAPCS64-cap, treat PTR_ANON as with any other anonymous argument: the
   PTR-ness only matters for the base PCS. This is because LAST_ANON itself
   handles any indirection required by AAPCS64-cap automatically, and any
   arguments that would be indirected in the base PCS are also indirected in
   AAPCS64-cap.  */
#define PTR_ANON(type,val,a64_off,c64_off) \
  LAST_ANON(type, val, a64_off, c64_off)

#else

#define LAST_ANON(type,val,offset,ignored,...) LAST_ARG(type, val, offset, __VA_ARGS__)
#define PTR_ANON(type, val,offset,ignored,...) PTR(type, val, offset, __VA_ARGS__)

#endif

#define ANON(type,val,a64_off,c64_off,...) \
  LAST_ANON(type, val, a64_off, c64_off, __VA_ARGS__)

#define ANON_PROMOTED(type,val,type_promoted, val_promoted, offset,...)	\
  ANON(type_promoted, val_promoted, offset, __VA_ARGS__)
/* Composite larger than 16 bytes is replaced by a pointer to a copy prepared
   by the caller, so here we extrat the pointer, deref it and compare the
   content with that of the original one.  */
#define PTR(type, val, offset, ...) {					\
  type * ptr;								\
  DUMP_ARG(type,val);							\
  ptr = *(type **)(stack + offset);					\
  if (memcmp (ptr, &val, sizeof (type)) != 0) abort ();			\
}
#define LAST_ANONPTR(type, val, offset, ...) PTR(type, val, offset, __VA_ARGS__)
#define DOTS

#endif /* AARCH64_MACRO_DEF_CHECK_INCOMING_ARGS */


/* AARCH64_MACRO_DEF_GEN_PARAM_TYPE_LIST
   Define macros to generate parameter type list.  */

#ifdef AARCH64_MACRO_DEF_GEN_PARAM_TYPE_LIST

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#undef PTR
#undef PTR_ANON
#undef LAST_ANONPTR

/* Generate parameter type list (without identifiers).  */
#define LAST_ARG(type,val,offset) type
#define LAST_ARG_NONFLAT(type, val, offset, layout) type
#define ARG(type,val,offset) LAST_ARG(type, val, offset),
#define ARG_NONFLAT(type, val, offset, layout) LAST_ARG (type, val, offset),
#define DOTS ...
#define ANON(type,val,a64_off,c64_off)
#define LAST_ANON(type,val,a64_off,c64_off)
#define PTR(type, val, offset) LAST_ARG(type, val, offset),
#define PTR_ANON(type, val, a64_off, c64_off)
#define LAST_ANONPTR(type, val, offset)

#endif /* AARCH64_MACRO_DEF_GEN_PARAM_TYPE_LIST */


/* AARCH64_MACRO_DEF_GEN_ARGUMENT_LIST
   Define macros to generate argument list.  */

#ifdef AARCH64_MACRO_DEF_GEN_ARGUMENT_LIST

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#undef PTR
#undef PTR_ANON
#undef LAST_ANONPTR
#undef ANON_PROMOTED

/* Generate the argument list; use VAL as the argument name.  */
#define LAST_ARG(type,val,offset,...) val
#define LAST_ARG_NONFLAT(type,val,offset,layout,...) val
#define ARG(type,val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define ARG_NONFLAT(type, val, offset, layout,...) LAST_ARG (type, val, \
							     offset, \
							     __VA_ARGS__),
#define DOTS
#define LAST_ANON(type,val,a64_off,c64_off,...) LAST_ARG(type, val, a64_off, __VA_ARGS__)
#define ANON(type,val,a64_off,c64_off,...) LAST_ARG(type, val, a64_off, __VA_ARGS__),
#define PTR(type, val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define PTR_ANON(type, val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define LAST_ANONPTR(type, val, offset,...) LAST_ARG(type, val, offset, __VA_ARGS__)
#define ANON_PROMOTED(type,val,type_promoted, val_promoted, offset,...)	\
  LAST_ARG(type, val, offset, __VA_ARGS__),

#endif /* AARCH64_MACRO_DEF_GEN_ARGUMENT_LIST */


/* AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST
   Define variadic macros to generate parameter type list.  */

#ifdef AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#undef PTR
#undef PTR_ANON
#undef LAST_ANONPTR
#undef ANON_PROMOTED

/* Generate parameter type list (without identifiers).  */
#define LAST_ARG(type,val,offset,...) type
#define LAST_ARG_NONFLAT(type, val, offset, layout, ...) type
#define ARG(type,val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define ARG_NONFLAT(type, val, offset, layout, ...) LAST_ARG (type, val, \
							      offset, \
							      __VA_ARGS__),
#define DOTS
#define ANON(type,val, offset,...) ARG(type,val,offset, __VA_ARGS__)
#define LAST_ANON(type,val, offset,...) LAST_ARG(type,val, offset, __VA_ARGS__)
#define PTR(type, val, offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define PTR_ANON(type, val, offset,...) PTR(type, val, offset, __VA_ARGS__)
#define LAST_ANONPTR(type, val, offset,...) LAST_ARG(type, val, offset, __VA_ARGS__)
#define ANON_PROMOTED(type,val,type_promoted, val_promoted, offset,...)	\
  LAST_ARG(type_promoted, val_promoted, offset, __VA_ARGS__),

#endif /*  AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST  */


/* AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST_WITH_IDENT
   Define variadic macros to generate parameter type list with
   identifiers.  */

#ifdef AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST_WITH_IDENT

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#undef PTR
#undef PTR_ANON
#undef LAST_ANONPTR
#undef ANON_PROMOTED

/* Generate parameter type list (with identifiers).
   The identifiers are named with prefix _f and suffix of the value of
   __VA_ARGS__.  */
#define LAST_ARG(type,val,offset,...) type _f##__VA_ARGS__
#define LAST_ARG_NONFLAT(type, val, offset, layout, ...) type _f##__VA_ARGS__
#define ARG(type,val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define ARG_NONFLAT(type, val, offset, layout, ...) LAST_ARG (type, val, \
							      offset, \
							      __VA_ARGS__),
#define DOTS ...
#define ANON(type,val, offset,...)
#define LAST_ANON(type,val, offset,...)
#define PTR(type, val, offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define PTR_ANON(type, val, offset,...)
#define LAST_ANONPTR(type, val, offset,...)
#define ANON_PROMOTED(type,val,type_promoted, val_promoted, offset,...)

#endif /* AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST_WITH_IDENT */


/* AARCH64_VARIADIC_MACRO_DEF_ASSIGN_LOCAL_VARS_WITH_ARGS
   Define variadic macros to generate assignment from the function
   incoming arguments to local variables.  */

#ifdef AARCH64_VARIADIC_MACRO_DEF_ASSIGN_LOCAL_VARS_WITH_ARGS

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#undef PTR
#undef PTR_ANON
#undef LAST_ANONPTR
#undef ANON_PROMOTED

/* Generate assignment statements.  For named args, direct assignment from
   the formal parameter is generated; for unnamed args, va_arg is used.
   The names of the local variables start with _x and end with the value of
   __VA_ARGS__.  */
#define LAST_ARG(type,val,offset,...) type _x##__VA_ARGS__ = _f##__VA_ARGS__;
#define LAST_ARG_NONFLAT(type, val, offset, layout, ...) \
  type _x##__VA_ARGS__ = _f##__VA_ARGS__;
#define ARG(type,val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__)
#define ARG_NONFLAT(type,val,offset,layout,...) \
  LAST_ARG (type, val, offset, __VA_ARGS__)
#define ANON(type,val,offset,...) type _x##__VA_ARGS__ = va_arg (ap, type);
#define LAST_ANON(type,val,offset,...) ANON(type, val, offset, __VA_ARGS__)
#define PTR(type, val,offset,...)  ARG(type, val, offset, __VA_ARGS__)
#define PTR_ANON(type, val, offset,...) ANON(type, val,offset, __VA_ARGS__)
#define LAST_ANONPTR(type, val, offset,...) ANON(type, val, offset, __VA_ARGS__)
#define ANON_PROMOTED(type,val,type_promoted, val_promoted, offset,...)	\
  ANON(type_promoted, val_promoted, offset, __VA_ARGS__)

#define DOTS

#endif /* AARCH64_VARIADIC_MACRO_DEF_ASSIGN_LOCAL_VARS_WITH_ARGS */


/* AARCH64_VARIADIC_MACRO_DEF_GEN_ARGUMENT_LIST
   Define variadic macros to generate argument list using the variables
   generated during AARCH64_VARIADIC_MACRO_DEF_ASSIGN_LOCAL_VARS_WITH_ARGS.  */

#ifdef AARCH64_VARIADIC_MACRO_DEF_GEN_ARGUMENT_LIST

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#undef PTR
#undef PTR_ANON
#undef LAST_ANONPTR
#undef ANON_PROMOTED

/* Generate the argument list; the names start with _x and end with the value of
   __VA_ARGS__.  All arguments (named or unnamed) in stdarg_func are passed to
   myfunc as named arguments.  */
#define LAST_ARG(type,val,offset,...) _x##__VA_ARGS__
#define LAST_ARG_NONFLAT(type, val, offset, layout, ...) _x##__VA_ARGS__
#define ARG(type,val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define ARG_NONFLAT(type, val, offset, layout, ...) \
  LAST_ARG_NONFLAT (type, val, offset, layout, __VA_ARGS__),
#define DOTS
#define LAST_ANON(type,val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__)
#define ANON(type,val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define PTR(type, val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define PTR_ANON(type, val,offset,...) LAST_ARG(type, val, offset, __VA_ARGS__),
#define LAST_ANONPTR(type, val, offset,...) LAST_ARG(type, val, offset, __VA_ARGS__)
#define ANON_PROMOTED(type,val,type_promoted, val_promoted, offset,...)	\
  ANON(type_promoted, val_promoted, offset, __VA_ARGS__)

#endif /* AARCH64_VARIADIC_MACRO_DEF_GEN_ARGUMENT_LIST */
