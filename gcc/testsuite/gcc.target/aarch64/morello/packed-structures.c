/* { dg-additional-options "-Wno-if-not-aligned" } */
/*
   This file is for testing the PCS implementation on packed structures.

   As it stands we just use this for manual checking, some time in the future
   we'll put it into the AAPCS64 testsuite for proper checking.  That "proper"
   checking can only happen when we have runtime execution, and makes it much
   harder to eyeball the assembly.  Hence we're leaving that for later.
 */

/* Smaller than 24, capability is in *latter* part of structure.
   Not sure what should happen ...  LLVM currently crashes.
   Is not really pratical to pass this while maintaining the validity of the
   capability.  Would have to have special handling for any structure of this
   sort that puts the structure on the stack with an alignment that allows the
   second capability argument to be valid (i.e. whatever means that capability
   is aligned to 16).  This decision based on the structure seems overly
   complicated for such a rare type of structure.  */
struct __attribute__ ((packed)) struct_latter { unsigned short b; void *a; };

/* Greater than 24, capability is in *latter* part of structure.
   Overlap => Should be passed in memory.  */
struct __attribute__ ((packed)) struct_overlap_latter
{ unsigned short b; unsigned long long c; void *a; };

/* Smaller than 24, capability is in *first* part of structure.
   Should pass in capability registers, can do so since can just load the
   relevant `w` or `x` register. 

   MORELLO TODO Do *not* know if the capability is aligned to 16 bytes.  Hence
   need to copy to stack using memcpy and load registers from there (since
   capability load requires 16 byte alignment).  Should raise a warning when
   doing this.   */
struct __attribute__ ((packed)) struct_former { void *a; unsigned short b; };

/* Greater than 24, capability is in *first* part of structure.
   Overlap on capability metadata => Should be passed in memory.  */
struct __attribute__ ((packed)) struct_overlap_former
{ void *a; unsigned short b; unsigned long long c; };

/* Packed attribute on the member rather than the entire structure.
   Likely want to completely disallow such structures.
   MORELLO TODO Look into disallowing such structures and giving an error
   similar to the warning LLVM currently gives.  */
struct member_latter
{ unsigned short int b; __attribute__((packed)) void *a; };


/* Packed non-capability attribute, should be passed in capability registers.
   Size is 32.  */
struct noncap_member_former
{ void *a; unsigned char c; __attribute__((packed)) unsigned short b; };

/* Packed attribute on the first capability member.
   Behaves same as if `packed` attribute was on the structure (i.e. in `c`
   registers and should be copied to stack using memcpy if coming from
   somewhere else).  */
struct member_former
{ __attribute__((packed)) void *a; unsigned short int b; };


/* Basic structures, passed in `c` registers.  */
struct basic_latter
{ unsigned short b; void *a; };
struct basic_former
{ void *a; unsigned short b; };

/* Structures which are packed *and* aligned to the correct alignment behave
   the same as without any attribute. */
struct __attribute__ ((packed,aligned(16))) struct_aligned_latter
{ unsigned short b; void *a; };
struct __attribute__ ((packed,aligned(16))) struct_aligned_former
{ void *a; unsigned short b; };

/* For each case I want:
     1) To know the size of the structure according to the compiler.
     2) To check how the structure is passed at the PCS boundary:
        - Returned
	- As an argument
     3) To ensure that to/from memory works fine.
     4) To ensure warnings make sense.  */
#define CHECK_FUNC(X) \
  extern struct X X##_val; \
  extern void call_func_##X (struct X); \
  extern void see_size_##X (unsigned long); \
  extern void to_from_mem_##X (struct X *); \
  struct X check_##X (struct X arg0) \
    {  \
      struct X ret; \
      call_func_##X (ret); \
      call_func_##X (arg0); \
      call_func_##X (X##_val); \
      see_size_##X (sizeof (struct X)); \
      to_from_mem_##X (&ret); \
      ret.b += 3; \
      return ret; \
    }

CHECK_FUNC (struct_latter);
CHECK_FUNC (struct_overlap_latter);
CHECK_FUNC (struct_former);
CHECK_FUNC (struct_overlap_former);
CHECK_FUNC (member_latter);
CHECK_FUNC (noncap_member_former);
CHECK_FUNC (member_former);
CHECK_FUNC (basic_latter);
CHECK_FUNC (basic_former);
CHECK_FUNC (struct_aligned_latter);
CHECK_FUNC (struct_aligned_former);
