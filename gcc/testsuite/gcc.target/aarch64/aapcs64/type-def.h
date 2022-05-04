/* This header file defines some types that are used in the AAPCS64 tests.  */


/* 64-bit vector of 2 floats.  */
typedef float vf2_t __attribute__((vector_size (8)));

/* 128-bit vector of 4 floats.  */
typedef float vf4_t __attribute__((vector_size (16)));

/* 128-bit vector of 4 ints.  */
typedef int vi4_t __attribute__((vector_size (16)));

/* 128-bit vector of 1 quad precision float.  */
typedef long double vlf1_t __attribute__((vector_size (16)));

/* signed quad-word (in an union for the convenience of initialization).  */
union int128_t
{
  __int128 i;
  struct
    {
      signed long long l64;
      signed long long h64;
    };
};

/* Homogeneous floating-point composite types.  */

struct hfa_fx1_t
{
  float a;
};

struct hfa_fx2_t
{
  float a;
  float b;
};

struct hfa_fx3_t
{
  float a;
  float b;
  float c;
};

struct hfa_f16x1_t
{
  __fp16 a;
};

struct hfa_f16x2_t
{
  __fp16 a;
  __fp16 b;
};

struct hfa_f16x3_t
{
  __fp16 a;
  __fp16 b;
  __fp16 c;
};

struct hfa_dx2_t
{
  double a;
  double b;
};

struct hfa_dx4_t
{
  double a;
  double b;
  double c;
  double d;
};

struct hfa_ldx3_t
{
  long double a;
  long double b;
  long double c;
};

struct hfa_ffs_t
{
  float a;
  float b;
  struct hfa_fx2_t c;
};

union hfa_union_t
{
  struct
    {
      float a;
      float b;
    } s;
  float c;
};

/* Non homogeneous floating-point-composite types.  */

struct non_hfa_fx5_t
{
  float a;
  float b;
  float c;
  float d;
  float e;
};

struct non_hfa_ffs_t
{
  float a;
  float b;
  struct hfa_dx2_t c;
};

struct non_hfa_ffs_2_t
{
  struct
    {
      int a;
      int b;
    } s;
  float c;
  float d;
};

struct hva_vf2x1_t
{
  vf2_t a;
};

struct hva_vf2x2_t
{
  vf2_t a;
  vf2_t b;
};

struct hva_vi4x1_t
{
  vi4_t a;
};

struct non_hfa_ffd_t
{
  float a;
  float b;
  double c;
};

struct non_hfa_ii_t
{
  int a;
  int b;
};

struct non_hfa_c_t
{
  char a;
};

struct non_hfa_ffvf2_t
{
  float a;
  float b;
  vf2_t c;
};

struct non_hfa_fffd_t
{
  float a;
  float b;
  float c;
  double d;
};

union non_hfa_union_t
{
  double a;
  float b;
};

#ifdef __CHERI__
/* Structures containing capabilities.  */
union cap_no_overlap_union_t
{
  __uintcap_t uic;
  int * __capability ip;
  int i;
};

union cap_overlap_union_t
{
  __uintcap_t uic;
  int * __capability ip;
  struct {
      long long ll1;
      long long ll2;
  } s;
};

struct cap_no_overlap_nc_t
{
  long long ll1;
  __uintcap_t uic;
};

struct cap_no_overlap_cn_t
{
  __uintcap_t uic;
  long long ll1;
};

struct cap_two_cap_t
{
  __uintcap_t uic;
  __intcap_t   ic;
};

struct cap_overlap_nc_t
{
  long long ll1;
  long long ll2;
  __uintcap_t uic;
};

struct cap_overlap_cn_t
{
  __uintcap_t uic;
  long long ll1;
  long long ll2;
};

struct cap_large_struct_t
{
  __uintcap_t uic1;
  __uintcap_t uic2;
  __uintcap_t uic3;
};

/* Smaller than 24, capability is in *latter* part of structure.
   Is not really pratical to pass this while maintaining the validity of the
   capability.  Would have to have special handling for any structure of this
   sort that puts the structure on the stack with an alignment that allows the
   second capability argument to be valid (i.e. whatever means that capability
   is aligned to 16).  This decision based on the structure seems overly
   complicated for such a rare type of structure.  */
struct __attribute__ ((packed)) packed_struct_latter
{ unsigned short b; __uintcap_t a; };

/* Greater than 24, capability is in *latter* part of structure.
   Overlap => Should be passed in memory.  */
struct __attribute__ ((packed)) packed_struct_overlap_latter
{ unsigned short b; unsigned long long c; __uintcap_t a; };

/* Smaller than 24, capability is in *first* part of structure.
   Should pass in capability registers, can do so since can just load the
   relevant `w` or `x` register. 

   MORELLO TODO Do *not* know if the capability is aligned to 16 bytes.  Hence
   need to copy to stack using memcpy and load registers from there (since
   capability load requires 16 byte alignment).  Should raise a warning when
   doing this.   */
struct __attribute__ ((packed)) packed_struct_former
{ __uintcap_t a; unsigned short b; };

/* Greater than 24, capability is in *first* part of structure.
   Overlap on capability metadata => Should be passed in memory.  */
struct __attribute__ ((packed)) packed_struct_overlap_former
{ __uintcap_t a; unsigned short b; unsigned long long c; };

/* Packed attribute on the member rather than the entire structure.
   Likely want to completely disallow such structures.
   MORELLO TODO Look into disallowing such structures and giving an error
   similar to the warning LLVM currently gives.  */
struct packed_member_latter
{ unsigned short int b; __attribute__((packed)) __uintcap_t a; };


/* Packed non-capability attribute, should be passed in capability registers.
   Size is 32.  */
struct packed_noncap_member_former
{ __uintcap_t a; unsigned char c; __attribute__((packed)) unsigned short b; };

/* Packed attribute on the first capability member.
   Behaves same as if `packed` attribute was on the structure (i.e. in `c`
   registers and should be copied to stack using memcpy if coming from
   somewhere else).  */
struct packed_member_former
{ __attribute__((packed)) __uintcap_t a; unsigned short int b; };


/* Structures which are packed *and* aligned to the correct alignment behave
   the same as without any attribute. */
struct __attribute__ ((packed,aligned(16))) packed_struct_aligned_latter
{ unsigned short b; __uintcap_t a; };
struct __attribute__ ((packed,aligned(16))) packed_struct_aligned_former
{ __uintcap_t a; unsigned short b; };


/* Nested structure containing a capability (should be passed in a single
   capability register).  */
struct nested_cap { struct { __uintcap_t a; } b; };
#endif
