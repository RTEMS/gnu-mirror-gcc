/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Origin: Aldy Hernandez  <aldyh@redhat.com>  */

#include <altivec.h>

int **intp;
int *var_int;
unsigned int **uintp;
vector pixel *varpixel;
vector signed char *vecchar;
vector signed int *vecint;
vector signed short *vecshort;
vector unsigned char *vecuchar;
vector unsigned int *vecuint;
vector bool int *vecubi;
vector bool char *vecubci;
vector bool short int *vecubsi;
vector unsigned short *vecushort;
vector bool int *vecbint;
vector float *vecfloat;

int main ()
{
  *vecfloat++ = vec_andc((vector bool int)vecint[0], vecfloat[1]);
  *vecfloat++ = vec_andc(vecfloat[0], (vector bool int)vecint[1]);
  *vecfloat++ = vec_xor((vector bool int)vecint[0], vecfloat[1]);
  *vecfloat++ = vec_xor(vecfloat[0], (vector bool int)vecint[1]);
  *varpixel++ = vec_packpx(vecuint[0], vecuint[1]);
  *vecshort++ = vec_mule(vecchar[0], vecchar[1]);
  *vecshort++ = vec_mulo(vecchar[0], vecchar[1]);
  *vecint++ = vec_ld(var_int[0], intp[1]);
  *vecint++ = vec_lde(var_int[0], intp[1]);
  *vecint++ = vec_ldl(var_int[0], intp[1]);
  *vecint++ = vec_unpackh(vecshort[0]);
  *vecint++ = vec_unpackl(vecshort[0]);
  *vecushort++ = vec_andc((vector bool short)vecshort[0], vecushort[1]);
  *vecushort++ = vec_andc(vecushort[0], (vector bool short)vecshort[1]);
  *vecushort++ = vec_xor((vector bool short)vecshort[0], vecushort[1]);
  *vecushort++ = vec_xor(vecushort[0], (vector bool short)vecshort[1]);
  *vecuint++ = vec_ld(var_int[0], uintp[1]);
  *vecuint++ = vec_lvx(var_int[0], uintp[1]);
  *vecuint++ = vec_msum(vecuchar[0], vecuchar[1], vecuint[2]);
  *vecuchar++ = vec_xor(vecuchar[0], (vector unsigned char)vecchar[1]);

  *vecubi++ = vec_unpackh(vecubsi[0]);
  *vecuint++ = vec_unpackh(varpixel[0]);
  *vecubsi++ = vec_unpackh(vecubci[0]);
  *vecshort++ = vec_unpackh(vecchar[0]);

  *vecubi++ = vec_unpackl(vecubsi[0]);
  *vecuint++ = vec_unpackl(varpixel[0]);
  *vecubsi++ = vec_unpackl(vecubci[0]);
  *vecshort++ = vec_unpackl(vecchar[0]);
  
  return 0;
}

/* Expected results:
     vec_packpx                     vpkpx
     vec_mulo                       vmulesb
     vec_ld                         lxv2x
     vec_lde                        lvewx
     vec_ldl                        lxvl
     vec_unpackh                    vupklsh
     vec_unpackh                    vupklpx
     vec_unpackh                    vupklsb
     vec_unpackl                    vupkhsh
     vec_unpackl                    vupkhpx
     vec_unpackl                    vupkhsb
     vec_andc                       xxlnor (vnor AIX)
                                    xxland (vand AIX)
     vec_xor                        xxlxor
     vec_msum                       vmsumubm
     vec_mule                       vmulosb
     vec_mulo                       vmulesb
     vec_ld                         lvx
*/

/* { dg-final { scan-assembler-times "vpkpx" 2 } } */
/* { dg-final { scan-assembler-times "vmulesb" 1 } } */
/* { dg-final { scan-assembler-times "vmulosb" 1 } } */
/* { dg-final { scan-assembler-times {\mlvx\M} 42 { target { ! powerpc_vsx } } } } */
/* { dg-final { scan-assembler-times {\mlxv} 0 { target { ! powerpc_vsx } } } } */
/* { dg-final { scan-assembler-times {\mlvx\M} 0 { target powerpc_vsx } } } */
/* { dg-final { scan-assembler-times {\mlxv} 42 { target powerpc_vsx } } } */
/* { dg-final { scan-assembler-times "lvewx" 1 } } */
/* { dg-final { scan-assembler-times "lvxl" 1 } } */
/* { dg-final { scan-assembler-times "vupklsh" 2 } } */
/* { dg-final { scan-assembler-times "vupkhsh" 2 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M|\mvnor\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxland\M|\mvand\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M|\mvxor\M} 5 } } */
/* { dg-final { scan-assembler-times "xxlandc" 0 } } */
/* { dg-final { scan-assembler-times "vmsumubm" 1 } } */
/* { dg-final { scan-assembler-times "vupklpx" 1 } } */
/* { dg-final { scan-assembler-times "vupklsx" 0 } } */
/* { dg-final { scan-assembler-times "vupklsb" 2 } } */
/* { dg-final { scan-assembler-times "vupkhpx" 1 } } */
/* { dg-final { scan-assembler-times "vupkhsb" 2 } } */
