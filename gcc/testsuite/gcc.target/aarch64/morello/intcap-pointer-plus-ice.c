/* { dg-do compile } */
/* We used to ICE here in initializer_constant_valid_p_1 because we weren't
   expecting to see anything other than an INTEGER_CST in a
   REPLACE_ADDRESS_VALUE.  */
int x;
__intcap c = (__intcap)&x + 1;
