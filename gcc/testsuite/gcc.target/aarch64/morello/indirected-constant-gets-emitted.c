/* Ensure that putting a constant pool address into our indirection table
   works.  Two problems that we hit during development were:
   a) The original constant not getting emitted (due to not being mentioned in
      the RTL stream).
   b) No .size (or .type) directive associated with constant pool entries.

   Both of these problems can be checked by ensuring that the binary links to
   an executable.  */
/* { dg-do link } */
__int128 val()
{
  __int128 ret = 0xffaffafafafaffafULL;
  return ret;
}
int main()
{
  return 0;
}
