/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
void *foo(void)
{
  return __builtin_thread_pointer();
}
