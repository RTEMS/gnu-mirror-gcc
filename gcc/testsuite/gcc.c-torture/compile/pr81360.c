typedef a;
b(void *c, a d) {
  if (c)
    e(0, __PRETTY_FUNCTION__);
}
typedef f, g;
__attribute__((optimize(0))) h() {
  g i;
#ifdef __GCC_ARM_CAPABILITY_ANY
  b((__intcap_t) i, sizeof(f));
#else
  b(i, sizeof(f));
#endif
}

