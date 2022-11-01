enum {
  CAP_SIZE = sizeof (void * __capability),
  PTR_SIZE = sizeof (void *),
};
#define CHECK_CAP(e) static_assert (sizeof (e) == CAP_SIZE)
#define CHECK_PTR(e) static_assert (sizeof (e) == PTR_SIZE)

#ifdef __CHERI__
#ifdef __CHERI_PURE_CAPABILITY__
static_assert (CAP_SIZE == PTR_SIZE);
#else
static_assert (CAP_SIZE > PTR_SIZE);
#endif
#endif
