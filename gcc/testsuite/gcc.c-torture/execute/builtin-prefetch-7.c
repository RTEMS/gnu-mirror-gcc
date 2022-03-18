/* Test that __builtin_prefetch does no harm.

   Prefetch using all valid combinations of cache, rw and locality values.
   These must be compile-time constants.  */

#define NO_TEMPORAL_LOCALITY 0
#define LOW_TEMPORAL_LOCALITY 1
#define MODERATE_TEMPORAL_LOCALITY 2
#define HIGH_TEMPORAL_LOCALITY 3

#define WRITE_ACCESS 1
#define READ_ACCESS 0

#define DATA_PRFCH 1
#define INST_PRFCH 0

enum locality { none, low, moderate, high };
enum rw { read, write };
enum cache { inst, data };

int arr[10];

void
good_const (const int *p)
{
  __builtin_prefetch (p, 1, 0, 1);
  __builtin_prefetch (p, WRITE_ACCESS, 1, DATA_PRFCH);
  __builtin_prefetch (p, 0, HIGH_TEMPORAL_LOCALITY, 0);
  __builtin_prefetch (p, READ_ACCESS, MODERATE_TEMPORAL_LOCALITY, INST_PRFCH);
}

void
good_enum (const int *p)
{
    __builtin_prefetch (p, write, none, data);
    __builtin_prefetch (p, write, low, data);
    __builtin_prefetch (p, read, moderate, inst);
    __builtin_prefetch (p, read, high, inst);
}

void
good_expr (const int *p)
{
  __builtin_prefetch (p, 1 + 0, 6 - (2 * 3), 1 + 0);
  __builtin_prefetch (p, 1 - 1, 1 + 2, 2 - 2);
}

int
main ()
{
  good_const (arr);
  good_enum (arr);
  good_expr (arr);
  exit (0);
}
