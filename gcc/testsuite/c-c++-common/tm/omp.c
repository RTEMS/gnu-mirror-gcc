/* { dg-do compile { target { ! aarch64_capability_any } } } */
/* { dg-options "-fgnu-tm -fopenmp" } */
/* { dg-require-effective-target pthread } */

__attribute__ ((transaction_pure))
unsigned long rdtsc();

typedef struct ENTER_EXIT_TIMES
{
  unsigned long enter;
} times_t;

void ParClassify()
{
  void * Parent;
#pragma omp parallel private(Parent)
  {
    times_t inside;
    __transaction_atomic {
       inside.enter = rdtsc();
    }
  }
}
