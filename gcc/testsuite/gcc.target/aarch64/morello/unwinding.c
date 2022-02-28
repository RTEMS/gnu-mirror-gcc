/* { dg-do run } */
/* { dg-additional-options "-fasynchronous-unwind-tables -Wno-int-conversion -std=gnu99" } */
/* { dg-skip-if "" { *-*-* } { "-fomit-frame-pointer" } { "" } }  */

/* Testing the unwinder for PureCap.
   This test does the following:
      - Ensure _Unwind_ForcedUnwind manages to unwind the entire stack.
      - Ensure that _Unwind_Backtrace gets the same frame pointers and return
	addresses as _Unwind_ForcedUnwind (i.e. for the entire stack).
      - Ensure that __builtin_frame_address finds the same frame pointers as
	the above two backtraces (comparing the GCC known information against
	the information that GCC passes to the unwinder through dwarf info).
      - Ensure (though with a non-fool-proof method) that we go through the
	expected foo/main/foo/main call stack.

    Combined we hope this should be a good test of the unwinding part (without
    personality functions and LSDA information) of libgcc.  This is the part
    that may be used in pure C code.

    We're putting this test here since it's only really needed for purecap,
    even though it should pass for any system compiled with
    -fasynchronous-unwind-tables.  */
#include <stdio.h>
#include <unwind.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

typedef __UINTPTR_TYPE__ uintptr_t;
#define NUM_INTERNAL_CALLS 20
bool check_performed = false;
static bool in_func (void *, size_t);
static _Unwind_Reason_Code trace_function (struct _Unwind_Context *, void *);
static _Unwind_Reason_Code unwind_stop (int, _Unwind_Action,
					_Unwind_Exception_Class,
					struct _Unwind_Exception *,
					struct _Unwind_Context *, void *);
static void unwind_cleanup (_Unwind_Reason_Code, struct _Unwind_Exception *);
static void test_unwinder (int);
static int foo (int);
static int ptr_compare (const void *, const void *);
static void sort_function_pointers ();
int main (int, char**);

static uintptr_t function_pointers[9] = { 0 };
static int foo_index = -1;
static int main_index = -1;

static bool in_func (void *pc, size_t index)
{
  size_t num_funcs = sizeof(function_pointers)/sizeof(function_pointers[0]);
  assert (index < num_funcs);
  /* The PC we get has two problems when trying to tell which function we're in
     from it.  First there's the Morello LSB which means the pointer is one
     more than the address it refers to.
     Second there's the fact that the return address points to the *next*
     instruction after a function call (and not the function call itself).
     For a `noreturn` function the BL instruction could be the very last
     instruction, which would mean that the return address is pointing into the
     next function.

     To account for these we use the `__builtin_code_address_from_pointer`
     builtin to remove the Morello LSB if it exists, and then subtract one from
     the pc before comparing it against function addresses.  */
  uintptr_t pcaddr = __builtin_code_address_from_pointer (pc);
  pcaddr -= 1;
  if (index == num_funcs - 1)
    return pcaddr > function_pointers[index];
  return pcaddr > function_pointers[index] && pcaddr < function_pointers[index+1];
}

struct frame_recording {
    __UINTPTR_TYPE__ retptr_array[100];  /* Assume enough.  */
    __UINTPTR_TYPE__ stack_ptr_array[100];  /* Assume enough.  */
    size_t num_frames;
};

static _Unwind_Reason_Code
trace_function (struct _Unwind_Context *context, void *trace_parameter)
{
  struct frame_recording *fr = (struct frame_recording *)trace_parameter;
  fr->retptr_array[fr->num_frames] = _Unwind_GetIP (context);
  fr->stack_ptr_array[fr->num_frames] = _Unwind_GetCFA (context);
  fr->num_frames += 1;
  return _URC_NO_REASON;
}

static _Unwind_Reason_Code
unwind_stop (int version, _Unwind_Action actions,
	     _Unwind_Exception_Class exc_class,
	     struct _Unwind_Exception *exc_obj,
	     struct _Unwind_Context *context, void *stop_parameter)
{
  if (stop_parameter == NULL)
    return _URC_FATAL_PHASE2_ERROR;
  struct frame_recording *fr = (struct frame_recording *)stop_parameter;
  fr->retptr_array[fr->num_frames] = _Unwind_GetIP (context);
  fr->stack_ptr_array[fr->num_frames] = _Unwind_GetCFA (context);
  fr->num_frames += 1;
  if (actions & _UA_END_OF_STACK)
    {
      /* Our caller should have pointed us to three `struct frame_recording`
	 structures.  During testing we should have gone through
	 _Unwind_Backtrace to fill out the second of these structures, we
	 should have used __builtin_frame_address to fill out the third of
	 these structures, and on the unwind to this end of the stack we shoud
	 have filled out the first.

	 We now check that the three match below the point where they were both
	 called in the same function but at different places.
	 There are caveats around the third structure:
	   1. It only has a return address for one of the frames.
	   2. The return address and stack address are offset for the GCC
	      builtin version.
	      We could make them not offset.*/
      bool found_matching_point = false;
      struct frame_recording *fr2
	= ((struct frame_recording *)stop_parameter) + 1;
      struct frame_recording *fr3
	= ((struct frame_recording *)stop_parameter) + 2;
      size_t second_count = 0;
      for (size_t i = 0; i < fr->num_frames; i++)
	{
	  if (found_matching_point)
	    {
	      assert (fr->retptr_array[i] == fr2->retptr_array[i]);
	      assert (fr->stack_ptr_array[i] == fr2->stack_ptr_array[i]);
	      if (second_count < fr3->num_frames)
		  assert (fr->stack_ptr_array[i] == fr3->stack_ptr_array[second_count]);
	      if (second_count < NUM_INTERNAL_CALLS)
		  /* For these frames we're either in foo or main.  */
		  assert (in_func (fr->retptr_array[i],
				   second_count % 2 ? foo_index : main_index));
	      second_count += 1;
	    }
	  else if (fr->retptr_array[i] == fr2->retptr_array[i])
	    {
	      assert (fr->stack_ptr_array[i] == fr2->stack_ptr_array[i]);
	      /* __builtin_return_addr only works for the first frame on
		 AArch64.  
		 The first point where the two unwinder backtraces have the
		 same return address should be the test_unwinder (depends on
		 the unwinder implementation, but I believe this should always
		 be the case).  If the backtrace is looking at test_unwinder,
		 then the return address should be the same as that found by
		 __builtin_return_address when used from the same function.

		 We store this in the first element of the third frame_record
		 structure, just to ensure that the IP pointer and frame
		 pointer are for the "previous" frame in that case.

	         __builtin_frame_address(0) returns the stack address of the
		 *current* frame, while __builtin_return_address(0) returns the
		 IP address of the *previous* frame.  */
	      assert (fr->retptr_array[i] == fr3->retptr_array[1]);
	      /* Since we start counting based on the return address matching
		 and we know that we have one extra frame of information for
		 the stack pointer, we check twice here.  */
	      assert (fr->stack_ptr_array[i-1] == fr3->stack_ptr_array[second_count]);
	      second_count += 1;
	      if (fr3->num_frames >= 2)
		assert (fr->stack_ptr_array[i] == fr3->stack_ptr_array[second_count]);
	      second_count += 1;
	      assert (in_func (fr->retptr_array[i], foo_index));
	      found_matching_point = true;
	    }
	  else
	    assert (i < 1);  /* Should be the _Unwind_* function's return
				address which is different (different places
				inside test_unwinder), but the stack pointer
				and return addresses should match from then on.
				*/
	}
      check_performed
	= found_matching_point && second_count >= NUM_INTERNAL_CALLS;
    }
  return _URC_NO_REASON;
}

static void
unwind_cleanup (_Unwind_Reason_Code reason, struct _Unwind_Exception *exc)
{
  abort ();
}

static void
test_unwinder (int arg)
{
  struct _Unwind_Exception exc;
  struct frame_recording records[3] = {0};
  _Unwind_Backtrace (trace_function, &records[1]);

  /* Arguments to __builtin_{return,frame}_address must be constant integers.
     Hence we just use a range that we know should work.  Based on the number
     of recursions we add, assuming that argc is 1 to start with.
     AArch64 happens to not support __builtin_return_address to any previous
     frame, but this does at least give us one frame to check against the
     unwinder implementation.  */
#ifndef OMIT_FRAME_POINTER
  records[2].num_frames = NUM_INTERNAL_CALLS;
  records[2].retptr_array[1] = __builtin_return_address ((unsigned int)0);
  records[2].stack_ptr_array[0] = __builtin_frame_address ((unsigned int)0);
  records[2].stack_ptr_array[1] = __builtin_frame_address ((unsigned int)1);
  records[2].stack_ptr_array[2] = __builtin_frame_address ((unsigned int)2);
  records[2].stack_ptr_array[3] = __builtin_frame_address ((unsigned int)3);
  records[2].stack_ptr_array[4] = __builtin_frame_address ((unsigned int)4);
  records[2].stack_ptr_array[5] = __builtin_frame_address ((unsigned int)5);
  records[2].stack_ptr_array[6] = __builtin_frame_address ((unsigned int)6);
  records[2].stack_ptr_array[7] = __builtin_frame_address ((unsigned int)7);
  records[2].stack_ptr_array[8] = __builtin_frame_address ((unsigned int)8);
  records[2].stack_ptr_array[9] = __builtin_frame_address ((unsigned int)9);
  records[2].stack_ptr_array[10] = __builtin_frame_address ((unsigned int)10);
  records[2].stack_ptr_array[11] = __builtin_frame_address ((unsigned int)11);
  records[2].stack_ptr_array[12] = __builtin_frame_address ((unsigned int)12);
  records[2].stack_ptr_array[13] = __builtin_frame_address ((unsigned int)13);
  records[2].stack_ptr_array[14] = __builtin_frame_address ((unsigned int)14);
  records[2].stack_ptr_array[15] = __builtin_frame_address ((unsigned int)15);
  records[2].stack_ptr_array[16] = __builtin_frame_address ((unsigned int)16);
  records[2].stack_ptr_array[17] = __builtin_frame_address ((unsigned int)17);
  records[2].stack_ptr_array[18] = __builtin_frame_address ((unsigned int)18);
  records[2].stack_ptr_array[19] = __builtin_frame_address ((unsigned int)19);
#else
  /* With -fomit-frame-pointer we can still look at the frame pointer in this
     frame because the use of the builtin ensures that the frame chain is
     emitted in this one function, but we can't check any further.

     N.b. GCC's implementation of __builtin_frame_address just takes c29, but
     it is possible that c29 is different to the CFA (which will essentially be
     CSP as it is when the *next* function is called).  Hence this check is not
     fool-proof.  If this turns out to be very flaky then we should remove it,
     but it seems stable enough to keep for now.  */
  records[2].num_frames = 1;
  records[2].retptr_array[1] = __builtin_return_address ((unsigned int)0);
  records[2].stack_ptr_array[0] = __builtin_frame_address ((unsigned int)0);
#endif

  exc.exception_class = 0;
  exc.exception_cleanup = unwind_cleanup;
  _Unwind_ForcedUnwind (&exc, unwind_stop, &records[0]);

  /* Exit code for testsuite.  */
  exit (check_performed ? 0 : 1);
}

/* Make a bit of a call-stack to follow.
   Even though multiple calls of a -> b -> a probably just checks the same code
   path multiple times there is a difference between N and 1, so it's still
   worth doing.  */
static int __attribute__ ((noinline))
foo (int argument)
{
  if (argument > (NUM_INTERNAL_CALLS / 2))
    test_unwinder (argument);
  /* Ensure we avoid tail-calls, since that could break our expected call-stack.  */
  int ret = main (argument, NULL);
  asm volatile ("// Volatile asm in foo" : : : );
  return ret;
}

static int ptr_compare (const void *a, const void *b)
{
  const uintptr_t * const x = a;
  const uintptr_t * const y = b;
  if (*x == *y)
    return 0;
  return *x < *y ? -1 : 1;
}

static void sort_function_pointers ()
{
  function_pointers[0] = &in_func;
  function_pointers[1] = &trace_function;
  function_pointers[2] = &unwind_stop;
  function_pointers[3] = &unwind_cleanup;
  function_pointers[4] = &test_unwinder;
  function_pointers[5] = &foo;
  function_pointers[6] = &ptr_compare;
  function_pointers[7] = &sort_function_pointers;
  function_pointers[8] = &main;
  size_t num_funcs = sizeof(function_pointers)/sizeof(function_pointers[0]);
  qsort (function_pointers, num_funcs, sizeof(void *), ptr_compare);
  for (size_t i = 0; i < num_funcs; ++i) {
    if (function_pointers[i] == (uintptr_t)&main)
      {
	assert (main_index == -1);
	main_index = i;
      }
    else if (function_pointers[i] == (uintptr_t)&foo)
      {
	assert (foo_index == -1);
	foo_index = i;
      }
  }
}

int main (int argc, char **argv)
{
  if (foo_index == -1 || main_index == -1)
    sort_function_pointers ();
  if (argc != 20)
    {
      int ret = foo (argc + 1);
      asm volatile ("// Volatile asm in main" : : : );
      return ret;
    }
  /* If argc > 20 test did not execute as expected.  Return a failure code.  */
  return 1;
}
