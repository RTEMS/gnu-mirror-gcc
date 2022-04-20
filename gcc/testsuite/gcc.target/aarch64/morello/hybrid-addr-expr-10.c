/* { dg-do compile } */

#define CAP(X) _Static_assert(sizeof(X) == sizeof(void *__capability))
#define NONCAP(X) _Static_assert(sizeof(X) == sizeof(void *))

//----------------------------------------------------------

int *__capability int_cap;
int *int_ptr;
int int_var;
struct { int f[2]; } int_struct;

CAP (int_cap);
NONCAP (int_ptr);

CAP (&*int_cap);
NONCAP (&*int_ptr);
NONCAP (&int_var);
NONCAP (&int_struct.f[0]);

//----------------------------------------------------------

__intcap *__capability intcap_cap;
__intcap *intcap_ptr;
__intcap intcap_var;
struct { __intcap f[2]; } intcap_struct;

CAP (&*intcap_cap);
NONCAP (&*intcap_ptr);
NONCAP (&intcap_var);
NONCAP (&intcap_struct.f[0]);

//----------------------------------------------------------

int *__capability *__capability int_cap_cap;
int **__capability int_ptr_cap;
int *__capability *int_cap_ptr;
int **int_ptr_ptr;

CAP (&int_cap_cap[0]);
CAP (&int_ptr_cap[0]);
NONCAP (&int_cap_ptr[0]);
NONCAP (&int_ptr_ptr[0]);

CAP (&*int_cap_cap);
CAP (&*int_ptr_cap);
NONCAP (&*int_cap_ptr);
NONCAP (&*int_ptr_ptr);

CAP (&int_cap_cap[0][0]);
CAP (&int_cap_ptr[0][0]);
NONCAP (&int_ptr_cap[0][0]);
NONCAP (&int_ptr_ptr[0][0]);

CAP (&**int_cap_cap);
CAP (&**int_cap_ptr);
NONCAP (&**int_ptr_cap);
NONCAP (&**int_ptr_ptr);

//----------------------------------------------------------

_Complex int *__capability complex_cap;
_Complex int *complex_ptr;
_Complex int complex_var;
struct { _Complex int f[2]; } complex_struct;

CAP (&__real complex_cap[0]);
CAP (&__imag complex_cap[0]);
NONCAP (&__real complex_ptr[0]);
NONCAP (&__imag complex_ptr[0]);

CAP (&__real *complex_cap);
CAP (&__imag *complex_cap);
NONCAP (&__real *complex_ptr);
NONCAP (&__imag *complex_ptr);
NONCAP (&__real complex_var);
NONCAP (&__imag complex_var);
NONCAP (&__real complex_struct.f[0]);
NONCAP (&__imag complex_struct.f[1]);

//----------------------------------------------------------

int (*__capability int_arr_cap_arr[100])[200];
int (*int_arr_ptr_arr[100])[200];
int int_arr_arr[100][200];

NONCAP (&int_arr_cap_arr);
NONCAP (&int_arr_ptr_arr);
NONCAP (&int_arr_arr);

NONCAP (int_arr_cap_arr + 0);
NONCAP (int_arr_ptr_arr + 0);
NONCAP (int_arr_arr + 0);

NONCAP (&*int_arr_cap_arr);
NONCAP (&*int_arr_ptr_arr);
NONCAP (&*int_arr_arr);

NONCAP (&int_arr_cap_arr[0]);
NONCAP (&int_arr_ptr_arr[0]);
NONCAP (&int_arr_arr[0]);

CAP (**int_arr_cap_arr + 0);
NONCAP (**int_arr_ptr_arr + 0);

CAP (*int_arr_cap_arr[0] + 0);
NONCAP (*int_arr_ptr_arr[0] + 0);
NONCAP (*int_arr_arr + 0);

CAP (int_arr_cap_arr[0][0] + 0);
NONCAP (int_arr_ptr_arr[0][0] + 0);
NONCAP (int_arr_arr[0] + 0);

CAP (&**int_arr_cap_arr);
NONCAP (&**int_arr_ptr_arr);
NONCAP (&**int_arr_arr);

CAP (&*int_arr_cap_arr[0]);
NONCAP (&*int_arr_ptr_arr[0]);
NONCAP (&*int_arr_arr[0]);

CAP (&int_arr_cap_arr[0][0]);
NONCAP (&int_arr_ptr_arr[0][0]);
NONCAP (&int_arr_arr[0][0]);

CAP (&***int_arr_cap_arr);
NONCAP (&***int_arr_ptr_arr);

CAP (&**int_arr_cap_arr[0]);
NONCAP (&**int_arr_ptr_arr[0]);

CAP (&*int_arr_cap_arr[0][0]);
NONCAP (&*int_arr_ptr_arr[0][0]);

CAP (&int_arr_cap_arr[0][0][0]);
NONCAP (&int_arr_ptr_arr[0][0][0]);

//----------------------------------------------------------

struct s {
  struct s *__capability link_cap;
  struct s *link_ptr;
};
struct s *__capability root_cap;
struct s *root_ptr;
struct s root_var;
struct s root_arr[4];

NONCAP (&root_cap);
NONCAP (&root_ptr);
NONCAP (&root_var);
NONCAP (&root_arr);

CAP (&*root_cap);
NONCAP (&*root_ptr);
NONCAP (&*root_arr);

CAP (&root_cap->link_cap);
CAP (&root_cap->link_ptr);
NONCAP (&root_ptr->link_cap);
NONCAP (&root_ptr->link_ptr);
NONCAP (&root_var.link_cap);
NONCAP (&root_var.link_ptr);
NONCAP (&root_arr->link_cap);
NONCAP (&root_arr->link_ptr);
NONCAP (&root_arr[0].link_cap);
NONCAP (&root_arr[0].link_ptr);

CAP (&*root_cap->link_cap);
NONCAP (&*root_cap->link_ptr);
CAP (&*root_ptr->link_cap);
NONCAP (&*root_ptr->link_ptr);
CAP (&*root_var.link_cap);
NONCAP (&*root_var.link_ptr);
CAP (&*root_arr->link_cap);
NONCAP (&*root_arr->link_ptr);
CAP (&*root_arr[0].link_cap);
NONCAP (&*root_arr[0].link_ptr);

CAP (&root_cap->link_cap->link_cap);
NONCAP (&root_cap->link_ptr->link_cap);
CAP (&root_ptr->link_cap->link_cap);
NONCAP (&root_ptr->link_ptr->link_cap);
CAP (&root_var.link_cap->link_cap);
NONCAP (&root_var.link_ptr->link_cap);
CAP (&root_arr->link_cap->link_cap);
NONCAP (&root_arr->link_ptr->link_cap);
CAP (&root_arr[0].link_cap->link_cap);
NONCAP (&root_arr[0].link_ptr->link_cap);

CAP (&root_cap->link_cap->link_ptr);
NONCAP (&root_cap->link_ptr->link_ptr);
CAP (&root_ptr->link_cap->link_ptr);
NONCAP (&root_ptr->link_ptr->link_ptr);
CAP (&root_var.link_cap->link_ptr);
NONCAP (&root_var.link_ptr->link_ptr);
CAP (&root_arr->link_cap->link_ptr);
NONCAP (&root_arr->link_ptr->link_ptr);
CAP (&root_arr[0].link_cap->link_ptr);
NONCAP (&root_arr[0].link_ptr->link_ptr);
