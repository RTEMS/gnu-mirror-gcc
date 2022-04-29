int f1(__intcap_t i) {
  switch (i)
    {
    case 1: return 1;
    case 4: return 2;
    default: return -1;
    }
}

int f2(__uintcap_t u) {
  switch (u)
    {
    case 1: return 1;
    case 4: return 2;
    default: return -1;
    }
}

int f3(void *__capability c) {
  switch (c) // { dg-error {switch quantity not an integer} }
    {
    case 1: return 1;
    case 4: return 2;
    default: return -1;
    }
}
