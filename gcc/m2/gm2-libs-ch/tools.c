/* tools.c provide routines for the tools for stage2 build.

Copyright (C) 2010-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "gm2-libs-host.h"

#if defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#if defined(HAVE_STDDEF_H)
/* to obtain a definition for NULL */
# include <stddef.h>
#endif

#if defined(HAVE_STDIO_H)
/* to obtain a definition for NULL */
# include <stdio.h>
#endif

#if defined(HAVE_STDLIB_H)
/* to obtain a prototype for free and malloc */
# include <stdlib.h>
#endif

#if defined(HAVE_STRING_H)
/* to obtain a definition for NULL */
# include <string.h>
#endif

#if defined(HAVE_STDLIB_H)
/* to obtain a prototype for free and malloc */
# include <stdlib.h>
#endif

#if defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#if defined(HAVE_STDDEF_H)
/* to obtain a definition for NULL */
# include <stddef.h>
#endif

#if defined(HAVE_UNISTD_H)
# include <unistd.h>
#endif

#if !defined(NULL)
# define NULL (void *)0
#endif

#if defined(HAVE_SELECT)
# define FDSET_T fd_set
#else
# define FDSET_T void
#endif

void fancy_abort(char const*, int, char const*)
{
  fprintf (stderr, "fancy_abort called\n");
  exit (1);
}
