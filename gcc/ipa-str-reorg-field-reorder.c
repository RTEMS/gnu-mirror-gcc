/* Interprocedural scalar replacement of aggregates
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

  Contributed by Your Name & Email address go here

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "gimple-iterator.h"
#include "pretty-print.h"
#include <vector>
#include <map>
#include <set>
#include "ipa-structure-reorg.h"
#include "dumpfile.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "langhooks.h"

int
str_reorg_field_reorder (Info *info)
{
  // TBD
  // DEBUG ( "Running str_reorg_field_reorder\n");
  return 0;
}

#if USE_NEW_INTERFACE
int
str_reorg_field_reorder_qual (Info *info)
{
  return 0;
}
int
str_reorg_field_reorder_trans (Info *info)
{
  return 0;
}
#endif
