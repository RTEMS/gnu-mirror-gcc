..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

Miscellaneous poly_int routines
*******************************

:samp:`print_dec ({value}, {file}, {sign})` :samp:`print_dec ({value}, {file})`
  Print :samp:`{value}` to :samp:`{file}` as a decimal value, interpreting
  the coefficients according to :samp:`{sign}`.  The final argument is
  optional if :samp:`{value}` has an inherent sign; for example,
  ``poly_int64`` values print as signed by default and
  ``poly_uint64`` values print as unsigned by default.

  This is a simply a ``poly_int`` version of a wide-int routine.