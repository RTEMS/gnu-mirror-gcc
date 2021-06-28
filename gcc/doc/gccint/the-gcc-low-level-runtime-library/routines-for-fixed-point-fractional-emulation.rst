..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fixed-point-fractional-library-routines:

Routines for fixed-point fractional emulation
*********************************************

.. index:: fixed-point fractional library

.. index:: fractional types

.. index:: Embedded C

The software fixed-point library implements fixed-point fractional
arithmetic, and is only activated on selected targets.

For ease of comprehension ``fract`` is an alias for the
``_Fract`` type, ``accum`` an alias for ``_Accum``, and
``sat`` an alias for ``_Sat``.

For illustrative purposes, in this section the fixed-point fractional type
``short fract`` is assumed to correspond to machine mode ``QQmode`` ;
``unsigned short fract`` to ``UQQmode`` ;
``fract`` to ``HQmode`` ;
``unsigned fract`` to ``UHQmode`` ;
``long fract`` to ``SQmode`` ;
``unsigned long fract`` to ``USQmode`` ;
``long long fract`` to ``DQmode`` ;
and ``unsigned long long fract`` to ``UDQmode``.
Similarly the fixed-point accumulator type
``short accum`` corresponds to ``HAmode`` ;
``unsigned short accum`` to ``UHAmode`` ;
``accum`` to ``SAmode`` ;
``unsigned accum`` to ``USAmode`` ;
``long accum`` to ``DAmode`` ;
``unsigned long accum`` to ``UDAmode`` ;
``long long accum`` to ``TAmode`` ;
and ``unsigned long long accum`` to ``UTAmode``.

Arithmetic functions
^^^^^^^^^^^^^^^^^^^^

.. function:: short fract __addqq3 (short fract a, short fract b)

.. function:: fract __addhq3 (fract a, fract b)

.. function:: long fract __addsq3 (long fract a, long fract b)

.. function:: long long fract __adddq3 (long long fract a, long long fract b)

.. function:: unsigned short fract __adduqq3 (unsigned short fract a, unsigned short fract b)

.. function:: unsigned fract __adduhq3 (unsigned fract a, unsigned fract b)

.. function:: unsigned long fract __addusq3 (unsigned long fract a, unsigned long fract b)

.. function:: unsigned long long fract __addudq3 (unsigned long long fract a, unsigned long long fract b)

.. function:: short accum __addha3 (short accum a, short accum b)

.. function:: accum __addsa3 (accum a, accum b)

.. function:: long accum __addda3 (long accum a, long accum b)

.. function:: long long accum __addta3 (long long accum a, long long accum b)

.. function:: unsigned short accum __adduha3 (unsigned short accum a, unsigned short accum b)

.. function:: unsigned accum __addusa3 (unsigned accum a, unsigned accum b)

.. function:: unsigned long accum __adduda3 (unsigned long accum a, unsigned long accum b)

.. function:: unsigned long long accum __adduta3 (unsigned long long accum a, unsigned long long accum b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}`.

.. function:: short fract __ssaddqq3 (short fract a, short fract b)

.. function:: fract __ssaddhq3 (fract a, fract b)

.. function:: long fract __ssaddsq3 (long fract a, long fract b)

.. function:: long long fract __ssadddq3 (long long fract a, long long fract b)

.. function:: short accum __ssaddha3 (short accum a, short accum b)

.. function:: accum __ssaddsa3 (accum a, accum b)

.. function:: long accum __ssaddda3 (long accum a, long accum b)

.. function:: long long accum __ssaddta3 (long long accum a, long long accum b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}` with signed saturation.

.. function:: unsigned short fract __usadduqq3 (unsigned short fract a, unsigned short fract b)

.. function:: unsigned fract __usadduhq3 (unsigned fract a, unsigned fract b)

.. function:: unsigned long fract __usaddusq3 (unsigned long fract a, unsigned long fract b)

.. function:: unsigned long long fract __usaddudq3 (unsigned long long fract a, unsigned long long fract b)

.. function:: unsigned short accum __usadduha3 (unsigned short accum a, unsigned short accum b)

.. function:: unsigned accum __usaddusa3 (unsigned accum a, unsigned accum b)

.. function:: unsigned long accum __usadduda3 (unsigned long accum a, unsigned long accum b)

.. function:: unsigned long long accum __usadduta3 (unsigned long long accum a, unsigned long long accum b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}` with unsigned saturation.

.. function:: short fract __subqq3 (short fract a, short fract b)

.. function:: fract __subhq3 (fract a, fract b)

.. function:: long fract __subsq3 (long fract a, long fract b)

.. function:: long long fract __subdq3 (long long fract a, long long fract b)

.. function:: unsigned short fract __subuqq3 (unsigned short fract a, unsigned short fract b)

.. function:: unsigned fract __subuhq3 (unsigned fract a, unsigned fract b)

.. function:: unsigned long fract __subusq3 (unsigned long fract a, unsigned long fract b)

.. function:: unsigned long long fract __subudq3 (unsigned long long fract a, unsigned long long fract b)

.. function:: short accum __subha3 (short accum a, short accum b)

.. function:: accum __subsa3 (accum a, accum b)

.. function:: long accum __subda3 (long accum a, long accum b)

.. function:: long long accum __subta3 (long long accum a, long long accum b)

.. function:: unsigned short accum __subuha3 (unsigned short accum a, unsigned short accum b)

.. function:: unsigned accum __subusa3 (unsigned accum a, unsigned accum b)

.. function:: unsigned long accum __subuda3 (unsigned long accum a, unsigned long accum b)

.. function:: unsigned long long accum __subuta3 (unsigned long long accum a, unsigned long long accum b)

  These functions return the difference of :samp:`{a}` and :samp:`{b}` ;
  that is, ``a - b``.

.. function:: short fract __sssubqq3 (short fract a, short fract b)

.. function:: fract __sssubhq3 (fract a, fract b)

.. function:: long fract __sssubsq3 (long fract a, long fract b)

.. function:: long long fract __sssubdq3 (long long fract a, long long fract b)

.. function:: short accum __sssubha3 (short accum a, short accum b)

.. function:: accum __sssubsa3 (accum a, accum b)

.. function:: long accum __sssubda3 (long accum a, long accum b)

.. function:: long long accum __sssubta3 (long long accum a, long long accum b)

  These functions return the difference of :samp:`{a}` and :samp:`{b}` with signed
  saturation;  that is, ``a - b``.

.. function:: unsigned short fract __ussubuqq3 (unsigned short fract a, unsigned short fract b)

.. function:: unsigned fract __ussubuhq3 (unsigned fract a, unsigned fract b)

.. function:: unsigned long fract __ussubusq3 (unsigned long fract a, unsigned long fract b)

.. function:: unsigned long long fract __ussubudq3 (unsigned long long fract a, unsigned long long fract b)

.. function:: unsigned short accum __ussubuha3 (unsigned short accum a, unsigned short accum b)

.. function:: unsigned accum __ussubusa3 (unsigned accum a, unsigned accum b)

.. function:: unsigned long accum __ussubuda3 (unsigned long accum a, unsigned long accum b)

.. function:: unsigned long long accum __ussubuta3 (unsigned long long accum a, unsigned long long accum b)

  These functions return the difference of :samp:`{a}` and :samp:`{b}` with unsigned
  saturation;  that is, ``a - b``.

.. function:: short fract __mulqq3 (short fract a, short fract b)

.. function:: fract __mulhq3 (fract a, fract b)

.. function:: long fract __mulsq3 (long fract a, long fract b)

.. function:: long long fract __muldq3 (long long fract a, long long fract b)

.. function:: unsigned short fract __muluqq3 (unsigned short fract a, unsigned short fract b)

.. function:: unsigned fract __muluhq3 (unsigned fract a, unsigned fract b)

.. function:: unsigned long fract __mulusq3 (unsigned long fract a, unsigned long fract b)

.. function:: unsigned long long fract __muludq3 (unsigned long long fract a, unsigned long long fract b)

.. function:: short accum __mulha3 (short accum a, short accum b)

.. function:: accum __mulsa3 (accum a, accum b)

.. function:: long accum __mulda3 (long accum a, long accum b)

.. function:: long long accum __multa3 (long long accum a, long long accum b)

.. function:: unsigned short accum __muluha3 (unsigned short accum a, unsigned short accum b)

.. function:: unsigned accum __mulusa3 (unsigned accum a, unsigned accum b)

.. function:: unsigned long accum __muluda3 (unsigned long accum a, unsigned long accum b)

.. function:: unsigned long long accum __muluta3 (unsigned long long accum a, unsigned long long accum b)

  These functions return the product of :samp:`{a}` and :samp:`{b}`.

.. function:: short fract __ssmulqq3 (short fract a, short fract b)

.. function:: fract __ssmulhq3 (fract a, fract b)

.. function:: long fract __ssmulsq3 (long fract a, long fract b)

.. function:: long long fract __ssmuldq3 (long long fract a, long long fract b)

.. function:: short accum __ssmulha3 (short accum a, short accum b)

.. function:: accum __ssmulsa3 (accum a, accum b)

.. function:: long accum __ssmulda3 (long accum a, long accum b)

.. function:: long long accum __ssmulta3 (long long accum a, long long accum b)

  These functions return the product of :samp:`{a}` and :samp:`{b}` with signed
  saturation.

.. function:: unsigned short fract __usmuluqq3 (unsigned short fract a, unsigned short fract b)

.. function:: unsigned fract __usmuluhq3 (unsigned fract a, unsigned fract b)

.. function:: unsigned long fract __usmulusq3 (unsigned long fract a, unsigned long fract b)

.. function:: unsigned long long fract __usmuludq3 (unsigned long long fract a, unsigned long long fract b)

.. function:: unsigned short accum __usmuluha3 (unsigned short accum a, unsigned short accum b)

.. function:: unsigned accum __usmulusa3 (unsigned accum a, unsigned accum b)

.. function:: unsigned long accum __usmuluda3 (unsigned long accum a, unsigned long accum b)

.. function:: unsigned long long accum __usmuluta3 (unsigned long long accum a, unsigned long long accum b)

  These functions return the product of :samp:`{a}` and :samp:`{b}` with unsigned
  saturation.

.. function:: short fract __divqq3 (short fract a, short fract b)

.. function:: fract __divhq3 (fract a, fract b)

.. function:: long fract __divsq3 (long fract a, long fract b)

.. function:: long long fract __divdq3 (long long fract a, long long fract b)

.. function:: short accum __divha3 (short accum a, short accum b)

.. function:: accum __divsa3 (accum a, accum b)

.. function:: long accum __divda3 (long accum a, long accum b)

.. function:: long long accum __divta3 (long long accum a, long long accum b)

  These functions return the quotient of the signed division of :samp:`{a}`
  and :samp:`{b}`.

.. function:: unsigned short fract __udivuqq3 (unsigned short fract a, unsigned short fract b)

.. function:: unsigned fract __udivuhq3 (unsigned fract a, unsigned fract b)

.. function:: unsigned long fract __udivusq3 (unsigned long fract a, unsigned long fract b)

.. function:: unsigned long long fract __udivudq3 (unsigned long long fract a, unsigned long long fract b)

.. function:: unsigned short accum __udivuha3 (unsigned short accum a, unsigned short accum b)

.. function:: unsigned accum __udivusa3 (unsigned accum a, unsigned accum b)

.. function:: unsigned long accum __udivuda3 (unsigned long accum a, unsigned long accum b)

.. function:: unsigned long long accum __udivuta3 (unsigned long long accum a, unsigned long long accum b)

  These functions return the quotient of the unsigned division of :samp:`{a}`
  and :samp:`{b}`.

.. function:: short fract __ssdivqq3 (short fract a, short fract b)

.. function:: fract __ssdivhq3 (fract a, fract b)

.. function:: long fract __ssdivsq3 (long fract a, long fract b)

.. function:: long long fract __ssdivdq3 (long long fract a, long long fract b)

.. function:: short accum __ssdivha3 (short accum a, short accum b)

.. function:: accum __ssdivsa3 (accum a, accum b)

.. function:: long accum __ssdivda3 (long accum a, long accum b)

.. function:: long long accum __ssdivta3 (long long accum a, long long accum b)

  These functions return the quotient of the signed division of :samp:`{a}`
  and :samp:`{b}` with signed saturation.

.. function:: unsigned short fract __usdivuqq3 (unsigned short fract a, unsigned short fract b)

.. function:: unsigned fract __usdivuhq3 (unsigned fract a, unsigned fract b)

.. function:: unsigned long fract __usdivusq3 (unsigned long fract a, unsigned long fract b)

.. function:: unsigned long long fract __usdivudq3 (unsigned long long fract a, unsigned long long fract b)

.. function:: unsigned short accum __usdivuha3 (unsigned short accum a, unsigned short accum b)

.. function:: unsigned accum __usdivusa3 (unsigned accum a, unsigned accum b)

.. function:: unsigned long accum __usdivuda3 (unsigned long accum a, unsigned long accum b)

.. function:: unsigned long long accum __usdivuta3 (unsigned long long accum a, unsigned long long accum b)

  These functions return the quotient of the unsigned division of :samp:`{a}`
  and :samp:`{b}` with unsigned saturation.

.. function:: short fract __negqq2 (short fract a)

.. function:: fract __neghq2 (fract a)

.. function:: long fract __negsq2 (long fract a)

.. function:: long long fract __negdq2 (long long fract a)

.. function:: unsigned short fract __neguqq2 (unsigned short fract a)

.. function:: unsigned fract __neguhq2 (unsigned fract a)

.. function:: unsigned long fract __negusq2 (unsigned long fract a)

.. function:: unsigned long long fract __negudq2 (unsigned long long fract a)

.. function:: short accum __negha2 (short accum a)

.. function:: accum __negsa2 (accum a)

.. function:: long accum __negda2 (long accum a)

.. function:: long long accum __negta2 (long long accum a)

.. function:: unsigned short accum __neguha2 (unsigned short accum a)

.. function:: unsigned accum __negusa2 (unsigned accum a)

.. function:: unsigned long accum __neguda2 (unsigned long accum a)

.. function:: unsigned long long accum __neguta2 (unsigned long long accum a)

  These functions return the negation of :samp:`{a}`.

.. function:: short fract __ssnegqq2 (short fract a)

.. function:: fract __ssneghq2 (fract a)

.. function:: long fract __ssnegsq2 (long fract a)

.. function:: long long fract __ssnegdq2 (long long fract a)

.. function:: short accum __ssnegha2 (short accum a)

.. function:: accum __ssnegsa2 (accum a)

.. function:: long accum __ssnegda2 (long accum a)

.. function:: long long accum __ssnegta2 (long long accum a)

  These functions return the negation of :samp:`{a}` with signed saturation.

.. function:: unsigned short fract __usneguqq2 (unsigned short fract a)

.. function:: unsigned fract __usneguhq2 (unsigned fract a)

.. function:: unsigned long fract __usnegusq2 (unsigned long fract a)

.. function:: unsigned long long fract __usnegudq2 (unsigned long long fract a)

.. function:: unsigned short accum __usneguha2 (unsigned short accum a)

.. function:: unsigned accum __usnegusa2 (unsigned accum a)

.. function:: unsigned long accum __usneguda2 (unsigned long accum a)

.. function:: unsigned long long accum __usneguta2 (unsigned long long accum a)

  These functions return the negation of :samp:`{a}` with unsigned saturation.

.. function:: short fract __ashlqq3 (short fract a, int b)

.. function:: fract __ashlhq3 (fract a, int b)

.. function:: long fract __ashlsq3 (long fract a, int b)

.. function:: long long fract __ashldq3 (long long fract a, int b)

.. function:: unsigned short fract __ashluqq3 (unsigned short fract a, int b)

.. function:: unsigned fract __ashluhq3 (unsigned fract a, int b)

.. function:: unsigned long fract __ashlusq3 (unsigned long fract a, int b)

.. function:: unsigned long long fract __ashludq3 (unsigned long long fract a, int b)

.. function:: short accum __ashlha3 (short accum a, int b)

.. function:: accum __ashlsa3 (accum a, int b)

.. function:: long accum __ashlda3 (long accum a, int b)

.. function:: long long accum __ashlta3 (long long accum a, int b)

.. function:: unsigned short accum __ashluha3 (unsigned short accum a, int b)

.. function:: unsigned accum __ashlusa3 (unsigned accum a, int b)

.. function:: unsigned long accum __ashluda3 (unsigned long accum a, int b)

.. function:: unsigned long long accum __ashluta3 (unsigned long long accum a, int b)

  These functions return the result of shifting :samp:`{a}` left by :samp:`{b}` bits.

.. function:: short fract __ashrqq3 (short fract a, int b)

.. function:: fract __ashrhq3 (fract a, int b)

.. function:: long fract __ashrsq3 (long fract a, int b)

.. function:: long long fract __ashrdq3 (long long fract a, int b)

.. function:: short accum __ashrha3 (short accum a, int b)

.. function:: accum __ashrsa3 (accum a, int b)

.. function:: long accum __ashrda3 (long accum a, int b)

.. function:: long long accum __ashrta3 (long long accum a, int b)

  These functions return the result of arithmetically shifting :samp:`{a}` right
  by :samp:`{b}` bits.

.. function:: unsigned short fract __lshruqq3 (unsigned short fract a, int b)

.. function:: unsigned fract __lshruhq3 (unsigned fract a, int b)

.. function:: unsigned long fract __lshrusq3 (unsigned long fract a, int b)

.. function:: unsigned long long fract __lshrudq3 (unsigned long long fract a, int b)

.. function:: unsigned short accum __lshruha3 (unsigned short accum a, int b)

.. function:: unsigned accum __lshrusa3 (unsigned accum a, int b)

.. function:: unsigned long accum __lshruda3 (unsigned long accum a, int b)

.. function:: unsigned long long accum __lshruta3 (unsigned long long accum a, int b)

  These functions return the result of logically shifting :samp:`{a}` right
  by :samp:`{b}` bits.

.. function:: fract __ssashlhq3 (fract a, int b)

.. function:: long fract __ssashlsq3 (long fract a, int b)

.. function:: long long fract __ssashldq3 (long long fract a, int b)

.. function:: short accum __ssashlha3 (short accum a, int b)

.. function:: accum __ssashlsa3 (accum a, int b)

.. function:: long accum __ssashlda3 (long accum a, int b)

.. function:: long long accum __ssashlta3 (long long accum a, int b)

  These functions return the result of shifting :samp:`{a}` left by :samp:`{b}` bits
  with signed saturation.

.. function:: unsigned short fract __usashluqq3 (unsigned short fract a, int b)

.. function:: unsigned fract __usashluhq3 (unsigned fract a, int b)

.. function:: unsigned long fract __usashlusq3 (unsigned long fract a, int b)

.. function:: unsigned long long fract __usashludq3 (unsigned long long fract a, int b)

.. function:: unsigned short accum __usashluha3 (unsigned short accum a, int b)

.. function:: unsigned accum __usashlusa3 (unsigned accum a, int b)

.. function:: unsigned long accum __usashluda3 (unsigned long accum a, int b)

.. function:: unsigned long long accum __usashluta3 (unsigned long long accum a, int b)

  These functions return the result of shifting :samp:`{a}` left by :samp:`{b}` bits
  with unsigned saturation.

Comparison functions
^^^^^^^^^^^^^^^^^^^^

The following functions implement fixed-point comparisons.  These functions
implement a low-level compare, upon which the higher level comparison
operators (such as less than and greater than or equal to) can be
constructed.  The returned values lie in the range zero to two, to allow
the high-level operators to be implemented by testing the returned
result using either signed or unsigned comparison.

.. function:: int __cmpqq2 (short fract a, short fract b)

.. function:: int __cmphq2 (fract a, fract b)

.. function:: int __cmpsq2 (long fract a, long fract b)

.. function:: int __cmpdq2 (long long fract a, long long fract b)

.. function:: int __cmpuqq2 (unsigned short fract a, unsigned short fract b)

.. function:: int __cmpuhq2 (unsigned fract a, unsigned fract b)

.. function:: int __cmpusq2 (unsigned long fract a, unsigned long fract b)

.. function:: int __cmpudq2 (unsigned long long fract a, unsigned long long fract b)

.. function:: int __cmpha2 (short accum a, short accum b)

.. function:: int __cmpsa2 (accum a, accum b)

.. function:: int __cmpda2 (long accum a, long accum b)

.. function:: int __cmpta2 (long long accum a, long long accum b)

.. function:: int __cmpuha2 (unsigned short accum a, unsigned short accum b)

.. function:: int __cmpusa2 (unsigned accum a, unsigned accum b)

.. function:: int __cmpuda2 (unsigned long accum a, unsigned long accum b)

.. function:: int __cmputa2 (unsigned long long accum a, unsigned long long accum b)

  These functions perform a signed or unsigned comparison of :samp:`{a}` and
  :samp:`{b}` (depending on the selected machine mode).  If :samp:`{a}` is less
  than :samp:`{b}`, they return 0; if :samp:`{a}` is greater than :samp:`{b}`, they
  return 2; and if :samp:`{a}` and :samp:`{b}` are equal they return 1.

Conversion functions
^^^^^^^^^^^^^^^^^^^^

.. function:: fract __fractqqhq2 (short fract a)

.. function:: long fract __fractqqsq2 (short fract a)

.. function:: long long fract __fractqqdq2 (short fract a)

.. function:: short accum __fractqqha (short fract a)

.. function:: accum __fractqqsa (short fract a)

.. function:: long accum __fractqqda (short fract a)

.. function:: long long accum __fractqqta (short fract a)

.. function:: unsigned short fract __fractqquqq (short fract a)

.. function:: unsigned fract __fractqquhq (short fract a)

.. function:: unsigned long fract __fractqqusq (short fract a)

.. function:: unsigned long long fract __fractqqudq (short fract a)

.. function:: unsigned short accum __fractqquha (short fract a)

.. function:: unsigned accum __fractqqusa (short fract a)

.. function:: unsigned long accum __fractqquda (short fract a)

.. function:: unsigned long long accum __fractqquta (short fract a)

.. function:: signed char __fractqqqi (short fract a)

.. function:: short __fractqqhi (short fract a)

.. function:: int __fractqqsi (short fract a)

.. function:: long __fractqqdi (short fract a)

.. function:: long long __fractqqti (short fract a)

.. function:: float __fractqqsf (short fract a)

.. function:: double __fractqqdf (short fract a)

.. function:: short fract __fracthqqq2 (fract a)

.. function:: long fract __fracthqsq2 (fract a)

.. function:: long long fract __fracthqdq2 (fract a)

.. function:: short accum __fracthqha (fract a)

.. function:: accum __fracthqsa (fract a)

.. function:: long accum __fracthqda (fract a)

.. function:: long long accum __fracthqta (fract a)

.. function:: unsigned short fract __fracthquqq (fract a)

.. function:: unsigned fract __fracthquhq (fract a)

.. function:: unsigned long fract __fracthqusq (fract a)

.. function:: unsigned long long fract __fracthqudq (fract a)

.. function:: unsigned short accum __fracthquha (fract a)

.. function:: unsigned accum __fracthqusa (fract a)

.. function:: unsigned long accum __fracthquda (fract a)

.. function:: unsigned long long accum __fracthquta (fract a)

.. function:: signed char __fracthqqi (fract a)

.. function:: short __fracthqhi (fract a)

.. function:: int __fracthqsi (fract a)

.. function:: long __fracthqdi (fract a)

.. function:: long long __fracthqti (fract a)

.. function:: float __fracthqsf (fract a)

.. function:: double __fracthqdf (fract a)

.. function:: short fract __fractsqqq2 (long fract a)

.. function:: fract __fractsqhq2 (long fract a)

.. function:: long long fract __fractsqdq2 (long fract a)

.. function:: short accum __fractsqha (long fract a)

.. function:: accum __fractsqsa (long fract a)

.. function:: long accum __fractsqda (long fract a)

.. function:: long long accum __fractsqta (long fract a)

.. function:: unsigned short fract __fractsquqq (long fract a)

.. function:: unsigned fract __fractsquhq (long fract a)

.. function:: unsigned long fract __fractsqusq (long fract a)

.. function:: unsigned long long fract __fractsqudq (long fract a)

.. function:: unsigned short accum __fractsquha (long fract a)

.. function:: unsigned accum __fractsqusa (long fract a)

.. function:: unsigned long accum __fractsquda (long fract a)

.. function:: unsigned long long accum __fractsquta (long fract a)

.. function:: signed char __fractsqqi (long fract a)

.. function:: short __fractsqhi (long fract a)

.. function:: int __fractsqsi (long fract a)

.. function:: long __fractsqdi (long fract a)

.. function:: long long __fractsqti (long fract a)

.. function:: float __fractsqsf (long fract a)

.. function:: double __fractsqdf (long fract a)

.. function:: short fract __fractdqqq2 (long long fract a)

.. function:: fract __fractdqhq2 (long long fract a)

.. function:: long fract __fractdqsq2 (long long fract a)

.. function:: short accum __fractdqha (long long fract a)

.. function:: accum __fractdqsa (long long fract a)

.. function:: long accum __fractdqda (long long fract a)

.. function:: long long accum __fractdqta (long long fract a)

.. function:: unsigned short fract __fractdquqq (long long fract a)

.. function:: unsigned fract __fractdquhq (long long fract a)

.. function:: unsigned long fract __fractdqusq (long long fract a)

.. function:: unsigned long long fract __fractdqudq (long long fract a)

.. function:: unsigned short accum __fractdquha (long long fract a)

.. function:: unsigned accum __fractdqusa (long long fract a)

.. function:: unsigned long accum __fractdquda (long long fract a)

.. function:: unsigned long long accum __fractdquta (long long fract a)

.. function:: signed char __fractdqqi (long long fract a)

.. function:: short __fractdqhi (long long fract a)

.. function:: int __fractdqsi (long long fract a)

.. function:: long __fractdqdi (long long fract a)

.. function:: long long __fractdqti (long long fract a)

.. function:: float __fractdqsf (long long fract a)

.. function:: double __fractdqdf (long long fract a)

.. function:: short fract __fracthaqq (short accum a)

.. function:: fract __fracthahq (short accum a)

.. function:: long fract __fracthasq (short accum a)

.. function:: long long fract __fracthadq (short accum a)

.. function:: accum __fracthasa2 (short accum a)

.. function:: long accum __fracthada2 (short accum a)

.. function:: long long accum __fracthata2 (short accum a)

.. function:: unsigned short fract __fracthauqq (short accum a)

.. function:: unsigned fract __fracthauhq (short accum a)

.. function:: unsigned long fract __fracthausq (short accum a)

.. function:: unsigned long long fract __fracthaudq (short accum a)

.. function:: unsigned short accum __fracthauha (short accum a)

.. function:: unsigned accum __fracthausa (short accum a)

.. function:: unsigned long accum __fracthauda (short accum a)

.. function:: unsigned long long accum __fracthauta (short accum a)

.. function:: signed char __fracthaqi (short accum a)

.. function:: short __fracthahi (short accum a)

.. function:: int __fracthasi (short accum a)

.. function:: long __fracthadi (short accum a)

.. function:: long long __fracthati (short accum a)

.. function:: float __fracthasf (short accum a)

.. function:: double __fracthadf (short accum a)

.. function:: short fract __fractsaqq (accum a)

.. function:: fract __fractsahq (accum a)

.. function:: long fract __fractsasq (accum a)

.. function:: long long fract __fractsadq (accum a)

.. function:: short accum __fractsaha2 (accum a)

.. function:: long accum __fractsada2 (accum a)

.. function:: long long accum __fractsata2 (accum a)

.. function:: unsigned short fract __fractsauqq (accum a)

.. function:: unsigned fract __fractsauhq (accum a)

.. function:: unsigned long fract __fractsausq (accum a)

.. function:: unsigned long long fract __fractsaudq (accum a)

.. function:: unsigned short accum __fractsauha (accum a)

.. function:: unsigned accum __fractsausa (accum a)

.. function:: unsigned long accum __fractsauda (accum a)

.. function:: unsigned long long accum __fractsauta (accum a)

.. function:: signed char __fractsaqi (accum a)

.. function:: short __fractsahi (accum a)

.. function:: int __fractsasi (accum a)

.. function:: long __fractsadi (accum a)

.. function:: long long __fractsati (accum a)

.. function:: float __fractsasf (accum a)

.. function:: double __fractsadf (accum a)

.. function:: short fract __fractdaqq (long accum a)

.. function:: fract __fractdahq (long accum a)

.. function:: long fract __fractdasq (long accum a)

.. function:: long long fract __fractdadq (long accum a)

.. function:: short accum __fractdaha2 (long accum a)

.. function:: accum __fractdasa2 (long accum a)

.. function:: long long accum __fractdata2 (long accum a)

.. function:: unsigned short fract __fractdauqq (long accum a)

.. function:: unsigned fract __fractdauhq (long accum a)

.. function:: unsigned long fract __fractdausq (long accum a)

.. function:: unsigned long long fract __fractdaudq (long accum a)

.. function:: unsigned short accum __fractdauha (long accum a)

.. function:: unsigned accum __fractdausa (long accum a)

.. function:: unsigned long accum __fractdauda (long accum a)

.. function:: unsigned long long accum __fractdauta (long accum a)

.. function:: signed char __fractdaqi (long accum a)

.. function:: short __fractdahi (long accum a)

.. function:: int __fractdasi (long accum a)

.. function:: long __fractdadi (long accum a)

.. function:: long long __fractdati (long accum a)

.. function:: float __fractdasf (long accum a)

.. function:: double __fractdadf (long accum a)

.. function:: short fract __fracttaqq (long long accum a)

.. function:: fract __fracttahq (long long accum a)

.. function:: long fract __fracttasq (long long accum a)

.. function:: long long fract __fracttadq (long long accum a)

.. function:: short accum __fracttaha2 (long long accum a)

.. function:: accum __fracttasa2 (long long accum a)

.. function:: long accum __fracttada2 (long long accum a)

.. function:: unsigned short fract __fracttauqq (long long accum a)

.. function:: unsigned fract __fracttauhq (long long accum a)

.. function:: unsigned long fract __fracttausq (long long accum a)

.. function:: unsigned long long fract __fracttaudq (long long accum a)

.. function:: unsigned short accum __fracttauha (long long accum a)

.. function:: unsigned accum __fracttausa (long long accum a)

.. function:: unsigned long accum __fracttauda (long long accum a)

.. function:: unsigned long long accum __fracttauta (long long accum a)

.. function:: signed char __fracttaqi (long long accum a)

.. function:: short __fracttahi (long long accum a)

.. function:: int __fracttasi (long long accum a)

.. function:: long __fracttadi (long long accum a)

.. function:: long long __fracttati (long long accum a)

.. function:: float __fracttasf (long long accum a)

.. function:: double __fracttadf (long long accum a)

.. function:: short fract __fractuqqqq (unsigned short fract a)

.. function:: fract __fractuqqhq (unsigned short fract a)

.. function:: long fract __fractuqqsq (unsigned short fract a)

.. function:: long long fract __fractuqqdq (unsigned short fract a)

.. function:: short accum __fractuqqha (unsigned short fract a)

.. function:: accum __fractuqqsa (unsigned short fract a)

.. function:: long accum __fractuqqda (unsigned short fract a)

.. function:: long long accum __fractuqqta (unsigned short fract a)

.. function:: unsigned fract __fractuqquhq2 (unsigned short fract a)

.. function:: unsigned long fract __fractuqqusq2 (unsigned short fract a)

.. function:: unsigned long long fract __fractuqqudq2 (unsigned short fract a)

.. function:: unsigned short accum __fractuqquha (unsigned short fract a)

.. function:: unsigned accum __fractuqqusa (unsigned short fract a)

.. function:: unsigned long accum __fractuqquda (unsigned short fract a)

.. function:: unsigned long long accum __fractuqquta (unsigned short fract a)

.. function:: signed char __fractuqqqi (unsigned short fract a)

.. function:: short __fractuqqhi (unsigned short fract a)

.. function:: int __fractuqqsi (unsigned short fract a)

.. function:: long __fractuqqdi (unsigned short fract a)

.. function:: long long __fractuqqti (unsigned short fract a)

.. function:: float __fractuqqsf (unsigned short fract a)

.. function:: double __fractuqqdf (unsigned short fract a)

.. function:: short fract __fractuhqqq (unsigned fract a)

.. function:: fract __fractuhqhq (unsigned fract a)

.. function:: long fract __fractuhqsq (unsigned fract a)

.. function:: long long fract __fractuhqdq (unsigned fract a)

.. function:: short accum __fractuhqha (unsigned fract a)

.. function:: accum __fractuhqsa (unsigned fract a)

.. function:: long accum __fractuhqda (unsigned fract a)

.. function:: long long accum __fractuhqta (unsigned fract a)

.. function:: unsigned short fract __fractuhquqq2 (unsigned fract a)

.. function:: unsigned long fract __fractuhqusq2 (unsigned fract a)

.. function:: unsigned long long fract __fractuhqudq2 (unsigned fract a)

.. function:: unsigned short accum __fractuhquha (unsigned fract a)

.. function:: unsigned accum __fractuhqusa (unsigned fract a)

.. function:: unsigned long accum __fractuhquda (unsigned fract a)

.. function:: unsigned long long accum __fractuhquta (unsigned fract a)

.. function:: signed char __fractuhqqi (unsigned fract a)

.. function:: short __fractuhqhi (unsigned fract a)

.. function:: int __fractuhqsi (unsigned fract a)

.. function:: long __fractuhqdi (unsigned fract a)

.. function:: long long __fractuhqti (unsigned fract a)

.. function:: float __fractuhqsf (unsigned fract a)

.. function:: double __fractuhqdf (unsigned fract a)

.. function:: short fract __fractusqqq (unsigned long fract a)

.. function:: fract __fractusqhq (unsigned long fract a)

.. function:: long fract __fractusqsq (unsigned long fract a)

.. function:: long long fract __fractusqdq (unsigned long fract a)

.. function:: short accum __fractusqha (unsigned long fract a)

.. function:: accum __fractusqsa (unsigned long fract a)

.. function:: long accum __fractusqda (unsigned long fract a)

.. function:: long long accum __fractusqta (unsigned long fract a)

.. function:: unsigned short fract __fractusquqq2 (unsigned long fract a)

.. function:: unsigned fract __fractusquhq2 (unsigned long fract a)

.. function:: unsigned long long fract __fractusqudq2 (unsigned long fract a)

.. function:: unsigned short accum __fractusquha (unsigned long fract a)

.. function:: unsigned accum __fractusqusa (unsigned long fract a)

.. function:: unsigned long accum __fractusquda (unsigned long fract a)

.. function:: unsigned long long accum __fractusquta (unsigned long fract a)

.. function:: signed char __fractusqqi (unsigned long fract a)

.. function:: short __fractusqhi (unsigned long fract a)

.. function:: int __fractusqsi (unsigned long fract a)

.. function:: long __fractusqdi (unsigned long fract a)

.. function:: long long __fractusqti (unsigned long fract a)

.. function:: float __fractusqsf (unsigned long fract a)

.. function:: double __fractusqdf (unsigned long fract a)

.. function:: short fract __fractudqqq (unsigned long long fract a)

.. function:: fract __fractudqhq (unsigned long long fract a)

.. function:: long fract __fractudqsq (unsigned long long fract a)

.. function:: long long fract __fractudqdq (unsigned long long fract a)

.. function:: short accum __fractudqha (unsigned long long fract a)

.. function:: accum __fractudqsa (unsigned long long fract a)

.. function:: long accum __fractudqda (unsigned long long fract a)

.. function:: long long accum __fractudqta (unsigned long long fract a)

.. function:: unsigned short fract __fractudquqq2 (unsigned long long fract a)

.. function:: unsigned fract __fractudquhq2 (unsigned long long fract a)

.. function:: unsigned long fract __fractudqusq2 (unsigned long long fract a)

.. function:: unsigned short accum __fractudquha (unsigned long long fract a)

.. function:: unsigned accum __fractudqusa (unsigned long long fract a)

.. function:: unsigned long accum __fractudquda (unsigned long long fract a)

.. function:: unsigned long long accum __fractudquta (unsigned long long fract a)

.. function:: signed char __fractudqqi (unsigned long long fract a)

.. function:: short __fractudqhi (unsigned long long fract a)

.. function:: int __fractudqsi (unsigned long long fract a)

.. function:: long __fractudqdi (unsigned long long fract a)

.. function:: long long __fractudqti (unsigned long long fract a)

.. function:: float __fractudqsf (unsigned long long fract a)

.. function:: double __fractudqdf (unsigned long long fract a)

.. function:: short fract __fractuhaqq (unsigned short accum a)

.. function:: fract __fractuhahq (unsigned short accum a)

.. function:: long fract __fractuhasq (unsigned short accum a)

.. function:: long long fract __fractuhadq (unsigned short accum a)

.. function:: short accum __fractuhaha (unsigned short accum a)

.. function:: accum __fractuhasa (unsigned short accum a)

.. function:: long accum __fractuhada (unsigned short accum a)

.. function:: long long accum __fractuhata (unsigned short accum a)

.. function:: unsigned short fract __fractuhauqq (unsigned short accum a)

.. function:: unsigned fract __fractuhauhq (unsigned short accum a)

.. function:: unsigned long fract __fractuhausq (unsigned short accum a)

.. function:: unsigned long long fract __fractuhaudq (unsigned short accum a)

.. function:: unsigned accum __fractuhausa2 (unsigned short accum a)

.. function:: unsigned long accum __fractuhauda2 (unsigned short accum a)

.. function:: unsigned long long accum __fractuhauta2 (unsigned short accum a)

.. function:: signed char __fractuhaqi (unsigned short accum a)

.. function:: short __fractuhahi (unsigned short accum a)

.. function:: int __fractuhasi (unsigned short accum a)

.. function:: long __fractuhadi (unsigned short accum a)

.. function:: long long __fractuhati (unsigned short accum a)

.. function:: float __fractuhasf (unsigned short accum a)

.. function:: double __fractuhadf (unsigned short accum a)

.. function:: short fract __fractusaqq (unsigned accum a)

.. function:: fract __fractusahq (unsigned accum a)

.. function:: long fract __fractusasq (unsigned accum a)

.. function:: long long fract __fractusadq (unsigned accum a)

.. function:: short accum __fractusaha (unsigned accum a)

.. function:: accum __fractusasa (unsigned accum a)

.. function:: long accum __fractusada (unsigned accum a)

.. function:: long long accum __fractusata (unsigned accum a)

.. function:: unsigned short fract __fractusauqq (unsigned accum a)

.. function:: unsigned fract __fractusauhq (unsigned accum a)

.. function:: unsigned long fract __fractusausq (unsigned accum a)

.. function:: unsigned long long fract __fractusaudq (unsigned accum a)

.. function:: unsigned short accum __fractusauha2 (unsigned accum a)

.. function:: unsigned long accum __fractusauda2 (unsigned accum a)

.. function:: unsigned long long accum __fractusauta2 (unsigned accum a)

.. function:: signed char __fractusaqi (unsigned accum a)

.. function:: short __fractusahi (unsigned accum a)

.. function:: int __fractusasi (unsigned accum a)

.. function:: long __fractusadi (unsigned accum a)

.. function:: long long __fractusati (unsigned accum a)

.. function:: float __fractusasf (unsigned accum a)

.. function:: double __fractusadf (unsigned accum a)

.. function:: short fract __fractudaqq (unsigned long accum a)

.. function:: fract __fractudahq (unsigned long accum a)

.. function:: long fract __fractudasq (unsigned long accum a)

.. function:: long long fract __fractudadq (unsigned long accum a)

.. function:: short accum __fractudaha (unsigned long accum a)

.. function:: accum __fractudasa (unsigned long accum a)

.. function:: long accum __fractudada (unsigned long accum a)

.. function:: long long accum __fractudata (unsigned long accum a)

.. function:: unsigned short fract __fractudauqq (unsigned long accum a)

.. function:: unsigned fract __fractudauhq (unsigned long accum a)

.. function:: unsigned long fract __fractudausq (unsigned long accum a)

.. function:: unsigned long long fract __fractudaudq (unsigned long accum a)

.. function:: unsigned short accum __fractudauha2 (unsigned long accum a)

.. function:: unsigned accum __fractudausa2 (unsigned long accum a)

.. function:: unsigned long long accum __fractudauta2 (unsigned long accum a)

.. function:: signed char __fractudaqi (unsigned long accum a)

.. function:: short __fractudahi (unsigned long accum a)

.. function:: int __fractudasi (unsigned long accum a)

.. function:: long __fractudadi (unsigned long accum a)

.. function:: long long __fractudati (unsigned long accum a)

.. function:: float __fractudasf (unsigned long accum a)

.. function:: double __fractudadf (unsigned long accum a)

.. function:: short fract __fractutaqq (unsigned long long accum a)

.. function:: fract __fractutahq (unsigned long long accum a)

.. function:: long fract __fractutasq (unsigned long long accum a)

.. function:: long long fract __fractutadq (unsigned long long accum a)

.. function:: short accum __fractutaha (unsigned long long accum a)

.. function:: accum __fractutasa (unsigned long long accum a)

.. function:: long accum __fractutada (unsigned long long accum a)

.. function:: long long accum __fractutata (unsigned long long accum a)

.. function:: unsigned short fract __fractutauqq (unsigned long long accum a)

.. function:: unsigned fract __fractutauhq (unsigned long long accum a)

.. function:: unsigned long fract __fractutausq (unsigned long long accum a)

.. function:: unsigned long long fract __fractutaudq (unsigned long long accum a)

.. function:: unsigned short accum __fractutauha2 (unsigned long long accum a)

.. function:: unsigned accum __fractutausa2 (unsigned long long accum a)

.. function:: unsigned long accum __fractutauda2 (unsigned long long accum a)

.. function:: signed char __fractutaqi (unsigned long long accum a)

.. function:: short __fractutahi (unsigned long long accum a)

.. function:: int __fractutasi (unsigned long long accum a)

.. function:: long __fractutadi (unsigned long long accum a)

.. function:: long long __fractutati (unsigned long long accum a)

.. function:: float __fractutasf (unsigned long long accum a)

.. function:: double __fractutadf (unsigned long long accum a)

.. function:: short fract __fractqiqq (signed char a)

.. function:: fract __fractqihq (signed char a)

.. function:: long fract __fractqisq (signed char a)

.. function:: long long fract __fractqidq (signed char a)

.. function:: short accum __fractqiha (signed char a)

.. function:: accum __fractqisa (signed char a)

.. function:: long accum __fractqida (signed char a)

.. function:: long long accum __fractqita (signed char a)

.. function:: unsigned short fract __fractqiuqq (signed char a)

.. function:: unsigned fract __fractqiuhq (signed char a)

.. function:: unsigned long fract __fractqiusq (signed char a)

.. function:: unsigned long long fract __fractqiudq (signed char a)

.. function:: unsigned short accum __fractqiuha (signed char a)

.. function:: unsigned accum __fractqiusa (signed char a)

.. function:: unsigned long accum __fractqiuda (signed char a)

.. function:: unsigned long long accum __fractqiuta (signed char a)

.. function:: short fract __fracthiqq (short a)

.. function:: fract __fracthihq (short a)

.. function:: long fract __fracthisq (short a)

.. function:: long long fract __fracthidq (short a)

.. function:: short accum __fracthiha (short a)

.. function:: accum __fracthisa (short a)

.. function:: long accum __fracthida (short a)

.. function:: long long accum __fracthita (short a)

.. function:: unsigned short fract __fracthiuqq (short a)

.. function:: unsigned fract __fracthiuhq (short a)

.. function:: unsigned long fract __fracthiusq (short a)

.. function:: unsigned long long fract __fracthiudq (short a)

.. function:: unsigned short accum __fracthiuha (short a)

.. function:: unsigned accum __fracthiusa (short a)

.. function:: unsigned long accum __fracthiuda (short a)

.. function:: unsigned long long accum __fracthiuta (short a)

.. function:: short fract __fractsiqq (int a)

.. function:: fract __fractsihq (int a)

.. function:: long fract __fractsisq (int a)

.. function:: long long fract __fractsidq (int a)

.. function:: short accum __fractsiha (int a)

.. function:: accum __fractsisa (int a)

.. function:: long accum __fractsida (int a)

.. function:: long long accum __fractsita (int a)

.. function:: unsigned short fract __fractsiuqq (int a)

.. function:: unsigned fract __fractsiuhq (int a)

.. function:: unsigned long fract __fractsiusq (int a)

.. function:: unsigned long long fract __fractsiudq (int a)

.. function:: unsigned short accum __fractsiuha (int a)

.. function:: unsigned accum __fractsiusa (int a)

.. function:: unsigned long accum __fractsiuda (int a)

.. function:: unsigned long long accum __fractsiuta (int a)

.. function:: short fract __fractdiqq (long a)

.. function:: fract __fractdihq (long a)

.. function:: long fract __fractdisq (long a)

.. function:: long long fract __fractdidq (long a)

.. function:: short accum __fractdiha (long a)

.. function:: accum __fractdisa (long a)

.. function:: long accum __fractdida (long a)

.. function:: long long accum __fractdita (long a)

.. function:: unsigned short fract __fractdiuqq (long a)

.. function:: unsigned fract __fractdiuhq (long a)

.. function:: unsigned long fract __fractdiusq (long a)

.. function:: unsigned long long fract __fractdiudq (long a)

.. function:: unsigned short accum __fractdiuha (long a)

.. function:: unsigned accum __fractdiusa (long a)

.. function:: unsigned long accum __fractdiuda (long a)

.. function:: unsigned long long accum __fractdiuta (long a)

.. function:: short fract __fracttiqq (long long a)

.. function:: fract __fracttihq (long long a)

.. function:: long fract __fracttisq (long long a)

.. function:: long long fract __fracttidq (long long a)

.. function:: short accum __fracttiha (long long a)

.. function:: accum __fracttisa (long long a)

.. function:: long accum __fracttida (long long a)

.. function:: long long accum __fracttita (long long a)

.. function:: unsigned short fract __fracttiuqq (long long a)

.. function:: unsigned fract __fracttiuhq (long long a)

.. function:: unsigned long fract __fracttiusq (long long a)

.. function:: unsigned long long fract __fracttiudq (long long a)

.. function:: unsigned short accum __fracttiuha (long long a)

.. function:: unsigned accum __fracttiusa (long long a)

.. function:: unsigned long accum __fracttiuda (long long a)

.. function:: unsigned long long accum __fracttiuta (long long a)

.. function:: short fract __fractsfqq (float a)

.. function:: fract __fractsfhq (float a)

.. function:: long fract __fractsfsq (float a)

.. function:: long long fract __fractsfdq (float a)

.. function:: short accum __fractsfha (float a)

.. function:: accum __fractsfsa (float a)

.. function:: long accum __fractsfda (float a)

.. function:: long long accum __fractsfta (float a)

.. function:: unsigned short fract __fractsfuqq (float a)

.. function:: unsigned fract __fractsfuhq (float a)

.. function:: unsigned long fract __fractsfusq (float a)

.. function:: unsigned long long fract __fractsfudq (float a)

.. function:: unsigned short accum __fractsfuha (float a)

.. function:: unsigned accum __fractsfusa (float a)

.. function:: unsigned long accum __fractsfuda (float a)

.. function:: unsigned long long accum __fractsfuta (float a)

.. function:: short fract __fractdfqq (double a)

.. function:: fract __fractdfhq (double a)

.. function:: long fract __fractdfsq (double a)

.. function:: long long fract __fractdfdq (double a)

.. function:: short accum __fractdfha (double a)

.. function:: accum __fractdfsa (double a)

.. function:: long accum __fractdfda (double a)

.. function:: long long accum __fractdfta (double a)

.. function:: unsigned short fract __fractdfuqq (double a)

.. function:: unsigned fract __fractdfuhq (double a)

.. function:: unsigned long fract __fractdfusq (double a)

.. function:: unsigned long long fract __fractdfudq (double a)

.. function:: unsigned short accum __fractdfuha (double a)

.. function:: unsigned accum __fractdfusa (double a)

.. function:: unsigned long accum __fractdfuda (double a)

.. function:: unsigned long long accum __fractdfuta (double a)

  These functions convert from fractional and signed non-fractionals to
  fractionals and signed non-fractionals, without saturation.

.. function:: fract __satfractqqhq2 (short fract a)

.. function:: long fract __satfractqqsq2 (short fract a)

.. function:: long long fract __satfractqqdq2 (short fract a)

.. function:: short accum __satfractqqha (short fract a)

.. function:: accum __satfractqqsa (short fract a)

.. function:: long accum __satfractqqda (short fract a)

.. function:: long long accum __satfractqqta (short fract a)

.. function:: unsigned short fract __satfractqquqq (short fract a)

.. function:: unsigned fract __satfractqquhq (short fract a)

.. function:: unsigned long fract __satfractqqusq (short fract a)

.. function:: unsigned long long fract __satfractqqudq (short fract a)

.. function:: unsigned short accum __satfractqquha (short fract a)

.. function:: unsigned accum __satfractqqusa (short fract a)

.. function:: unsigned long accum __satfractqquda (short fract a)

.. function:: unsigned long long accum __satfractqquta (short fract a)

.. function:: short fract __satfracthqqq2 (fract a)

.. function:: long fract __satfracthqsq2 (fract a)

.. function:: long long fract __satfracthqdq2 (fract a)

.. function:: short accum __satfracthqha (fract a)

.. function:: accum __satfracthqsa (fract a)

.. function:: long accum __satfracthqda (fract a)

.. function:: long long accum __satfracthqta (fract a)

.. function:: unsigned short fract __satfracthquqq (fract a)

.. function:: unsigned fract __satfracthquhq (fract a)

.. function:: unsigned long fract __satfracthqusq (fract a)

.. function:: unsigned long long fract __satfracthqudq (fract a)

.. function:: unsigned short accum __satfracthquha (fract a)

.. function:: unsigned accum __satfracthqusa (fract a)

.. function:: unsigned long accum __satfracthquda (fract a)

.. function:: unsigned long long accum __satfracthquta (fract a)

.. function:: short fract __satfractsqqq2 (long fract a)

.. function:: fract __satfractsqhq2 (long fract a)

.. function:: long long fract __satfractsqdq2 (long fract a)

.. function:: short accum __satfractsqha (long fract a)

.. function:: accum __satfractsqsa (long fract a)

.. function:: long accum __satfractsqda (long fract a)

.. function:: long long accum __satfractsqta (long fract a)

.. function:: unsigned short fract __satfractsquqq (long fract a)

.. function:: unsigned fract __satfractsquhq (long fract a)

.. function:: unsigned long fract __satfractsqusq (long fract a)

.. function:: unsigned long long fract __satfractsqudq (long fract a)

.. function:: unsigned short accum __satfractsquha (long fract a)

.. function:: unsigned accum __satfractsqusa (long fract a)

.. function:: unsigned long accum __satfractsquda (long fract a)

.. function:: unsigned long long accum __satfractsquta (long fract a)

.. function:: short fract __satfractdqqq2 (long long fract a)

.. function:: fract __satfractdqhq2 (long long fract a)

.. function:: long fract __satfractdqsq2 (long long fract a)

.. function:: short accum __satfractdqha (long long fract a)

.. function:: accum __satfractdqsa (long long fract a)

.. function:: long accum __satfractdqda (long long fract a)

.. function:: long long accum __satfractdqta (long long fract a)

.. function:: unsigned short fract __satfractdquqq (long long fract a)

.. function:: unsigned fract __satfractdquhq (long long fract a)

.. function:: unsigned long fract __satfractdqusq (long long fract a)

.. function:: unsigned long long fract __satfractdqudq (long long fract a)

.. function:: unsigned short accum __satfractdquha (long long fract a)

.. function:: unsigned accum __satfractdqusa (long long fract a)

.. function:: unsigned long accum __satfractdquda (long long fract a)

.. function:: unsigned long long accum __satfractdquta (long long fract a)

.. function:: short fract __satfracthaqq (short accum a)

.. function:: fract __satfracthahq (short accum a)

.. function:: long fract __satfracthasq (short accum a)

.. function:: long long fract __satfracthadq (short accum a)

.. function:: accum __satfracthasa2 (short accum a)

.. function:: long accum __satfracthada2 (short accum a)

.. function:: long long accum __satfracthata2 (short accum a)

.. function:: unsigned short fract __satfracthauqq (short accum a)

.. function:: unsigned fract __satfracthauhq (short accum a)

.. function:: unsigned long fract __satfracthausq (short accum a)

.. function:: unsigned long long fract __satfracthaudq (short accum a)

.. function:: unsigned short accum __satfracthauha (short accum a)

.. function:: unsigned accum __satfracthausa (short accum a)

.. function:: unsigned long accum __satfracthauda (short accum a)

.. function:: unsigned long long accum __satfracthauta (short accum a)

.. function:: short fract __satfractsaqq (accum a)

.. function:: fract __satfractsahq (accum a)

.. function:: long fract __satfractsasq (accum a)

.. function:: long long fract __satfractsadq (accum a)

.. function:: short accum __satfractsaha2 (accum a)

.. function:: long accum __satfractsada2 (accum a)

.. function:: long long accum __satfractsata2 (accum a)

.. function:: unsigned short fract __satfractsauqq (accum a)

.. function:: unsigned fract __satfractsauhq (accum a)

.. function:: unsigned long fract __satfractsausq (accum a)

.. function:: unsigned long long fract __satfractsaudq (accum a)

.. function:: unsigned short accum __satfractsauha (accum a)

.. function:: unsigned accum __satfractsausa (accum a)

.. function:: unsigned long accum __satfractsauda (accum a)

.. function:: unsigned long long accum __satfractsauta (accum a)

.. function:: short fract __satfractdaqq (long accum a)

.. function:: fract __satfractdahq (long accum a)

.. function:: long fract __satfractdasq (long accum a)

.. function:: long long fract __satfractdadq (long accum a)

.. function:: short accum __satfractdaha2 (long accum a)

.. function:: accum __satfractdasa2 (long accum a)

.. function:: long long accum __satfractdata2 (long accum a)

.. function:: unsigned short fract __satfractdauqq (long accum a)

.. function:: unsigned fract __satfractdauhq (long accum a)

.. function:: unsigned long fract __satfractdausq (long accum a)

.. function:: unsigned long long fract __satfractdaudq (long accum a)

.. function:: unsigned short accum __satfractdauha (long accum a)

.. function:: unsigned accum __satfractdausa (long accum a)

.. function:: unsigned long accum __satfractdauda (long accum a)

.. function:: unsigned long long accum __satfractdauta (long accum a)

.. function:: short fract __satfracttaqq (long long accum a)

.. function:: fract __satfracttahq (long long accum a)

.. function:: long fract __satfracttasq (long long accum a)

.. function:: long long fract __satfracttadq (long long accum a)

.. function:: short accum __satfracttaha2 (long long accum a)

.. function:: accum __satfracttasa2 (long long accum a)

.. function:: long accum __satfracttada2 (long long accum a)

.. function:: unsigned short fract __satfracttauqq (long long accum a)

.. function:: unsigned fract __satfracttauhq (long long accum a)

.. function:: unsigned long fract __satfracttausq (long long accum a)

.. function:: unsigned long long fract __satfracttaudq (long long accum a)

.. function:: unsigned short accum __satfracttauha (long long accum a)

.. function:: unsigned accum __satfracttausa (long long accum a)

.. function:: unsigned long accum __satfracttauda (long long accum a)

.. function:: unsigned long long accum __satfracttauta (long long accum a)

.. function:: short fract __satfractuqqqq (unsigned short fract a)

.. function:: fract __satfractuqqhq (unsigned short fract a)

.. function:: long fract __satfractuqqsq (unsigned short fract a)

.. function:: long long fract __satfractuqqdq (unsigned short fract a)

.. function:: short accum __satfractuqqha (unsigned short fract a)

.. function:: accum __satfractuqqsa (unsigned short fract a)

.. function:: long accum __satfractuqqda (unsigned short fract a)

.. function:: long long accum __satfractuqqta (unsigned short fract a)

.. function:: unsigned fract __satfractuqquhq2 (unsigned short fract a)

.. function:: unsigned long fract __satfractuqqusq2 (unsigned short fract a)

.. function:: unsigned long long fract __satfractuqqudq2 (unsigned short fract a)

.. function:: unsigned short accum __satfractuqquha (unsigned short fract a)

.. function:: unsigned accum __satfractuqqusa (unsigned short fract a)

.. function:: unsigned long accum __satfractuqquda (unsigned short fract a)

.. function:: unsigned long long accum __satfractuqquta (unsigned short fract a)

.. function:: short fract __satfractuhqqq (unsigned fract a)

.. function:: fract __satfractuhqhq (unsigned fract a)

.. function:: long fract __satfractuhqsq (unsigned fract a)

.. function:: long long fract __satfractuhqdq (unsigned fract a)

.. function:: short accum __satfractuhqha (unsigned fract a)

.. function:: accum __satfractuhqsa (unsigned fract a)

.. function:: long accum __satfractuhqda (unsigned fract a)

.. function:: long long accum __satfractuhqta (unsigned fract a)

.. function:: unsigned short fract __satfractuhquqq2 (unsigned fract a)

.. function:: unsigned long fract __satfractuhqusq2 (unsigned fract a)

.. function:: unsigned long long fract __satfractuhqudq2 (unsigned fract a)

.. function:: unsigned short accum __satfractuhquha (unsigned fract a)

.. function:: unsigned accum __satfractuhqusa (unsigned fract a)

.. function:: unsigned long accum __satfractuhquda (unsigned fract a)

.. function:: unsigned long long accum __satfractuhquta (unsigned fract a)

.. function:: short fract __satfractusqqq (unsigned long fract a)

.. function:: fract __satfractusqhq (unsigned long fract a)

.. function:: long fract __satfractusqsq (unsigned long fract a)

.. function:: long long fract __satfractusqdq (unsigned long fract a)

.. function:: short accum __satfractusqha (unsigned long fract a)

.. function:: accum __satfractusqsa (unsigned long fract a)

.. function:: long accum __satfractusqda (unsigned long fract a)

.. function:: long long accum __satfractusqta (unsigned long fract a)

.. function:: unsigned short fract __satfractusquqq2 (unsigned long fract a)

.. function:: unsigned fract __satfractusquhq2 (unsigned long fract a)

.. function:: unsigned long long fract __satfractusqudq2 (unsigned long fract a)

.. function:: unsigned short accum __satfractusquha (unsigned long fract a)

.. function:: unsigned accum __satfractusqusa (unsigned long fract a)

.. function:: unsigned long accum __satfractusquda (unsigned long fract a)

.. function:: unsigned long long accum __satfractusquta (unsigned long fract a)

.. function:: short fract __satfractudqqq (unsigned long long fract a)

.. function:: fract __satfractudqhq (unsigned long long fract a)

.. function:: long fract __satfractudqsq (unsigned long long fract a)

.. function:: long long fract __satfractudqdq (unsigned long long fract a)

.. function:: short accum __satfractudqha (unsigned long long fract a)

.. function:: accum __satfractudqsa (unsigned long long fract a)

.. function:: long accum __satfractudqda (unsigned long long fract a)

.. function:: long long accum __satfractudqta (unsigned long long fract a)

.. function:: unsigned short fract __satfractudquqq2 (unsigned long long fract a)

.. function:: unsigned fract __satfractudquhq2 (unsigned long long fract a)

.. function:: unsigned long fract __satfractudqusq2 (unsigned long long fract a)

.. function:: unsigned short accum __satfractudquha (unsigned long long fract a)

.. function:: unsigned accum __satfractudqusa (unsigned long long fract a)

.. function:: unsigned long accum __satfractudquda (unsigned long long fract a)

.. function:: unsigned long long accum __satfractudquta (unsigned long long fract a)

.. function:: short fract __satfractuhaqq (unsigned short accum a)

.. function:: fract __satfractuhahq (unsigned short accum a)

.. function:: long fract __satfractuhasq (unsigned short accum a)

.. function:: long long fract __satfractuhadq (unsigned short accum a)

.. function:: short accum __satfractuhaha (unsigned short accum a)

.. function:: accum __satfractuhasa (unsigned short accum a)

.. function:: long accum __satfractuhada (unsigned short accum a)

.. function:: long long accum __satfractuhata (unsigned short accum a)

.. function:: unsigned short fract __satfractuhauqq (unsigned short accum a)

.. function:: unsigned fract __satfractuhauhq (unsigned short accum a)

.. function:: unsigned long fract __satfractuhausq (unsigned short accum a)

.. function:: unsigned long long fract __satfractuhaudq (unsigned short accum a)

.. function:: unsigned accum __satfractuhausa2 (unsigned short accum a)

.. function:: unsigned long accum __satfractuhauda2 (unsigned short accum a)

.. function:: unsigned long long accum __satfractuhauta2 (unsigned short accum a)

.. function:: short fract __satfractusaqq (unsigned accum a)

.. function:: fract __satfractusahq (unsigned accum a)

.. function:: long fract __satfractusasq (unsigned accum a)

.. function:: long long fract __satfractusadq (unsigned accum a)

.. function:: short accum __satfractusaha (unsigned accum a)

.. function:: accum __satfractusasa (unsigned accum a)

.. function:: long accum __satfractusada (unsigned accum a)

.. function:: long long accum __satfractusata (unsigned accum a)

.. function:: unsigned short fract __satfractusauqq (unsigned accum a)

.. function:: unsigned fract __satfractusauhq (unsigned accum a)

.. function:: unsigned long fract __satfractusausq (unsigned accum a)

.. function:: unsigned long long fract __satfractusaudq (unsigned accum a)

.. function:: unsigned short accum __satfractusauha2 (unsigned accum a)

.. function:: unsigned long accum __satfractusauda2 (unsigned accum a)

.. function:: unsigned long long accum __satfractusauta2 (unsigned accum a)

.. function:: short fract __satfractudaqq (unsigned long accum a)

.. function:: fract __satfractudahq (unsigned long accum a)

.. function:: long fract __satfractudasq (unsigned long accum a)

.. function:: long long fract __satfractudadq (unsigned long accum a)

.. function:: short accum __satfractudaha (unsigned long accum a)

.. function:: accum __satfractudasa (unsigned long accum a)

.. function:: long accum __satfractudada (unsigned long accum a)

.. function:: long long accum __satfractudata (unsigned long accum a)

.. function:: unsigned short fract __satfractudauqq (unsigned long accum a)

.. function:: unsigned fract __satfractudauhq (unsigned long accum a)

.. function:: unsigned long fract __satfractudausq (unsigned long accum a)

.. function:: unsigned long long fract __satfractudaudq (unsigned long accum a)

.. function:: unsigned short accum __satfractudauha2 (unsigned long accum a)

.. function:: unsigned accum __satfractudausa2 (unsigned long accum a)

.. function:: unsigned long long accum __satfractudauta2 (unsigned long accum a)

.. function:: short fract __satfractutaqq (unsigned long long accum a)

.. function:: fract __satfractutahq (unsigned long long accum a)

.. function:: long fract __satfractutasq (unsigned long long accum a)

.. function:: long long fract __satfractutadq (unsigned long long accum a)

.. function:: short accum __satfractutaha (unsigned long long accum a)

.. function:: accum __satfractutasa (unsigned long long accum a)

.. function:: long accum __satfractutada (unsigned long long accum a)

.. function:: long long accum __satfractutata (unsigned long long accum a)

.. function:: unsigned short fract __satfractutauqq (unsigned long long accum a)

.. function:: unsigned fract __satfractutauhq (unsigned long long accum a)

.. function:: unsigned long fract __satfractutausq (unsigned long long accum a)

.. function:: unsigned long long fract __satfractutaudq (unsigned long long accum a)

.. function:: unsigned short accum __satfractutauha2 (unsigned long long accum a)

.. function:: unsigned accum __satfractutausa2 (unsigned long long accum a)

.. function:: unsigned long accum __satfractutauda2 (unsigned long long accum a)

.. function:: short fract __satfractqiqq (signed char a)

.. function:: fract __satfractqihq (signed char a)

.. function:: long fract __satfractqisq (signed char a)

.. function:: long long fract __satfractqidq (signed char a)

.. function:: short accum __satfractqiha (signed char a)

.. function:: accum __satfractqisa (signed char a)

.. function:: long accum __satfractqida (signed char a)

.. function:: long long accum __satfractqita (signed char a)

.. function:: unsigned short fract __satfractqiuqq (signed char a)

.. function:: unsigned fract __satfractqiuhq (signed char a)

.. function:: unsigned long fract __satfractqiusq (signed char a)

.. function:: unsigned long long fract __satfractqiudq (signed char a)

.. function:: unsigned short accum __satfractqiuha (signed char a)

.. function:: unsigned accum __satfractqiusa (signed char a)

.. function:: unsigned long accum __satfractqiuda (signed char a)

.. function:: unsigned long long accum __satfractqiuta (signed char a)

.. function:: short fract __satfracthiqq (short a)

.. function:: fract __satfracthihq (short a)

.. function:: long fract __satfracthisq (short a)

.. function:: long long fract __satfracthidq (short a)

.. function:: short accum __satfracthiha (short a)

.. function:: accum __satfracthisa (short a)

.. function:: long accum __satfracthida (short a)

.. function:: long long accum __satfracthita (short a)

.. function:: unsigned short fract __satfracthiuqq (short a)

.. function:: unsigned fract __satfracthiuhq (short a)

.. function:: unsigned long fract __satfracthiusq (short a)

.. function:: unsigned long long fract __satfracthiudq (short a)

.. function:: unsigned short accum __satfracthiuha (short a)

.. function:: unsigned accum __satfracthiusa (short a)

.. function:: unsigned long accum __satfracthiuda (short a)

.. function:: unsigned long long accum __satfracthiuta (short a)

.. function:: short fract __satfractsiqq (int a)

.. function:: fract __satfractsihq (int a)

.. function:: long fract __satfractsisq (int a)

.. function:: long long fract __satfractsidq (int a)

.. function:: short accum __satfractsiha (int a)

.. function:: accum __satfractsisa (int a)

.. function:: long accum __satfractsida (int a)

.. function:: long long accum __satfractsita (int a)

.. function:: unsigned short fract __satfractsiuqq (int a)

.. function:: unsigned fract __satfractsiuhq (int a)

.. function:: unsigned long fract __satfractsiusq (int a)

.. function:: unsigned long long fract __satfractsiudq (int a)

.. function:: unsigned short accum __satfractsiuha (int a)

.. function:: unsigned accum __satfractsiusa (int a)

.. function:: unsigned long accum __satfractsiuda (int a)

.. function:: unsigned long long accum __satfractsiuta (int a)

.. function:: short fract __satfractdiqq (long a)

.. function:: fract __satfractdihq (long a)

.. function:: long fract __satfractdisq (long a)

.. function:: long long fract __satfractdidq (long a)

.. function:: short accum __satfractdiha (long a)

.. function:: accum __satfractdisa (long a)

.. function:: long accum __satfractdida (long a)

.. function:: long long accum __satfractdita (long a)

.. function:: unsigned short fract __satfractdiuqq (long a)

.. function:: unsigned fract __satfractdiuhq (long a)

.. function:: unsigned long fract __satfractdiusq (long a)

.. function:: unsigned long long fract __satfractdiudq (long a)

.. function:: unsigned short accum __satfractdiuha (long a)

.. function:: unsigned accum __satfractdiusa (long a)

.. function:: unsigned long accum __satfractdiuda (long a)

.. function:: unsigned long long accum __satfractdiuta (long a)

.. function:: short fract __satfracttiqq (long long a)

.. function:: fract __satfracttihq (long long a)

.. function:: long fract __satfracttisq (long long a)

.. function:: long long fract __satfracttidq (long long a)

.. function:: short accum __satfracttiha (long long a)

.. function:: accum __satfracttisa (long long a)

.. function:: long accum __satfracttida (long long a)

.. function:: long long accum __satfracttita (long long a)

.. function:: unsigned short fract __satfracttiuqq (long long a)

.. function:: unsigned fract __satfracttiuhq (long long a)

.. function:: unsigned long fract __satfracttiusq (long long a)

.. function:: unsigned long long fract __satfracttiudq (long long a)

.. function:: unsigned short accum __satfracttiuha (long long a)

.. function:: unsigned accum __satfracttiusa (long long a)

.. function:: unsigned long accum __satfracttiuda (long long a)

.. function:: unsigned long long accum __satfracttiuta (long long a)

.. function:: short fract __satfractsfqq (float a)

.. function:: fract __satfractsfhq (float a)

.. function:: long fract __satfractsfsq (float a)

.. function:: long long fract __satfractsfdq (float a)

.. function:: short accum __satfractsfha (float a)

.. function:: accum __satfractsfsa (float a)

.. function:: long accum __satfractsfda (float a)

.. function:: long long accum __satfractsfta (float a)

.. function:: unsigned short fract __satfractsfuqq (float a)

.. function:: unsigned fract __satfractsfuhq (float a)

.. function:: unsigned long fract __satfractsfusq (float a)

.. function:: unsigned long long fract __satfractsfudq (float a)

.. function:: unsigned short accum __satfractsfuha (float a)

.. function:: unsigned accum __satfractsfusa (float a)

.. function:: unsigned long accum __satfractsfuda (float a)

.. function:: unsigned long long accum __satfractsfuta (float a)

.. function:: short fract __satfractdfqq (double a)

.. function:: fract __satfractdfhq (double a)

.. function:: long fract __satfractdfsq (double a)

.. function:: long long fract __satfractdfdq (double a)

.. function:: short accum __satfractdfha (double a)

.. function:: accum __satfractdfsa (double a)

.. function:: long accum __satfractdfda (double a)

.. function:: long long accum __satfractdfta (double a)

.. function:: unsigned short fract __satfractdfuqq (double a)

.. function:: unsigned fract __satfractdfuhq (double a)

.. function:: unsigned long fract __satfractdfusq (double a)

.. function:: unsigned long long fract __satfractdfudq (double a)

.. function:: unsigned short accum __satfractdfuha (double a)

.. function:: unsigned accum __satfractdfusa (double a)

.. function:: unsigned long accum __satfractdfuda (double a)

.. function:: unsigned long long accum __satfractdfuta (double a)

  The functions convert from fractional and signed non-fractionals to
  fractionals, with saturation.

.. function:: unsigned char __fractunsqqqi (short fract a)

.. function:: unsigned short __fractunsqqhi (short fract a)

.. function:: unsigned int __fractunsqqsi (short fract a)

.. function:: unsigned long __fractunsqqdi (short fract a)

.. function:: unsigned long long __fractunsqqti (short fract a)

.. function:: unsigned char __fractunshqqi (fract a)

.. function:: unsigned short __fractunshqhi (fract a)

.. function:: unsigned int __fractunshqsi (fract a)

.. function:: unsigned long __fractunshqdi (fract a)

.. function:: unsigned long long __fractunshqti (fract a)

.. function:: unsigned char __fractunssqqi (long fract a)

.. function:: unsigned short __fractunssqhi (long fract a)

.. function:: unsigned int __fractunssqsi (long fract a)

.. function:: unsigned long __fractunssqdi (long fract a)

.. function:: unsigned long long __fractunssqti (long fract a)

.. function:: unsigned char __fractunsdqqi (long long fract a)

.. function:: unsigned short __fractunsdqhi (long long fract a)

.. function:: unsigned int __fractunsdqsi (long long fract a)

.. function:: unsigned long __fractunsdqdi (long long fract a)

.. function:: unsigned long long __fractunsdqti (long long fract a)

.. function:: unsigned char __fractunshaqi (short accum a)

.. function:: unsigned short __fractunshahi (short accum a)

.. function:: unsigned int __fractunshasi (short accum a)

.. function:: unsigned long __fractunshadi (short accum a)

.. function:: unsigned long long __fractunshati (short accum a)

.. function:: unsigned char __fractunssaqi (accum a)

.. function:: unsigned short __fractunssahi (accum a)

.. function:: unsigned int __fractunssasi (accum a)

.. function:: unsigned long __fractunssadi (accum a)

.. function:: unsigned long long __fractunssati (accum a)

.. function:: unsigned char __fractunsdaqi (long accum a)

.. function:: unsigned short __fractunsdahi (long accum a)

.. function:: unsigned int __fractunsdasi (long accum a)

.. function:: unsigned long __fractunsdadi (long accum a)

.. function:: unsigned long long __fractunsdati (long accum a)

.. function:: unsigned char __fractunstaqi (long long accum a)

.. function:: unsigned short __fractunstahi (long long accum a)

.. function:: unsigned int __fractunstasi (long long accum a)

.. function:: unsigned long __fractunstadi (long long accum a)

.. function:: unsigned long long __fractunstati (long long accum a)

.. function:: unsigned char __fractunsuqqqi (unsigned short fract a)

.. function:: unsigned short __fractunsuqqhi (unsigned short fract a)

.. function:: unsigned int __fractunsuqqsi (unsigned short fract a)

.. function:: unsigned long __fractunsuqqdi (unsigned short fract a)

.. function:: unsigned long long __fractunsuqqti (unsigned short fract a)

.. function:: unsigned char __fractunsuhqqi (unsigned fract a)

.. function:: unsigned short __fractunsuhqhi (unsigned fract a)

.. function:: unsigned int __fractunsuhqsi (unsigned fract a)

.. function:: unsigned long __fractunsuhqdi (unsigned fract a)

.. function:: unsigned long long __fractunsuhqti (unsigned fract a)

.. function:: unsigned char __fractunsusqqi (unsigned long fract a)

.. function:: unsigned short __fractunsusqhi (unsigned long fract a)

.. function:: unsigned int __fractunsusqsi (unsigned long fract a)

.. function:: unsigned long __fractunsusqdi (unsigned long fract a)

.. function:: unsigned long long __fractunsusqti (unsigned long fract a)

.. function:: unsigned char __fractunsudqqi (unsigned long long fract a)

.. function:: unsigned short __fractunsudqhi (unsigned long long fract a)

.. function:: unsigned int __fractunsudqsi (unsigned long long fract a)

.. function:: unsigned long __fractunsudqdi (unsigned long long fract a)

.. function:: unsigned long long __fractunsudqti (unsigned long long fract a)

.. function:: unsigned char __fractunsuhaqi (unsigned short accum a)

.. function:: unsigned short __fractunsuhahi (unsigned short accum a)

.. function:: unsigned int __fractunsuhasi (unsigned short accum a)

.. function:: unsigned long __fractunsuhadi (unsigned short accum a)

.. function:: unsigned long long __fractunsuhati (unsigned short accum a)

.. function:: unsigned char __fractunsusaqi (unsigned accum a)

.. function:: unsigned short __fractunsusahi (unsigned accum a)

.. function:: unsigned int __fractunsusasi (unsigned accum a)

.. function:: unsigned long __fractunsusadi (unsigned accum a)

.. function:: unsigned long long __fractunsusati (unsigned accum a)

.. function:: unsigned char __fractunsudaqi (unsigned long accum a)

.. function:: unsigned short __fractunsudahi (unsigned long accum a)

.. function:: unsigned int __fractunsudasi (unsigned long accum a)

.. function:: unsigned long __fractunsudadi (unsigned long accum a)

.. function:: unsigned long long __fractunsudati (unsigned long accum a)

.. function:: unsigned char __fractunsutaqi (unsigned long long accum a)

.. function:: unsigned short __fractunsutahi (unsigned long long accum a)

.. function:: unsigned int __fractunsutasi (unsigned long long accum a)

.. function:: unsigned long __fractunsutadi (unsigned long long accum a)

.. function:: unsigned long long __fractunsutati (unsigned long long accum a)

.. function:: short fract __fractunsqiqq (unsigned char a)

.. function:: fract __fractunsqihq (unsigned char a)

.. function:: long fract __fractunsqisq (unsigned char a)

.. function:: long long fract __fractunsqidq (unsigned char a)

.. function:: short accum __fractunsqiha (unsigned char a)

.. function:: accum __fractunsqisa (unsigned char a)

.. function:: long accum __fractunsqida (unsigned char a)

.. function:: long long accum __fractunsqita (unsigned char a)

.. function:: unsigned short fract __fractunsqiuqq (unsigned char a)

.. function:: unsigned fract __fractunsqiuhq (unsigned char a)

.. function:: unsigned long fract __fractunsqiusq (unsigned char a)

.. function:: unsigned long long fract __fractunsqiudq (unsigned char a)

.. function:: unsigned short accum __fractunsqiuha (unsigned char a)

.. function:: unsigned accum __fractunsqiusa (unsigned char a)

.. function:: unsigned long accum __fractunsqiuda (unsigned char a)

.. function:: unsigned long long accum __fractunsqiuta (unsigned char a)

.. function:: short fract __fractunshiqq (unsigned short a)

.. function:: fract __fractunshihq (unsigned short a)

.. function:: long fract __fractunshisq (unsigned short a)

.. function:: long long fract __fractunshidq (unsigned short a)

.. function:: short accum __fractunshiha (unsigned short a)

.. function:: accum __fractunshisa (unsigned short a)

.. function:: long accum __fractunshida (unsigned short a)

.. function:: long long accum __fractunshita (unsigned short a)

.. function:: unsigned short fract __fractunshiuqq (unsigned short a)

.. function:: unsigned fract __fractunshiuhq (unsigned short a)

.. function:: unsigned long fract __fractunshiusq (unsigned short a)

.. function:: unsigned long long fract __fractunshiudq (unsigned short a)

.. function:: unsigned short accum __fractunshiuha (unsigned short a)

.. function:: unsigned accum __fractunshiusa (unsigned short a)

.. function:: unsigned long accum __fractunshiuda (unsigned short a)

.. function:: unsigned long long accum __fractunshiuta (unsigned short a)

.. function:: short fract __fractunssiqq (unsigned int a)

.. function:: fract __fractunssihq (unsigned int a)

.. function:: long fract __fractunssisq (unsigned int a)

.. function:: long long fract __fractunssidq (unsigned int a)

.. function:: short accum __fractunssiha (unsigned int a)

.. function:: accum __fractunssisa (unsigned int a)

.. function:: long accum __fractunssida (unsigned int a)

.. function:: long long accum __fractunssita (unsigned int a)

.. function:: unsigned short fract __fractunssiuqq (unsigned int a)

.. function:: unsigned fract __fractunssiuhq (unsigned int a)

.. function:: unsigned long fract __fractunssiusq (unsigned int a)

.. function:: unsigned long long fract __fractunssiudq (unsigned int a)

.. function:: unsigned short accum __fractunssiuha (unsigned int a)

.. function:: unsigned accum __fractunssiusa (unsigned int a)

.. function:: unsigned long accum __fractunssiuda (unsigned int a)

.. function:: unsigned long long accum __fractunssiuta (unsigned int a)

.. function:: short fract __fractunsdiqq (unsigned long a)

.. function:: fract __fractunsdihq (unsigned long a)

.. function:: long fract __fractunsdisq (unsigned long a)

.. function:: long long fract __fractunsdidq (unsigned long a)

.. function:: short accum __fractunsdiha (unsigned long a)

.. function:: accum __fractunsdisa (unsigned long a)

.. function:: long accum __fractunsdida (unsigned long a)

.. function:: long long accum __fractunsdita (unsigned long a)

.. function:: unsigned short fract __fractunsdiuqq (unsigned long a)

.. function:: unsigned fract __fractunsdiuhq (unsigned long a)

.. function:: unsigned long fract __fractunsdiusq (unsigned long a)

.. function:: unsigned long long fract __fractunsdiudq (unsigned long a)

.. function:: unsigned short accum __fractunsdiuha (unsigned long a)

.. function:: unsigned accum __fractunsdiusa (unsigned long a)

.. function:: unsigned long accum __fractunsdiuda (unsigned long a)

.. function:: unsigned long long accum __fractunsdiuta (unsigned long a)

.. function:: short fract __fractunstiqq (unsigned long long a)

.. function:: fract __fractunstihq (unsigned long long a)

.. function:: long fract __fractunstisq (unsigned long long a)

.. function:: long long fract __fractunstidq (unsigned long long a)

.. function:: short accum __fractunstiha (unsigned long long a)

.. function:: accum __fractunstisa (unsigned long long a)

.. function:: long accum __fractunstida (unsigned long long a)

.. function:: long long accum __fractunstita (unsigned long long a)

.. function:: unsigned short fract __fractunstiuqq (unsigned long long a)

.. function:: unsigned fract __fractunstiuhq (unsigned long long a)

.. function:: unsigned long fract __fractunstiusq (unsigned long long a)

.. function:: unsigned long long fract __fractunstiudq (unsigned long long a)

.. function:: unsigned short accum __fractunstiuha (unsigned long long a)

.. function:: unsigned accum __fractunstiusa (unsigned long long a)

.. function:: unsigned long accum __fractunstiuda (unsigned long long a)

.. function:: unsigned long long accum __fractunstiuta (unsigned long long a)

  These functions convert from fractionals to unsigned non-fractionals;
  and from unsigned non-fractionals to fractionals, without saturation.

.. function:: short fract __satfractunsqiqq (unsigned char a)

.. function:: fract __satfractunsqihq (unsigned char a)

.. function:: long fract __satfractunsqisq (unsigned char a)

.. function:: long long fract __satfractunsqidq (unsigned char a)

.. function:: short accum __satfractunsqiha (unsigned char a)

.. function:: accum __satfractunsqisa (unsigned char a)

.. function:: long accum __satfractunsqida (unsigned char a)

.. function:: long long accum __satfractunsqita (unsigned char a)

.. function:: unsigned short fract __satfractunsqiuqq (unsigned char a)

.. function:: unsigned fract __satfractunsqiuhq (unsigned char a)

.. function:: unsigned long fract __satfractunsqiusq (unsigned char a)

.. function:: unsigned long long fract __satfractunsqiudq (unsigned char a)

.. function:: unsigned short accum __satfractunsqiuha (unsigned char a)

.. function:: unsigned accum __satfractunsqiusa (unsigned char a)

.. function:: unsigned long accum __satfractunsqiuda (unsigned char a)

.. function:: unsigned long long accum __satfractunsqiuta (unsigned char a)

.. function:: short fract __satfractunshiqq (unsigned short a)

.. function:: fract __satfractunshihq (unsigned short a)

.. function:: long fract __satfractunshisq (unsigned short a)

.. function:: long long fract __satfractunshidq (unsigned short a)

.. function:: short accum __satfractunshiha (unsigned short a)

.. function:: accum __satfractunshisa (unsigned short a)

.. function:: long accum __satfractunshida (unsigned short a)

.. function:: long long accum __satfractunshita (unsigned short a)

.. function:: unsigned short fract __satfractunshiuqq (unsigned short a)

.. function:: unsigned fract __satfractunshiuhq (unsigned short a)

.. function:: unsigned long fract __satfractunshiusq (unsigned short a)

.. function:: unsigned long long fract __satfractunshiudq (unsigned short a)

.. function:: unsigned short accum __satfractunshiuha (unsigned short a)

.. function:: unsigned accum __satfractunshiusa (unsigned short a)

.. function:: unsigned long accum __satfractunshiuda (unsigned short a)

.. function:: unsigned long long accum __satfractunshiuta (unsigned short a)

.. function:: short fract __satfractunssiqq (unsigned int a)

.. function:: fract __satfractunssihq (unsigned int a)

.. function:: long fract __satfractunssisq (unsigned int a)

.. function:: long long fract __satfractunssidq (unsigned int a)

.. function:: short accum __satfractunssiha (unsigned int a)

.. function:: accum __satfractunssisa (unsigned int a)

.. function:: long accum __satfractunssida (unsigned int a)

.. function:: long long accum __satfractunssita (unsigned int a)

.. function:: unsigned short fract __satfractunssiuqq (unsigned int a)

.. function:: unsigned fract __satfractunssiuhq (unsigned int a)

.. function:: unsigned long fract __satfractunssiusq (unsigned int a)

.. function:: unsigned long long fract __satfractunssiudq (unsigned int a)

.. function:: unsigned short accum __satfractunssiuha (unsigned int a)

.. function:: unsigned accum __satfractunssiusa (unsigned int a)

.. function:: unsigned long accum __satfractunssiuda (unsigned int a)

.. function:: unsigned long long accum __satfractunssiuta (unsigned int a)

.. function:: short fract __satfractunsdiqq (unsigned long a)

.. function:: fract __satfractunsdihq (unsigned long a)

.. function:: long fract __satfractunsdisq (unsigned long a)

.. function:: long long fract __satfractunsdidq (unsigned long a)

.. function:: short accum __satfractunsdiha (unsigned long a)

.. function:: accum __satfractunsdisa (unsigned long a)

.. function:: long accum __satfractunsdida (unsigned long a)

.. function:: long long accum __satfractunsdita (unsigned long a)

.. function:: unsigned short fract __satfractunsdiuqq (unsigned long a)

.. function:: unsigned fract __satfractunsdiuhq (unsigned long a)

.. function:: unsigned long fract __satfractunsdiusq (unsigned long a)

.. function:: unsigned long long fract __satfractunsdiudq (unsigned long a)

.. function:: unsigned short accum __satfractunsdiuha (unsigned long a)

.. function:: unsigned accum __satfractunsdiusa (unsigned long a)

.. function:: unsigned long accum __satfractunsdiuda (unsigned long a)

.. function:: unsigned long long accum __satfractunsdiuta (unsigned long a)

.. function:: short fract __satfractunstiqq (unsigned long long a)

.. function:: fract __satfractunstihq (unsigned long long a)

.. function:: long fract __satfractunstisq (unsigned long long a)

.. function:: long long fract __satfractunstidq (unsigned long long a)

.. function:: short accum __satfractunstiha (unsigned long long a)

.. function:: accum __satfractunstisa (unsigned long long a)

.. function:: long accum __satfractunstida (unsigned long long a)

.. function:: long long accum __satfractunstita (unsigned long long a)

.. function:: unsigned short fract __satfractunstiuqq (unsigned long long a)

.. function:: unsigned fract __satfractunstiuhq (unsigned long long a)

.. function:: unsigned long fract __satfractunstiusq (unsigned long long a)

.. function:: unsigned long long fract __satfractunstiudq (unsigned long long a)

.. function:: unsigned short accum __satfractunstiuha (unsigned long long a)

.. function:: unsigned accum __satfractunstiusa (unsigned long long a)

.. function:: unsigned long accum __satfractunstiuda (unsigned long long a)

.. function:: unsigned long long accum __satfractunstiuta (unsigned long long a)

  These functions convert from unsigned non-fractionals to fractionals,
  with saturation.