..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _dollar-signs:

Dollar Signs in Identifier Names
********************************

.. index:: $

.. index:: dollar signs in identifier names

.. index:: identifier names, dollar signs in

In GNU C, you may normally use dollar signs in identifier names.
This is because many traditional C implementations allow such identifiers.
However, dollar signs in identifiers are not supported on a few target
machines, typically because the target assembler does not allow them.