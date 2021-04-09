.. _packages:

Packages and Visibility Rules
-----------------------------

..  -

.. index:: Packages

* All program units and subprograms have their name at the end:

  .. code-block:: ada

          package P is
             ...
          end P;

* We will use the style of ``use`` -ing ``with`` -ed packages, with
  the context clauses looking like:

  .. index:: use clauses

  .. code-block:: ada

          with A; use A;
          with B; use B;

* Names declared in the visible part of packages should be
  unique, to prevent name clashes when the packages are ``use`` d.

  .. index:: Name clash avoidance

  .. code-block:: ada

          package Entity is
             type Entity_Kind is ...;
             ...
          end Entity;

* After the file header comment, the context clause and unit specification
  should be the first thing in a program_unit.

* Preelaborate, Pure and Elaborate_Body pragmas should be added right after the
  package name, indented an extra level and using the parameterless form:

  .. code-block:: ada

          package Preelaborate_Package is
             pragma Preelaborate;
             ...
          end Preelaborate_Package;

..  -

