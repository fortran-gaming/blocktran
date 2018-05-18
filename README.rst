.. image:: https://travis-ci.org/scivision/tetran.svg?branch=master
    :target: https://travis-ci.org/scivision/tetran

======
Tetran
======

:current author/maintainer:  `Michael Hirsch Ph.D. <https://www.scivision.co/blog>`_
:original author: `Lewis Bobbermen <https://github.com/lewisjb>`_

Text/console falling-block tetromino game written in Modern Fortran.
It has diverged and been enhanced considerably from L. Bobberman's original demo.

.. image:: tests/tetran.gif
   :alt: Tetran gameplay demo

* user-configurable playfield size
* Logs pieces played to ``tetran.log`` so you can recreate memorable games.
* uniform random game piece generation.
* clean, modern Fortran 2008 syntax
* Ncurses used for display, called directly from Fortran code (this is a blocker for native Windows).

.. contents::

Prereq
======
Tetran works on Mac, Linux, Cygwin and Windows Subsystem for Linux.
Any modern Fortran compiler (including Flang with CMake >= 3.10) should work.


* Linux / WSL: ``apt install gfortran libncurses-dev``
* Mac: ``brew install gcc ncurses``
* Cygwin: ``setup-x86_64.exe -P libncurses-devel cmake make gcc-gfortran``


Build
=====

.. code:: bash

    cd bin
    cmake ..
    make
    make test


Optional: specify a compiler by setting ``FC=``.

* Intel: ``FC=ifort cmake ..``
* Flang: ``FC=flang cmake ..``
* PGI: ``FC=pgf95 cmake ..``



Play
====

.. code:: bash

    ./tetran


difficulty level
----------------
adjust cadence of falling blocks with ``-d`` option, including decimal point:

.. code:: bash

    ./tetran -d 1.2

Higher number => more difficult. 
Must include decimal point.

random source file
------------------
On Windows with non-Fortran 2018 compliant compilers, the randomness file MUST be specified as

.. code:: bash

    ./tetran -r file:/dev/urandom
    
    
debug mode
----------
Debug logging is enabled by:

.. code:: bash

    ./tetran --debug



Controls
--------

Other "secret" cheat keys exist!
You can also use arrow keys.

========= ======
Key       Effect
========= ======
W         Rotate piece
A/D       Left and right respectively
S         Move down faster
Q or Esc  Exit the game
========= ======


Notes
=====

FIXME: commit a8c654445ae5bc7ed7ee68cfb33b5ba8b9d744e6 is where the ^A started to appear at the bottom right.
Symptoms:

  * OK on ifort
  * Gfortran 5-8, appears for -O1,-O2,-O3, but not for -O0

=> May have been just because that's where optimization was first turned on!?

References
----------

`Control codes <https://en.wikipedia.org/wiki/C0_and_C1_control_codes>`_
