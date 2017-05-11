======
Tetran
======

Basic tetris-style game written in Modern Fortran.

Building
========
Tetran works on Mac, Linux, Cygwin and Windows Subsystem for Linux.

Prereq
------
::

    sudo apt install gfortran libncurses-dev


Compile
-------
::

    make

You can optionally specify a compiler by setting ``FC=``. 
For example, to use the Intel Fortran compiler::

    FC=ifort make

Play
====
::

    ./main


Controls
--------

=== ======
Key Effect
=== ======
W   Rotate piece
A/D Left and right respectively
S   Move down faster
Q   Exit the game
=== ======
