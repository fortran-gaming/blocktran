======
Tetran
======

Basic tetris-style game written in Modern Fortran.

Building
========

Installing gfortran
-------------------
::

    sudo apt install gfortran


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
