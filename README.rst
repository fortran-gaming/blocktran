.. image:: https://travis-ci.org/lewisjb/tetran.svg?branch=master
    :target: https://travis-ci.org/lewisjb/tetran

======
Tetran
======

Basic tetris-style game written in Modern Fortran.

Building
========
Tetran works on Mac, Linux, Cygwin and Windows Subsystem for Linux.

Prereq
------
Linux::

    sudo apt install gfortran libncurses-dev

Mac::

    brew install gcc ncurses


Compile
-------
::
    cd bin
    cmake ..
    make

You can optionally specify a compiler by setting ``FC=``. 
For example, to use the Intel Fortran compiler::

    FC=ifort cmake ..
    make

Play
====
::

    ./tetran


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
