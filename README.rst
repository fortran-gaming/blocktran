.. image:: https://travis-ci.org/lewisjb/tetran.svg?branch=master
    :target: https://travis-ci.org/lewisjb/tetran

======
Tetran
======

:author: Lewis Bobbermen, Michael Hirsch Ph.D.

Basic tetris-style game written in Modern Fortran.

* Currently only one level, play until pieces overflow
* Fortran 2008 syntax
* play details given at game over and logged to ``tetran.log``
* Ncurses used for display, called directly from Fortran code.

.. contents::

Tetran Prereq
=============
Tetran works on Mac, Linux, Cygwin and Windows Subsystem for Linux.


Linux 
------------

    sudo apt install gfortran libncurses-dev

Mac
----------

    brew install gcc ncurses


Compile Tetran
==============
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

The difficulty level may be set (rate of falling blocks higher) by::

    ./tetran 10

The first number sets the difficulty level (higher number more difficult; positive integers only.
This integer number is used to divide the wait time period by



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


References
==========

`Standard Tetris Specifications <http://www.colinfahey.com/tetris/tetris.html>`_
