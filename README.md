[![Build Status](https://travis-ci.com/fortran-gaming/tetran.svg?branch=master)](https://travis-ci.com/fortran-gaming/tetran)
[![Build status](https://ci.appveyor.com/api/projects/status/bhta29i7fvgl90ig?svg=true)](https://ci.appveyor.com/project/scivision/tetran)

# Tetran

Text/console falling-block tetromino game written in object-oriented Fortran 2008.

![Tetran gameplay demo](doc/tetran.gif)

* user-configurable playfield size
* Logs pieces played to `tetran.log` so you can recreate memorable games.
* uniform random game piece generation.
* clean, object-oriented Fortran 2008 syntax, well structured using Fortran 2008 `submodule`
* Curses (Ncurses, PDcurses) used for display, called directly from Fortran code.

## Prereq

Tetran works on Mac, Linux, native Windows, Cygwin, Windows Subsystem for Linux.
Requires:

* Fortran 2008 compilers supporting Fortran `submodule`
* CMake &ge; 3.12

Obtain these items by:

* Linux / WSL: `apt install gfortran libncurses-dev`.  Use [cmake_setup.sh](https://github.com/scivision/cmake-utils/blob/master/cmake_setup.sh) for CMake &ge; 3.12.
* Mac: `brew install gcc ncurses cmake`
* Cygwin: `setup-x86_64.exe -P libncurses-devel cmake make gcc-gfortran`

## Build

from the top-level `tetran` directory:

```bash
cmake -B build -S .

cmake --build build -j --target install

cd build

ctest -V
```

### Compiler selection

As usual, optionally specify a compiler by setting environment variables `FC` AND `CC`.
Failing to set both results in segfaults.

### Native Windows
The easiest way to use Tetran on Windows is via Windows Subsystem for Linux or Cygwin.
Otherwise, on native Windows, you must:

1. compile [PDcurses](https://pdcurses.sourceforge.io/) yourself, which results in `pdcurses.a` file
2. from PDcurses file you extracted and pdcurses.a you compiled, copy curses.h and pdcurses.a to C:\pdcurses\{curses.h, curses.a}
3. use initial CMake options `cmake -G "MinGW Makefiles" -B build -S .`

## Play
From any Terminal:
```bash
tetran
```

The command line options are described next in the following sections.

### difficulty level

adjust cadence of falling blocks with `-d` option, including decimal
point:

```bash
tetran -d 1.2
```

Higher number increase difficulty. Must include decimal point.

### Playfield size

specify width and height of the playfield with `-s` option:

```bash
tetran -s 20 15
```

### play against computer

The computer player is for now rudimentary, the AI algorithm is being developed offline.

```bash
tetran -p 2
```

### debug mode

Debug logging is enabled by:

```bash
tetran --debug
```

### Controls

Other "secret" cheat keys exist! You can also use arrow keys.

  Key      |  Effect
-----------|-----------------------------
  W        | Rotate piece
  A/D      | Left and right respectively
  S        | Move down faster
  Q or Esc | Exit the game

## Notes

### Block Randomness

unlike some games, block distribution is uniformly random as confirmed by:

    ./blockrand

#### Normalized benchmarks

using `time ./blockrand 100000000` *relative normalized* execution times were:

`-O3`:

    GNU   1.00 (fastest normalized)
    Flang 1.90
    PGI:  1.82
    ifort 8.22

Tested with:

* Flang 6.0
* Gfortran 6, 7, 8
* Intel Fortran 2019
* PGI 18.10

So Intel Fortran takes over 5 times longer than Gfortran at this simple single-thread benchmark.

### References

* [Control codes](https://en.wikipedia.org/wiki/C0_and_C1_control_codes)
* legacy / demo author:   [Lewis Bobbermen](https://github.com/lewisjb)

