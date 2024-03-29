# BlockTran

[![DOI](https://zenodo.org/badge/122394926.svg)](https://zenodo.org/badge/latestdoi/122394926)
![Actions Status](https://github.com/fortran-gaming/blocktran/workflows/ci_cmake/badge.svg)

Text/console falling-block tetromino game written in object-oriented Fortran 2008.

![BlockTran gameplay demo](doc/blocktran.gif)

* user-configurable playfield size
* Logs pieces played to `blocktran.log` so you can recreate memorable games.
* uniform random game piece generation.
* clean, object-oriented Fortran 2008 syntax
* Curses (Ncurses, PDcurses) used for display, called directly from Fortran code.

## Build

BlockTran works on Mac, Linux, native Windows, Cygwin, Windows Subsystem for Linux.
Requires a Fortran compiler and CMake.
Known to work with GCC Gfortran and Intel oneAPI (Classic or LLVM).

```sh
cmake -B build
cmake --build build
```

or using Fortran FPM:

```sh
fpm build
fpm run
```

The main executable file is build/blocktran.
You can copy this file to your Desktop or wherever you like.

### Linux / macOS

Ncurses is required.
Alternatively, PDCurses + X11 development libraries are required for building on Linux.

Install X11 on Linux:

```sh
dnf install libX11-devel libXt-devel libXaw-devel ncurses-devel
# or
apt install libmxu-dev libxpm-dev libxt-dev libxaw7-dev libx11-dev
```

on macOS:

```sh
brew install libxt libxaw libx11
```

## Play

### difficulty level

adjust cadence of falling blocks with `-d` option, including decimal point:

```bash
blocktran -d 1.2
```

Higher number increase difficulty. Must include decimal point.

### Playfield size

specify width and height of the playfield with `-s` option:

```bash
blocktran -s 20 15
```

### play against computer

The computer player is rudimentary.

```bash
blocktran -p 2
```

### debug mode

Debug logging is enabled by:

```bash
blocktran --debug
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

Block distribution is uniformly random as confirmed by:

```sh
./blockrand
```

### References

* [Control codes](https://en.wikipedia.org/wiki/C0_and_C1_control_codes)
* legacy / demo author:   [Lewis Bobbermen](https://github.com/lewisjb)
