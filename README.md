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

## Build and Play

BlockTran works on Mac, Linux, native Windows, Cygwin, Windows Subsystem for Linux.
Requires a Fortran compiler and CMake.
Known to work with GCC Gfortran and Intel oneAPI (Classic or LLVM).

```sh
cmake -B build
cmake --build build
```

CMake will automatically download and build Curses if you don't have it.

The main exectuable file is build/blocktran.
You can copy this file to your Desktop or wherever you like.

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

### Block Randomness

unlike some games, block distribution is uniformly random as confirmed by:

```sh
./blockrand
```

### References

* [Control codes](https://en.wikipedia.org/wiki/C0_and_C1_control_codes)
* legacy / demo author:   [Lewis Bobbermen](https://github.com/lewisjb)
