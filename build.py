#!/usr/bin/env python
"""

## PGI

We assume you're running from the PGI console on Windows, or otherwise have PATH set
to PGI compilers in environment before running this script.


## Intel

We assume you've already:

* Windows: compilervars.bat intel64
* Linux / Mac: source compilervars.sh intel64

or otherwise set environment variable MKLROOT.
This is how Numpy finds Intel MKL / compilers as well.

## MKL

MSVC requires MKL too

"""
from pathlib import Path
import os
import shutil
import subprocess
from typing import Dict, List, Tuple
from argparse import ArgumentParser

MESON = shutil.which('meson')
NINJA = shutil.which('ninja')
CMAKE = shutil.which('cmake')

MSVC = 'Visual Studio 15 2017'

# Must have .resolve() to work in general regardless of invocation directory
SRC = Path(__file__).parent.resolve()
BUILD = SRC / 'build'


# %% function
def do_build(buildsys: str, compilers: Dict[str, str],
             args: List[str],
             wipe: bool = True,
             dotest: bool = True,
             install: str = None):
    """
    attempts build with Meson or CMake
    """
    if buildsys == 'meson' and MESON and NINJA:
        meson_setup(compilers, args, wipe, dotest, install)
    elif buildsys == 'cmake' and CMAKE:
        cmake_setup(compilers, args, wipe, dotest, install)
    else:
        raise FileNotFoundError('Could not find CMake or Meson + Ninja')


def _needs_wipe(fn: Path, wipe: bool) -> bool:
    if not fn.is_file():
        return False

    with fn.open() as f:
        for line in f:
            if line.startswith('CMAKE_C_COMPILER:FILEPATH'):
                cc = line.split('/')[-1]
                if cc != compilers['CC']:
                    wipe = True
                    break
            elif line.startswith('CMAKE_GENERATOR:INTERNAL'):
                gen = line.split('=')[-1]
                if gen.startswith('Unix') and os.name == 'nt':
                    wipe = True
                    break
                elif gen.startswith(('MinGW', 'Visual')) and os.name != 'nt':
                    wipe = True
                    break
                elif gen.startswith('Visual') and compilers['CC'] != 'cl':
                    wipe = True
                    break

    return wipe


def cmake_setup(compilers: Dict[str, str],
                args: List[str],
                wipe: bool = True, dotest: bool = True,
                install: str = None):
    """
    attempt to build using CMake >= 3

    The automatic detection of when a wipe of CMakeCache.txt is needed isn't perfect.
    """
    if compilers['CC'] == 'cl':
        wopts = ['-G', MSVC, '-A', 'x64']
    elif os.name == 'nt':
        wopts = ['-G', 'MinGW Makefiles', '-DCMAKE_SH="CMAKE_SH-NOTFOUND']
    else:
        wopts = []

    wopts += args

    if isinstance(install, str) and install.strip():  # path specified
        wopts.append('-DCMAKE_INSTALL_PREFIX:PATH='+str(Path(install).expanduser()))

    cachefile = BUILD / 'CMakeCache.txt'

    wipe = _needs_wipe(cachefile, wipe)

    if wipe and cachefile.is_file():
        cachefile.unlink()

    # we didn't use -S -B to be compatible with CMake < 3.12
    ret = subprocess.run([CMAKE] + wopts + [str(SRC)],
                         cwd=BUILD, env=os.environ.update(compilers))
    if ret.returncode:
        raise SystemExit(ret.returncode)

    ret = subprocess.run([CMAKE, '--build', str(BUILD)], universal_newlines=True)

    test_result(ret)

# %% test
    if dotest:
        ctest_exe = shutil.which('ctest')
        if not ctest_exe:
            raise FileNotFoundError('CTest not available')

        if compilers['CC'] == 'cl':
            ret = subprocess.run([CMAKE, '--build', str(BUILD), '--target', 'RUN_TESTS'])
            if ret.returncode:
                raise SystemExit(ret.returncode)
        else:
            subprocess.run([ctest_exe, '--output-on-failure'], cwd=BUILD)
            if ret.returncode:
                raise SystemExit(ret.returncode)
# %% install
    if install is not None:  # blank '' or ' ' etc. will use dfault install path
        subprocess.run([CMAKE, '--build', str(BUILD), '--target', 'install'])
        if ret.returncode:
            raise SystemExit(ret.returncode)


def meson_setup(compilers: Dict[str, str],
                args: List[str],
                wipe: bool = True, dotest: bool = True,
                install: str = None):
    """
    attempt to build with Meson + Ninja
    """
    build_ninja = BUILD / 'build.ninja'

    meson_setup = [MESON] + ['setup'] + args

    if isinstance(install, str) and install.strip():  # path specified
        meson_setup.append('--prefix '+str(Path(install).expanduser()))

    if wipe and build_ninja.is_file():
        meson_setup.append('--wipe')
    meson_setup += [str(BUILD), str(SRC)]

    if wipe or not build_ninja.is_file():
        ret = subprocess.run(meson_setup, env=os.environ.update(compilers))
        if ret.returncode:
            raise SystemExit(ret.returncode)

    ret = subprocess.run([NINJA, '-C', str(BUILD)], stderr=subprocess.PIPE,
                         universal_newlines=True)

    test_result(ret)

    if dotest:
        if not ret.returncode:
            ret = subprocess.run([MESON, 'test', '-C', str(BUILD)])  # type: ignore     # MyPy bug
            if ret.returncode:
                raise SystemExit(ret.returncode)

    if install:
        if not ret.returncode:
            ret = subprocess.run([MESON, 'install', '-C', str(BUILD)])  # type: ignore     # MyPy bug
            if ret.returncode:
                raise SystemExit(ret.returncode)


def test_result(ret: subprocess.CompletedProcess):
    if not ret.returncode:
        print('\nBuild Complete!')
    else:
        raise SystemExit(ret.returncode)


# %% compilers
def clang_params(impl: str) -> Tuple[Dict[str, str], List[str]]:
    """
    LLVM compilers e.g. Clang, Flang
    """
    compilers = {'CC': 'clang', 'CXX': 'clang++', 'FC': 'flang'}

    args: List[str] = []

    return compilers, args


def gnu_params(impl: str) -> Tuple[Dict[str, str], List[str]]:
    """
    GNU compilers e.g. GCC, Gfortran
    """
    compilers = {'FC': 'gfortran', 'CC': 'gcc', 'CXX': 'g++'}

    args: List[str] = []

    return compilers, args


def intel_params() -> Tuple[Dict[str, str], List[str]]:
    """
    Intel compilers
    """
    if not os.environ.get('MKLROOT'):
        raise EnvironmentError('must have set MKLROOT by running compilervars.bat or source compilervars.sh before this script.')

    # %% compiler variables
    compilers = {'FC': 'ifort'}

    if os.name == 'nt':
        compilers['CC'] = compilers['CXX'] = 'icl'
    else:
        compilers['CC'] = 'icc'
        compilers['CXX'] = 'icpc'

    args: List[str] = []

    return compilers, args


def msvc_params() -> Tuple[Dict[str, str], List[str]]:
    """
    Micro$oft Visual Studio

    Note in general MSVC doesn't have good modern C++ features,
    so don't be surprised if a C++11 or newer program doesn't compile.
    """
    if not shutil.which('cl'):
        raise EnvironmentError('Must have PATH set to include MSVC cl.exe compiler bin directory')

    compilers = {'CC': 'cl', 'CXX': 'cl'}

    args: List[str] = []

    return compilers, args


def pgi_params(impl: str) -> Tuple[Dict[str, str], List[str]]:
    """
    Nvidia PGI compilers

    pgc++ is not available on Windows at this time
    """
    if not shutil.which('pgcc') or not shutil.which('pgfortran'):
        raise EnvironmentError('Must have PATH set to include PGI compiler bin directory')

    # %% compiler variables
    compilers = {'FC': 'pgfortran', 'CC': 'pgcc'}
    if os.name != 'nt':
        compilers['CXX'] = 'pgc++'

    args: List[str] = []

    return compilers, args


if __name__ == '__main__':
    p = ArgumentParser()
    p.add_argument('vendor', help='compiler vendor [clang, gnu, intel, msvc, pgi]', nargs='?', default='gnu')
    p.add_argument('-wipe', help='wipe and rebuild from scratch', action='store_true')
    p.add_argument('-b', '--buildsys', help='default build system', default='cmake')
    p.add_argument('-i', '--implementation',
                   help='which LAPACK implementation')
    p.add_argument('-n', '--no-test', help='do not run self-test / example', action='store_false')
    p.add_argument('-install', help='specify directory to install to')
    a = p.parse_args()

    dotest = a.no_test

    if a.vendor == 'clang':
        compilers, args = clang_params(a.implementation)
    elif a.vendor in ('gnu', 'gcc'):
        compilers, args = gnu_params(a.implementation)
    elif a.vendor == 'intel':
        compilers, args = intel_params()
    elif a.vendor == 'msvc':
        compilers, args = msvc_params()
    elif a.vendor == 'pgi':
        compilers, args = pgi_params(a.implementation)
    else:
        raise ValueError('unknown compiler vendor {}'.format(a.vendor))

    do_build(a.buildsys, compilers, args, a.wipe, dotest, a.install)
