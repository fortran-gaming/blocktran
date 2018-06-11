#!/bin/bash
# boilerplate to test with popular Fortran compilers, helping check for quirks
cmd="./tetran -s 10 10"

set -e

# --- colors https://stackoverflow.com/a/20983251
red=`tput setaf 1`
reset=`tput sgr0`

# --- loops
for comp in gfortran nag flang pgf95 ifort
do

(
[[ $comp == ifort ]] && . ~/intel.sh

cd "${0%/*}"  # change to directory of this script

echo
if $comp --version; then
  echo  
  echo "testing with"
  echo $comp
  echo "press Enter to proceed."
  read
else
  echo
  echo "${red}*** skipping $comp *** ${reset}"
  continue
fi

rm -r ../bin/*
cd ../bin

FC=$comp cmake ..

make -j -l 2
make test

$cmd
)
  
done

cd "${0%/*}"  # change to directory of this script
rm -r ../bin/*
