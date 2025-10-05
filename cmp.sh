#!/bin/bash

perc="/home/user/geolab"
cd "${perc}"
echo "Compiling ${1}.f90 -> ${1}.exe"

gfortran -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace "${perc}/${1}.f90" -o "${perc}/${1}.exe"
