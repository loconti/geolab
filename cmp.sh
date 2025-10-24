#!/bin/bash

# usage: cmpl file_fortran_with_no_ext (-g ?)
# all other flags are passed directly to gfortran

echo "Compiling ${1}.f90 -> ${1}.exe"

gdbgflags=""
supplemental_flags=""
filename=""

for arg in "${@}"; do
    if [[ "${arg}" == "-g" ]]; then
        gdbgflags="-g -Og"
        echo "Debug mode"
    elif [[ "${arg}" == -* ]]; then
        supplemental_flags="${supplemental_flags} ${arg}"
        echo "With supplemental: ${arg}"
    else
        filename="${arg}"
    fi
done

if gfortran ${gdbflags} ${supplemental_flags} -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace "${filename}.f90" -o "${filename}.exe"; then
    echo "Success!"
else
    echo "Insuccess!"
fi
