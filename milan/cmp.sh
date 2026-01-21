#!/bin/bash
perc="/home/user/geolab/milan"

gfortran  -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  -c ${perc}/modulefile.f90 -o ${perc}/modulefile.o
gfortran  -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  -c ${perc}/modulezpT.f90 -o ${perc}/modulezpT.o
gfortran  -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  ${perc}/modulefile.o ${perc}/modulezpT.o ${perc}/zpT.f90 -o ${perc}/zpT.exe
