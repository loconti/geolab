#!/bin/bash
perc="/home/user/geolab/sonic"

gfortran -g -O0 -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  -c ${perc}/modulefile.f90 -o ${perc}/modulefile.o
gfortran -g -O0 -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  ${perc}/modulefile.o ${perc}/modulewind.o ${perc}/wind.f90 -o ${perc}/wind.exe
