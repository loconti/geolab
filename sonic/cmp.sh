#!/bin/bash
perc="/home/user/geolab/sonic"

gfortran  -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  -c ${perc}/modulefile.f90 -o ${perc}/modulefile.o
gfortran  -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  -c ${perc}/modulewind.f90 -o ${perc}/modulewind.o
gfortran  -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  ${perc}/modulefile.o ${perc}/modulewind.o ${perc}/wind.f90 -o ${perc}/wind.exe
