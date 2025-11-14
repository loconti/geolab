#!/bin/bash
perc="/home/user/geolab/australia"

gfortran  -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  -c ${perc}/modulematrix.f90 -o ${perc}/modulematrix.o
gfortran  -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace  ${perc}/modulematrix.o ${perc}/australia_temperature.f90 -o ${perc}/australia_temperature.exe
