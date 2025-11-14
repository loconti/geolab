#!/bin/bash
perc="/home/user/geolab/radiation"

gfortran -Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace ${perc}/radiation_daily.f90 -o ${perc}/radiation_daily.exe
