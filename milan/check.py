#!/bin/python3
import sys

COL = 2
DEC = True  # decresing order

if len(sys.argv) != 2:
    print("Error: give a file name as input")
    exit(1)

with open(sys.argv[1], 'r') as file:
    line = ''
    col_v = []
    prev = None
    while len(line) > 20 or not line:
        line = file.readline()
        if len(line.split()) == 0:
            line = file.readline()
            break
    while line:
        f_string = line.split()[COL-1]
        if f_string and f_string != '\n':
            val = float(f_string)
            col_v.append(val)
            if prev is not None:
                if DEC and prev < val:
                    print(len(col_v), 'not decreasing at', val)
                if not DEC and prev > val:
                    print(len(col_v), 'not increasing at', val)
            prev = val
        else:
            break
        line = file.readline()

print('Checked', len(col_v), 'values')
exit(0)