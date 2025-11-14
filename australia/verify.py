#!/bin/python3
import sys

if len(sys.argv) != 3:
    print("Error: give two file names as input")
    exit(2)

with open(sys.argv[1], 'r') as file1, open(sys.argv[2], 'r') as file2:
    line1 = ''
    line2 = ''
    while len(line1) < 100:
        line1 = file1.readline()
        
    while len(line2) < 100:
        line2 = file2.readline()

    while line1 and line2:
        line1 = file1.readline()
        line2 = file2.readline()
        n1 = [float(x) for x in line1.split(' ') if x and x!='\n']
        n2 = [float(x) for x in line2.split(' ') if x and x!='\n']
        if len(n1) != len(n2):
            print('Line length Mismatch')
            exit(1)
        if any([abs(i-j) > 0.01 for i,j in zip(n1,n2)]):
            print('Files Mismatch')
            exit(1)

    if line1 != line2:
        print('File lenght Mismatch')
        print('first' if line1 else 'second', 'is longer')
        exit(1)

print('Files Match')
exit(0)
