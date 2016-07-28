#!/bin/bash
gcc -std=c99 -O2 -Wall -c udev.c 
#gcc -g -c partition.c
mv *.o ..
echo "movido"

