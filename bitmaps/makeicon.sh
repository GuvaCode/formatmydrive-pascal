#!/bin/sh
rm ../icons.hpp ../icons.cxx

FORMAT=*.png

reswrap-1.7 -k --header -o ../icons.hpp $FORMAT --source -k --include icons.hpp -o ../icons.cxx $FORMAT


