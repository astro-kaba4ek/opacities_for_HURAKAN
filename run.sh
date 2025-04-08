#!/bin/bash

mkdir build res 
cd src; rm -rf *.mod; cd ..
cd build &&
cmake .. -DCMAKE_BUILD_TYPE=Debug && 
make main && 
time ./main 
