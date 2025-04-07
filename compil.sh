mkdir build res 
cd src; rm -rf *.mod; cd ..
cd build &&
cmake .. -DCMAKE_BUILD_TYPE=Release && 
make main #&&
# time ./main 
