# ismip6

This repository is intended to hold useful general functions related to running the ISMIP6 stand-alone ice-sheet model simulations. 

## Getting started

1. Clone/download the repository.
2. Modify the Makefile to match your system (mainly just the variables `FC=gfortran` to specify the compiler and `INC_NC`/`LIB_NC` to specify the location of your NetCDF installation). 
3. In the file `test.nml`, check that the file paths match your available datasets.
4. Compile and run the test program: 
  
  ```
  make test
  ./test.x 
  ``` 

That's it. You should see some output to the screen indicating that the program is able to load some data.

