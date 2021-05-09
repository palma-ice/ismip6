# ismip6

This repository is intended to hold useful general functions related to running the ISMIP6 stand-alone ice-sheet model simulations. 

## Getting started

1. Clone/download the repository.
2. Modify the Makefile to match your system (mainly just the variables `FC=gfortran` to specify the compiler and `INC_NC`/`LIB_NC` to specify the location of your NetCDF installation). 
3. Compile the test program: `make test`. 
4. In the file `test.f90`, change the following variables to match your file paths:

  ```
  file_ts_ref = PATH1
  file_ts     = PATH2 
  file_smb    = PATH3 
  ```

5. Run the test program: `./test.x`. 

That's it. You should see some output to the screen indicating that the program is able to load some data.

