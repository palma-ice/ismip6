# ismip6

This repository is intended to hold useful general functions related to running the ISMIP6 stand-alone ice-sheet model simulations. The ismip6 module contains a set of functions for loading ISMIP6 data corresponding to particular simulations without the need for defining many variables. 

The idea is to perform metadata and data loading as seperate steps. So, in a program where we want to use some ISMIP6 forcing dataset, first we could call `ismip6_forcing_init`:

```
! Initialize variables inside of ismip6 object 
    call ismip6_forcing_init(ismp,"ismip6.nml","noresm_rcp85", &
                            domain="Antarctica",grid_name="ANT-32KM")
```

Here, the `ismp` object (derived type) will be populated with the meta data relevant to our datasets. The information about each variable to be loaded is obtained from the namelist parameter file `ismip6.nml`, and specifically in this case we are interested in loading data for the `noresm_rcp85` experiment on the `Antarctica` domain and grid with name `ANT-32KM`. 

This step has not loaded any data. But now `ismp` will be able to load data corresponding to a specific time step when we want. To do so, we can call:

```
! Update ismip6 forcing to current time
call ismip6_forcing_update(ismp,time)
```

Here, the actual data is loaded, but only for the specific time of interest designated by `time`. Typically, this routine would be called within a time loop, so that the ismip6 forcing data could be updated to each timestep. 

## Quick start

1. Clone/download the repository.
2. Modify the Makefile to match your system (mainly just the variables `FC=gfortran` to specify the compiler and `INC_NC`/`LIB_NC` to specify the location of your NetCDF installation). 
3. In the file `test.nml`, check that the file paths match your available datasets.
4. Compile and run the test program: 
  
  ```
  make test
  ./test.x 
  ``` 

That's it. You should see some output to the screen indicating that the program is able to load some data.

## To do
  
- Add additional fields (retreat masks, etc) to ISMIP6 object.