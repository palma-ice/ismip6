program test

    use ncio 

    use ismip6
    use varslice 

    implicit none 

    ! Internal constants
    integer,  parameter :: dp  = kind(1.d0)
    integer,  parameter :: sp  = kind(1.0)

    ! Choose the working precision of the library (sp,dp)
    integer,  parameter :: wp = sp 

    ! Define default missing value 
    real(wp), parameter :: mv = -9999.0_wp 
    
    ! Define seconds in a year
    real(wp), parameter :: sec_year = 31536000.0     ! [s/yr]  365*24*3600

    type(ismip6_forcing_class) :: ismip6 

    type(varslice_class)   :: ts_ref 
    type(varslice_class)   :: ts 
    type(varslice_class)   :: smb 
    type(varslice_class)   :: tf 

    character(len=1024)    :: file_ts_ref 
    character(len=1024)    :: file_ts
    character(len=1024)    :: file_smb 
    
    real(wp) :: time_init, time_end, time, dt
    integer  :: n

    real(wp) :: conv_smb 

    ! Define unit conversion factor [kg m-2 s-1] == [mm w.e. s-1] => [m i.e. yr-1]
    conv_smb = sec_year*1e-3*(1000.0/910.0)
    
    ! Define filenames 
    file_ts_ref = "ice_data/ISMIP6/Atmosphere/Antarctica/AIS-32KM/NorESM_RCP85/Atm_clim_1995-2014.nc"
    file_ts     = "ice_data/ISMIP6/Atmosphere/Antarctica/AIS-32KM/NorESM_RCP85/Atm_anom_1950-1994.nc"
    file_smb    = "ice_data/ISMIP6/Atmosphere/Antarctica/AIS-32KM/NorESM_RCP85/Atm_anom_1950-1994.nc"

    ! ======================================================================
    ! Initialize variables

    if (.FALSE.) then 

        ! Initialize variables using subroutine arguments 

        ! Define a variable without a time dimension
        call varslice_init_arg(ts_ref,filename=file_ts_ref,name="ts", &
            units_in="K",units_out="K",with_time=.FALSE.)

        ! Define variables with a time dimension
        call varslice_init_arg(ts,filename=file_ts,name="ts", &
            units_in="K",units_out="K",with_time=.TRUE.,time_par=[1950.,1994.,1.])

        call varslice_init_arg(smb,filename=file_smb,name="smb", &
            units_in="kg m-2 s-1",units_out="m i.e. yr-1", &
            scale=conv_smb,offset=0.0,with_time=.TRUE.,time_par=[1950.,1994.,1.])

    else 

        ! Initialize variables using namelist file 

        ! Define a variable without a time dimension
        call varslice_init_nml(ts_ref,filename="test.nml",group="noresm_rcp85_ts_ref")

        ! Define variables with a time dimension
        call varslice_init_nml(ts, filename="test.nml",group="noresm_rcp85_ts")
        call varslice_init_nml(smb,filename="test.nml",group="noresm_rcp85_smb")

        call varslice_init_nml(tf,filename="test.nml",group="noresm_rcp85_tf")

    end if 

    ! Initialize variables inside of ismip6 object 
    call ismip6_forcing_init(ismip6,"test.nml","noresm_rcp85")

    ! ======================================================================
    
    ! Write some info to the screen
    write(*,*) "info: " 
    write(*,*) "conv_smb = ", conv_smb 
    write(*,*) size(ts_ref%var,1), size(ts_ref%var,2), size(ts_ref%var,3)
    write(*,*) size(ts%var,1),     size(ts%var,2),     size(ts%var,3)
    write(*,*) "time: ", ts%time 

    ! Load variable without time dimension
    call varslice_update(ts_ref)

    ! Check data 
    write(*,*) "ts_ref: ",  minval(ts_ref%var,mask=ts_ref%var.ne.mv), &
                            maxval(ts_ref%var,mask=ts_ref%var.ne.mv)


    ! Perform timestepping
    time_init = 1950.0 
    time_end  = 1994.0 
    dt        = 1.0 

    do n = 1, ceiling((time_end-time_init)/dt)+1

        ! Get current time 
        time = time_init + (n-1)*dt

        if (.FALSE.) then
            ! Use local varslice variables 

            ! Load variables for current time 
            call varslice_update(ts,time)
            call varslice_update(smb,time)
            call varslice_update(tf,time)

            ! Check data
            call print_var_range(ts%var, "ts", mv,time) 
            call print_var_range(smb%var,"smb",mv,time) 
            call print_var_range(tf%var, "tf", mv,time) 
            write(*,*) 

        else 
            ! Use ismip6 object variables 

            call ismip6_forcing_update(ismip6,time)

            ! Check data
            call print_var_range(ismip6%ts%var, "ts", mv,time) 
            call print_var_range(ismip6%smb%var,"smb",mv,time) 
            call print_var_range(ismip6%tf%var, "tf", mv,time) 
            write(*,*) 

        end if 

    end do

    stop "Done testing ismip6 forcing."

contains

    subroutine print_var_range(var,name,mv,time)

        implicit none 

        real(wp),         intent(IN) :: var(:,:,:) 
        character(len=*), intent(IN) :: name
        real(wp),         intent(IN) :: mv 
        real(wp), intent(IN), optional :: time 

        if (present(time)) then 
            write(*,"(f10.1,2x,2a,2f14.3)") time, trim(name), ": ", &
                minval(var,mask=var.ne.mv), maxval(var,mask=var.ne.mv)
        else 
            write(*,"(10x,2x,2a,2f14.3)") trim(name), ": ", &
                minval(var,mask=var.ne.mv), maxval(var,mask=var.ne.mv)
        end if 

        return 

    end subroutine print_var_range


end program test



