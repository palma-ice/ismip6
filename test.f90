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

    type(varslice_class)   :: ts_ref 
    type(varslice_class)   :: ts 
    type(varslice_class)   :: smb 
    
    real(wp) :: time_init, time_end, time, dt
    integer  :: n

    real(wp) :: conv_smb 

    ! Define unit conversion factor [kg m-2 s-1] == [mm w.e. s-1] => [m i.e. yr-1]
    conv_smb = sec_year*1e-3*(1000.0/910.0)
    
    ! Define a variable without a time dimension
    call varslice_init_arg(ts_ref, &
        filename="ice_data/ISMIP6/Atmosphere/Antarctica/AIS-32KM/NorESM_RCP85/Atm_clim_1995-2014.nc", &
        name="ts",units_in="K",units_out="K",with_time=.FALSE.)

    ! Define variables with a time dimension
    call varslice_init_arg(ts, &
        filename="ice_data/ISMIP6/Atmosphere/Antarctica/AIS-32KM/NorESM_RCP85/Atm_anom_1950-1994.nc", &
        name="ts",units_in="K",units_out="K",with_time=.TRUE.,time_par=[1950.,1994.,1.])

    call varslice_init_arg(smb, &
        filename="ice_data/ISMIP6/Atmosphere/Antarctica/AIS-32KM/NorESM_RCP85/Atm_anom_1950-1994.nc", &
        name="smb",units_in="kg m-2 s-1",units_out="m i.e. yr-1", &
        scale=conv_smb,offset=0.0,with_time=.TRUE.,time_par=[1950.,1994.,1.])

    ! Write some info to the screen
    write(*,*) "info: " 
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

        ! Load variables for current time 
        call varslice_update(ts,time)
        call varslice_update(smb,time)

        ! Check data 
        write(*,*) ts%time(n)," ts:  ", minval(ts%var,mask=ts%var.ne.mv), &
                                        maxval(ts%var,mask=ts%var.ne.mv)
        write(*,*) ts%time(n)," smb: ", minval(smb%var,mask=smb%var.ne.mv), &
                                        maxval(smb%var,mask=smb%var.ne.mv)

    end do

    stop "Done testing ismip6 forcing."

end program test



