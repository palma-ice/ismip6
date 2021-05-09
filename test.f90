

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
    
    type(varslice_class)   :: vs_ts_ref 
    type(varslice_class)   :: vs_ts 
      
    real(wp) :: time_init, time_end, time, dt
    integer  :: n

    ! Testing varslice 
    call varslice_init_arg(vs_ts_ref, &
        filename="ice_data/ISMIP6/Atmosphere/Antarctica/AIS-32KM/NorESM_RCP85/Atm_clim_1995-2014.nc", &
        name="ts",units_in="K",units_out="K",with_time=.FALSE.)
    call varslice_init_arg(vs_ts, &
        filename="ice_data/ISMIP6/Atmosphere/Antarctica/AIS-32KM/NorESM_RCP85/Atm_anom_1950-1994.nc", &
        name="ts",units_in="K",units_out="K",with_time=.TRUE.,time_par=[1950.,1994.,1.])

    write(*,*) "info: " 
    write(*,*) size(vs_ts_ref%var,1), size(vs_ts_ref%var,2), size(vs_ts_ref%var,3)
    write(*,*) size(vs_ts%var,1), size(vs_ts%var,2), size(vs_ts%var,3)
    write(*,*) vs_ts%time 

    call varslice_update(vs_ts_ref)

    ! Check data 
    write(*,*) "vs_ts_ref: ", minval(vs_ts_ref%var,mask=vs_ts_ref%var.ne.mv), &
                                 maxval(vs_ts_ref%var,mask=vs_ts_ref%var.ne.mv)

    time_init = 1950.0 
    time_end  = 1994.0 
    dt        = 1.0 

    do n = 1, ceiling((time_end-time_init)/dt)+1

        ! Get current time 
        time = time_init + (n-1)*dt

        call varslice_update(vs_ts,time)

        ! Check data 
        write(*,*) vs_ts%time(n)," : ",  minval(vs_ts%var,mask=vs_ts%var.ne.mv), &
                                            maxval(vs_ts%var,mask=vs_ts%var.ne.mv)

    end do 

    stop "Done testing ismip6 forcing."

end program test



