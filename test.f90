program test

    use ncio 

    use ismip6

    implicit none 

    ! Internal constants
    integer,  parameter :: dp  = kind(1.d0)
    integer,  parameter :: sp  = kind(1.0)

    ! Choose the working precision of the library (sp,dp)
    integer,  parameter :: wp = sp 

    ! Define default missing value 
    real(wp), parameter :: mv = -9999.0_wp 
    


    type(ismip6_forcing_class) :: ismp 


    real(wp) :: time_init, time_end, time, dt
    integer  :: n, k

    ! === Testing output writing 

    call ismip6_write_step(filename="icesheet_ismip6.nc",file_nml="ismip6.nml")


    stop 

    ! ======================================================================

    ! Initialize variables inside of ismip6 object 
    call ismip6_forcing_init(ismp,"ismip6.nml","noresm_rcp85", &
                            domain="Antarctica",grid_name="ANT-32KM")


    ! Print some information for static variables
    write(*,*) "================" 
    call print_var_range(ismp%ts_ref%var, "ts_ref", mv) 
    call print_var_range(ismp%pr_ref%var, "pr_ref", mv) 
    call print_var_range(ismp%smb_ref%var,"smb_ref",mv) 
    write(*,*) "----"
    call print_var_range(ismp%to_ref%var, "to_ref", mv) 
    call print_var_range(ismp%so_ref%var, "so_ref", mv) 
    call print_var_range(ismp%tf_ref%var, "tf_ref", mv) 
    call print_var_range(ismp%tf_cor%var, "tf_cor", mv) 
    write(*,*) 

    ! ======================================================================
    
    ! Perform timestepping
    time_init = 1840.0 
    time_end  = 2110.0 
    dt        = 10.0 

    do n = 1, ceiling((time_end-time_init)/dt)+1

        ! Get current time 
        time = time_init + (n-1)*dt

        ! Update ismip6 forcing to current time
        call ismip6_forcing_update(ismp,time)


        ! Check data
        write(*,*) "================"
        call print_var_range(ismp%ts%var, "ts", mv,time) 
        call print_var_range(ismp%pr%var, "pr", mv,time) 
        call print_var_range(ismp%smb%var,"smb",mv,time) 
        write(*,*) "----"
        call print_var_range(ismp%to%var, "to", mv,time) 
        call print_var_range(ismp%so%var, "so", mv,time) 
        call print_var_range(ismp%tf%var, "tf", mv,time) 
        write(*,*) 

    end do

    stop "Done testing ismip6 forcing."


contains

    subroutine print_var_range(var,name,mv,time)

        implicit none 

        real(wp),         intent(IN) :: var(:,:,:,:) 
        character(len=*), intent(IN) :: name
        real(wp),         intent(IN) :: mv 
        real(wp), intent(IN), optional :: time 

        ! Local variables 
        real(wp) :: vmin, vmax 

        if (count(var.ne.mv) .gt. 0) then 
            vmin = minval(var,mask=var.ne.mv)
            vmax = maxval(var,mask=var.ne.mv)
        else 
            vmin = mv 
            vmax = mv 
        end if 

        if (present(time)) then 
            write(*,"(f10.1,2x,a10,a3,2f14.3)") time, trim(name), ": ", vmin, vmax
        else 
            write(*,"(10x,2x,a10,a3,2f14.3)") trim(name), ": ", vmin, vmax
        end if 

        return 

    end subroutine print_var_range


end program test



