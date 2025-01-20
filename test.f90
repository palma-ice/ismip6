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

    type(ismip6_experiment_class) :: iexp 
    type(ismip6_forcing_class) :: ismp 
    type(varslice_class)       :: v1 

    real(wp) :: time_init, time_end, time, dt
    integer  :: n, k

    ! Testing methods on one varslice variable ==============
if (.FALSE.) then 
    call make_test_file("var_test.nc")
    call varslice_init_nml(v1,"ismip6.nml",group="var1")
    call varslice_update(v1, [1959.15_wp],method="exact")
    call print_var_range(v1%var, "var1", mv) 
    call varslice_update(v1, [1959.15_wp],method="interp")
    call print_var_range(v1%var, "var1", mv) 
    call varslice_update(v1, [1959.15_wp],method="extrap")
    call print_var_range(v1%var, "var1", mv) 
    stop 
end if 
    ! =======================================================


    ! === Testing output writing ===

    ! call ismip6_write_step(filename="icesheet_ismip6.nc",file_nml="ismip6.nml",time=0.0_wp)
    ! stop 

    ! ======================================================================

    ! Running Antarctica domain, load Antarctica specific parameters
    call ismip6_experiment_def(iexp,"ctrlAE","ismip6_ant.nml","UCM","YELMO")

    ! Initialize variables inside of ismip6 object 
    call ismip6_forcing_init(ismp,"ismip6_ant.nml","Antarctica","ANT-32KM", &
                                experiment=iexp%experiment,shlf_collapse=iexp%shlf_collapse)
    
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

        if (time .eq. time_init) then 
            call make_test_file_2("test.nc",ismp%tf%var(:,:,1,1),time,init=.TRUE.)
        else 
            call make_test_file_2("test.nc",ismp%tf%var(:,:,1,1),time,init=.FALSE.)
        end if 

    end do

    write(*,*) "Done testing ismip6 forcing."
    write(*,*)

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

    subroutine make_test_file(filename)

        implicit none 

        character(len=*), intent(IN) :: filename 

        ! Local variables
        integer :: k, nt  
        real(wp), allocatable :: var(:,:,:) 

        ! Create the netcdf file 
        call nc_create(filename)

        ! Add grid axis variables to netcdf file
        call nc_write_dim(filename,"xc",x=[1,2,3],units="1")

        call nc_write_dim(filename,"yc",x=[2,4,6],units="1")
        
        ! Add time axis with current value 
        call nc_write_dim(filename,"time", x=1950.0_wp,dx=1.0_wp,nx=11,units="years",unlimited=.TRUE.)
        
        
        nt = 11 
        allocate(var(3,3,nt))

        do k = 1, nt 
            var(:,:,k) = 1950.0 + real(k-1,wp)
        end do 

        call nc_write(filename,"var1",var,dim1="xc",dim2="yc",dim3="time")

        return 

    end subroutine make_test_file


    subroutine make_test_file_2(filename,var,time,init)

        implicit none 

        character(len=*), intent(IN) :: filename 
        real(wp),         intent(IN) :: var(:,:) 
        real(wp),         intent(IN) :: time
        logical,          intent(IN) :: init 

        ! Local variables
        integer  :: nx, ny, n 
        real(wp) :: time_prev 

        nx = size(var,1) 
        ny = size(var,2) 

        if (init) then 
            ! Create the netcdf file 
            call nc_create(filename)

            ! Add grid axis variables to netcdf file
            call nc_write_dim(filename,"xc",x=1,dx=1,nx=nx,units="1")
            call nc_write_dim(filename,"yc",x=1,dx=1,nx=ny,units="1")
        
            ! Add time axis with current value 
            call nc_write_dim(filename,"time", x=time,dx=1.0_wp,nx=1,units="years",unlimited=.TRUE.)
        
        end if  

        ! Determine current writing time step 
        n = nc_size(filename,"time")
        call nc_read(filename,"time",time_prev,start=[n],count=[1]) 
        if (abs(time-time_prev).gt.1e-5) n = n+1 

        ! Update the time step
        call nc_write(filename,"time",time,dim1="time",start=[n],count=[1])

        ! Write the variable
        call nc_write(filename,"var",var,dim1="xc",dim2="yc",dim3="time", &
                                                start=[1,1,n],count=[nx,ny,1])

        return 

    end subroutine make_test_file_2

    
end program test



