module ismip6
    ! This module contains routines that help with performing the ISMIP6 suite
    ! of experiments. 
    
    use varslice 

    implicit none 

    ! Internal constants
    integer,  parameter :: dp  = kind(1.d0)
    integer,  parameter :: sp  = kind(1.0)

    ! Choose the working precision of the library (sp,dp)
    integer,  parameter :: wp = sp 

    ! Define default missing value 
    real(wp), parameter :: mv = -9999.0_wp 

    ! Class for holding ice-forcing data from ISMIP6 archives
    type ismip6_forcing_class
        
        ! Atmospheric fields
        type(varslice_class)   :: ts_ref 
        type(varslice_class)   :: ts 
        type(varslice_class)   :: smb

        ! Oceanic fields 
        type(varslice_class)   :: tf 

    end type

    ! Class for holding ice output for writing to standard formats...
    type ismip6_ice_class

    end type 


    private
    public :: ismip6_forcing_class
    public :: ismip6_ice_class
    public :: ismip6_forcing_init
    public :: ismip6_forcing_update

contains
    

    subroutine ismip6_forcing_init(ism,filename,experiment)

        implicit none 

        type(ismip6_forcing_class), intent(INOUT) :: ism
        character(len=*), intent(IN) :: filename
        character(len=*), intent(IN) :: experiment 

        ! Local variables 
        character(len=256) :: group_prefix 


        select case(trim(experiment))

            case("noresm_rcp85")

                group_prefix = "noresm_rcp85_"

            case DEFAULT 

                write(*,*) "ismip6_forcing_init:: Error: experiment not recognized."
                write(*,*) "experiment = ", trim(experiment) 
                stop 

        end select

        ! Initialize all variables from namelist entries 

        ! Amospheric fields
        call varslice_init_nml(ism%ts_ref,filename,group=trim(group_prefix)//"ts_ref")
        call varslice_init_nml(ism%ts, filename,group=trim(group_prefix)//"ts")
        call varslice_init_nml(ism%smb,filename,group=trim(group_prefix)//"smb")

        ! Oceanic fields
        call varslice_init_nml(ism%tf,filename,group=trim(group_prefix)//"tf")

        return 

    end subroutine ismip6_forcing_init


    subroutine ismip6_forcing_update(ism,time)

        implicit none 

        type(ismip6_forcing_class), intent(INOUT) :: ism
        real(wp), intent(IN) :: time

        ! Get slices for current time

        ! Atmospheric fields
        call varslice_update(ism%ts,time)
        call varslice_update(ism%smb,time)

        ! Oceanic fields 
        call varslice_update(ism%tf,time)


        ! Apply additional calculations 

        ! [To do, as needed] 
        
        return 

    end subroutine ismip6_forcing_update



end module ismip6