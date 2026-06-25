module wam_output_netcdf_module
! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS ALL NECESSARY SERIAL NETCDF OUTPUT RELATED            !   
!   SUBROUTINES.                                                               !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

use netcdf
use wam_mpi_module, only: irank, petotal, i_out_par 
use wam_file_module, only: IU06
use wam_grid_module, only: NX, NY, AMOWEP, AMOSOP, AMOEAP, AMONOP
use wam_output_parameter_module, only: params, CMEMS_OUTPUT_FLAG
use wam_coordinate_module, only: M_DEGREE_R
use wam_output_set_up_module, only: NFLAG_P
use wam_timopt_module, only: time_conversion

IMPLICIT NONE
public :: create_netcdf_output_file
public :: write_variables_to_netcdf_output_file
integer, parameter :: total_int_parameters = 70
integer :: TIME_DIM_ID, LAT_DIM_ID, LON_DIM_ID, NETCDF_FILE_ID
integer :: VARIABLE_IDS(total_int_parameters+4) = -1
character(len=:), allocatable :: filepath_name
real*8, allocatable, dimension(:) :: longitude_grid, latitude_grid
real*8  :: xdello, xdella
integer :: i
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

contains

subroutine check_status(status)
  implicit none
  INTEGER, intent (in) :: status
  !character(len=20), intent(in) :: routine
    if (status /= NF90_NOERR) then
       write(IU06, *) "+++ Error : ", NF90_STRERROR(status), "+++  Aborting!!"
       stop
    end if

end subroutine check_status

subroutine create_netcdf_output_file(output_date_time)
  
  character(len=*), intent(in) :: output_date_time
  !character(len=*), intent(out) :: filepath_name
  integer :: status
  
  filepath_name = 'WAVE'//TRIM(output_date_time)//'.nc'
  NETCDF_FILE_ID = -1
  
  if (irank == i_out_par) then
    status = nf90_create(path = filepath_name,       & 
                       cmode = ior(NF90_CLOBBER, NF90_NETCDF4),                     &
                       ncid = NETCDF_FILE_ID)
    call check_status(status)                   
    call check_status(nf90_enddef(NETCDF_FILE_ID))
  end if
  
end subroutine

subroutine create_lon_lat_arrays()
  
  allocate(longitude_grid(NX), latitude_grid(NY))

  ! -- Initial lon lat values 
  longitude_grid(1) = AMOWEP
  latitude_grid(1) = AMOSOP
  
  xdello = (AMOEAP - AMOWEP)/float(NX-1)
  xdella = (AMONOP - AMOSOP)/float(NY-1)
  
  ! -- Longitude loop
  DO i=2,NX
    longitude_grid(i) = longitude_grid(i-1) + xdello
  END DO

  DO i=2,NY
    latitude_grid(i) = latitude_grid(i-1) + xdella
  END DO

  longitude_grid = longitude_grid/M_DEGREE_R
  latitude_grid = latitude_grid/M_DEGREE_R

end subroutine
subroutine create_dimensions()

  character(len=100)   :: long_name_int_params
  character(len=100)   :: std_name_int_params, coordinates
  character(len=60)   :: name_int_params
  character(len=15)   :: units_int_params
  real                :: scaling_factor_int_params, vl_min_int_params, vl_max_int_params
  real                :: fill_value, missing_value
  logical             :: direction_flag_int_params
  integer             :: NO_FILL, ix, iy
  integer :: i
  character(len=8)    :: dateb
  character(len=10)   :: timeb
  
  if (irank == i_out_par) then
    call create_lon_lat_arrays()
    CALL DATE_AND_TIME(dateb,timeb)

    call check_status(nf90_open(path = filepath_name, mode = ior(nf90_write,nf90_share), ncid =NETCDF_FILE_ID))
    call check_status(nf90_redef(NETCDF_FILE_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID,'time', NF90_UNLIMITED, TIME_DIM_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID,'longitude', NX, LON_DIM_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID,'latitude', NY, LAT_DIM_ID))
    
    call check_status(nf90_def_var(NETCDF_FILE_ID,'longitude', NF90_DOUBLE, LON_DIM_ID, VARIABLE_IDS(total_int_parameters+1)))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+1), "standard_name", "longitude"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+1), "long_name", "longitude"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+1), "units", "degrees_east"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+1), "axis", "X"))

    call check_status(nf90_def_var(NETCDF_FILE_ID,'latitude', NF90_DOUBLE, LAT_DIM_ID, VARIABLE_IDS(total_int_parameters+2)))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+2), "standard_name", "latitude"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+2), "long_name", "latitude"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+2), "units", "degrees_north"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+2), "axis", "Y"))

    call check_status(nf90_def_var(NETCDF_FILE_ID,'time', NF90_DOUBLE, TIME_DIM_ID, VARIABLE_IDS(total_int_parameters+3)))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+3), "standard_name", "time"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+3), "long_name", "time"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+3), & 
                                  "units", "seconds since 1950-01-01 00:00:00"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+3), "calendar", "standard"))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+3), "axis", "T"))

    IF (CMEMS_OUTPUT_FLAG) THEN
      call check_status(nf90_def_var(NETCDF_FILE_ID,'processing_status', NF90_INT,    &
                                     TIME_DIM_ID, VARIABLE_IDS(total_int_parameters+4)))
      call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+4), &
                                     "standard_name", "status_flag"))
      call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+4), &
                                     "long_name", "Data processing status flag"))
      call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+4), &
                                     "flag_meanings", "consolidated intermediate"))
      call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+4), &
                                     "flag_values", (/0, 1/) ))
    END IF
    
    call check_status(nf90_put_att(NETCDF_FILE_ID,0,"source", "WAM Cycle 7.1"))
    call check_status(nf90_put_att(NETCDF_FILE_ID,0,"Conventions", "CF-1.6"))
    
    IF (CMEMS_OUTPUT_FLAG) THEN
      call check_status(nf90_put_att(NETCDF_FILE_ID,0,"institution", "Helmholtz-Zentrum Hereon, Germany"))
      call check_status(nf90_put_att(NETCDF_FILE_ID,0,"bulletin_type", "reanalysis"))
      call check_status(nf90_put_att(NETCDF_FILE_ID,0,"contact", "servicedesk.cmems@mercator-ocean.eu"))
      call check_status(nf90_put_att(NETCDF_FILE_ID,0,"references", & 
           "Please check in CMEMS catalogue the INFO section for product & 
           NWSHELF_MULTIYEAR_WAV_004_015 - http://marine.copernicus.eu"))
      call check_status(nf90_put_att(NETCDF_FILE_ID,0,"comment", & 
          "Please check in CMEMS catalogue the INFO section for product & 
          NWSHELF_MULTIYEAR_WAV_004_015 - http://marine.copernicus.eu"))
      call check_status(nf90_put_att(NETCDF_FILE_ID,0,"field_type", "hourly_instantaneous_at_time_field"))
      call check_status(nf90_put_att(NETCDF_FILE_ID,0,"title", "Wave Products (2D) - Instantaneous Field"))
      call check_status(nf90_put_att(NETCDF_FILE_ID,0,"license",  &
          "https://marine.copernicus.eu/user-corner/service-commitments-and-licence"))
    END IF 

    call check_status(nf90_put_att(NETCDF_FILE_ID,0,"title", "Wave Model Data (2D) - Instantaneous Field"))
    call check_status(nf90_put_att(NETCDF_FILE_ID,0,"history", " "))

    DO i=1,total_int_parameters
      
      IF (NFLAG_P(i)) THEN
        long_name_int_params = params%get_long_name(id = i)
        name_int_params = params%get_name_ip(id = i)
        scaling_factor_int_params = params%get_scaling_factor(id = i)
        direction_flag_int_params = params%get_direction_flag(id = i)
        units_int_params = params%get_units(id = i)
        std_name_int_params = params%get_standard_name(id = i)
        vl_min_int_params = params%get_vl_min(id=i)
        vl_max_int_params = params%get_vl_max(id=i)
        fill_value = params%get_fill_value(id=i)
        missing_value = params%get_missing_value(id=i)
        coordinates=params%get_Coordinates(id=i)

        WRITE(IU06,*) "Creating variable #",i
        call check_status(nf90_def_var(NETCDF_FILE_ID, TRIM(name_int_params), NF90_FLOAT, &
        &                               (/LON_DIM_ID, LAT_DIM_ID, TIME_DIM_ID/),  &
        &                                VARIABLE_IDS(i)))
        call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(i), "_FillValue", fill_value))
        call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(i), "standard_name", TRIM(std_name_int_params)))
        call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(i), "long_name", TRIM(long_name_int_params)))
        call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(i), "units", TRIM(units_int_params)))
        call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(i), "missing_value", missing_value))
        call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(i), "valid_min", vl_min_int_params))
        call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(i), "valid_max", vl_max_int_params))
        call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(i), "Coordinates", coordinates))
      END IF 
    END DO
    
    call check_status(nf90_enddef(NETCDF_FILE_ID))
    
    call check_status(nf90_put_var(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+1), longitude_grid)) 
    call check_status(nf90_put_var(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+2), latitude_grid)) 
     
    call check_status(nf90_close(NETCDF_FILE_ID))
    deallocate(longitude_grid, latitude_grid)
  end if

end subroutine

subroutine write_time_vector_to_netcdf_output_file(CDTPRO, time_step)
    character(len=*), intent(in) :: CDTPRO
    integer, intent(in) :: time_step
    real(kind=8) :: time_vector
    integer :: start(1)
    integer :: proc_stat
    time_vector = 0.0d0

    call time_conversion(time_vector, CDTPRO)

    call check_status(nf90_open(path = filepath_name, mode = ior(nf90_write, nf90_share), ncid = NETCDF_FILE_ID))
    call check_status(nf90_put_var(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+3), time_vector, start=(/time_step/)))
    IF (CMEMS_OUTPUT_FLAG) THEN
       proc_stat = 0
       call check_status(nf90_put_var(NETCDF_FILE_ID, VARIABLE_IDS(total_int_parameters+4), proc_stat, start=(/time_step/)))
    END IF
    call check_status(nf90_close(NETCDF_FILE_ID))

end subroutine write_time_vector_to_netcdf_output_file

subroutine write_variables_to_netcdf_output_file(id_int_params, grid_values, time_step)
  integer, intent(in) :: id_int_params
  real, dimension(NX, NY), intent(in) :: grid_values
  integer, intent(in) :: time_step

  real*8, dimension(NX, NY) :: local_grid_values
  character(len=100)   :: long_name_int_params
  character(len=100)   :: std_name_int_params
  character(len=60)   :: name_int_params
  character(len=15)   :: units_int_params
  real                :: scaling_factor_int_params, vl_min_int_params, vl_max_int_params
  real                :: fill_value, missing_value
  logical             :: direction_flag_int_params
  integer             :: NO_FILL, ix, iy
 
  local_grid_values = grid_values
  do iy=1,NY
    do ix=1,NX
      if (grid_values(ix,iy)==-9999999) local_grid_values(ix,iy) = -999
    enddo
  enddo

  if (irank == i_out_par) then
    
    call check_status(nf90_open(path = filepath_name, mode = ior(nf90_write, nf90_share), ncid = NETCDF_FILE_ID))
    call check_status(nf90_put_var(NETCDF_FILE_ID, VARIABLE_IDS(id_int_params), local_grid_values, &
    &    start=(/1,1,time_step/), count=(/NX,NY,1/)))
    call check_status(nf90_close(NETCDF_FILE_ID))
  
  end if

end subroutine write_variables_to_netcdf_output_file
end module wam_output_netcdf_module


















