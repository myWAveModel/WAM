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
use wam_grid_module, only: NX, NY
use wam_output_parameter_module, only: params

IMPLICIT NONE
public :: create_netcdf_output_file
public :: write_variables_to_netcdf_output_file
integer, parameter :: total_int_parameters = 70
integer :: TIME_DIM_ID, LAT_DIM_ID, LON_DIM_ID, NETCDF_FILE_ID
integer :: VARIABLE_IDS(total_int_parameters+3) = -1
character(len=:), allocatable :: filepath_name


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

subroutine create_dimensions()
  
  if (irank == i_out_par) then
    call check_status(nf90_open(path = filepath_name, mode = ior(nf90_write,nf90_share), ncid =NETCDF_FILE_ID))
    call check_status(nf90_redef(NETCDF_FILE_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID,'time', NF90_UNLIMITED, TIME_DIM_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID,'longitude', NX, LON_DIM_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID,'latitude', NY, LAT_DIM_ID))
    call check_status(nf90_def_var(NETCDF_FILE_ID,'lon', NF90_FLOAT, LON_DIM_ID, VARIABLE_IDS(total_int_parameters+1)))
    call check_status(nf90_def_var(NETCDF_FILE_ID,'lat', NF90_FLOAT, LAT_DIM_ID, VARIABLE_IDS(total_int_parameters+2)))
    call check_status(nf90_def_var(NETCDF_FILE_ID,'time', NF90_FLOAT, TIME_DIM_ID, VARIABLE_IDS(total_int_parameters+3)))
    call check_status(nf90_enddef(NETCDF_FILE_ID))
    call check_status(nf90_close(NETCDF_FILE_ID))

  end if

end subroutine

subroutine write_variables_to_netcdf_output_file(id_int_params, grid_values)
  integer, intent(in) :: id_int_params
  real, dimension(NX, NY), intent(in) :: grid_values

  character(len=100)   :: long_name_int_params
  character(len=100)   :: std_name_int_params
  character(len=60)   :: name_int_params
  character(len=15)   :: units_int_params
  real                :: scaling_factor_int_params, vl_min_int_params, vl_max_int_params
  logical             :: direction_flag_int_params
  integer             :: NO_FILL

   
  long_name_int_params = params%get_long_name(id = id_int_params)
  name_int_params = params%get_name_ip(id = id_int_params)
  scaling_factor_int_params = params%get_scaling_factor(id = id_int_params)
  direction_flag_int_params = params%get_direction_flag(id = id_int_params)
  units_int_params = params%get_units(id = id_int_params)
  std_name_int_params = params%get_standard_name(id = id_int_params)
  vl_min_int_params = params%get_vl_min(id=id_int_params)
  vl_max_int_params = params%get_vl_max(id=id_int_params)

  if (irank == i_out_par) then
    
    call check_status(nf90_open(path = filepath_name, mode = ior(nf90_write, nf90_share), ncid = NETCDF_FILE_ID))
    call check_status(nf90_redef(NETCDF_FILE_ID)) 
    
    call check_status(nf90_def_var(NETCDF_FILE_ID, TRIM(name_int_params), NF90_FLOAT, &
    &                               (/LON_DIM_ID, LAT_DIM_ID, TIME_DIM_ID/),  &
    &                                VARIABLE_IDS(id_int_params)))
    
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(id_int_params), "standard_name", TRIM(std_name_int_params)))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(id_int_params), "long_name", TRIM(long_name_int_params)))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(id_int_params), "scaling_factor", scaling_factor_int_params))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(id_int_params), "units", TRIM(units_int_params)))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(id_int_params), "variable_min", vl_min_int_params))
    call check_status(nf90_put_att(NETCDF_FILE_ID, VARIABLE_IDS(id_int_params), "variable_max", vl_max_int_params))
    call check_status(nf90_enddef(NETCDF_FILE_ID))  
    call check_status(nf90_put_var(NETCDF_FILE_ID, VARIABLE_IDS(id_int_params), grid_values))
    call check_status(nf90_close(NETCDF_FILE_ID))
  
  end if

end subroutine write_variables_to_netcdf_output_file
end module wam_output_netcdf_module


















