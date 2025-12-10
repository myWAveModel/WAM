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
public :: create_netcdf_output_file, write_variables_to_netcdf_output_file, check_inputs_for_netcdf
integer, parameter :: total_int_parameters = 70
integer :: TIME_DIM_ID, LAT_DIM_ID, LON_DIM_ID, NETCDF_FILE_ID
integer :: VARIABLE_IDS(total_int_parameters+3) = -1


! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

contains

subroutine check_status(status)
  implicit none
  INTEGER, intent (in) :: status

    if (status /= NF90_NOERR) then
       write(IU06, *) "+++ Error : ", NF90_STRERROR(status), "+++  Aborting-ad !!"
       stop
    end if

end subroutine check_status

subroutine create_netcdf_output_file
  
  integer :: status

  NETCDF_FILE_ID = -1
  if (irank == 1) then
    status = nf90_create(path = '/work/ka1298/k202203/WAM_testing/int_parameters.nc',       & 
                       cmode = NF90_CLOBBER,                     &
                       ncid = NETCDF_FILE_ID)
    call check_status(status)                   
    call check_status(nf90_enddef(NETCDF_FILE_ID))
  end if
  
end subroutine

subroutine create_dimensions()
  
  if (irank == 1) then
    call check_status(nf90_open(path = "/work/ka1298/k202203/WAM_testing/int_parameters.nc", mode = nf90_write, ncid = NETCDF_FILE_ID))
    call check_status(nf90_redef(NETCDF_FILE_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID, 'time', NF90_UNLIMITED, TIME_DIM_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID, 'longitude', NX, LON_DIM_ID))
    call check_status(nf90_def_dim(NETCDF_FILE_ID, 'latitude', NY, LAT_DIM_ID))
    call check_status(nf90_def_var(NETCDF_FILE_ID,'lon', NF90_FLOAT, LON_DIM_ID, VARIABLE_IDS(total_int_parameters+1)))
    call check_status(nf90_def_var(NETCDF_FILE_ID,'lat', NF90_FLOAT, LAT_DIM_ID, VARIABLE_IDS(total_int_parameters+2)))
    call check_status(nf90_def_var(NETCDF_FILE_ID,'time', NF90_FLOAT, TIME_DIM_ID, VARIABLE_IDS(total_int_parameters+3)))
    call check_status(nf90_enddef(NETCDF_FILE_ID))
  end if

end subroutine

subroutine write_variables_to_netcdf_output_file(integrated_parameter, id_int_params)
  
  real, dimension(:), intent(in) :: integrated_parameter  
  integer, intent(in) :: id_int_params
  character(len=60) :: title_int_params
  
  title_int_params = params%get_title(id = id_int_params)
  call check_status(nf90_open(path = "home/k/k202203/WAM/output/int_parameters.nc", mode = nf90_write, ncid = NETCDF_FILE_ID))
  call check_status(nf90_redef(NETCDF_FILE_ID)) 
  call check_status(nf90_def_var(NETCDF_FILE_ID, title_int_params, NF90_FLOAT, &
  &                               (/LON_DIM_ID, LAT_DIM_ID, TIME_DIM_ID/),  &
  &                                VARIABLE_IDS(id_int_params)))
  call check_status(nf90_enddef(NETCDF_FILE_ID))
  
  call check_status(nf90_close(NETCDF_FILE_ID))
  
end subroutine write_variables_to_netcdf_output_file
end module wam_output_netcdf_module


















