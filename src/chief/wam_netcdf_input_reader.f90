!!!! READ FOR WIND, SSH, CURRENT, ICE FROM EXISTING NETCDF FILES
module wam_netcdf_input_reader

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
use wam_grid_module, only: NX, NY, XDELLA, XDELLO, AMOWEP, AMOSOP
use wam_wind_module, only: SET_WIND_FIELD, SET_WIND_HEADER


IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !


INTEGER, PARAMETER :: KIND_D = 8

INTEGER, SAVE         :: ICODE = 3  !! WIND CODE: 1= USTAR; 2= USTRESS; 3= U10
INTEGER, SAVE         :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER, SAVE         :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL (KIND=KIND_D)    :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: WEST       !! WEST LONGITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: EAST       !! EAST LONGITUDE OF GRID [DEG].
REAL,    ALLOCATABLE  :: U_MAP(:,:) !! 1. COMPONENT OF WIND MAP [M/S].
REAL,    ALLOCATABLE  :: V_MAP(:,:) !! 2. COMPONENT OF WIND MAP [M/S].
CHARACTER (LEN=14)    :: CDTWIR     !! DATE/TIME OF WIND FIELD

integer :: TIME_DIMID, LAT_DIMID, LON_DIMID, NETCDF_FILE_ID
real, allocatable :: lon(:), lat(:)
integer :: ntime
integer, dimension(5) :: var_ids
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


subroutine read_wind_header_data()
    
    if (irank == 1) then
    call check_status(nf90_open(path="wind_data.nc", mode=NF90_NOWRITE, ncid=NETCDF_FILE_ID))
    WRITE(IU06,*) "netcdf file id: ", NETCDF_FILE_ID
   
    ! Get dimension ids
    call check_status(nf90_inq_dimid(NETCDF_FILE_ID, "lon", LON_DIMID))
    call check_status(nf90_inq_dimid(NETCDF_FILE_ID, "lat", LAT_DIMID))
    !call check_status(nf90_inq_dimid(NETCDF_FILE_ID, "time", TIME_DIMID))

    ! Get dimension sizes
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, LON_DIMID, len=N_LON))
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, LAT_DIMID, len=N_LAT))
    !call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, TIME_DIMID, len=ntime))

    WRITE(IU06,*) "dimension sizes lon/lat/time: ", N_LON, N_LAT, ntime
    
    allocate(lon(N_LON), lat(N_LAT))
    
    ! Get variable ids
    call check_status(nf90_inq_varid(NETCDF_FILE_ID, "lon", var_ids(1)))
    call check_status(nf90_inq_varid(NETCDF_FILE_ID, "lat", var_ids(2)))
    call check_status(nf90_inq_varid(NETCDF_FILE_ID, "time", var_ids(3)))

    call check_status(nf90_get_var(NETCDF_FILE_ID, var_ids(1), lon))
    call check_status(nf90_get_var(NETCDF_FILE_ID, var_ids(2), lat))

    WRITE(IU06,*) "Longitude:", lon
    WRITE(IU06,*) "Latitude:", lat

    
    
    
    call check_status(nf90_close(NETCDF_FILE_ID))
    deallocate(lon,lat)
    end if 

end subroutine

end module

