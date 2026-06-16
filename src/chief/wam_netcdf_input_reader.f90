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
use wam_general_module, only: ABORT1
use wam_mpi_module, only: irank, petotal, i_out_par
use wam_file_module, only: IU06
use wam_grid_module, only: NX, NY, XDELLA, XDELLO, AMOWEP, AMOSOP
use wam_wind_module, only: SET_WIND_FIELD, SET_WIND_HEADER, SET_WIND_TIMESTEPS
use wam_timopt_module, only: time_conversion

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !


INTEGER, PARAMETER :: KIND_D = 8

INTEGER, SAVE         :: ICODE = 3  !! WIND CODE: 1= USTAR; 2= USTRESS; 3= U10
INTEGER, SAVE         :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER, SAVE         :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL,    ALLOCATABLE  :: U_MAP(:,:), u_buffer(:,:) !! 1. COMPONENT OF WIND MAP [M/S].
REAL,    ALLOCATABLE  :: V_MAP(:,:), v_buffer(:,:) !! 2. COMPONENT OF WIND MAP [M/S].
CHARACTER (LEN=14)    :: CDTWIR     !! DATE/TIME OF WIND FIELD

integer :: TIME_DIMID, LAT_DIMID, LON_DIMID, NETCDF_FILE_ID
real(kind=KIND_D), allocatable :: lon(:), lat(:)
real (kind=KIND_D) :: west, east, south, north, delta_lon, delta_lat
real (kind=KIND_D), allocatable :: time(:), new_time_array(:)
real (kind=KIND_D) :: time_units_reference, wind_read_time, start_date_seconds
integer :: ntime, status, wind_x_var_id, wind_y_var_id
integer, dimension(5) :: var_ids
character(len=16) :: file_path !! ../../input/wam/
character(len=16)   :: file_name !! WIND_YYYYMMDD.nc 
logical, save     :: first_time
logical           :: lat_descending
integer :: status_fileopen
integer :: i,j,k, input_frequency, input_frequency_sec, xday, xhour, xmin
character(len=8)  :: yyyymmdd
character(len=64) :: time_units_string
character(len=10) :: time_units
character(len=4)  :: time_units_year 
character(len=2)  :: time_units_month, time_units_day, time_units_hour, time_units_min, time_units_sec
character(len=14) :: time_units_cdate
character(len=10), dimension(4) :: wind_variable_names_x_component, wind_variable_names_y_component
integer :: ndims, nt, nlat_var, nlon_var
integer, dimension(3)::dimids

!! ----- CONTROL WIND READING -----!!!

! 1. Getting the start date of the run (Directly imported from WAM_UDER_MODULE)

! 2. Open the respective (daily) input file based on the suffix, e.g. *_y2025m01d25.nc
! 3. Reading the time vector of the nc file
! 4. Check if the first time step of the simulation is available (mandatory)

! 5. Calculate the time step of the nc files and check if they are constant, e.g. only 1-h or 3-h steps
! 6. Set/override the respective namelist values IDELWO + IDELWI with this time step (to be done only once)
! 7. Search for the lat/lon vectors based on 1. standard names and 2. variable names

! 8. Calculate: SOUTH,NORTH,WEST,EAST,D_LON,D_LAT,N_LON,N_LAT

! 9. Search for the input fields (e.g. wind) based on 1. standard names and 2. variable names
! 10. Read the wind fields and provide them to WAM, e.g. as U_MAP/V_MAP (similar to the read_wind_input examples)

! 11. Also provide the header information (see step 8.) + time information as CDTWIR (see step 5.)
! 12. Open the next file when a new day approaches (tbd if the checks should be done again)

!! ---------------------------------!!!




contains

subroutine check_status(status)
  implicit none
  INTEGER, intent (in) :: status
  if (status /= NF90_NOERR) then
    write(IU06, *) "+++ Error : ", NF90_STRERROR(status), "+++  Aborting!!"
    stop
  end if
end subroutine check_status

subroutine get_time_attributes(START_DATE)
  
  CHARACTER(len=14), INTENT(IN), optional :: START_DATE
  integer :: idx
  
  xmin = 60
  xhour = 3600
  xday = 86400

  ! Get time dimension id and size
  call check_status(nf90_inq_dimid(NETCDF_FILE_ID, "time", TIME_DIMID))
  call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, TIME_DIMID, len=ntime))
 
  allocate(time(ntime), new_time_array(ntime))
  
  ! Get time variable id and variable data
  call check_status(nf90_inq_varid(NETCDF_FILE_ID, "time", var_ids(3)))
  call check_status(nf90_get_var(NETCDF_FILE_ID, var_ids(3), time))
  WRITE(IU06,*) "Time dimension: " , ntime
  
  !Get time attributes - units 
  call check_status(nf90_get_att(NETCDF_FILE_ID, var_ids(3),"units", time_units_string)) 
  READ(time_units_string(1:10),*) time_units
  
  ! Get frequency of data available
  input_frequency = time(2) - time(1)
  WRITE(IU06,*) "delta t:" , input_frequency
 
  if(time_units == 'minutes'.OR. time_units == 'MINUTES') then
    input_frequency_sec = input_frequency * xmin
  else if(time_units == 'hours' .OR. time_units == 'HOURS') then
    input_frequency_sec = input_frequency * xhour
  else if(time_units == 'days'.OR. time_units == 'DAYS') then
    input_frequency_sec = input_frequency * xday
  end if
  input_frequency_sec = input_frequency
  WRITE(IU06,*) "input_frequency_sec: " , input_frequency_sec 
  
  ! Reading time units and converting to timestring from input file

  READ(time_units_string(INDEX(time_units_string,'-')-4:INDEX(time_units_string,'-')+5),'(A4,1X,A2,1X,A2)') time_units_year, time_units_month, time_units_day
  
  READ(time_units_string(INDEX(time_units_string,':')-2:INDEX(time_units_string,':')+5),'(A2,1X,A2,1X,A2)') time_units_hour, time_units_min, time_units_sec

  time_units_cdate(1:4) = time_units_year
  time_units_cdate(5:6) = time_units_month
  time_units_cdate(7:8) = time_units_day
  time_units_cdate(9:10) = time_units_hour
  time_units_cdate(11:12) = time_units_min
  time_units_cdate(13:14) = time_units_sec

  WRITE(IU06,*) "time_units_cdate:" , time_units_cdate 

  call time_conversion(time_units_reference, time_units_cdate)
  WRITE(IU06, *) "time_units_reference:" , time_units_reference

  DO i = 1, ntime
    new_time_array(i) = time(i) + time_units_reference
  END DO
  WRITE(IU06,*) "new_time_array:" , new_time_array
  
  if (first_time) then
    first_time =.false.
    CALL SET_WIND_TIMESTEPS(IN=input_frequency_sec, OUT=input_frequency_sec)

    idx = get_index_matching_timestamp(START_DATE)
    IF (idx == 0) THEN
      WRITE(IU06,*) "Starting timestamp does not exist in the first input file. Check input file"
      CALL ABORT1
    END IF
  end if 

  deallocate(time)

end subroutine

integer function get_index_matching_timestamp(CD_WIND_READ) result(idx)
  implicit none
  character(len=14) , intent(in) :: CD_WIND_READ
  integer :: i
  idx =0
  call time_conversion(wind_read_time, CD_WIND_READ)
  do i = 1, ntime
    if(wind_read_time == new_time_array(i)) then
      idx =i
      EXIT
    end if
  end do
end function

subroutine read_wind_fields(CD_WIND_READ, WIND_INPUT_FILE_IDENTIFIER)
  
  character(len=14) , intent(in) :: CD_WIND_READ
  character(LEN=10), intent(in) :: WIND_INPUT_FILE_IDENTIFIER

  integer, dimension(3) :: start, count
  integer :: idx
  WRITE(IU06,*) "entering read_wind_fields"
  idx = get_index_matching_timestamp(CD_WIND_READ)

  if (idx == 0) then
    call close_netcdf_file()
    call open_netcdf_file(CD_WIND_READ, WIND_INPUT_FILE_IDENTIFIER)
    call get_time_attributes()
    idx = get_index_matching_timestamp(CD_WIND_READ)   
  end if

  start = (/1,1,idx/)  
  count = (/N_LON, N_LAT,1/)
  
  write(iu06,*) "idx, start, count:" , idx, start, count

  allocate(U_MAP(N_LON,N_LAT), V_MAP(N_LON,N_LAT))
  allocate(u_buffer(N_LAT,N_LON), v_buffer(N_LAT,N_LON))
  wind_variable_names_x_component = (/"u10", "U10M", "var165", "U10"/)
  wind_variable_names_y_component = (/"v10", "V10M", "var166", "V10"/)
  
  
  DO k =1,4
    status = nf90_inq_varid(NETCDF_FILE_ID, wind_variable_names_x_component(k),wind_x_var_id)
    call check_status(nf90_inquire_variable(NETCDF_FILE_ID, wind_x_var_id, ndims=ndims, dimids=dimids))
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, dimids(1), len=nt))
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, dimids(2), len=nlat_var))
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, dimids(3), len=nlon_var))

    if (status == NF90_NOERR) then
      call check_status(nf90_get_var(NETCDF_FILE_ID, wind_x_var_id, U_MAP, start=start, count=count))
      WRITE(IU06,*) "Shape of wind var:", SHAPE(U_MAP)
      exit
    end if
  END DO

  DO j =1,4
    status = nf90_inq_varid(NETCDF_FILE_ID, wind_variable_names_y_component(j),wind_y_var_id)
    call check_status(nf90_inquire_variable(NETCDF_FILE_ID, wind_y_var_id, ndims=ndims, dimids=dimids))
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, dimids(1), len=nt))
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, dimids(2), len=nlat_var))
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, dimids(3), len=nlon_var))

    write(iu06,*) "wind_y_var_id: nt, nlat_var, nlon_var:" , nt, nlat_var, nlon_var

    if (status == NF90_NOERR) then
      WRITE(IU06,*) "wind_variable:" , wind_variable_names_y_component(j)
      call check_status(nf90_get_var(NETCDF_FILE_ID, wind_y_var_id, V_MAP, start=start, count=count))
      !WRITE(IU06,*) "wind_data (V_MAP):", V_MAP
      exit
    end if
  END DO

  if (lat_descending) then
    U_MAP = U_MAP(:, N_LAT:1:-1)
    V_MAP = V_MAP(:, N_LAT:1:-1)
  end if
  
  CALL SET_WIND_FIELD(CD_WIND_READ, U_MAP, V_MAP)
  deallocate(U_MAP, V_MAP, u_buffer, v_buffer)
end subroutine

subroutine read_wind_init(START_DATE, WIND_INPUT_FILE_IDENTIFIER)
    ! reads the first file 
    ! gets wind timestep
    character(len=14), intent(in) :: START_DATE
    character(len=10), intent(in) :: WIND_INPUT_FILE_IDENTIFIER
    first_time = .true.

    CALL open_netcdf_file(START_DATE, WIND_INPUT_FILE_IDENTIFIER)
    call get_time_attributes(START_DATE)
    call read_wind_header_data()
end subroutine

subroutine open_netcdf_file(WIND_CDATE, WIND_INPUT_FILE_IDENTIFIER )  
  
  character(len=14),intent(in) :: WIND_CDATE
  character(len=10), intent(in) :: WIND_INPUT_FILE_IDENTIFIER
  integer :: status_fileopen
  
  yyyymmdd = WIND_CDATE(1:8)
  file_name = TRIM(WIND_INPUT_FILE_IDENTIFIER)//'_'//yyyymmdd//'.nc'
  write(IU06,*) "File name created:" , file_name

  status_fileopen = nf90_open(path=file_name, mode=NF90_NOWRITE, ncid=NETCDF_FILE_ID)
  if (status_fileopen /= 0) then 
    WRITE(IU06,*) "File not available / path wrong"
    CALL ABORT1
  end if 

end subroutine

subroutine close_netcdf_file()

    call check_status(nf90_close(NETCDF_FILE_ID))

end subroutine


subroutine read_wind_header_data()
    
    ! Get dimension ids
    call check_status(nf90_inq_dimid(NETCDF_FILE_ID, "lon", LON_DIMID))
    call check_status(nf90_inq_dimid(NETCDF_FILE_ID, "lat", LAT_DIMID))

    ! Get dimension sizes
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, LON_DIMID, len=N_LON))
    call check_status(nf90_inquire_dimension(NETCDF_FILE_ID, LAT_DIMID, len=N_LAT))

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
    
    west = lon(1)
    east = lon(N_LON)
    delta_lon = lon(2) - lon(1)
    
    lat_descending = (lat(2) < lat(1)) 
    
    if (lat_descending) then
      south = lat(N_LAT)
      north = lat(1)
      delta_lat = lat(1) - lat(2)
    else
      south = lat(1)
      north = lat(N_LAT)
      delta_lat = lat(2) - lat(1)
    end if
    
    WRITE(IU06,*) "west, south, east, north: ", west, south, east, north 
    WRITE(IU06,*) "dlon, dlat, nlon, nlat: " , delta_lon, delta_lat, N_LON, N_LAT
    WRITE(IU06,*) 'types check: ', KIND(west), KIND(south), KIND(east), KIND(north), KIND(delta_lon), KIND(delta_lat)
    
    call SET_WIND_HEADER(WEST=west, SOUTH=south, EAST = east, NORTH=north, D_LON=delta_lon, D_LAT=delta_lat, N_LON=N_LON, N_LAT=N_LAT)

    !call check_status(nf90_close(NETCDF_FILE_ID))
    deallocate(lon,lat)
    !end if 

end subroutine

end module

