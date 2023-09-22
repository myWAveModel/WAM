SUBROUTINE READ_WIND_INPUT

! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_WIND_INPUT - ROUTINE TO READ WINDFIELDS.                              !
!                                                                              !
!     HEINZ GUNTHER    GKSS    JANUARY 2001                                    !
!     ERIK MYKLEBUST           NOVEMBER 2004                                   !
!     Arno Behrens     GKSS    February 2007  (DWD wind fields)                !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO READ A WIND FIELD AND TRANSFER IT TO THE WAM_WIND_MODULE.           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!        FORMATTED READ FROM UNIT IU01, FILE01.                                !
!                                                                              !
!       FILE01, WHICH IS DEFINED IN THE USER INPUT, IS ASSIGNED TO IU01.       !
!       THE FILE MUST STORE:                                                   !
!           1. RECORD: THE WIND DATA HEADER.                                   !
!           2. RECORD: THE WIND DATA TIME.                                     !
!           FOLLOWING RECORDS: THE WIND DATA MATRIX.                           !
!           (RECORD 2 AND FOLLOWING RECORDS ARE REPEATED FOR NEXT WIND FIELD.) !
!                                                                              !
!       THE FOLLOWING INFORMATION HAS TO BE TRANSFERRED BY                     !
!       SUB. SET_WIND_HEADER TO THE WAM_WIND_MODULE:                           !
!          INTEGER :: N_LON      !! NUMBER OF LONGITUDES IN GRID.              !
!          INTEGER :: N_LAT      !! NUMBER OF LATITUDES IN GRID.               !
!          REAL    :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].          !
!          REAL    :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].         !
!          REAL    :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].              !
!          REAL    :: NORTH      !! NORTH LATITUDE OF GRID [DEG].              !
!          REAL    :: WEST       !! WEST LONGITUDE OF GRID [DEG].              !
!          REAL    :: EAST       !! EAST LONGITUDE OF GRID [DEG].              !
!          INTEGER :: ICODE      !! WIND CODE: 1= USTAR; 2= USTRESS; 3= U10    !
!                                                                              !
!       THE FOLLOWING INFORMATION HAS TO BE TRANSFERRED BY                     !
!       SUB. SET_WIND_FIELD TO THE WAM_WIND_MODULE:                            !
!          CHARACTER (LEN=14) :: CDTWIR     !! DATE/TIME OF WIND FIELD.        !
!          REAL               :: U_MAP(:,:) !! U COMPONENT OF WIND MAP [M/S].  !
!          REAL               :: V_MAP(:,:) !! V COMPONENT OF WIND MAP [M/S].  !
!                                                                              !
!       THE WINDS MUST BE ON A REGULAR LATITUDE-LONGITUDE GRID ARRANGED        !
!       FROM  WEST TO EAST AND FROM SOUTH TO NORTH, WHICH IS:                  !
!       IN THE ARRAYS "U_MAP(I,K)" AND  "V_MAP(I,K)" THE CORNER POINTS ARE:    !
!                 (    1,    1 ) <==> SOUTH WEST                               !
!                 (N_LON,    1 ) <==> SOUTH EAST                               !
!                 (    1, N_LAT) <==> NORTH WEST                               !
!                 (N_LON, N_LAT) <==> NORTH EAST                               !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
USE netcdf

USE WAM_GENERAL_MODULE,    ONLY: &
&       ABORT1,                  & !! TERMINATES PROCESSING.
&       incdate,                 & !! calculate new date
&       difdate                    !! difference between two dates in seconds

USE WAM_WIND_MODULE,       ONLY: &
&       SET_WIND_HEADER,         & !! SETS WIND HEADER
&       SET_WIND_FIELD,          & !! SETS WIND FIELD
&       PRINT_WIND_STATUS          !! PRINTS WIND MODULE STATUS

use wam_special_module,    only: &
&       chready                    !! wait for wind/ice files

! ---------------------------------------------------------------------------- !
!                                                                              !
!     MODULE VARIABLES.                                                        !
!     -----------------                                                        !

USE WAM_FILE_MODULE,    ONLY: IU06, ITEST, IU01, FILE01
use wam_timopt_module,  only: ifcst, cdatea, cda
use wam_wind_module,    only: idelwi
use wam_special_module, only: readyf

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER, PARAMETER    :: KIND_D = 8
INTEGER, SAVE         :: ICODE = 3  !! WIND CODE: 1= USTAR; 2= USTRESS; 3= U10
INTEGER, SAVE         :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER, SAVE         :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL (KIND=KIND_D)    :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: WEST	    !! WEST LONGITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: EAST	    !! EAST LONGITUDE OF GRID [DEG].
REAL,    ALLOCATABLE  :: U_MAP(:,:) !! 1. COMPONENT OF WIND MAP [M/S].
REAL,    ALLOCATABLE  :: V_MAP(:,:) !! 2. COMPONENT OF WIND MAP [M/S].
CHARACTER (LEN=14)    :: CDTWIR     !! DATE/TIME OF WIND FIELD

LOGICAL, SAVE  :: FRSTIME = .TRUE.
INTEGER        :: LEN, IOS, J

character (len=14), save :: chelp
character (len=14) :: hcdttor

CHARACTER (LEN=80)  :: name_var, name_dim
INTEGER   :: nDimensions, nVariables, nAttributes,unlimitedDimId
INTEGER   :: var_id_u, var_id_v, var_id_lon, var_id_lat
INTEGER   :: xtype, ndims_var, nAtts
INTEGER, DIMENSION(3)   :: dimids_var, countA, startA
INTEGER  :: lat_dim_id, lon_dim_id, u_dim_id, v_dim_id
INTEGER  :: err, L
REAL, ALLOCATABLE     :: LON(:), LAT(:)
INTEGER ::  YEAR, MONTH, DAY, HOUR, MINUTE, SECOND

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. FOR FIRST CALL: OPEN FILE AND READ HEADER.                            !
!        ------------------------------------------                            !
if (frstime) then
   chelp = '00000000000000'
   hcdttor = '00000000000000'
   chelp(1:4) = file01(11:14)
   chelp(5:6) = file01(16:17)
   chelp(7:10) = '0100'
else
   call incdate (chelp,idelwi)
   file01(11:14) = chelp(1:4)
   file01(16:17) = chelp(5:6)
endif

 WRITE (IU06,*) chelp
READ (chelp,'(I4,5I2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
hcdttor = chelp


WRITE (IU06,*) ' *    HOUR = ', HOUR
WRITE (IU06,*) ' *    chelp= ', chelp
WRITE (IU06,*) ' *    file = ', file01

L = LEN_TRIM(file01)
err =  NF90_OPEN(file01(1:L), NF90_CLOBBER, iu01)
IF (err.NE.0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *       FATAL ERROR IN SUB. READ_WIND_INPUT        *'
   WRITE (IU06,*) ' *       ===================================        *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * WIND INPUT FILE COULD NOT BE OPENED              *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
   WRITE (IU06,*) ' *    FILE NAME IS  FILE01 = ', TRIM(file01)
   WRITE (IU06,*) ' *    UNIT IS         IU01 = ', iu01
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF
!   WRITE (IU06,*) ' ****************************************************'
!   WRITE (IU06,*) ' *    FILE NAME IS  file01 = ', TRIM(file01)
!   WRITE (IU06,*) ' ****************************************************'


IF (FRSTIME) THEN
   err = nf90_inquire(iu01, nDimensions, nVariables, nAttributes, unlimitedDimId)
   err=  nf90_inq_dimid(iu01,"lat" , lat_dim_id)
   err=  nf90_inq_dimid(iu01,"lon" , lon_dim_id)
   err = nf90_inquire_dimension(iu01, lat_dim_id, name_dim, N_LAT)
   err = nf90_inquire_dimension(iu01, lon_dim_id, name_dim, N_LON)

   err = nf90_inq_varid(iu01,"lon", var_id_lon)
   err = nf90_inquire_variable(iu01, var_id_lon, name_var, xtype, ndims_var, dimids_var, nAtts)
   ALLOCATE(LON(N_LON))
   err = nf90_get_var(iu01, var_id_lon, LON)
   err = nf90_inq_varid(iu01,"lat", var_id_lat)
   err = nf90_inquire_variable(iu01, var_id_lat, name_var, xtype, ndims_var, dimids_var, nAtts)
   ALLOCATE(LAT(N_LAT))
   err = nf90_get_var(iu01, var_id_lat, LAT)

   WEST = LON(1)
   EAST = LON(N_LON)
   SOUTH = LAT(1)
   NORTH = LAT(N_LAT)
   D_LON = (EAST-WEST)/REAL(N_LON-1)
   D_LAT = (NORTH-SOUTH)/REAL(N_LAT-1)

   CALL SET_WIND_HEADER (WEST=WEST,   SOUTH=SOUTH,    &
&                        EAST=EAST,   NORTH=NORTH,    &
&                        D_LON=D_LON, D_LAT=D_LAT,    &
&                        N_LON=N_LON, N_LAT=N_LAT,    &
&                        CODE=ICODE)
   IF (ITEST.GT.0) CALL PRINT_WIND_STATUS
   FRSTIME = .FALSE.
END IF


! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. ALLOCATE WIND INPUT ARRAYS.                                           !
!        ---------------------------                                           !

IF (.NOT.ALLOCATED(U_MAP) ) ALLOCATE(U_MAP(N_LON,N_LAT))
IF (.NOT.ALLOCATED(V_MAP) ) ALLOCATE(V_MAP(N_LON,N_LAT))

! ---------------------------------------------------------------------------- !
!                                                                              !
!    3. READ WIND FIELD.                                                       !
!       -------------------
                                                  !
!call incdate (hcdttor, 24*3600)
 CDTWIR=hcdttor
!ecmwf Winde alle 6 Stunden:
startA    = 1
startA(3) = (DAY-1)*4 + (HOUR/6)+1
countA(1) = N_LON
countA(2) = N_LAT
countA(3) = 1

WRITE(IU06,*) '     HOUR = ',HOUR
WRITE(IU06,*) '     readField = ',startA(3)

err = nf90_inq_varid(IU01,"u10", var_id_u)
err = nf90_inquire_variable(IU01, var_id_u, name_var, xtype, ndims_var, dimids_var, nAtts)
err = nf90_get_var(IU01, var_id_u, U_MAP, startA, countA)
err = nf90_inq_varid(IU01,"v10", var_id_v)
err = nf90_inquire_variable(IU01, var_id_v, name_var, xtype, ndims_var, dimids_var, nAtts)
err = nf90_get_var(IU01, var_id_v, V_MAP, startA, countA)

err =  nf90_close(IU01)


 CALL SET_WIND_FIELD (CDTWIR, U_MAP, V_MAP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!    4. WRITE TEST OUTPUT AND DEALLOCATE ARRAYS.                               !
!       ----------------------------------------                               !

IF (ITEST.GT.1) THEN
   WRITE(IU06,*) ' READ_WIND_INPUT -  WIND FIELD FOR THE CDTWIR = ', CDTWIR
   WRITE(IU06,'(1X,24F5.2)') U_MAP(1:MIN(24,N_LON),1:MIN(5,N_LAT))
   WRITE(IU06,*) ' '
   WRITE(IU06,'(1X,24F5.2)') V_MAP(1:MIN(24,N_LON),1:MIN(5,N_LAT))
END IF

DEALLOCATE(U_MAP)
DEALLOCATE(V_MAP)

RETURN

END SUBROUTINE READ_WIND_INPUT
