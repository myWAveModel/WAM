SUBROUTINE READ_TOPO_INPUT

! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_TOPO_INPUT - ROUTINE TO READ TOPO FIELDS.                             !
!                                                                              !
!     KATHRIN WAHLE     HZG        MARCH 2011                                  !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO READ AN ELEVATION FIELD FROM GETM NETCDF OUTPUT                     !
!       AND TRANSFER IT TO THE WAM_TOPO_MODULE.                                !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!        FORMATTED READ FROM UNIT IU08, FILE08.                                !
!                                                                              !
!       FILE08, WHICH IS DEFINED IN THE USER INPUT, IS ASSIGNED TO IU08.       !
!       THE FILE MUST BE A GETM NETCDF FILE                                    !
!                                                                              !
!       THE FOLLOWING INFORMATION HAS TO BE TRANSFERRED BY                     !
!       SUB. SET_TOPO_HEADER TO THE WAM_TOPO_MODULE:                           !
!          INTEGER :: N_LON      !! NUMBER OF LONGITUDES IN GRID.              !
!          INTEGER :: N_LAT      !! NUMBER OF LATITUDES IN GRID.               !
!          REAL*8  :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].          !
!          REAL*8  :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].         !
!          REAL*8  :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].              !
!          REAL*8  :: NORTH      !! NORTH LATITUDE OF GRID [DEG].              !
!          REAL*8  :: WEST       !! WEST LONGITUDE OF GRID [DEG].              !
!          REAL*8  :: EAST       !! EAST LONGITUDE OF GRID [DEG].              !
!          INTEGER :: CODE       !! TOPO CODE: 1 = TOTAL WATER DEPTH           !
!                                   OTHERWISE ELEVATION OVER NN.               !
!                                                                              !
!       THE FOLLOWING INFORMATION HAS TO BE TRANSFERRED BY                     !
!       SUB. SET_TOPO_FIELD TO THE WAM_TOPO_MODULE:                            !
!          CHARACTER (LEN=14) :: CDTTOR     !! DATE/TIME OF TOPO FIELD.        !
!          REAL               :: D_MAP(:,:) !! TOPO MAP [M].                   !
!                                                                              !
!       THE TOPO DATA MUST BE ON A REGULAR LATITUDE-LONGITUDE GRID ARRANGED    !
!       FROM  WEST TO EAST AND FROM SOUTH TO NORTH, WHICH IS:                  !
!       IN THE ARRAY "D_MAP(I,K)" THE CORNER POINTS ARE:                       !
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

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  & !! TERMINATES PROCESSING.
&       incdate                   !! calculate new date

USE WAM_TOPO_MODULE,       ONLY: &
&       SET_TOPO_HEADER,         & !! SETS TOPO HEADER
&       SET_TOPO_FIELD             !! SETS TOPO FIELD

! ---------------------------------------------------------------------------- !
!                                                                              !
!     MODULE VARIABLES.                                                        !
!     -----------------                                                        !

USE WAM_FILE_MODULE, ONLY: IU06, ITEST, IU08, FILE08
use wam_topo_module,    only: idelti



IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER, PARAMETER :: KIND_D = 8

INTEGER               :: CODE       !! TOPO CODE: 1 = TOTAL WATER DEPTH,
                                    !!      OTHERWISE ELEVATION OVER NN.
INTEGER, SAVE         :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER, SAVE         :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL (KIND=KIND_D)    :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: WEST       !! WEST LONGITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: EAST       !! EAST LONGITUDE OF GRID [DEG].
REAL,    ALLOCATABLE  :: D_MAP(:,:) !! TOPOGRAPHY DATA [M].
CHARACTER (LEN=14)    :: CDTTOR     !! DATE/TIME OF TOPO FIELD

LOGICAL, SAVE  :: FRSTIME = .TRUE.
INTEGER        :: L, IOS, J
character (len=14), save :: chelp
character (len=14) :: hcdttor

CHARACTER (LEN=80)  :: name_var, name_dim
INTEGER   :: nDimensions, nVariables, nAttributes,unlimitedDimId
INTEGER   :: var_id_elev, var_id_lonc, var_id_latc
INTEGER   :: xtype, ndims_var, nAtts
INTEGER, DIMENSION(3)   :: dimids_var, countA, startA
INTEGER  :: lat_dim_id, lon_dim_id, elev_dim_id
INTEGER  :: err
REAL, ALLOCATABLE     :: LONC(:), LATC(:)
INTEGER ::  YEAR, MONTH, DAY, HOUR, MINUTE, SECOND

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. FOR FIRST CALL: OPEN FILE AND READ GRID  .                            !
!        ------------------------------------------                            !

if (frstime) then
   chelp = '00000000000000'
   hcdttor = '00000000000000'
   chelp(1:8) = file08(8:15)
else
   call incdate (chelp,idelti)
   file08(8:15) = chelp(1:8)
endif

!if hour==0 file of previous day has to opened and
!hour has to be set to 24
READ (chelp,'(I4,5I2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
hcdttor = chelp
CDTTOR = hcdttor

!WRITE (IU06,*) ' ****************************************************'
!WRITE (IU06,*) ' *    CDTTOR = ', CDTTOR
!WRITE (IU06,*) ' ****************************************************'
!WRITE (IU06,*) ' *    HOUR = ', HOUR

IF (HOUR.EQ.0) THEN
	call incdate (hcdttor, -24*3600)
	!WRITE (IU06,*) ' *    hcdttor = ', hcdttor
        file08(8:15) = hcdttor(1:8)
        HOUR = 24
END IF


L = LEN_TRIM(FILE08)
err =  NF90_OPEN(FILE08(1:L), NF90_CLOBBER, IU08)
IF (err.NE.0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *       FATAL ERROR IN SUB. READ_TOPO_INPUT        *'
   WRITE (IU06,*) ' *       ===================================        *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * TOPO INPUT FILE COULD NOT BE OPENED              *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
   WRITE (IU06,*) ' *    FILE NAME IS  FILE08 = ', TRIM(FILE08)
   WRITE (IU06,*) ' *    UNIT IS         IU08 = ', IU08
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF
!   WRITE (IU06,*) ' ****************************************************'
!   WRITE (IU06,*) ' *    FILE NAME IS  FILE08 = ', TRIM(FILE08)
!   WRITE (IU06,*) ' ****************************************************'


IF (FRSTIME) THEN
   err = nf90_inquire(IU08, nDimensions, nVariables, nAttributes, unlimitedDimId)
   err=  nf90_inq_dimid(IU08,"latc" , lat_dim_id)
   err=  nf90_inq_dimid(IU08,"lonc" , lon_dim_id)
   err = nf90_inquire_dimension(IU08, lat_dim_id, name_dim, N_LAT)
   err = nf90_inquire_dimension(IU08, lon_dim_id, name_dim, N_LON)

   err = nf90_inq_varid(IU08,"lonc", var_id_lonc)
   err = nf90_inquire_variable(IU08, var_id_lonc, name_var, xtype, ndims_var, dimids_var, nAtts)
   ALLOCATE(LONC(N_LON))
   err = nf90_get_var(IU08, var_id_lonc, LONC)
   err = nf90_inq_varid(IU08,"latc", var_id_latc)
   err = nf90_inquire_variable(IU08, var_id_latc, name_var, xtype, ndims_var, dimids_var, nAtts)
   ALLOCATE(LATC(N_LAT))
   err = nf90_get_var(IU08, var_id_latc, LATC)

!       THE DEPTH ARRAY "D_MAP(I,K)" MUST BE ORDERED AS                        !
!                 (    1,    1 ) <==> SOUTH WEST                               !
!                 (N_LON,    1 ) <==> SOUTH EAST                               !
!                 (    1, N_LAT) <==> NORTH WEST                               !
!                 (N_LON, N_LAT) <==> NORTH EAST
   WEST = LONC(1)
   EAST = LONC(N_LON)
   SOUTH = LATC(1)
   NORTH = LATC(N_LAT)
   D_LON = (EAST-WEST)/REAL(N_LON-1)
   D_LAT = (NORTH-SOUTH)/REAL(N_LAT-1)

   CALL SET_TOPO_HEADER (WEST=WEST,   SOUTH=SOUTH,    &
&                        EAST=EAST,   NORTH=NORTH,    &
&                        D_LON=D_LON, D_LAT=D_LAT,    &
&                        N_LON=N_LON, N_LAT=N_LAT,    &
&                        CODE=0)
   FRSTIME = .FALSE.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. ALLOCATE TOPO INPUT ARRAYS.                                           !
!        ---------------------------                                           !

IF (.NOT.ALLOCATED(D_MAP) ) ALLOCATE(D_MAP(N_LON,N_LAT))

! ---------------------------------------------------------------------------- !
!                                                                              !
!    3. READ TOPO FIELD.                                                       !
!       -------------------                                                    !

startA    = 1
startA(3) = HOUR
countA(1) = N_LON
countA(2) = N_LAT
countA(3) = 1

!WRITE(IU06,*) '     HOUR = ',HOUR

err = nf90_inq_varid(IU08,"elev", var_id_elev)
err = nf90_inquire_variable(IU08, var_id_elev, name_var, xtype, ndims_var, dimids_var, nAtts)
err = nf90_get_var(IU08, var_id_elev, D_MAP, startA, countA)

err =  nf90_close(IU08)

WHERE (D_MAP < -100.0) D_MAP = 0.0

CALL SET_TOPO_FIELD (CDTTOR, D_MAP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!    4. WRITE TEST OUTPUT AND DEALLOCATE ARRAYS.                               !
!       ----------------------------------------                               !

IF (ITEST.GT.1) THEN
   WRITE(IU06,*) ' '
   WRITE(IU06,*) '     SUB. READ_TOPO_INPUT -  TOPO FIELD FOR CDTTOR = ',CDTTOR
   WRITE(IU06,'(5X,20F6.0)') D_MAP(1:MIN(20,N_LON),1:MIN(5,N_LAT))
END IF

DEALLOCATE(D_MAP)

END SUBROUTINE READ_TOPO_INPUT
