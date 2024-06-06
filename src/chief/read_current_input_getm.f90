SUBROUTINE READ_CURRENT_INPUT

! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_CURRENT_INPUT - READ CURRENT FIELDS.                                  !
!                                                                              !
!     KATHRIN WAHLE   HZG     MARCH 2011                                       !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO READ A CURRENT FIELD FROM GETM NETCDF FILE                          !
!           AND TRANSFER IT TO THE WAM_CURRENT_MODULE.                         !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       FORMATTED READ FROM UNIT IU09, FILE09.                                 !
!                                                                              !
!       FILE09, WHICH IS DEFINED IN THE USER INPUT, IS ASSIGNED TO IU09.       !
!       THE FILE MUST BE A GETM NETCDF FILE                                    !
!                                                                              !
!       THE FOLLOWING INFORMATION HAS TO BE TRANSFERRED BY                     !
!       SUB. SET_CURRENT_HEADER TO THE WAM_CURRENT_MODULE:                     !
!          INTEGER :: N_LON      !! NUMBER OF LONGITUDES IN GRID.              !
!          INTEGER :: N_LAT      !! NUMBER OF LATITUDES IN GRID.               !
!          REAL*8  :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].          !
!          REAL*8  :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].         !
!          REAL*8  :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].              !
!          REAL*8  :: NORTH      !! NORTH LATITUDE OF GRID [DEG].              !
!          REAL*8  :: WEST       !! WEST LONGITUDE OF GRID [DEG].              !
!          REAL*8  :: EAST       !! EAST LONGITUDE OF GRID [DEG].              !
!          INTEGER :: CODE       !! CURRENT CODE:                              !
!                                !! 1 = SPEED (U-MAP) AND DIRECTION (V_MAP),   !
!                                !! OTHERWISE: COMPONENTS                      !
!                                                                              !
!       THE FOLLOWING INFORMATION HAS TO BE TRANSFERRED BY                     !
!       SUB. SET_CURRENT_FIELD TO THE WAM_CURRENT_MODULE:                      !
!          CHARACTER (LEN=14) :: CDTCUR     !! DATE/TIME OF CURRENT FIELD.     !
!          REAL               :: U_MAP(:,:) !! U COMPONENT OF CURRENT MAP [M/S]!
!          REAL               :: V_MAP(:,:) !! V COMPONENT OF CURRENT MAP [M/S]!
!                                                                              !
!       THE CURRENTS MUST BE ON A REGULAR LATITUDE-LONGITUDE GRID ARRANGED     !
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

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  & !! TERMINATES PROCESSING.
&       incdate                    !! calculate new date

USE WAM_CURRENT_MODULE,   ONLY:  &
&       SET_CURRENT_HEADER,         & !! SETS CURRENT HEADER
&       SET_CURRENT_FIELD             !! SETS CURRENT FIELD

! ---------------------------------------------------------------------------- !
!                                                                              !
!     MODULE VARIABLES.                                                        !
!     -----------------                                                        !

USE WAM_FILE_MODULE,  ONLY:  IU06, ITEST, IU09, FILE09

use wam_current_module,    only: idelci

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER, PARAMETER :: KIND_D = 8

INTEGER               :: CODE       !! CURRENT CODE:
                                    !! 1 = SPEED (U-MAP) AND DIRECTION (V_MAP),
                                    !! OTHERWISE: COMPONENTS
INTEGER, SAVE         :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER, SAVE         :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL (KIND=KIND_D)    :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: WEST       !! WEST LONGITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: EAST       !! EAST LONGITUDE OF GRID [DEG].
REAL,    ALLOCATABLE  :: U_MAP(:,:) !! 1. COMPONENT OF CURRENT MAP [M/S].
REAL,    ALLOCATABLE  :: V_MAP(:,:) !! 2. COMPONENT OF CURRENT MAP [M/S].
CHARACTER (LEN=14)    :: CDTCUR     !! DATE/TIME OF CURRENT FIELD

LOGICAL, SAVE  :: FRSTIME = .TRUE.
INTEGER        :: L, IOS, J
character (len=14), save :: chelp
character (len=14) :: hcdtcur

CHARACTER (LEN=80)  :: name_var, name_dim
INTEGER   :: nDimensions, nVariables, nAttributes,unlimitedDimId
INTEGER   :: var_id_uu, var_id_vv, var_id_lonc, var_id_latc
INTEGER   :: xtype, ndims_var, nAtts
INTEGER, DIMENSION(4)   :: dimids_var, countA, startA
INTEGER  :: lat_dim_id, lon_dim_id, uu_dim_id, vv_dim_id
INTEGER  :: err, nsigma, sigma_dim_id
REAL, ALLOCATABLE     :: LONC(:), LATC(:)
INTEGER ::  YEAR, MONTH, DAY, HOUR, MINUTE, SECOND



! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. FOR FIRST CALL: OPEN FILE AND READ HEADER.                            !
!        ------------------------------------------                            !

if (frstime) then
   chelp = '00000000000000'
   hcdtcur = '00000000000000'
   chelp(1:8) = file09(8:15)
else
   call incdate (chelp,idelci)
   file09(8:15) = chelp(1:8)
endif

!if hour==0 file of previous day has to opened and
!hour has to be set to 24
READ (chelp,'(I4,5I2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
hcdtcur = chelp
CDTCUR = hcdtcur

IF (HOUR.EQ.0) THEN
	call incdate (hcdtcur, -24*3600)
	!WRITE (IU06,*) ' *    hcdtcur = ', hcdtcur
        file09(8:15) = hcdtcur(1:8)
        HOUR = 24
END IF

L = LEN_TRIM(FILE09)
err =  NF90_OPEN(FILE09(1:L), NF90_CLOBBER, IU09)
IF (err.NE.0) THEN
      WRITE (IU06,*) ' ****************************************************'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *       FATAL ERROR IN SUB. READ_TOPO_INPUT        *'
      WRITE (IU06,*) ' *       ===================================        *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' * TOPO INPUT FILE COULD NOT BE OPENED              *'
      WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
      WRITE (IU06,*) ' *    FILE NAME IS  FILE09 = ', TRIM(FILE09)
      WRITE (IU06,*) ' *    UNIT IS         IU09 = ', IU09
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' ****************************************************'
      CALL ABORT1
END IF
WRITE (IU06,*) ' ****************************************************'
WRITE (IU06,*) ' *    FILE NAME IS  FILE09 = ', TRIM(FILE09)
WRITE (IU06,*) ' ****************************************************'




IF (FRSTIME) THEN

   err = nf90_inquire(IU09, nDimensions, nVariables, nAttributes, unlimitedDimId)
   err=  nf90_inq_dimid(IU09,"latc" , lat_dim_id)
   err=  nf90_inq_dimid(IU09,"lonc" , lon_dim_id)
   err = nf90_inquire_dimension(IU09, lat_dim_id, name_dim, N_LAT)
   err = nf90_inquire_dimension(IU09, lon_dim_id, name_dim, N_LON)

   err = nf90_inq_varid(IU09,"lonc", var_id_lonc)
   err = nf90_inquire_variable(IU09, var_id_lonc, name_var, xtype, ndims_var, dimids_var, nAtts)
   ALLOCATE(LONC(N_LON))
   err = nf90_get_var(IU09, var_id_lonc, LONC)
   err = nf90_inq_varid(IU09,"latc", var_id_latc)
   err = nf90_inquire_variable(IU09, var_id_latc, name_var, xtype, ndims_var, dimids_var, nAtts)
   ALLOCATE(LATC(N_LAT))
   err = nf90_get_var(IU09, var_id_latc, LATC)

!  CURRENT ARRAYS MUST BE ORDERED
!  (    1,    1 ) <==> SOUTH WEST !
!  (N_LON,    1 ) <==> SOUTH EAST !
!  (    1, N_LAT) <==> NORTH WEST !
!  (N_LON, N_LAT) <==> NORTH EAST !


   WEST = LONC(1)
   EAST = LONC(N_LON)
   SOUTH = LATC(1)
   NORTH = LATC(N_LAT)


   D_LON = (EAST-WEST)/REAL(N_LON-1)
   D_LAT = (NORTH-SOUTH)/REAL(N_LAT-1)

   CALL SET_CURRENT_HEADER (WEST=WEST,   SOUTH=SOUTH,    &
&                           EAST=EAST,   NORTH=NORTH,    &
&                           D_LON=D_LON, D_LAT=D_LAT,    &
&                           N_LON=N_LON, N_LAT=N_LAT,    &
&                           CODE=0)
   FRSTIME = .FALSE.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. ALLOCATE CURRENT INPUT ARRAYS.                                        !
!        ------------------------------                                        !

IF (.NOT.ALLOCATED(U_MAP)) ALLOCATE(U_MAP(N_LON,N_LAT))
IF (.NOT.ALLOCATED(V_MAP)) ALLOCATE(V_MAP(N_LON,N_LAT))

! ---------------------------------------------------------------------------- !
!                                                                              !
!    3. READ CURRENT FIELD.                                                    !
!       -------------------                                                    !

!number of sigma levels
err=  nf90_inq_dimid(IU09, "sigma" , sigma_dim_id)
err = nf90_inquire_dimension(IU09, sigma_dim_id, name_dim, nsigma)

startA    = 1
startA(3) = nsigma
startA(4) = HOUR
countA    = 1
countA(1) = N_LON
countA(2) = N_LAT

WRITE(IU06,*) '     HOUR = ',HOUR

err = nf90_inq_varid(IU09,"uu", var_id_uu)
err = nf90_inquire_variable(IU09, var_id_uu, name_var, xtype, ndims_var, dimids_var, nAtts)
err = nf90_get_var(IU09, var_id_uu, U_MAP, startA, countA)
err = nf90_inq_varid(IU09,"vv", var_id_vv)
err = nf90_inquire_variable(IU09, var_id_vv, name_var, xtype, ndims_var, dimids_var, nAtts)
err = nf90_get_var(IU09, var_id_vv, V_MAP, startA, countA)

err =  nf90_close(IU09)

WHERE (U_MAP < -100.0) U_MAP = 0.0
WHERE (V_MAP < -100.0) V_MAP = 0.0

CALL SET_CURRENT_FIELD (CDTCUR, U_MAP, V_MAP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!    4. WRITE TEST OUTPUT AND DEALLOCATE ARRAYS.                               !
!       ----------------------------------------                               !

IF (ITEST.GT.1) THEN
   WRITE(IU06,*) ' '
   WRITE(IU06,*) '     SUB. READ_CURRENT_INPUT: FIELD FOR CDTCUR = ',CDTCUR
   WRITE(IU06,'(1X,20F6.2)') U_MAP(1:20,(N_LAT-5):N_LAT)
   WRITE(IU06,'(1X,20F6.2)') V_MAP(1:20,(N_LAT-5):N_LAT)
!   WRITE(IU06,'(1X,20F6.2)') U_MAP(1:20,1:5)
!   WRITE(IU06,'(1X,20F6.2)') V_MAP(1:20,1:5)
END IF

DEALLOCATE (U_MAP, V_MAP)

END SUBROUTINE READ_CURRENT_INPUT
