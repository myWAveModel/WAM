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
!          REAL*8  :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].          !
!          REAL*8  :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].         !
!          REAL*8  :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].              !
!          REAL*8  :: NORTH      !! NORTH LATITUDE OF GRID [DEG].              !
!          REAL*8  :: WEST       !! WEST LONGITUDE OF GRID [DEG].              !
!          REAL*8  :: EAST       !! EAST LONGITUDE OF GRID [DEG].
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
!
! MB, june 2023: the routine reads in monthly netcdf files that starts at after 
! 00.00 ( first timestep of the netcdf is e.g. 201003010300). Netcdf of the  
! month before the startdate of the simulations is also required(e.g. 
! simulations start on 2010010100, also 200912 netcdf wind file is needed      !
!     
!   EXTERNALS.                                                               !
!     ----------                                                               !

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

USE netcdf

IMPLICIT NONE


INTEGER :: status, ioerror
INTEGER :: ncid(10), varid(10,50), dimid(10,10), NN(10,10), ON(10,10)=1

INTEGER :: NX, NY, ND, NT, NV
INTEGER :: n, n0, n1, n2, x, x0, x1, x2, y, y0, y1, y2, d, d0, d1, d2, t, t0, t1, t2


LOGICAL, ALLOCATABLE, DIMENSION(:,:,:) :: mask
REAL   , ALLOCATABLE, DIMENSION(:)       :: lon,lat,tiv



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
REAL,    ALLOCATABLE  :: U_MAP(:,:,:) !! 1. COMPONENT OF WIND MAP [M/S].
REAL,    ALLOCATABLE  :: V_MAP(:,:,:) !! 2. COMPONENT OF WIND MAP [M/S].
CHARACTER (LEN=14)    :: CDTWIR     !! DATE/TIME OF WIND FIELD
INTEGER, SAVE         :: ix         !! String position of filename

LOGICAL, SAVE  :: FRSTIME = .TRUE.
INTEGER        :: LEN, IOS, J

character (len=14), save :: chelp, chelp_back 

INTEGER  :: err, L
INTEGER ::  YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, YEAR_b, MONTH_b, DAY_b, HOUR_b, MINUTE_b, SECOND_b
INTEGER ::  TT_STEP, timestep



! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. FOR FIRST CALL: OPEN FILE                           !
!        ------------------------------------------                            ! 
                                                                               !

timestep = idelwi/3600


if (frstime) then

   chelp = CDATEA
   chelp_back = CDATEA	
   call incdate (chelp_back,-idelwi) 

   ix=index(file01,'_y',.TRUE.)  !! find the last occurrence of ./.  
   file01(ix+2:ix+5)= chelp_back(1:4) 
   file01(ix+7:ix+8)= chelp_back(5:6) !compone file name

   READ (chelp_back,'(I4,5I2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
   
   TT_STEP= ((HOUR+timestep)/timestep) + ((DAY*24/timestep) - (24/timestep))
  
 else  
    call incdate (chelp,idelwi)    !chelp == CDATEA  only at first timestep. then CDATEA increases with wam integration timestep while chelp with wind input timestep idelwi
    READ (chelp,'(I4,5I2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
    chelp_back = chelp
    call incdate (chelp_back,-idelwi)
    READ (chelp_back,'(I4,5I2)') YEAR_b, MONTH_b, DAY_b, HOUR_b, MINUTE_b, SECOND_b  

    IF (MONTH .EQ. MONTH_b) THEN

      file01(ix+2:ix+5)= chelp(1:4) 
      file01(ix+7:ix+8)= chelp(5:6) 
      TT_STEP = ((HOUR+timestep)/timestep) + ((DAY*24/timestep) - (24/timestep)) -1

    ELSE

      file01(ix+2:ix+5)= chelp_back(1:4) 
      file01(ix+7:ix+8)= chelp_back(5:6) !compone file name
      TT_STEP= ((HOUR_b+timestep)/timestep) + ((DAY_b*24/timestep) - (24/timestep))

   ENDIF   

endif

WRITE (IU06,*) 'wwind date', chelp
WRITE (IU06,*) 'wwind file', file01
WRITE (IU06,*) 'wwind timestep', TT_STEP




WRITE (IU06,*) ' *    HOUR = ', HOUR
WRITE (IU06,*) ' *    MONTH = ', MONTH
WRITE (IU06,*) ' *    DAY = ', DAY
WRITE (IU06,*) ' *    MINUTE = ', MINUTE
WRITE (IU06,*) ' *    SECOND = ', SECOND
WRITE (IU06,*) ' *    chelp= ', chelp
WRITE (IU06,*) ' *    file = ', file01


LEN = LEN_TRIM(FILE01)  !number of characters of the file01 filename string


WRITE (IU06,*) 'Read'//TRIM(FILE01)

err =  NF90_OPEN(file01(1:LEN), NF90_CLOBBER, ncid(1))
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

CALL check(nf90_open(TRIM(FILE01), NF90_NOWRITE, ncid(1)))

IF (frstime) then
   CALL check(nf90_open(TRIM(FILE01), NF90_NOWRITE, ncid(1)))

   CALL check(nf90_inq_dimid(ncid(1), 'time', dimid(1,3))); CALL check(nf90_inquire_dimension(ncid(1), dimid(1,3), len=NN(1,3)))
   CALL check(nf90_inq_dimid(ncid(1),  'lat', dimid(1,2))); CALL check(nf90_inquire_dimension(ncid(1), dimid(1,2), len=NN(1,2)))
   CALL check(nf90_inq_dimid(ncid(1),  'lon', dimid(1,1))); CALL check(nf90_inquire_dimension(ncid(1), dimid(1,1), len=NN(1,1)))

   NX=NN(1,1);NY=NN(1,2);NT=NN(1,3)

   ALLOCATE(lon(NX),lat(NY),tiv(NT),mask(NX,NY,NT))

   CALL check(nf90_inq_varid(ncid(1), 'time', varid(1,3))); CALL check(nf90_get_var(ncid(1), varid(1,3),  tiv, start=ON(1,3:3), count=NN(1,3:3)))
   CALL check(nf90_inq_varid(ncid(1),  'lat', varid(1,2))); CALL check(nf90_get_var(ncid(1), varid(1,2),  lat, start=ON(1,2:2), count=NN(1,2:2)))
   CALL check(nf90_inq_varid(ncid(1),  'lon', varid(1,1))); CALL check(nf90_get_var(ncid(1), varid(1,1),  lon, start=ON(1,1:1), count=NN(1,1:1)))

   N_LON=NX
   N_LAT=NY

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

IF (.NOT.ALLOCATED(U_MAP) ) ALLOCATE(U_MAP(N_LON,N_LAT,1))
IF (.NOT.ALLOCATED(V_MAP) ) ALLOCATE(V_MAP(N_LON,N_LAT,1))

! ---------------------------------------------------------------------------- !
!                                                                              !
!    3. READ WIND FIELD.                                                       !
!       -------------------


!for CMIP6 data

!CALL check(nf90_inq_varid(ncid(1), 'uas', varid(1,4))); CALL check(nf90_get_var(ncid(1), varid(1,4), U_MAP, start=(/1,1,TT_STEP/), count=(/N_LON,N_LAT,1/)))
!CALL check(nf90_inq_varid(ncid(1), 'vas', varid(1,5))); CALL check(nf90_get_var(ncid(1), varid(1,5), V_MAP, start=(/1,1,TT_STEP/), count=(/N_LON,N_LAT,1/)))



!for ERA5 data

CALL check(nf90_inq_varid(ncid(1), 'u10', varid(1,4))); CALL check(nf90_get_var(ncid(1), varid(1,4), U_MAP, start=(/1,1,TT_STEP/), count=(/N_LON,N_LAT,1/)))
CALL check(nf90_inq_varid(ncid(1), 'v10', varid(1,5))); CALL check(nf90_get_var(ncid(1), varid(1,5), V_MAP, start=(/1,1,TT_STEP/), count=(/N_LON,N_LAT,1/)))

CDTWIR=chelp

CALL SET_WIND_FIELD (CDTWIR, RESHAPE(REAL(U_MAP),(/N_LON,N_LAT/)), &
                             RESHAPE(REAL(V_MAP),(/N_LON,N_LAT/)))

! ---------------------------------------------------------------------------- !
!                                                                              !
!    4. WRITE TEST OUTPUT AND DEALLOCATE ARRAYS.                               !
!       ----------------------------------------                               !
IF (ITEST.GT.1) THEN
   WRITE(IU06,*) ' READ_WIND_INPUT -  WIND FIELD FOR THE CDTWIR = ', CDTWIR
   WRITE(IU06,'(1X,24F5.2)') U_MAP(1:MIN(24,N_LON),1:MIN(5,N_LAT),1)
   WRITE(IU06,*) ' '
   WRITE(IU06,'(1X,24F5.2)') V_MAP(1:MIN(24,N_LON),1:MIN(5,N_LAT),1)
END IF

DEALLOCATE(U_MAP)
DEALLOCATE(V_MAP)

close (iu01)
RETURN



CONTAINS

SUBROUTINE check(status)
INTEGER, intent (in) :: status

IF(status /= NF90_NOERR) THEN
PRINT *, TRIM(NF90_STRERROR(status))
STOP "Stopped"
END IF

END SUBROUTINE check


END SUBROUTINE READ_WIND_INPUT




