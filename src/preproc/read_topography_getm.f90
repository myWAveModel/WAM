SUBROUTINE READ_TOPOGRAPHY

! ---------------------------------------------------------------------------- !
!                                                                              !
!    READ_TOPOGRAPHY - READ TOPOGRAPHY.                                        !
!                                                                              !
!     H. GUNTHER      GKSS  JANUARY 2002                                       !
!     A. Behrens      HZG   January 2014  Topography real                      !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO READ A TOPOGRAPHY FILE FROM GETM NetCDF-FILE AND                    !
!       TRANSFER THE DATA TO THE PREPROC MODULE.                               !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       FILE08, WHICH IS DEFINED IN THE USER INPUT, IS ASSIGNED TO IU08.       !
!       THE NetCDF - FILE MUST CONTAIN THE FOLLOWING VARIABLES                 !
!                    LATC - LATITUDES                                          !
!                    LONC - LONGITUDES                                         !
!                    BATHYMETRY                                                !
!                                                                              !
!       THE FOLLOWING INFORMATION HAS TO BE TRANSFERRED BY SUB. SET_TOPOGRAPHY !
!       TO THE WAM_GRID_MODULE:                                                !
!         INTEGER    :: N_LON      !! NUMBER OF LONGITUDES.                    !
!         INTEGER    :: N_LAT      !! NUMBER OF LATITUDES.                     !
!         REAL*8     :: D_LAT      !! LATITUDE INCREMENT.                      !
!         REAL*8     :: D_LON      !! LONGITUDE INCREMENT.                     !
!         REAL*8     :: SOUTH      !! SOUTH LATITUDE.                          !
!         REAL*8     :: NORTH      !! NORTH LATITUDE.                          !
!         REAL*8     :: WEST       !! WEST LONGITUDE.                          !
!         REAL*8     :: EAST       !! EAST LONGITUDE.                          !
!         REAL       :: D_MAP(:,:) !! WATER DEPTH [M].                         !
!                                                                              !
!      ALL INCREMENTS, LATITUDES AND LONGITUDES MUST BE REAL*8 IN DEGREES      !
!                                                                              !
!      THE TOPOGRAPHY MUST BE ON A REGULAR LATITUDE-LONGITUDE GRID ARRANGED    !
!      FROM  WEST TO EAST AND FROM SOUTH TO NORTH, WHICH IS                    !
!      THE DEPTH ARRAY "D_MAP(I,K)" MUST BE ORDERED AS                         !
!                 (    1,    1 ) <==> SOUTH WEST                               !
!                 (N_LON,    1 ) <==> SOUTH EAST                               !
!                 (    1, N_LAT) <==> NORTH WEST                               !
!                 (N_LON, N_LAT) <==> NORTH EAST                               !
!       POSITIVE VALUES ARE SEA DEPTHS AND NEGATIVE VALUES ARE LAND.           !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !

USE netcdf                                                            !
USE WAM_COORDINATE_MODULE           !! COORDINATE PROCEDURES

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1                      !! TERMINATES PROCESSING.

USE PREPROC_MODULE,       ONLY:  &
&       SET_TOPOGRAPHY              !! TRANSFERS DEPTH DATA TO MODULE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     MODULE VARIABLES.                                                        !
!     -----------------                                                        !

USE WAM_FILE_MODULE,   ONLY: IU06, IU08, FILE08

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER               :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER               :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL (KIND=KIND_D)    :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL (KIND=KIND_D)    :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: WEST       !! WEST LONGITUDE OF GRID [DEG].
REAL (KIND=KIND_D)    :: EAST       !! EAST LONGITUDE OF GRID [DEG].
REAL, ALLOCATABLE     :: D_MAP(:,:) !! WATER DEPTH [M].

INTEGER                  :: I, K, L, IMAX, IOS, IA, IE
CHARACTER*1, ALLOCATABLE :: AX(:)

CHARACTER (LEN=80)  :: name_var, name_dim
INTEGER   :: nDimensions, nVariables, nAttributes,unlimitedDimId
INTEGER   :: var_id_bathy, var_id_lonc, var_id_latc
INTEGER   :: xtype, ndims_var, nAtts
INTEGER, DIMENSION(4)   :: dimids_var, countA, startA
INTEGER  :: lat_dim_id, lon_dim_id, bathy_dim_id
INTEGER  :: err
REAL, ALLOCATABLE     :: LONC(:), LATC(:)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. OPEN NETCDF FILE.                                                     !
!                                                                              !
L = LEN_TRIM(FILE08)
err =  NF90_OPEN(FILE08(1:L), NF90_CLOBBER, IU08)
IF (err.NE.0) THEN
   WRITE (IU06,*) ' *****************************************************'
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' *     FATAL  ERROR IN SUB. READ_TOPOGRAPHY          *'
   WRITE (IU06,*) ' *     =====================================         *'
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' * ERROR WHEN OPENING TOPOGRAPHY_FILE                *'
   WRITE (IU06,*) ' * FILE NAME IS            FILE08 = ', FILE08(1:L)
   WRITE (IU06,*) ' * ERROR CODE                 ERR = ', err
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' *      PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' *****************************************************'
   CALL ABORT1
ELSE

   WRITE(IU06,*) ' SUB. READ TOPOGRAPHY: FILE CONNECTED TO UNIT =', IU08,      &
&                ' FILE NAME IS: ',FILE08(1:L)
END IF

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

err = nf90_inq_varid(IU08,"bathymetry", var_id_bathy)
err = nf90_inquire_variable(IU08, var_id_bathy, name_var, xtype, ndims_var, dimids_var, nAtts)
ALLOCATE(D_MAP(N_LON, N_LAT))
err = nf90_get_var(IU08, var_id_bathy, D_MAP)


err =  nf90_close(IU08)

WEST = LONC(1)
EAST = LONC(N_LON)
NORTH = LATC(N_LAT)
SOUTH = LATC(1)

CALL ADJUST (WEST, EAST)

D_LON = (EAST-WEST)/REAL(N_LON-1)
D_LAT = (NORTH-SOUTH)/REAL(N_LAT-1)



! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. TRANSFER TO MODEULE.                                                  !
!        --------------------                                                  !

CALL SET_TOPOGRAPHY (N_LON, N_LAT, D_LON, D_LAT, SOUTH, NORTH, WEST, EAST,     &
&                    D_MAP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. DEALLOCATE ARRAYS.                                                    !
!        ---------------------------------                                     !

DEALLOCATE (D_MAP, LATC, LONC)

END SUBROUTINE READ_TOPOGRAPHY
