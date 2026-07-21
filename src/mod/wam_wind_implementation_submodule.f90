submodule (wam_wind_module) wam_wind_implementation_submodule

  use wam_netcdf_input_reader, only : read_wind_fields
  USE WAM_MPI_MODULE,     ONLY: NIJS, NIJL
  implicit none

contains
module procedure wam_wind
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL, PARAMETER :: ALPHACH = 0.0185

INTEGER :: IJ
REAL    :: UU, VV, USTAR, Z0, CD

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. READ WIND DATA AND CHECK DATE.                                        !
!        ------------------------------                                        !
if (input_filetype == 2) then
   WRITE(iu06,*) "ENTERING NETCDF READER" 
    CALL read_wind_fields(CD_START, wind_input_file_name)
else
DO
  WRITE(IU06,*) "Entering ASCII reader"
   CALL READ_WIND_INPUT

   IF (CD_READ.EQ.CD_START) EXIT
   IF (CD_READ.GT.CD_START) THEN
         WRITE (IU06,*) ' ******************************************'
         WRITE (IU06,*) ' *                                        *'
         WRITE (IU06,*) ' * WIND DATE READ IS LATER THAN EXPECTED  *'
         WRITE (IU06,*) ' * DATE READ IS      CD_READ = ', CD_READ
         WRITE (IU06,*) ' * DATE EXPECTED IS CD_START = ', CD_START
         WRITE (IU06,*) ' *                                        *'
         WRITE (IU06,*) ' *   PROGRAM ABORTS  PROGRAM ABORTS       *'
         WRITE (IU06,*) ' *                                        *'
         WRITE (IU06,*) ' ******************************************'
         CALL ABORT1
   END IF
END DO
end if
! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. INTERPOLATE AND BLOCK WINDFIELD                                       !
!        -------------------------------                                       !

IF (EQUAL_GRID) THEN
   DO IJ = NIJS, NIJL
      US(IJ)= U_IN(IFROMIJ(IJ),KFROMIJ(IJ))
      DS(IJ)= V_IN(IFROMIJ(IJ),KFROMIJ(IJ))
   END DO
ELSE
   CALL INTERPOLATION_TO_GRID (US, DS)
END IF
! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. TRANSFORM TO MAGNITUDE AND DIRECTION.                                 !
!         -------------------------------------                                !

DO IJ = NIJS, NIJL
   UU = US(IJ)
   VV = DS(IJ)
   US(IJ) = SQRT(UU**2 + VV**2)
   IF (US(IJ).NE.0.) THEN
      DS(IJ) = ATAN2(UU,VV)
   ELSE
      DS(IJ) = 0.
   ENDIF
   IF (DS(IJ).LT.0.) DS(IJ) = DS(IJ) + ZPI
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. PROCESS WINDS ACCORDING TO TYPE                                       !
!        NOTHING TO DO FOR WIND SPEED U10 (CODE_IN = 3).                       !
!        ---------------------------------------------                         !

IF (CODE_IN.EQ.1) THEN

!     3.2  INPUT IS FRICTION VELOCITY.                                         !
!          ---------------------------                                         !

   DO IJ = NIJS, NIJL
         USTAR = MAX(0.01,US(IJ))
         Z0  = ALPHACH/G*USTAR**2
         CD  = XKAPPA/ALOG(10./Z0)
         US(IJ) = USTAR/CD
   END DO

ELSE IF (CODE_IN.EQ.2) THEN

!     3.3 INPUT WINDS ARE SURFACE STRESSES.                                    !
!         ---------------------------------                                    !
!                                                                              !
   DO IJ = NIJS, NIJL
         USTAR = MAX (0.01, SQRT(US(IJ)/ROAIR))
         Z0  = ALPHACH/G*USTAR**2
         CD  = XKAPPA/ALOG(10./Z0)
         US(IJ) = USTAR/CD
   END DO
END IF

US  = MAX(US, 2.0)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. TEST OUTPUT OF WAVE MODEL BLOCKS                                      !
!        ---------------------------------                                     !

IF (ITEST.GE.3) THEN
   IJ = MIN(NIJS+10,NIJL)
   WRITE (IU06,*) ' '
   WRITE (IU06,*) '      SUB. WAM_WIND: WINDFIELDS CONVERTED TO MODEL GRID'
   WRITE (IU06,*) ' '
   WRITE (IU06,*) ' US(NIJS:NIJS+10) = ', US(NIJS:IJ)
   WRITE (IU06,*) ' DS(NIJS:NIJS+10) = ', DS(NIJS:IJ)
END IF

end procedure wam_wind

module procedure INTERPOLATION_TO_GRID

! ---------------------------------------------------------------------------- !
!                                                                              !
!   INTERPOLATION_TO_GRID - INTERPOLATES TO MODEL GRID POINTS.                 !
!                                                                              !
!     H. GUNTHER    GKSS  DECEMBER 2001.                                       !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        LOCATE AND INTERPOLATE IN INPUT GRID.                                 !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       DOUBLE LINEAR INTERPOLATION IN INPUT GRID. OPTIONAL A SECOND INPUT     !
!       CAN BE INTERPOLATED AT THE SAME CALL.                                  !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.
!     --------------------

!REAL,    INTENT(OUT)  :: US(NIJS:NIJL)  !! SPACE INTERPOLATED OUTPUT FIELD.
!REAL,    INTENT(OUT)  :: VS(NIJS:NIJL)  !! OPTIONAL SECOND OUTPUT FIELD.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.
!     ----------------

LOGICAL, SAVE :: FRSTIME = .TRUE.

INTEGER :: IJ
INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: I1, I2, K1, K2
REAL,    SAVE ,ALLOCATABLE, DIMENSION(:) :: DI, DK

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIALIZE INTERPOLATION WEIGHTS.                                     !
!        ---------------------------------                                     !

IF (FRSTIME) THEN
   CALL INITIALIZE
   FRSTIME =.FALSE.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. LINEAR INTERPOLATION.                                                 !
!        ----------------------                                                !
!     2.1 FIRST FIELD.

DO IJ = NIJS,NIJL
   US(IJ) = (U_IN(I1(IJ),K1(IJ))*(1.-DI(IJ))+U_IN(I2(IJ),K1(IJ))*DI(IJ))*(1.-DK(IJ)) &
&         + (U_IN(I1(IJ),K2(IJ))*(1.-DI(IJ))+U_IN(I2(IJ),K2(IJ))*DI(IJ))*DK(IJ)
END DO

!     2.2 SECOND FIELD.

DO IJ =  NIJS,NIJL
   VS(IJ) = (V_IN(I1(IJ),K1(IJ))*(1.-DI(IJ))+V_IN(I2(IJ),K1(IJ))*DI(IJ))*(1.-DK(IJ)) &
&         + (V_IN(I1(IJ),K2(IJ))*(1.-DI(IJ))+V_IN(I2(IJ),K2(IJ))*DI(IJ))*DK(IJ)
END DO

CONTAINS

SUBROUTINE INITIALIZE

WRITE(IU06,*) "NIJS, NIJL" , NIJS , NIJL

ALLOCATE (I1(NIJS:NIJL))
ALLOCATE (I2(NIJS:NIJL))
ALLOCATE (K1(NIJS:NIJL))
ALLOCATE (K2(NIJS:NIJL))
ALLOCATE (DI(NIJS:NIJL))
ALLOCATE (DK(NIJS:NIJL))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. TRANSFORM MODEL COORDINATES TO INPUT GRID.                            !
!        ------------------------------------------                            !

I1(NIJS:NIJL) = AMOWEP + (IFROMIJ(NIJS:NIJL)-1)*ZDELLO(KFROMIJ(NIJS:NIJL)) - WEST_IN
I1(NIJS:NIJL) = MOD(I1(NIJS:NIJL)+2*M_S_PER,M_S_PER)
DI(NIJS:NIJL) = REAL(I1(NIJS:NIJL))/REAL(DX_IN)

K1(NIJS:NIJL) = AMOSOP + (KFROMIJ(NIJS:NIJL)-1)*XDELLA - SOUTH_IN
DK(NIJS:NIJL) = REAL(K1(NIJS:NIJL))/REAL(DY_IN)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE CORNER POINT INDICES IN INPUT GRID.                           !
!        -------------------------------------------                           !

I1(NIJS:NIJL)  = INT(DI(NIJS:NIJL))+1
K1(NIJS:NIJL)  = INT(DK(NIJS:NIJL))+1
K2(NIJS:NIJL)  = MIN(NY_IN,K1(NIJS:NIJL)+1)
I2(NIJS:NIJL)  = I1(NIJS:NIJL)+1

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. DISTANCES OF INTERPOLATION POINT FROM LOW LEFT CORNER POINT.          !
!        ------------------------------------------------------------          !

DI(NIJS:NIJL) = DI(NIJS:NIJL)-REAL(I1(NIJS:NIJL))+1.
DK(NIJS:NIJL) = DK(NIJS:NIJL)-REAL(K1(NIJS:NIJL))+1.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. CORRECTIONOF FIRST AND LAST GRID LINES (PERIODIC OR UNPERIODIC GRID). !
!        --------------------------------------------------------------------- !

IF (PER) THEN
   WHERE (I1(NIJS:NIJL).EQ.NX_IN) I2(NIJS:NIJL) = 1
   WHERE (I1(NIJS:NIJL).EQ.0 ) I1(NIJS:NIJL) = NX_IN
ELSE
   WHERE (I1(NIJS:NIJL).EQ.NX_IN) I2(NIJS:NIJL) = NX_IN
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. CHECK WHETHER POINTS ARE IN GRID.                                     !
!        ---------------------------------                                     !

IF (MINVAL(I1).LT.1 .OR. MAXVAL(I1).GT.NX_IN .OR.                              &
&   MINVAL(K1).LT.1 .OR. MAXVAL(K1).GT.NY_IN) THEN
   WRITE(IU06,*) ' *******************************************'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *  FATAL ERROR IN INTERPOLATION_TO_GRID   *'
   WRITE(IU06,*) ' *  ====================================   *'
   WRITE(IU06,*) ' * POINT IS OUTSIDE OF INPUT GRID          *'
   WRITE(IU06,*) ' * DIMENSION OF INPUT GRID IS   NX_IN = ', NX_IN
   WRITE(IU06,*) ' *                              NY_IN = ', NY_IN
   WRITE(IU06,*) ' * MIN AND MAX OF INDEX ARE                *'
   WRITE(IU06,*) ' * I1:  MIN, MAX = ', MINVAL(I1), MAXVAL(I1)
   WRITE(IU06,*) ' * I2:  MIN, MAX = ', MINVAL(I2), MAXVAL(I2)
   WRITE(IU06,*) ' * K1:  MIN, MAX = ', MINVAL(K1), MAXVAL(K1)
   WRITE(IU06,*) ' * K2:  MIN, MAX = ', MINVAL(K2), MAXVAL(K2)
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *  PROGRAM ABORTS     PROGRAM ABORTS      *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *******************************************'
   CALL ABORT1
END IF

END SUBROUTINE INITIALIZE

end procedure INTERPOLATION_TO_GRID

end submodule
