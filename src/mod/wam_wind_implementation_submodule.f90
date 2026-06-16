submodule (wam_wind_module) wam_wind_implementation_submodule

  use wam_netcdf_input_reader, only : read_wind_fields
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
    CALL read_wind_fields(CD_START, wind_input_file_identifier)
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

US(NIJS:NIJL)  = MAX(US(NIJS:NIJL), 2.0)

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
end submodule
