PROGRAM PRINT_SCR_FILE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     PRINT_SCR_FILE - PRINTS SCR FROM GRIDDED WAMODEL OUTPUT.                 !
!                                                                              !
!      H. GUNTHER     GKSS/ECMWF  DECEMBER 1989                                !
!                     HZG         DECEMBER 2011      RE-ORGANISED              !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       POSTPROCESSING OF WAM MODEL INTEGRATED DATA.                           !
!       PRINTS MAPS FROM GRIDDED WAMODEL OUTPUT.                               !
!                                                                              !
!     INTERFACE.                                                               !
!     ----------                                                               !
!                                                                              !
!          IU01    INPUT UNIT OF INTEGRATED PARAMETER FILE.                    !
!          IU05    USER INPUT FILE.                                            !
!          IU06    PRINTER OUTPUT.                                             !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!          THIS PROGRAM READS THE WAM MODEL GRIDDED OUTPUT FILES AND           !
!          PRINTS SELECTED FIELDS OF INTEGRATED DATA AT SPECIFIED TIMES.       !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!        NONE.                                                                 !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!      EXTERNALS.                                                              !
!      ----------                                                              !

USE WAM_GENERAL_MODULE, ONLY:  &
&       INCDATE,               &  !! UPDATES A DATE/TIME GROUP.
&       PRINT_ARRAY,           &  !! PRINT AN ARRAY.
&       OPEN_FILE                 !! OPEN A FILE.


USE WAM_PRINT_MODULE,   ONLY:  &
&       PRINT_SCR_USER           !! PRINT A PROTOCOLL OF USER SETTINGS.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     MODULE VARIABLES.                                                        !
!     -----------------                                                        !

USE WAM_FILE_MODULE,  ONLY: IU05, FILE05, IU06, FILE06, ITEST
USE WAM_PRINT_MODULE, ONLY: CDATEA, CDATEE, IDELDO,                            &
&                           NOUTT, COUTT,  NOUT_SCR, CFLAG_SCR, TITL_SCR,      &
&                           IU01, FILE01, CDTFILE, IDFILE,                     &
&                           NX, NY, AMOWEP, AMOSOP, AMOEAP, AMONOP,            &
&                           XDELLA, XDELLO, PFLAG_SCR, CDTINTT, GRID
USE WAM_OUTPUT_PARAMETER_MODULE, ONLY: SCAL_SCR
IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

LOGICAL      :: IEOF             !! END OF FILE ENCOUNTED IN SUB. INGRID.
INTEGER      :: IFAIL            !! OPEN ERROR
INTEGER      :: I                !! LOOP COUNTER.
LOGICAL,SAVE :: FRSTIME = .TRUE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITALISATION.                                                        !
!        --------------                                                        !

!     1.1 SET USER INPUT AND PROTOCOLL FILE NAMES.                             !

FILE05 = 'Scr_User'
FILE06 = 'Scr_Prot'

!     1.2  OPEN USER FILE AND READ USER INPUT.                                 !

OPEN (UNIT=IU06, FILE=FILE06, FORM="FORMATTED", STATUS="UNKNOWN")
CALL READ_SCR_USER
CALL PRINT_SCR_USER

!     1.3 FIRST AND LAST OUTPUT DATE.                                          !

IF (NOUTT.GT.0) THEN
   CDATEE = COUTT(1)
   CDATEA = COUTT(1)
   DO I = 1,NOUTT
      IF (COUTT(I).LT.CDATEA) CDATEA = COUTT(I)
      IF (COUTT(I).GT.CDATEE) CDATEE = COUTT(I)
   END DO
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. LOOP OVER OUTPUT FILES.                                               !
!        -----------------------                                               !

FILES: DO

!     2.1 FETCH FILE.                                                          !

   CALL OPEN_FILE (IU06, IU01, FILE01, CDTFILE, 'OLD', IFAIL)
   IF (IFAIL.NE.0) STOP

!     2.2  LOOP OVER OUTPUT TIMES.                                             !

   TIMES: DO

!     2.2.1 READ IN WIND AND WAVE FIELDS.                                      !

      CALL READ_SCR_FILE (IU01, IEOF)

      IF (IEOF) EXIT TIMES     !! IF END OF FILE ENCOUNTED THEN EXIT TIME LOOP
      IF (ITEST.GT.0) THEN
         WRITE (IU06,*) 'SUB. READ_GRID_FILE DONE'
         WRITE (IU06,*) 'NEXT OUTPUT DATE, PARAMETER INPUT DATE: ',            &
&                        CDATEA, CDTINTT
      END IF


!     2.2.2 OUTPUT TIME FOUND?                                                 !

      IF (CDTINTT.LT.CDATEA) CYCLE TIMES
      DO WHILE (CDTINTT.GT.CDATEA)
         CALL NEXT_OUTPUT_TIME
         IF (CDATEA.GT.CDATEE) EXIT FILES   !! ALL DONE?
         IF (CDTINTT.LT.CDATEA) CYCLE TIMES
      END DO

!     2.2.3 DO OUTPUT OF REQUESTED FIELDS.                                     !

      WRITE (IU06,*) ' '
      IF (FRSTIME) THEN
         DO I = 1,NOUT_SCR
           IF (.NOT.PFLAG_SCR(I) .AND. CFLAG_SCR(I)) THEN
              WRITE(IU06,*) TITL_SCR(I), 'IS NOT STORED IN FILE'
            END IF
         END DO
         FRSTIME = .FALSE.
         WRITE (IU06,*) ' '
      END IF
      DO I = 1,NOUT_SCR
         IF (PFLAG_SCR(I) .AND. CFLAG_SCR(I)) CALL PRINT_ARRAY (IU06, CDTINTT,     &
&           TITL_SCR(I), GRID(:,:,I), AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL_SCR(I))
      END DO

!     2.2.7 NEXT OUTPUT TIME.                                                  !

      CALL NEXT_OUTPUT_TIME
      IF (CDATEA.GT.CDATEE) EXIT FILES  !! ALL DONE?

   END DO TIMES

   CLOSE (UNIT=IU01, STATUS='KEEP')     !! CLOSE OLD FILE
   IF (IDFILE.GT.0) THEN
      CALL INCDATE (CDTFILE, IDFILE)    !! INCREMENT DATE FOR THE NEXT FILE.
   ELSE
      EXIT FILES
   END IF
END DO FILES

! ---------------------------------------------------------------------------- !

CONTAINS

   SUBROUTINE NEXT_OUTPUT_TIME

   CHARACTER (LEN=14) :: IHH

      IF (NOUTT.EQ.0) THEN
         CALL INCDATE (CDATEA,IDELDO)
      ELSE
         IHH = '99999999999999'
         DO I=1,NOUTT
            IF (COUTT(I).GT.CDATEA .AND. COUTT(I).LT.IHH) IHH = COUTT(I)
         END DO
         CDATEA = IHH
      END IF

   END  SUBROUTINE NEXT_OUTPUT_TIME

END PROGRAM PRINT_SCR_FILE
