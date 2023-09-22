SUBROUTINE INGRID (IUNIT, IEOF)

! ---------------------------------------------------------------------------- !
!                                                                              !
!      INGRID - READS WAVE MODEL OUTPUT FILE OF INTEGRATED DATA                !
!                                                                              !
!     H. GUNTHER          ECMWF             DECEMBER 1989                      !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       POST PROCESSING ROUTINE FOR WAVE MODEL.                                !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       UNFORMATED READ.                                                       !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
!                                                                              !
!       *ABORT*     - TERMINATES PROCESSING.                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_PRINT_MODULE, ONLY: NX, NY, AMOWEP, AMOSOP, AMOEAP, AMONOP,            &
&                           XDELLA, XDELLO, GRID

USE WAM_OUTPUT_SET_UP_MODULE,ONLY: CDTINTT, NOUT_P, PFLAG_P

IMPLICIT NONE

INTEGER, INTENT(IN)  :: IUNIT !! INPUT UNIT FOR WAVE MODEL GRID OUTPUT FILE.
LOGICAL, INTENT(OUT) :: IEOF  !! END OF FILE INDICATOR.

INTEGER :: I
REAL    :: DNX, DNY
character (len=14) :: cstart

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. DATA HEADER FROM WAVE MODEL OUTPUT (SUB. WRITE_INT_PAR_OUTPUT)        !
!        --------------------------------------------------------------        !

READ(IUNIT, END=3000) CDTINTT, DNX, DNY, AMOWEP, AMOSOP, AMOEAP, AMONOP, cstart
READ(IUNIT, END=3000) PFLAG_P(1:NOUT_P)

NX = NINT(DNX)
NY = NINT(DNY)

IF (ANY(PFLAG_P(1:NOUT_P)) .AND. .NOT.ALLOCATED(GRID)) THEN
   ALLOCATE(GRID(NX,NY,NOUT_P))
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. DATA FROM WAVE MODEL OUTPUT (SUB. WRITE_INT_PAR_OUTPUT)               !
!        -------------------------------------------------------               !

DO I =1,NOUT_P
   IF (PFLAG_P(I)) READ (IUNIT, END=3000) GRID(:,:,I)
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. END OF FILE.                                                          !
!        ------------                                                          !
!                                                                              !
      IEOF = .FALSE.
      RETURN

 3000 CONTINUE
      IEOF = .TRUE.

END SUBROUTINE INGRID
