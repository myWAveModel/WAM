SUBROUTINE READ_SPECTRUM (IU06, IUNIT, IEOF)

! ---------------------------------------------------------------------------- !
!                                                                              !
!    READ_SPECTRUM -  READS IN A SPECTRUM                                      !
!                                                                              !
!     M. DE LAS HERAS  KNMI/PCM  FEBRUARY  1990                                !
!                                                                              !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        READ SPECTRA FROM WAMODEL OUTPUT FILE                                 !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
!                                                                              !
!       *NONE*                                                                 !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLE                                                       !

USE WAM_PRINT_MODULE, ONLY: KL, ML, CO, FR, THETA,                             &
&                           SPEC_LAT, SPEC_LON, SPEC_DATE,                     &
&                           SPEC, U10, UDIR, US, DEPTH, CSPEED, CDIR,          &
&                           HS, PPER, MPER, TM1, TM2, MDIR, SPRE, TAUW,        &
&                           SPEC_SEA,                                          &
&                           HS_SEA, PPER_SEA, MPER_SEA, TM1_SEA, TM2_SEA,      &
&                           MDIR_SEA, SPRE_SEA,                                &
&                           SPEC_SWELL,                                        &
&                           HS_SWELL, PPER_SWELL, MPER_SWELL, TM1_SWELL,       &
&                           TM2_SWELL, MDIR_SWELL, SPRE_SWELL

USE WAM_OUTPUT_SET_UP_MODULE,ONLY: NOUT_S, PFLAG_S

IMPLICIT NONE

INTEGER,    INTENT(IN)  :: IU06     !! PRINTER OUTPUT UNIT.
INTEGER,    INTENT(IN)  :: IUNIT    !! INPUT UNIT.
LOGICAL,    INTENT(OUT) :: IEOF     !! .TRUE. IF END OF FILE ENCOUNTED.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE                                                           !

INTEGER :: K, M
REAL    :: XANG, XFRE, TH1, FR1
REAL, PARAMETER :: RAD = 3.1415927/180.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. DATA HEADER FROM WAVE MODEL OUTPUT (SUB OUTSPP).                      !
!        ------------------------------------------------                      !

IEOF = .FALSE.

READ (IUNIT, END=4000) SPEC_LON, SPEC_LAT, SPEC_DATE, XANG, XFRE, TH1, FR1, CO
READ (IUNIT, END=4000) PFLAG_S(1:NOUT_S)
KL = NINT(XANG)
ML = NINT(XFRE)
READ (IUNIT, END=4000) U10, UDIR, US, DEPTH, CSPEED, CDIR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. CHECK ARRAYS.                                                         !
!        -------------                                                         !

IF (.NOT.ALLOCATED(FR)   ) THEN
   ALLOCATE (FR(1:ML))
   FR(1) = FR1              !! COMPUTE FREQUENCIES.
   DO M=2,ML
      FR(M) = CO*FR(M-1)
   END DO
END IF
IF (.NOT.ALLOCATED(THETA)) THEN
    ALLOCATE (THETA(1:KL))  !! COMPUTE DIRECTIONS.
    DO K = 1, KL
      THETA(K) = (TH1 + REAL(K-1)*360./XANG)*RAD
   END DO
END IF
IF (PFLAG_S(1) .AND. .NOT.ALLOCATED(SPEC) )      ALLOCATE (SPEC(1:KL,1:ML))
IF (PFLAG_S(2) .AND. .NOT.ALLOCATED(SPEC_SEA)  ) ALLOCATE (SPEC_SEA(1:KL,1:ML))
IF (PFLAG_S(3) .AND. .NOT.ALLOCATED(SPEC_SWELL)) ALLOCATE (SPEC_SWELL(1:KL,1:ML))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. DATA FROM WAVE MODEL OUTPUT (SUB OUTSPP).                             !
!        -----------------------------------------                             !

IF (PFLAG_S(1)) THEN
   READ(IUNIT) HS, PPER, MPER, TM1, TM2, MDIR, SPRE
   READ(IUNIT) SPEC
END IF
IF (PFLAG_S(2)) THEN
   READ(IUNIT) HS_SEA, PPER_SEA, MPER_SEA, TM1_SEA, TM2_SEA, MDIR_SEA, SPRE_SEA
   READ(IUNIT) SPEC_SEA
END IF
IF (PFLAG_S(3)) THEN
   READ(IUNIT) HS_SWELL, PPER_SWELL, MPER_SWELL, TM1_SWELL, TM2_SWELL,         &
&              MDIR_SWELL, SPRE_SWELL
   READ(IUNIT) SPEC_SWELL
END IF

RETURN

!----------------------------------------------------------------------------- !
!                                                                              !
!     4. END OF FILE                                                           !
!        -----------                                                           !

 4000 CONTINUE
      IEOF = .TRUE.

RETURN
END SUBROUTINE READ_SPECTRUM
