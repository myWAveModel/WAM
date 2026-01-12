MODULE WAM_ASSI_SET_UP_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS THE O-I DATA ASSIMILATION FOR THE WAM MODEL.          !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_COORDINATE_MODULE           !! COORDINATE TYPE AND PROCEDURES

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  &  !! TERMINATE PROCESSING.
&       INCDATE,                 &  !! UPDATE DATE TIME GROUP.
&       OPEN_FILE                   !! OPENS A FILE.

USE WAM_OUTPUT_SET_UP_MODULE, ONLY:  &
&       SAVE_OUTPUT_FILES           !! CLOSES AND OPENS OUTPUT FILES.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE, ONLY: RAD, DEG
USE WAM_FILE_MODULE,    ONLY: IU06
USE WAM_TIMOPT_MODULE,  ONLY: CDTPRO, SPHERICAL_RUN
USE WAM_GRID_MODULE,    ONLY: NY, XDELLA, ZDELLO, SINPH, COSPH, NLON_RG, IPER
USE WAM_OUTPUT_SET_UP_MODULE, ONLY: FFLAG20, FFLAG25, PFLAG20, PFLAG25

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

INTEGER, PRIVATE :: LENT, LENT_S

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. ASSIMILATION OPTIONS.                                                 !
!        ---------------------                                                 !
INTEGER             :: IASSI_SPECTRA       !! ASSIMILATION FLAG SPECTRA !! SA2025
INTEGER             :: IASSI_ALTIMETER     !! ASSIMILATION FLAG         !! SA2025
                                           !!  = 1  FOR ASSIMILATION
                                           !! OTHERWISE NO ASSIMILATION
REAL                :: DIST      !! RADIUS OF INFLUENCE IN DEGREES
REAL                :: SIGOBS    !! MEASUREMENT SCATTER.
REAL                :: SIGMOD    !! MODEL SCATTER.

INTEGER             :: SARCORDIA !! CORRELATION DISTANCE IN METERS
REAL                :: PPSARCF   !! CORRELATION DISTANCE COEFFICIENT
REAL                :: COEFPHI   !! CROSS CORRELATION COEFFICIENT
INTEGER             :: MPARTSW   !! MAXIMUM NUMBER O FPARTITIONINGS
REAL                :: LOWEST  
REAL                :: ALEVEL    
REAL                :: FCUT
    
INTEGER  :: LMAX   = 11          !! MAXIMUM NO. OF GRID UNITS TO SCAN FOR DATA

INTEGER, ALLOCATABLE :: LLON(:)  !! LONGITUDE GRID UNITS TO SCAN FOR DATA
INTEGER  :: LLAT                 !! LATITUDE GRID UNITS TO SCAN FOR DATA
INTEGER  :: NDIM2                !! DIMENSION OF OI ARRAY

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. ASSIMILATION TIME STATUS.                                             !
!        -------------------------                                             !

CHARACTER*14 :: CDATAA  = ' '    !! START DATE  (VVYYMMDDHHMMSS).
CHARACTER*14 :: CDATAE  = ' '    !! END DATE (VVYYMMDDHHMMSS).
CHARACTER*14 :: CDTASS  = ' '    !! DATE OF NEXT ASSIMILATION.
INTEGER      :: IDELASS = -1     !! TIMESTEP ASSIMILATION IN SECONDS.

!     2.1 SPECTRAL ASSIMILATION TIME STATUS                             !! SA2025!
!         ---------------------------------

CHARACTER*14 :: CDATSA  = ' '    !! START DATE  (VVYYMMDDHHMMSS).
CHARACTER*14 :: CDATSE  = ' '    !! END DATE (VVYYMMDDHHMMSS).
CHARACTER*14 :: CDTSPS  = ' '    !! DATE OF NEXT ASSIMILATION.
INTEGER      :: IDELSPE = -1     !! TIMESTEP ASSIMILATION IN SECONDS.



! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. OBSERVATION UNIT AND FILE IDENTIFIER.                                 !
!        -------------------------------------                                 !
!                                                                              !
!  FILE UNIT AND NAME CAN BE REDEFINED BY SET_XXX_FILE SUBROUTINES.            !
!  THE FULL FILE NAME IS THE FILE IDENTIFIER EXTENDED BY THE FILE DATE.        !
!  SEE GFILE FOR DETAILS.                                                      !

!  ALTIMETER DATA
INTEGER      :: IU80   = 80           !! OBSERVATION FILE.
CHARACTER*80 :: FILE80 = 'OBS'        !! OBSERVATION FILE IDENTIFIER.

! SPECTRAL DATA
INTEGER      :: IU90   = 90           !! SPECTRAL OBSERVATION FILE.
CHARACTER(LEN=80) :: FILE90 = 'SWI'   !! SPECTRAL OBSERVATION FILE IDENTIFIER.   			

LOGICAL      :: FG_OUTPUT = .false.   !! FIRST GUESS OUTPUT FLAG.

INTEGER      :: IU30 = 30             !! FIRST GUESS OUTPUT UNIT FOR 
                                      !! INTEGRATED PARAMETER
CHARACTER*80 :: FILE30 = 'MAPFG'      !! FILE IDENTIFIER

INTEGER      :: IU35 = 35             !! FIRST GUESS OUTPUT UNIT FOR SPECTRA AT
                                      !! CERTAIN GRID POINTS.
CHARACTER*80 :: FILE35 = 'OUTFG'      !! FILE IDENTIFIER

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE SET_ASSIMILATION_OPTION_ALTIMETER   !! SETS ASSIMILATION OPTIONS FOR ALTIMETER.
   MODULE PROCEDURE SET_ASSIMILATION_OPTION_ALTIMETER   !! SA2025
END INTERFACE
PUBLIC SET_ASSIMILATION_OPTION_ALTIMETER

INTERFACE SET_ASSIMILATION_OPTION_SPECTRA   !! SETS ASSIMILATION OPTION FOR SPECTRA
   MODULE PROCEDURE SET_ASSIMILATION_OPTION_SPECTRA
END INTERFACE
PUBLIC SET_ASSIMILATION_OPTION_SPECTRA

INTERFACE SET_ASSIMILATION_PERIOD_SPECTRA              !! SETS SPECTRAL ASSIMILATION PERIOD.
   MODULE PROCEDURE SET_ASSIMILATION_PERIOD_SPECTRA
END INTERFACE
PUBLIC SET_ASSIMILATION_PERIOD_SPECTRA

INTERFACE SET_ASSIMILATION_OUTPUT           !! SETS ASSIMILATION FG_OUTPUT.
   MODULE PROCEDURE SET_ASSIMILATION_OUTPUT
END INTERFACE
PUBLIC SET_ASSIMILATION_OUTPUT

INTERFACE SET_ASSIMILATION_PERIOD              !! SETS ASSIMILATION PERIOD.
   MODULE PROCEDURE SET_ASSIMILATION_PERIOD
END INTERFACE
PUBLIC SET_ASSIMILATION_PERIOD

INTERFACE SET_ALTIMETER_OBSERVATION_FILE           !! SETS OBSERVATION FILE IDENTIFIER.
   MODULE PROCEDURE SET_ALTIMETER_OBSERVATION_FILE
END INTERFACE
PUBLIC SET_ALTIMETER_OBSERVATION_FILE

INTERFACE SET_SPECTRAL_OBSERVATION_FILE           !! SETS OBSERVATION FILE IDENTIFIER.
   MODULE PROCEDURE SET_SPECTRAL_OBSERVATION_FILE
END INTERFACE
PUBLIC SET_SPECTRAL_OBSERVATION_FILE


INTERFACE SET_ASSI_MAP_FILE
   MODULE PROCEDURE SET_ASSI_MAP_FILE
END INTERFACE
PUBLIC SET_ASSI_MAP_FILE

INTERFACE SET_ASSI_SPECTRA_FILE
   MODULE PROCEDURE SET_ASSI_SPECTRA_FILE
END INTERFACE
PUBLIC SET_ASSI_SPECTRA_FILE

INTERFACE PRINT_ASSIMILATION_STATUS            !! PRINTS MODULE STATUS.
   MODULE PROCEDURE PRINT_ASSIMILATION_STATUS
END INTERFACE
PUBLIC PRINT_ASSIMILATION_STATUS

INTERFACE PREPARE_ASSIMILATION             !! PREPARE ASSIMILATION.
   MODULE PROCEDURE PREPARE_ASSIMILATION
END INTERFACE
PUBLIC PREPARE_ASSIMILATION

INTERFACE PREPARE_SPECTRAL_ASSIMILATION             !! PREPARE ASSIMILATION.
   MODULE PROCEDURE PREPARE_SPECTRAL_ASSIMILATION
END INTERFACE
PUBLIC PREPARE_SPECTRAL_ASSIMILATION

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ASSIMILATION_OPTION_SPECTRA (AS_SPEC, CD, SC, CC, MP, LT, AL, F_CUT) !! SA2025

INTEGER, INTENT(IN)           :: AS_SPEC  !! ASSIMILATION FLAG
                                          !!  = 1  FOR ASSIMILATION
                                          !! OTHERWISE NO ASSIMILATION
INTEGER,    INTENT(IN), OPTIONAL :: CD    !! CORRELATION DISTANCE IN METERS
REAL,       INTENT(IN), OPTIONAL :: SC    !! CORRELATION DISTANCE COEFFICIENT
REAL,       INTENT(IN), OPTIONAL :: CC    !! CROSS CORRELATION COEFFICIENT 
INTEGER,    INTENT(IN), OPTIONAL :: MP    !! MAXIMUM NUMBER OF PARTITIONINGS 
REAL,       INTENT(IN), OPTIONAL :: LT    !! FACTOR FOR THRESHOLD CHECK
REAL,       INTENT(IN), OPTIONAL :: AL    !! MINIMUM DISTANCE BETWEEN TWO WAVE
!                                            SYSTEMS IN K-SPACE FOR COMBINING
!                                            MODEL AND OBSERVATIONS PEAKS.
REAL,       INTENT(IN), OPTIONAL :: F_CUT  !! CUTOFF FREQUENCY      
                              
IF (AS_SPEC.EQ.1) THEN
   IASSI_SPECTRA = 1   !! SA2025
ELSE
   IASSI_SPECTRA = 0   !! SA2025
END IF

IF (AS_SPEC.NE.1) RETURN

IF (.NOT.PRESENT(CD) .OR. .NOT. PRESENT(SC) .OR. .NOT. PRESENT(CC) .OR. .NOT. PRESENT(MP) & 
&   .OR. .NOT. PRESENT(LT) .OR. .NOT. PRESENT(AL) .OR. .NOT. PRESENT(F_CUT))     THEN
   WRITE(IU06,*) ' *******************************************************'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *FATAL ERROR IN SUB. SET_ASSIMILATION_OPTION_SPECTRA  *'
   WRITE(IU06,*) ' *=====================================================*'
   WRITE(IU06,*) ' * A SPECTRAL ASSIMILATION RUN IS REQUESTED BUT        *'
   WRITE(IU06,*) ' * CORRELATION DISTANCE(SARCORDIA)                     *'
   WRITE(IU06,*) ' * AND / OR                                            *'
   WRITE(IU06,*) ' * CORRELATION DISTANCE COEFFICIENT(PPSARCF)           *'
   WRITE(IU06,*) ' * AND / OR                                            *'
   WRITE(IU06,*) ' * CROSS CORRELATION COEFFICIENT(COEFPHI)              *' 
   WRITE(IU06,*) ' * AND / OR                                            *'
   WRITE(IU06,*) ' * MAXIMUM NUMBER OF PARTITIONINGS(MPARTSW)            *'
   WRITE(IU06,*) ' * AND / OR                                            *'
   WRITE(IU06,*) ' * FACTOR FOR THRESHOLD CHECK(LOWEST)                  *'
   WRITE(IU06,*) ' * AND / OR                                            *'
   WRITE(IU06,*) ' * MINIMUM DISTANCE FOR PEAK COMBINING(ALEVEL)         *'   
   WRITE(IU06,*) ' * AND / OR                                            *'    
   WRITE(IU06,*) ' * CUTOFF FREQUENCY(FCUT)                              *'    
   WRITE(IU06,*) ' * IS NOT DEFINED IN WAM_USER FILE                     *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS              *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *******************************************************'
   CALL ABORT1
END IF

SARCORDIA = CD
PPSARCF   = SC
COEFPHI   = CC
MPARTSW   = MP
LOWEST    = LT
ALEVEL    = AL
FCUT      = F_CUT
END SUBROUTINE SET_ASSIMILATION_OPTION_SPECTRA


SUBROUTINE SET_ASSIMILATION_PERIOD_SPECTRA (C, D, STEP_SPEC)

CHARACTER*14, INTENT(IN)   :: C     !! START DATE OF ASSIMILATION.
CHARACTER*14, INTENT(IN)   :: D     !! END DATE OF ASSIMILATION.
INTEGER,      INTENT(IN)   :: STEP_SPEC  !!MILATION TIMSTEP [S].

CDATSA  = C
CDATSE  = D
IDELSPE = STEP_SPEC

END SUBROUTINE SET_ASSIMILATION_PERIOD_SPECTRA


SUBROUTINE SET_ASSIMILATION_OPTION_ALTIMETER (AS, RA, SO, SM) !! SA2025

INTEGER, INTENT(IN)           :: AS  !! ASSIMILATION FLAG
                                     !!  = 1  FOR ASSIMILATION
                                     !! OTHERWISE NO ASSIMILATION
REAL,    INTENT(IN), OPTIONAL :: RA  !! RADIUS OF INFLUENCE IN DEGREES
REAL,    INTENT(IN), OPTIONAL :: SO  !! OBSERVATION SCATTER
REAL,    INTENT(IN), OPTIONAL :: SM  !! MODEL SCATTER
                                    
IF (AS.EQ.1) THEN
   IASSI_ALTIMETER = 1   !! SA2025
ELSE
   IASSI_ALTIMETER = 0   !! SA2025
END IF

IF (AS.NE.1) RETURN

IF (.NOT.PRESENT(RA) .OR. .NOT. PRESENT(SO) .OR. .NOT. PRESENT(SM)) THEN
   WRITE(IU06,*) ' *******************************************************'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *FATAL ERROR IN SUB. SET_ASSIMILATION_OPTION_ALTIMETER*'
   WRITE(IU06,*) ' *=====================================================*'
   WRITE(IU06,*) ' * AN ASSIMILATION RUN IS REQUESTED BUT                *'
   WRITE(IU06,*) ' * RADIUS OF INFLUENCE                                 *'
   WRITE(IU06,*) ' * AND / OR                                            *'
   WRITE(IU06,*) ' * OBSERVATION SCATTER                                 *'
   WRITE(IU06,*) ' * AND / OR                                            *'
   WRITE(IU06,*) ' * MODEL SCATTER                                       *'
   WRITE(IU06,*) ' * IS NOT DEFINED IN WAM_USER FILE                     *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS              *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *******************************************************'
   CALL ABORT1
END IF

DIST   = RA*RAD
SIGOBS = SO
SIGMOD = SM

END SUBROUTINE SET_ASSIMILATION_OPTION_ALTIMETER

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ASSIMILATION_OUTPUT (P)

LOGICAL, INTENT(IN)   :: P  !! FIRST GUESS OUTPUT FLAG.

FG_OUTPUT = P

END SUBROUTINE SET_ASSIMILATION_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ASSIMILATION_PERIOD (B, E, STEP)

CHARACTER*14, INTENT(IN)   :: B     !! START DATE OF ASSIMILATION.
CHARACTER*14, INTENT(IN)   :: E     !! END DATE OF ASSIMILATION.
INTEGER,      INTENT(IN)   :: STEP  !! ASSIMILATION TIMSTEP [S].

CDATAA  = B
CDATAE  = E
IDELASS = STEP

END SUBROUTINE SET_ASSIMILATION_PERIOD

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ALTIMETER_OBSERVATION_FILE (NAME, UNIT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

CHARACTER, INTENT(IN)           :: NAME*(*)  !! FILE NAME
INTEGER,   INTENT(IN), OPTIONAL :: UNIT      !! UNIT NUMBER

! ---------------------------------------------------------------------------- !

LENT = LEN_TRIM(NAME)
IF (LENT.GT.0) FILE80 = NAME(1:LENT)
IF (PRESENT(UNIT)) IU80 = UNIT

END SUBROUTINE SET_ALTIMETER_OBSERVATION_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_SPECTRAL_OBSERVATION_FILE (NAME_S, UNIT_S)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

CHARACTER, INTENT(IN)           :: NAME_S*(*)  !! FILE NAME
INTEGER,   INTENT(IN), OPTIONAL :: UNIT_S      !! UNIT NUMBER

! ---------------------------------------------------------------------------- !

LENT_S = LEN_TRIM(NAME_S)
IF (LENT_S.GT.0) FILE90 = NAME_S(1:LENT_S)
IF (PRESENT(UNIT_S)) IU90 = UNIT_S

END SUBROUTINE SET_SPECTRAL_OBSERVATION_FILE


! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ASSI_MAP_FILE (NAME, UNIT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

CHARACTER, INTENT(IN)           :: NAME*(*)  !! FILE NAME
INTEGER,   INTENT(IN), OPTIONAL :: UNIT      !! UNIT NUMBER

! ---------------------------------------------------------------------------- !

LENT = LEN_TRIM(NAME)
IF (LENT.GT.0) FILE30 = NAME(1:LENT)
IF (PRESENT(UNIT)) IU30 = UNIT

END SUBROUTINE SET_ASSI_MAP_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_ASSI_SPECTRA_FILE (NAME, UNIT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

CHARACTER, INTENT(IN)           :: NAME*(*)  !! FILE NAME
INTEGER,   INTENT(IN), OPTIONAL :: UNIT      !! UNIT NUMBER

! ---------------------------------------------------------------------------- !

LENT = LEN_TRIM(NAME)
IF (LENT.GT.0) FILE35 = NAME(1:LENT)
IF (PRESENT(UNIT)) IU35 = UNIT

END SUBROUTINE SET_ASSI_SPECTRA_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_ASSIMILATION_STATUS

LOGICAL       :: OPND

WRITE(IU06,*) '  '
WRITE(IU06,*) ' ------------------------------------------------- '
WRITE(IU06,*) '              MODEL ASSIMILATION STATUS:'
WRITE(IU06,*) ' ------------------------------------------------- '

WRITE(IU06,*) '  '
IF (IASSI_ALTIMETER.NE.1) THEN   !! SA2025
   WRITE(IU06,*) ' MODEL RUNS WITHOUT ALTIMETER ASSIMILATION'
   WRITE(IU06,*) '  '
ELSE
   WRITE(IU06,*) ' MODEL RUNS WITH ALTIMETER ASSIMILATION'
   
   WRITE(IU06,*) ' RADIUS OF INFLUENCE................: ', DIST*DEG,' DEG'
   WRITE(IU06,*) ' OBSERVATION SCATTER................: ', SIGOBS
   WRITE(IU06,*) ' MODEL SCATTER......................: ', SIGMOD
   WRITE(IU06,*) '  '
   WRITE(IU06,*) ' START DATE (YYYYMMDDHHMMSS) IS.....: ', CDATAA
   WRITE(IU06,*) ' END   DATE (YYYYMMDDHHMMSS) IS.....: ', CDATAE
   WRITE(IU06,*) ' DATE OF NEXT ASSIMILATION   IS.....: ', CDTASS
   WRITE(IU06,*) ' ASSIMILATION TIME STEP ............: ', IDELASS,' SECS'
      LENT = LEN_TRIM(FILE80)
   WRITE (IU06,'('' ALTIMETER OBSERVATION FILE .. UNIT:'',I3,'', ID .: '',A)')             &
   &                                                         IU80, FILE80(1:LENT)
   INQUIRE (UNIT=IU80, OPENED=OPND)
   IF (OPND) THEN
      WRITE(IU06,*) '  FILE IS ASSIGNED'
   ELSE
      WRITE(IU06,*) '  FILE IS NOT ASSIGNED'
   END IF
   WRITE(IU06,*) '  '

   IF ((FFLAG20 .OR. PFLAG20) .AND. FG_OUTPUT) THEN
      WRITE(IU06,*) '  OUTPUT OF FIRST GUESS INTEGRATED PARAMETERS'
      WRITE(IU06,*) ' TO PRINTER AND/OR FILE: ', TRIM(FILE30),'YYYYMMDDHHMMSS'
   ELSE
      WRITE(IU06,*) '  OUTPUT OF FIRST GUESS INTEGRATED PARAMETERS IS NOT REQUESTED'
   END IF
   WRITE(IU06,*) '  '

   IF  ((FFLAG25 .OR. PFLAG25) .AND. FG_OUTPUT) THEN
      WRITE(IU06,*) '  OUTPUT OF FIRST GUESS SPECTRA'
      WRITE(IU06,*) ' TO PRINTER AND/OR FILE : ', TRIM(FILE35),'YYYYMMDDHHMMSS'
   ELSE
      WRITE(IU06,*) '  OUTPUT OF FIRST GUESS SPECTRA IS NOT REQUESTED'
   END IF
   WRITE(IU06,*) '  '
END IF

!---------------------Spectral Assimilation---------------------!

WRITE(IU06,*) '  '
IF (IASSI_SPECTRA.NE.1) THEN       !! SA2025
   WRITE(IU06,*) ' MODEL RUNS WITHOUT SPECTRAL ASSIMILATION'
   WRITE(IU06,*) '  '
ELSE
   WRITE(IU06,*) ' MODEL RUNS WITH SPECTRAL ASSIMILATION'

   WRITE(IU06,*) ' MAX CORRELATION DISTANCE...........: ', SARCORDIA,' M'
   WRITE(IU06,*) ' MAX INFLUENCE DISTANCE.............: ', PPSARCF*SARCORDIA, ' M'
   WRITE(IU06,*) ' PHI COEFFICIENT....................: ', COEFPHI
   WRITE(IU06,*) ' MAX NUMBER OF PARTITIONINGS........: ', MPARTSW
   WRITE(IU06,*) ' THRESHHOLD FACTOR..................: ', LOWEST
   WRITE(IU06,*) ' MIN DIST. BETWEEN WAVE SYSTEMS (k).: ', ALEVEL
   WRITE(IU06,*) ' CUTOFF FREQUENCY...................: ', FCUT
   WRITE(IU06,*) '  '
   WRITE(IU06,*) ' START DATE (YYYYMMDDHHMMSS) IS.....: ', CDATSA
   WRITE(IU06,*) ' END   DATE (YYYYMMDDHHMMSS) IS.....: ', CDATSE
   WRITE(IU06,*) ' DATE OF NEXT ASSIMILATION   IS.....: ', CDTSPS
   WRITE(IU06,*) ' ASSIMILATION TIME STEP ............: ', IDELSPE,' SECS'

   LENT_S = LEN_TRIM(FILE90)
   WRITE (IU06,'('' SPECTRA OBSERVATION FILE .. UNIT:'',I3,'', ID .: '',A)')    &
   &                                                         IU90, FILE90(1:LENT_S)
   INQUIRE (UNIT=IU90, OPENED=OPND)
   IF (OPND) THEN
      WRITE(IU06,*) ' SPECTRA  FILE IS ASSIGNED'
   ELSE
      WRITE(IU06,*) ' SPECTRA FILE IS NOT ASSIGNED'
   END IF
   WRITE(IU06,*) '  '
END IF

END SUBROUTINE PRINT_ASSIMILATION_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_SPECTRAL_ASSIMILATION
! ---------------------------------------------------------------------------- !
! ---------------------------------------------------------------------------- !
! ------------------------- SPECTRAL ASSIMILATION ---------------------------- !
!! SA2025
 
CDTSPS = CDATSA
DO WHILE (CDTSPS.LT.CDTPRO)
   CALL INCDATE(CDTSPS,IDELSPE)
END DO


IF (CDTSPS.GT.CDATSE) THEN
   WRITE(IU06,*) ' *******************************************************'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *     FATAL ERROR IN SUB. PREPARE_ASSIMILATION        *'
   WRITE(IU06,*) ' *     ========================================        *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' * DATES DO NOT MATCH IN SPECTRAL ASSIMILATION         *'
   WRITE(IU06,*) ' * START DATE OF SPECTRAL ASSIMILATION IS: ', CDATSA
   WRITE(IU06,*) ' * END   DATE OF SPECTRAL ASSIMILATION IS: ', CDATSE
   WRITE(IU06,*) ' * MODEL DATE OF                       IS: ', CDTPRO
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS              *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *******************************************************'
   CALL ABORT1
ENDIF
!! SA2025 
END SUBROUTINE PREPARE_SPECTRAL_ASSIMILATION

SUBROUTINE PREPARE_ASSIMILATION

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER :: J
REAL    :: DELLON

CDTASS = CDATAA
DO WHILE (CDTASS.LT.CDTPRO)
   CALL INCDATE(CDTASS,IDELASS)
END DO

IF (CDTASS.GT.CDATAE) THEN
   WRITE(IU06,*) ' *******************************************************'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *     FATAL ERROR IN SUB. PREPARE_ASSIMILATION        *'
   WRITE(IU06,*) ' *     ========================================        *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' * DATES DO NOT MATCH:                                 *'
   WRITE(IU06,*) ' * START DATE OF ASSIMILATION IS: ', CDATAA
   WRITE(IU06,*) ' * END   DATE OF ASSIMILATION IS: ', CDATAE
   WRITE(IU06,*) ' * MODEL DATE OF              IS: ', CDTPRO
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS              *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *******************************************************'
   CALL ABORT1
ENDIF

IF (SIGOBS .GT. 1.0 .OR. SIGMOD .GT. 1.0 .OR.                                &
&       SIGOBS .LT. 0.1 .OR. SIGMOD .LT. 0.1 ) THEN
   WRITE(IU06,*) ' *******************************************************'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *     FATAL ERROR IN SUB. PREPARE_ASSIMILATION        *'
   WRITE(IU06,*) ' *     ========================================        *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' * CONSTRAINTS:                                        *'
   WRITE(IU06,*) ' * 0.1 < SIGOBS <= 1.0  CURRENT SETTING: ', SIGOBS
   WRITE(IU06,*) ' * 0.1 < SIGMOD <= 1.0  CURRENT SETTING: ', SIGMOD
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS              *'
   WRITE(IU06,*) ' *                                                     *'
   WRITE(IU06,*) ' *******************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE SEARCH DISTANCE.                                              !
!        ------------------------                                              !

ALLOCATE (LLON(NY))                        !! SEARCH DISTANCE ON LATITUDES.
IF (SPHERICAL_RUN) THEN
   DO J = 1,NY
!AB      DELLON = ACOS((COS(DIST*2.)-SINPH(J)**2)/COSPH(J)**2)
      DELLON = (COS(DIST*2.)-SINPH(J)**2)/COSPH(J)**2
      DELLON = MAX(DELLON, -1.)
      DELLON = MIN(DELLON, +1.)
      DELLON = ACOS(DELLON) 
      LLON(J) = NINT(DEG*DELLON/REAL(ZDELLO(J))*REAL(M_DEGREE))
   END DO
ELSE
   LLON(:) = NINT(DEG*DIST*2./REAL(ZDELLO(:))*REAL(M_DEGREE))
ENDIF
LLON = MIN(LMAX,LLON)
IF (IPER) LLON = MIN(LLON,NLON_RG/2-1)

LLAT = NINT(DEG*DIST*2./REAL(XDELLA)*REAL(M_DEGREE)) !! SEARCH DISTANCE ON LONGITUDES.
LLAT = MIN(LMAX,LLAT)

NDIM2 = (2*MAX(LLAT,MAXVAL(LLON)) +1)**2    !! DIMENSION OF OI ARRAY

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.  IF FIRST GUEST OUTPUT, OPEN FIRST OUTPUT FILES.                      !
!        ------------------------------------------------                      !

IF (FG_OUTPUT) CALL SAVE_OUTPUT_FILES (IU30, FILE30, IU35, FILE35)

END SUBROUTINE PREPARE_ASSIMILATION

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PRIVATE MODULE PROCEDURES.                                            !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_ASSI_SET_UP_MODULE
