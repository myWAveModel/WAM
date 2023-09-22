MODULE WAM_SOURCE_OUTPUT_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS ALL VARIABLES, CONSTANTS AND PROCEDURES FOR THE       !
!   OUTPUT OF SOURCE FUNCTION.                                                 !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,     ONLY:  &
&             ABORT1,              &  !! TERMINATES PROCESSING.
&             INCDATE,             &  !! UPDATES DATE/TIME GROUP.
&             OPEN_FILE,           &  !! OPENS A FILE
&             PRINT_ARRAY             !! PRINTS AN ARRAY

USE WAM_INTERFACE_MODULE,   ONLY:  &
&       TOTAL_ENERGY,              &  !! COMPUTATION OF TOTAL ENERGY.
&       TM1_TM2_PERIODS,           &  !! COMPUTATION OF MEAN PERIODE.
&       WM1_WM2_WAVENUMBER,        &  !! COMPUTATION OF MEAN WAVENUMBER.
&       PEAK_DIRECTION                !! COMPUTATION OF PEAK DIRECTION.

USE WAM_ICE_MODULE,         ONLY:  &
&             PUT_ICE                 !! PUTS ICE INDICATOR INTO DATA FILED.

USE WAM_TOPO_MODULE,        ONLY:  &
&             PUT_DRY                 !! PUTS DRY INDICATOR INTO DATA FILED.

USE wam_mpi_comp_module,        ONLY:  &
&             MPI_GATHER_BLOCK

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE, ONLY: ZPI, DEG
USE WAM_FILE_MODULE,    ONLY: IU06, ITEST, IU28, FILE28
USE WAM_FRE_DIR_MODULE, ONLY: DFIM
USE WAM_GRID_MODULE,    ONLY: NX, NY, NSEA, AMOWEP, AMOSOP, AMOEAP, AMONOP,    &
&                             L_S_MASK
USE WAM_MODEL_MODULE,   ONLY: INDEP
USE WAM_OUTPUT_SET_UP_MODULE, ONLY: IDEL_OUT

USE WAM_TIMOPT_MODULE,  ONLY: CDATEE, CDTPRO, CDTSOU, IDELT, COLDSTART,        &
&                             SHALLOW_RUN, WAVE_BREAKING_RUN, PHILLIPS_RUN,    &
&                             cdatea

use wam_mpi_module,           only: petotal, irank, nstart, nend,              &
&                                   klentop, klenbot, mpmaxlength,             &
&                                   nnext, nprevious, ninf, nsup,              &
&                                   extime, comtime, ijs=>nijs, ijl=>nijl

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE
include 'mpif.h'
PRIVATE

CHARACTER (LEN=14) , PARAMETER :: ZERO = ' '
INTEGER :: I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. FILE AND PRINT OUTPUT.                                                !
!        ----------------------                                                !

INTEGER, PARAMETER :: N_OUT = 10   !! MAXIMUM NUMBER OF OUTPUT PARAMETER.

LOGICAL :: FFLAG(N_OUT) = (/(.FALSE.,I=1,N_OUT)/) !! FILE OUTPUT FLAG FOR EACH
                                                  !! PARAMETER.
LOGICAL :: PFLAG(N_OUT) = (/(.FALSE.,I=1,N_OUT)/) !! PRINT OUTPUT FLAG FOR EACH
                                                  !! PARAMETER.
LOGICAL :: CFLAG(N_OUT) = (/(.FALSE.,I=1,N_OUT)/) !! = FFLAG .OR. PFLAG

INTEGER            :: DEL_SOURCE_OUT = 0  !! OUTPUT TIME INCREMENT.
CHARACTER (LEN=14) :: CDT_SCR_OUT = ZERO       !! NEXT DATE TO WRITE SOURCE OUTPUT.
PUBLIC CDT_SCR_OUT

INTEGER            :: DELFIL = 0       !! FILE TIME INCREMENT.
CHARACTER (LEN=14) :: CDTFIL = ZERO    !! NEXT DATE TO DISPOSE FILE.

CHARACTER(LEN=60), DIMENSION(N_OUT) :: TITL = (/                     &
& ' WIND INPUT PHILLIPS SOURCE TERM ( M*M/S )                  ',    &   !!  1
& ' WIND INPUT SOURCE TERM ( M*M/S )                           ',    &   !!  2
& ' NON-LINEAR SOURCE TERM ( M*M/S )                           ',    &   !!  3
& ' WHITE CAPPING DISSIPATION SOURCE TERM ( M*M/S )            ',    &   !!  4
& ' BOTTOM FRICTION DISSIPATION SOURCE TERM ( M*M/S )          ',    &   !!  5
& ' WAVE BREAKING DISSIPATION SOURCE TERM ( M*M/S )            ',    &   !!  6
& ' FRACTION OF BREAKING                                       ',    &   !!  7
& ' SIG. WAVE HEIGHT ( M )                                     ',    &   !!  8
& ' PEAK WAVE DIRECTION ( DEG )                                ',    &   !!  9
& ' MEAN WAVE LENGTH ( M )                                     '/)       !! 10

REAL, PARAMETER, DIMENSION(N_OUT) :: SCAL = (/                                 &
&                      0.            ,    &   !!  1
&                      0.            ,    &   !!  2
&                      0.            ,    &   !!  3
&                      0.            ,    &   !!  4
&                      0.            ,    &   !!  5
&                      0.            ,    &   !!  6
&                      0.            ,    &   !!  7
&                     10.            ,    &   !!  8
&                      1.            ,    &   !!  9
&                      1.            /)       !! 10

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. SOURCE FUNCTIONS INTEGRATED OVER FEQENCY AND DIRECTION.               !
!        -------------------------------------------------------               !

CHARACTER (LEN=14) :: CDT_SOURCE = ZERO !! DATE OF SOURCE.

REAL, ALLOCATABLE :: SOURCE_ARRAY(:,:)  !! INTEGRATED SOURCE VALUESE AT
                                        !! ALL SEA POINTS (FIRST INDEX).
                                        !! SECOND INDEX ARE DIFFERENT SOURCES.

REAL, SAVE         :: ZMISS=-999.       !! MISSING VALUE (ICE OR DRY POINT)

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE SAVE_SOURCE                          !! SAVES SOURCE FOR OUTPUT.
   MODULE PROCEDURE SAVE_SOURCE
END INTERFACE
PUBLIC :: SAVE_SOURCE

INTERFACE SET_SOURCE_OUTPUT_TIMES              !! SETS TIMES FOR COMPUTATION.
   MODULE PROCEDURE SET_SOURCE_OUTPUT_TIMES
END INTERFACE
PUBLIC SET_SOURCE_OUTPUT_TIMES

INTERFACE SET_SOURCE_OUTPUT_FILE               !! SETS FILE FOR SOURCE OUTPUT.
   MODULE PROCEDURE SET_SOURCE_OUTPUT_FILE
END INTERFACE
PUBLIC SET_SOURCE_OUTPUT_FILE

INTERFACE PRINT_SOURCE_OUTPUT_MODULE           !! PRINTS SOURCE OUTPUT SETTING.
   MODULE PROCEDURE PRINT_SOURCE_OUTPUT_MODULE
END INTERFACE
PUBLIC PRINT_SOURCE_OUTPUT_MODULE

INTERFACE PREPARE_SOURCE_OUTPUT               !! PREPARES SOURCE OUTPUT MODULE.
   MODULE PROCEDURE PREPARE_SOURCE_OUTPUT
END INTERFACE
PUBLIC PREPARE_SOURCE_OUTPUT

INTERFACE SOURCE_OUTPUT              !! OUTPUT OF GRIDDED SOURCE FUNCTIONS.
   MODULE PROCEDURE SOURCE_OUTPUT
END INTERFACE
PUBLIC SOURCE_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !


INTERFACE SAVE_SOURCE_FILE           !! SAVES AND OPENS OUTPUT FILE.
   MODULE PROCEDURE SAVE_SOURCE_FILE
END INTERFACE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SAVE_SOURCE (SL, ICASE)
use wam_oasis_module,     only: Wam_oasis_send_gcm,use_oasis_ocea_out
! ---------------------------------------------------------------------------- !
!                                                                              !
!   SAVE_SOURCE - SAVES SOURCE FUNCTION                                        !
!                                                                              !
!     H. GUNTHER         HZG         AUGUST 2011                               !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO SAVE THE DIFFERENT SOURCE FUNCTIONS.                                !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       SOURECE FUNCTIONS ARE INTEGRATED OVER DIRECTIONS AND FREQUENCIES       !
!       AND STORED IN THIS MODULE.                                             !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLE.                                                      !
!     -------------------                                                      !

REAL,    INTENT(IN) :: SL(:,:,:)  !! BLOCK OF SOURCE FUNCTION.
INTEGER, INTENT(IN) :: ICASE      !! SOURCE FUNCTION FLAG.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL    :: TEMP(SIZE(SL,1),SIZE(SL,3))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER FREQUENCIES AND DIRECTION.                             !
!        -----------------------------------------                             !

if(use_oasis_ocea_out.or.CDT_SCR_OUT.ne.' ')then
SELECT CASE (ICASE)
   CASE (1:6)
      TEMP = SUM(SL, DIM=2)
      SOURCE_ARRAY(ijs:ijl,ICASE) = MATMUL(TEMP, DFIM)
   CASE (7)
      SOURCE_ARRAY(ijs:ijl,ICASE) = SL(:,1,1)
   CASE (8:10)
      CALL TOTAL_ENERGY (SL, SOURCE_ARRAY(ijs:ijl,8))
      CALL PEAK_DIRECTION (SL, SOURCE_ARRAY(ijs:ijl,9))
      SOURCE_ARRAY(ijs:ijl,9) = SOURCE_ARRAY(ijs:ijl,9)*DEG

      IF (SHALLOW_RUN) THEN
         CALL WM1_WM2_WAVENUMBER (SL, SOURCE_ARRAY(ijs:ijl,8),                 &
&                             WM2=SOURCE_ARRAY(ijs:ijl,10), IN=INDEP(ijs:ijl))
      ELSE
         CALL WM1_WM2_WAVENUMBER (SL, SOURCE_ARRAY(ijs:ijl,8),                 &
&                             WM2=SOURCE_ARRAY(ijs:ijl,10))
      END IF
      SOURCE_ARRAY(ijs:ijl,8) = 4.*SQRT(SOURCE_ARRAY(ijs:ijl,8))
      SOURCE_ARRAY(ijs:ijl,10) = ZPI/SOURCE_ARRAY(ijs:ijl,10)
   CASE DEFAULT

END SELECT

CDT_SOURCE = CDTSOU
CALL INCDATE (CDT_SOURCE,IDELT)
if(use_oasis_ocea_out.and.icase==6)call Wam_oasis_send_gcm(CDTPRO,SOURCE_ARRAY)
end if
END SUBROUTINE SAVE_SOURCE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_SOURCE_OUTPUT_TIMES (OUT_DEL, PF, FF)

INTEGER,           INTENT(IN) :: OUT_DEL    !! TIME INCREMENT FOR
                                            !! SOUREC OUTPUT.

LOGICAL, OPTIONAL, INTENT(IN) :: PF(N_OUT)  !! .TRUE. IF PRINTER OUTPUT.
LOGICAL, OPTIONAL, INTENT(IN) :: FF(N_OUT)  !! .TRUE. IF FILE OUTPUT.

! ---------------------------------------------------------------------------- !

DEL_SOURCE_OUT = OUT_DEL

PFLAG = .FALSE.
FFLAG = .FALSE.
IF (DEL_SOURCE_OUT.GE.0) THEN
   IF (PRESENT(PF)) PFLAG = PF
   IF (PRESENT(FF)) FFLAG = FF
END IF

CFLAG = FFLAG .OR. PFLAG

END SUBROUTINE SET_SOURCE_OUTPUT_TIMES

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_SOURCE_OUTPUT_FILE (FILE_INC, NAME, UNIT)

INTEGER,   INTENT(IN), OPTIONAL :: FILE_INC     !! TIME INCREMENT TO SAVE
                                                !! OUTPUT FILE. IF .LE. 0.
                                                !! FILE IS SAVED AT
                                                !! THE END OF THE RUN
CHARACTER,  INTENT(IN), OPTIONAL :: NAME*(*)    !! SOURCE FILE NAME
INTEGER,    INTENT(IN), OPTIONAL :: UNIT        !! LOGICAL FILE UNIT NO.

! ---------------------------------------------------------------------------- !

DELFIL   = 0
IF (PRESENT(FILE_INC)) DELFIL = MAX(FILE_INC, 0)

FILE28 = 'SRC'
IF (PRESENT(NAME)) THEN
   IF (LEN_TRIM(NAME).GT.0) FILE28 = TRIM(NAME)
END IF

IU28 = 28
IF (PRESENT(UNIT)) THEN
   IF (UNIT.GT.0) IU28 = UNIT
END IF

END SUBROUTINE SET_SOURCE_OUTPUT_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_SOURCE_OUTPUT_MODULE

INTEGER :: I

WRITE (IU06,*) '  '
WRITE (IU06,*) ' ------------------------------------------------'
WRITE (IU06,*) '    MODEL SPECIFICATIONS FOR SOURCE OUTPUT:'
WRITE (IU06,*) ' ------------------------------------------------'
WRITE (IU06,*) '  '

IF (DEL_SOURCE_OUT.LE.0.) THEN
   WRITE(IU06,*) ' OUTPUT OF SOURCE FUNCTIONS IS NOT REQUESTED '
   RETURN
END IF

WRITE(IU06,*) ' OUTPUT OF SOURCE FUNCTIONS EVERY ..: ', DEL_SOURCE_OUT, ' [S]'
WRITE(IU06,*) ' THE NEXT OUTPUT DATE IS ...........: ', CDT_SCR_OUT
IF (ANY(PFLAG)) WRITE(IU06,*) ' OUTPUT IS PRINTED'
IF (ANY(FFLAG)) THEN
   WRITE(IU06,*) ' OUTPUT IS WRITTEN TO FILE'
   WRITE(IU06,*) ' FILE ID IS ........................: ', TRIM(FILE28)
   WRITE(IU06,*) ' UNIT IS............................: ', IU28
   IF (DELFIL.GT.0.) THEN
      WRITE(IU06,*) ' A NEW FILE WILL BE USED EVERY .....: ', DELFIL, ' [S]'
      WRITE(IU06,*) ' THE PRESENT FILE DATE .............: ', CDTFIL
   ELSE
      WRITE(IU06,*) ' THE FILE DATE IS AT END OF RUN.....: ', CDTFIL
   END IF
END IF
WRITE(IU06,*) '  '
WRITE(IU06,*) ' LIST OF OUTPUT PARAMETERS TO BE GENERATED:'
WRITE(IU06,*) '  '
DO I = 1,N_OUT
   IF (CFLAG(I))  THEN
        WRITE(IU06,'(1X,A60,'' SCALED BY '', F10.3, 2L3)') TITL(I), SCAL(I),PFLAG(I),FFLAG(I)
    END IF
END DO

WRITE(IU06,*) '  '
IF (CDT_SOURCE.NE.ZERO) THEN
   WRITE(IU06,*) ' DATE OF SOURCE FUNCTIONS STORED IN MODULE IS: ', CDT_SOURCE
ELSE
   WRITE(IU06,*) ' SOURCE FUNCTIONS ARE NOT STORED IN MODULE'
END IF
WRITE(IU06,*) '  '

END SUBROUTINE PRINT_SOURCE_OUTPUT_MODULE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_SOURCE_OUTPUT
use wam_oasis_module,     only: use_oasis_ocea_out,Wam_oasis_send_gcm

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. IF TIME INCREMENTS = 0: SET TO DEFAULT.                               !
!        ---------------------------------------                               !


IF (DEL_SOURCE_OUT.EQ. 0) DEL_SOURCE_OUT = IDELT
IF (DELFIL        .LE. 0) DELFIL         = MAX(IDEL_OUT,0)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. OUTPUT REQUESTED?                                                     !
!        ------------------                                                    !

IF (DEL_SOURCE_OUT.LE.0 .OR. .NOT. ANY(CFLAG)) THEN
   CDT_SOURCE = ' '
   DEL_SOURCE_OUT = -1
   PFLAG = .FALSE.
   FFLAG = .FALSE.
   CFLAG = .FALSE.
   if(use_oasis_ocea_out.and.coldstart)then
     IF (ALLOCATED(SOURCE_ARRAY )) DEALLOCATE(SOURCE_ARRAY)
     ALLOCATE (SOURCE_ARRAY(1:NSEA,1:N_OUT))
     SOURCE_ARRAY = 0.
     call Wam_oasis_send_gcm(cdatea,SOURCE_ARRAY)
     endif
   RETURN
END IF

IF (DEL_SOURCE_OUT.GT.0) THEN
   IF (.NOT.PHILLIPS_RUN) THEN
      PFLAG(1) = .FALSE.
      FFLAG(1) = .FALSE.
   END IF
   IF (.NOT.SHALLOW_RUN) THEN
      PFLAG(5) = .FALSE.
      FFLAG(5) = .FALSE.
   END IF
   IF (.NOT.WAVE_BREAKING_RUN) THEN
      PFLAG(6) = .FALSE.
      FFLAG(6) = .FALSE.
   END IF
   CFLAG = PFLAG .OR. FFLAG
   IF (.NOT.ANY(CFLAG)) THEN
      WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(IU06,*) '+                                                       +'
      WRITE(IU06,*) '+     WARNING ERROR IN SUB. PREPARE_SOURCE_OUTPUT       +'
      WRITE(IU06,*) '+     ===========================================       +'
      WRITE(IU06,*) '+                                                       +'
      WRITE(IU06,*) '+  OUTPUT FLAGS DO NOT FIT TO ACTIVE SOURCE FUNCTIONS   +'
      WRITE(IU06,*) '+                                                       +'
      WRITE(IU06,*) '+     SOURCE OUTPUT ARE NOT PROCESSED.                  +'
      WRITE(IU06,*) '+                                                       +'
      WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      DEL_SOURCE_OUT = 0.
      CDT_SOURCE     = ' '
   RETURN
   END IF
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. CHECK INCREMENTS.                                                     !
!        -----------------                                                     !

IF (((DEL_SOURCE_OUT/IDELT)*IDELT).NE.DEL_SOURCE_OUT) THEN
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
   WRITE(IU06,*) '+                                                       +'
   WRITE(IU06,*) '+     WARNING ERROR IN SUB. PREPARE_SOURCE.             +'
   WRITE(IU06,*) '+     =====================================             +'
   WRITE(IU06,*) '+                                                       +'
   WRITE(IU06,*) '+  OUTPUT INCREMENT OF SOURCEFUNCTIONS IS NOT A         +'
   WRITE(IU06,*) '+  MULTIPLE OF SOURCE FUNCTION TIME STEP.               +'
   WRITE(IU06,*) '+  OUTPUT INCREMENT CHANGED.                            +'
   WRITE(IU06,*) '+  OLD OUTPUT INCREMENT WAS : ' ,DEL_SOURCE_OUT
   DEL_SOURCE_OUT = MAX(((DEL_SOURCE_OUT/IDELT)*IDELT), IDELT)
   WRITE(IU06,*) '+  NEW OUTPUT INCREMENT IS  : ' ,DEL_SOURCE_OUT
   WRITE(IU06,*) '+                                                       +'
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
END IF

IF (ANY(FFLAG)) THEN
   IF (((DELFIL/DEL_SOURCE_OUT)*DEL_SOURCE_OUT).NE.DELFIL) THEN
      WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(IU06,*) '+                                                       +'
      WRITE(IU06,*) '+     WARNING ERROR IN SUB. PREPARE_SOURCE.             +'
      WRITE(IU06,*) '+     =====================================             +'
      WRITE(IU06,*) '+                                                       +'
      WRITE(IU06,*) '+  FILE INCREMENT OF SOURCE OUTPUT IS NOT A             +'
      WRITE(IU06,*) '+  MULTIPLE OF OUTPUT INCREMENT.                        +'
      WRITE(IU06,*) '+  FILE INCREMENT CHANGED.                              +'
      WRITE(IU06,*) '+  OLD FILE INCREMENT WAS : ' ,DELFIL
      DELFIL = MAX(((DELFIL/DEL_SOURCE_OUT)*DEL_SOURCE_OUT), DEL_SOURCE_OUT)
      WRITE(IU06,*) '+  NEW FILE INCREMENT IS  : ' ,DELFIL
      WRITE(IU06,*) '+                                                       +'
      WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
   END IF
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. INITIAL.                                                              !
!        --------                                                              !

IF (ALLOCATED(SOURCE_ARRAY )) DEALLOCATE(SOURCE_ARRAY)
ALLOCATE (SOURCE_ARRAY(1:NSEA,1:N_OUT))

SOURCE_ARRAY = 0.

CDT_SOURCE = ' '     !! DATE OF SOURCE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. TIME COUNTER FOR NEXT OUTPUT.                                         !
!        -----------------------------                                         !

CDT_SCR_OUT = ' '
IF (DEL_SOURCE_OUT .GT. 0.) THEN
   CDT_SCR_OUT = CDTPRO
   CALL INCDATE (CDT_SCR_OUT, DEL_SOURCE_OUT)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. TIME COUNTER FOR NEXT OUTPUT FILES AND OPEN FIRST OUTPUT FILE.        !
!        --------------------------------------------------------------        !

CDTFIL = ' '
IF (ANY(FFLAG)) THEN
   IF (DELFIL.EQ.0.) THEN
      CDTFIL = CDATEE
   ELSE
      CDTFIL = CDTPRO
   END IF
   CALL SAVE_SOURCE_FILE
END IF

if(use_oasis_ocea_out.and.coldstart)call Wam_oasis_send_gcm(cdatea,SOURCE_ARRAY)

END SUBROUTINE PREPARE_SOURCE_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SOURCE_OUTPUT

REAL     :: GRID(NX,NY)
INTEGER  :: irecv

REAL     :: TOTAL(SIZE(SOURCE_ARRAY,1))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. WRITE OUTPUT.                                                         !
!        -------------                                                         !

irecv = petotal         !! output will be written by the last pe

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.1 GATHER PARAMETER BLOCKS ON ONE PROCESSOR.                            !
!         -----------------------------------------                            !

irecv = petotal         !! output will be written by the last pe

IF (irank==irecv) then
   IF (ANY(FFLAG)) THEN
      WRITE (IU28) CDT_SOURCE, REAL(NX), REAL(NY),                             &
&                  AMOWEP, AMOSOP, AMOEAP, AMONOP, cdatea
      WRITE (IU28) FFLAG
   END IF
END IF

DO I = 1, N_OUT
   IF (.NOT.CFLAG(I)) CYCLE
   call mpi_gather_block (irecv, SOURCE_ARRAY(ijs:ijl,I), SOURCE_ARRAY(:,I))
   IF (irank==irecv) then
      GRID = UNPACK (SOURCE_ARRAY(:,I), L_S_MASK, ZMISS)
      IF (FFLAG(I)) WRITE (IU28) GRID
      IF (PFLAG(I)) CALL PRINT_ARRAY (IU06, CDT_SOURCE, TITL(I), GRID,         &
&                       AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(I))
   END IF
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. UP-DATE OUTPUT TIME.                                                  !
!        --------------------                                                  !

CALL INCDATE (CDT_SCR_OUT, DEL_SOURCE_OUT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!    3. SAVE OUTPUT FILE.                                                      !
!        ----------------                                                      !

IF (CDTFIL.EQ.CDTPRO) CALL SAVE_SOURCE_FILE

END SUBROUTINE SOURCE_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVATE MODULE PROCEDURES.                                            !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SAVE_SOURCE_FILE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SAVE_SOURC_FILE - TO SAVE SOURCE OUTPUT FILES.                             !
!                                                                              !
!     HEINZ GUNTHER       GKSS  JANUARY   2005                                 !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO CLOSE ASSIGNED SOURCE OUTPUT FILE AND OPEN NEW A FILE               !
!       IF NECCESSARY.                                                         !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
!                                                                              !
!       ABORT1    - TERMINATES PROCESSING.                                     !
!       OPEN_FILE - OPEN A NEW FILE.                                           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!        ASSIGNED FILES ARE CLOSED AND NEW FILES ARE OPENED IF THE             !
!        MODEL RUN IS NOT FINISHED.                                            !
!                                                                              !
!        THE NEW FILE DATE IS INCEREMENTED.                                    !
!        THE FILE NAME CONVENTION IS EXPLAINED IN SUB OPENFIL.                 !
!                                                                              !
!     REFERENCES.                                                              !
!     -----------                                                              !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTEGER           :: IFAIL
CHARACTER*7, SAVE :: STAT='UNKNOWN'

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. SAVE THE OLD SOURCE FILE.                                             !
!        -------------------------                                             !

CLOSE(UNIT=IU28,STATUS='KEEP')

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. OPEN A NEW SOURCE FILE.                                               !
!        -----------------------                                               !

IF (CDTPRO.LT.CDATEE) THEN
   CALL INCDATE (CDTFIL, DELFIL)
   CALL OPEN_FILE (IU06, IU28, FILE28, CDTFIL, STAT, IFAIL)
   IF (IFAIL.NE.0) CALL ABORT1
END IF

END SUBROUTINE SAVE_SOURCE_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_SOURCE_OUTPUT_MODULE
