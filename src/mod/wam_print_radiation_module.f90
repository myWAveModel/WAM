MODULE WAM_PRINT_RADIATION_MODULE

! ---------------------------------------------------------------------------- !
!
!   THIS MODULE CONTAINS: OUTPUT TIMES, FLAGS, AND ARRAYS NECESSARY FOR GRIDDED
!                         FIELDS OF PARAMTERS.
!
! ---------------------------------------------------------------------------- !

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. PRINTER OUTPUT UNIT, FILE NAME AND TEST FLAGS.                        !
!        ----------------------------------------------                        !

INTEGER            :: IU06   = 66        !! UNIT FOR PRINTER OUTPUT.
CHARACTER (LEN=80) :: FILE06 = 'Radiation_Prot'

INTEGER      :: ITEST = 0                !! TEST OUTPUT LEVEL:
                                         !!   .LE. 0  NO OUTPUT
                                         !!   .GT. 0  OUTPUT TO FILE05

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. USER INPUT FILE UNIT AND NAME.                                        !
!        ------------------------------                                        !

INTEGER            :: IU05   = 55                !! INPUT OF USER DATA.
CHARACTER (LEN=80) :: FILE05 = 'Radiation_User'  !! (SUB READ_RADIATION_USER).

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. NEXT OUTPUT TIMES AND TIME INCREMENTS.                                !
!        --------------------------------------                                !

CHARACTER (LEN=14) :: CDATEA    !! START DATE OF PRINT OUPUT  (YYMMDDHHMM).
CHARACTER (LEN=14) :: CDATEE    !! END DATE OF PRINT OUPUT (YYMMDDHHMM).
INTEGER            :: IDELDO    !! PRINT OUTPUT TIMESTEP IN SECONDS.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. OUTPUT TIMES AS DEFINED IN INPUT FILE.                                !
!        --------------------------------------                                !

INTEGER                         :: NOUTT = 0 !! NUMBER OF OUTPUT TIMES.
CHARACTER (LEN=14), ALLOCATABLE :: COUTT(:)  !! OUTPUT TIMES.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. FILE NAME INFORMATION.                                                !
!        ----------------------                                                !

INTEGER            :: IU01 = 1       !! INPUT UNIT FOR INTEGRATED PARAMETER
CHARACTER (LEN=80) :: FILE01 = 'RAD' !! FILE IDENTIFIER
CHARACTER (LEN=3)  :: USERID         !! USERID FOR FILE NAMES.
CHARACTER (LEN=3)  :: RUNID          !! RUN IDENTIFIER FOR FILE NAMES.
CHARACTER (LEN=60) :: PATH           !! PATH NAME FOR FILES.
INTEGER            :: DELFIL = 0     !! FILE TIME INCREMENT.
CHARACTER (LEN=14) :: CDTFIL = ' '   !! NEXT DATE TO DISPOSE FILE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. FILE AND PRINT OUTPUT.                                                !
!        ----------------------                                                !

INTEGER, PARAMETER :: N_OUT = 6   !! MAXIMUM NUMBER OF OUTPUT PARAMETER.

LOGICAL :: FFLAG(N_OUT) = (/(.FALSE.,I=1,N_OUT)/) !! FILE INPUT FLAG FOR EACH
                                                  !! PARAMETER.
LOGICAL :: PFLAG(N_OUT) = (/(.FALSE.,I=1,N_OUT)/) !! PRINT OUTPUT FLAG FOR EACH
                                                  !! PARAMETER.

CHARACTER(LEN=60), DIMENSION(N_OUT) :: TITL = (/                     &
& ' RADIATION STRESS TENSOR SXX ( 0.1 M*M*M/S/S )              ',    &   !!  1
& ' RADIATION STRESS TENSOR SYY ( 0.1 M*M*M/S/S )              ',    &   !!  2
& ' RADIATION STRESS TENSOR SXY ( 0.1 M*M*M/S/S )              ',    &   !!  3
& ' DUMMY                                                      ',    &   !!  4
& ' X-COMP. RADIATION STRESS ( 0.00001 M/S/S )                 ',    &   !!  5
& ' Y-COMP. RADIATION STRESS ( 0.00001 M/S/S )                 '/)       !!  6

REAL, PARAMETER, DIMENSION(N_OUT) :: SCAL = (/                                 &
&                     10.            ,    &   !!  1
&                     10.            ,    &   !!  2
&                     10.            ,    &   !!  3
&                      1.            ,    &   !!  4
&                 100000.            ,    &   !!  5
&                 100000.            /)       !!  6

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. GENERAL GRID INFORMATION.                                             !
!        -------------------------                                             !

INTEGER  :: NX           !! NUMBER OF LONGITUDES IN GRID.
INTEGER  :: NY           !! NUMBER OF LATITUDES  IN GRID.
LOGICAL  :: PER          !! = .TRUE. IF GRID IS PERIODIC.
REAL     :: AMOWEP       !! MOST WESTERN LONGITUDE IN GRID [DEG].
REAL     :: AMOSOP       !! MOST SOUTHERN LATITUDE IN GRID [DEG].
REAL     :: AMOEAP       !! MOST EASTERN LONGITUDE IN GRID [DEG].
REAL     :: AMONOP       !! MOST NORTHERN LATITUDE IN GRID [DEG].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. GRIDDED INTEGRATED PARAMETER.                                         !
!        -----------------------------                                         !

CHARACTER (LEN=14)  :: CDT_RAD
REAL,   ALLOCATABLE :: GRID(:,:,:)  !! GRIDDED MODEL OUTPUT PARAMETERS.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     9. GENERAL SPECTRA INFORMATION.                                          !
!        ----------------------------                                          !


END MODULE WAM_PRINT_RADIATION_MODULE
