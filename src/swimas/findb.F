      SUBROUTINE FINDB (NDIM, NBOUN, BLATB, BLNGB, IGARB, IJARB,
     &                  NPROC, NSTART,NEND, IRANK)

! ----------------------------------------------------------------------

!**** *FINDB* - FIND BLOCK AND GRID POINT NUMBER.

!     R. PORTZ     MPI         15/01/1991
!     J. BIDLOT    ECMWF       JUNE 1996  MESSAGE PASSING

!*    PURPOSE.
!     -------

!       FIND BLOCK AND GRID POINT NUMBER FOR A GIVEN ARRAY
!       OF LONGITUDES AND LATITUDES.
!       IN CASE OF MESSAGE PASSING ARCHITECHTURE, THE SEARCH IS ONLY
!       DONE ON THE SUBREGION WHICH IS DEFINED ON THAT PE.

!**   INTERFACE.
!     ----------

!       *CALL* *FINDB (NDIM, NBOUN, BLATB, BLNGB, IGARB, IJARB)*
!          *NDIM*    - DIMENSION OF ARRAYS.
!          *NBOUN*   - NUMBER OF POINTS IN ARRAYS.
!          *BLATB*   - INPUT LATITUDES.
!          *BLNGB*   - INPUT LONGITUDES.
!          *IGARB*   - OUTPUT BLOCK NUMBERS.
!          *IJARB*   - OUTPUT SEA POINT NUMBERS.
!          *NPROC*     DIMENSION OF NSTART AND NEND
!          *NSTART*    INDEX OF THE FIRST POINT OF THE SUB GRID DOMAIN
!          *NEND*      INDEX OF THE LAST POINT OF THE SUB GRID DOMAIN
!          *IRANK*     PE INDEX


!     METHOD.
!     -------

!       NONE.

!     EXTERNALS.
!     ----------

!       NONE.

!     REFERENCE.
!     ----------

!       NONE.

! ----------------------------------------------------------------------

      USE YOWMESPAS,         ONLY : LMESSPASS

      USE WAM_GRID_MODULE,   ONLY : IXLG, KXLT, AMOWEP,
     &                              NLONRGG => NLON_RG, 
     &                              NGY => NY         ,
     &                              AMOSOP, AMONOP    ,
     &                              XDELLA, XDELLO    ,
     &                              ZDELLO, IPER      ,
     &                              AMOEAP    
    
      USE wam_mpi_module,    ONLY : IJS => NIJS, IJL => NIJL, 
     &                              localcomm 

      USE WAM_MPI_MODULE,    ONLY : NPR => PETOTAL    

! ----------------------------------------------------------------------

      IMPLICIT NONE



      INTEGER :: IP, IJ, IO, NDIM, NPROC, IRANK, NBOUN
      INTEGER :: ILATS, ILATN, IOLT, IOLG
      REAL    :: ALONG, ZDEL

      REAL,    DIMENSION(NDIM)  :: BLATB, BLNGB
      INTEGER, DIMENSION(NDIM)  :: IGARB, IJARB
      INTEGER, DIMENSION(NPROC) :: NSTART, NEND           

      INTEGER, ALLOCATABLE :: KXLTMIN(:)  
      INTEGER, ALLOCATABLE :: KXLTMAX(:)  
      
      INTEGER :: ierr 
      
      INTEGER, PARAMETER            :: KIND_D = 8
      REAL (KIND=KIND_D), PARAMETER :: FRAC_SEC_R = 100.D0 
      REAL (KIND=KIND_D), PARAMETER :: M_MINUTE_R = 60.D0*FRAC_SEC_R 
      REAL (KIND=KIND_D), PARAMETER :: M_DEGREE_R = 60.D0*M_MINUTE_R 
      REAL (KIND=KIND_D), PARAMETER :: M_S_PER_R = 360.D0*M_DEGREE_R 

      REAL (KIND=KIND_D) :: XDELLA_REAL
      REAL (KIND=KIND_D) :: XDELLO_REAL
      REAL (KIND=KIND_D) :: AMOSOP_REAL
      REAL (KIND=KIND_D) :: AMONOP_REAL
      REAL (KIND=KIND_D) :: AMOWEP_REAL
      REAL (KIND=KIND_D) :: AMOEAP_REAL
      REAL (KIND=KIND_D), ALLOCATABLE  :: ZDELLO_REAL(:)
! ----------------------------------------------------------------------

      IF(.NOT.ALLOCATED(ZDELLO_REAL)) ALLOCATE(ZDELLO_REAL(NGY))

      XDELLA_REAL = XDELLA
      ZDELLO_REAL = ZDELLO
      XDELLO_REAL = XDELLO
      AMOSOP_REAL = AMOSOP
      AMOWEP_REAL = AMOWEP
      AMONOP_REAL = AMONOP
      AMOEAP_REAL = AMOEAP

      XDELLA_REAL = DBLE(XDELLA_REAL)/M_DEGREE_R
      ZDELLO_REAL = DBLE(ZDELLO_REAL)/M_DEGREE_R
      XDELLO_REAL = DBLE(XDELLO_REAL)/M_DEGREE_R
      AMOSOP_REAL = DBLE(AMOSOP_REAL)/M_DEGREE_R
      AMONOP_REAL = DBLE(AMONOP_REAL)/M_DEGREE_R
      AMOWEP_REAL = DBLE(AMOWEP_REAL)/M_DEGREE_R
      AMOEAP_REAL = DBLE(AMOEAP_REAL)/M_DEGREE_R

!     FIND THE MIN AND MAX OF KXLT
!     ----------------------------

      IF (ALLOCATED(KXLTMIN)) DEALLOCATE(KXLTMIN)
      ALLOCATE(KXLTMIN(NPR))
      IF (ALLOCATED(KXLTMAX)) DEALLOCATE(KXLTMAX)
      ALLOCATE(KXLTMAX(NPR))

      DO IP=1,NPR
        KXLTMIN(IP)=NGY
        KXLTMAX(IP)=1
        DO IJ=NSTART(IP),NEND(IP)
           KXLTMIN(IP)=MIN(KXLTMIN(IP),KXLT(IJ))
           KXLTMAX(IP)=MAX(KXLTMAX(IP),KXLT(IJ))
        ENDDO
      ENDDO

     
!*    1. LOOP OVER INPUT LATITUDES, LONGITUDES.
!        --------------------------------------

      IF(LMESSPASS) THEN
        ILATS=KXLTMIN(IRANK)-1
        ILATN=KXLTMAX(IRANK)+1
      ELSE
        ILATS=1
        ILATN=NGY
      ENDIF

      DO IO = 1,NBOUN

!*    1.1 COMPUTE GRID MATRIX INDICES.
!         ----------------------------

        IOLT = NINT((BLATB(IO)-AMOSOP_REAL)/XDELLA_REAL+1.0)
        ALONG = MOD(BLNGB(IO)-AMOWEP_REAL+720.,360.)
        IF(IOLT.LT.ILATS.OR.IOLT.GT.ILATN ) THEN
          ZDEL=XDELLO_REAL
        ELSE
          ZDEL=ZDELLO_REAL(IOLT)
        ENDIF
        IOLG = NINT(ALONG/ZDEL+1.0)



        IF (IOLG.EQ.(NLONRGG(IOLT)+1) .AND. IPER.EQ.1) IOLG = 1

  
        IF(IOLG.LT.1.OR.IOLG.GT.NLONRGG(IOLT).OR.
     &     IOLT.LT.ILATS.OR.IOLT.GT.ILATN ) THEN

          IGARB(IO) = 0
          IJARB(IO) = 0
        ELSE

!*    1.2 SEARCH BLOCK NUMBER AND SEA POINT NUMBER.
!         -----------------------------------------

          IF(LMESSPASS) THEN
            IGARB(IO) = 0
            IJARB(IO) = 0


            DO IJ = NSTART(IRANK),NEND(IRANK)
              IF (IXLG(IJ).EQ.IOLG .AND. KXLT(IJ).EQ.IOLT) THEN
                IGARB(IO) = 1
                IJARB(IO) = IJ
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE FINDB
