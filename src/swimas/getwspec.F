       SUBROUTINE GETWSPEC
 
!---------------------------------------------------------------------

!     PURPOSE
!     -------
!     TO COLLECT THE MODEL SPECTRA INFLUENCED BY SAR DATA POINTS 
!     AND ORGANIZE THEM IN A ONE DIMENSIONAL ARRAY.

!     AUTHOR
!     ------
!     S.HASSELMANN, MPM, HAMBURG,GERMANY
!     R.BROKOPF DTO.
!     P.LIONELLO (UNIV. OF PADUA) , HAMBURG, 11/93
!     J. BIDLOT  ECMWF MAY 1999.

!     EXTERNALS
!     ---------
!       *RESIZE_GETWSPEC*

!**********************************************************************

!     MODULES:
!     --------
      USE YOWSARAS ,            ONLY : NSPEC, 
     &            NSPECW   ,JPSIW    ,NW1D     ,NTOSIW1D ,    IGSAR,
     &            IJSAR    ,LONG     ,LAT      ,COSSARLAT,SINSARLAT,
     &            NSIW1D   ,SPECW    ,DIST     ,
     &            UW10     ,USW      ,THWW

      USE WAM_TIMOPT_MODULE,    ONLY : CDTPRO 

      USE WAM_FILE_MODULE,      ONLY : IU06

      USE WAM_GENERAL_MODULE,   ONLY : RAD, DEG, R, PI 

      USE WAM_GRID_MODULE,      ONLY : IXLG, KXLT, AMOWEP,    ZDELLO, 
     &                                 AMOSOP, AMOEAP, AMONOP, 
     &                                 XDELLA, NGY => NY, SINPH, 
     &                                 COSPH                              

      USE WAM_MPI _MODULE,      ONLY : IRANK, NSUP, NINF            ,
     &                                 NPROC => PETOTAL             ,
     &                                 NIBLO => NPOI                ,
     &                                 NSTART, NEND                 ,
     &                                 IJS => NSTART, IJL => NEND   ,
     &                                 IGL => PETOTAL


      USE WAM_MPI_MODULE,       ONLY : NBLO => PETOTAL

      USE WAM_MODEL_MODULE,     ONLY : FL3
     
      USE WAM_FRE_DIR_MODULE,   ONLY : NANG => KL, NFRE => ML

      USE WAM_MODEL_MODULE,     ONLY : U10OLD => U10, USOLD => USTAR,
     &                                 THWOLD => UDIR
      
      USE WAM_ICE_MODULE,       ONLY : GET_ICE, PUT_ICE, ICE_RUN, IJ_ICE

      USE WAM_ICE_GRID_MODULE,  ONLY : ICE_GRID

      USE WAM_ASSI_SET_UP_MODULE, ONLY : SARCORDIA, PPSARCF

!----------------------------------------------------------------------
!     INTERFACE
!     ---------
      IMPLICIT NONE

!           
!     VARIABLES
!     ---------
      INTEGER :: JBL,JP,JSAR,JFRE,JANG,ICOUNT,MAXJPSIW,LLAT,NSIW1D0
      INTEGER :: ICOUNTOLD,IC,ICC,J,K
      INTEGER,DIMENSION(NSPEC) :: IS,KS 
      INTEGER,ALLOCATABLE :: INDEX(:)

      REAL :: DELLON, XI, THRSCI
      REAL :: XMAX, DEFAULT_DIST
      REAL,ALLOCATABLE,DIMENSION(:) :: ADS
     
      LOGICAL,ALLOCATABLE,DIMENSION(:) :: LOSAR 
     
      INTEGER, ALLOCATABLE :: KXLTMIN(:)   
      INTEGER, ALLOCATABLE :: KXLTMAX(:)   


      INTEGER, PARAMETER :: KIND_D = 8

      REAL (KIND=KIND_D) :: XDELLA_REAL
      REAL (KIND=KIND_D) :: AMOWEP_REAL
      REAL (KIND=KIND_D), ALLOCATABLE :: ZDELLO_REAL(:)      
     

      INTEGER :: IP, IJ,IX       

      REAL (KIND=KIND_D), PARAMETER :: FRAC_SEC_R = 100.D0           !! M_SEC = SECONDS * FRAC_SEC.
      REAL (KIND=KIND_D), PARAMETER :: M_MINUTE_R = 60.D0*FRAC_SEC_R !! M_SEC = MINUTES * M_MINUTE.
      REAL (KIND=KIND_D), PARAMETER :: M_DEGREE_R = 60.D0*M_MINUTE_R !! M_SEC = DEGREES * M_DEGREE.
      REAL (KIND=KIND_D), PARAMETER :: M_S_PER_R = 360.D0*M_DEGREE_R !! M_SEC PER CYCLE OF LONGITUDES.

!--------------------------------------------------------------------

      XDELLA_REAL = XDELLA
      AMOWEP_REAL = AMOWEP 

! --------------------------------------------------------------------

!     DETERMINE KXLTMIN, KXLTMAX

      IF (ALLOCATED(KXLTMIN)) DEALLOCATE(KXLTMIN)
      ALLOCATE(KXLTMIN(NPROC))
      IF (ALLOCATED(KXLTMAX)) DEALLOCATE(KXLTMAX)
      ALLOCATE(KXLTMAX(NPROC))

      DO IP=1,NPROC
        KXLTMIN(IP)=NGY
        KXLTMAX(IP)=1
        DO IJ=NSTART(IP),NEND(IP)
           KXLTMIN(IP)=MIN(KXLTMIN(IP),KXLT(IJ))
           KXLTMAX(IP)=MAX(KXLTMAX(IP),KXLT(IJ))
        ENDDO
      ENDDO

! ---------------------------------------------------------------------

      THRSCI=0.3

!     1.0 ALLOCATE ARRAYS FROM MODULE
!     --------------------------------


      IF(.NOT.ALLOCATED(ZDELLO_REAL)) ALLOCATE(ZDELLO_REAL(NGY))

      ZDELLO_REAL = ZDELLO

      XDELLA_REAL = DBLE(XDELLA_REAL)/M_DEGREE_R
      AMOWEP_REAL = DBLE(AMOWEP_REAL)/M_DEGREE_R
      ZDELLO_REAL = DBLE(ZDELLO_REAL)/M_DEGREE_R


      ALLOCATE(NW1D(NINF-1:NSUP,NBLO))

      IF(XDELLA_REAL.LT.0.3) THEN
        NSPECW=250000/NPROC        ! 20000 can be changed according to model domain and nproc
      ELSE
        NSPECW=INT((1.5/XDELLA_REAL)**2*9000./NPROC)
      ENDIF


      JPSIW=4
      NSIW1D0=0

      ALLOCATE(NTOSIW1D(NSPECW))
      ALLOCATE(NSIW1D(NSPECW,JPSIW))
      NSIW1D=NSIW1D0
      ALLOCATE(DIST(NSPECW,JPSIW))

             
!     2.0 LABEL WAM SPECTRA THAT ARE INFLUENCED BY THE SAR 
!         OBSERVATIONS AND ASSOCIATE THEM TO SAR WITHIN A 
!         XMAX DISTANCE (in radian).

!     MAXIMUN EXTEND OF XMAX IN NUMBER OF GRID POINTS 
      XMAX=PPSARCF*SARCORDIA/R

      LLAT = NINT(DEG*XMAX/XDELLA_REAL)+1

      IF(XMAX.GT.0.0) THEN
        DEFAULT_DIST=10*XMAX
!        DEFAULT_DIST=10.
      ELSE
        DEFAULT_DIST=10.
      ENDIF

      DIST=DEFAULT_DIST
      NTOSIW1D=0
      MAXJPSIW=0
      NW1D=0
      ICOUNT=0

      DO JSAR=1,NSPEC
        IF(IJSAR(JSAR).NE.0) THEN
          IS(JSAR)=IXLG(IJSAR(JSAR))
          KS(JSAR)=KXLT(IJSAR(JSAR))
        ELSE
          IS(JSAR)=0
          KS(JSAR)=0
        ENDIF
      ENDDO


!     FIND POINTS WHICH FALL WITHIN XMAX FROM SAR OBSERVATIONS

        ALLOCATE(INDEX(1+MAXVAL(IJL(1:IGL)-IJS(1:IGL))))

        ALLOCATE(ADS(IJS(IRANK):IJL(IRANK)))
        ALLOCATE(LOSAR(IJS(IRANK):IJL(IRANK)))
        DO JP=IJS(IRANK),IJL(IRANK)
          LOSAR(JP)=.FALSE.
        ENDDO

        
        DO JSAR=1,NSPEC
!         exclude sar point that are obviously outside the sub region
          IF(KS(JSAR).GE.KXLTMIN(IRANK)-LLAT .AND.
     &       KS(JSAR).LE.KXLTMAX(IRANK)+LLAT .AND.
     &       IS(JSAR).GT.0 .AND. KS(JSAR).GT.0 ) THEN

            DO JP=IJS(IRANK),IJL(IRANK)
              IF(.NOT.ICE_RUN .OR. (ICE_GRID(IXLG(JP),KXLT(JP))
     &           .LE.THRSCI).AND. ABS(KS(JSAR)-KXLT(JP)).LE.LLAT) THEN
!              IF(.NOT.ICE_RUN .OR. IJ_ICE(IRANK).LE.THRSCI) THEN  
                XI = AMOWEP_REAL +REAL(IXLG(JP)-1)*ZDELLO_REAL(KXLT(JP))
                DELLON=RAD*(LONG(JSAR,2) - XI)
                ADS(JP)=COS(DELLON)*COSSARLAT(JSAR)*COSPH(KXLT(JP))+
     &                              SINSARLAT(JSAR)*SINPH(KXLT(JP))
                ADS(JP)=ACOS(MAX(MIN(ADS(JP),1.),-1.))
              ELSE
                ADS(JP)=DEFAULT_DIST
              ENDIF
            ENDDO

!           the point is already influences by an observation,
!           increment its number by 1.
            IC=0
            DO JP=IJS(IRANK),IJL(IRANK)
              IF(ADS(JP).LE.XMAX.AND.LOSAR(JP))THEN
                IC=IC+1
                INDEX(IC)=NW1D(JP,IRANK)
              ENDIF
            ENDDO
            DO ICC=1,IC
              NTOSIW1D(INDEX(ICC)) = NTOSIW1D(INDEX(ICC)) + 1
            ENDDO
           

            DO JP=IJS(IRANK),IJL(IRANK)
              IF(ADS(JP).LE.XMAX.AND.LOSAR(JP))THEN
                MAXJPSIW=MAX(MAXJPSIW,NTOSIW1D(NW1D(JP,IRANK)))
              ENDIF
            ENDDO

!           the point is not yet influenced by an observation
!           add it to the list
            IC=0
            DO JP=IJS(IRANK),IJL(IRANK)
              IF(ADS(JP).LE.XMAX.AND..NOT.LOSAR(JP))THEN
                IC=IC+1
                INDEX(IC)=JP
              ENDIF
            ENDDO

            ICOUNTOLD=ICOUNT
            ICOUNT=ICOUNTOLD+IC


            IF(ICOUNT.GT.NSPECW)
     &         CALL RESIZE_GETWSPEC(ICOUNT, JPSIW, DEFAULT_DIST,
     &                              NSIW1D0)
            DO ICC=1,IC
              JP=INDEX(ICC)
              LOSAR(JP)=.TRUE.
              NW1D(JP,IRANK)=ICOUNTOLD+ICC
              NTOSIW1D(ICOUNTOLD+ICC)=1
            ENDDO

            IF(MAXJPSIW.GT.JPSIW)
     &         CALL RESIZE_GETWSPEC(NSPECW, MAXJPSIW, DEFAULT_DIST,
     &                              NSIW1D0)

            DO JP=IJS(IRANK),IJL(IRANK)
              IF(ADS(JP).LE.XMAX)THEN
                NSIW1D(NW1D(JP,IRANK),NTOSIW1D(NW1D(JP,IRANK)))=JSAR
                DIST(NW1D(JP,IRANK),NTOSIW1D(NW1D(JP,IRANK)))=ADS(JP)
              ENDIF
            ENDDO

          ENDIF
        ENDDO

        DEALLOCATE(ADS)
        DEALLOCATE(LOSAR)


        DO JP=IJS(IRANK),IJL(IRANK)
           IF(NW1D(JP,IRANK).GT.0) THEN
             MAXJPSIW=MAX(MAXJPSIW,NTOSIW1D(NW1D(JP,IRANK)))
           ENDIF
        ENDDO


      DEALLOCATE(INDEX)

      IF(ICOUNT.LE.0 .OR. MAXJPSIW.LE. 0 ) THEN
        NSPECW = 0
      ELSE 
        CALL RESIZE_GETWSPEC(ICOUNT, MAXJPSIW, DEFAULT_DIST, NSIW1D0)
      ENDIF
      WRITE(IU06,*)' '
      WRITE(IU06,*)'  IN GETWSPEC: '
      WRITE(IU06,*)'  NUMBER OF MODEL PTS INFLUENCED BY SAR PTS ',NSPECW
      WRITE(IU06,*)'  MAX NUMBER OF SAR PTS INFLUENCING A MODEL POINT ',
     &             MAXJPSIW
      WRITE(IU06,*)' '


!      3. COLLECTING THE WAM SPECTRA INFLUENCED BY SAR DATA POINTS.
!      ------------------------------------------------------------

      IF(NSPECW .GT. 0 ) THEN
       ALLOCATE(SPECW(0:NSPECW,NANG,NFRE))
       SPECW=0
!       READ FIRST BLOCK
!      ENDIF
!        DO JBL=1,IGL
!         ODD BLOCKS
!            IF(IRANK.NE.IGL) THEN
              DO JFRE=1,NFRE
                DO JANG=1,NANG
                  DO JP = IJS(IRANK),IJL(IRANK)                  
                    IF(NW1D(JP,IRANK).GT.0) THEN
                      SPECW(NW1D(JP,IRANK),JANG,JFRE)=FL3(JP,JANG,JFRE)
                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
!            ENDIF
!        ENDDO

!      write(iu06,*) 'MIN MAX SPECW', MINVAL(SPECW), MAXVAL(SPECW)       


!       4. READ U10 WINDS AND DEFINE U10 AND THW FROM WAM SPECTRA
!       --------------------------------------------------------

        ALLOCATE(UW10(NSPECW))
        ALLOCATE(USW(NSPECW))
        ALLOCATE(THWW(NSPECW))

        DO JP = IJS(IRANK),IJL(IRANK)
          IF(NW1D(JP,IRANK).GT.0) THEN
            UW10(NW1D(JP,IRANK)) = U10OLD(JP)       
            USW(NW1D(JP,IRANK)) = USOLD(JP)       
            THWW(NW1D(JP,IRANK)) = THWOLD(JP)       
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END SUBROUTINE GETWSPEC
