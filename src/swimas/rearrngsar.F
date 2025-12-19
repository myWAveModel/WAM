       SUBROUTINE REARRNGSAR(TH0) 
 
!---------------------------------------------------------------------

!     PURPOSE
!     -------
!     TO ADJUST THE SAR SPECTRA TO THE FIRST DIRECTION OF THE MODEL
!     (TH0) AND TO FETCH THE MODEL SPECTRA AND MODEL WINDS THE CLOSEST
!     TO SAR POINT LOCATIONS. 

!     AUTHOR
!     ------
!     J. BIDLOT  ECMWF JUNE 1999.

!     EXTERNALS
!     ---------
!     *FINDB*   

!**********************************************************************

!     MODULES:
!     --------
      USE YOWSARAS,             ONLY : NSPEC,    IGSAR,     IJSAR,
     &                                 SPEC,     LONG,      LAT,
     &                                 U10,      USSAR,     THW   

      USE YOWMESPAS,            ONLY : LMESSPASS

      USE wam_mpi_module,       ONLY : IJS => NIJS, IJL => NIJL   ,
     &                                 IGL => PETOTAL, NIBLO => NPOI,
     &                                 IRANK, NPROC => PETOTAL, 
     &                                 NSTART, NEND,
     &                                 NBLO => PETOTAL     

      USE WAM_GRID_MODULE,      ONLY : IXLG, KXLT, AMOWEP,
     &                                 AMOSOP, XDELLA, 
     &                                 XDELLO, ZDELLO, NGY => NY 

      USE WAM_MODEL_MODULE,     ONLY : U10OLD => U10, USOLD => USTAR,
     &                                 THWOLD => UDIR, FL3 

      USE WAM_ICE_MODULE,       ONLY : GET_ICE, PUT_ICE, ICE_RUN, IJ_ICE
      
      USE WAM_FRE_DIR_MODULE,   ONLY : NANG => KL, NFRE => ML

      USE WAM_FILE_MODULE,      ONLY : IU06, ITEST

      USE WAM_ICE_GRID_MODULE,  ONLY : ICE_GRID

      USE WAM_GENERAL_MODULE,   ONLY : ZPI 

      USE WAM_COORDINATE_MODULE
!----------------------------------------------------------------------

      IMPLICIT NONE

      REAL :: TH0 ! FIRST DIRECTION IN WAM SPECTRUM.

!     VARIABLES
!     ---------

      INTERFACE
        SUBROUTINE READ_ICE_INPUT            !! READ AN ICE MAP.
        END SUBROUTINE READ_ICE_INPUT
      END INTERFACE


      REAL (KIND=KIND_D) :: XDELLA_REAL
      REAL (KIND=KIND_D) :: AMOSOP_REAL
      REAL (KIND=KIND_D) :: AMOWEP_REAL
      REAL (KIND=KIND_D),ALLOCATABLE  :: ZDELLO_REAL(:)


      REAL, ALLOCATABLE :: FL(:,:)
      REAL :: FTH, ADIF, BDIF, THRSCI
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ISPECPE 
      INTEGER :: ISPEC, K, M, KC, KC1, INC
      INTEGER :: JBL,JP,JFRE,JANG,NSPECPE
      INTEGER :: ix,iy

      THRSCI      = 0.3

      IF(.NOT.ALLOCATED(ZDELLO_REAL)) ALLOCATE(ZDELLO_REAL(NGY))

      XDELLA_REAL = XDELLA
      ZDELLO_REAL = ZDELLO
      AMOSOP_REAL = AMOSOP
      AMOWEP_REAL = AMOWEP

      XDELLA_REAL = DBLE(XDELLA_REAL)/M_DEGREE_R
      ZDELLO_REAL = DBLE(ZDELLO_REAL)/M_DEGREE_R
      AMOSOP_REAL = DBLE(AMOSOP_REAL)/M_DEGREE_R
      AMOWEP_REAL = DBLE(AMOWEP_REAL)/M_DEGREE_R



!     ROTATE INVERTED SAR SPECTRA TO THE FIRST DIRECTION OF WAM (TH0)
!     --------------------------------------------------------------- 
!     !!!! NOTE IT IS ASSUMED THAT INPUT INVERTED SPECTRA ARE DEFINED
!          WITH 0 AS THEIR FIRST DIRECTION.

      FTH = MOD(-TH0+ZPI,ZPI)
      FTH = FTH * REAL(NANG) / ZPI
      INC = INT(FTH)
      ADIF = FTH - INC
      BDIF = 1. - ADIF
      ALLOCATE(FL(NANG,NFRE))


      DO ISPEC = 1,NSPEC
        DO K=1,NANG
          KC  = K  - INC
          IF (KC .LT. 1) KC  = KC + NANG
          KC1 = KC - 1
          IF (KC1.LT. 1) KC1 = KC1+ NANG
          DO M=1,NFRE
            FL(K,M) = BDIF*SPEC(ISPEC,KC,M,2)+ADIF*SPEC(ISPEC,KC1,M,2)
          ENDDO
        ENDDO
        DO M=1,NFRE
          DO K=1,NANG
            SPEC(ISPEC,K,M,2) = FL(K,M)
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(FL)


!     GET WAM SPECTRA AT OBSERVATION LOCATION
!     ---------------------------------------

      ALLOCATE(IGSAR(NSPEC))
      ALLOCATE(IJSAR(NSPEC))
      ALLOCATE(ISPECPE(NSPEC))
      ISPECPE=0

!     FIND BLOCK POSITION OF SAR OBSERVATION POINTS

      CALL FINDB(NSPEC,NSPEC,LAT(1,2),LONG(1,2),IGSAR,IJSAR,
     &           NPROC,NSTART,NEND,IRANK)


      DO IX = 1, NSPEC
         IF(IJSAR(IX).NE.0.OR.IGSAR(IX).NE.0) THEN
           PRINT *, "After FINDB in rearr: I=", IX,
     &           " IGSAR=", IGSAR(IX),   
     &           " IJSAR=", IJSAR(IX),  
     &           " IRANK=", IRANK   
         ENDIF
      ENDDO

!     REPLACE FIRST GUESS SPECTRA USED IN THE SAR INVERSION BY WAM
!     SPECTRA FOUND AT THE CLOSEST GRID POINT

      NSPECPE=0

      DO JBL=1,IGL
          DO ISPEC = 1, NSPEC 
            JP=IJSAR(ISPEC)
            IF(JP.NE.0) THEN
              IF(IGSAR(ISPEC).EQ.JBL) THEN
                IF(.NOT.ICE_RUN .OR. ICE_GRID(IXLG(JP),KXLT(JP))
     &             .LE.THRSCI) THEN
!                IF(.NOT.ICE_RUN .OR. IJ_ICE(JBL).LE.THRSCI) THEN
                  DO JFRE=1,NFRE
                    DO JANG=1,NANG
                      SPEC(ISPEC,JANG,JFRE,1)=FL3(JP,JANG,JFRE)
                    ENDDO
                  ENDDO
                  LAT(ISPEC,1)=AMOSOP_REAL+REAL(KXLT(JP)-1)*XDELLA_REAL
                  LONG(ISPEC,1)=AMOWEP_REAL+
     &                        REAL(IXLG(JP)-1)*ZDELLO_REAL(KXLT(JP))
                ELSE
                  DO JFRE=1,NFRE
                    DO JANG=1,NANG
                      SPEC(ISPEC,JANG,JFRE,1)=SPEC(ISPEC,JANG,JFRE,2)
                    ENDDO
                  ENDDO
                  LAT(ISPEC,1)=LAT(ISPEC,2)
                  LONG(ISPEC,1)=LONG(ISPEC,2)
                ENDIF
                NSPECPE=NSPECPE+1
                ISPECPE(NSPECPE)=ISPEC
              ENDIF
            ELSE
              DO JFRE=1,NFRE
                DO JANG=1,NANG
                  SPEC(ISPEC,JANG,JFRE,1)=SPEC(ISPEC,JANG,JFRE,2)
                ENDDO
              ENDDO
              LAT(ISPEC,1)=LAT(ISPEC,2)
              LONG(ISPEC,1)=LONG(ISPEC,2)
            ENDIF
          ENDDO
      ENDDO

! REPLACE WINDS FROM SAR INPUT BY MODEL WINDS

      ALLOCATE(U10(NSPEC))
      ALLOCATE(USSAR(NSPEC))
      ALLOCATE(THW(NSPEC))

        DO ISPEC = 1, NSPEC
          JP=IJSAR(ISPEC)
          IF(IGSAR(ISPEC).EQ.1) THEN
            IF (JP.NE.0) THEN
               U10(ISPEC) = U10OLD(JP)
               USSAR(ISPEC) = USOLD(JP)
               THW(ISPEC) = THWOLD(JP)
            ENDIF
          ELSE
            U10(ISPEC) = 0.
            USSAR(ISPEC) = 0.
            THW(ISPEC) = 0.
          ENDIF
        ENDDO


!     EXCHANGE THE DIFFERENT CONTRIBUTIONS OF THE MODEL VALUES AT SAR
!     LOCATION WITH ALL THE OTHER PE'S.

      IF(LMESSPASS) THEN
        IF(ITEST.GT.0)
     &  WRITE(IU06,*)'-----------MPEXCHNGSARIN BEGINS------------'
        CALL MPEXCHNGSARIN(NSPECPE,ISPECPE)
        IF(ITEST.GT.0)
     &  WRITE(IU06,*)'-----------MPEXCHNGSARIN ENDS------------'
      ENDIF

      DEALLOCATE(ISPECPE)

      RETURN
      END SUBROUTINE REARRNGSAR 
