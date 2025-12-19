       SUBROUTINE MERGESARCOR (FCUT)

!---------------------------------------------------------------------

!     PURPOSE
!     -------

!     MERGE ANALYSED SPECTRA INTO BLOCKED GRID SPECTRA.
!     MERGE ANALYSED WINDS INTO WIND BLOCKED ARRAYS.


!     AUTHOR: RENATE BROKOPF, MPM HAMBURG, GERMANY. MAY 1994.
!             JEAN BIDLOT ECMWF MAY 1999.
!     

!     EXTERNALS
!     ---------
       
!     *SPLITBL*

!***********************************************************************

!     MODULES:
!     --------  

      USE YOWSARAS,             ONLY : NW1D     ,SPECW    ,UW10     ,
     &                                 THWW     ,NSPECW
  
      USE WAM_MPI_MODULE,       ONLY : IJS => NSTART, IJL => NEND,
     &                                 IGL => PETOTAL, IRANK
    
      USE WAM_FRE_DIR_MODULE,   ONLY : NANG => KL, NFRE => ML

      USE WAM_MPI _MODULE,      ONLY : NIBLO => NPOI, NBLO => PETOTAL

      USE WAM_FILE_MODULE,      ONLY : IU06

      USE WAM_FRE_DIR_MODULE,   ONLY : FR

      USE WAM_MODEL_MODULE,     ONLY : FL3

!-----------------------------------------------------------------------

!     INTERFACE
!     ---------

      IMPLICIT NONE

!     VARIABLES
!     ---------

      INTEGER JBL,JP,JFRE,JANG,IFIL, M10 , M10P

      REAL :: UST(2), USTM, FCUT, CO, SP, SP1
     
!     
!-----------------------------------------------------------------------

!      1. UPDATING SPECTRA 
!         ----------------

      M10 = 0
!      FCUT = 0.095
      CO = 1.1

      DO JFRE =1, NFRE
          IF (FR(JFRE).LT.FCUT) M10=M10+1
      ENDDO
      M10P=M10+1

! added to account the model energy at the cutoff

           DO JANG=1,NANG
             DO JP = IJS(IRANK),IJL(IRANK)
               IF(NW1D(JP,IRANK).NE.0 ) THEN
                 SPECW(NW1D(JP,IRANK),JANG,M10)=FL3(JP,JANG,M10)
               ENDIF
             ENDDO
           ENDDO       

!            DO JFRE=1,NFRE
            DO JFRE=1,M10
              DO JANG=1,NANG
                DO JP = IJS(IRANK),IJL(IRANK)
		  IF(NW1D(JP,IRANK).NE.0 ) THEN
            FL3(JP,JANG,JFRE)=SPECW(NW1D(JP,IRANK),JANG,JFRE)
!              IF (JFRE.EQ.M10) FL1(JP,JANG,JFRE)=(SP1+SP)/2
                  ENDIF
                ENDDO
              ENDDO
            ENDDO



!     2. UPDATING WINDS
!        --------------

!     no longer used.

      RETURN
      END SUBROUTINE MERGESARCOR
