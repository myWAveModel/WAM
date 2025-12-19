      SUBROUTINE TRANSMEANS10(
C                     DIMENSIONS
     &                  NSPEC ,  NANG , NFRE , FCUT,
!                     INPUT
     &                  SPECW ,
C                     OUTPUT
     &                  MEANTE , MEANTANG , MEANTFRE ,
C                     WORK SPACE
     &                  SUM0 , SUMX0 , SUMY0 )
C
C-------------------------------------------------------------------

      USE YOWFRED,              ONLY : FRTAIL, WETAIL
 
      USE WAM_FRE_DIR_MODULE,   ONLY : FR, DFIM, COSTH, SINTH, DELTH,
     &                                 DFIMOFR

      USE WAM_GENERAL_MODULE,   ONLY : PI

      IMPLICIT NONE

      REAL, PARAMETER :: EPSMIN = 0.1E-32
!      REAL, PARAMETER :: WETAIL = 0.25
!      REAL, PARAMETER :: FRTAIL = 0.2 


C
C     PURPOSE:
C     --------
C
C     MEAN VALUES OF TRANSFORMED SPECTRA
C     COMPUTES MEAN ENERGY,MEAN ANGLE AND MEAN FREQUENCY
C
C     INTERFACE:
C     ----------
C
      INTEGER NSPEC, NANG, NFRE
!                     DIMENSIONS.
      REAL SPECW(0:NSPEC,NANG,NFRE)
!                     2D SPECTRUM.
      REAL MEANTE(NSPEC),
     &     MEANTANG(NSPEC), 
     &     MEANTFRE(NSPEC),
     &     MEANTF10(NSPEC)
C                     MEAN PARAMETERS.
      REAL SUM0(NSPEC), SUMX0(NSPEC), SUMY0(NSPEC)
C                     WORK SPACE FOR TEMPORARY SUMS.   
C
C     VARIABLES:
C     ----------
C
      INTEGER ISPEC, IANG, IFRE, M10, M10P
C                     LOOP VARIABLES.
      REAL PI2NANG, FACTOR, DELTH2, FCUT
      REAL C10, D10, C11, C12, DF, SIH, COH, D11, CO, CO1
C
C-------------------------------------------------------------------
C


C     INITIALIZE:
C     -----------
      M10 = 0
!      FCUT = 0.095
      CO = 1.1


C     COMPUTE MEAN ENERGIES:
C     ----------------------

      PI2NANG = PI / 2 / FLOAT(NANG)
      DELTH2 = DELTH/2

      DO IFRE =1, NFRE
          IF (FR(IFRE).LT.FCUT) M10=M10+1
      ENDDO

      DO ISPEC = 1,NSPEC
        MEANTE(ISPEC) = 0.
      END DO

      M10P = M10+1
      CO1 = CO-1
      C10  = (FCUT - FR(M10))*DELTH - CO1*FR(M10)*DELTH2
      D10  = (FCUT - FR(M10))**2*DELTH2/(CO1*FR(M10))
      DO ISPEC =1, NSPEC
        DO IANG = 1, NANG
	  DO IFRE = 1, M10
	MEANTE(ISPEC) = MEANTE(ISPEC)+DFIM(IFRE)*SPECW(ISPEC,IANG,IFRE)
          ENDDO
       D11 = SPECW(ISPEC,IANG,M10P)-SPECW(ISPEC,IANG,M10)
	MEANTE(ISPEC) = MEANTE(ISPEC)+C10*SPECW(ISPEC,IANG,M10)
     &			+ D10*D11
	ENDDO
      ENDDO

C     COMPUTE MEAN ANGLES:
C     --------------------

      DO ISPEC = 1,NSPEC
        SUMX0(ISPEC) = 0.
        SUMY0(ISPEC) = 0.
      END DO
      M10P = M10 + 1
      C10 = (FCUT-FR(M10))*DELTH-(FR(M10P)-FR(M10))*DELTH2
      D10 = (FCUT - FR(M10))**2*DELTH2/(FR(M10P)-FR(M10))
      
      DO ISPEC = 1, NSPEC
        SUMX0(ISPEC) = 0.
        SUMY0(ISPEC) = 0.
	DO IANG = 1, NANG
	  SIH = SINTH(IANG)
	  COH = COSTH(IANG)
	  DO IFRE = 1,M10
            IF (IFRE.GT.1) THEN
              IF (IFRE.NE.NFRE) THEN
                DF=(FR(IFRE+1)+FR(IFRE-1))*DELTH2
              ELSE
                DF=(FR(IFRE)+FR(IFRE-1))*DELTH2
              ENDIF
            ELSE
              DF=(FR(IFRE+1)+FR(IFRE))*DELTH2
            ENDIF
      SUMX0(ISPEC) = SUMX0(ISPEC)+SPECW(ISPEC,IANG,IFRE)*SIH*DF
      SUMY0(ISPEC) = SUMY0(ISPEC)+SPECW(ISPEC,IANG,IFRE)*COH*DF
          ENDDO
      C11 = SPECW(ISPEC,IANG,M10P)-SPECW(ISPEC,IANG,M10)
      SUMX0(ISPEC) = SUMX0(ISPEC)+C10*SPECW(ISPEC,IANG,M10)*SIH
      SUMX0(ISPEC) = SUMX0(ISPEC)+D10*C11*SIH
      SUMY0(ISPEC) = SUMY0(ISPEC)+C10*SPECW(ISPEC,IANG,M10)*COH
      SUMY0(ISPEC) = SUMY0(ISPEC)+D10*C11*COH
        
	ENDDO
        IF( SUMY0(ISPEC).NE.0..OR.SUMX0(ISPEC).NE.0. )THEN
          MEANTANG(ISPEC) = ATAN2(SUMX0(ISPEC),SUMY0(ISPEC))
        ELSE
          MEANTANG(ISPEC) = 0.
        END IF
      END DO

C     COMPUTE MEAN FREQUENCIES:
C     -------------------------
        
      C10  = (FCUT - FR(M10))*DELTH
      C10  = C10/FCUT 
      C11  = CO1*FR(M10)*DELTH2
      C11  = C11/FR(M10)
      D10  = (FCUT - FR(M10))**2*DELTH2/(CO1*FR(M10))
      D10  = D10/FCUT

      DO ISPEC = 1,NSPEC
        MEANTFRE(ISPEC) = 0.
      END DO
      DO ISPEC = 1,NSPEC
          DO IANG=1,NANG
            DO IFRE = 1,M10
              FACTOR = DFIM(IFRE) / FR(IFRE)
              MEANTFRE(ISPEC) = MEANTFRE(ISPEC)
     &      + FACTOR*SPECW(ISPEC,IANG,IFRE)
            ENDDO
	    D11 = SPECW(ISPEC,IANG,M10P)-SPECW(ISPEC,IANG,M10)
	    MEANTFRE(ISPEC) = MEANTFRE(ISPEC)
     &      + (C10-C11)*SPECW(ISPEC,IANG,M10)+D10*D11
          ENDDO
      ENDDO

      
      DO ISPEC=1,NSPEC
        IF( MEANTFRE(ISPEC).NE.0. )THEN
          MEANTFRE(ISPEC) = MEANTE(ISPEC) / MEANTFRE(ISPEC)
        ELSE
          MEANTFRE(ISPEC) = 0.
        END IF
      END DO
C
      RETURN
      END SUBROUTINE TRANSMEANS10
