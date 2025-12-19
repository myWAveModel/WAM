      SUBROUTINE PEAKFR_SAR (F3, FPEAK)

      USE WAM_FRE_DIR_MODULE,     ONLY : NANG => KL, NFRE => ML

      USE WAM_FRE_DIR_MODULE,     ONLY : FRATIO=>CO, FR

      USE WAM_FILE_MODULE,        ONLY : IU06    

      USE WAM_ASSI_SET_UP_MODULE, ONLY : FCUT
  
      implicit none


      INTEGER :: K, M, IANG, IFRE, M10
      REAL, DIMENSION (NANG,NFRE) :: F3
      REAL :: FPEAK, TEMP2, TEMP1


      M10 = 0
!      FCUT = 0.095
      DO M=1,NFRE
        IF (FR(M).LT.FCUT) M10=M10+1
      ENDDO

      TEMP1 = 0
      DO M=1,NFRE
      	TEMP2 = 0      
         DO K = 1,NANG
              TEMP2 = TEMP2 + F3(K,M)
         ENDDO
         IF (TEMP1.LT.TEMP2) THEN
             TEMP1 = TEMP2
             FPEAK = FR(M)
         ENDIF
      ENDDO
      
      RETURN
      END SUBROUTINE PEAKFR_SAR
