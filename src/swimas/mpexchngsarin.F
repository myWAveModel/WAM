      SUBROUTINE MPEXCHNGSARIN(NSPECPE,ISPECPE) 


!****  *MPEXCHNGSARIN* - EXCHANGES MODEL DATA AT SAR DATA POINTS. 

!     J. BIDLOT    ECMWF   NOVEMBER 1999

!     PURPOSE.
!     --------

!     BROADCASTS MODEL DATA AT SAR DATA POINTS FROM ALL PE'S TO
!     ALL PE'S 

!*    INTERFACE.
!     ----------

!     CALL *MPEXCHNGSARIN*(NSPECPE,ISPECPE,ITAG)


!     *NSPECPE* - NUMBER OF SAR DATA POINT ON THAT PE.
!     *ISPECPE* - ARRAY INDEX OF EACH POINT BELONGING TO THAT PE.
!     *ITAG*    - TAG USED FOR MESSAGE PASSING.  
!     METHOD.
!     -------

!     EXTERNALS.
!     ----------
!     MPL PACKAGE :     
!         MPL_BROADCAST

!     REFERENCES.
!     -----------
!         NONE
!
!     !!!!!MPL PACKAGES ARE SWITCHED WITH MPI PACKAGES - 2025
! -------------------------------------------------------------------

      USE YOWSARAS,           ONLY : NSPEC    ,SPEC     ,IGSAR    ,
     &                               IJSAR    ,LONG     ,LAT      ,
     &                               U10      ,USSAR    ,THW

      USE WAM_MPI _MODULE,    ONLY : IRANK, NPROC => PETOTAL,
     &                               localcomm

      USE WAM_FRE_DIR_MODULE, ONLY : NANG => KL, NFRE => ML

      USE mpi_f08 
!----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER :: NSPECPE, KSPECPE, IP, ISPEC, IFRE, IANG, NSP
      INTEGER :: ITAG, MZC, KCOUNT, IREAD, NTEMP(1), ICOUNTS(NPROC)
      INTEGER :: ISPECPE(NSPEC)              
      INTEGER :: IERR, I, sendcount, sendcount1                      
      INTEGER :: MAX_KSPECPE                                         
      INTEGER :: LOCAL_OFFSET, K_OFFSET  
      REAL   , ALLOCATABLE :: ZCOMBUF(:),ZRECVBUF(:)
      INTEGER, ALLOCATABLE :: IDISPL(:),IDISPL1(:), recvcounts(:)
      INTEGER, ALLOCATABLE :: NSPECALLPE(:),recvcounts1(:)                              


      ALLOCATE(IDISPL(NPROC))   
      ALLOCATE(IDISPL1(NPROC))    
      ALLOCATE(RECVCOUNTS(NPROC))
      ALLOCATE(RECVCOUNTS1(NPROC))  
      MAX_KSPECPE = SIZE(IGSAR)  

      IF(NPROC.EQ.1) THEN
        RETURN
      ELSE
! Communication of the number of spec per proc

! ---------------------- Calculating sendcount and recvcounts ---------------------------

        SENDCOUNT = 1
        CALL MPI_Allgather(sendcount, 1, MPI_INTEGER, recvcounts, 1, 
     &                     MPI_INTEGER, localcomm, ierr)

! ----------------------------------- Calculating IDISPL --------------------------------

        IDISPL(1) = 0
       
        DO I = 2, NPROC
           IDISPL(I) = IDISPL(I-1) + RECVCOUNTS(I-1)
        END DO

        ALLOCATE(NSPECALLPE(NPROC))

! -------------------- Sending the number of SAR data to all PE's ------------------------


        NTEMP(1)=NSPECPE
        ICOUNTS(:)=1


        CALL MPI_ALLGATHERV(NTEMP, 1, MPI_INTEGER,
     &                      NSPECALLPE, recvcounts, IDISPL, MPI_INTEGER,
     &                      localcomm, IERR)

        MZC=1+NSPECPE*(8+NANG*NFRE)
        ALLOCATE(ZCOMBUF(MZC))

! The data for each proc
        KCOUNT=1
        ZCOMBUF(KCOUNT)=FLOAT(NSPECPE)
        DO ISPEC = 1,NSPECPE
          KCOUNT=KCOUNT+1
          ZCOMBUF(KCOUNT)=FLOAT(ISPECPE(ISPEC))
          KCOUNT=KCOUNT+1
          ZCOMBUF(KCOUNT)=FLOAT(IGSAR(ISPECPE(ISPEC)))
          KCOUNT=KCOUNT+1
          ZCOMBUF(KCOUNT)=FLOAT(IJSAR(ISPECPE(ISPEC)))
          KCOUNT=KCOUNT+1
          ZCOMBUF(KCOUNT)=LONG(ISPECPE(ISPEC),1)
          KCOUNT=KCOUNT+1
          ZCOMBUF(KCOUNT)=LAT(ISPECPE(ISPEC),1)
          KCOUNT=KCOUNT+1
          ZCOMBUF(KCOUNT)=U10(ISPECPE(ISPEC))
          KCOUNT=KCOUNT+1
          ZCOMBUF(KCOUNT)=USSAR(ISPECPE(ISPEC))
          KCOUNT=KCOUNT+1
          ZCOMBUF(KCOUNT)=THW(ISPECPE(ISPEC))
          DO IFRE = 1,NFRE
            DO IANG = 1,NANG
              KCOUNT=KCOUNT+1
              ZCOMBUF(KCOUNT)=SPEC(ISPECPE(ISPEC),IANG,IFRE,1)
            ENDDO
          ENDDO
        ENDDO
        DO IP=1,NPROC
          NSPECALLPE(IP)=1+NSPECALLPE(IP)*(8+NANG*NFRE)
        ENDDO

! ---------------------- Calculating sendcount and recvcounts ---------------------------

        sendcount1 = NSPECALLPE(IRANK)

        CALL MPI_Allgather(sendcount1, 1, MPI_INTEGER, recvcounts1, 1, 
     &                     MPI_INTEGER, localcomm, ierr)

! ----------------------------------- Calculating IDISPL --------------------------------

        IDISPL1(1) = 0
       
        DO I = 2, NPROC
           IDISPL1(I) = IDISPL1(I-1) + RECVCOUNTS1(I-1)
        END DO

        ALLOCATE(ZRECVBUF(SUM(RECVCOUNTS1)))

        CALL MPI_ALLGATHERV(ZCOMBUF, sendcount1, MPI_REAL,
     &                      ZRECVBUF, recvcounts1, IDISPL1, MPI_REAL,
     &                      localcomm, ierr)  



        DO IP = 1, NPROC
           LOCAL_OFFSET = IDISPL1(IP) + 1
           NSP = NINT(ZRECVBUF(LOCAL_OFFSET)) 
           K_OFFSET = LOCAL_OFFSET 

           IF (NSP > 0) THEN 
              DO ISPEC = 1, NSP
                 K_OFFSET = K_OFFSET + 1 
                 KSPECPE = NINT(ZRECVBUF(K_OFFSET))
                 K_OFFSET = K_OFFSET + 1 
                 IGSAR(KSPECPE) = NINT(ZRECVBUF(K_OFFSET))
                 K_OFFSET = K_OFFSET + 1 
                 IJSAR(KSPECPE) = NINT(ZRECVBUF(K_OFFSET))
                 K_OFFSET = K_OFFSET + 1 
                 LONG(KSPECPE,1) = ZRECVBUF(K_OFFSET)
                 K_OFFSET = K_OFFSET + 1 
                 LAT(KSPECPE,1) = ZRECVBUF(K_OFFSET)
                 K_OFFSET = K_OFFSET + 1 
                 U10(KSPECPE) = ZRECVBUF(K_OFFSET)
                 K_OFFSET = K_OFFSET + 1 
                 USSAR(KSPECPE) = ZRECVBUF(K_OFFSET)
                 K_OFFSET = K_OFFSET + 1 
                 THW(KSPECPE) = ZRECVBUF(K_OFFSET)

                 DO IFRE = 1, NFRE
                    DO IANG = 1, NANG
                       K_OFFSET = K_OFFSET + 1 
                       SPEC(KSPECPE,IANG,IFRE,1) = ZRECVBUF(K_OFFSET)
                    ENDDO
                 ENDDO
              ENDDO
           ENDIF
        ENDDO

        DEALLOCATE(ZCOMBUF)
        DEALLOCATE(ZRECVBUF)

      ENDIF

      RETURN
      END SUBROUTINE MPEXCHNGSARIN
