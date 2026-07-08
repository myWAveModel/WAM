      subroutine numofobservations(iu90, file90)

! ---------------------------------------------------!
!     Assists finding the number of observations     !
!     in the satellite data at every assimilation    !
!     time step. Searches for observation files      !
!     which fall into time window around the         !
!     assimilation time step. Time window is         !
!     prescribed as half of the forecast horizon     !
!     (idelspe/2) below.                             !
!                                                    !
!                                                    !
!     RUMEYSA YILMAZ  - Helmholtz-Zentrum Hereon     !
!                  11.2025                           !
!                                                    !
! ---------------------------------------------------!

      USE WAM_MPI_MODULE,      ONLY : IRANK
      USE WAM_GENERAL_MODULE,  ONLY : PI, DEG, OPEN_FILE, ABORT1,
     &                                INCDATE
      USE WAM_FILE_MODULE,     ONLY : IU06
      USE YOWSARAS ,           ONLY : NSPEC, SPEC, LAT, LONG, XKLOW,
     &                                XKUP, XKCUTOFF, USAR, RA, CUT, NVI
      USE WAM_TIMOPT_MODULE,   ONLY : CDTPRO
      USE WAM_FRE_DIR_MODULE,  ONLY : ML, KL
      USE WAM_ASSI_SET_UP_MODULE, ONLY : CDTSPS, CDATSE, IDELSPE

      IMPLICIT NONE
      
      INTEGER       :: INUNIT, IFAIL , ISPECX, I
      CHARACTER*200 :: YONAME
      INTEGER       :: IU90
      CHARACTER*80  :: FILE90
      CHARACTER*256 :: LINE
      CHARACTER*14  :: CBEGINDTS
      CHARACTER*14  :: CENDDTS
      CHARACTER*14  :: CDATES


      CBEGINDTS = CDTPRO
      CALL INCDATE (CBEGINDTS, -(IDELSPE/2))
      CENDDTS = CBEGINDTS
      CALL INCDATE(CENDDTS, (IDELSPE-10))

      WRITE(IU06,*) ' SUB. NUMOFOBS : SELECTING SPECTRAL MEASUREMENTS '
      WRITE(IU06,*) '                 PERIOD FROM ', CBEGINDTS,' TO ',
     &                                CENDDTS


      nspec = 0
      cdates = cbegindts
      
      file_loop: do
      
        if (cdates.gt.cenddts) exit file_loop

        inunit = iu90
        yoname = trim(file90) // trim(cdates)

        call open_file (iu06,iu90,file90,cdates,'OLD',ifail,
     &                                  forma='formatted')
     
        if(ifail.ne.0) then
           write(iu06,*) 'NO SPECTRAL FILE FOR ', CDATES
           call incdate (cdates,1800)                   ! 0.5 hour increment
           cycle file_loop
!          call abort1
        endif
        write(iu06,*) 'SPECTRAL FILE EXISTS FOR ', CDATES
        

        ispecx = 0 
        numspec: do
           read(iu90, '(A)',iostat = ifail) line

           if (ifail.ne.0) exit numspec

           ispecx = ispecx +1
         
           do i = 1,ml

             read(iu90, '(A)', iostat = ifail) line
	     if (ifail.ne.0) exit
  
           enddo
        enddo numspec

        nspec = nspec + ispecx 
        write(iu06,*) 'SPECTRA IN FILE ', ispecx
        
        close(iu90)
        call incdate (cdates,1800)                      ! 0.5 hour increment

      end do file_loop

      write(iu06,*) 'TOTAL NUM OF SPEC IN THIS ASSIMILATION TIME STEP ',
     &               CDTPRO , '  IS ', NSPEC

      end subroutine numofobservations
