	subroutine readswispec (iu90,file90,lnodata,dtg)
!-------------------------------------------------------------!
!	        read observed spectra file                    !
!                                                             !
!                                                             !
!                          ***                                !
!                        update                               !
!       OBSERVATION FILES ARE READ WITHIN THE TIME WINDOW     !
!       AROUND THE ASSIMILATION TIME STEP. IF THERE IS NO     !
!       SATELLITE DATA FOR THE TIME STEP, ASSIMILATION IS     !
!       NOT CARRIED OUT AND THE TIME STEP IS UPDATED AT       !
!       END OF SWIMAS.                                        !
!                          ***                                !
!       RUMEYSA YILMAZ - HELMHOLTZ ZENTRUM HEREON - 2025      !
!                                                             !
!-------------------------------------------------------------!

      USE YOWSARAS,               ONLY : NSPEC,  SPEC, LAT, LONG, XKLOW,
     &                                   XKUP,  XKCUTOFF, USAR, RA, CUT,
     &                                   NVI

      USE WAM_MPI_MODULE,         ONLY: IRANK

      USE WAM_TABLES_MODULE,      ONLY : FLMINFR,FLMIN

      USE WAM_FRE_DIR_MODULE,     ONLY : NANG => KL, NFRE => ML


      USE WAM_GENERAL_MODULE,     ONLY : PI, DEG, OPEN_FILE, ABORT1,
     &                                   INCDATE
 
      USE WAM_TIMOPT_MODULE,      ONLY : CDTPRO     

      USE WAM_FILE_MODULE,        ONLY : IU06

      USE WAM_ASSI_SET_UP_MODULE, ONLY : CDTSPS, IDELSPE, IU80, FILE80


      implicit none

      INTEGER :: inunit
      INTEGER :: ISPEC, ISS, IC, IANG, IFRE
      REAL, ALLOCATABLE :: ETOT(:),FMEAN(:),THQ(:),FPSAR(:),ANG_TR(:)
      REAL, ALLOCATABLE :: SP2(:,:)

      CHARACTER*14 DTG
      CHARACTER*200 YONAME
      LOGICAL :: LNODATA

      INTEGER :: IFAIL

      REAL :: E2, F2, TH2, FP

      INTEGER       :: IU90
      CHARACTER*80  :: FILE90
      CHARACTER*14  :: CBEGINDTSS
      CHARACTER*14  :: CENDDTSS
      CHARACTER*14  :: CDATESS

      IF (.NOT.ALLOCATED(SPEC)) ALLOCATE(SPEC(0:NSPEC,NANG,NFRE,2))
      ALLOCATE(SP2(NANG,NFRE))
      ALLOCATE(LONG(NSPEC,2))
      ALLOCATE(LAT(NSPEC,2))
      ALLOCATE(XKLOW(NSPEC))
      ALLOCATE(XKUP(NSPEC))
      ALLOCATE(XKCUTOFF(NSPEC))
      ALLOCATE(ETOT(NSPEC))
      ALLOCATE(FMEAN(NSPEC))
      ALLOCATE(THQ(NSPEC))
      ALLOCATE(FPSAR(NSPEC))
      IF (.NOT.ALLOCATED(USAR)) ALLOCATE(USAR(NSPEC))
      IF (.NOT.ALLOCATED(RA)) ALLOCATE(RA(NSPEC))
      IF (.NOT.ALLOCATED(CUT)) ALLOCATE(CUT(NSPEC))
      IF (.NOT.ALLOCATED(NVI)) ALLOCATE(NVI(NSPEC))
      IF (.NOT.ALLOCATED(ANG_TR)) ALLOCATE(ANG_TR(NSPEC))

      DO IC=1,2
        DO IANG=1,NANG
          DO IFRE=1,NFRE
            DO ISS=0,NSPEC
            SPEC(ISS,IANG,IFRE,IC) = 0.25*FLMINFR(1,IFRE) 
            ENDDO
          ENDDO
        ENDDO
      ENDDO


      CBEGINDTSS = CDTPRO
      CALL INCDATE (CBEGINDTSS, -(IDELSPE/2))
      CENDDTSS = CBEGINDTSS
      CALL INCDATE(CENDDTSS, (IDELSPE-10))

      CDATESS = CBEGINDTSS

      ISPEC = 1
      LONG=0.
      LAT=0.
      XKLOW=0.
      XKUP=0.
      XKCUTOFF=0.
      LNODATA=.FALSE.
      inunit = IU90

      DO WHILE (CDATESS.LE.CENDDTSS)

        yoname = TRIM(FILE90) // TRIM(cdatess)
	WRITE(IU06,*) 'FILE WILL BE READ ', CDATESS


        ifail = 0
        call open_file (iu06,iu90,file90,cdatess,'OLD',ifail,
     &                                  forma='formatted')
      
        rewind(iu90)
!        write(*,*)'ifail', ifail
        IF (IFAIL.NE.0) THEN
	   CALL INCDATE (CDATESS, 1800)     ! 0.5 hour increment
	   CYCLE
!          CALL ABORT1
        ENDIF

   10 continue
        read(iu90,*,end=20) dtg, long(ispec,2), lat(ispec,2),
     &    usar(ispec), ra(ispec), cut(ispec), nvi(ispec), ang_tr(ispec)
        do ifre=1,nfre
          read(iu90,*) (spec(ispec,iang,ifre,2), iang=1,nang)
        end do
        
        ispec = ispec + 1
        if (ispec > nspec) goto 20
        goto 10
   20 continue
	close(iu90)
	call incdate(cdatess, 1800)        ! 0.5 hour increment
      ENDDO


      write(*,*) 'Number of spectra read = ', NSPEC	      
      write(*,*) 'min_max_val_obs: ',         minval(spec), maxval(spec)

 

      if (nspec.eq.0) lnodata=.true.

      XKCUTOFF(:)=CUT(:)

c     calculate mean parameters
      do ispec=1,nspec
        e2 = 0.
        f2 = 0.
        th2 = 0.
        do ifre=1,nfre
          do iang=1,nang
!      if (spec(ispec,iang,ifre,2).lt.0.) spec(ispec,iang,ifre,2)=FLMIN
      if (spec(ispec,iang,ifre,2).lt.0.) spec(ispec,iang,ifre,2)=0
!      if (ifre.lt.6) spec(ispec,iang,ifre,2)=0.
            sp2(iang,ifre) = spec(ispec,iang,ifre,2)
           end do
        end do
!        call mean_pars15(sp2,e2,f2,th2)
!        etot(ispec) = e2
!        fmean(ispec) = f2
!        thq(ispec) = th2
         call peakfr_sar (sp2, fp)
        if (fp.lt.0.056) then
!write(*,*) 'long waves ', fp
        fpsar(ispec) = fp
!!        do ifre=1,nfre
!!          do iang=1,nang
!!            spec(ispec,iang,ifre,2)=0.
!!          enddo
!!        enddo
        endif
      end do


      return
      end subroutine readswispec
c------------------------------------------------------------------------
