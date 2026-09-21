!***************i******************************************************************
! 05-12-2017 DirOP/MAR
! 
! lecture des spectres CFOSAT
!
!*********************************************************************************

      program read_cfo_nc
!********************************************************************************
      implicit none


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      include 'netcdf.inc'


      INTEGER, PARAMETER :: NFREQ_WAM = 25      !  numbers of angles in WAM
      INTEGER, PARAMETER :: NANG_WAM  = 24      !  numbers of angles in WAM
      REAL,    PARAMETER :: f0_WAM = 0.04118    !  WAM first frequency
      character(len=256) :: filename,fileout
      character(len=2)   :: beam                !  beam angle
      character(len=20)  :: fmt_str

      integer   :: ncid,vid_t,dimid,i,j,ii,jj,nberr,k,l,jj0,jj02
      integer   :: ik, iphi, ipn
      integer   :: nbox,nposneg,nk,nphi,nbeaml2,ntim
      real      :: Pi=3.14,diff,u10,az,nvi,ra,snl,q1,Hs2,Hs,dphi,delta,w
      real      :: ddir

      real,dimension(1:NFREQ_WAM)             :: ff       !  WAM frequency vector
      real,dimension(1:NANG_WAM)              :: dir_wam  !  WAM direction vector
      real,dimension(1:NFREQ_WAM,1:NANG_WAM)  :: spf2, spf3     

      real,dimension(:),allocatable :: k_spectra,phi      !  wavenumber, direction in CFOSAT
      real,dimension(:),allocatable :: lam,fr,ck
      real,dimension(:,:),allocatable :: lat,lon,lat2,lon2
      real,dimension(:,:),allocatable :: spf              !  Spectra to be written in SWI file
      real,dimension(:,:),allocatable :: time, time2
      real,dimension(:,:,:,:),allocatable :: f2D, f2D2    !  CFOSAT Spectra
      character*12 :: fdate
      integer      :: an,mo,jo,ho1,mn1,se1,ibeam



      CALL get_command_argument(1,filename)
      CALL get_command_argument(2,fileout)
      CALL get_command_argument(3,beam)

!!! ------------ Check beam angle in netcdf file  --------------- !!!
!!! ------ Adjust the ibeam in lance_cfo_nc.sh accordingly ------ !!!
!!!           ibeam= 1 -> 6° | = 2 -> 8° | = 3 -> 10°             !!!

      read(beam,'(I1)')ibeam
      u10=0
      az=0
      nvi=1.6
      snl=20
      ra=170
      print*,filename,ibeam

! Open file :
! ---------------------------
      call hdlerr(NF_OPEN(filename,NF_NOWRITE,ncid))

! retrieval of dimensions
! ---------------------------
      call hdlerr(NF_INQ_DIMID(ncid,'obs',dimid))
      call hdlerr(NF_INQ_DIMLEN(ncid,dimid,nbox))

      call hdlerr(NF_INQ_DIMID(ncid,'side_looking',dimid))
      call hdlerr(NF_INQ_DIMLEN(ncid,dimid,nposneg))

      call hdlerr(NF_INQ_DIMID(ncid,'spectrum_wavenumber',dimid))
      call hdlerr(NF_INQ_DIMLEN(ncid,dimid,nk))

      call hdlerr(NF_INQ_DIMID(ncid,'spectrum_direction',dimid))
      call hdlerr(NF_INQ_DIMLEN(ncid,dimid,nphi))

! allocate arrays
! ---------------------------
      ALLOCATE (f2D (nk,nphi,nposneg,nbox))
      ALLOCATE (f2D2 (nbox,nposneg,nphi,nk))
      ALLOCATE (phi (nphi))
      ALLOCATE (k_spectra (nk))
      ALLOCATE (lat (nposneg,nbox))
      ALLOCATE (lat2 (nbox,nposneg))
      ALLOCATE (lon (nposneg,nbox))
      ALLOCATE (lon2 (nbox,nposneg))
      ALLOCATE (time (nposneg,nbox))
      ALLOCATE (time2 (nbox,nposneg))
      ALLOCATE (lam (nk))
      ALLOCATE (fr (nk))
      ALLOCATE (ck (nk))
      
!  Reverse order of dimensions
! ---------------------------
      do ik = 1, nk    
         do iphi = 1, nphi
            do ipn = 1, nposneg
               f2D2(:, ipn, iphi, ik) = f2D(ik, iphi, ipn, :)
            end do
         end do
      end do

      DEALLOCATE(f2D)
      ALLOCATE(f2D(nbox,nposneg,nphi,nk))

      f2D = f2D2    

      DEALLOCATE(f2D2)

      lat2 = transpose(lat)
      lat  = lat2
      lon2 = transpose(lon)
      lon  = lon2
      time2 = transpose(time)
      time = time2



! retrieval of variables
! ---------------------------
      call hdlerr( NF_INQ_VARID(ncid,'wavenumber_spec',vid_t)) ! spectrum wavenumber
      call hdlerr( NF_GET_VAR_REAL(ncid,vid_t,k_spectra))      ! units [rad/m]


      call hdlerr( NF_INQ_VARID(ncid,'direction_spectrum',vid_t)) ! spectrum direction
      call hdlerr( NF_GET_VAR_REAL(ncid,vid_t,phi))               ! degrees versus North clockwise


      call hdlerr( NF_INQ_VARID(ncid,'latitude',vid_t))
      call hdlerr( NF_GET_VAR_REAL(ncid,vid_t,lat))


      call hdlerr( NF_INQ_VARID(ncid,'longitude',vid_t))
      call hdlerr( NF_GET_VAR_REAL(ncid,vid_t,lon))



      call hdlerr( NF_INQ_VARID(ncid,'time',vid_t))
      call hdlerr( NF_GET_VAR_REAL(ncid,vid_t,time))


      call hdlerr( NF_INQ_VARID(ncid,'wave_spec',vid_t)) ! L3 data wave spectrum
      call hdlerr( NF_GET_VAR_REAL(ncid,vid_t,f2D))      ! These are spectra E/k with units m^2 where E is the  
                                                         ! wave variance spectrum on the wavenumber direction grid 
                                                         ! with units m³/rad


! This has to be checked!!!!!!!!!!!!!
! --------------------------------------
      do i = 1,nbox
         do j = 1,nposneg
            do k = 1, nphi
               do l = 1,nk
                  if (f2D(i,j,k,l) .LT. -990) then
                     f2D(i,j,k,l) = 0.0
                  endif    
               enddo   
            enddo   
         enddo   
      enddo

! create WAM frequency vector
! ---------------------------     
      ff(1) = f0_WAM
      do i=2,NFREQ_WAM,1
        ff(i)=1.1*ff(i-1)
      enddo
     

!!! ---  This transformation is water depths dependent, but this in neglected for the time beeing
      do i=1,nk 
        lam(i)=2*Pi/k_spectra(i)                        ! wavelength [m] 
        fr(i)=1.0/(2.0*Pi)*sqrt(9.81*k_spectra(i))      ! frequencies of CFOSAT wavenumbers
        ck(i)=4.0*Pi*sqrt(1.0/9.81)*(k_spectra(i)**1.5) ! 
      enddo

      
! height spectrum conversion
! Converts (k,phi) CFOSAT spectrum to (f,phi) grid keeping dimensions 
! ---------------------------------------------------------------------- 
      do i=1,nbox
        do j=1,nposneg
         do k=1,nphi
            if ((lat(i,j).LT.1000).AND. &
       (lon(i,j).LT.1000)) then
              do l=1,nk
                if (f2D(i,j,k,l).LT.20000) then
                  f2D(i,j,k,l)=f2D(i,j,k,l)/k_spectra(l)**2
                  f2D(i,j,k,l)=f2D(i,j,k,l)*ck(l)
                else
                  f2D(i,j,k,l)=0.00001
                endif
              enddo
            else
              do l=1,nk
                f2D(i,j,k,l)=0
              enddo
            endif
          enddo
        enddo
      enddo


! Write Spectra to Output 
! ---------------------------
      open(4,file=fileout)    ! open file 

!     ALLOCATE(spf(NFREQ_WAM,NANG_WAM))
     ALLOCATE(spf(nfreq_wam,nphi))

! interpolation du spectre
! ---------------------------
! first  - frequency interpolation
! second - direction interpolation

      do i=1,nbox        ! goes through all spectra 
        do j=1,nposneg   ! goes through all spectra (side looking) 
          spf=0
! find first WAM bin, which is in measurement grid 
          ii=1
          do while (ff(ii).lt.fr(1)) ! ff is WAM frequency , fr is CFOSAT frequency
            ii=ii+1
          enddo
          diff=999
          do l=1,nk                  ! loop over observation wave fequencies 
            if(abs(fr(l)-ff(ii)) .lt. diff) then
              diff=abs(fr(l)-ff(ii))
            else
              if(l.eq.2)then
                spf(ii,:)=f2D(i,j,:,l-1)
              else
                if(fr(l-1).gt.ff(ii))then
                  spf(ii,:)=(f2D(i,j,:,l-2)*(fr(l-1)-ff(ii))+  &
             f2D(i,j,:,l-1)*(ff(ii)-fr(l-2)))/(fr(l-1)-fr(l-2))
                else
                  spf(ii,:)=(f2D(i,j,:,l)*(ff(ii)-fr(l-1))+ &
              f2D(i,j,:,l-1)*(fr(l)-ff(ii)))/(fr(l)-fr(l-1))
                endif
              endif
              ii=ii+1
              diff=abs(fr(l)-ff(ii))
            endif
          enddo

          ! Define WAM direction vector without half bin shift!!
         
          DO iphi=1,NANG_WAM
              DIR_WAM(iphi) = DBLE(iphi-1)*360.0/DBLE(NANG_WAM)
          END DO
 
          dphi = 360.0d0/nphi
        
          do ii=1,NFREQ_WAM
            do jj=1,NANG_WAM
                ! find closest observation direction phi(jj0) to DIR_WAM(jj)

               ddir = modulo(dir_wam(jj), 360.0)
               ddir = modulo(ddir + phi(1), 360.0)
               jj0  = modulo(nint(ddir/dphi)-1, nphi) +1
               jj02 = modulo(jj0,nphi) + 1

               w = modulo(ddir - phi(jj0), 360.0)/dphi
               w = max(0.0d0, min(1.0d0,w))

              spf2(ii,jj) = spf(ii,jj0) + &
                            w*(spf(ii,jj02) - spf(ii,jj0))

!               spf2(ii,jj0) = spf(ii,jj) 
                ! for linear interpolation find intervall [phi(jj0), phi(jj1)] such that 
                ! DIR_WAM(jj) is in that intervall and then  interpolate between phi(jj0) and phi(jj1)
            end do
          end do 

! direction ambiguity
! zone masked between dir 24 and 2 and dir 12 and 14
          do ii=1,NFREQ_WAM
            do jj=1,NANG_WAM/2
              spf2(ii,jj)=max(spf2(ii,jj),spf2(ii,jj+NANG_WAM/2))
              spf2(ii,jj+NANG_WAM/2)=spf2(ii,jj) 
            enddo    
          enddo

          spf3 = 2*spf2  !!!! update

! open output file
! ---------------------------

          call number2date((time(i,j)),an,mo,jo,ho1,mn1,se1)
!          if (lon(i,j) .lt. 0) lon(i,j)=lon(i,j)+360


! delete empty spectra
! ---------------------------
          write(fmt_str, "(A,I0,A)") '(', NANG_WAM, 'F8.3)'       
          if (maxval(spf2).ge.0.001) then
            write(fdate,70) int(an),int(mo),int(jo),int(ho1),int(mn1)
   70       format(i4.4,4i2.2)
            write(4,'(a12,2f10.3,2i5)')fdate,lon(i,j),lat(i,j),i,j
            do ii=1,NFREQ_WAM
              write(4,fmt_str) (spf3(ii,jj),jj=1,NANG_WAM)    !!!!
!!!                                                    update, it ws spf2 
!!! Be careful with the formatting above, it needs to match the NANG_WAM
            enddo
          endif
  
        enddo
      enddo
      close(4)


      call hdlerr( NF_CLOSE(ncid))

      end program read_cfo_nc

!********************************************************************************
       subroutine hdlerr(istatus)
!********************************************************************************
!     Fonction retourant le message associe a la
!     valeur retour du status des NF_ fonctoins

!     Function returning the message associated with the
!     return value of the status of NF_ functions
       include 'netcdf.inc'


       integer :: istatus

       if (istatus .ne. NF_NOERR) then
          print *,'HDLERR erreur :', NF_STRERROR(istatus)
          stop 'stopped'
       endif
       return

       end subroutine hdlerr

!********************************************************************************
       subroutine number2date(nombre,an,mois,jour,heure,minut,sec)
!********************************************************************************

      integer :: yy_ref,mm_ref,dd_ref,fullday
      real    :: fracday, nombre
      integer :: an,mois,jour,heure,minut,sec
      integer, dimension(12) :: nb_jour,nb_jour_bis
      character(len=14) :: datestr
      logical :: flag


!date origine
      yy_ref=1950 !! time is 'days since 01.01.1950!
      mm_ref=01
      dd_ref=01
!nb jours au debut du mois
      nb_jour=(/31,28,31,30,31,30,31,31,30,31,30,31/)
      nb_jour_bis=(/31,29,31,30,31,30,31,31,30,31,30,31/)
        
      nombre = nombre +1

      fullday = int(nombre)
      fracday = nombre - fullday
!heures
      heure = int(fracday *24)
!minut
      minut = int(mod(fracday*24.0,1.0)*60.0)
!sec
      sec = int(mod(mod(fracday * 24.0,1.0) * 60.0,1.0) * 60.0)
      nombre = fullday

! seconde
!      sec=modulo(nombre,60)
!      nombre=(nombre-sec)/60
! minutes
!      minut=modulo(nombre,60)
!      nombre=(nombre-minut)/60
! heures
!      heure=modulo(nombre,24)
!      nombre=(nombre-heure)/24+1
! annee
      an=yy_ref
      flag=.TRUE.
! si on est dans l'annee dorigine
      if(nombre.le.365)then
        flag=.FALSE.
      endif
      if(nombre.eq.366 .and. modulo(an,4).eq.0)then
        flag=.FALSE.
      endif
      do while( flag )
        if ( modulo(an,4).eq.0) then
          nombre=nombre-366
          if ( nombre .le.365) flag=.FALSE.
        else if ( modulo(an,4).eq.3) then
          nombre=nombre-365
          if ( nombre .le. 366) flag=.FALSE.
        else
          nombre=nombre-365
          if ( nombre .le. 365) flag=.FALSE.
        endif
        an=an+1
      enddo
! mois jour
      flag=.TRUE.
      mois=1
      if(nombre.le.31) flag=.FALSE.
      do while (flag)
        if (modulo(an,4).eq.0) then
          nombre=nombre-nb_jour_bis(mois)
          if (nombre.le.nb_jour_bis(mois+1)) flag=.FALSE.
        else
          nombre=nombre-nb_jour(mois)
          if (nombre.le.nb_jour(mois+1)) flag=.FALSE.
        endif
        mois=mois+1
      enddo
      jour=nombre !+1
      return

       end subroutine number2date

!********************************************************************************

