!
!	OASIS coupling interface for WAM model
!
!	One module and an abort routine.
!	Separating the abort routine from the module
!	avoids many dependencies in the make file.
!
!	As the settings in the namcouple file are binding
!	it is avoided to steer the coupling through the
!	ordinary user input.
!	Instead, coupling settings obtained from the namcouple
!	file are returned to the WAM model.
!	A namelist file wam_oasis.nml may be used to alter
!	the default model, grid, and variable names, or to
!	switch off the coupling.
!
! ref http://www.cerfacs.fr/oa4web/papers_oasis/oasis3mct_UserGuide_2.0.pdf
!__________________________________________________________________________

MODULE WAM_OASIS_MODULE
  USE MOD_OASIS
  USE MPI,			ONLY:	MPI_COMM_RANK,MPI_COMM_WORLD,MPI_IN_PLACE,	&
					 MPI_DATATYPE_NULL,MPI_REAL,MPI_LOGICAL,	&
					 MPI_COMM_SPLIT,MPI_BARRIER,	&
					 MPI_UNDEFINED,MPI_COMM_NULL,MPI_INTEGER
  USE WAM_GENERAL_MODULE,	ONLY:	DIFDATE,INCDATE,ZPI
  USE WAM_MPI_MODULE,		ONLY:	LOCALCOMM,rank=>pelocal,petot=>petotal
  USE WAM_TIMOPT_MODULE,        ONLY:   IDELT,IDELPRO,cdatea,TOPO_RUN,CURRENT_RUN,	&
    CDA,CDATEWO,CDTA,CD_TOPO_NEW,CDCA,CD_CURR_NEW,CDTSOU,CDTPRO,L_DECOMP,COLDSTART
  USE WAM_FILE_MODULE,		ONLY:	IU06,ITEST
  USE WAM_FRE_DIR_MODULE,	ONLY:	ML,KL
  USE WAM_OUTPUT_PARAMETER_MODULE,	ONLY:	TITL_P,TITL_SCR
  USE WAM_NEST_MODULE,	        ONLY:   N_NEST,NBOUNC,n_code,IJARC,BLATC,BLNGC,N_NAME,coarse,	&
					fine,nbounf,nbinp,c_name,blngf,blatf,ijarf,ibfl,ibfr,bfw
  USE WAM_INTERFACE_MODULE, ONLY:  &
    FEMEAN, 		 	   &  !! COMPUTATION OF MEAN FREQUENCY.
    INTSPEC,		 	   &  !! INTERPOLATE A SPECTRUM.
    MEAN_DIRECTION, 	 	   &  !! COMPUTATION OF MEAN DIRECTION AND SPREAD.
    TOTAL_ENERGY		      !! COMPUTATION OF TOTAL ENERGY.

 IMPLICIT NONE
 PRIVATE					! default control variables
  LOGICAL		:: use_oasis =.TRUE.,			&
                            use_oasis_wind_in=.FALSE.,		&
			    use_oasis_curr_in=.FALSE.,		&
			    use_oasis_elev_in=.FALSE.,		&
			    use_oasis_ice_in=.FALSE.,		&
			    use_oasis_bdy_in=.FALSE.,		&
			    bdy_in=.FALSE.,			&
                            use_oasis_nest_out=.FALSE.,		&
			    use_oasis_ocea_out=.FALSE.,		&
			    use_oasis_force_source=.FALSE.,	&
			    use_oasis_force_output=.FALSE.,	&
			    force_output=.FALSE.
  CHARACTER(len=80)	:: comp_name = 'WAM'		! default model name
  INTEGER, PARAMETER	:: nvars = 22
  INTEGER, PARAMETER	:: nnest = 20			! from preproc_user_module
  INTEGER, PARAMETER	:: vgcmo=1,vgcmi=6,vatmi=9,vicei=11,vbdyi=12,vouto=13,vnsto=nvars+1
  INTEGER		:: a,iot(nvars+nnest)=[(2,a=1,5),(1,a=6,12),(2,a=13,nvars+nnest)],	&
                            comp_id,ierror,var_id(nvars+nnest)=-1,ioi(nvars+nnest),		&
	bdyuse,		    ij,b,c,nix,nr,bdycomm,nestcomm(nnest),an(nnest)=0,bn(nnest)=0,	&
 			    oux(vouto:nvars)=[(a,a=57,62),9,32,11,4]
!!			    oux(vouto:nvars)=[(a,a=57,62),9,32,1,2]	! For testing only !!
  INTEGER, PARAMETER	:: wp = SELECTED_REAL_KIND(12,307) ! double
  CHARACTER(len=80)	:: vnamen(nvars+nnest) =     			&	! default
   ['SNDWLEN','SNDPKDIR','SNDQB','SNDDISWB','SNDDISWS',			&	! variable
    'RECELEV','RECUCUR','RECVCUR','RECUWIN','RECVWIN','RECICE',		&	! names
    'RECBDYSPC',							&
    'SNDSTOKESX','SNDSTOKESY','SND1ERG2OCEAN','SND1ERG2WAVES',		&
    'SNDMOMENTUMX','SNDMOMENTUMY','SNDHS','SNDRLZ0','SNDTMP','SNDDRAGC',&
    ('SNDNSTSPC',a=1,nnest)]
  LOGICAL, ALLOCATABLE	:: oasis_output_flags(:,:)
  INTEGER, ALLOCATABLE	:: ix(:),ofs(:),rcc(:),ijarco(:)
  REAL(KIND=wp),ALLOCATABLE	:: buf(:),bug(:)
  REAL,ALLOCATABLE	:: fl3i(:,:),fl3o(:,:)
  CHARACTER(len=64)	:: gname = 'WAVE'		! default grid name
  CHARACTER(len=64)	:: gnamo = 'WAVO'		! default interior grid name
  CHARACTER(len=64)	:: bname = 'BNDI'		! boundary input
  CHARACTER(len=64),ALLOCATABLE  :: nstname(:)		! names of nest grids
  CHARACTER (LEN=8)	:: date
  CHARACTER (LEN=10)	:: time
  CHARACTER (LEN=20)	:: starttime,endtime

 PUBLIC use_oasis,						&
  use_oasis_wind_in,use_oasis_curr_in,use_oasis_elev_in,	&
  use_oasis_bdy_in,use_oasis_nest_out,				&
  use_oasis_ice_in,						&
  use_oasis_ocea_out,use_oasis_force_output,			&
  use_oasis_force_source,comp_id,comp_name,oasis_output_flags

 PUBLIC									&
  Wam_oasis_init_comp,Wam_oasis_write_grid,Wam_oasis_write_part,	&
  Wam_oasis_send_gcm,Wam_oasis_rec_topo,Wam_oasis_rec_current,		&
  Wam_oasis_rec_atmo,Wam_oasis_rec_ice,					&
  Wam_oasis_send_output_parameter,Wam_oasis_terminate,			&
  Wam_oasis_rec_boundary,Wam_oasis_send_nest,Wam_oasis_check_out


 CONTAINS
!______________________________________________
!
!	Initializing coupling with OASIS
!
!	Checking for and reading of input files
!	Handling MPI stuff
!______________________________________________
!
  SUBROUTINE Wam_oasis_init_comp
   CHARACTER(len=13)	:: nml_name = 'wam_oasis.nml'			! should be variable
   LOGICAL		:: ex
   NAMELIST /wam_oasis_nml/ use_oasis,comp_name,gname,gnamo,vnamen,bname,nstname   ! control items
    itest=9
    IF(ITEST.GE.2)WRITE(IU06,*)'Start Wam_oasis_init_comp',rank
    WRITE(vnamen(nvars+1:),'("SNDNEST",i2.2,"SPC")')[(a,a=1,nnest)]
    ALLOCATE(nstname(nnest))
    WRITE(nstname,'("NS",i2.2)')[(a,a=1,nnest)]
    CALL DATE_AND_TIME(date, time)
    WRITE(starttime,'(a9,a11)')date,time
    INQUIRE(FILE='namcouple',EXIST=ex)	! checking for namcouple
    IF(.NOT.ex)THEN			! no coupling without namcouple
      WRITE(IU06,*)TRIM(comp_name)//': No namcouple file, OASIS deactivated'
      use_oasis=.false.
      RETURN
      ENDIF
    INQUIRE(FILE=nml_name,EXIST=ex)	! checking for namelist
    IF(ex)THEN				! reading namelist
      OPEN (1,FILE=nml_name)
      READ (1,NML=wam_oasis_nml)
      CLOSE(1)
      IF(.NOT.use_oasis)THEN		! no coupling on request
	WRITE(IU06,*)TRIM(comp_name)//': Namelist requests OASIS deactivation'
	RETURN
	ENDIF
      ENDIF
    WRITE(IU06,nml=wam_oasis_nml)				! print control items
    !CALL OASIS_INIT_COMP(comp_id,comp_name,ierror,.true.)	! start coupler
    CALL OASIS_INIT_COMP(comp_id,comp_name,ierror)		! start coupler
    IF(ierror/=0)call Oaa('oasis_init_comp','oasis_init_comp failed')
    CALL OASIS_GET_LOCALCOMM(localcomm,ierror)			! MPI local communicator
    IF(ierror/=0)call Oaa('oasis_get_localcomm','oasis_get_localcomm failed')
    CALL MPI_Comm_Rank(MPI_COMM_WORLD,ij,ierror)		! get MPI global rank
    IF(ierror/=0)call Oaa('global MPI_Comm_Rank','MPI_Comm_Rank global failed')
    CALL MPI_Comm_Rank(localcomm,rank,ierror)			! get MPI local rank
    IF(ierror/=0)call Oaa('local MPI_Comm_Rank','MPI_Comm_Rank local failed')
    IF(ITEST.GE.2)THEN
      WRITE(IU06,*)'-----------------------------------------------------------'
      WRITE(IU06,*)'MPI_COMM_WORLD is :',MPI_COMM_WORLD
      WRITE(IU06,*)'localcomm is :     ',localcomm
      WRITE(IU06,*)TRIM(comp_name), ' Running with reals compiled as kind =',wp
      WRITE(IU06,*)'I am component ',TRIM(comp_name),' global rank:',ij,'local rank:',rank
      WRITE(IU06,*)'----------------------------------------------------------'
      WRITE(IU06,*)'End   Wam_oasis_init_comp',rank,ij,localcomm,MPI_COMM_WORLD
      ENDIF						! print MPI info
    END SUBROUTINE Wam_oasis_init_comp
!________________________________________________
!
!	Setup of coupling with OASIS
!
!	Obtaining and passing on grid information
!	Generating auxiliary information
!	This happens only, when used by preproc
!________________________________________________
!
  SUBROUTINE Wam_oasis_write_grid
   USE WAM_GRID_MODULE,ONLY: AMOWEP, AMOSOP, AMOEAP, AMONOP, &
		             i=>NX,j=>NY, L_S_MASK
   USE WAM_COORDINATE_MODULE,ONLY: M_SEC_TO_DEG
   INTEGER k,msk(i,j),il_flag,ijar(size(ijarc))
   DOUBLE PRECISION lon(i,j),lat(i,j),srf(i,j),clon(i,j,4), &
     clat(i,j,4),dla,dlo,wsen(6)
    wsen=M_SEC_TO_DEG([AMOWEP, AMOSOP, AMOEAP, AMONOP])
    ij=SIZE(l_s_mask)		! # of points in wave grid
    nix=COUNT(l_s_mask)		! # of sea points in wave grid
    IF(rank==0.and.comp_name==' ')THEN		! passing on grid information to coupler
      IF(ITEST.GE.6)WRITE(IU06,*)'Start Wam_oasis_write_grid'
      IF(nbounf.GT.0)WRITE(IU06,'(2x,a5,1x,a)')bname,c_name
      IF(n_nest.GT.0)WRITE(IU06,'(i2,a5,1x,a)')((a,nstname(a),N_NAME(a)),a=1,n_nest)
      IF(wsen(3)>wsen(1))THEN	! only one process is doing that
        dlo=wsen(3)-wsen(1)
      ELSE
        dlo=wsen(3)-wsen(1)+360
	ENDIF
      dlo=dlo/(i-1)			! longitude step (assumes equally spaced)
      dla=(wsen(4)-wsen(2))/(j-1)	! latitude step (assumes equally spaced)
      lon=SPREAD(wsen(1)+dlo*[(k,k=0,i-1)],2,j)	! 2D-array with longitudes
      lat=SPREAD(wsen(2)+dla*[(k,k=0,j-1)],1,i)	! 2D-array with latitudes
      WHERE(l_s_mask)		! 2D-array with mask for coupler
	msk=0			! for historical reasons 0 denotes active points
      ELSE WHERE
	msk=1			! for historical reasons 1 denotes inactive points
       END WHERE
      srf=1.d12/81.d0*dla*dlo*COS(zpi/360.d0*lat)			! area
      clon=SPREAD(SPREAD(dlo/2*[1,-1,-1,1],1,j),1,i)+SPREAD(lon,3,4)	! corners
      clat=SPREAD(SPREAD(dla/2*[1,1,-1,-1],1,j),1,i)+SPREAD(lat,3,4)	! lon,lat
      CALL oasis_start_grids_writing(il_flag)		! passing info to coupler
      CALL Write_grid(gname)
      msk(1::i-1,:)=1
      msk(:,1::j-1)=1
      CALL Write_grid(gnamo)
      if(N_NEST.gt.0)then
	DO k=1,N_NEST
          CALL Write_boundary_grid(nstname(k),dble(blngc(1:NBOUNC(k),k)),dble(blatc(1:NBOUNC(k),k)))
          END DO
        ijar=0
	do k=1,N_NEST
	  ijar((k-1)*size(ijarc,1)+1:(k-1)*size(ijarc,1)+NBOUNC(k))=ijarc(1:NBOUNC(k),k)
	  enddo
	endif
	flush(IU06)
      CALL oasis_terminate_grids_writing()
      IF(ITEST.GE.2)WRITE(IU06,*)'Wam_oasis_write_grid ',N_NEST,NBOUNC,n_code,N_NAME,NBOUNf
      CALL oasis_enddef(ierror)		! finish coupler definition phase
      IF (ierror/=0)call Oaa('oasis_enddef','oasis_enddef failed')
      IF(ITEST.GE.9)WRITE(IU06,*)'enddef'
      CALL Wam_oasis_terminate(ierror)
      ENDIF
    IF(ITEST.GE.6)WRITE(IU06,*)'End   Wam_oasis_write_grid'


    CONTAINS
      SUBROUTINE Write_grid(name)
       CHARACTER(len=4),INTENT(IN)	:: name				! grid name
        IF(ITEST.GE.9)WRITE(IU06,*)'Wam_oasis_write_grid ',name
	CALL oasis_write_grid(name,i,j,lon,lat)
	CALL oasis_write_corner(name,i,j,4,clon,clat)
	CALL oasis_write_area(name,i,j,srf)
	CALL oasis_write_mask(name,i,j,msk)
        END SUBROUTINE Write_grid
      SUBROUTINE Write_boundary_grid(name,lon,lat)
       CHARACTER(len=4),INTENT(IN)	:: name				! grid name
       DOUBLE PRECISION,INTENT(IN)	:: lon(:),lat(:)
       DOUBLE PRECISION			:: blon(ml*kl+3,size(lon)),blat(ml*kl+3,size(lat))
        IF(ITEST.GE.9)WRITE(IU06,*)'Wam_oasis_write_grid boundary ',name
        blon=spread(lon/3.6d5,1,ml*kl+3)		&
	 +spread(reshape(spread(dlo/ml*[(i,i=0,ml-1)],2,kl+1),[ml*kl+3]),2,size(lon))
        blat=spread(lat/3.6d5,1,ml*kl+3)		&
	 +spread(reshape(spread(dla/(kl+1)*[(i,i=0,kl)],1,ml),[ml*kl+3]),2,size(lat))
        CALL oasis_write_grid(name,ml*kl+3,size(lon),blon,blat)
        CALL oasis_write_mask(name,ml*kl+3,size(lon),spread(spread(0,1,ml*kl+3),2,size(lon)))
        END SUBROUTINE Write_boundary_grid
    END SUBROUTINE Wam_oasis_write_grid
!_________________________________________________________________
!
!	Setup of coupling with OASIS
!
!	Obtaining and passing on of information for each processor
!	Creating variables for coupling
!	Generating auxiliary information
!_________________________________________________________________
!
! For the coupling it is assumend, that we have a regular matrix of points.
! Oasis identifies points by their sequence in storage, starting at index 0.
!
! WAM also allows "reduced grids", for such grids coupling is not implemented.
!
! Otherwise
! WAM uses also a global point counter on the regular matrix of points, starting at 1.
! But for computations WAM uses a local set of sea points only. For the local set of sea
! points belonging to this processor and some of the land points, we will determine
! the corresponding oasis indices, such that from all processors the complete matrix
! will exactly be covered. Finally we need to know where the information from our local
! set of WAM sea points goes in the oasis partition, that will be declared below.
!
  SUBROUTINE Wam_oasis_write_part
   USE WAM_TOPO_MODULE,    		ONLY:	idelti,idelto
   USE WAM_CURRENT_MODULE, 		ONLY:	idelci,idelco
   USE WAM_WIND_MODULE,    		ONLY:	idelwi,idelwo
   USE WAM_MPI_MODULE,	   		ONLY:	nijs,nijl,ij2newij
   USE WAM_OUTPUT_PARAMETER_MODULE,	ONLY:	NOUT_P,TITL_P
   USE WAM_OUTPUT_SET_UP_MODULE,	ONLY:	CFLAG_P
   USE WAM_GRID_MODULE,			ONLY:	L_S_MASK,IXLG,KXLT,nx
   INTEGER part_id,part_bi,part_no,j,k,rw(2),ijar(size(ijarc)),rk,nd(2),ns(4),rt(nvars+nnest)
   INTEGER,ALLOCATABLE	::	iy(:),iseg(:)
    IF(ITEST.GE.6)WRITE(IU06,*)'Start Wam_oasis_write_part',rank,nijs,nijl
    rk=rank+1
				! WAM active points partitioning
    IF(petot>1)THEN			! more than one processor available
      ALLOCATE(ofs(petot),rcc(petot))	! space for offsets and lengths
    ELSE
      ALLOCATE(ofs(1))
      DEALLOCATE(ofs)
      ENDIF
!
! WAM partitioning is 1D.
!
! The sea points are counted and evenly distributed to the processors. Each oasis partition
! starts with a WAM sea point and ends where the next partition starts. For covering all 
! points the first partition includes all leading land points, if any. The last partition
! includes all trailing land points, if any. Information will go only to indices of sea points.
!
    IF(L_DECOMP)THEN		! Decomposition is 1D
      k=nix/petot			! average length
      nr=nix-k*petot			! remainder of integer division
      IF(nr>0)rcc(1:nr)=k+1		! first partitions are longer
      rcc(nr+1:)=k			! last partitions are shorter
      ofs(1)=0				! first offset is zero
      do k=1,petot-1
	ofs(k+1)=ofs(k)+rcc(k)		! offsets of the partitions in the sequence of sea points
	enddo
      ALLOCATE(iy(nix))					! space for global point counter for WAM
      iy=PACK([(k,k=1,ij)],RESHAPE(l_s_mask,[ij]))	! global point counter for WAM sea points
      IF(nijs==1)THEN		! section of all points belonging to this process
	a=1			! 1.partion has all leading land points
      ELSE
	a=iy(nijs)		! others start with 1.sea point
	ENDIF
      IF(nijl==nix)THEN
	b=ij			! last partition ends with trailing land points
      ELSE
	b=iy(nijl+1)-1		! others end before start of next partition
	ENDIF
      b=b-a+1			! length of this partition including land points
      ALLOCATE(buf(b),iseg(3),ix(nijl-nijs+1))
      ix=iy(nijs:nijl)		! copy index array part for this partition
      IF(nijs/=1)ix=ix-ix(1)+1	! index into buffer
      DEALLOCATE(iy)		! free old index
      iseg(1)=1			! oasis partition type (apple)
      iseg(2)=a-1		! start element of this partition counted from 0
      iseg(3)=b			! length of this partition
!
! WAM partitioning is 2D.
!
! The sea points are counted and distributed to the processors. The shape of the subdomains
! are somewhat irregular boxes. For covering all points the land points are counted and evenly
! distributed to the processors. Each oasis partition starts with the sea points and ends whith
! its share of land points. Both parts land and sea are sorted in ascending oasis numbering.
! Information will go only to indices of sea points but needs to be reordered.
!
    ELSE				! Decomposition is 2D (and petot > 1 !!!)
      ofs(rank+1)=nijl			! highest sea point number in the local set
      CALL MPI_ALLGATHER(MPI_IN_PLACE,0,MPI_DATATYPE_NULL, &
	ofs,1,mpi_integer,localcomm,ierror)
      rcc=ofs				! copy highest sea point numbers in all local sets
      ofs=[0,rcc(:petot-1)]		! start index of local set starting from 0
      rcc=rcc-ofs			! lengthes of all WAM sea point sets
      a=rcc(rank+1)			! number of sea points in this partition
      k=(ij-nix)/petot			! number of land points in this partition
      IF(rank<MOD(ij-nix,petot))k=k+1
      b=a+k				! total number of points in this partition (sea+land)
      ALLOCATE(buf(b),iseg(b+2))	! buffer and partition description space
      iseg(1)=4				! oasis partition type (point)
      iseg(2)=b				! length of this partition
      IF(k>0)THEN			! if there are land points in this partition
	ALLOCATE(ix(ij-nix))					! space for all land indices 
	ix=PACK([(j,j=0,ij-1)],.NOT.RESHAPE(l_s_mask,[ij]))	! oasis indices of land points
        j=(ij-nix)/petot*rank+MIN(MOD(ij-nix,petot),rank)	! land points before this partition
        iseg(a+3:)=ix(j+1:j+k)					! copy indices to description
	DEALLOCATE(ix)						! free index space
	ENDIF
      ALLOCATE(ix(nix))							! space for all sea indices 
      ix=PACK([(k,k=0,ij-1)],RESHAPE(l_s_mask,[ij]))			! oasis indices of sea points
      iseg(3:2+a)=PACK(ix,(nijs<=ij2newij(1:)).AND.(ij2newij(1:)<=nijl))! copy indices of this set
      DEALLOCATE(ix)							! free index space
      ALLOCATE(ix(a),iy(a))				! space for index for this partition
      iy=PACK([(k,k=1,nix)],(nijs<=ij2newij(1:)).AND.(ij2newij(1:)<=nijl)) ! WAM 1d sea indices
      DO k=1,a						! reordering of sea points 1d -> 2d
        ix(ij2newij(iy(k))-nijs+1)=k
        ENDDO
      DEALLOCATE(iy)					! free index space
iseg(3:)=iseg(3:)+1		! is absolutely neccessary ! don't ask me why !
      ENDIF

    CALL oasis_def_partition(part_id,iseg,ierror)	! pass partition info to coupler
    IF (ierror/=0)call Oaa('oasis_def_partition','oasis_def_partition failed')

				! define variables for coupler
    rw=[OASIS_In,OASIS_Out]	! coupling type
    iot=rw(iot)			! coupling type for each variable
    nd=[1,1]
    ns(1:2)=[1,b]
    DO k=1,nvars		! pass variable info to coupler
      IF(k<vbdyi.OR.vbdyi<k)THEN
	CALL oasis_def_var(var_id(k),vnamen(k),part_id, &
	 nd,iot(k),ns(1:2),OASIS_Real,ierror)
	IF (ierror/=0)call Oaa('oasis_def_var','oasis_def_var failed')
	ENDIF
      ENDDO
				! WAM boundary input partitioning
    nd=[2,1]
    ns=[1,1,ml*kl+3,nbinp]
    IF(NBOUNf>0)THEN		! boundary input is required
      a=COUNT(ijarf<nijs)	! number of boundary points before this process
      b=COUNT(ijarf<=nijl)	! number of boundary points before and including this process
      IF(a==b)THEN		! no boundary points in this process
	j=MPI_UNDEFINED
      ELSE			! there are boundary points in this process
        j=1
	ENDIF			! create communicator for distributing boundary data
      CALL MPI_COMM_SPLIT(localcomm,j,rank,bdyuse,ierror)	! for mpi_bcast use_oasis_bdy_in
      IF (ierror/=0)call Oaa('mpi_comm_split for bdyuse','mpi_comm_split for bdyuse failed')
      iseg(1:3)=0		! create communicator for input of boundary data
      IF(a>0.OR.b==0)THEN	! not first process with boundary input
	j=MPI_UNDEFINED
      ELSE			! first process with boundary input
        j=1
	iseg(3)=(ml*kl+3)*nbinp	! serial partition
	bdy_in=.true.
	ENDIF			! create communicator for input of boundary data
      CALL MPI_COMM_SPLIT(localcomm,j,rank,bdycomm,ierror)	! for mpi_bcast use_oasis_bdy_in
      IF (ierror/=0)call Oaa('mpi_comm_split for bdycomm','mpi_comm_split for bdycomm failed')
      IF(bdy_in)THEN		! pass communicator to oasis 
	CALL oasis_set_couplcomm(bdycomm,ierror)
	IF (ierror/=0)call Oaa('oasis_set_couplcomm for bdycomm','oasis_set_couplcomm for bdycomm failed')
	CALL oasis_def_partition(part_bi,iseg(1:3),ierror)	! pass partition info to coupler
	IF (ierror/=0)call Oaa('oasis_def_partition for bdy','oasis_def_partition for bdy failed')
	CALL oasis_def_var(var_id(vbdyi),vnamen(vbdyi),part_bi,nd,iot(vbdyi),ns,OASIS_Real,ierror)
	IF (ierror/=0)call Oaa('oasis_def_var for bdy','oasis_def_var failed for bdy')
        use_oasis_bdy_in=var_id(vbdyi)>=0	! does OASIS boundary input ?
	ENDIF
      IF(a/=b)THEN		! notify all boundary input processes (communicator bdyuse)
	CALL mpi_bcast(use_oasis_bdy_in,1,mpi_logical,0,bdyuse,ierror)
	IF (ierror/=0)call Oaa('mpi_bcast for bdy_in','mpi_bcast failed for bdy_in')
   	ENDIF
      ENDIF
    DEALLOCATE(iseg)
				! WAM nest output partitioning
    DO k=1,n_nest
      call Nestpart(ijarc(1:NBOUNC(k),k),NBOUNC(k),k,vnsto+k-1)
      ENDDO
    !IF(n_nest>0)coarse=any(n_code(1:n_nest)/=2)		! disables standard nest output

    a=nijs	! copy WAM partition lower boundary for later use
    b=nijl	! copy WAM partition upper boundary for later use

    IF(ITEST.GE.9)WRITE(IU06,*)'Nach Nestpart ',coarse,n_code(1:n_nest)
    CALL oasis_set_couplcomm(localcomm,ierror)					! seems to be essential to set couplcomm
    IF (ierror/=0)call Oaa('oasis_set_couplcomm','oasis_set_couplcomm failed')	! to localcom before enddef !!!
    IF(ITEST.GE.9)WRITE(IU06,*)'Vor oasis_enddef '
    CALL oasis_enddef(ierror)			! finish coupler definition phase
    IF (ierror/=0)call Oaa('oasis_enddef','oasis_enddef failed')
    IF(ITEST.GE.9)WRITE(IU06,*)'enddef'
      						! check which variables are in namcouple
    use_oasis_ocea_out=ANY(var_id(vgcmo:vgcmi-1)>=0)
    use_oasis_elev_in =var_id(vgcmi)>=0
    use_oasis_curr_in =ANY(var_id(vgcmi+1:vgcmi+2)>=0)
    use_oasis_wind_in =ANY(var_id(vatmi:vatmi+1)>=0)
    use_oasis_ice_in  =var_id(vicei)>=0
    use_oasis_force_output=ANY(var_id(vouto:nvars)>=0)
    use_oasis_force_source=ANY(var_id(vouto+2:nvars)>=0)
    force_output      =ANY(var_id(vouto:nvars)>=0)
    use_oasis_nest_out    =ANY(var_id(vnsto:)>=0)

    if(.not.use_oasis_ice_in)deallocate(ofs)	! offsets are (yet) only needed for ice input

    DO k=1,nvars+n_nest				! get i/o-intervals
      ioi(k)=Get_freq_info(var_id(k))
      ENDDO
						! check exchange times
    rt=[(1,k=1,5),(2,k=6,nvars+nnest)]
    rw=[idelt,idelpro]
    rt=rw(rt)
    rt=mod(ioi,rt)

    IF(use_oasis_wind_in)THEN			! set WAM model variables
      cda=cdatea				! for wind input
      cdatewo=cda
      CALL INCDATE(cdatewo,ioi(var_id(vatmi)))
      idelwi=ioi(var_id(vatmi))
      IF(idelwi>idelwo)idelto=3600
      ALLOCATE(bug(size(buf)))			! buffer for v
      ENDIF
    IF(use_oasis_elev_in)THEN			! set WAM model variables
      cdta=cdatea				! for water level
      cd_topo_new=cdta
      CALL INCDATE(cd_topo_new,ioi(var_id(vgcmi)))
      idelti=ioi(var_id(vgcmi))
      IF(idelti>idelto)idelto=3600
      ENDIF
    IF(use_oasis_curr_in)THEN			! set WAM model variables
      cdca=cdatea				! for current
      cd_curr_new=cdca
      CALL INCDATE(cd_curr_new,ioi(var_id(vgcmi+2)))
      idelci=ioi(var_id(vgcmi+1))
      IF(idelci>idelco)idelco=3600
      ENDIF
    IF(use_oasis_force_output)THEN		! set flags for WAM outputs
      ALLOCATE(oasis_output_flags(nout_p,3))
      oasis_output_flags(:,1)=.false.
      oasis_output_flags(oux,1)=var_id(vouto:nvars)>=0
      oasis_output_flags(:,2)=cflag_p
      oasis_output_flags(:,3)=oasis_output_flags(:,1)
      ENDIF
    IF(use_oasis_nest_out)THEN			! allocate buffer for nestoutput
      ALLOCATE(fl3o(kl*ml+3,maxval(bn-an,mask=var_id(vnsto:)>=0)))
      ENDIF
    IF(ITEST.GE.9.OR.maxval(rt)>0)then
      WRITE(IU06,*)'Oasis configuration'
      WRITE(IU06,*)'Model name: '//TRIM(comp_name)
      WRITE(IU06,*)'Time step source:      ',idelt
      WRITE(IU06,*)'Time step propagation: ',idelpro
      WRITE(IU06,*)'Boundary send receive: ',coarse,fine
      WRITE(IU06,*)'Variables:'
      DO k=1,nvars+n_nest
	WRITE(IU06,'(2i3,2i6,i3,1x,a)')k,iot(k),ioi(k),rt(k),var_id(k),trim(vnamen(k))
	ENDDO
      WRITE(IU06,*)'Communicators:'
      WRITE(IU06,*)MPI_COMM_WORLD,'MPI_COMM_WORLD'
      WRITE(IU06,*)localcomm,'localcomm'
      WRITE(IU06,*)bdycomm,'bdycomm'
      WRITE(IU06,*)bdyuse,'bdyuse'
      DO k=1,n_nest
        WRITE(IU06,*)k,nestcomm(k),'nestcomm'
	ENDDO
      WRITE(IU06,*)'Switches:'
      WRITE(IU06,*)use_oasis,'use_oasis'
      WRITE(IU06,*)use_oasis_wind_in,'use_oasis_wind_in'
      WRITE(IU06,*)use_oasis_curr_in,'use_oasis_curr_in'
      WRITE(IU06,*)use_oasis_elev_in,'use_oasis_elev_in'
      WRITE(IU06,*)use_oasis_ice_in,'use_oasis_ice_in'
      WRITE(IU06,*)use_oasis_bdy_in,'use_oasis_bdy_in'
      WRITE(IU06,*)use_oasis_nest_out,'use_oasis_nest_out'
      WRITE(IU06,*)use_oasis_ocea_out,'use_oasis_ocea_out'
      WRITE(IU06,*)use_oasis_force_source,'use_oasis_force_source'
      WRITE(IU06,*)use_oasis_force_output,'use_oasis_force_output'
      ENDIF
    IF(MAXVAL(rt)>0)then
      WRITE(IU06,*)'check iotimes abort by model compid ',comp_id,idelt,idelpro
      CALL OASIS_ABORT(comp_id,comp_name,'iotimes no compatible')
      endif
    a=nijs
    b=nijl
    c=nijl-nijs+1
    IF(ITEST.GE.6)WRITE(IU06,*)'End   Wam_oasis_write_part',rank,use_oasis_ocea_out,use_oasis_elev_in,use_oasis_curr_in,use_oasis_wind_in

    CONTAINS
      SUBROUTINE Nestpart(ijar,n,i1,i2)
       INTEGER,INTENT(IN)	::	ijar(n),n,i1,i2
       INTEGER			::	a,b,k,p,mk3
       INTEGER,ALLOCATABLE	::	iseg(:)
	mk3=ml*kl+3		! length of one spectrum and three integrated values
	IF(L_DECOMP)THEN	! Decomposition is 1D
	  ALLOCATE(iseg(5))
	  a=COUNT(ijar<nijs)	! number of boundary points before this process
	  b=COUNT(ijar<=nijl)	! number of boundary points before and including this process
	  IF(a==b)THEN		! no boundary points in this process
            iseg=0
	    k=MPI_UNDEFINED
	  ELSE			! there are boundary points in this process
            k=1
	    iseg(1)=2		! oasis partition type (box)
	    iseg(2)=a*mk3	! upper left counted from 0
	    iseg(3)=mk3		! local extent in x
	    iseg(4)=b-a		! local extent in y
	    iseg(5)=mk3		! global extent in x
	    ENDIF
	ELSE			! Decomposition is 2D
	  a=COUNT(ij2newij(ijar)<nijs)	! number of boundary points before this process
	  b=COUNT(ij2newij(ijar)<=nijl)	! boundary points before and including this process
	  ALLOCATE(iseg(MAX(5,2*(b-a)+2)))	! space for partition description
	  IF(a==b)THEN		! no boundary points in this process
            iseg=0
	    k=MPI_UNDEFINED
	  ELSE			! there are boundary points in this process
	    iseg(1)=3		! oasis partition type (orange)
	    iseg(2)=b-a		! number of segments (<=200)
	    iseg(3:)=mk3	! segments local extent
	    iseg(3::2)=mk3*PACK([(k,k=0,n-1)],(nijs<=ij2newij(ijar)).AND.(ij2newij(ijar)<=nijl))
            k=1
	    ENDIF
	  ENDIF			! create communicator for output of boundary data for this nest
        CALL oasis_create_couplcomm(k,localcomm,nestcomm(i1),ierror)
	IF (ierror/=0)call Oaa('oasis_create_couplcomm','oasis_create_couplcomm failed')
        CALL oasis_def_partition(p,iseg,ierror)	! pass partition info to coupler
	IF (ierror/=0)call Oaa('oasis_def_partition','oasis_def_partition failed')
	IF(a<b)THEN		! output for this nest is done in this process
	  ns(4)=b-a		! define OASIS variable for boundary output
	  CALL oasis_def_var(var_id(i2),vnamen(i2),p, &
	   nd,iot(i2),ns,OASIS_Real,ierror)
	  IF (ierror/=0)call Oaa('oasis_def_var','oasis_def_var failed')
          ENDIF
	an(i1)=a
	bn(i1)=b
	DEALLOCATE(iseg)
      END SUBROUTINE Nestpart
    END SUBROUTINE Wam_oasis_write_part
!__________________________________________________
!
!	Set output parameter flag for next timestep
!__________________________________________________
!
  SUBROUTINE Wam_oasis_check_out
   INTEGER		:: isec,k
    IF(force_output)THEN
      CALL DIFDATE(CDATEA,CDTpro,isec)		! seconds since model start
      IF(ITEST.GE.9)WRITE(IU06,*)'Start Wam_oasis_check_out',rank,CDTpro,isec
      DO k=vouto,nvars
        IF(oasis_output_flags(oux(k),3))THEN
	  call oasis_put_inquire(var_id(k),isec,ierror)
	  IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Sent)	&
	    call Oaa('oasis_put_inquire for output','oasis_put_inquire failed')
          oasis_output_flags(oux(k),1)=ierror/=OASIS_Ok
	  ENDIF
        ENDDO
      use_oasis_force_output=ANY(oasis_output_flags(oux,1))
      use_oasis_force_source=ANY(oasis_output_flags(oux(vouto+2:vouto+5),1))
      IF(ITEST.GE.9)WRITE(IU06,*)'End   Wam_oasis_check_out',rank,CDTpro,isec, &
       use_oasis_force_source,use_oasis_force_output,oasis_output_flags(oux,1)
      ENDIF
    END SUBROUTINE Wam_oasis_check_out
!_____________________________________________________
!
!	Receiving U10 wind for this process with OASIS
!
!	Obtaining wind components [m/s]
!	Writing WAM model variables with
!	wind speed and direction
!_____________________________________________________
!
  SUBROUTINE Wam_oasis_rec_atmo
   USE WAM_MODEL_MODULE,ONLY: U10,UDIR
   INTEGER		:: isec
    CALL DIFDATE(CDATEA,CDTpro,isec)		! seconds since model start
    IF(ITEST.GE.7)WRITE(IU06,*)'Start Wam_oasis_rec_atmo',rank,CDTpro,isec
    IF(var_id(vatmi)>=0)THEN
      CALL oasis_get(var_id(vatmi),isec,buf,ierror) ! only u wind for this process
      IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Recvd)	&
        call Oaa('oasis_get for wind u','oasis_get u10 failed')
      CALL oasis_get(var_id(vatmi+1),isec,bug,ierror) ! only v wind for this process
      IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Recvd)	&
        call Oaa('oasis_get for wind v','oasis_get v10 failed')
      IF(ierror/=OASIS_Ok)THEN			! if data obtained
        IF(ITEST.GE.6)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_atmo u10 v10 ',TRIM(comp_name),isec,ierror
	cda=cdatea				! fill in WAM model variables
	CALL INCDATE(cda,isec)
	cdatewo=cda
	CALL INCDATE(cdatewo,ioi(var_id(vatmi)))
	IF((isec>0).OR.coldstart)then			! isec=0 -> wind field from restart
          IF(petot>1)THEN				! several processes
	    u10(a:b)=SQRT(buf(ix)**2 + bug(ix)**2)	! wind speed for this process, no halo
	    WHERE(u10(a:b)>0)				! wind direction
	      udir(a:b)=ATAN2(buf(ix),bug(ix))
	    ELSE WHERE
	      udir(a:b)=0.
	      END WHERE
	    WHERE(udir(a:b)<0)udir(a:b)=udir(a:b)+zpi
          ELSE						! only one process
	    u10=SQRT(buf(ix)**2 + bug(ix)**2)		! wind speed
	    WHERE(u10>0)				! wind direction
	      udir=ATAN2(buf(ix),bug(ix))
	    ELSE WHERE
	      udir=0.
	      END WHERE
	    WHERE(udir<0)udir=udir+zpi
	    ENDIF
	ELSE						! at start time take restart if available
          IF(ITEST.GE.6)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_atmo values discarded'
          ENDIF
        ENDIF
      ENDIF
    IF(ITEST.GE.7)WRITE(IU06,*)'End   Wam_oasis_rec_atmo',rank,CDTpro,isec,ierror
    END SUBROUTINE Wam_oasis_rec_atmo
!_____________________________
!
!	Reading boundary input
!_____________________________
!
  SUBROUTINE Wam_oasis_rec_boundary
   USE WAM_MODEL_MODULE,ONLY	: FL3, DEPTH
   USE WAM_MPI_MODULE,ONLY	: nijs,nijl
   REAL			:: DEL1L, FMEAN, EMEAN, THQ
   REAL,allocatable	:: buf(:,:)
   INTEGER		:: isec,ij,ijf,IBCL,IBCR,km
   LOGICAL		:: gotdata,fl3iavail=.false.
    km=kl*ml
    CALL DIFDATE(cdatea,CDTPRO,isec)		! seconds since model start
    IF(ITEST.GE.7)WRITE(IU06,*)'Start Wam_oasis_rec_boundary ',CDTPRO,isec,bdy_in,bdyuse
    if(bdy_in)then
      allocate(buf(km+3,nbinp))
      CALL oasis_get(var_id(vbdyi),isec,buf,ierror)
      IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Recvd)	&
	call Oaa('oasis_get for boundary input','oasis_get boundary input failed')
      gotdata=ierror/=OASIS_Ok			! if data obtained
      if(gotdata)then
        if(.not.fl3iavail)then
          if(allocated(fl3i))deallocate(fl3i)
          allocate(fl3i(ml*kl+3,nbinp))
          fl3iavail=.true.
	  endif
        fl3i=buf
	endif
      deallocate(buf)
      ENDIF
    call mpi_bcast(gotdata,1,mpi_logical,[0],bdyuse,ierror)
    IF(gotdata)THEN
      if(.not.fl3iavail)then
        if(allocated(fl3i))deallocate(fl3i)
        allocate(fl3i(ml*kl+3,nbinp))
        fl3iavail=.true.
	endif
      call mpi_bcast(fl3i,size(fl3i),mpi_real,[0],bdyuse,ierror)
      IF(ierror/=0)call Oaa('mpi_bcast for boundary input','mpi_bcast boundary input failed')
      IF(ITEST>=6)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_boundary ',TRIM(comp_name),isec,ierror
      ENDIF
    call mpi_barrier (bdyuse, ierror)
    IF(fl3iavail)THEN
      DO IJ = 1,NBOUNF
	IJF = IJARF(IJ)
	IF(IJF.GE.nijs .and. IJF.LE.nijl) THEN
          IBCL = IBFL(IJ)
          IBCR = IBFR(IJ)
          DEL1L = BFW(IJ)
          CALL INTSPEC (1., DEL1L,								&
	   reshape(fl3i(:km,IBCL),[kl,ml]),fl3i(km+3,IBCL),fl3i(km+1,IBCL),fl3i(km+2,IBCL),	&
	   reshape(fl3i(:km,IBCR),[kl,ml]),fl3i(km+3,IBCR),fl3i(km+1,IBCR),fl3i(km+2,IBCR),	&
	   FL3(IJF,:,:), FMEAN, EMEAN, THQ)
          ENDIF
        ENDDO
    ELSE
      ij=count(ijarf.ge.nijs.and.ijarf.le.nijl)
      if(ij.gt.0)then
	if(allocated(fl3i))then
          fl3(pack(ijarf,ijarf.ge.nijs.and.ijarf.le.nijl),:,:)=reshape(fl3i,[ij,kl,ml])
	else
          allocate(fl3i(ij,km))
          fl3i=reshape(fl3(pack(ijarf,ijarf.ge.nijs .and.ijarf.le.nijl),:,:),[ij,km])
          endif
        endif
      ENDIF
    IF(ITEST.GE.7)WRITE(IU06,*)'End   Wam_oasis_rec_boundary ',CDTPRO,isec
    END SUBROUTINE Wam_oasis_rec_boundary
!_________________________________________________
!
!	Sending boundary output to nested model(s)
!_________________________________________________
!
  SUBROUTINE Wam_oasis_send_nest
   USE WAM_MODEL_MODULE,         ONLY: FL3, DEPTH
   USE WAM_MPI_MODULE,		 ONLY: nijs,nijl
   INTEGER		:: isec,k
    CALL DIFDATE(cdatea,CDTPRO,isec)		! seconds since model start
    IF(ITEST.GE.7)WRITE(IU06,*)'Start Wam_oasis_send_nest ',CDTPRO,isec
    DO k=1,n_nest
      IF(var_id(vnsto+k-1)>=0)THEN
	CALL oasis_put_inquire(var_id(vnsto+k-1),isec,ierror)
	IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Sent)				&
	  call Oaa('oasis_put_inquire for nest','oasis_put_inquire failed')
	IF(ierror.NE.OASIS_Ok)THEN
          IF(L_DECOMP)THEN			! Decomposition is 1D
            CALL Sendnest(ijarc(an(k)+1:bn(k),k),bn(k)-an(k),vnsto+k-1)
	  ELSE					! Decomposition is 2D
            CALL Sendnest(PACK(ijarc(:,k),(nijs<=ijarc(:,k))	&
	                 .AND.(ijarc(:,k)<=nijl)),bn(k)-an(k),vnsto+k-1)
	    ENDIF
	  ENDIF
	ENDIF
      ENDDO
    IF(ITEST.GE.7)WRITE(IU06,*)'End   Wam_oasis_send_nest ',CDTPRO,isec

    CONTAINS
      SUBROUTINE Sendnest(ijar,j,i)
       INTEGER,INTENT(IN)	::	ijar(j),i,j
        fl3o(1:ml*kl,:j)=reshape(fl3(ijar,:,:),[kl*ml,j],order=[2,1])	!fl3o=(1.+rank/10.)*fl3o
	CALL TOTAL_ENERGY(fl3(ijar,:,:),fl3o(ml*kl+1,:j))
	CALL FEMEAN(fl3(ijar,:,:),fl3o(ml*kl+1,:j),FM=fl3o(ml*kl+3,:j))
	CALL MEAN_DIRECTION(fl3(ijar,:,:),THQ=fl3o(ml*kl+2,:))
	CALL oasis_put(var_id(i),isec,fl3o(:,:j),ierror)	! only values for this process
	IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Sent)	&
	  call Oaa('oasis_put','oasis_put nest failed')
	IF(ierror/=OASIS_Ok.AND.ITEST.GE.6) &
	  WRITE(IU06,*)'SUBROUTINE Wam_oasis_send_nest data sent at ',isec,ierror,TRIM(vnamen(i))
      END SUBROUTINE Sendnest
    END SUBROUTINE Wam_oasis_send_nest
!________________________________________________________
!
!	Receiving water level for this process with OASIS
!
!	Obtaining water level [m]
!	Writing WAM model variable with water level
!	mean water depth will be added after this routine
!________________________________________________________
!
  SUBROUTINE Wam_oasis_rec_topo(cdat,gotfield)
   USE WAM_MODEL_MODULE,ONLY: DEPTH
   USE wam_mpi_comp_module,ONLY: mpi_exchng
   CHARACTER (LEN=14)	:: cdat
   INTEGER		:: isec
   LOGICAL,INTENT(Out)  :: gotfield
   CHARACTER (LEN=10)	:: date,time
    CALL DIFDATE(cdatea,cdat,isec)	! seconds since model start
    IF(ITEST.GE.7)WRITE(IU06,*)'Start Wam_oasis_rec_topo',rank,cdat,isec,gotfield
    gotfield=.false.
    CALL oasis_get(var_id(vgcmi),isec,buf,ierror)   ! only depth for this process
    IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Recvd)	&
      call Oaa('oasis_get for water level z','oasis_get z failed')
    IF(ierror/=OASIS_Ok)THEN	! if data obtained
      IF(ITEST.GE.6)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_topo ',cdat,isec,ierror
      cdta=cdatea		! fill in WAM model variables
      CALL INCDATE(cdta,isec)
      cd_topo_new=cdta
      CALL INCDATE(cd_topo_new,ioi(var_id(vgcmi)))
      IF((isec>0).OR.coldstart)then	! map oasis data to wam partition
	IF(petot>1)THEN
	  depth(a:b)=buf(ix)		! set water level for this process
	  call mpi_exchng(depth)	! get water level for halo of this process
	ELSE
	  depth=buf(ix)
          ENDIF
        gotfield=.true.
      ELSE				! at starttime take restart depths if available
        IF(ITEST.GE.6)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_topo values discarded'
	ENDIF
      ENDIF
    IF(ITEST.GE.7)WRITE(IU06,*)'End   Wam_oasis_rec_topo',rank,cdat,isec,ierror
    END SUBROUTINE Wam_oasis_rec_topo
!____________________________________________________
!
!	Receiving current for this process with OASIS
!
!	Obtaining current components [m/s]
!	Writing WAM model variables with
!	current components
!____________________________________________________
!
  SUBROUTINE Wam_oasis_rec_current(cdat,gotfield)
   USE WAM_MODEL_MODULE,ONLY: U,V
   USE wam_mpi_comp_module,ONLY: mpi_exchng
   CHARACTER (LEN=14)	:: cdat
   INTEGER		:: isec
   LOGICAL,INTENT(Out)  :: gotfield
    CALL DIFDATE(cdatea,cdat,isec)		! seconds since model start
    IF(ITEST.GE.7)WRITE(IU06,*)'Start Wam_oasis_rec_current',rank,cdat,isec
    gotfield=.false.
    CALL oasis_get(var_id(vgcmi+1),isec,buf,ierror)   ! only u for this process
    IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Recvd)	&
      call Oaa('oasis_get for current u','oasis_get u failed')
    IF(ierror/=OASIS_Ok)THEN		! if data obtained
      if((isec>0).or.coldstart)then	! map oasis data to wam partition
	IF(petot>1)then			! treat variable u
          u(a:b)=buf(ix)		! set u for this process
	  call mpi_exchng(u)		! get u for halo of this process
	ELSE
          u=buf(ix)
          ENDIF
	ENDIF
      gotfield=.true.
      ENDIF
    CALL oasis_get(var_id(vgcmi+2),isec,buf,ierror)   ! only v for this process
    IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Recvd)	&
      call Oaa('oasis_get for current v','oasis_get v failed')
    IF(ierror/=OASIS_Ok)THEN		! if data obtained
      IF(ITEST.GE.6)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_current ',cdat,isec,ierror
      cdca=cdatea			! fill in WAM model variables
      CALL INCDATE(cdca,isec)
      cd_curr_new=cdca
      CALL INCDATE(cd_curr_new,ioi(var_id(vgcmi+2)))
      if((isec>0).or.coldstart)then	! map oasis data to wam partition
	IF(petot>1)THEN			! treat variable v
          v(a:b)=buf(ix)		! set v for this process
	  call mpi_exchng(v)		! get v for halo of this process
	ELSE
          v=buf(ix)
          ENDIF
        IF(.not.gotfield)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_current only got v but not u !'
        gotfield=.true.
      else
        IF(ITEST.GE.6)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_current values discarded'
        gotfield=.false.
	ENDIF
      ENDIF
    IF(ITEST.GE.7)WRITE(IU06,*)'End   Wam_oasis_rec_current',rank,cdat,isec,ierror
    END SUBROUTINE Wam_oasis_rec_current
!____________________________________________________________
!
!	Receiving ice information for this process with OASIS
!
!	Obtaining ice information
!	Writing WAM model variables with
!	ice information
!____________________________________________________________
!
  SUBROUTINE Wam_oasis_rec_ice
   USE WAM_grid_MODULE,ONLY: nsea
   USE WAM_ICE_MODULE,ONLY:  N_ICE,IJ_ICE
   INTEGER		:: k,isec
   INTEGER, ALLOCATABLE :: loi(:)
    CALL DIFDATE(CDATEA,CDTpro,isec)			! seconds since model start
    IF(ITEST.GE.7)WRITE(IU06,*)'Start Wam_oasis_rec_ice',rank,CDTpro,isec
    CALL oasis_get(var_id(vicei),isec,buf,ierror)	! only ice for this process
    IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Recvd)	&
      call Oaa('oasis_get for ice information',		&
	       'oasis_get ice failed')
    IF(ierror/=OASIS_Ok)THEN				! if data obtained fill in WAM model variables
      IF(ITEST.GE.6)WRITE(IU06,*)'SUBROUTINE Wam_oasis_rec_ice ',CDTpro,isec,ierror,COUNT(buf(ix)>0)
      ALLOCATE(loi(nsea))				! get work space for ice
      IF(ALLOCATED(IJ_ICE))THEN				! consider present ice information for merge
        loi=0						! preset no ice
        loi(IJ_ICE)=1					! fill in present ice
        WHERE(buf(ix)==0) buf(ix)=loi(a:b)		! if oasis icevalue is zero set it to present ice
        DEALLOCATE(IJ_ICE)
	ENDIF
      WHERE(buf(ix)>=0.5) buf(ix)=1			! set wam ice flag if oasis icevalue at least .5
      WHERE(buf(ix)< 0.5) buf(ix)=0			! unset wam ice flag if oasis icevalue less .5
      IF(petot>1)THEN					! needs to collect ice from other processors
	loi(a:b)=buf(ix)				! copy current process ice from oasis buffer
	CALL MPI_ALLGATHERV(MPI_IN_PLACE,0,MPI_DATATYPE_NULL, &
	 loi,rcc,ofs,mpi_integer,localcomm,ierror)	! gather ice data from all processors
	IF(ierror/=0)	&
	  call Oaa('MPI_ALLGATHERV for ice','MPI_ALLGATHERV ice failed')
      ELSE						! no need to collect ice from other processors
        loi=buf(ix)					! copy ice from oasis buffer
        ENDIF
      N_ICE=COUNT(loi==1)				! count ice points
      if(N_ICE>0)then					! check if ice information is needed
        ALLOCATE(IJ_ICE(N_ICE))				! space for ice information
        IJ_ICE=PACK([(k,k=1,nsea)],loi==1)		! store wam point numbers of ice points
	endif
      DEALLOCATE(loi)					! free work space for ice
      ENDIF
    IF(ITEST.GE.7)WRITE(IU06,*)'End   Wam_oasis_rec_ice',rank,CDTpro,isec,ierror,N_ICE
    END SUBROUTINE Wam_oasis_rec_ice
!_____________________________________________________________
!
!	Sending source parameters from this process with OASIS
!_____________________________________________________________
!
  SUBROUTINE Wam_oasis_send_gcm(cdat,SOURCE_ARRAY)
   CHARACTER (LEN=14)	:: cdat
   REAL			:: SOURCE_ARRAY(:,:)
   INTEGER		:: iwk,k,sax(vgcmo:vgcmo+4)=[10,9,7,5,4],ierr(vgcmo:vgcmi-1),isec
    CALL DIFDATE(cdatea,cdat,isec)		! seconds since model start
    IF(ITEST.GE.7)WRITE(IU06,*)'Start Wam_oasis_send_gcm ',rank,cdat,isec
    buf=-999.d0
    ierr=OASIS_Ok
    DO k=lbound(sax,1),ubound(sax,1)
      IF(var_id(k)>=0)THEN
	buf(ix)=SOURCE_ARRAY(a:b,sax(k))
	IF(ITEST.GE.9)WRITE(IU06,*)k,sax(k),MINVAL(buf(ix)),MAXVAL(buf(ix))
	CALL oasis_put(var_id(k),isec,buf,ierror)	! only values for this process
	IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Sent)	&
	  call Oaa('oasis_put for gcm','oasis_put gcm failed')
	ENDIF
	ierr(k)=ierror
      END DO
    IF(ANY(ierr/=OASIS_Ok).AND.ITEST.GE.6) &
      WRITE(IU06,*)'SUBROUTINE Wam_oasis_send_gcm ',isec,ierror,OASIS_Ok
    IF(ITEST.GE.7)WRITE(IU06,*)'End   Wam_oasis_send_gcm ',rank,cdat,isec,ierror
    END SUBROUTINE Wam_oasis_send_gcm
!_____________________________________________________________
!
!	Sending output parameters from this process with OASIS
!_____________________________________________________________
!
  SUBROUTINE Wam_oasis_send_output_parameter(block)
   !USE WAM_MODEL_MODULE,ONLY: Z0
   INTEGER		:: k,isec,ierr(vouto:nvars)
   REAL			:: block(:,:)
    CALL DIFDATE(cdatea,CDTPRO,isec)		! seconds since model start
    IF(ITEST.GE.7)WRITE(IU06,*)'Start Wam_oasis_send_output_parameter ',CDTPRO,isec
    !buf=-999.d0					!?buf=-rank-1-[(k,k=1,size(buf))]/10000.
    ierr=OASIS_Ok
    DO k=lbound(oux,1),ubound(oux,1)
      IF(oasis_output_flags(oux(k),1))THEN
	IF(ITEST.GE.9)WRITE(IU06,*)trim(vnamen(k)),trim(titl_p(oux(k))),MINVAL(block(:,oux(k))),MAXVAL(block(:,oux(k)))
	buf(ix)=block(:,oux(k))				!?buf(ix)=rank+1+[(k,k=1,size(ix))]/10000.
	CALL oasis_put(var_id(k),isec,buf,ierror)	! only values for this process
	IF(ierror.NE.OASIS_Ok.AND.ierror.LT.OASIS_Sent)			&
	  call oaa('oasis_put in Wam_oasis_send_output_parameter',	&
		   'oasis_put in Wam_oasis_send_output_parameter failed')
	ENDIF
	ierr(k)=ierror
      END DO
    IF(ALL((ierr==OASIS_Ok).OR.(ierr==OASIS_Sent)).AND.ITEST.GE.6) &
      WRITE(IU06,*)'SUBROUTINE Wam_oasis_send_output_parameter ',isec,ierr,OASIS_Ok
    IF(ITEST.GE.7)WRITE(IU06,*)'End   Wam_oasis_send_output_parameter',CDTPRO,isec
    END SUBROUTINE Wam_oasis_send_output_parameter
!________________________________________________
!
!	Normal termination of this process
!
!	Finishing this process of the WAM model,
!	but let all processes of all
!	coupled models continue to finish as well
!________________________________________________
!
  SUBROUTINE Wam_oasis_terminate(ierr)
   INTEGER ierr
    CALL DATE_AND_TIME(date, time)
    WRITE(endtime,'(a9,a11)')date,time
    WRITE(IU06,*)'REGULAR END OF OASIS COUPLED RUN ',trim(comp_name),rank,starttime,endtime
    CALL oasis_terminate(ierror)
    IF (ierror /= 0) THEN
      WRITE(IU06,*)'oasis_terminate abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'oasis_terminate failed')
      ENDIF
    DEALLOCATE(ix)
    ierr=ierror
    END SUBROUTINE Wam_oasis_terminate
!____________________________________________
!
!	Obtain input/output interval
!
!	Obtain shortest input/output interval
!	for the given variable
!____________________________________________
!
  INTEGER FUNCTION Get_freq_info(vi)
   INTEGER,INTENT(IN)::vi
   INTEGER ncpl,kin,mot(1)
   INTEGER,ALLOCATABLE::cplf(:)
    IF(vi<0)THEN				! variable inactive
      Get_freq_info=0				! return zero
    ELSE
      CALL Oasis_get_ncpl(vi,ncpl,kin)		! number of couplings
      IF(ncpl>0)THEN				! where this variable
	ALLOCATE(cplf(ncpl))			! is involved
	mot=pack(iot,var_id.eq.vi)
	!CALL Oasis_get_freqs(vi,mot(1),ncpl,cplf,kin)	! get the time steps
	CALL Oasis_get_freqs(vi,ncpl,cplf,kin)	! get the time steps
        IF(ITEST.GE.5)WRITE(IU06,*)'Oasis Coupling intervals for '//pack(vnamen,var_id.eq.vi)//':',cplf
	Get_freq_info=cplf(1)
	DO kin=2,ncpl
	  Get_freq_info=GGT(Get_freq_info,cplf(kin))
	  ENDDO
	DEALLOCATE(cplf)
      ELSE
	Get_freq_info=0				! return zero
	ENDIF
      ENDIF
    CONTAINS
      INTEGER FUNCTION GGT(m,n)
       INTEGER,INTENT(in)::m,n
       INTEGER j,r
	IF(m<n)THEN
          ggt=m;j=n
	ELSE
          j=m;ggt=n
	  ENDIF
	DO
          r=MOD(j,ggt)
	  IF(r==0)RETURN
	  j=ggt;ggt=r
	  ENDDO
	END FUNCTION GGT
    END FUNCTION Get_freq_info
!_______________________________
!
!	Abort due to OASIS error
!
!	Send error messages to
!	WAM and OASIS log files
!_______________________________
!
    SUBROUTINE Oaa(msgwamlog,msgoasislog)
     CHARACTER*(*) msgwamlog,msgoasislog
      WRITE(IU06,*)msgwamlog,' abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,msgoasislog)
    END SUBROUTINE Oaa
  END MODULE WAM_OASIS_MODULE
!
!###############################################################################
!
!	Abormal termination of this process
!
!	Stopping this process of the WAM model,
!	and killing all processes of all coupled models.
!	This is an emergency exit.
!_______________________________________________________
!
SUBROUTINE Wam_oasis_abort
 USE MOD_OASIS
 USE WAM_OASIS_MODULE, ONLY: comp_id,comp_name
  CALL OASIS_ABORT(comp_id,comp_name,'For the reason see WAM logfiles')
  END SUBROUTINE Wam_oasis_abort
