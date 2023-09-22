PROGRAM standard
USE netcdf
USE omp_lib
IMPLICIT NONE

REAL, parameter :: grav = 9.80665000, pi = 3.14159265, naut=1.85200000
REAL, parameter :: novalue = -999.
REAL            :: novalue_in=novalue

CHARACTER (len=*), parameter :: FNAME_LIST="list"
CHARACTER (len=*), parameter :: FNAME_INFa="input.nc"
CHARACTER (len=*), parameter :: FNAME_OUT1="output.nc"

CHARACTER (len=35)  :: TIME_SECS="seconds since 2000-01-01 00:00:00.0"
CHARACTER (len=33)  :: TIME_HOUR="hours since 2000-01-01 00:00:00.0"
CHARACTER (len=32)  :: TIME_DAYS="days since 2000-01-01 00:00:00.0"
CHARACTER (len=800) :: CDUMMY

INTEGER :: status, ioerror
INTEGER :: ncid(10), varid(10,50), dimid(10,10), NN(10,10), ON(10,10)=1

INTEGER :: NX, NY, ND, NT, NV, NX0, NY0, ND0, NT0, NV0, NX1, NY1, ND1, NT1, NV1, NX2, NY2, ND2, NT2, NV2
INTEGER :: n, n0, n1, n2, x, x0, x1, x2, y, y0, y1, y2, d, d0, d1, d2, t, t0, t1, t2
REAL    :: sval, sval0, sval1, sval2, area, area0, area1, area2, loop, loop0, loop1, loop2

INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: ncount
LOGICAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: mask
REAL   , ALLOCATABLE, DIMENSION(:)       :: lon,lat,dep,tiv
REAL   , ALLOCATABLE, DIMENSION(:,:,:,:) :: rval

! CALL SYSTEM('ls '//TRIM(FNAME_INFa)//' >'//TRIM(FNAME_LIST))
! WRITE(*,*) 'Lese ', FNAME_LIST
! OPEN(UNIT=1, FILE=FNAME_LIST,STATUS='OLD', ACTION='READ', IOSTAT=ioerror)
!
! 	DO
! 	READ(1,*,IOSTAT=ioerror) CDUMMY
! 	IF(ioerror/=0) EXIT
! 	END DO
!
! CLOSE(1)
! CALL SYSTEM('rm '//TRIM(FNAME_LIST))

WRITE(*,*) 'Lese '//TRIM(FNAME_INFa)
CALL check(nf90_open(TRIM(FNAME_INFa), NF90_NOWRITE, ncid(1)))

		CALL check(nf90_inq_dimid(ncid(1), 'time', dimid(1,4))); CALL check(nf90_inquire_dimension(ncid(1), dimid(1,4), len=NN(1,4)))
		    status=nf90_inq_dimid(ncid(1),  'lev', dimid(1,3)) ; IF(status/=NF90_NOERR) CALL check(nf90_inq_dimid(ncid(1), 'depth', dimid(1,3)))
		                                                         CALL check(nf90_inquire_dimension(ncid(1), dimid(1,3), len=NN(1,3)))
		CALL check(nf90_inq_dimid(ncid(1),  'lat', dimid(1,2))); CALL check(nf90_inquire_dimension(ncid(1), dimid(1,2), len=NN(1,2)))
		CALL check(nf90_inq_dimid(ncid(1),  'lon', dimid(1,1))); CALL check(nf90_inquire_dimension(ncid(1), dimid(1,1), len=NN(1,1)))

		NX=NN(1,1);NY=NN(1,2);ND=NN(1,3);NT=NN(1,4)

	ALLOCATE(lon(NX),lat(NY),dep(ND),tiv(NT),mask(NX,NY,ND,NT),rval(NX,NY,ND,NT))

		CALL check(nf90_inq_varid(ncid(1), 'time', varid(1,1))); CALL check(nf90_get_var(ncid(1), varid(1,1),  tiv, start=ON(1,4:4), count=NN(1,4:4)))
		    status=nf90_inq_varid(ncid(1),  'lev', varid(1,2)) ; IF(status/=NF90_NOERR) CALL check(nf90_inq_varid(ncid(1),'depth', varid(1,2)))
		                                                       ; CALL check(nf90_get_var(ncid(1), varid(1,2),  dep, start=ON(1,3:3), count=NN(1,3:3)))
		CALL check(nf90_inq_varid(ncid(1),  'lat', varid(1,3))); CALL check(nf90_get_var(ncid(1), varid(1,3),  lat, start=ON(1,2:2), count=NN(1,2:2)))
		CALL check(nf90_inq_varid(ncid(1),  'lon', varid(1,4))); CALL check(nf90_get_var(ncid(1), varid(1,4),  lon, start=ON(1,1:1), count=NN(1,1:1)))
		CALL check(nf90_inq_varid(ncid(1), 'rval', varid(1,5))); CALL check(nf90_get_var(ncid(1), varid(1,5), rval, start=ON(1,1:4), count=NN(1,1:4)))

WRITE(*,*) 'Schreibe '//TRIM(FNAME_OUT1)
CALL check(nf90_create(TRIM(FNAME_OUT1), NF90_CLOBBER, ncid(2)))

		NN(2,:)=NN(1,:)
		CALL check(nf90_def_dim(ncid(2), 'time',       0, dimid(2,4))); CALL check(nf90_def_var(ncid(2), 'time', NF90_FLOAT,  dimid(2,4:4), varid(2,1)))
		CALL check(nf90_def_dim(ncid(2),'depth', NN(2,3), dimid(2,3))); CALL check(nf90_def_var(ncid(2),'depth', NF90_FLOAT,  dimid(2,3:3), varid(2,2)))
		CALL check(nf90_def_dim(ncid(2),  'lat', NN(2,2), dimid(2,2))); CALL check(nf90_def_var(ncid(2),  'lat', NF90_FLOAT,  dimid(2,2:2), varid(2,3)))
		CALL check(nf90_def_dim(ncid(2),  'lon', NN(2,1), dimid(2,1))); CALL check(nf90_def_var(ncid(2),  'lon', NF90_FLOAT,  dimid(2,1:1), varid(2,4)))

		CALL check(nf90_def_var(ncid(2), 'rval', NF90_FLOAT,  dimid(2,1:4), varid(2,5)))

		CALL check(nf90_put_att(ncid(2), varid(2,1), 'units', TRIM(TIME_SECS)))
		CALL check(nf90_put_att(ncid(2), varid(2,2), 'units', 'm'))
		CALL check(nf90_put_att(ncid(2), varid(2,3), 'units', 'degrees_N'))
		CALL check(nf90_put_att(ncid(2), varid(2,4), 'units', 'degrees_E'))
		DO n=5,5
			status=nf90_get_att(ncid(1), varid(1,n), 'units', CDUMMY); IF(status==NF90_NOERR) CALL check(nf90_put_att(ncid(2), varid(2,n), 'units', TRIM(CDUMMY)))
			CALL check(nf90_put_att(ncid(2), varid(2,n), 'missing_value', novalue))
			CALL check(nf90_put_att(ncid(2), varid(2,n), '_FillValue'   , novalue))
		END DO

	CALL check(nf90_enddef(ncid(2)))

		CALL check(nf90_put_var(ncid(2), varid(2,1),  tiv, start=ON(2,4:4), count=NN(2,4:4)))
		CALL check(nf90_put_var(ncid(2), varid(2,2),  dep, start=ON(2,3:3), count=NN(2,3:3)))
		CALL check(nf90_put_var(ncid(2), varid(2,3),  lat, start=ON(2,2:2), count=NN(2,2:2)))
		CALL check(nf90_put_var(ncid(2), varid(2,4),  lon, start=ON(2,1:1), count=NN(2,1:1)))
		CALL check(nf90_put_var(ncid(2), varid(2,5), rval, start=ON(2,1:4), count=NN(2,1:4)))

CALL check(nf90_close(ncid(2)))
CALL check(nf90_close(ncid(1)))

CONTAINS
SUBROUTINE kalman(OPT_INT,SILENT,THREADS,LOCAL,NX,NY,ND,NT,NE,TPOS,novalue,eoff,obsf,oerf,recf,errf,rpcv)

IMPLICIT NONE

LOGICAL, intent(in)    :: OPT_INT               !USING AN OPTIMAL INTERPOLATION APPROACHE (OBSERVATION ERROR IS FORCED TO APPROX. ZERO)
LOGICAL, intent(in)    :: SILENT                !.FALSE. => CREATE LOGFILE
CHARACTER(len=800)     :: WORKLOGF='kalman_sub' !NAME OF LOGFILE

INTEGER         , PARAMETER :: HALO=1    !HALO OF LOCALISATIION (MINIMUM=1)
DOUBLE PRECISION, PARAMETER :: EPS0=1D-8 !TRESHOLD FOR PC VARIANCE IN INVERSION

INTEGER                             , intent(in)    :: THREADS     !NUMBER OF THREADS FOR OPEN-MP. ONLY USED IN LOCALISATION (LO>0)
INTEGER, DIMENSION(ND,3)            , intent(in)    :: LOCAL       !LOCALISE 3D (NX;NY;NT) OVER DIM-INDEX (LOCAL=0 => global)
INTEGER                             , intent(in)    :: NX,NY,ND,NT !DIMENSIONS 4D
INTEGER                             , intent(in)    :: NE          !NUMBER OF EOFS (NE)
INTEGER                             , intent(in)    :: TPOS        !POSITION INDEX OF RECONSTRUCTION ON T-DIMENSION
REAL                                , intent(in)    :: novalue     !NOVALUE IN EOFF,OBSF,OERF,RECF,ERRF
REAL, DIMENSION(NX,NY,ND,NT,NE)     , intent(inout) :: eoff        !EOFS
REAL, DIMENSION(NX,NY,ND,NT)        , intent(inout) :: obsf        !OBSERVATIONS
REAL, DIMENSION(NX,NY,ND,NT)        , intent(inout) :: oerf        !OBSERVATION ERRORS (STDV)
REAL, DIMENSION(NX,NY,ND)           , intent(inout) :: recf        !RECONSTRUCTION
REAL, DIMENSION(NX,NY,ND)           , intent(inout) :: errf        !ERROR OF RECONSTRUCTION (% OF STDV IN EOFS)
REAL, DIMENSION(NX,NY,NE), OPTIONAL , intent(inout) :: rpcv        !GIVES THE EIGENVALUES USED FOR RECONSTRUCTION

LOGICAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: xmask,omask !INTERNAL MASK FOR STATE,OBSERVATIONS
LOGICAL, ALLOCATABLE, DIMENSION(:,:,:)   :: done        !INTERNAL MASK FOR RECONSTRUCTIONS

DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: im0      !INVERSION MATRIX
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: work,lam !INVERSION AUXILIARY VECTOR

INTEGER, ALLOCATABLE, DIMENSION(:,:,:)   :: mpoo                         !TASK SPECIFIC POSITION OF OBSERVATIONS
INTEGER, ALLOCATABLE, DIMENSION(:,:)     :: mpov                         !TASK SPECIFIC POSITION OF RECONSTRUCTIONS
REAL   , ALLOCATABLE, DIMENSION(:)       :: obsv,oerv                    !TASK SPECIFIC VALUE OF OBSERVATIONS, OBSERVATION ERRORS
REAL   , ALLOCATABLE, DIMENSION(:,:)     :: eofv,opcv,epcv,evm,pvm,A0,At !TASK SPECIFIC EIGENVECTOR, EIGENVALUE, COVARIANCE

INTEGER (KIND=8)                  :: NSTA,NOBS,NM,MOBS,DOBS !INTERNAL VECTOR DIMENSIONS: STATE, OBSERVATIONS, TASKS, MAXIMUM TASK SPECIFIC OBSERVATIONS, TASK SPECIFIC OBSERVATIONS
INTEGER (KIND=8), DIMENSION(ND,3) :: ILOCAL                 !INTERNAL LOCALISATION VECTOR
INTEGER (KIND=8)                  :: WDI(ND,3)              !LOCALISATION WINDOW DIMENSIONS

INTEGER (KIND=8) :: n,n1,n2,m,m1,m2      !AUXILIARY
INTEGER :: ALOB,x,y,z,d,t,x1,y1,z1,d1,t1 !AUXILIARY
REAL    :: c                             !AUXILIARY
CHARACTER (len=800) :: LDUMMY            !AUXILIARY
CHARACTER (len=800) :: WORKPATH          !AUXILIARY

IF(.NOT.SILENT) THEN;
	CALL SYSTEM('pwd > '//TRIM(WORKLOGF))
	OPEN(UNIT=100, FILE=TRIM(WORKLOGF),STATUS='OLD', ACTION='READ', IOSTAT=ioerror)
		READ(100,'(A800)') WORKPATH
	CLOSE(100)
	CALL SYSTEM('rm '//TRIM(WORKLOGF))
	LDUMMY=TRIM(WORKLOGF)
	IF(WORKPATH(LEN_TRIM(WORKPATH):LEN_TRIM(WORKPATH))=='/') LDUMMY=(TRIM(WORKPATH)//TRIM(WORKLOGF))
	IF(WORKPATH(LEN_TRIM(WORKPATH):LEN_TRIM(WORKPATH))/='/') LDUMMY=(TRIM(WORKPATH)//'/'//TRIM(WORKLOGF))
	WORKLOGF=TRIM(LDUMMY)
	OPEN(UNIT=100,FILE=TRIM(WORKLOGF),STATUS='UNKNOWN',ACTION='WRITE'); CLOSE(100)
END IF

ALOB=0
ALLOCATE(xmask(1:NX,1:NY,1:ND,1:NT),omask(1:NX,1:NY,1:ND,1:NT),done(1:NX,1:NY,1:ND),STAT=ALOB)
IF(ALOB/=0) THEN
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'ALLOCATION ERROR';CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'NX:  ',NX;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'NY:  ',NY;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'ND:  ',ND;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'NT:  ',NT;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	STOP
END IF

	xmask(1:NX,1:NY,1:ND,1:NT)=.TRUE.
	DO n=1,NE
	WHERE(eoff(1:NX,1:NY,1:ND,1:NT,n)==novalue) xmask(1:NX,1:NY,1:ND,1:NT)=.FALSE.
	END DO

	omask(1:NX,1:NY,1:ND,1:NT)=.TRUE.
	WHERE(obsf(1:NX,1:NY,1:ND,1:NT)  ==novalue) omask(1:NX,1:NY,1:ND,1:NT)=.FALSE.

	done(1:NX,1:NY,1:ND)=.TRUE.
	DO t=1,NT
	WHERE(xmask(1:NX,1:NY,1:ND,t)) done(1:NX,1:NY,1:ND)=.FALSE.
	END DO

	NSTA=COUNT(xmask(1:NX,1:NY,1:ND,1:NT))
	NOBS=COUNT(omask(1:NX,1:NY,1:ND,1:NT))

recf(1:NX,1:NY,1:ND)=novalue
errf(1:NX,1:NY,1:ND)=novalue
IF(PRESENT(rpcv)) rpcv(1:NX,1:NY,1:NE)=novalue

	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) MAX(1,MIN(NT,TPOS)),'NSTA:',NSTA;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) MAX(1,MIN(NT,TPOS)),'NOBS:',NOBS;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; CALL LWRITE(TRIM(WORKLOGF),'',0); END IF
IF(MAXVAL(ILOCAL)<0 .OR. NE<=0 .OR. NSTA<=0 .OR. NOBS<=0) THEN
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'ILOCAL:',MAXVAL(ILOCAL);CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'NE:   ',NE;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'NSTA: ',NSTA;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'NOBS: ',NOBS;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'CANCELED';CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
ELSE

		ILOCAL=LOCAL
		WHERE(ILOCAL(:,1)<=0) ILOCAL(:,1)=10000*NX
		WHERE(ILOCAL(:,2)<=0) ILOCAL(:,2)=10000*NY
		WHERE(ILOCAL(:,3)<=0) ILOCAL(:,3)=10000*NT

		WDI=MAX(HALO+1,2)*ILOCAL
		t=MAX(1,MIN(NT,TPOS))
		NM=0
		MOBS=0

CALL OMP_SET_NUM_THREADS(THREADS)
!$OMP PARALLEL PRIVATE(x,y,x1,y1,z1,t1,DOBS)
!$OMP DO
		DO x=1,NX
		DO y=1,NY
		IF(.NOT. done(x,y,1)) THEN

			DOBS=0
			DO z1=1,ND
			DO x1=MAX(1,x-WDI(z1,1)),MIN(NX,x+WDI(z1,1))
			DO y1=MAX(1,y-WDI(z1,2)),MIN(NY,y+WDI(z1,2))
			DO t1=MAX(1,t-WDI(z1,3)),MIN(NT,t+WDI(z1,3))
			IF(omask(x1,y1,z1,t1)) DOBS=DOBS+1
			END DO
			END DO
			END DO
			END DO

		IF(DOBS==0) THEN
			done(x,y,1:ND)=.TRUE.
		ELSE
!$OMP CRITICAL
			NM=NM+1
			MOBS=MAX(MOBS,DOBS)
IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) MAX(1,MIN(NT,TPOS)),'LOCS:',NM;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),1); END IF
!$OMP END CRITICAL
		END IF
		END IF
		END DO
		END DO
!$OMP END DO
!$OMP END PARALLEL
IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) MAX(1,MIN(NT,TPOS)),'MOBS:',MOBS;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
IF(.NOT.SILENT) THEN; CALL LWRITE(TRIM(WORKLOGF),'',0); END IF

	ALLOCATE(mpov(1:NM,1:3),mpoo(1:NM,1:MOBS,1:4),STAT=ALOB)
	IF(ALOB/=0) THEN
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'ALLOCATION ERROR';CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'LOCS:',NM;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'MOBS:',MOBS;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		STOP
	END IF

		m=0

CALL OMP_SET_NUM_THREADS(THREADS)
!$OMP PARALLEL PRIVATE(x,y,x1,y1,z1,t1,m1,DOBS)
!$OMP DO
		DO x=1,NX
		DO y=1,NY
		IF(.NOT. done(x,y,1)) THEN
!$OMP CRITICAL
			m =m+1
			m1=m
IF(.NOT.SILENT) THEN; WRITE(LDUMMY,'(I13,1X,"Get OBS_VEC:",1X,I8,1X,I8)') MAX(1,MIN(NT,TPOS)),m1,NM;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),1); END IF
!$OMP END CRITICAL
			DOBS=0
			DO z1=1,ND
			DO x1=MAX(1,x-WDI(z1,1)),MIN(NX,x+WDI(z1,1))
			DO y1=MAX(1,y-WDI(z1,2)),MIN(NY,y+WDI(z1,2))
			DO t1=MAX(1,t-WDI(z1,3)),MIN(NT,t+WDI(z1,3))
			IF(omask(x1,y1,z1,t1)) THEN
				DOBS=DOBS+1
				mpoo(m1,DOBS,1)=x1
				mpoo(m1,DOBS,2)=y1
				mpoo(m1,DOBS,3)=z1
				mpoo(m1,DOBS,4)=t1
			END IF
			END DO
			END DO
			END DO
			END DO

			mpov(m1,1)=x
			mpov(m1,2)=y
			mpov(m1,3)=DOBS

		END IF
		END DO
		END DO
!$OMP END DO
!$OMP END PARALLEL
IF(.NOT.SILENT) THEN; CALL LWRITE(TRIM(WORKLOGF),'',0); END IF

	IF(NE<=MOBS) THEN
		ALLOCATE(obsv(MOBS),oerv(MOBS),eofv(1:NE,1:1),opcv(1:NE,1:1),epcv(1:NE,1:1),&
		&A0(MOBS,NE),At(NE,MOBS),evm(NE,NE),pvm(NE,NE),&
		&im0(NE,NE),work(3*NE-1),lam(NE),STAT=ALOB)
	ELSE
		ALLOCATE(obsv(MOBS),oerv(MOBS),eofv(1:NE,1:1),opcv(1:NE,1:1),epcv(1:NE,1:1),&
		&A0(MOBS,NE),At(NE,MOBS),evm(NE,NE),pvm(NE,NE),&
		&im0(MOBS,MOBS),work(3*MOBS-1),lam(MOBS),STAT=ALOB)
	END IF
	IF(ALOB/=0) THEN
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'ALLOCATION ERROR';CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'MOBS:',MOBS;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'NE:  ',NE;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		STOP
	END IF

		m1=0

CALL OMP_SET_NUM_THREADS(THREADS)
!$OMP PARALLEL PRIVATE(m,DOBS,ALOB,n,n1,x,y,z,d,x1,y1,z1,d1,t1,c,obsv,oerv,A0,At,evm,pvm,eofv,opcv,epcv,im0,work,lam)
!$OMP DO
		DO m=1,NM
!$OMP CRITICAL
		m1=m1+1
IF(.NOT.SILENT) THEN; WRITE(LDUMMY,'(I13,1X,"Calculating:",1X,I8,1X,I8)') MAX(1,MIN(NT,TPOS)),m1,NM;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),1); END IF
!$OMP END CRITICAL

		x=mpov(m,1)
		y=mpov(m,2)
		DOBS=mpov(m,3)

			n=0
			DO n1=1,DOBS;x1=mpoo(m,n1,1);y1=mpoo(m,n1,2);z1=mpoo(m,n1,3);t1=mpoo(m,n1,4)
			IF(omask(x1,y1,z1,t1)) THEN
				n=n+1

				c=0.
				c=c-2.*ABS(REAL(x1)-REAL(x))**2/MAX(1.,REAL(ILOCAL(z1,1)))**2
				c=c-2.*ABS(REAL(y1)-REAL(y))**2/MAX(1.,REAL(ILOCAL(z1,2)))**2
				c=c-2.*ABS(REAL(t1)-REAL(t))**2/MAX(1.,REAL(ILOCAL(z1,3)))**2
				c=1./(EXP(c))

				obsv(n)=obsf(x1,y1,z1,t1)
				A0(n,1:NE)=eoff(x1,y1,z1,t1,1:NE)

			IF(OPT_INT) THEN
				oerv(n)=1e-5+(c-1.)*(SUM(eoff(x1,y1,z1,t1,1:NE))/REAL(NE))**2
			ELSE
				oerv(n)=c*oerf(x1,y1,z1,t1)**2
			END IF
			END IF
			END DO

			WHERE(oerv<0. .OR. 1e5<oerv) oerv=1e5

	IF(NE<=MOBS) THEN
			FORALL(n=1:DOBS) At(1:NE,n)=A0(n,1:NE)*1.0/SQRT(oerv(n))
			im0(1:NE,1:NE)=MATMUL(At(1:NE,1:DOBS),TRANSPOSE(At(1:NE,1:DOBS)))
			FORALL(n=1:NE) im0(n,n)=im0(n,n)+1.

			CALL DSYEV('V','U',NE,im0(1:NE,1:NE),NE,lam(1:NE),work(1:3*NE-1),3*NE-1,ALOB)

!$OMP CRITICAL
	IF(ALOB/=0) THEN
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) "THERE IS A PROBLEM WITH THE SVD NEEDED FOR MATRIX INVERSION!";CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'NE:  ',NE;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'M/X/Y',m,x,y;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		STOP
	END IF
!$OMP END CRITICAL
			WHERE(lam(1:NE)<=EPS0) lam(1:NE)=0.0
			WHERE(lam(1:NE)> EPS0) lam(1:NE)=1.0/SQRT(lam(1:NE))

			FORALL(n=1:NE) im0(1:NE,n)=im0(1:NE,n)*lam(n)

			im0(1:NE,1:NE)=MATMUL(im0(1:NE,1:NE),TRANSPOSE(im0(1:NE,1:NE)))

			At(1:NE,1:DOBS)=MATMUL(im0(1:NE,1:NE),TRANSPOSE(A0(1:DOBS,1:NE)))
			FORALL(n=1:DOBS) At(1:NE,n)=At(1:NE,n)*1.0/oerv(n)
	ELSE
			im0(1:DOBS,1:DOBS)=MATMUL(A0(1:DOBS,1:NE),TRANSPOSE(A0(1:DOBS,1:NE)))
			FORALL(n=1:DOBS) im0(n,n)=im0(n,n)+oerv(n)

			CALL DSYEV('V','U',DOBS,im0(1:DOBS,1:DOBS),DOBS,lam(1:DOBS),work(1:3*DOBS-1),3*DOBS-1,ALOB)

!$OMP CRITICAL
	IF(ALOB/=0) THEN
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) "THERE IS A PROBLEM WITH THE SVD NEEDED FOR MATRIX INVERSION!";CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'DOBS:',DOBS;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'M/X/Y',m,x,y;CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		STOP
	END IF
!$OMP END CRITICAL
			WHERE(lam(1:DOBS)<=EPS0) lam(1:DOBS)=0.0
			WHERE(lam(1:DOBS)> EPS0) lam(1:DOBS)=1.0/SQRT(lam(1:DOBS))

			FORALL(n=1:DOBS) im0(1:DOBS,n)=im0(1:DOBS,n)*lam(n)

			im0(1:DOBS,1:DOBS)=MATMUL(im0(1:DOBS,1:DOBS),TRANSPOSE(im0(1:DOBS,1:DOBS)))

			At(1:NE,1:DOBS)=MATMUL(TRANSPOSE(A0(1:DOBS,1:NE)),im0(1:DOBS,1:DOBS))
	END IF

			pvm(1:NE,1:NE)=MATMUL(At(1:NE,1:DOBS),A0(1:DOBS,1:NE))
			evm=0.;FORALL(n=1:NE) evm(n,n)=1.
			evm(1:NE,1:NE)=evm(1:NE,1:NE)-pvm(1:NE,1:NE)

			FORALL(n=1:DOBS) A0(n,1:NE)=At(1:NE,n)*oerv(n)

			pvm(1:NE,1:NE)=MATMUL(At(1:NE,1:DOBS),A0(1:DOBS,1:NE))
			evm(1:NE,1:NE)=MATMUL(evm(1:NE,1:NE),TRANSPOSE(evm(1:NE,1:NE)))
			evm(1:NE,1:NE)=evm(1:NE,1:NE)+pvm(1:NE,1:NE)

			opcv(1:NE,1:1)=MATMUL(At(1:NE,1:DOBS),SPREAD(obsv(1:DOBS),2,1))

			IF(PRESENT(rpcv)) rpcv(x,y,1:NE)=opcv(1:NE,1)

		DO z1=1,ND
		IF(.NOT. done(x,y,z1)) THEN

			eofv(1:NE,1)  =eoff(x,y,z1,t,1:NE)
			epcv(1:NE,1:1)=MATMUL(evm(1:NE,1:NE),eofv(1:NE,1:1))


! 	recf(x,y,z1)=SUM(eofv(1:1,1)*opcv(1:1,1))
! 	errf(x,y,z1)=100.*(1.-(SQRT(SUM(eofv(1:1,1)**2))-SQRT(SUM(eofv(1:1,1)*epcv(1:1,1))))/SQRT(SUM(eofv(1:1,1)**2)))
	recf(x,y,z1)=SUM(eofv(1:NE,1)*opcv(1:NE,1))
	errf(x,y,z1)=100.*(1.-(SQRT(SUM(eofv(1:NE,1)**2))-SQRT(SUM(eofv(1:NE,1)*epcv(1:NE,1))))/SQRT(SUM(eofv(1:NE,1)**2)))
	done(x,y,z1)=.TRUE.
		END IF
		END DO
		END DO
!$OMP END DO
!$OMP BARRIER
!$OMP END PARALLEL

	DEALLOCATE(mpov,mpoo,obsv,oerv,eofv,opcv,epcv,A0,At,evm,pvm,im0,work,lam,STAT=ALOB)
	IF(ALOB/=0) THEN
		IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'DEALLOCATION ERROR';CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
		STOP
	END IF

END IF

FORALL(x=1:NX,y=1:NY,z=1:ND,COUNT(xmask(x,y,z,:))/=0 .AND. (recf(x,y,z)==novalue .OR. errf(x,y,z)==novalue))
	recf(x,y,z)=0.
	errf(x,y,z)=100.
END FORALL

DEALLOCATE(xmask,omask,done,STAT=ALOB)
IF(ALOB/=0) THEN
	IF(.NOT.SILENT) THEN; WRITE(LDUMMY,*) 'DEALLOCATION ERROR';CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0); END IF
	STOP
END IF

RETURN

END SUBROUTINE kalman
SUBROUTINE eofanalysis(THREADS,n,m,XMAT,YMAT,EMAT,MEAN,PMAT)

IMPLICIT NONE

DOUBLE PRECISION, PARAMETER :: EPS0=10D-10

INTEGER                                 , intent(in)    :: THREADS
INTEGER                                 , intent(in)    :: m,n  !DIMENSIONS NSPACE,NTIME
REAL            , DIMENSION(n,m)        , intent(inout) :: XMAT !EIGENVECTOR [NEOF,NSPACE]         !!!NORMALIZED!!! FIELD[NSPACE,NTIME]=MEAN+TRANSPOSE(XMAT)*DIAG(SQRT(EMAT))*YMAT
REAL            , DIMENSION(n,n)        , intent(inout) :: YMAT !PRINCIPAL COMPONENTS [NEOF,NTIME] !!!NORMALIZED!!! FIELD[NSPACE,NTIME]=MEAN+TRANSPOSE(XMAT)*DIAG(SQRT(EMAT))*YMAT
DOUBLE PRECISION, DIMENSION(n)          , intent(inout) :: EMAT !EIGENVALUE [NEOF]
DOUBLE PRECISION, DIMENSION(m)          , intent(inout) :: MEAN !MEAN [NSPACE]
DOUBLE PRECISION, DIMENSION(n), OPTIONAL, intent(inout) :: PMAT !EXPLAINED VARIANCE [NEOF]

REAL            , DIMENSION(m,n)   :: TMAT
DOUBLE PRECISION, DIMENSION(m)     :: mvec
DOUBLE PRECISION, DIMENSION(n)     :: nvec
DOUBLE PRECISION, DIMENSION(n,n)   :: L
DOUBLE PRECISION, DIMENSION(3*n-1) :: WORK

INTEGER :: i,j,k,ALOB
DOUBLE PRECISION :: sum0

CALL omp_set_num_threads(THREADS)

!$omp parallel private(i) shared(XMAT,MEAN)
!$omp do
DO i=1,m
	MEAN(i)   = SUM(XMAT(:,i))/REAL(n)
	XMAT(:,i) = XMAT(:,i)-MEAN(i)
END DO
!$omp end do
!$omp end parallel

TMAT = TRANSPOSE(XMAT)
XMAT = XMAT/SQRT(REAL(n)) ! normalise

!$omp parallel private(j) shared(L,XMAT)
!$omp do
DO i=1,CEILING(REAL(n)/2.0)
	DO j=i,n
		L(i,j) = SUM(DBLE(XMAT(i,:))*DBLE(XMAT(j,:)))
		L(j,i) = L(i,j)
	END DO
	DO j=n-i+1,n
		L(n-i+1,j) = SUM(DBLE(XMAT(n-i+1,:))*DBLE(XMAT(j,:)))
		L(j,n-i+1) = L(n-i+1,j)
	END DO
END DO
!$omp end do
!$omp end parallel

CALL DSYEV('V','U',n,L,n,EMAT,WORK,3*n-1,ALOB)
IF (ALOB/=0) THEN
	WRITE(*,*) "Problem with eigenvector computation !!"
	STOP
ENDIF

DO i=1,n
	nvec(i) = SQRT(1D0/(EMAT(i)+EPS0))
END DO

!$omp parallel private(i) shared(L,XMAT)
!$omp do
DO i=1,m
	XMAT(:,i) = MATMUL(TRANSPOSE(L),DBLE(XMAT(:,i)))
END DO
!$omp end do
!$omp end parallel

DO i=1,n
	XMAT(i,:) = DBLE(XMAT(i,:))*nvec(i) ! Divide each row (column of D)
END DO

! reverse EOF order (dominant first)
DO i=1,FLOOR(REAL(n)/2.0)
	mvec = XMAT(n-i+1,:)
	XMAT(n-i+1,:) = XMAT(i,:)
	XMAT(i,:) = mvec

	sum0 = EMAT(n-i+1)
	EMAT(n-i+1) = EMAT(i)
	EMAT(i) = sum0
END DO

YMAT=MATMUL(XMAT,TMAT)
DO i=1,n
! 	XMAT(i,:)=XMAT(i,:)*   SQRT(MAX(EMAT(i),EPS0))
	YMAT(i,:)=YMAT(i,:)*1./SQRT(MAX(EMAT(i),EPS0))
END DO
IF(PRESENT(PMAT)) THEN
	sum0=SUM(EMAT)
	PMAT=EMAT/DBLE(sum0)
	DO i=n,1,-1
		PMAT(i)=SUM(PMAT(1:i))*100.
	END DO
END IF

RETURN

END SUBROUTINE eofanalysis
SUBROUTINE bilin5(SILENT,THREADS,tmp_nov,INVI,ixvv,iyvv,idat,ONXI,ONYI,oxvv,oyvv,mask,odat)

CHARACTER(len=*), PARAMETER :: WORKPATH='/data/grayek/WORK'
CHARACTER(len=*), PARAMETER :: WORKLOGF=(TRIM(WORKPATH)//'/'//'bilin_sub')
CHARACTER (len=800) :: LDUMMY

INTEGER, PARAMETER :: EXTPRANGE=1,SMTHRANGE=20
INTEGER            :: INTPRANGE=2

LOGICAL              , INTENT(IN)    :: SILENT
REAL                 , INTENT(in)    :: tmp_nov
INTEGER              , INTENT(in)    :: THREADS,INVI,ONXI,ONYI
REAL, DIMENSION(INVI), INTENT(inout) :: ixvv,iyvv,idat
REAL, DIMENSION(ONXI), INTENT(inout) :: oxvv
REAL, DIMENSION(ONYI), INTENT(inout) :: oyvv

LOGICAL, DIMENSION(ONXI,ONYI), INTENT(inout) :: mask
REAL   , DIMENSION(ONXI,ONYI), INTENT(inout) :: odat

INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: imap
INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: info
LOGICAL, ALLOCATABLE, DIMENSION(:,:)   :: snap
LOGICAL, ALLOCATABLE, DIMENSION(:)     :: tmap
REAL   , ALLOCATABLE, DIMENSION(:,:,:) :: demp
REAL   , ALLOCATABLE, DIMENSION(:)     :: temp

REAL    :: ixspc=0.,iyspc=0.,oxspc=0.,oyspc=0.

INTEGER :: n0,n1,x0,x1,y0,y1,try
REAL    :: vv(4,2),xv(4,2),yv(4,2)

ALLOCATE(imap(ONXI,ONYI,4),demp(ONXI,ONYI,4),info(ONXI,ONYI),snap(ONXI,ONYI),tmap(INVI),temp(INVI)); imap=0; demp=tmp_nov; info=0; snap=.FALSE.; tmap=.FALSE.; temp=0.
CALL omp_set_num_threads(THREADS)

oxspc=ABS(oxvv(ONXI)-oxvv(1))/REAL(ONXI-1)
oyspc=ABS(oyvv(ONYI)-oyvv(1))/REAL(ONYI-1)
DO n0=1,INVI
temp=ABS(ixvv-ixvv(n0));ixspc=(ixspc*REAL(n0-1)+MINVAL(temp,temp>1e-5))/REAL(n0)
temp=ABS(iyvv-iyvv(n0));iyspc=(iyspc*REAL(n0-1)+MINVAL(temp,temp>1e-5))/REAL(n0)
END DO
INTPRANGE=MAX(INTPRANGE,1+FLOOR(0.5+MAX(ixspc/oxspc,iyspc/oyspc)))

	DO x0=1,ONXI; DO y0=1,ONYI
	IF(mask(x0,y0)) THEN

		tmap=.FALSE.; temp=0.
!$OMP PARALLEL PRIVATE(n0)
!$OMP DO
		DO n0=1,INVI
		IF(ixvv(n0)-oxvv(x0)<=0. .AND. iyvv(n0)-oyvv(y0)<=0. .AND. SQRT((ixvv(n0)-oxvv(x0))**2+(iyvv(n0)-oyvv(y0))**2)<=SQRT((REAL(INTPRANGE)*oxspc)**2+(REAL(INTPRANGE)*oyspc)**2)) THEN
		tmap(n0)=.TRUE.; temp(n0)=SQRT((ixvv(n0)-oxvv(x0))**2+(iyvv(n0)-oyvv(y0))**2)
		END IF
		END DO
!$OMP END DO
!$OMP BARRIER
!$OMP END PARALLEL
		FORALL(n0=1:4,imap(x0,y0,n0)/=0) tmap(imap(x0,y0,n0))=.FALSE.
		IF(COUNT(tmap)/=0) imap(x0,y0,1:1)=MINLOC(temp,tmap)

		tmap=.FALSE.; temp=0.
!$OMP PARALLEL PRIVATE(n0)
!$OMP DO
		DO n0=1,INVI
		IF(ixvv(n0)-oxvv(x0)<=0. .AND. iyvv(n0)-oyvv(y0)>=0. .AND. SQRT((ixvv(n0)-oxvv(x0))**2+(iyvv(n0)-oyvv(y0))**2)<=SQRT((REAL(INTPRANGE)*oxspc)**2+(REAL(INTPRANGE)*oyspc)**2)) THEN
		tmap(n0)=.TRUE.; temp(n0)=SQRT((ixvv(n0)-oxvv(x0))**2+(iyvv(n0)-oyvv(y0))**2)
		END IF
		END DO
!$OMP END DO
!$OMP BARRIER
!$OMP END PARALLEL
		FORALL(n0=1:4,imap(x0,y0,n0)/=0) tmap(imap(x0,y0,n0))=.FALSE.
		IF(COUNT(tmap)/=0) imap(x0,y0,2:2)=MINLOC(temp,tmap)

		tmap=.FALSE.; temp=0.
!$OMP PARALLEL PRIVATE(n0)
!$OMP DO
		DO n0=1,INVI
		IF(ixvv(n0)-oxvv(x0)>=0. .AND. iyvv(n0)-oyvv(y0)>=0. .AND. SQRT((ixvv(n0)-oxvv(x0))**2+(iyvv(n0)-oyvv(y0))**2)<=SQRT((REAL(INTPRANGE)*oxspc)**2+(REAL(INTPRANGE)*oyspc)**2)) THEN
		tmap(n0)=.TRUE.; temp(n0)=SQRT((ixvv(n0)-oxvv(x0))**2+(iyvv(n0)-oyvv(y0))**2)
		END IF
		END DO
!$OMP END DO
!$OMP BARRIER
!$OMP END PARALLEL
		FORALL(n0=1:4,imap(x0,y0,n0)/=0) tmap(imap(x0,y0,n0))=.FALSE.
		IF(COUNT(tmap)/=0) imap(x0,y0,3:3)=MINLOC(temp,tmap)

		tmap=.FALSE.; temp=0.
!$OMP PARALLEL PRIVATE(n0)
!$OMP DO
		DO n0=1,INVI
		IF(ixvv(n0)-oxvv(x0)>=0. .AND. iyvv(n0)-oyvv(y0)<=0. .AND. SQRT((ixvv(n0)-oxvv(x0))**2+(iyvv(n0)-oyvv(y0))**2)<=SQRT((REAL(INTPRANGE)*oxspc)**2+(REAL(INTPRANGE)*oyspc)**2)) THEN
		tmap(n0)=.TRUE.; temp(n0)=SQRT((ixvv(n0)-oxvv(x0))**2+(iyvv(n0)-oyvv(y0))**2)
		END IF
		END DO
!$OMP END DO
!$OMP BARRIER
!$OMP END PARALLEL
		FORALL(n0=1:4,imap(x0,y0,n0)/=0) tmap(imap(x0,y0,n0))=.FALSE.
		IF(COUNT(tmap)/=0) imap(x0,y0,4:4)=MINLOC(temp,tmap)

	END IF
	END DO; END DO

DEALLOCATE(tmap,temp)

DO try=1,MAX(1,EXTPRANGE+1)
IF(COUNT(mask)==COUNT(snap)) EXIT

!$OMP PARALLEL PRIVATE(x0,y0,xv,yv,vv)
!$OMP DO
	DO x0=1,ONXI; DO y0=1,ONYI
	IF(mask(x0,y0) .AND. .NOT.snap(x0,y0) .AND. COUNT(imap(x0,y0,1:4)/=0 .OR. demp(x0,y0,1:4)/=tmp_nov)==4) THEN

		xv=0.;yv=0.;vv=0.
		IF(demp(x0,y0,1)/=tmp_nov) THEN; xv(1,1)=oxvv(x0)-oxspc/2.; yv(1,1)=oyvv(y0)-oyspc/2.; vv(1,1)=demp(x0,y0,1); END IF
		IF(demp(x0,y0,2)/=tmp_nov) THEN; xv(2,1)=oxvv(x0)-oxspc/2.; yv(2,1)=oyvv(y0)+oyspc/2.; vv(2,1)=demp(x0,y0,2); END IF
		IF(demp(x0,y0,3)/=tmp_nov) THEN; xv(3,1)=oxvv(x0)+oxspc/2.; yv(3,1)=oyvv(y0)+oyspc/2.; vv(3,1)=demp(x0,y0,3); END IF
		IF(demp(x0,y0,4)/=tmp_nov) THEN; xv(4,1)=oxvv(x0)+oxspc/2.; yv(4,1)=oyvv(y0)-oyspc/2.; vv(4,1)=demp(x0,y0,4); END IF
		DO n0=1,4
		IF(imap(x0,y0,n0)/=0)      THEN; xv(n0,1)=ixvv(imap(x0,y0,n0)); yv(n0,1)=iyvv(imap(x0,y0,n0)); vv(n0,1)=idat(imap(x0,y0,n0)); END IF
		END DO

		xv(1,2)=oxvv(x0);yv(1,2)=(yv(1,1)+yv(4,1))/2.
		yv(2,2)=oyvv(y0);xv(2,2)=(xv(1,1)+xv(2,1))/2.
		xv(3,2)=oxvv(x0);yv(3,2)=(yv(2,1)+yv(3,1))/2.
		yv(4,2)=oyvv(y0);xv(4,2)=(xv(4,1)+xv(3,1))/2.

		vv(1,2)=vv(1,1)+(vv(4,1)-vv(1,1))*SQRT((xv(1,2)-xv(1,1))**2+(yv(1,2)-yv(1,1))**2)/SQRT((xv(4,1)-xv(1,1))**2+(yv(4,1)-yv(1,1))**2)
		vv(2,2)=vv(1,1)+(vv(2,1)-vv(1,1))*SQRT((xv(2,2)-xv(1,1))**2+(yv(2,2)-yv(1,1))**2)/SQRT((xv(2,1)-xv(1,1))**2+(yv(2,1)-yv(1,1))**2)
		vv(3,2)=vv(2,1)+(vv(3,1)-vv(2,1))*SQRT((xv(3,2)-xv(2,1))**2+(yv(3,2)-yv(2,1))**2)/SQRT((xv(3,1)-xv(2,1))**2+(yv(3,1)-yv(2,1))**2)
		vv(4,2)=vv(4,1)+(vv(3,1)-vv(4,1))*SQRT((xv(4,2)-xv(4,1))**2+(yv(4,2)-yv(4,1))**2)/SQRT((xv(3,1)-xv(4,1))**2+(yv(3,1)-yv(4,1))**2)

		odat(x0,y0)=(vv(1,2)+(vv(3,2)-vv(1,2))*(oyvv(y0)-yv(1,2))/(yv(3,2)-yv(1,2)) +&
                           & vv(2,2)+(vv(4,2)-vv(2,2))*(oxvv(x0)-xv(2,2))/(xv(4,2)-xv(2,2)))/2.

		snap(x0,y0)=.TRUE.

		demp(x0,y0,1)=(vv(1,2)+(vv(3,2)-vv(1,2))*(oyvv(y0)-oyspc/2.-yv(1,2))/(yv(3,2)-yv(1,2)) +&
                             & vv(2,2)+(vv(4,2)-vv(2,2))*(oxvv(x0)-oxspc/2.-xv(2,2))/(xv(4,2)-xv(2,2)))/2.
		demp(x0,y0,2)=(vv(1,2)+(vv(3,2)-vv(1,2))*(oyvv(y0)+oyspc/2.-yv(1,2))/(yv(3,2)-yv(1,2)) +&
                             & vv(2,2)+(vv(4,2)-vv(2,2))*(oxvv(x0)-oxspc/2.-xv(2,2))/(xv(4,2)-xv(2,2)))/2.
		demp(x0,y0,3)=(vv(1,2)+(vv(3,2)-vv(1,2))*(oyvv(y0)+oyspc/2.-yv(1,2))/(yv(3,2)-yv(1,2)) +&
                             & vv(2,2)+(vv(4,2)-vv(2,2))*(oxvv(x0)+oxspc/2.-xv(2,2))/(xv(4,2)-xv(2,2)))/2.
		demp(x0,y0,4)=(vv(1,2)+(vv(3,2)-vv(1,2))*(oyvv(y0)-oyspc/2.-yv(1,2))/(yv(3,2)-yv(1,2)) +&
                             & vv(2,2)+(vv(4,2)-vv(2,2))*(oxvv(x0)+oxspc/2.-xv(2,2))/(xv(4,2)-xv(2,2)))/2.

	END IF
	END DO; END DO
!$OMP END DO
!$OMP BARRIER
!$OMP END PARALLEL
!$OMP PARALLEL PRIVATE(x0,y0,x1,y1)
!$OMP DO
	DO x0=1,ONXI; DO y0=1,ONYI
	IF(mask(x0,y0) .AND. .NOT.snap(x0,y0)) THEN

		x1=MAX(1,x0-1);y1=MAX(1,y0-1)
		IF(COUNT(snap(MIN(x0,x1):MAX(x0,x1),MIN(y0,y1):MAX(y0,y1)))/=0)&
              & demp(x0,y0,1)=SUM((/demp(x0,y0,1),demp(x0,y1,2),demp(x1,y1,3),demp(x1,y0,4)/),&
                                 &(/snap(x0,y0)  ,snap(x0,y1)  ,snap(x1,y1)  ,snap(x1,y0)/))&
                          &/COUNT((/snap(x0,y0)  ,snap(x0,y1)  ,snap(x1,y1)  ,snap(x1,y0)/))

		x1=MAX(1,x0-1);y1=MIN(y0+1,ONYI)
		IF(COUNT(snap(MIN(x0,x1):MAX(x0,x1),MIN(y0,y1):MAX(y0,y1)))/=0)&
              & demp(x0,y0,2)=SUM((/demp(x0,y1,1),demp(x0,y0,2),demp(x1,y0,3),demp(x1,y1,4)/),&
                                  &(/snap(x0,y1)  ,snap(x0,y0)  ,snap(x1,y0)  ,snap(x1,y1)/))&
                           &/COUNT((/snap(x0,y1)  ,snap(x0,y0)  ,snap(x1,y0),  snap(x1,y1)/))

		x1=MIN(x0+1,ONXI);y1=MIN(y0+1,ONYI)
		IF(COUNT(snap(MIN(x0,x1):MAX(x0,x1),MIN(y0,y1):MAX(y0,y1)))/=0)&
              & demp(x0,y0,3)=SUM((/demp(x1,y1,1),demp(x1,y0,2),demp(x0,y0,3),demp(x0,y1,4)/),&
                                 &(/snap(x1,y1)  ,snap(x1,y0)  ,snap(x0,y0)  ,snap(x0,y1)/))&
                          &/COUNT((/snap(x1,y1)  ,snap(x1,y0)  ,snap(x0,y0)  ,snap(x0,y1)/))

		x1=MIN(x0+1,ONXI);y1=MAX(1,y0-1)
		IF(COUNT(snap(MIN(x0,x1):MAX(x0,x1),MIN(y0,y1):MAX(y0,y1)))/=0)&
              & demp(x0,y0,4)=SUM((/demp(x1,y0,1),demp(x1,y1,2),demp(x0,y1,3),demp(x0,y0,4)/),&
                                 &(/snap(x1,y0)  ,snap(x1,y1)  ,snap(x0,y1)  ,snap(x0,y0)/))&
                          &/COUNT((/snap(x1,y0)  ,snap(x1,y1)  ,snap(x0,y1)  ,snap(x0,y0)/))

	END IF
	END DO; END DO
!$OMP END DO
!$OMP BARRIER
!$OMP END PARALLEL
IF(try==1) THEN; WHERE(snap .AND. info==0) info=1
           ELSE; WHERE(snap .AND. info==0) info=2
END IF
END DO

DO try=1,MAX(0,SMTHRANGE)
IF(COUNT(mask)==COUNT(snap)) EXIT
!$OMP PARALLEL PRIVATE(x0,y0)
!$OMP DO
	DO x0=1,ONXI; DO y0=1,ONYI
	IF(mask(x0,y0) .AND. .NOT.snap(x0,y0) .AND. COUNT(info(MAX(1,x0-1):MIN(x0+1,ONXI),MAX(1,y0-1):MIN(y0+1,ONYI))/=0)/=0) THEN

		odat(x0,y0)=SUM(odat(MAX(1,x0-1):MIN(x0+1,ONXI),MAX(1,y0-1):MIN(y0+1,ONYI)),info(MAX(1,x0-1):MIN(x0+1,ONXI),MAX(1,y0-1):MIN(y0+1,ONYI))/=0)&
			&/COUNT(info(MAX(1,x0-1):MIN(x0+1,ONXI),MAX(1,y0-1):MIN(y0+1,ONYI))/=0)

snap(x0,y0)=.TRUE.

	END IF
	END DO; END DO
!$OMP END DO
!$OMP BARRIER
!$OMP END PARALLEL
WHERE(snap .AND. info==0) info=3
END DO
IF(.NOT.SILENT) THEN
	WRITE(LDUMMY,*) 'MASK : ',COUNT(mask);CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0)
	WRITE(LDUMMY,*) 'INTR.: ',COUNT(info==1);CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0)
	WRITE(LDUMMY,*) 'EXTR.: ',COUNT(info==2);CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0)
	WRITE(LDUMMY,*) 'SMTH.: ',COUNT(info==3);CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0)
	WRITE(LDUMMY,*) 'DONE : ',COUNT(snap);CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0)
	IF(COUNT(mask)/=COUNT(snap)) WRITE(LDUMMY,*) 'COULD NOT FILL UP THE WHOLE MASK, PLEASE CHECK I/O.';CALL LWRITE(TRIM(WORKLOGF),TRIM(LDUMMY),0)
END IF

DEALLOCATE(imap,info,demp,snap)
RETURN

END SUBROUTINE bilin5
SUBROUTINE lin_reg(length, novalue, xv, yv, alpha, beta, r2, corr)

IMPLICIT NONE

INTEGER, intent(in)                 :: length
REAL, DIMENSION(length), intent(in) :: xv, yv
REAL, intent(in)                    :: novalue

REAL, intent(out) :: alpha, beta, corr, r2

INTEGER :: NV
REAL :: xm, ym, cov, xvv, yvv, err, tot

	NV=COUNT(xv/=novalue .AND. yv/=novalue)

IF(3<=NV) THEN
	xm  = SUM(xv                                               /REAL(NV),xv/=novalue .AND. yv/=novalue)
	ym  = SUM(yv                                               /REAL(NV),xv/=novalue .AND. yv/=novalue)
	cov = SUM(((xv-xm)*(yv-ym))                                /REAL(NV),xv/=novalue .AND. yv/=novalue)
	xvv = SUM(((xv-xm)*(xv-xm))                                /REAL(NV),xv/=novalue .AND. yv/=novalue)
	yvv = SUM(((yv-ym)*(yv-ym))                                /REAL(NV),xv/=novalue .AND. yv/=novalue)
	err = SUM(((xv*cov/xvv-xm*cov/xvv)*(xv*cov/xvv-xm*cov/xvv))/REAL(NV),xv/=novalue .AND. yv/=novalue)
	tot = SUM(((yv-ym)*(yv-ym))                                /REAL(NV),xv/=novalue .AND. yv/=novalue)

	alpha = ym-cov/xvv*xm
	beta  = cov/xvv
	corr  = cov/(SQRT(xvv)*SQRT(yvv))
	r2    = err/tot
ELSE
	alpha = novalue
	beta  = 0.
	corr  = 0.
	r2    = 0.
END IF

RETURN

END SUBROUTINE lin_reg
SUBROUTINE TIMESET(NT,vec,TIME_SOURCE,TIME_TARGET)

IMPLICIT NONE

INTEGER            , intent(in)    :: NT
CHARACTER(len=800) , intent(in)    :: TIME_SOURCE,TIME_TARGET
REAL, DIMENSION(NT), intent(inout) :: vec
CHARACTER(len=800) :: CDUMMY
INTEGER :: MDAYS,yy,mo,RYY,RMO,RDY,EYY,EMO,EDY
REAL    :: RRC,REC

	READ(TIME_SOURCE(INDEX(TIME_SOURCE,'-')-4:INDEX(TIME_SOURCE,'-')+5),'(I4.4,1X,I2.2,1X,I2.2)') RYY,RMO,RDY
	READ(TIME_SOURCE(1:INDEX(TIME_SOURCE,'-')-5),*) CDUMMY
	IF(TRIM(CDUMMY)=='days') THEN; RRC=1.; ELSEIF(TRIM(CDUMMY)=='hours') THEN; RRC=1./24.; ELSEIF(TRIM(CDUMMY)=='seconds') THEN; RRC=1./(24.*3600.); ELSE; RRC=0.; END IF

	READ(TIME_TARGET(INDEX(TIME_TARGET,'-')-4:INDEX(TIME_TARGET,'-')+5),'(I4.4,1X,I2.2,1X,I2.2)') EYY,EMO,EDY
	READ(TIME_TARGET(1:INDEX(TIME_TARGET,'-')-5),*) CDUMMY
	IF(TRIM(CDUMMY)=='days') THEN; REC=1.; ELSEIF(TRIM(CDUMMY)=='hours') THEN; REC=1.*24.; ELSEIF(TRIM(CDUMMY)=='seconds') THEN; REC=1.*(24.*3600.); ELSE; REC=0.; END IF

	IF(RRC/=0. .AND. REC/=0.) THEN

		MDAYS=0
		DO yy=MIN(RYY,EYY),MAX(RYY,EYY)-1
		DO mo=1,12
			    IF(mo==4 .OR. mo==6 .OR.mo==9 .OR. mo==11)          THEN; MDAYS=MDAYS+SIGN(30,RYY-EYY)
			ELSEIF(mo==2 .AND. (MOD(yy,4)==0 .AND. MOD(yy,400)/=0)) THEN; MDAYS=MDAYS+SIGN(29,RYY-EYY)
			ELSEIF(mo==2 .AND. (MOD(yy,4)/=0 .OR.  MOD(yy,400)==0)) THEN; MDAYS=MDAYS+SIGN(28,RYY-EYY)
			ELSE                                                        ; MDAYS=MDAYS+SIGN(31,RYY-EYY)
			END IF
		END DO
		END DO
		DO mo=MIN(RMO,EMO),MAX(RMO,EMO)-1
			    IF(mo==4 .OR. mo==6 .OR.mo==9 .OR. mo==11)            THEN; MDAYS=MDAYS+SIGN(30,RMO-EMO)
			ELSEIF(mo==2 .AND. (MOD(EYY,4)==0 .AND. MOD(EYY,400)/=0)) THEN; MDAYS=MDAYS+SIGN(29,RMO-EMO)
			ELSEIF(mo==2 .AND. (MOD(EYY,4)/=0 .OR.  MOD(EYY,400)==0)) THEN; MDAYS=MDAYS+SIGN(28,RMO-EMO)
			ELSE                                                          ; MDAYS=MDAYS+SIGN(31,RMO-EMO)
			END IF
		END DO
		MDAYS=MDAYS+RDY-EDY
		vec=(vec*RRC+REAL(MDAYS))*REC
	ELSE
	WRITE(*,*) 'TIME REFERENCE NOT RECOGNIZED!'
	END IF

RETURN

END SUBROUTINE TIMESET
SUBROUTINE LWRITE(FNAME,WCHAR,PBACK)

IMPLICIT NONE

CHARACTER (len=*), intent ( in) :: FNAME,WCHAR
INTEGER          , intent ( in) :: PBACK
INTEGER :: n

OPEN(UNIT=1000,FILE=TRIM(FNAME),STATUS='UNKNOWN',ACTION='WRITE',POSITION='APPEND')
IF(PBACK>0) THEN; DO n=1,PBACK; BACKSPACE(1000); END DO; END IF
WRITE(1000,*) TRIM(WCHAR)
CLOSE(1000)

END SUBROUTINE LWRITE
SUBROUTINE check(status)
INTEGER, intent (in) :: status

IF(status /= NF90_NOERR) THEN
PRINT *, TRIM(NF90_STRERROR(status))
STOP "Stopped"
END IF

END SUBROUTINE check
END PROGRAM standard