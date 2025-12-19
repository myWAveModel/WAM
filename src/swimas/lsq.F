	subroutine lsq(a,m,n,mp,np,b,tol,x,u,v,w)
c	solves LSQ problem Ax = b using SVD
c	using Num.Rec. routines
c	svdcmp (singular value decomposition), and
c	svbksb (solve x in Ax=b)
c
c	LSQ is a replacement for NAG routine f04jgf

c	Variables:
c	a(mp,np)	real	input	Matrix A 
c	m		integer	input	# of rows of A
c	n		integer	input	# of columns of A (n <= m)
c	mp		integer input	Physical dim of A (>= m)
c	np		integer input	Physical dim of A (>= n)
c	b(mp)		real	input	Input vector b (1..m)
c	tol		real	input	Tolerance: all singular 
c					values smaller than tol times
c					the maximum singular value of A
c					are set to 0 (to avoid num.
c					errors)
c	x(np)		real	output 	Output vector x (1..n) 
c	u(mp,np)	real		Workspace
c	v(np,np)	real		Workspace
c	w(np)		real		Workspace

c	***************************************************************

	implicit none

	integer :: m,n,mp,np
	real, dimension(mp,np) :: a(mp,np),u(mp,np)
        real, dimension(np,np) :: v(np,np)
        real, dimension(np) :: w, x
        real, dimension(mp) :: b

	integer :: i,j
	real ::	wmin, wmax, tol
	
c	***************************************************************

c	initialisation
	do j = 1,np
	  do i = 1,mp
	    u(i,j) = a(i,j)
	  end do
	  x(j)=0.
	end do

c	singular value decomposition
	call svdcmp(u,m,n,mp,np,w,v)

c	Set all sing values < tol * (max sing value) to zero
	wmax = 0.
	do i = 1,n
	  if(w(i).gt.wmax) wmax=w(i)
	end do
	wmin = wmax*tol
	do i = 1,n
	  if(w(i).lt.wmin)w(i)=0.
	end do

c	Solve (LSQ) Ax = b
	call svbksb(u,w,v,m,n,mp,np,b,x)

	return
	end subroutine lsq
C-------------------------------------------------------------------------
