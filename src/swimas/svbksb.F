      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x)
      INTEGER :: m,mp,n,np
      real, dimension(mp) :: b
      real, dimension(mp,np) :: u
      real, dimension(np,np) :: v
      real, dimension(np) :: w, x
      INTEGER, PARAMETER :: NMAX=500
      INTEGER i,j,jj
      REAL :: s
      real, dimension(NMAX) :: tmp
      do 12 j=1,n
        s=0.
        if(w(j).ne.0.)then
          do 11 i=1,m
            s=s+u(i,j)*b(i)
11        continue
          s=s/w(j)
        endif
        tmp(j)=s
12    continue
      do 14 j=1,n
        s=0.
        do 13 jj=1,n
          s=s+v(j,jj)*tmp(jj)
13      continue
        x(j)=s
14    continue
      return
      END SUBROUTINE SVBKSB
