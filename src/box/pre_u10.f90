
!--  program reads NetCDF wind fields and converts   ---
!--  those to the format required by WAM   -------------
!-------------------------------------------------------
!--------------------   Arno Behrens, August 2016   ----
!-------------------------------------------------------
program pre_u10
implicit none
real, allocatable, dimension (:,:,:) :: uu, vv, u2, v2
real, allocatable, dimension (:,:) :: u, v
character (len=100) :: path1, path2, fnameu, fwind
character (len=14) :: cstart, cdate
character (len= 5) :: xox
character (len= 1) :: equal
character (len= 6) :: string
character (len= 2) :: tplus
integer :: ios, nx, ny, loop, k, iall, ltim, i, j
integer :: time, icode, iparau, iparav, ishift, iz
real :: west, east, south, north, dx, dy

namelist /nlwind/ path1, path2, cstart, tplus
read (5,nlwind,iostat=ios)
if (ios==0) then
   write (6,*) ' +++ read namelist successfully !'
else
   write (6,*) ' +++ read namelist error !'
   stop
endif
west = -19.                       !! wind grid
east = 42.00
south = 30.00
north = 48.00
dx = 0.125
dy = 0.125
icode = 3
iparau = 33
iparav = 34
if (tplus=='00') then
   ishift = 21600
else
   ishift = 10800
endif
cdate = cstart
fnameu = trim(path1)//'ecmwf_'//cdate(1:8)//'_'//tplus
print *, ' +++ fnameu = ', trim(fnameu)
if (tplus=='12') then
   call incdate (cdate,54000)
endif

open (10,file=fnameu)
!
!==>  read u-component of original wind field
!
read (10,'(1x)')
read (10,'(1x)')
read (10,'(7x,i3)') nx
read (10,'(7x,i3)') ny
if (tplus=='00') then
   read (10,'(24x,i2)') time
else
   read (10,'(24x,i3)') time
endif
print *, ' +++ nx, ny, time : ', nx, ny, time

allocate (u(nx,ny), v(nx,ny))
allocate (uu(nx,ny,time), vv(nx,ny,time))
allocate (u2(nx,ny,time), v2(nx,ny,time))

string = 'xxxxxx'
iz = 0
do while (string/='U10M =')
   read (10,'(1x,a6)') string
   iz = iz+1
enddo
print *, ' +++ iz, string = ', iz, string

do ltim = 1,time                             !! time steps
   read (10,*) u
   u2(:,:,ltim) = u
enddo
read (10,'(1x)')
read (10,'(1x,a6)') string
print *, ' +++ string : ', string
do ltim = 1,time
   read (10,*) v
   v2(:,:,ltim) = v
enddo

fwind = trim(path2)//'w'//cdate(1:10)
do ltim = 1,time
   do k=1,ny
      do i=1,nx
         uu(i,ny-k+1,ltim) = u2(i,k,ltim)    !! turn wind field from
         vv(i,ny-k+1,ltim) = v2(i,k,ltim)    !! north to south
      enddo
   enddo
   print *, ' +++ fwind : ', trim(fwind)
   open (40,file=trim(fwind))
!
!==>  write final wind fields in WAM format
!
   write (40,'(6f12.7,3i6)') south, north, west, east, dx, dy, nx ,ny, icode
   write (40,'(3a2,a4,i4)') cdate(9:10), cdate(7:8), cdate(5:6), cdate(1:4), iparau
   write (40,'(13f6.1)') uu(:,:,ltim)
   write (40,'(3a2,a4,i4)') cdate(9:10), cdate(7:8), cdate(5:6), cdate(1:4), iparav
   write (40,'(13f6.1)') vv(:,:,ltim)
   close (40)
   if (ltim>23) then
      ishift = 21600
   endif
   call incdate (cdate,ishift)
   fwind = trim(path2)//'w'//cdate(1:10)
enddo
stop
end program pre_u10
