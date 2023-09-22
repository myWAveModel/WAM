program bsfile
implicit none
character (len=55) :: bsname
character (len=10) :: ctime, cdate, zone
character (len=14) :: cdatea
character (len=16) :: radin, radout
character (len=17) :: wavein
character (len= 2) :: type
character (len=54) :: string1
character (len=43) :: string2
character (len=76) :: string3
integer, dimension (8) :: datdt
integer :: ios

namelist /nlbsfile/ cdatea, type
read (5,nlbsfile,iostat=ios)
if (ios==0) then
   write (6,*) ' +++ read namelist successfully !'
else
   write (6,*) ' +++ read namelist error !'
   stop
endif
radin = 'RADyyyymmddhh.nc'            !! WAM radiation input file
radin(4:7) = cdatea(1:4)
radin(8:9) = cdatea(5:6)
radin(10:11) = cdatea(7:8)
radin(12:13) = cdatea(9:10)
!print *, ' +++ radin = ', radin
radout = 'radyyyymmddhh.nc'           !! WAM radiation output file
radout(4:7) = cdatea(1:4)
radout(8:9) = cdatea(5:6)
radout(10:11) = cdatea(7:8)
radout(12:13) = cdatea(9:10)
!print *, ' +++ radout = ', radout
string1(1:18) = 'ncks -v VSDX,VSDY '
string1(19:34) = radin
string1(35:35) = ' '
string1(36:51) = radout
string1(52:54) = ' -h'
print *, ' +++ string1 = ', string1
call system (string1)
wavein = 'WAVEyyyymmddhh.nc'          !! WAM inegrated parameters input file
wavein(5:8) = cdatea(1:4)
wavein(9:10) = cdatea(5:6)
wavein(11:12) = cdatea(7:8)
wavein(13:14) = cdatea(9:10)
!print *, ' +++ wavein = ', wavein
string2(1:9) = 'ncks -Ah '
string2(10:25) = radout
string2(26:26) = ' '
string2(27:43) = wavein
print *, ' +++ string2 = ', string2
call system (string2)
bsname = 'yyyymmdd_h-HZG--WAVES-BSeas3-BS-byyyymmdd_fc-fv07.00.nc'
call date_and_time (cdate, ctime, zone, datdt)
call incdate (cdatea,86400)
bsname(1:8) = cdatea(1:8)
bsname(34:41) = cdate
bsname(43:44) = type
!print *, ' +++ bsname = ', bsname
string3(1:3) = 'mv '
string3(4:20) = wavein
string3(21:21) = ' '
string3(22:76) = bsname
print *, ' +++ string3 = ', string3
call system (string3)
stop
end program bsfile
