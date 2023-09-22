!
!------------------------------------------------------------------
!--   Program creates the WAM userinput including the correct   ---
!--   time dependent parameters and hot start option   ------------
!------------------------------------------------------------------
!----------------------------   Arno Behrens, June 2009  ----------
!------------------------------------------------------------------
!
program make_wam_userin
implicit none
character (len=14) :: cdatea, cdatee
character (len=80) :: filein, fileut, string
character (len= 2) :: type
logical :: lbounf, lcoldstart
integer :: iopti, loop, moutp, io, idt
namelist /nlwamin/ filein, fileut, cdatea, cdatee, lbounf, type
read (5,nlwamin)
cdatea(11:14) = '0000'
cdatee(11:14) = '0000'
print *, ' +++ new WAM UserInput file = ', trim(fileut)
if (type=='sm') then
  idt = 6
else if (type=='fc') then
  idt = 3
else
  idt = 6
endif

open (10,file=trim(filein))
open (20,file=trim(fileut))
open (30,file='RESTART')

read (30,*) iopti
if (iopti==1) then
   lcoldstart = .true.
else
   lcoldstart = .false.
endif
loop = 1
do while (.true.)
   read (10,'(a)',iostat=io) string
   if (io<0) exit
   if (loop==23) then
      string( 2:15) = cdatea
      string(18:31) = cdatee
   endif
   if (loop==34) write (string(11:11),'(l1)') lcoldstart
   if (loop==119) write (string(17:17),'(l1)') lbounf
   if (loop==152) then
     write (string(8:8),'(i1)') idt
     write (string(19:19),'(i1)') idt
   endif
   if (loop==297) then
     string(64:64) = 'w'
     string(65:74) = cdatea(1:10)
   endif
   write (20,'(a)') trim(string)
   loop = loop+1
enddo
print *, ' +++ Lines included in WAM userinput : ', loop-1
stop
end program make_wam_userin
