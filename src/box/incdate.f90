SUBROUTINE INCDATE (CDATE, ISHIFT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   INCDATE - TO UPDATE DATE TIME GROUP                                        !
!                                                                              !
!     L. BERTOTTI, P.JANSSEN.                                                  !
!                                                                              !
!     H. GUNTHER   ECMWF  NOVEMBER 1989    NEGATIVE INCREMENTS.                !
!     H. GUNTHER   GKSS   NOVEMBER 2001    FT90 AND CENTURY AND SECONDS.       !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       UPDATING DATE TIME GROUP.                                              !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
!     REFERENCES.                                                              !
!     -----------                                                              !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!
!     INTERFACE VARIABLES.
!     --------------------

CHARACTER (LEN=14), INTENT(INOUT) :: CDATE  !! DATE TIME GROUP (YYYYMMDDHHMMSS).
INTEGER,            INTENT(IN)    :: ISHIFT !! TIME INCREMENT IN SECONDS.

! ---------------------------------------------------------------------------- !
!
!     LOCAL VARIABLES.
!     ----------------

INTEGER, SAVE ::  MON(12) =(/31,28,31,30,31,30,31,31,30,31,30,31/)

INTEGER ::  YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
INTEGER ::  DELT, MDAY

! ---------------------------------------------------------------------------- !
!
!   1.0 RETURN IF TIME INCREMENT IS ZERO.
!       ---------------------------------

DELT = ISHIFT
IF (ABS(DELT).EQ.0) RETURN

! ---------------------------------------------------------------------------- !
!                                                                              !
!*    2.0 SPLITE DATE TIME GROUP INTO SECONDS, MINUTE, HOUR, DAY, MONTH, YEAR. !
!         -------------------------------------------------------------------- !

READ (CDATE,'(I4,5I2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.0 ADD AND CHECK SECONDS.                                               !
!         ----------------------                                               !
!                                                                              !
!   2.1 IF SECONDS ARE BETWEEN 0 AND 60 RETURN
!       --------------------------------------

SECOND = SECOND + DELT
IF (SECOND.GE.0. .AND. SECOND.LT.60.) THEN
   WRITE(CDATE,'(I4.4,5I2.2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
   RETURN
END IF

!*  2.2 NEW MIMUTES AND SECONDS.
!       ------------------------

DELT = MODULO(SECOND,60)
MINUTE = MINUTE +(SECOND-DELT)/60
SECOND = DELT

! ---------------------------------------------------------------------------- !
!
!   3.0 CHECK MINUTES.
!       --------------

IF (MINUTE.GE.60) THEN
   HOUR = HOUR + MINUTE/60        !! MINUTES > 59 ==> NEW HOURS.
ELSE IF (MINUTE.LT.0) THEN
   HOUR = HOUR + (MINUTE-59)/60   !! MINUTES < 0  ==> NEW HOURS.
ELSE
   WRITE (CDATE,'(I4.4,5I2.2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
   RETURN                         !! ALL DONE  ==>  RETURN
END IF
MINUTE = MODULO(MINUTE,60)        !! NEW MINUTES.

! ---------------------------------------------------------------------------- !
!
!   4.0 CHECK HOURS.
!       ------------

IF (HOUR.GE.24) THEN
   DAY =  DAY + HOUR/24           !! HOURS > 23 ==> NEW DAYS.
ELSE IF (HOUR.LT.0) THEN
   DAY =  DAY + (HOUR-23)/24      !! HOURS < 0  ==> NEW DAYS.
ELSE
   WRITE (CDATE,'(I4.4,5I2.2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
   RETURN                         !! ALL DONE  ==>  RETURN
END IF
HOUR = MODULO(HOUR,24)            !! NEW HOURS.

! ---------------------------------------------------------------------------- !
!
!   5.0 CHECK DAYS.
!       ------------
!
!   5.1 IF DAYS ARE GREATER THAN DAYS OF MONTH. NEW DAY AND MONTH AND YEAR.
!       -------------------------------------------------------------------

MDAY = MON(MONTH)
IF (MONTH.EQ.2 .AND. MOD(YEAR,4).EQ.0) THEN
   IF (MOD(YEAR,400).EQ.0 .OR. MOD(YEAR,100).NE.0) MDAY = 29
END IF

DO WHILE (DAY > MDAY)
   DAY =  DAY - MDAY
   MONTH = MONTH+1
   IF (MONTH.GE.13) THEN
      YEAR = YEAR+1
      MONTH = MONTH-12
   END IF
   MDAY = MON(MONTH)
   IF (MONTH.EQ.2 .AND. MOD(YEAR,4).EQ.0) THEN
      IF (MOD(YEAR,400).EQ.0 .OR. MOD(YEAR,100).NE.0) MDAY = 29
   END IF
END DO

!   5.2 IF DAYS ARE LESS THAN 1. NEW DAY AND MONTH AND YEAR.
!       ----------------------------------------------------

DO WHILE ( DAY < 1)
   MONTH = MONTH-1
   IF (MONTH.EQ.0) THEN
      MONTH = 12
      YEAR = YEAR-1
   END IF
   MDAY = MON(MONTH)
   IF (MONTH.EQ.2 .AND. MOD(YEAR,4).EQ.0) THEN
      IF (MOD(YEAR,400).EQ.0 .OR. MOD(YEAR,100).NE.0) MDAY = 29
   END IF
   DAY = DAY + MDAY
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6.0 COMPOSE NEW DATE TIME GROUP.                                         !
!         ----------------------------                                         !

WRITE (CDATE,'(I4.4,5I2.2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND

END SUBROUTINE INCDATE
