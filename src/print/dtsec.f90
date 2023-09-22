SUBROUTINE TIMESET(NT,vec,TIME_SOURCE,TIME_TARGET)

IMPLICIT NONE

INTEGER            , intent(in)    :: NT
CHARACTER(len=800) , intent(in)    :: TIME_SOURCE,TIME_TARGET
REAL*8, DIMENSION(NT), intent(inout) :: vec
CHARACTER(len=800) :: CDUMMY
INTEGER :: MDAYS,yy,mo,RYY,RMO,RDY,EYY,EMO,EDY
REAL*8  :: RRC, REC, xhour
xhour = 3600.

READ(TIME_SOURCE(INDEX(TIME_SOURCE,'-')-4:INDEX(TIME_SOURCE,'-')+5),'(I4.4,1X,I2.2,1X,I2.2)') RYY,RMO,RDY
READ(TIME_SOURCE(1:INDEX(TIME_SOURCE,'-')-5),*) CDUMMY
IF(TRIM(CDUMMY)=='days') THEN; RRC=1.; ELSEIF(TRIM(CDUMMY)=='hours') THEN; RRC=1./24.; ELSEIF(TRIM(CDUMMY)=='seconds') THEN; RRC=1.0d0/(24.0d0*xhour); ELSE; RRC=0.; END IF

READ(TIME_TARGET(INDEX(TIME_TARGET,'-')-4:INDEX(TIME_TARGET,'-')+5),'(I4.4,1X,I2.2,1X,I2.2)') EYY,EMO,EDY
READ(TIME_TARGET(1:INDEX(TIME_TARGET,'-')-5),*) CDUMMY
IF(TRIM(CDUMMY)=='days') THEN; REC=1.; ELSEIF(TRIM(CDUMMY)=='hours') THEN; REC=1.*24.; ELSEIF(TRIM(CDUMMY)=='seconds') THEN; REC=1.0d0*(24.0d0*xhour); ELSE; REC=0.; END IF

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
