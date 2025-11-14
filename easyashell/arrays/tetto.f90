PROGRAM tetto
  IMPLICIT NONE

  INTEGER :: filestatus, i, dayi, hour, day, day1
  INTEGER, PARAMETER :: MAX_DAYS=3, unit=42, days_of_month=31
  REAL(KIND=8) :: dT(MAX_DAYS)=0, aT(MAX_DAYS)=0, T(24)=0
  REAL(KIND=8) :: Ti
  CHARACTER(len=100) :: io_msg

  OPEN (unit, file='Exercises/dati_tetto.txt', iostat=filestatus, iomsg=io_msg,&
       status='OLD', action='READ', access='SEQUENTIAL')
  IF (filestatus /= 0) THEN
     WRITE (*,*) io_msg
     STOP
  END IF
  READ (unit, '(I2, 30X)') day1
  REWIND (unit)

  do_over_days: DO dayi = 1,MAX_DAYS
     IF (filestatus /= 0) THEN
        WRITE (*,*) "Error: too many days: expected up to", MAX_DAYS
        STOP
     END IF
     
     do_read_24: DO i =1,24
        READ (unit,'(I2,9X,I2,1X,F8.5)', iostat=filestatus) day, hour, Ti
        ! check match of hour and day in file
        ! supposing 31 days in month
        IF (day /= (day1 + MOD(dayi-1,days_of_month)) .OR. hour /= i-1) THEN
           WRITE (*,*) "Error: mismatch day:", day, "hour:", hour
           STOP
        END IF
        aT(dayi) = aT(dayi) + Ti ! averaging
        T(i) = Ti
     END DO do_read_24
     aT(dayi) = aT(dayi)/24  ! this ends the construction of the average
     do_deviation: DO i = 1,24
        dT(dayi) = dT(dayi) + (T(i) - aT(dayi))**2  ! deviation
     END DO do_deviation
     dT(dayi) = SQRT (dT(dayi)/23) ! this ends the construction of the deviation
 END DO do_over_days

 ! write theaverage and error for each day
 DO i = 1,dayi-1
    WRITE (*,'(A,I2,4X,F5.2,1X,A,1X,F4.2)') "media giorno", i, aT(i), '+-', dT(i)
 END DO

 CLOSE(unit)
 
END PROGRAM tetto
