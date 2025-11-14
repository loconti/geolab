PROGRAM day_progressive
  IMPLICIT NONE

  INTEGER :: day, month, year, day_pro=0, month_i
  LOGICAL :: leap_year

  WRITE (*,*) "Write a date:"
  WRITE (*,*) "day:"
  READ (*,*) day
  WRITE (*,*) "month:"
  READ (*,*) month
  WRITE (*,*) "year:"
  READ (*,*) year

  IF (year < 0) THEN
     WRITE (*,*) "Error: year is not valid"
     STOP
  ELSE IF (MOD(year,400) == 0) THEN
     leap_year = .TRUE.
  ELSE IF (MOD(year,4) == 0 .AND. MOD(year,100) /= 0) THEN
     leap_year = .TRUE.
  ELSE
     leap_year = .FALSE.
  END IF

  IF (month < 1 .OR. month > 12) THEN
     WRITE (*,*) "Error: month is not valid"
     STOP
  ELSE IF (month == 11 .OR. month == 4 .OR. month == 6 .OR. month == 9) THEN
     IF (day < 0 .OR. day > 30) THEN
        WRITE (*,*) "Error: day is not valid"
        STOP
     END IF
  ELSE IF (month == 2 .AND. leap_year .AND. (day < 0 .OR. day > 29)) THEN
     WRITE (*,*) "Error: day is not valid"
     STOP
  ELSE IF (month == 2 .AND. (day < 0 .OR. day > 28)) THEN  ! not leap
     WRITE (*,*) "Error: day is not valid"
     STOP
  ELSE IF (day < 0 .OR. day > 31) THEN
     WRITE (*,*) "Error: day is not valid"
     STOP
  ENd IF

  DO month_i = 1, month-1
     IF (month_i == 11 .OR. month_i == 4 .OR. month_i == 6 .OR. month_i == 9) THEN
        day_pro = day_pro + 30
     ELSE IF (month_i /= 2) THEN
        day_pro = day_pro + 31
     ELSE IF (leap_year) THEN ! Feb. leap
        day_pro = day_pro + 29
     ELSE ! Feb. not leap
        day_pro = day_pro + 28
     END IF
  END DO
  day_pro = day_pro + day
  WRITE (*,*) "day progressive is:", day_pro 

END PROGRAM day_progressive
