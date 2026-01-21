MODULE modulemath
IMPLICIT NONE

REAL(kind=8), PARAMETER :: deg2rad=3.14159265359/180. ! rad = deg * deg2rad

CONTAINS

SUBROUTINE AVG_STD (values, nan, avg, std)
  ! Returns the average (avg) and/or the standard deviation (std) of values
  ! Minimum 3 values for the standard deviation
  ! If nan is present: parse not a number values
  ! Avg and std are set to nan if not calculable
  
  REAL(kind=8), INTENT(in) :: values(:)
  REAL(kind=8), INTENT(in), OPTIONAL :: nan
  REAL(kind=8), INTENT(out), OPTIONAL :: avg, std
  REAL(kind=8) :: tmp_avg
  INTEGER :: size_count

  ! check values size for averaging
  IF (SIZE(values) < 1) THEN
     ERROR STOP "Empty array 'values' to SUBROUTINE AVG_STD"
  END IF

  ! check presence of outbound arguments
  IF (.NOT. PRESENT(avg) .AND. .NOT. PRESENT(std)) THEN
     ERROR STOP "No output argument to SUBROUTINE AVG_STOP"
  END IF

  ! compute the average
  IF (PRESENT(nan)) THEN
     size_count = COUNT (ABS(nan-values) > 0.01)
     IF (size_count < 1) THEN
        tmp_avg = nan
     ELSE
        tmp_avg = SUM(values, ABS(nan-values) > 0.01) / size_count
     END IF
  ELSE      
     size_count = SIZE(values)
     tmp_avg = SUM(values) / size_count
  END IF

  ! return avg
  IF (PRESENT(avg)) THEN
     avg = tmp_avg
  END IF

  ! compute the standard deviation
  IF (PRESENT(std)) THEN
     ! check values size for standard deviation
     IF (SIZE(values) < 3) THEN
        ERROR STOP "Short size of 'vlues' array to SUBROUTINE AVG_STOP"
     END IF
     IF (PRESENT(nan)) THEN
        IF (size_count < 3) THEN
           std = nan
        ELSE
           std = SQRT(SUM((values-tmp_avg)**2, ABS(nan-values) > 0.01)/(size_count-1))
        END IF
     ELSE
        std = SQRT(SUM((values-tmp_avg)**2)/(size_count-1))
     END IF
  END IF
  
END SUBROUTINE AVG_STD

SUBROUTINE PARSE_DATE (day, month, year, leap_year, valid, day_pro)
  ! Check leap_year and validity of the provided date
  ! If present day_pro returns the number of the day progressive from start of the year
  INTEGER :: day, month, year,
  LOGICAL, INTENT(out) :: leap_year, valid
  INTEGER, INTENT(out), OPTIONAL :: day_pro
  INTEGER :: month_i

  valid = .TRUE.

  IF (year < 0) THEN
     ! WRITE (*,*) "Error: year is not valid"
     valid = .FALSE.
     RETURN
  ELSE IF (MOD(year,400) == 0) THEN
     leap_year = .TRUE.
  ELSE IF (MOD(year,4) == 0 .AND. MOD(year,100) /= 0) THEN
     leap_year = .TRUE.
  ELSE
     leap_year = .FALSE.
  END IF

  IF (month < 1 .OR. month > 12) THEN
     ! WRITE (*,*) "Error: month is not valid"
     valid = .FALSE.
     RETURN

  ELSE IF (month == 11 .OR. month == 4 .OR. month == 6 .OR. month == 9) THEN
     IF (day < 0 .OR. day > 30) THEN
        ! WRITE (*,*) "Error: day is not valid"
        valid = .FALSE.
        RETURN
     END IF
  ELSE IF (month == 2 .AND. leap_year .AND. (day < 0 .OR. day > 29)) THEN
     ! WRITE (*,*) "Error: day is not valid"
     valid = .FALSE.
     RETURN
  ELSE IF (month == 2 .AND. (day < 0 .OR. day > 28)) THEN  ! not leap
     ! WRITE (*,*) "Error: day is not valid"
     valid = .FALSE.
     RETURN
  ELSE IF (day < 0 .OR. day > 31) THEN
     ! WRITE (*,*) "Error: day is not valid"
     valid = .FALSE.
     RETURN
  ENd IF

  IF (PRESENT(day_pro)) THEN
     day_pro = 0
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
  END IF

END SUBROUTINE PARSE_DATE

END MODULE modulemath

PROGRAM test
  USE  modulemath
  IMPLICIT NONE

  REAL(kind=8), DIMENSION(3) :: val=(/7,7,7/)
  REAL(kind=8) a, s
  CALL AVG_STD (val, nan=7._8, avg=a, std=s)
  WRITE (*,*) "a: ", a, "s: ", s
  
END PROGRAM test
