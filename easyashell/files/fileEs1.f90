PROGRAM file_rad
IMPLICIT NONE
INTEGER, PARAMETER :: in_unit=42, out_unit=43
INTEGER :: iostat, round, degree, minute, i_second
LOGICAL :: decimal=.TRUE., int_seconds=.TRUE., trunc_seconds=.TRUE.
CHARACTER :: input='a'
REAL :: r_angle, d_angle, r_second
REAL, PARAMETER :: deg2rad=3.14159265358/180.0  ! rad = gradi * deg2rad
! Asking the user for the output format
WRITE (*,*) 'Print d for decimal or s for sessagesimal'
DO WHILE (input /= 'd' .AND. input /= 's')
   READ (*,*) input
END DO
IF (input == 's') decimal=.FALSE.
IF (.NOT. decimal) THEN
   input='a'
   ! Asking the user for the seconds format
   WRITE (*,*) 'Print i for integer or r for real representation of seconds'
   DO WHILE (input /= 'i' .AND. input /= 'r')
      READ (*,*) input
   END DO
   IF (input == 'r') int_seconds=.FALSE.
   IF (int_seconds) THEN
         input='a'
         WRITE (*,*) 'Print t for trunc or r for round representation of seconds'
         DO WHILE (input /= 't' .AND. input /= 'r')
            READ (*,*) input
         END DO
         IF (input == 'r') trunc_seconds=.FALSE.
      END IF   
END IF
OPEN(UNIT=in_unit, file='radians.dat', STATUS='old', ACCESS='sequential', ACTION='read')
OPEN(UNIT=out_unit, file='degrees.out', STATUS='replace', ACTION='write')
! Writing the header of the output file
WRITE (out_unit,*) 'Conversion from radians to degrees'
IF (decimal) WRITE (out_unit,*) 'radians -> rounds, decimal degrees'
IF (.NOT. decimal) THEN
   IF (int_seconds) THEN
      IF (trunc_seconds) THEN
         WRITE (out_unit,*) 'radians -> rounds, degrees, minutes, integer seconds (truncated)'
      ELSE
         WRITE (out_unit,*) 'radians -> rounds, degrees, minutes, integer seconds (rounded)'
      END IF
   ELSE
      WRITE (out_unit,*) 'radians -> rounds, degrees, minutes, real seconds'
   END IF
END IF
WRITE (out_unit,*) '----------------------------------------------'
DO  ! Loop over the input file
   READ (in_unit,*, IOSTAT=iostat) r_angle
   IF (iostat /= 0) EXIT  ! Exit the loop at the end of the file
   d_angle = r_angle / deg2rad
   round = INT(d_angle / 360)
   d_angle = d_angle - round
   IF (decimal) THEN
      WRITE (out_unit,*) r_angle, '->', round, 'round', d_angle, 'degrees'
   ELSE
      degree = INT(d_angle)
      d_angle = d_angle - degree
      minute = INT(d_angle*60.)
      d_angle = d_angle - minute*(1./60.)
      IF (int_seconds) THEN
         IF (trunc_seconds) THEN
            i_second = INT(d_angle*3600.)
         ELSE
            i_second = NINT(d_angle*3600.)
         END IF
         WRITE (out_unit,*) r_angle, '->', round, 'round', degree, 'degrees', minute, 'minutes', i_second, 'seconds'
      ELSE
         r_second = d_angle*3600.
         WRITE (out_unit,'(F10.6,A,I4,A,I4,A,I4,A,F6.2,A)') r_angle, '    ->    ', round, '  round    ', degree, '  degrees    ', minute, '  minutes    ', r_second, '  seconds    '
      END IF
   END IF
END DO
CLOSE(in_unit)
CLOSE(out_unit)
END PROGRAM file_rad