PROGRAM sensors
  IMPLICIT NONE

  INTEGER, PARAMETER :: unitR1=40, unitR2=41, unitW=42
  INTEGER :: filestatus_1, filestatus_2, filestatus_3, n_lines=1, code_1, code_2
  LOGICAL :: filecheck
  REAL :: T1, T2, RH1, RH2, SH1, SH2
  CHARACTER (len=20) :: datetime_1, datetime_2
  CHARACTER (len=100) :: io_msg, line, format_read
  CHARACTER (len=50), PARAMETER :: filename_out='sensors_out.txt'
  CHARACTER (len=50), PARAMETER :: filename_1='Exercises/10486917_4.txt', filename_2='Exercises/10512170_4_new.txt'

  ! try to open first file
  OPEN (unitR1, file=filename_1, iostat=filestatus_1, iomsg=io_msg,&
       status='OLD', action='READ', access='SEQUENTIAL')
  IF (filestatus_1 /= 0) THEN
     WRITE (*,*) io_msg
     STOP
  END IF

  ! try to open second file
  OPEN (unitR2, file=filename_2, iostat=filestatus_2, iomsg=io_msg,&
       status='OLD', action='READ', access='SEQUENTIAL')
  IF (filestatus_2 /= 0) THEN
     WRITE (*,*) io_msg
     STOP
  END IF

  ! check existance of file out: if true: replace
  INQUIRE (file=filename_out, exist=filecheck)
  IF (filecheck) THEN
     OPEN (unitW, file=filename_out, iostat=filestatus_3, iomsg=io_msg,&
          status='REPLACE', action='WRITE', access='SEQUENTIAL')
  ELSE
     OPEN (unitW, file=filename_out, iostat=filestatus_3, iomsg=io_msg,&
          status='NEW', action='WRITE', access='SEQUENTIAL')
  END IF
  IF (filestatus_3 /= 0) THEN
     WRITE (*,*) io_msg
     STOP
  END IF

  ! identifieres of the sensors
  READ (unitR1, '(12X,I8)') code_1
  READ (unitR2, '(12X,I8)') code_2
  ! skipping headers
  READ (unitR1, *)
  READ (unitR2, *)
  ! write the output header
  WRITE (unitW, *)  'Specific Humidity from sensors', code_1, 'and', code_2
  WRITE (unitW, *)  '# Date, Time "GMT+01:00", Temp1, Temp2 "°C", RH1, RH2 "%", SH1, SH2'
  ! loop through input files
  input_loop: DO WHILE (filestatus_1 == 0 .AND. filestatus_2 == 0)
     ! shifting the first read based on the number of the line
     ! counting +1 for the space
     ! in the format: "(...)" what is in: '...' is written directly in: format_read 
     WRITE (format_read,"('(', I1, 'X,A19,8X,F6.3,8X,F6.3)')") INT(LOG10(REAL(n_lines)))+1+1
     READ(unitR1,format_read,iostat=filestatus_1)&
          datetime_1, T1, RH1
     READ(unitR2,format_read,iostat=filestatus_2)&
          datetime_2, T2, RH2
     IF (datetime_1 /= datetime_2) THEN
        WRITE (*,*) 'Error: there are not matching lines at:', n_lines
        STOP
     END IF
     ! compute the specific humidity
     SH1 = specHum (T1,RH1)
     SH2 = specHum (T2,RH2)
     ! write the output
     WRITE (unitW, '(I5,4X,A19,4X,F6.3,4X,F6.3,4X,F6.3,4X,F6.3,4X,F6.3,4X,F6.3)')&
          n_lines, datetime_1, T1, T2, RH1, RH2, SH1, SH2
     n_lines = n_lines + 1
  END DO input_loop
  ! check if both files ended
  IF (filestatus_1 == 0) THEN
     READ (unitR1,*) line
     IF (LEN (line) > 10) WRITE (*,*) 'Error: file', code_1, 'longer than file', code_2
  END IF
  IF (filestatus_2 == 0) THEN
     READ (unitR2,*) line
     IF (LEN (line) > 10) WRITE (*,*) 'Error: file', code_2, 'longer than file', code_1
  END IF
  CLOSE (unitR1)
  CLOSE (unitR2)
  CLOSE (unitW)

CONTAINS
  FUNCTION specHum(T,RH) result(SH)
    REAL, INTENT(in) :: T, RH
    REAL :: SH, e
    INTEGER, PARAMETER :: p=1013

    e = 0.01 * 6.112 * RH * EXP((17.67*T)/(T+243.5))
    SH = 0.622 * (e/(p-e)) * 1000
  END FUNCTION specHum


END PROGRAM sensors


! Plot Title: 10486917       
! # Date "Time, GMT+01:00" "Temp, °C (LGR S/N: 10486917, SEN S/N: 10486917)" "Temp - Avg, °C (LGR S/N: 10486917, SEN S/N: 10486917)" "RH, % (LGR S/N: 10486917, SEN S/N: 10486917)" "RH - Avg, % (LGR S/N: 10486917, SEN S/N: 10486917)" "DewPt, °C (LGR S/N: 10486917, SEN S/N: 10486917)"
! 1 18/05/2015 12.00.00 25.409 25.409 43.827 43.827 12.229
! 2 18/05/2015 12.10.00 25.167 25.385 42.986 43.499 11.716
! 3 18/05/2015 12.20.00 25.118 25.167 42.557 42.986 11.520


! Plot Title: 10512170       
! # Date "Time, GMT+01:00" "Temp, °C (LGR S/N: 10512170, SEN S/N: 10512170)" "Temp - Avg, °C (LGR S/N: 10512170, SEN S/N: 10512170)" "RH, % (LGR S/N: 10512170, SEN S/N: 10512170)" "RH - Avg, % (LGR S/N: 10512170, SEN S/N: 10512170)" "DewPt, °C (LGR S/N: 10512170, SEN S/N: 10512170)"
! 1 18/05/2015 12.00.00 25.555 25.555 50.698 50.698 14.599
! 2 18/05/2015 12.10.00 25.579 25.531 50.510 50.599 14.563
! 3 18/05/2015 12.20.00 25.628 25.603 49.910 50.194 14.423
