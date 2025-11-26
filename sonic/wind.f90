! Program to read sonic wind data file, rotate the wind vector
! input file as:
!Mode = 1
!Analog inputs = 3 
!Time 15:51:48 Date 07/04/07

!  4.31  -4.63   1.32 345.60   139    12  3000
PROGRAM wind_sonic
  USE modulefile
  USE modulewind
  IMPLICIT NONE

  ! variables
  INTEGER :: n_sonic_par, error_i, line_i, p_j, v_j
  TYPE(WIND), DIMENSION(:), ALLOCATABLE :: measures
  REAL :: theta, psi, phi
  REAL, PARAMETER :: deg2rad=3.1415926/180.0  ! rad = gradi * deg2rad

  ! file variables
  CHARACTER(len=100) :: fileIn_name="test_allegato.dat"
  CHARACTER(len=50) :: datetime_line
  CHARACTER(len=100) :: fileOut_name="wind_out.dat"
  CHARACTER(len=200) :: io_msg
  CHARACTER :: input_char
  INTEGER :: fileIn_status, fileOut_status, nlines
  INTEGER, PARAMETER :: fileIn=40, fileOut=42

  NAMELIST /paramlist /phi,theta,psi,fileIn_name,fileOut_name

  ! try to open the namelist file
  OPEN (FILE="paramlist.nml", UNIT=fileIn, iostat=fileIn_status, iomsg=io_msg,&
       STATUS='old', ACTION='read')
  ! read namelist if file opened
  IF (fileIn_status == 0) THEN
     READ (fileIn, NML=paramlist, iostat=fileIn_status)
  ELSE
     WRITE (*,*) TRIM(io_msg)
  END IF
  ! if namelist didn't open correctly write defaults
  IF (fileIn_status /= 0) THEN
     CLOSE(fileIn)
     WRITE (*,*) "Worning: namelist not loaded, using default parameters:"
     WRITE (*, NML=paramlist)
     ! ask to continue with defaults
     WRITE (*,*) "Type (c) to continue: "
     READ (*,*) input_char
     IF (input_char /= 'c') STOP
     ! ask to save defaults to file
     WRITE (*,*) "Save this defaults to file (overwrite paramlist.nml)?"
     WRITE (*,*) "Type (y) to confirm: "
     READ (*,*) input_char
     IF (input_char == 'y') THEN
        ! open namelist and write the defaults in it
        CALL OPEN_OUT ("paramlist.nml", fileOut, iostat=fileOut_status)
        IF (fileOut_status == 0) THEN
           WRITE (fileOut, NML=paramlist)
        ELSE
           WRITE (*,*) "Writing paramlist.nml FAILED"
        END IF
        CLOSE(fileOut)
     END IF
  END IF

  ! initialize the angles to radiants
  theta = theta * deg2rad
  phi = phi * deg2rad
  psi = psi * deg2rad

  ! initialize the rotation_matrix  
  rotation_matrix = MATRIX_ROTATE (phi, theta, psi)

  ! open input file
  OPEN (FILE=fileIn_name, UNIT=fileIn, iostat=fileIn_status, iomsg=io_msg,&
       STATUS='old', ACTION='read', ACCESS='STREAM', FORM='FORMATTED')
  IF (fileIn_status /= 0) THEN
     WRITE (*,*) TRIM(io_msg)
     STOP
  END IF

  ! open output file
  CALL OPEN_OUT (fileOut_name, fileOut, fileOut_status)
  IF (fileOut_status /= 0) STOP

  ! read the header
  READ (fileIn,*) ! skip first line
  READ (fileIn,'(15X,I2)') n_sonic_par
  READ (fileIn,'(A)') datetime_line
  READ (fileIn,*)
  WRITE (*,*) "parameters number is: ", n_sonic_par
  IF (n_sonic_par > max_sonic_par) THEN
     WRITE (*,'("Error: Number of parameters is more than expected (<",I2,")")')&
          max_sonic_par
     STOP
  END IF

  CALL COUNT_LINES(fileIn, nlines, fileIn_status, error_i)
  IF (fileIn_status /= 0) THEN
     WRITE (*,*) "Error: Input file closed, unable to read"
     STOP
  END IF
  IF (error_i /= 0) THEN
     WRITE (*,*) "Error: in COUNT_LINES with code: ", error_i
     STOP
  END IF
     

  ! allocate the measures array
  ALLOCATE (measures(nlines), stat=error_i)
  IF (error_i /= 0) THEN
     WRITE (*,*) "Error in allocation with code:", error_i
     STOP
  END IF

  ! read all fileIn, and rotate
  DO line_i=1,nlines
     READ (fileIn,'(3(F6.2,1X),F6.2,1X,*(I5))', iostat=fileIn_status) &
          (measures(line_i)%velocity(v_j), v_j=1,3), measures(line_i)%c,&
          (measures(line_i)%sonic_par(p_j), p_j=1,n_sonic_par)
     IF (fileIn_status /= 0) THEN
        WRITE (*,*) "Error: input file closed before last line; at row: ", line_i,&
             " of ", nlines
        STOP
     END IF

     ! rotate the measure 
     measures(line_i) =  WIND_ROTATE (measures(line_i))
  END DO

  ! write the header
  WRITE (fileOut, *) "Wind measures in the laborator coordinates"
  WRITE (fileOut, '("Analog inputs = ",I2)') n_sonic_par
  WRITE (fileOut, '("NOT A NUMBER ) ",F5.2)') NAN
  WRITE (fileOut, '("number of rows = ",I6)') nlines
  WRITE (fileOut, '(A)') TRIM(datetime_line)

  ! write in fileOut
  DO line_i=1,nlines
     WRITE (fileOut,'(3(F6.2,1X),F6.2,1X,*(I5))') &
          (measures(line_i)%velocity(v_j), v_j=1,3), measures(line_i)%c,&
          (measures(line_i)%sonic_par(p_j), p_j=1,n_sonic_par)
  END DO
  WRITE (*,*) "Total number of NULL data: ", nan_total 
  WRITE (*,*) "Wrote in ", fileOut_name
  CLOSE(fileOut)
  CLOSE(fileIn)

END PROGRAM wind_sonic
