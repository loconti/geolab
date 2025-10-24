PROGRAM orbitsFile
  IMPLICIT NONE

  CHARACTER (len=100) :: input, filename='', io_msg
  CHARACTER :: choice
  INTEGER, PARAMETER :: unit=42
  INTEGER :: filestatus
  LOGICAL :: filecheck
  INTEGER :: dt(8) ! date and time

  REAL :: epsilon=0, r
  REAL, PARAMETER :: delta=0.25
  INTEGER, PARAMETER :: size=1200  ! Km
  INTEGER :: theta
  REAL, PARAMETER :: deg2rad=3.14159265358/180.0  ! rad = gradi * deg2rad

  CALL date_and_time(values=dt)
  output_file: DO WHILE (filename == '')  ! opening output file loop
     WRITE (*,*) 'Type the output file name or q to exit:'
     READ (*,*) input
     IF (LEN(input) == 100) THEN  ! Worning maximum length of input reached
        WRITE (*,*) "Worning Reached Max. buffer size (100)"
        WRITE (*,*) "File name read as", input
     END IF
     IF (input == 'q') STOP
     INQUIRE (file=input, exist=filecheck)
     if_file_present: IF (filecheck) THEN  ! file exists will it be replaced?
        choice = 'a'
        WRITE (*,*) 'File exists, trunk and overwrite it?  (y/n)'
        overwrite: DO WHILE (choice /= 'y' .AND. choice /= 'n')
           READ (*,*) choice
           IF (choice == 'y') THEN  ! file will be replaced
              OPEN (unit, file=input, iostat=filestatus, iomsg=io_msg,&
                   status='REPLACE', action='WRITE', access='SEQUENTIAL')
              IF (filestatus == 0) THEN
                 filename = input  ! now the opening file loop closes
                 EXIT overwrite
              ELSE
                 WRITE (*,*) io_msg
                 EXIT overwrite ! user will be prompted again for file name
              END IF
           ELSE  ! user dont want to trunk
              EXIT
           END IF
        END DO overwrite
     ELSE
        OPEN (unit, file=input, iostat=filestatus, iomsg=io_msg,&
             status='NEW', action='WRITE', access='SEQUENTIAL')
        IF (filestatus == 0) THEN
           filename = input  ! opening file loop closes
           EXIT
        ELSE
           WRITE (*,*) io_msg  ! user will be prompted again
        END IF
     END IF if_file_present
  END DO output_file

  WRITE (unit,*) "Campionating orbits for different values of eccentricity"
  write(unit, '(I2.2, "/", I2.2, "/", I4.4, "  ", I2.2, ":", I2.2, ":", I2.2)') dt(3), dt(2), dt(1), dt(4), dt(5), dt(6)
  ! evaluating orbits and writing to file
  epsilon_do: DO WHILE (epsilon <= 0.5)
     WRITE (unit,*) 'epsilon: ', epsilon
     theta_do: DO theta = 1,360,15
        r = size / (1 - epsilon * COS(theta*deg2rad))
        WRITE (unit,*) '   theta: ', theta, '   r: ', r
     END DO theta_do
     WRITE (unit,*) '.............................' 
     epsilon = epsilon + delta
  END DO epsilon_do
  CALL date_and_time(values=dt)
  write(unit, '(I2.2, "/", I2.2, "/", I4.4, "  ", I2.2, ":", I2.2, ":", I2.2)') dt(3), dt(2), dt(1), dt(4), dt(5), dt(6)
  CLOSE (unit)

END PROGRAM orbitsFile
