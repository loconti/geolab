PROGRAM zpT
  USE modulefile, ONLY: OPEN_OUT
  USE modulezpT, ONLY: MEASURE, ALTITUDE, MIDDLE_PRESSURE
  IMPLICIT NONE

  INTEGER :: nA, nB, iA, iB, iAB, nAB, zmin, zmax, z200, error_i
  TYPE(MEASURE), DIMENSION(:), ALLOCATABLE :: measureA, measureB, measureAB
  TYPE(MEASURE) :: measureA0, measureB0
  ! file variables
  INTEGER, PARAMETER :: fileIn=40, fileOut=42
  REAL :: p200A, p200B
  ! default file names
  CHARACTER(len=200) :: fileA_name="A-pT.dat", fileB_name="B-pT.dat",&
       fileA_out_name="A-zpT.dat", fileB_out_name="B-zpT.dat", fileAB_out_name="AB-Dp.dat"
  CHARACTER(len=200) :: io_msg, lineA_str, lineB_str
  CHARACTER :: input_char
  INTEGER :: fileIn_status, fileOut_status, str_index
  NAMELIST /paramlist /fileA_name,fileB_name,fileA_out_name, fileB_out_name, fileAB_out_name

  ! try to open the namelist file
  OPEN (FILE="paramlist.nml", UNIT=fileIn, iostat=fileIn_status, iomsg=io_msg,&
       STATUS='old', ACTION='read')
  ! read namelist if file opened
  IF (fileIn_status == 0) THEN
     READ (fileIn, NML=paramlist, iostat=fileIn_status)
  ELSE
     WRITE (*,*) TRIM(io_msg)
  END IF
  CLOSE(fileIn)
  ! if namelist didn't open correctly write defaults
  IF (fileIn_status /= 0) THEN
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

  ! working on file A
  OPEN (FILE=fileA_name, UNIT=fileIn, iostat=fileIn_status, iomsg=io_msg,&
       STATUS='old', ACTION='read')
  IF (fileIn_status /= 0) THEN
     WRITE (*,*) TRIM(io_msg)
     STOP
  END IF
  ! create fileA_out, overwrite if exists
  CALL OPEN_OUT (fileA_out_name, fileOut, iostat=fileOut_status)
  IF (fileOut_status /= 0) THEN
     STOP
  END IF

  ! read header
  READ (fileIn,'(A)') lineA_str
  ! h0
  str_index = INDEX (lineA_str, "h0=")
  IF (str_index<1) THEN
     WRITE (*,*) "Error: missing h0 in header of ", fileA_name
     STOP
  END IF
  READ (lineA_str(str_index+3:),*) measureA0%z 
  ! p0
  str_index = INDEX (lineA_str, "p0=")
  IF (str_index<1) THEN
     WRITE (*,*) "Error: missing p0 in header of ", fileA_name
     STOP
  END IF
  READ (lineA_str(str_index+3:),*) measureA0%p 
  !T0
  str_index = INDEX (lineA_str, "T0=")
  IF (str_index<1) THEN
     WRITE (*,*) "Error: missing T0 in header of ", fileA_name
     STOP
  END IF
  READ (lineA_str(str_index+3:),*) measureA0%T 

  ! count lines of A
  nA = 0
  DO
     READ (fileIn, *,iostat=fileIn_status)
     IF (fileIn_status == 0) THEN
        nA = nA +1
     ELSE IF (fileIn_status == -1) THEN
        EXIT
     ELSE
        WRITE (*,*) "Error: while reading ", fileA_name
        STOP
     END IF
  END DO
  ! rewind and skip header
  REWIND (fileIn)
  READ (fileIn,*)

  ! allocate measures for file A
  ALLOCATE (measureA(nA), stat=error_i)
  IF (error_i /= 0) THEN
     WRITE  (*,*) "Error: allocation of measureA failed with error: ", error_i
     STOP
  END IF

  ! read data from file A
  DO iA = 1,nA
     READ (fileIn, *, iostat=fileIn_status) measureA(iA)%p, measureA(iA)%T
     IF (fileIn_status /= 0) THEN
        WRITE (*,*) "Error: fileA closed before all lines were read"
        STOP
     END IF
     ! compute the altitude from previous record
     IF (iA > 1) THEN
        measureA(iA)%z = ALTITUDE(measureA(iA), measureA(iA-1))
     ELSE
        measureA(iA)%z = ALTITUDE(measureA(iA), measureA0)
     END IF
  END DO

  ! wrte output for file A
  ! write the header
  WRITE (fileOut,*) "Dati di altezza, pressione, temperatura; (altezza ricavata tramite formula di Laplace)"
  WRITE (fileOut,*) TRIM(lineA_str)
  WRITE (fileOut,*) "Z (m),  P (mBar),  T (C)"
  ! write the data
  DO iA=1,nA
     WRITE (fileOut,'(F8.1," ",F6.1,"   ",F5.1)') &
          measureA(iA)%z, measureA(iA)%p, measureA(iA)%T
  END DO
  CLOSE (fileIn)
  CLOSE (fileOut)
  WRITE (*,*) "Wrote in ", fileA_out_name

  ! working on file B
  OPEN (FILE=fileB_name, UNIT=fileIn, iostat=fileIn_status, iomsg=io_msg,&
       STATUS='old', ACTION='read')
  IF (fileIn_status /= 0) THEN
     WRITE (*,*) TRIM(io_msg)
     STOP
  END IF
  ! create fileB_out, overwrite if exists
  CALL OPEN_OUT (fileB_out_name, fileOut, iostat=fileOut_status)
  IF (fileOut_status /= 0) THEN
     STOP
  END IF

  ! read header
  READ (fileIn,'(A)') lineB_str
  str_index = INDEX (lineB_str, "h0=")
  IF (str_index<1) THEN
     WRITE (*,*) "Error: missing h0 in header of ", fileB_name
     STOP
  END IF
  READ (lineB_str(str_index+3:),*) measureB0%z 
  str_index = INDEX (lineB_str, "p0=")
  IF (str_index<1) THEN
     WRITE (*,*) "Error: missing p0 in header of ", fileB_name
     STOP
  END IF
  READ (lineB_str(str_index+3:),*) measureB0%p 
  str_index = INDEX (lineB_str, "T0=")
  IF (str_index<1) THEN
     WRITE (*,*) "Error: missing T0 in header of ", fileB_name
     STOP
  END IF
  READ (lineB_str(str_index+3:),*) measureB0%T 

  ! count lines of B
  nB = 0
  DO
     READ (fileIn,*,iostat=fileIn_status)
     IF (fileIn_status == 0) THEN
        nB = nB +1
     ELSE IF (fileIn_status == -1) THEN
        EXIT
     ELSE
        WRITE (*,*) "Error: while reading ", fileB_name
        STOP
     END IF
  END DO
  ! rewind and skip header
  REWIND (fileIn)
  READ (fileIn,*)

  ! allocate measures for file B
  IF (ALLOCATED (measureB)) THEN
     DEALLOCATE (measureB)
  END IF
  ALLOCATE (measureB(nB), stat=error_i)
  IF (error_i /= 0) THEN
     WRITE  (*,*) "Error: allocation of measureB failed with error: ", error_i
     STOP
  END IF

  ! read data from file B
  DO iB = 1,nB
     READ (fileIn, *, iostat=fileIn_status) measureB(iB)%p, measureB(iB)%T
     IF (fileIn_status /= 0) THEN
        WRITE (*,*) "Error: fileB closed before all lines were read"
        STOP
     END IF
     ! compute the altitude from previous record
     IF (iB > 1) THEN
        measureB(iB)%z = ALTITUDE(measureB(iB), measureB(iB-1))
     ELSE
        measureB(iB)%z = ALTITUDE(measureB(iB), measureB0)
     END IF
  END DO

  ! wrte output for file B
  ! write the header
  WRITE (fileOut,*) "Dati di altezza, pressione, temperatura; (altezza ricavata tramite formula di Laplace)"
  WRITE (fileOut,*) TRIM(lineB_str)
  WRITE (fileOut,*) "Z (m),  P (mBar),  T (C)"
  ! write the data
  DO iB=1,nB
     WRITE (fileOut,'(F8.1," ",F6.1,"   ",F5.1)') &
          measureB(iB)%z, measureB(iB)%p, measureB(iB)%T
  END DO
  CLOSE (fileIn)
  CLOSE (fileOut)
  WRITE (*,*) "Wrote in ", fileB_out_name

  ! create fileAB_out, overwrite if exists
  CALL OPEN_OUT (fileAB_out_name, fileOut, iostat=fileOut_status)
  IF (fileOut_status /= 0) THEN
     STOP
  END IF

  ! merging A and B at every multiple of 200
  ! shared altitude interval between A and B
  zmin = CEILING(MAX(measureA(1)%z, measureB(1)%z))
  zmax = FLOOR(MIN(measureA(nA)%z, measureB(nB)%z))
  ! set the first altitude to compute in the interval
  ! as the first multiple of 200 greater than zmin
  IF (MOD(zmin,200) /= 0) THEN
     z200 = zmin + 200 - MOD(zmin,200)
  ELSE
     z200 = zmin
  END IF
  nAB = (zmax-z200)/200 + 1
  IF (nAB < 1) THEN
     WRITE (*,*) "Error: empty shared interval between A and B"
     STOP
  END IF

  ! allocating the merged array
  IF (ALLOCATED(measureAB)) THEN
     DEALLOCATE (measureAB)
  END IF
  ALLOCATE (measureAB(nAB), stat=error_i)
  if (error_i /= 0) THEN
     WRITE  (*,*) "Error: allocation of measureAB failed with error: ", error_i
     STOP
  END IF

  iA = 1
  iB = 1
  iAB = 1
  DO iAB = 1,nAB
     ! update iA index
     DO WHILE (iA <= nA)
        IF (measureA(iA)%z >= z200) EXIT
        iA = iA+1
     END DO
     IF (iA > nA) THEN
        WRITE (*,*) "Error: Out of scope in array measureA during merge"
        STOP
     END IF

     ! update iB index
     DO WHILE (iB <= nB)
        IF (measureB(iB)%z >= z200) EXIT
        iB = iB+1
     END DO
     IF (iB > nB) THEN
        WRITE (*,*) "Error: Out of scope in array measureB during merge"
        STOP
     END IF

     IF (ABS(measureA(iA)%z - z200) < 1) THEN
        p200A = measureA(iA)%p
     ELSE IF (iA > 1) THEN
        ! iA is above z200 and iA-1 under
        ! middle_pressure is an estimate at z200
        p200A = MIDDLE_PRESSURE(REAL(z200), measureA(iA-1), measureA(iA))
     ELSE
        WRITE (*,*) "Error: Out of scope in array measureA during merge"
        STOP
     END IF
     IF (ABS(measureB(iB)%z - z200) < 1) THEN
        p200B = measureB(iB)%p
     ELSE IF (iB > 1) THEN
        p200B = MIDDLE_PRESSURE(REAL(z200), measureB(iB-1), measureB(iB))
     ELSE
        WRITE (*,*) "Error: Out of scope in array measureB during merge"
        STOP
     END IF
     measureAB(iAB)%z = z200
     measureAB(iAB)%p = p200A - p200B
     z200 = z200 + 200
  END DO

  ! writing file AB
  ! write the header
  WRITE (fileOut,*) "Confronto tra dati di altezza e pressione provenienti da due misurazioni"
  WRITE (fileOut,'("Numero righe=",I5," zmin=",I5," zmax=",I5)') nAB, zmin, zmax 
  WRITE (fileOut,*) "A: " // TRIM(lineA_str)
  WRITE (fileOut,*) "B: " // TRIM(lineB_str)
  WRITE (fileOut,*) "Z (m),  P_A-P_B (mBar)"
  ! write the data
  DO iAB=1,nAB
     WRITE (fileOut,'(F8.1," ",F6.2)')  measureAB(iAB)%z, measureAB(iAB)%p
  END DO
  CLOSE (fileOut)
  WRITE (*,*) "Wrote in ", fileAB_out_name

  DEALLOCATE (measureA, measureB, measureAB)

END PROGRAM zpT
