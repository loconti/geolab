! Take as input a file, with header:
! ncols         886
! nrows         691
! xllcorner     111.975
! yllcorner     -44.525
! cellsize      0.05
! NODATA_value  -9999
! The data is displayed as a matrix of latitude longitude, up to latitude: yllcorner
PROGRAM australia_temperature
  USE modulematrix
  IMPLICIT NONE

  ! This program takes as input a matrix of temperatures measured across Australia
  ! Writes the output in three files: Lat, Filter, Mean
  ! Lat: mean values across each latitude
  ! Filter: The matrix as averaged by a filter: (3x3)
  ! Mean: The matrix averaged over each sub-block: (avg_dim x avg_dim)

  ! variables
  ! T: matrix of temperatures, Tf: T filtered, Ta: T averaged 
  REAL(kind=8), DIMENSION(:,:), ALLOCATABLE :: T, Tf, Ta
  ! Tl: vectors of mean temperature at each latitude
  REAL(kind=8), DIMENSION(:), ALLOCATABLE :: Tl
  REAL(kind=8) :: NAN
  REAL :: lat, long_inf, lat_inf, step
  INTEGER :: lat_i, long_i, nrows, ncols, error_i, count_temp, total_nan
  ! mean matrix dimension
  INTEGER :: a_cols, a_rows, avg_dim=9
  ! format for writing the coordinates in the header
  CHARACTER(len=100) :: format_coordinates="('With coordinates in range: (N:W) ',F7.3,':',F7.3,' -> (S:E) ',F7.3,':',F7.3,' with step: ', F4.2)"
  ! file variables
  CHARACTER(len=100) :: fileIn_name='monthly_temperature_sample.txt'
  CHARACTER(len=100) :: fileLat_name='temperature_latitude.txt'
  CHARACTER(len=100) :: fileFilter_name='temperature_filtered.txt'
  CHARACTER(len=100) :: fileAvg_name='temperature_averaged.txt'
  CHARACTER(len=100) :: io_msg
  CHARACTER(len=20) :: read_line  ! read the header
  LOGICAL :: filecheck
  INTEGER :: fileIn_status, fileOut_status
  INTEGER, PARAMETER :: fileIn=40, fileOutL=42, fileOutF=44, fileOutA=46

  ! namelist with file names and parameters
  NAMELIST /paramlist  /fileIn_name,fileLat_name,fileFilter_name,fileAvg_name,avg_dim

  ! try to read the namelist
  OPEN(FILE="paramlist.nml", UNIT=fileIn, ACTION='read', STATUS='old',&
       iostat=fileIn_status, iomsg=io_msg)
  IF (fileIn_status /= 0) THEN
     WRITE (*,*) TRIM(io_msg)
     WRITE (*,*) "Warning: failed to read namelist: using default paramters"
  ELSE
     READ (fileIn, NML=paramlist)
  END IF
  WRITE(*,*) "Using parameters:"
  WRITE(*,*) "Input file: ", TRIM(fileIn_name)
  WRITE(*,*) "Latitude output file: ", TRIM(fileLat_name)
  WRITE(*,*) "Filtered output file: ", TRIM(fileFilter_name)
  WRITE(*,*) "Averaged output file: ", TRIM(fileAvg_name)
  WRITE(*,*) "Averaging sub-blocks dimension: ", avg_dim
  WRITE(*,*)
  CLOSE(fileIn)

  ! open fileIn
  OPEN (FILE=fileIn_name, UNIT=fileIn, ACTION='read', STATUS='old',&
       iostat=fileIn_status, iomsg=io_msg)
  IF (fileIn_status /= 0) THEN
     WRITE (*,*) TRIM(io_msg)
     STOP
  END IF

  ! open fileOut latitude, replace it if exists
  INQUIRE (FILE=fileLat_name, EXIST=filecheck)
  IF (filecheck) THEN
     OPEN (FILE=fileLat_name, UNIT=fileOutL, ACTION='write', STATUS='replace',&
          iostat=fileOut_status, iomsg=io_msg)
  ELSE 
     OPEN (FILE=fileLat_name, UNIT=fileOutL, ACTION='write', STATUS='new',&
          iostat=fileOut_status, iomsg=io_msg)
  END IF
  IF  (fileOut_status /= 0) THEN
     WRITE (*,*) TRIM(io_msg)
     STOP
  END IF

  ! open fileOut filtered, replace it if exists
  INQUIRE (FILE=fileFilter_name, EXIST=filecheck)
  IF (filecheck) THEN
     OPEN (FILE=fileFilter_name, UNIT=fileOutF, ACTION='write', STATUS='replace',&
          iostat=fileOut_status, iomsg=io_msg)
  ELSE 
     OPEN (FILE=fileFilter_name, UNIT=fileOutF, ACTION='write', STATUS='new',&
          iostat=fileOut_status, iomsg=io_msg)
  END IF
  IF  (fileOut_status /= 0) THEN
     WRITE (*,*) TRIM(io_msg)
     STOP
  END IF

  ! open fileOut averaged, replace it if exists
  INQUIRE (FILE=fileAvg_name, EXIST=filecheck)
  IF (filecheck) THEN
     OPEN (FILE=fileAvg_name, UNIT=fileOutA, ACTION='write', STATUS='replace',&
          iostat=fileOut_status, iomsg=io_msg)
  ELSE 
     OPEN (FILE=fileAvg_name, UNIT=fileOutA, ACTION='write', STATUS='new',&
          iostat=fileOut_status, iomsg=io_msg)
  END IF
  IF  (fileOut_status /= 0) THEN
     WRITE (*,*) TRIM(io_msg)
     STOP
  END IF

  ! read info. from the header
  READ(fileIn, '(14X,I4)') ncols
  READ(fileIn, '(14X,I4)') nrows
  READ(fileIn, '(14X,A)') read_line
  READ(read_line,*) long_inf
  READ(fileIn, '(14X,A)') read_line
  READ(read_line,*) lat_inf
  READ(fileIn, '(14X,A)') read_line
  READ(read_line,*) step
  READ(fileIn, '(14X,A)') read_line
  READ(read_line,*) NAN ! NODATA line

  ! allocate the temperature arrays
  ALLOCATE (T(nrows,ncols), Tl(nrows),  stat=error_i)
  IF (error_i /= 0) THEN
     WRITE (*,*) "Error allocating T and Tl with code: ", error_i
     STOP
  END IF

  ! read input file to compute the mean line by line
  total_nan = 0
  DO lat_i = 1,nrows   
     READ (fileIn, *,iostat=fileIn_status) (T(lat_i,long_i), long_i=1, ncols)
     IF (fileIn_status /= 0) THEN
        WRITE (*,*) "Error: file closed at line ", lat_i, ", before all ", nrows, " were read" 
        STOP
     END IF
     ! compute the mean of the line
     count_temp = COUNT(ABS(T(lat_i,:) - NAN) > 1)
     total_nan = total_nan + ncols - count_temp
     IF (count_temp /= 0) THEN
        Tl(lat_i) = SUM(T(lat_i,:), ABS(T(lat_i,:) - NAN) > 1) / count_temp
     ELSE  ! if all the line is not a number 
        Tl(lat_i) = NAN
     END IF
  END DO
  WRITE (*,*) "Total nan values: ", total_nan

  ! writing in the latitude mean temperatures file
  lat = lat_inf + (step*nrows)  ! moving latitude north
  ! write the header
  WRITE (fileOutL, *) "Mean temperature across Australia at each latitude"
  WRITE (fileOutL, format_coordinates) lat_inf+(step*nrows), long_inf, lat_inf, long_inf+(step*ncols), step
  WRITE (fileOutL,'("nrows ", I4)') nrows
  WRITE (fileOutL, '("NODATA_value ",F10.3)') NAN

  ! writing the latitude temperatures in the output file
  WRITE (fileOutL, *) "latitude    temperature"
  DO lat_i = 1,nrows
     lat = lat - step  ! moving south
     WRITE (fileOutL, '(F7.3,"    ",F10.3)') lat, Tl(lat_i) 
  END DO

  WRITE (*,*) "Wrote in ", TRIM(fileLat_name)
  DEALLOCATE (Tl)

  ! allocate the filtered temperature matrix 
  ALLOCATE (Tf(nrows,ncols), stat=error_i) 
  IF (error_i /= 0) THEN
     WRITE (*,*) "Error allocating Tf with code: ", error_i
     STOP
  END IF

  CALL FILTER_MATRIX (T, nrows, ncols, NAN, Tf)

  ! write in filtered file
  ! write the header
  WRITE (fileOutF,*) "Filtered temperatures across Australia"
  WRITE (fileOutF, format_coordinates) lat_inf+(step*nrows), long_inf, lat_inf, long_inf+(step*ncols), step
  WRITE (fileOutF,'("ncols ", I4)') ncols
  WRITE (fileOutF,'("nrows ", I4)') nrows
  WRITE (fileOutF, '("NODATA_value ",F10.3)') NAN

  ! write the filtered matrix
  DO lat_i = 1,nrows
     WRITE (fileOutF,'(*(F10.3))') (Tf(lat_i,long_i), long_i=1,ncols)
  END DO

  WRITE (*,*) "Wrote in ", TRIM(fileFilter_name)
  DEALLOCATE (Tf)

  ! dimensionis of mean matrix
  a_cols = ncols / avg_dim 
  a_rows = nrows / avg_dim

  ! allocate the temperature meadiated matrix
  ALLOCATE (Ta(a_rows,a_cols),  stat=error_i)
  IF (error_i /= 0) THEN
     WRITE (*,*) "Error allocating Tm with code: ", error_i
     STOP
  END IF

  CALL AVERAGE_MATRIX (T, nrows, ncols, avg_dim, a_rows, a_cols,  nan, Ta, error_i)
  IF (error_i > 0) THEN
     WRITE (*,*) "Stopped Execution"
     STOP
  END IF

  ! write in averaged file
  ! write the header
  WRITE (fileOutA,*) "Average temperatures across Australia"
  WRITE (fileOutA, format_coordinates) lat_inf+(step*nrows), long_inf, &
       lat_inf+(MOD(nrows,avg_dim)*step), long_inf+(step*a_cols*avg_dim), step
  WRITE (fileOutA,'("ncols ", I4)') a_cols
  WRITE (fileOutA,'("nrows ", I4)') a_rows
  WRITE (fileOutA, '("NODATA_value ",F10.3)') NAN

  ! write the mean matrix
  DO lat_i = 1,a_rows
     WRITE (fileOutA,'(*(F10.3))') (Ta(lat_i,long_i), long_i=1,a_cols)
  END DO

  WRITE (*,*) "Wrote in ", TRIM(fileAvg_name)
  DEALLOCATE (T, Ta)
  CLOSE (fileIn)
  CLOSE(fileOutL)
  CLOSE(fileOutF)
  CLOSE(fileOutA)

END PROGRAM australia_temperature
