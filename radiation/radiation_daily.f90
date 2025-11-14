PROGRAM radiation_daily
IMPLICIT NONE

CHARACTER (len=200) :: filename="radiation-data.dat", io_msg
CHARACTER (len=10) :: date, radiation_char
INTEGER, PARAMETER :: fileIn=42
INTEGER :: file_status, nlines, line_i, error_i, inter_n, inter_i, line_0
REAL(kind=8), DIMENSION(:), ALLOCATABLE :: radiation
REAL(kind=8) :: long_sum, short_sum, NAN=-999., inter_m, inter_q, inter

OPEN (UNIT=fileIn, FILE=filename, iostat=file_status, iomsg=io_msg, status='OLD', action='read')
IF (file_status /= 0) THEN
   WRITE (*,*) TRIM(io_msg)
   STOP
END IF

! count the number of data rows
nlines = -1  ! skip the first line as header
DO
   READ (fileIn,*,iostat=file_status)
   IF (file_status == 0) THEN
      nlines = nlines + 1
   ELSE
      EXIT
   END IF
END DO

! check if the number of lines as expected
IF (nlines /= 60*24) THEN
   WRITE (*,*) "Worning: number of lines in input file: ", nlines, "does not match the number of minutes in a day"
   WRITE (*,*) ""
END IF

! allocating moemory for radiation data
ALLOCATE (radiation(nlines), stat=error_i)
IF (error_i /= 0) THEN
   WRITE (*,*) "Error in allocation with code:", error_i
   STOP
END IF

! reading all radiations in file
REWIND(fileIn)
READ(fileIn,*, iostat=file_status) ! skip header
DO line_i = 1,nlines
   IF (file_status /= 0) THEN
      WRITE (*,*) "Error: file closed before all lines were read"
      STOP
   END IF
   READ (fileIn, '(A10,10X,A)', iostat=file_status) date, radiation_char
   READ (radiation_char,*) radiation(line_i)
END DO
CLOSE (fileIn)

! check for invalid data in first or last positions
IF (radiation(1) < NAN+1) THEN
   WRITE (*,*) "Error: invalid data at first row: cannot interpolate"
   STOP
END IF
IF (radiation(nlines) < NAN+1) THEN
   WRITE (*,*) "Error: invalid data at last row: cannot interpolate"
   STOP
END IF

! complete the data with interpolation
inter_n = 0  ! number of points to interpolate
line_0 = 1  ! last valid point position
DO line_i=2,nlines
   inter_n = inter_n
   IF (radiation(line_i) < NAN+1) THEN  ! this data point is missing
      ! interpolate no more than 4 consecutive values
      IF (inter_n >= 4) THEN
         WRITE (*,*) "Error: too many consecutive missing values"
         STOP
      ELSE
         inter_n = inter_n + 1
      END IF
   ELSE IF (inter_n > 0) THEN  ! first valid point after a series of missing ones
      ! parameters of the linear interpol
      inter_q = radiation(line_0)
      inter_m = (radiation(line_i)-inter_q)/(inter_n+1)
      WRITE (*,*) "Interpolating from line:", line_0+1
      WRITE (*,'(I4,"   ",F8.4)') line_0+1, radiation(line_0)
      ! interpolating the previous inter_n points
      DO inter_i = 1,inter_n
         inter = inter_m * inter_i + inter_q          
         WRITE (*,'(I4," i.",F8.4)') line_0+inter_i+1, inter 
         radiation(line_0+inter_i) = inter
      END DO
      WRITE (*,'(I4,"   ",F8.4)') line_i+1, radiation(line_i)
      WRITE (*,*) ""

      inter_n = 0
      line_0 = line_i
   ELSE
      line_0 = line_i ! last valid point position update
   END IF
END DO

! Compute the sum of the radiation
long_sum = 0
short_sum = 0
DO line_i=1,nlines
   IF (radiation(line_i) > 0) THEN
      short_sum = short_sum + radiation(line_i)
   ELSE
      long_sum = long_sum + radiation(line_i)
   END IF
END DO

! Print the total radiation
WRITE (*,'("Data of incident radiation in date: ", A)') date
WRITE (*,'("The total short wave radiation is:  ", F12.4, A)') short_sum, " W/m2"
WRITE (*,'("The total long wave radiation is:   ", F12.4, A)') long_sum, " W/m2"

DEALLOCATE (radiation)

END PROGRAM radiation_daily
