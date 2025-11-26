MODULE modulefile
  IMPLICIT NONE

CONTAINS
  SUBROUTINE OPEN_OUT(file, unit, iostat)
    ! open file, replace it if exists
    
    CHARACTER(len=*), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER(len=200) :: io_msg
    LOGICAL :: filecheck
    ! check if file exists
    INQUIRE (FILE=file, EXIST=filecheck)
    IF (filecheck) THEN
      !  open and replace
       OPEN (FILE=file, UNIT=unit, ACTION='write', STATUS='replace',&
            iostat=iostat, iomsg=io_msg)
    ELSE 
      ! create new file
       OPEN (FILE=file, UNIT=unit, ACTION='write', STATUS='new',&
            iostat=iostat, iomsg=io_msg)
    END IF
    IF  (iostat /= 0) THEN
       WRITE (*,*) TRIM(io_msg)
    END IF
  END SUBROUTINE OPEN_OUT

  SUBROUTINE COUNT_LINES(unit, nlines, iostat, error_i)
    ! counts lines in fileIn from current position to the first blank line or EOF
    ! then repositions to the previous position
    INTEGER, INTENT(in) :: unit
    INTEGER, INTENT(out) :: nlines, iostat, error_i
    INTEGER :: position
    LOGICAL :: filecheck
    CHARACTER(len=15) :: file_access
    CHARACTER(len=3) :: line_head 

    nlines = 0
    error_i = 0
    ! require file opened and with stream access
    INQUIRE (unit, opened=filecheck, pos=position, access=file_access)
    IF (.NOT. filecheck) THEN
       WRITE (*,*) "Error in COUNT_LINES: file is not open"
       error_i = 1
       RETURN
    END IF
    IF (file_access /= 'STREAM') THEN
       WRITE (*,*) "Error in COUNT_LINES: file is not set with ACCESS='STREAM'"
       error_i = 2 
       RETURN
    END IF
    
    ! count lines
    DO
       READ (unit,*,iostat=iostat) line_head
       IF (iostat == 0 .AND. LEN(line_head) > 0) THEN  ! until blank line or EOF
          nlines = nlines + 1
       ELSE
          EXIT
       END IF
    END DO
    ! reposition to previous position
    READ (unit, '()', iostat=iostat, pos=position, advance='NO')
  END SUBROUTINE COUNT_LINES
  
END MODULE modulefile
