MODULE modulematrix
  IMPLICIT NONE
CONTAINS
  SUBROUTINE FILTER_MATRIX (matrix, nrows, ncols, nan, filtered)

    ! Filters a matrix by averaging each coefficent withs its neighbours
    INTEGER, INTENT(IN) :: nrows, ncols
    REAL(kind=8), INTENT(IN) :: nan ! not a number
    REAL(kind=8), DIMENSION(nrows,ncols), INTENT(IN) :: matrix
    REAL(kind=8), DIMENSION(nrows,ncols), INTENT(OUT) :: filtered
    INTEGER :: i, j, fi, fj, wi, wj
    REAL(kind=8) :: filter_sum, weights_sum
    REAL(kind=8), DIMENSION(3,3) :: weights
    weights = RESHAPE((/0.3,0.5,0.3,0.5,1.,0.5,0.3,0.5,0.3/), SHAPE(weights))

    ! loop over matrix indices
    DO i=1,nrows
       DO j=1,ncols

          IF (ABS(matrix(i,j) - nan) < 1) THEN
             filtered(i,j) = nan
             CYCLE
          END IF

          ! loop over filters weights
          weights_sum = 0
          filter_sum = 0
          DO wi=-1,1
             DO wj=-1,1

                fi = i+wi
                fj = j+wj
                IF (fi > 0 .AND. fi <= nrows .AND. fj > 0 .AND. fj <= ncols) THEN
                   IF (ABS(matrix(fi,fj) - nan) > 1) THEN  ! skip nan values
                      filter_sum = filter_sum + matrix(fi,fj)*weights(wi+2,wj+2)
                      weights_sum = weights_sum + weights(wi+2,wj+2)
                   END IF
                END IF

             END DO
          END DO

          filtered(i,j) = filter_sum/weights_sum

       END DO
    END DO
  END SUBROUTINE FILTER_MATRIX

  SUBROUTINE AVERAGE_MATRIX (matrix, nrows, ncols, avg_dim, a_rows, a_cols, nan, averaged, error)

    ! Average matrix over sub-blocks (avg_dim x avg_dim)
    INTEGER, INTENT(IN) :: nrows, ncols  ! dimensions of matrix
    INTEGER, INTENT(IN) :: avg_dim, a_rows, a_cols  ! dimensions of averaged matrix
    REAL(kind=8), INTENT(IN) :: nan  ! not a number
    REAL(kind=8), DIMENSION(nrows,ncols), INTENT(IN) :: matrix
    INTEGER, INTENT(OUT) :: error
    REAL(kind=8), DIMENSION(a_rows, a_cols), INTENT(OUT) :: averaged
    REAL(kind=8) :: avg_sum
    INTEGER :: avg_n, i, j, i_a, j_a, i_min, i_max, j_min, j_max

    error = 0
    ! verify avg_dim
    IF (avg_dim <= 1 .OR. avg_dim > ncols .OR. avg_dim > nrows) THEN
       WRITE (*,*) "Error: not valid sub-blocks dimension to AVERAGE_MATRIX SUBROUTINE"
       error = 1
       RETURN
       ! verify coherence of avg_dim with a_rows, a_cols
    ELSE IF (nrows / avg_dim /= a_rows .OR. ncols / avg_dim /= a_cols) THEN
       WRITE (*,*) "Error: incoherent dimensions of averageed matrix to AVERAGE_MATRIX SUBROUTINE"
       error = 1
       RETURN
    END IF

    ! averaging matrix
    avg_sum = 0
    avg_n = 0
    ! loop over averaged matrix
    DO i_a = 1,a_rows
       DO j_a = 1,a_cols
          !  calculate the boundaries of the (i_a,j_a) submatrix
          i_min = avg_dim*(i_a-1) + 1
          i_max = i_a*avg_dim
          j_min = avg_dim*(j_a-1) + 1
          j_max = j_a * avg_dim
          IF (i_max > nrows .OR. j_max > ncols .OR. i_min < 1 .OR. j_min < 1) THEN
             WRITE (*,*) "Error: Exited the matrix index range"
             WRITE (*,*) "     Indexes: (", i_min, i_max, j_min, j_max, ")"
             error = 2
             RETURN
          END IF
          ! loop over the sub-block
          DO i = i_min, i_max
             DO j = j_min, j_max
                IF (ABS(matrix(i,j) - nan) > 1) THEN  ! count a coefficent only if is not nan
                   avg_sum = avg_sum + matrix(i,j)
                   avg_n = avg_n + 1
                END IF
             END DO
          END DO
          IF (avg_n /= 0) THEN
             averaged(i_a, j_a) = avg_sum / avg_n
          ELSE  ! all values in submatrix are NAN
             averaged(i_a, j_a) = nan
          END IF
          avg_sum = 0
          avg_n = 0
       END DO
    END DO

  END SUBROUTINE AVERAGE_MATRIX

END MODULE modulematrix
