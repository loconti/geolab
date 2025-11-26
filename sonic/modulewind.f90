MODULE modulewind
  IMPLICIT NONE

  INTEGER, PARAMETER :: max_sonic_par=9
  REAL, PARAMETER :: NAN=99.99 
  INTEGER :: nan_total=0
  REAL, DIMENSION(3,3) :: rotation_matrix
  
  TYPE :: WIND
     REAL, DIMENSION(3) :: velocity
     REAL :: c
     INTEGER, DIMENSION(max_sonic_par) :: sonic_par  ! accessorial parameters up to 9
  END TYPE WIND
  
CONTAINS
  TYPE(WIND) FUNCTION WIND_ROTATE(wind_measure)
    ! rotate the velocity of wind measure according to the rotation matrix
    TYPE(WIND), INTENT(in) :: wind_measure

    wind_rotate%c = wind_measure%c
    wind_rotate%sonic_par = wind_measure%sonic_par
    ! check for NAN values in any velocity component
    IF (count(ABS(wind_measure%velocity-NAN) < 1) > 1) THEN
       nan_total = nan_total +1
       wind_rotate%velocity = [NAN,NAN,NAN]
       RETURN
    END IF
    ! rotate the velocity
   wind_rotate%velocity = MATMUL (rotation_matrix, wind_measure%velocity) 
  END FUNCTION WIND_ROTATE

  FUNCTION MATRIX_ROTATE(phi, theta, psi) RESULT(matrix_rotated)
    REAL, INTENT(in) :: phi, theta, psi
    REAL, DIMENSION(3,3), INTENT(out) :: matrix_rotated
    
    matrix_rotated(1,:) = [COS(psi)*COS(phi)-COS(theta)*SIN(phi)*SIN(psi),&
         (-1)*SIN(psi)*COS(phi)-COS(theta)*SIN(phi)*COS(psi), SIN(theta)*SIN(phi)]
    matrix_rotated(2,:) = [COS(psi)*sin(phi)-COS(theta)*cos(phi)*SIN(psi),&
         (-1)*SIN(psi)*SIN(phi)-COS(theta)*COS(phi)*COS(psi), (-1)*SIN(theta)*COS(phi)]
    matrix_rotated(3,:) = [SIN(theta)*SIN(psi), SIN(theta)*COS(psi), COS(theta)]
  END FUNCTION MATRIX_ROTATE
  
  
END MODULE modulewind
