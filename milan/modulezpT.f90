MODULE modulezpT
  IMPLICIT NONE

  INTEGER, PARAMETER :: R=287  ! J[K Kg]^-1
  REAL, PARAMETER :: g=9.81, CtoK=272.15  ! m/s^-2 [K]=[C]+CtoK
  
  TYPE :: MEASURE
     REAL :: z, p, T
  END TYPE MEASURE

CONTAINS

  REAL FUNCTION ALTITUDE (measure1, measure0)
    ! computes the altitude of measure1 from measure0
    ! with the barometric formula of Laplace
    TYPE(MEASURE), INTENT(IN) :: measure1, measure0
    REAL :: T

    T = measure0%T+measure1%T
    T = T/2
    T = T + CtoK  ! average temperature in Kelvin
    ALTITUDE = (R/g)*T*LOG(measure0%p / measure1%p) + measure0%z
  END FUNCTION ALTITUDE

  REAL FUNCTION INTERPOLATE (x, x1, x2, y1, y2)
    ! interpolates y(x) from (x1,y1) and (x2,y2)
    REAL, INTENT(in) :: x, x1, x2, y1, y2
    REAL :: m

    m = (y2 - y1) / (x2 - x1)
    INTERPOLATE = y1 + m * (x - x1)
  END FUNCTION INTERPOLATE
  
  REAL FUNCTION MIDDLE_PRESSURE (z, measure1, measure2)
    ! computes the pressure at z from two measures above and below
    ! with the barometric formula of Laplace
    REAL, INTENT(in) :: z 
    TYPE(MEASURE), INTENT(in) :: measure1, measure2
    REAL :: T

    T = INTERPOLATE (z, measure1%z, measure2%z, measure1%T, measure2%T)
    T = T + measure1%T
    T = T + CtoK  ! average temperature in Kelvin

    MIDDLE_PRESSURE = EXP(LOG(measure1%p) + (measure1%z - z)*g/(R*T))
  END FUNCTION MIDDLE_PRESSURE

END MODULE modulezpT
