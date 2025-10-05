PROGRAM projectile
IMPLICIT NONE
! Posizione velocita' e massima altezza di un proiettile dopo un tempo t
! lanciato con velocita' u da terra con angolo alpha
REAL :: alpha, t, u, g=9.81  ! gradi, secondi, m/s, m/s^2
REAL :: deg2rad=3.14159265358/180.0  ! rad = gradi * deg2rad
REAL :: x, y, vx, vy, v, theta, ymax
READ (*,*) alpha, t, u  ! 35 3.81 41.7
alpha = alpha * deg2rad  ! angolo di partenza in radianti
x = u * t * COS(alpha)
y = u * t * SIN(alpha) - 0.5 * g * t ** 2
vx = u * COS(alpha)
vy = u * SIN(alpha) - g * t
v = SQRT(vx ** 2 + vy ** 2)
theta = ATAN2(vy, vx) / deg2rad  ! angolo tra velocita' e orizzontale
ymax = (u * SIN(alpha)) ** 2 / (2 * g)  ! massima quota
WRITE (*,*) 'x =', x, 'y =', y, 'v =', v, 'theta =', theta, 'ymax =', ymax
END PROGRAM projectile