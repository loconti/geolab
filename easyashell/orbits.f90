PROGRAM orbits
        REAL :: epsilon=0, r
        REAL, PARAMETER :: delta=0.25
        INTEGER, PARAMETER :: size=1200  ! Km
        INTEGER :: theta
        REAL, PARAMETER :: deg2rad=3.14159265358/180.0  ! rad = gradi * deg2rad
        DO WHILE (epsilon <= 0.5)
                WRITE (*,*) 'epsilon: ', epsilon
                DO theta = 1,360,15
                        r = size / (1 - epsilon * COS(theta*deg2rad))
                        WRITE (*,*) '   theta: ', theta, '   r: ', r
                END DO
                WRITE (*,*) '.............................' 
                epsilon = epsilon + delta
        END DO
END PROGRAM orbits
