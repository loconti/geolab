PROGRAM grades
    IMPLICIT NONE
    INTEGER :: score
    READ (*,*) score
    SELECT CASE (score)
        CASE (90:100)
            WRITE (*,*) 'A'
        CASE (80:89)
            WRITE (*,*) 'B'
        CASE (70:79)
            WRITE (*,*) 'C'
        CASE (60:69)
            WRITE (*,*) 'D'
        CASE DEFAULT
            WRITE (*,*)  'Invalid score'
    END SELECT
END PROGRAM grades
