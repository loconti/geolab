PROGRAM medie_tetto
!Programma che apre il file dati_tetto.txt, legge i dati
!calcola 3 medie giornaliere e scrive i risultati

!Dichiarazioni delle variabili  
  IMPLICIT NONE
  INTEGER, PARAMETER :: igiorni=3, N=24
  INTEGER :: ios, i, j, k, kk
  INTEGER :: giorno, mese, anno, ora
  REAL, DIMENSION(igiorni*N):: T
  REAL, DIMENSION (igiorni):: medie_T,sigma_T,sigma2_T
  REAL :: sum_T,sum_sigma,sum_T2

!Apertura del file di ingresso  
  OPEN(44,file='dati_tetto.txt', STATUS='OLD',IOSTAT=ios)
  IF(ios/=0)THEN
     WRITE(*,*) 'ATTENZIONE: errore di apertura'
     STOP
  END IF

!Lettura delle igiorni*N temperature - in questo caso saranno 72
  DO k=1,igiorni*N
     READ(44,*)giorno, mese, anno, ora,T(k)
  END DO
  
!Calcolo delle 3 medie giornaliere
  DO i=1,igiorni   !loop sui tre giorni
     sum_T=0  !azzero la somma delle temperature
     sum_T2=0 !azzero la somma dei quadrati di T
     DO j=1,N !loop sul singolo giorno
        kk=(i-1)*N+j !kk va da 1a 24, da 25 a 48, e da 49 a 72
        sum_T=sum_T+T(kk)
        sum_T2=sum_T2+T(kk)**2
     END DO
     medie_T(i)=sum_T/N
     sigma2_T(i)=SQRT((N*sum_T2-sum_T**2)/(N*(N-1)))
     WRITE(*,100)'media giorno ',i,' = ',medie_T(i),' +- ',sigma2_T(i)
  END DO
  
  !Calcolo delle deviazioni standard
  DO i=1,igiorni   !loop sui tre giorni
     sum_sigma=0  !azzero la somma
     DO j=1,N !loop sul singolo giorno
        kk=(i-1)*N+j !kk va da 1 a 24, da 25 a 48, e da 49 a 72
        sum_sigma=sum_sigma+(T(kk)-medie_T(i))**2
        !WRITE(*,*)kk,T(kk),medie_T(i),(T(kk)-medie_T(i))**2
     END DO
     sigma_T(i)=SQRT(sum_sigma/(N-1))
     WRITE(*,100)'media giorno ',i,' = ',medie_T(i),' +- ',sigma_T(i)
  END DO
  
!Formato di scrittura  
100 FORMAT(A14,I3,A3,F6.2,A4,F6.2)

!Fine  
  STOP
  END
