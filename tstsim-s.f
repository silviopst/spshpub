C     QSQSQ$*$TURCO(1).TSTSIM/S
C     6**************************************************************
C      TEST SE Q E' UN QL SIMMETRICO RISPETTO ALLA DIAGONALE PRINCIPALE
C       SE SI : TEST=.TRUE.
C       SE NO : TEST=.FALSE.
C
C     6**************************************************************
C
      SUBROUTINE TSTSIM(Q,N,TEST)
      IMPLICIT INTEGER (A-Z)
      LOGICAL TEST
      DIMENSION Q(N,N)
C
C     >> INIZIO
C
      TEST=.FALSE.
C
      N1=N
      DO 20 R=1,N1
      R1=R+1
      DO 20 C=R1,N
      IF(Q(R,C).NE.Q(C,R)) RETURN  ! Q NON SIMMETRICO
20    CONTINUE
      TEST=.TRUE.     ! ESITO POSITIVO
C     Q E' SIMMETRICO
      RETURN
      END
