C     QSQSQ$*$TURCO(1).THEOR1/S
C     6**************************************************************
C   PROCEDURA PER STABILIRE SE I DUE QL -L- E -M- SONO GRUPPI ABELIANI
C   RISPETTO ALLA MOLTIPLICAZIONE DELLA 1-A COL. PER LA 1-A RIGA.
C       SE SI : TEST=.TRUE.
C       SE NO : TEST=.FALSE.
C
C     6**************************************************************
C
      SUBROUTINE THEOR1(L,M,N,TEST)
      IMPLICIT INTEGER (A-Z)
      LOGICAL TEST, TEST1
      DIMENSION L(N,N), M(N,N)
C
C     >> INIZIO
C
      TEST=.FALSE.
C
C     L ED M SONO SIMMETRICI?
C
      CALL TSTSIM(L,N,TEST1)
      IF(.NOT.TEST1) RETURN
      CALL TSTSIM(M,N,TEST1)
      IF(.NOT.TEST1) RETURN
C
C   L ED M SONO GRUPPI?
C
      CALL TESTAS(L,N,TEST1)
      IF(.NOT.TEST1) RETURN
      CALL TESTAS(M,N,TEST1)
      IF(TEST1) TEST=.TRUE.     ! ESITO POSITIVO
      RETURN
      END
