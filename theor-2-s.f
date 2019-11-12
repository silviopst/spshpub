C     QSQSQ$*$TURCO(1).THEOR2/S
C     6**************************************************************
C   PROCEDURA PER STABILIRE SE IL QL -L- VERIFICA L'IPOTESI DEL TEOR. 2
C   CIOE' ESISTE UNA RIGA DI L DI ORDINE N O (N-1)
C       SE SI : TEST=.TRUE.
C       SE NO : TEST=.FALSE.
C    output: ROW, ORD, TEST
C     6**************************************************************
C
      SUBROUTINE THEOR2(L,N,TEST, ROW,ORD)
      IMPLICIT INTEGER (A-Z)
      PARAMETER (M=16)
      LOGICAL TEST
      DIMENSION L(N,N), LISTA(M), LS(M),LL(M)
C
C     >> INIZIO
C
      IF(N.GT.M) STOP 10
      TEST=.FALSE.
      ORD=N
      DO 50 R=1,N
        DO 10 K=1,N
 10   LISTA(K)=L(K,R)
      CALL ASSPRM(LISTA,LS,LL,NC,N)
      IF(NC.EQ.1) GOTO 30
      IF(NC.NE.2) GOTO 50
      IF((LL(1).NE.1).AND.(LL(2).NE.1)) GOTO 50
      ORD=N-1
 30   TEST=.TRUE.
      ROW=R
      RETURN    ! ESITO POSITIVO
 50   CONTINUE
      RETURN    ! ESITO NEGATIVO
      END
