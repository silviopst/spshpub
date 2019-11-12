C     QSQSQ$*$TURCO(1).Testas/S
C     6**************************************************************
C    TEST DI ASSOCIATIVITA' DEL PRODOTTO (1-A COL)*(1-A RIGA) SUL QL Q
C       SE SI : TEST=.TRUE.  Q E' UN GRUPPO
C       SE NO : TEST=.FALSE.
C
C     6**************************************************************
C
      SUBROUTINE TESTAS(Q,N,TEST)
      IMPLICIT INTEGER (A-Z)
      LOGICAL TEST
      PARAMETER (Y=16)
      DIMENSION Q(N,N), RM1(Y), CM1(Y)
C
C     >> INIZIO: COSTRUISCI RM1 E CM1
C                INVERSI DELL'ORLO DELLA 1-A RIGA E 1-A COLONNA
C
      TEST=.FALSE.
C
      DO 10 I=1,N
        IND=Q(I,1)
        RM1(IND)=I
        IND=Q(1,I)
 10     CM1(IND)=I
C
C    VERIFICA SE: (A*B)*C=A*(B*C), P.O. A,B,C=1,N
C
      DO 50 A=1,N
        T=CM1(A)
        DO 50 B=1,N
            S=RM1(B)
            S1=CM1(B)
C
C  P1==A*B
C
      P1=Q(S,T)
      P1=CM1(P1)
      DO 50 C=1,N
      U=RM1(C)
C
C  LP <- (A*B)*C=P1*C
C
      LP=Q(U,P1)
C
C   PL=B*C
C
      P2=Q(U,S1)
      P2=RM1(P2)
C
C  RP <- A*(B*C)=A*P2
C
      RP =Q(P2,T)
C
C  (A*B)*C=A*(B*C)  ??
C
      IF(RP.NE.LP) GOTO 70
C
 50   CONTINUE         !  LOOP
C
C  E' ASSOCIATIVO => Q E' UN GRUPPO
C
      TEST=.TRUE.
      RETURN
C
C   Q NON E' UN GRUPPO
C
 70   CONTINUE
      TEST=.FALSE.
      RETURN
      END
