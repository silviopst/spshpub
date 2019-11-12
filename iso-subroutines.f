C
C       SUBROUTINES LATIN-SQUARE SOFTWARE
C       Q$~$Q$*$TURCO(l)'PRPRM/ISQ
C
      SUBROUTINE PRPRM (P1,P2,P3,N)
      IMPLICIT INTEGER (A-Z)
      DIMENSION P1(N), P2(N), P3(N)
C
C ESEGUI IL PRODOTTO: P1*P2=P3
C
      DO 10 J= 1,N
      IND=P1(J)
 10   P3(J)=P2(IND)
      RETURN
      END
C
C
C       Q$Q$QS*$TURCO(l).PRTPRM/S
C
      SUBROUTINE PRTPRM (L,LL,NC,N,NP)
C
C       STAMPA LA PERMUTAZIONE -L- PER CICLI
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION L(N ), LL(N)
      PRINT 100, NP, NC
      WRITE(2,100) NP, NC
 100  FORMAT(1H0,15X,'PERMUTAZIONE',I6,I6/)
      P=1
      DO 10 I= 1,NC
         P1=P+LL(I)-1
         PRINT 110, I,(L(IND), IND=P, P1)
         WRITE(2,110) I,(L(IND), IND=P, P1)

 10      P=P+LL(I)
 110     FORMAT(1H0,10X, 'CICLO :', I6, 10X, 20I3)
         RETURN
         END

C
C
C       QSQSQS*STURCO(l).ASSPRM/S
C       file OCR_FORTRAN-PAG-10-16.pdf
C
      SUBROUTINE ASSPRM(LISTA,L,LL,NC,N)
      IMPLICIT INTEGER (A-Z)
      DIMENSION LISTA(N), L(N), LL(N)
      IF(LISTA(1).EQ.0) STOP 'ERROR ASSPRM'
      CONT =1
      J=1
      NC=1
      I=1
C
 10   TOP=I
      REG=TOP
      L(J)=REG
 20   REG=LISTA(REG)
      IF(TOP.EQ.REG) GOTO 30
      J=J+1
      CONT=CONT+1
      L(J)=REG
      GOTO 20
 30   LL(NC)=CONT
      IF( J.EQ.N) GOTO 90
      NC=NC+1
      CONT=1            !
C
C       I<-[ PRIMO ELEMENTO CHE NON COMPARE IN L]
C
      DO 50 H=1,N
      DO 40 K=1,J
         IF(H.EQ.L(K)) GOTO 50
 40      CONTINUE
         I =H
         J=J+1
         GO TO 10
 50      CONTINUE
         STOP 3
 90      CONTINUE
C
C       PERMUTAZIONE COSTRUITA
C
      RETURN
      END


      SUBROUTINE LISTVA(A,I,L,N)
C
C       COSTRUISCI LA LISTA -L- DEI VALORI I NEL LATIN SQUARE -A-
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION A(N,N), L(N)
      DO 20 J=1,N
      DO 10 K=1,N
      IF(A(K,J).EQ.I) GOTO 20
 10   CONTINUE
      STOP 1
 20   L(J)=K
      RETURN
      END
