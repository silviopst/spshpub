C     ********  FILE: OCR_FORTRAN-PAG-10-16.pdf $TURCO.SORT3/SPS
      SUBROUTINE HSORT3 (V,N,N3,L,HN,FL)
C     INPUT: V INSIEME DELLE LISTE DA ORDINARE, N E' LA DIMENSIONE
C     FL E' UN FLAG, FL=1, L E' COMPLESSA, FL=0 L E' SEMPLICE
C     ********
C
      IMPLICIT INTEGER (A-Z)
C      LOGICAL VR,VER
      LOGICAL VER
      DIMENSION L(HN), V(N3,N)
C
C     INIZIALIZZO
      S=INT(HN/2)+1
      R=HN
C
C     DECREMENTA S O R
C
 5    CONTINUE
      IF(S.GT.1) GOTO 10
      T=L(R)
      L(R)=L(1)
      R=R-1
      IF(R.NE.1) GOTO 20
      L(1)=T
      RETURN
 10   S=S-1
      T=L(S)
C
C       PREPARA IL SIFT-UP
C
 20   J=S
C
C       PROCEDI IN BASSO
C
 25   CONTINUE
      I=J
      J=J*2
      IF (J-R) 30,40,50
C
C       CERCO IL FIGLIO MAGGIORE
C
 30   IF(.NOT.VR(L(J),L(J+1))) J=J+1
C
C       PIU' GRANDE DI T
C
 40   VER=VR(T,L(J))
      IF(VER) GOTO 50
      L(I)=L(J)
      GOTO 25
 50   L(I)=T
      GOTO 5
      CONTAINS  ! INTERNAL FUNCTIONS follow
C
      LOGICAL FUNCTION VR(C,B)
      IMPLICIT INTEGER (A-Z)
C       COMPIE IL CONFRONTO C.GE.B
C
C       L E' UNA LISTA COMPLESSA ?
      VR=.TRUE.
      IF(FL.EQ.1) GOTO 10
C       L E' SEMPLICE
      IF(C.LT.B) VR=.FALSE.
      RETURN
 10   CONTINUE
      DO 20 II=1,N3
         IF(V(II,C)-V(II,B)) 30,20,40
 20      CONTINUE
         RETURN
 30      VR=.FALSE.
 40      CONTINUE
      RETURN
      END FUNCTION
      END    ! END SUBROUTINE
