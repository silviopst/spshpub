C     ********  FILE: OCR_FORTRAN-PAG-10-16.pdf $TURCO.SORT3/SPS
C       COSTRUISCI IL CAYLEYANO SINISTRO J DEL GRUPPO QL
C
      SUBROUTINE CAYLEY (QL,N,I,J,L,LL,NC)
C
C     ********
C
      IMPLICIT INTEGER (A-Z)
C
C       * 1, COSTRUISCI: LIST=(QL(*,I)**-1)*QL(*,J)
C
      PARAMETER (M=8)
      DIMENSION LIST (M), L(N), LL(N), QL(N,N)
C
C     QL(K,I) -> QL(K,J)
C
      DO 10 K=1,N
         IND=QL(K,I)
 10      LIST(IND)=QL(K,J)
C
C       PONI -LIST- SOTTO FORMA DI PRODOTTO DI CICLI
C
         CALL ASSPRM(LIST,L,LL,NC,N)
         RETURN
      END
