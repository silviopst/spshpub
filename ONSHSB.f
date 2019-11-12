C     ONSHSB/S
      SUBROUTINE  ONSHOT(L,LL,NCICLI,N,NN,N3, LS,V,RCF, RW, DEBUG)
C     PROCEDURA ONE-SHOT
      IMPLICIT INTEGER (A-Z)
C      INTEGER CALC, CALCD
      CHARACTER DEBUG
      DIMENSION  V(3,N3) ,L(N) ,LL(N) ,LS(N3),RCF(N,NN)
C      DEFINE TWIN(C,R)=C+(R-1)*N
C      DEFINE IC(Z)=INT((Z-1)/NN)+1
C      DEFINE IR(Z)=MOD((Z-1),NN)+1
       TWIN(C,R)=C+(R-1)*N
       IC(Z)=INT((Z-1)/NN)+1
       IR(Z)=MOD((Z-1),NN)+1  !  errata, OK
C
C     LISTA RAPPRESENTANTE PRODOTTI DI CICLI =L
C     LISTA DELLE LUNGHEZZE DEI CICLI=LL
C     INSIEME DELLE TRIPLE=V
C     NCICLI E' IL NUMERO DEI CICLI
C     STAMPA LA PERMUTAZIONE
C     CALCOLA LJ, LI, DJI
C
      DO 60 J=1,N
         LJ=CALC(J)
         DO 50 I=1,N
            ADDR=TWIN(I,J)+(RW-1)*NN
            V(1,ADDR)=CALC(I)
            V(2,ADDR)=LJ
            V(3,ADDR)=CALCD(J,I)
c         IF(DEBUG.EQ.'D') print *, ADDR,'V(',V(1,ADDR),',',V(2,ADDR),
c     $',',V(3,ADDR),')'
c         IF(DEBUG.EQ.'D') print *, 'ADDR=', ADDR, 'V(1,ADDR)=',V(1,ADDR)
c         IF(DEBUG.EQ.'D') print *, 'ADDR=', ADDR, 'V(2,ADDR)=',V(2,ADDR)
c         IF(DEBUG.EQ.'D') print *, 'ADDR=', ADDR, 'V(3,ADDR)=',V(3,ADDR)
 50         CONTINUE
 60         CONTINUE
C     COSTRUITE LE TERNE
         IF(DEBUG.EQ.'D') print *, 'COSTRUITE LE TERNE'
       IF(RW.LT.N) RETURN
C     ORDINAMENTO TERNE
       DO 70 K=1,N3
 70       LS(K)=K
C
          CALL HSORT3(V,N3,3,LS,N3,1)
         IF(DEBUG.EQ.'D') print *, 'ORDINATE LE TERNE'

         IF(DEBUG.EQ.'D') CALL stampav(V,N3)
C
C     CODIFICA TERNE
          CALL CODE             !RISULTATO IN A
          RETURN
C
C     FUNZIONI INTERNE
        CONTAINS  ! INTERNAL FUNCTIONS follow
C
          INTEGER FUNCTION CALC(P)
C     TROVA IL CICLO CHE CONTIENE P
          K=0
 10       IF(K.EQ.N) STOP 1     ! ERRORE NEI DATI
          K=K+1
          IF(P.EQ.L(K)) GOTO 20
          GOTO 10
 20       H=1
          CONTR=LL(1)
 30       IF(K.LE.CONTR) GOTO 40
          IF(H.EQ.NCICLI) STOP 2  ! ERRORE NEI DATI
          H=H+1
          CONTR=CONTR+LL(H)
          GOTO 30
 40       CALC=LL(H)
          RETURN
          END FUNCTION
C
          INTEGER FUNCTION CALCD(S,T)

          IF(S.EQ.T) GOTO 90
C     'S' E 'T' APPARTENGONO A CICLI DIVERSI?
          K=1
          II=0                          ! NON PRESENTE IN LISTING
 10       IF(T.EQ.L(K)) GOTO 20
          IF(K.EQ.N) STOP 3     ! ERRORE NEI DATI
          K=K+1
          GOTO 10
 20       H=1
          LUNG=LL(1)
 25       IF(K.LE.LUNG) GOTO 30
          H=H+1
          LUNG=LUNG+LL(H)
          GOTO 25
C     'T'  STA NEL CICLO H-ESIMO
C     C'E' ANCHE 'S'
 30       I1=LUNG-LL(H)
          II=I1+1       ! errata, ok
 35       IF(S.EQ.L(II)) GOTO 40
          II=II+1
          IF(II-LUNG) 35,35,80
 40       JJ=II
          CONT=0
 45       IF(JJ.EQ.LUNG) JJ=I1  ! errata, ok
          JJ=JJ+1
          CONT=CONT+1
          IF(T-L(JJ)) 45,70,45  ! errata, ok
 70       CALCD=CONT
          RETURN
 80       CALCD=N
          RETURN
 90       CALCD=0
          RETURN
          END FUNCTION
C
      SUBROUTINE CODE
C
          NCOD=0
          I1=IC(LS(1))
          I2=IR(LS(1))
          RCF(I1,I2)=NCOD
          DO 30 I=2,N3
             CALL LMATCH(LS(I),LS(I-1),*10,*20) ! $ REPLACED BY * RETURN TO 10 OR 20
 10          NCOD=NCOD+1
 20          I1=IC(LS(I))
             I2=IR(LS(I))
             RCF(I1,I2)=NCOD
      IF(DEBUG.EQ.'D') write(2,*),'>CODE RCF(', I1,',',I2,')',RCF(I1,I2)
 30          CONTINUE
             RETURN
             END
C
      SUBROUTINE LMATCH(B,C,*,*)
C
             DO 1110 K=1,3
               IF(V(K,B).NE.V(K,C)) RETURN 1  ! 1 previously 3
 1110          CONTINUE
             RETURN 2   ! 2 previously 4
             END
C
      subroutine stampav(V,on3)
      DIMENSION  V(3,on3)
      print 2100,  V
      write(2,2100) V

 2100 FORMAT (1H ,5X,'matrice delle terne ',
     $2H :,///,(6(2X,6('(',I5,',',I5,',',I5,'), ')//)////))
      RETURN
      END
            END
