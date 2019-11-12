C    Q$Q$*$TURCO(l).ISORDN/S
C     ****************************************************************
C     PROCEDURA PER VERIFICARE L'ESISTENZA O MENO DI UNA PERMUTAZIONE -ALFA-
C     TALE CHE: L->M , <ALFA, ALFA, ALFA>,
C     L ED M SONO 2 QUADRATI LATINI. SI SUPPONE CHE LA RIGA -I- DI L E'
C     RAPPRESENTATA DA UNA PERMUTAZIONE DI ORDINE -N-, O , (N-1).
C     SI SFRUTTA IL TEOREMA NO. 2
C
      SUBROUTINE ISORDN(L,M,N,I,ORD,TEST,ALFA,*)
C
C     AL RIENTRO SI AVRA': TEST=.TRUE. <=> L.ISO.M /
C                          TEST=.FALSE. <=> L.NISO.M
C     * = ERROR-EXIT   ($ IN UNIVAC SOSTITUITO IN *)
C     ****************************************************************
C
      IMPLICIT INTEGER(A-Z)
      PARAMETER (N1=6)
      DIMENSION L(N,N), M(N,N),ALFA(N),ALFM(N1) ,LIST(N1)
     $ , LIST1(N1)
      DIMENSION LS(N1) ,LS1(N1),LL(N1), LL1(N1)
      LOGICAL TEST,LOGIC
C
C     1.   *INIZIO*
C
      IF(ORD.NE.N) GOTO 1
      LOGIC=.TRUE.
      GOTO 5
 1    IF(ORD.NE.(N-1)) RETURN 8
      LOGIC=.FALSE.
 5    CONTINUE
      DO 10 K=1,N
 10      LIST(K)=L(K,I)
         CALL ASSPRM(LIST, LS1,LL1,NC1,N)
C
C     * 2. ESAMINA LE RIGHE M(*,J), J =1,N
C
      DO 100 J=1,N         ! <<<<<
C
C     * 3. M(*,J) E' DI ORDINE -ORD-?
C
      DO 15 K=1,N
 15      LIST(K)=M(K,J)
         CALL ASSPRM(LIST, LS,LL,NC,N)
C
         IF(NC.NE.1) GOTO 20
         IF(LOGIC) GOTO 25             ! CASO NC 1-1 ACCEPT
         GOTO 100                      ! CASO NC 2-1 REJECT
 20      IF(NC.NE.2) GOTO 100          ! CASO NC  -3 REJECT
         IF(LOGIC) GOTO 100            ! CASO NC 1-2 REJECT
         IF((LL(1).NE.1).AND.(LL(2).NE.1)) GOTO 100 ! CASO NC 2-2?
 25      CONTINUE                      ! ACCEPT
C
C     * 4.PREPARA LE 2 PRM -LS-, -LS1-
C
         IF(LOGIC) GOTO 30
C
C     ORDINA I PRODOTTI DEI CICLI DI LS,LSl : (l)*(N~l)
C
         IF(LL(1).EQ.1) GOTO 30
         CALL ORDCIC(LS,LL, N)
         IF(LL1(1).EQ.1) GOTO 30
         CALL ORDCIC(LS1,LL1, N)
 30      CONTINUE
C
C     * 5. COSTRUISCI UNA POSSIBILE: AlFA -ORD- POSSIBILITA'
C
         DO 90 K=1,ORD !<<<<<<
         DO 35 E=1,N
         IND=LS1(E)
 35      ALFA(IND)=LS(E)
C
C    *   6. TEST: E' l'ALFA CERCATA ?
C    *   (ALFA**-1)*L(*,K)*ALFA.EQ.M(*,AlFA(K)) ,K=l,N
C
         DO 40 E=1,N
         IND=ALFA(E)
 40      ALFM(IND)=E
C
         DO 50 H=1,N  !  <<<<<<<<< RIGHE DI L
         DO 45 EE=1,N
 45      LIST(EE)=L(EE,H)
         CALL PRPRM(ALFM,LIST,LIST1,N)
         CALL PRPRM(LIST1,ALFA,LIST,N)
         IND=ALFA(H)
         DO 50 E=1,N
         IF(LIST(E).NE.M(E,IND)) GO TO 60
 50      CONTINUE
C
C     SE ESCE DI QUI  E' FINITI: ALFA E' LA PRM CERCATA: L->M
C
         TEST=.TRUE.
         RETURN
 60      CONTINUE
C
C    *   7. SHIFT A DESTRA DI UN POSTO IL CICLO DI LS
C
         IF(LOGIC) GOTO 65
         IND=3
         GOTO 70
 65      IND=2
 70      T=LS(N)
         DO 80 T1=N,IND,-1
 80      LS(T1)=LS((T1-1))
         LS(IND-1)=T
 90      CONTINUE
C
 100     CONTINUE                     !<<<<<<<<<<<<<<
C
C     SE ESCE DI QUI E' FINITO, MA L ED M NON SONO ISOMORFI.
C
         TEST=.FALSE.
         RETURN
         END
C
C     ****************************************************************
C
        SUBROUTINE ORDCIC(X,Y,N2)
C   N2==N ORDINE QL
        IMPLICIT INTEGER (A-Z)
        DIMENSION X(N2), Y(N2)
        IN=Y(1)
        Y(1)=Y(2)
        Y(2)=IN
        IN=X(N2)
        DO 10 K=N2,2,-1
 10     X(K)=X(K-1)
        X(1)=IN
        RETURN
C
        END
