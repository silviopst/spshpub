C     QSQSQ$*$TURCO(1).COR3/S
C     6**************************************************************
C
C     ALGORITMO  DEL COROLLARIO 3
C     INPUT: L ED M  QUADRATI LATINI Dl ORDINE N
C     COROLLARIO 3:
C     SE ALMENO UNA COPPIA DI Q.L, DELLE 6 OTTENIBILI DA
C     L ED M ,(COME IN OSS.1) VERIFICA LE IPOTESI 0 DEL TEOR-1 O DEL TEOR 2
C     ALLORA ESISTE UN ALGORITMO POLINOMIALE [ QUESTO !] PER DECIDERE
C     L'ESISTENZA DI UNA PERMUTAZIONE -ALFA- T.C. L-->M,<ALFA,ALFA,ALFA>
C     QUALORA L'HP DEL TEOREMA 2 SIA VERIFICATA SI CONOSCE ANCHE ALFA
C     6**************************************************************
C
      IMPLICIT INTEGER (A-Z)
C     EXTERNAL REPAG, CONPAG, TESTAS, TSTSIM, ISABEL
      CHARACTER*50 ALF
      LOGICAL TEST
      CHARACTER DEBUG
C      PARAMETER (N=6)
      PARAMETER (N=8)
      PARAMETER (NN=N*N)
      DIMENSION L(N,N), M(N,N), TL(NN,3),TM(NN,3), PERM(3,5),ALFA(N)
      DIMENSION LS(N), LL(N)
      DATA PERM / 1,3,2, 3,2,1, 2,1,3, 2,3,1, 3,1,2 /
C
C     INIZIO
C
      PRINT *, 'ENTER D FOR DEBUG, N '
      READ *, DEBUG
      OPEN(unit = 2, file = 'c:\\G77\\OUT-COR3-pag55.TXT')
      IF(DEBUG.EQ.'D') PRINT *, 'OPENING ', FILE, ' N= ', N , ' NN= ',NN
C      ALF='c:\\G77\\QR-06-pag51.dat'
      ALF='c:\\G77\\QR-08-pag55.dat'
      PRINT *, 'ENTER Input File Name ', ALF
C      READ *, ALF
      IF(DEBUG.EQ.'D') PRINT *, 'OPENING ', ALF
C      PRINT *, 'ENTER L'
C      READ(5,200) L
C      PRINT *, 'ENTER M'
C      READ(5,200) M
      PRINT *, 'opening... ', ALF
      OPEN(UNIT=1,FILE=ALF)
      READ(1,200) ((L(I,J),I=1,N),J=1,N)
C
      WRITE(2,250)
C      ALF='L'
      WRITE(2, 510) 'L',L
C
      READ(1,200) ((M(I,J),I=1,N),J=1,N)
      CLOSE(1)

C      ALF='M'
      WRITE(2,510) 'M',M
 510  FORMAT(1H1, 10X, 'QUADRATO  ', A5,2H :,///,(5X, 8I5//))
C
C      0. FORMA LE TERNE TL, TM
C
       K=0
       DO 10 I=1,N
       DO 10 J=1,N
          K=K+1
          TL(K,1)=I
          TM(K,1)=I
          TL(K,2)=J
          TM(K,2)=J
          TL(K,3)=L(J,I)
 10       TM(K,3)=M(J,I)
       PRINT 250
       WRITE(2, 250)
       I=1
       PRINT 260, I,L
       WRITE(2, 260) I,L
       I=2
       PRINT 260, I,M
       WRITE(2, 260) I,M
       CALL CONPAG ! dummy call
C
       I=0
C
 1     CONTINUE ! < ITERAZIONE SUI 6 QL >
C     1. L E/O M VERIFICANO LE HP DEL TEOR-l O DEL TEOR-2
       CALL THEOR1(L,M,N,TEST)
C
       PRINT 600, TEST
       WRITE(2, 600) TEST

 600   FORMAT(1H0,10X,'DA THEOR1 :', L3)
       IF(TEST) GOTO 20         ! L E M VERIFICANO LE HP DEL TEOR-l
C
       CALL THEOR2(L,N,TEST,ROW,ORD)
C
       PRINT 610, TEST
       WRITE(2, 610) TEST
610   FORMAT(1H0,10X,'DA THEOR2 :', L3)
       IF(.NOT.TEST) GOTO 50 ! LE HP DEI DUE TEOREMI NON SONO VERiFICATE
C
C      2.  L ED M SONO ISOMORFI?
C
       CALL ISORDN(L,M,N,ROW,ORD,TEST,ALFA,*500)  ! $ REPLACED BY * RETURN TO 500
C
       IF(.NOT.TEST) GOTO 400 ! L .NON ISO. M
       THEOR=2
       GO TO 450 !  L.ISO.M
 20    CONTINUE
C
C      2.  L ED M SONO ISOMORFI?
C
       CALL ISABEL (L,M,N,TEST)
C
       IF(.NOT.TEST) GOTO 400 ! L .NON ISO. M
       THEOR=1
       GO TO 450 !  L.ISO.M
 50    CONTINUE
C
C     3. FINITO? SE I=5, HP DEL COR. 3 NON RISPETTATA
       IF(I.EQ.5) GOTO 460
       I=I+1
C
C     4 COSTRUISCI UN'ALTRA COPPIA DI QL ASSOCIATI AD L ED M
C
       IND1=PERM(1,I)     !R
       IND2=PERM(2,I)     !C
       IND3=PERM(3,I)     !V
       DO 70 IT=1,NN
          DEX2=TL(IT,IND1) !R
          DEX1=TL(IT,IND2)
          L(DEX1,DEX2)=TL(IT,IND3)
          DEX2=TM(IT,IND1)
          DEX1=TM(IT,IND2)
          M(DEX1,DEX2)=TM(IT,IND3)
 70    CONTINUE
       K=1
       PRINT 260, K,L
       WRITE(2, 260) K,L
       K=2
       PRINT 260, K,M
       WRITE(2, 260) K,M
       GO TO 1         !<<<<<
C
 400   PRINT 270  ! L.NOT ISO.M
       WRITE(2, 270)
       CALL REPAG
       CLOSE(2)
       STOP
 450   CONTINUE   ! L.ISO.M
       PRINT 280
       WRITE(2, 280)
       IF(DEBUG.EQ.'D') PRINT *, 'THEOR=', THEOR
       IF (THEOR.NE.2) STOP
C     STAMPA ALFA
       PRINT 290
       WRITE(2, 290)
       CALL ASSPRM(ALFA, LS,LL,NC,N)
       CALL PRTPRM(LS,LL,NC, N, -999999)
       CALL REPAG ! dummy routine
       CLOSE(2)
       STOP
 500   PRINT 300  !ERROR EXIT
       WRITE(2, 300)
       CALL REPAG  ! dummy routine
       CLOSE(2)
       STOP
 460   PRINT 310
       WRITE(2, 310)
       CALL REPAG  ! dummy routine
       CLOSE(2)
       STOP
C
C     6**************************************************************
C     FORMATI I/O
C
  200  FORMAT(8I1) ! lettura da file ALF
C  200  FORMAT(36I1) lettura da schermo

 250  FORMAT(1H1,10X,'ISOMORFISMO TRA QUADRATI LATINI',//,11X,'ALGORITMO
     $ DEL COROLLARIO 3')
 260  FORMAT(1H0,40X,'QUADRATO', I5,2X,1H:,//,(36X,8I5//))
 270  FORMAT(1H0,10X,'I DUE QUADRATI LATINI NON SONO ISOMORFI <ALFA,
     $ALFA, ALFA>.')
 280  FORMAT(1H0,10X,'I DUE QUADRATI LATINI SONO ISOMORFI <ALFA,ALFA,
     $ ALFA>.')
 290  FORMAT(1H0,10X,'LA SEGUENTE PERMUTAZIONE ',2HE',' LA ALFA:')
 300  FORMAT(1H0,10X,'ERROR EXIT FROM ISORDN.')
 310  FORMAT(1H0,10X,'LE IPOTESI DEL COROLLARIO 3 NON SONO VERIFICATE
     $ PER I DUE QUADRATI LATINI')
C
C   DUMMY FUNCTIONS
C
      CONTAINS  ! INTERNAL  follow
      SUBROUTINE CONPAG()
        PRINT *, "DUMMY CONPAG"
        RETURN
      END SUBROUTINE
C
      SUBROUTINE REPAG()
        PRINT *, "DUMMY REPAG"
        RETURN
      END SUBROUTINE

      END
