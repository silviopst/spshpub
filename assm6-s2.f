C     QSQSQ$*$TURCO(1).ASSM6/S2
C     OCR_FORTRAN-PAG-24-31.pdf
C     6**************************************************************
C     COSTRUISCI LA MATRICE A (CODIFICATA) ASSOCIATA ALLE PERMUTAZIONI
C     RAPPRESENTANTI LE RIGHE, LE COLONNE, I VALORI DEL QUADRATO LATINO Q.
C     6**************************************************************
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*50 ALF
c      LOGICAL TEST
      CHARACTER DEBUG
      PARAMETER (N=6, M=3)
      PARAMETER (NN=N*N, N3=N*N*N)
      DIMENSION Q(N,N), RCF(N,NN), V(M,NN), VLS(N3),VSB(M,N3), QA(N,N)
      DIMENSION LISTA(N), LC(N), PRM(N), LS(NN)
C
C      DEFINE TWIN(C,R)=C+N*(R-1)
C      DEFINE IR(Z)=INT((Z-1)/N)+1
C      DEFINE IC(Z)=MOD((Z-1),N)+1
       TWIN(C,R)=C+N*(R-1)
       IR(Z)=INT((Z-1)/N)+1
       IC(Z)=MOD((Z-1),N)+1  !  errata, OK
        DO 1001 I=1,N
            DO 1001 J=1,N
        Q(I,J)=0
 1001   QA(I,J)=0
C
C     INIZIO: READ QUADRATO Q
C
      open (unit = 2, file = 'OUT-ASSM6-S2.TXT')
      WRITE(2,250)
      PRINT *, 'ENTER D FOR DEBUG, N '
      READ *, DEBUG
      ALF='QR06-ASS6-in.dat'
      PRINT *, 'ENTER Input File Name ', ALF
      READ *, ALF
C      PRINT *, 'ENTER Q'
C      READ(5,200) Q
      IF(DEBUG.EQ.'D') PRINT *, 'OPENING ', ALF, ' N= ', N , ' NN= ',NN
     $ , ' N3= ', N3
      OPEN(UNIT=1,FILE=ALF)
      IF(DEBUG.EQ.'D') PRINT *, 'OPENING ', ALF
      READ(1,200, IOSTAT=REASON) ((Q(I,J),I=1,N),J=1,N)
      IF(REASON.NE.0) GOTO 999
  200  FORMAT(6I1) ! lettura da file ALF
c      IF(DEBUG.EQ.'D') PRINT *, 'END Reading ', ALF

      CLOSE(1)
C
      PRINT 300, 'Q',Q
      WRITE(2, 300) 'Q',Q

C     WRITE(2, 510) 'Q',Q
C 510  FORMAT(1H1, 10X, 'QUADRATO  ', A5,2H :,///,(15X, 6I5//))
C
C   COSTRUISCI LA MATRICE DELLE TERNE
C
        IF(DEBUG.EQ.'D') PRINT *, 'COSTRUISCI LA MATRICE DELLE TERNE'
      DO 10 INDECS=3,1,-1         !
        FL=INDECS-2
c            IF(DEBUG.EQ.'D') PRINT *, 'loop 10 FL= ', FL

C
C   MAKE: COSTRUISCI RFC
C
      CALL MAKE(FL)
      FL=4-INDECS
C
C   COSTRUISCI LA COMPONENTE -FL- DELLA TERNA <T1,T2,T3>
C   T1=RI(I,J)
C   T2=CJ(I,J)
C   T3=VK(I,J)
C
      DO 100 I=1,N
        DO 100 J=1,N
            ADDR=TWIN(J,I)
            IF(FL.EQ.1) GOTO 90
            IF(FL.EQ.2) GOTO 80
            KAP=Q(J,I)
            V(FL, ADDR)=RCF(KAP,ADDR)
            GOTO 100
 80   V(FL,ADDR) =RCF(J,ADDR)
      GOTO 100
 90   V(FL,ADDR) =RCF(I,ADDR)
 100  CONTINUE
 10   CONTINUE
C
C   IN -V- GLI ELEMENTI NON CODIFICATI DI A
C
C   ORDINA E CODIFICA LE TERNE DEGLI ELEMENTI DI A(IN V)
C
      DO 20 K=1,NN
 20   LS(K)=K
C
      CALL HSORT3(V,NN,M,LS,NN,1)
C
C   CODIFICA V
C
      NCOD=0
      I1=IC(LS(1))
      I2=IR(LS(1))
      QA(I1,I2)=NCOD       ! Q:=A
      DO 50 I=2,NN
        B=LS(I)
        C=LS(I-1)
        DO 30 K=1,M
            IF(V(K,B).NE.V(K,C)) GOTO 35
 30   CONTINUE
      GOTO 40
 35   NCOD=NCOD+1    ! errata OK
 40   I1=IC(LS(I))
      I2=IR(LS(I))
      QA(I1,I2)=NCOD
 50   CONTINUE
C
C   STAMPA A (=:QA)
        PRINT 310, QA
        WRITE(2, 310) QA
        STOP
 999  PRINT *, 'ERROR OPENING', ALF,  REASON
      STOP
C
C     FORMATI I/O
C
C 200  FORMAT(6I1) ! lettura da file ALF
C  200  FORMAT(25I1) lettura da schermo
 250  FORMAT (1H ,20X,'QSQSQ$*$TURCO(1).ASSM6/S2')
 300  FORMAT(1H ,5X,'MATRICE ', A3,2H :,///,
     $(5X,6I5//))
 310  FORMAT(1H ,5X,'MATRICE ASSOCIATA A Q (CODIFICATA) :',///,
     $(5X,6I5//))



C
C   SUBROUTINES  *****************************************************
C
      CONTAINS  ! INTERNAL  follow
      SUBROUTINE MAKE(FLAG)
C
C
      CHARACTER*50 ALFA
      K=0
      DO 90 RI=1,N

      IF(FLAG) 50,30, 10
 10   DO 20 K=1,N
c
 20   LISTA(K)=Q(K,RI)
C      ALFA(1)='LE RIG'
C      ALFA(2)='HE    '
      ALFA='LE RIGHE'
      GOTO 70
C
 30   DO 40 K=1,N
 40   LISTA(K)=Q(RI,K)
C      ALFA(1)='LE COL'
C      ALFA(2)='ONNE  '
      ALFA='LE COLONNE'
      GOTO 70
C
 50   CONTINUE
      DO 60 I=1,N
      DO 55 J=1,N
      IF(RI.EQ.Q(J,I))GOTO 60
 55   CONTINUE
      STOP 1                   ! ERRORE DATI
 60   LISTA(I)=J
C      ALFA(1)='I VALO'
C      ALFA(2)='RI    '
       ALFA='I VALORI'
C
 70   CONTINUE
C
      CALL ASSPRM(LISTA,PRM,LC,NCR,N)
      IF(DEBUG.EQ.'D') CALL PRTPRM(PRM,LC,NCR,N,-999999)
      CALL ONSHOT(PRM,LC,NCR,N,NN,N3,VLS,VSB,RCF,RI,DEBUG)
C
C
 90   CONTINUE
C
C   COSTRUITO RCF
C
      PRINT *, 'LE',N,' MATRICI ASSOCIATE ALLA PERMUTAZIONI RAPPRE',
     $'SENTANTI ', ALFA,  ':'

      WRITE (2,*),'LE',N,' MATRICI ASSOCIATE ALLA PERMUTAZIONI RAPPRE',
     $'SENTANTI ', ALFA,  ':'
C
      PRINT 500, ((RCF(K,L), L=1,NN), K=1,N)
C
      WRITE (2,500) ((RCF(K,L), L=1,NN), K=1,N)
 500  FORMAT(2H :,///,(6(10X,6I5//)////))
      RETURN
      END SUBROUTINE
      END
