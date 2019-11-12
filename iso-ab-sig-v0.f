C	********
C	FILE: OCR_FORTRAN-PAG-01-06.pdf $TIJRCO.ISO-AB/SIG
C	ISOMORFISMO  <ALFA,BETA, ID> TRA QUADRATI LATINI
C
C	********
C
	IMPLICIT INTEGER (A-Z)
        CHARACTER ALF
	PARAMETER (N=10)
	DIMENSION Q(N,N), P(N,N),BETA(N), ALFA(N), RIQ(N), CIQ(N), RIP(N)
	DIMENSION CTIP(N), SM1(N),VKQ(N), VKP(N), LIST(N), LIST1(N)
	EQUIVALENCE (RIP,CTIP), (SM1,RIP), (LIST,VKQ)
	PRINT *, 'ENTER Q'
        READ(5,200) Q
	PRINT *, 'ENTER P'
	READ(5,200) P
	PRINT 300
	ALF='Q'
	PRINT 310, ALF, Q
	ALF='P'
	PRINT 310, ALF, P
C
C     FISSA UNA RIGA DI Q: -II-
C
        II=N/2
C
C     1. COSTRUISCI: -RIQ-,-CIQ-
C
        DO 10 J=1,N
        RIQ(J)=Q(J,II)
 10     CIQ(J)=Q(II,J)
C
C     ITERAZIONE
C
        DO 100 I=1,N
C     * COSTRUISCI -BETA-
C
C     COSTRUISCI -RIP- (-1)
C
        DO 20 J=1,N
        K=P(J,I)
 20     RIP(K)=J
C
C     BETA <-- RIQ*RIP
C
        CALL PRPRM(RIQ,RIP,BETA,N)
C
C     * COSTRUISCI -ALFA-
C
C     COSTRUISCI -CTIP- (-I)
        K=BETA(II)
        DO 40 J=1,N
           IND=P(K,J)
 40        CTIP(IND)=J
C
C     ALFA=CIQ*CTIP
c
           CALL PRPRM(CIQ,CTIP,ALFA,N)
C
C     ** VERIFICA SE ALFA E BETA SONO LE PERMUTAZIONI CERCATE
C
C     SM1=ALFA**-1
C
           DO 50 J=1,N
              IND=ALFA(J)
 50           SM1(IND)=J
              DO 80 K=1,N
C
C     COSTRUISCI -VKP- -VKQ- (LISTA VALORI I DI -P-)
C
                 CALL LISTVA(Q,K,VKQ,N)
                 CALL LISTVA(P,K,VKP,N)
C
C     ESEGUI I PRODOTTI: (SM1*VKQ)*BETA := LIST
C
                 CALL PRPRM(SM1,VKQ,LIST1,N)
                 CALL PRPRM(LIST1,BETA,LIST,N)
C
C     LIST=VKP ?
C
                 DO 70 J=1,N
                    IF(VKP(J).NE.LIST(J)) GOTO 100
 70                 CONTINUE
 80                 CONTINUE
C     Q.ISO.P => AlFA,BETA LE PERMUTAZIONI
C
                    GOTO 400   ! Q.ISO.P
C
 100                CONTINUE ! <<<< CHIUDE IL CICLO  <<<<
C
                    PRINT 210
                    STOP
 400                PRINT 220
                    CALL ASSPRM(ALFA,LIST,LIST1,NC,N)
                    CALL PRTPRM(LIST,LIST1,NC,N,-999999)
                    PRINT 250
                    CALL ASSPRM(BETA,LIST,LIST1,NC,N)
                    CALL PRTPRM(LIST,LIST1,NC,N,-999999)
                    PRINT 230
                    STOP
C
C	FORMATI I/O
C
 200	FORMAT(10I2)
 210	FORMAT(1H1,10X, 'Q ED P NON SONO ISOMORFI <ALFA,BETA,ID>', //,10X,'ELABORAZIONE TERMINATA.')

 220	FORMAT(1H1, 10X,'Q ED R SONO ISOMORFI <ALFA, BETA>',//,10X,'LA PERMUTAZIONE ALFA ', 2HE',' LA SEGUENTE :')

 230	FORMAT(1H0, //,10X,'ELABORAZIONE TERMINATA.')
 240	FORMAT(1H0, //,(35X,I5,4H -->  , I5//))
 250	FORMAT(1H1, 10X,//,'LA PERMUTAZIONE BETA ', 2HE',' LA SEGUENTE :')
 300	FORMAT(1H1, //, 44X,'* TEST DI ISOMORFISMO TRA QUADRATI LATINI *')
 310	FORMAT(1H1,40X, 'QUADRATO  ', A2,2H :,///,(35X, 10I5//))
C
        END
