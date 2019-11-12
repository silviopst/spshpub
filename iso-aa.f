C	********	FILE: OCR_FORTRAN-PAG-01-06.pdf $TIJRCO.ISO-AA/SIG				
C	TEST DI ISOMORFISMO TRA  QUADRATI LATINI					
C	INPUT: Q,R, QUADRATI LATINI DI ORDINE N					
C	VIENE STABILITO SE I DUE QUADRATI SONO ISOMORFI <ALFA,ALFA, ID>.					
C						
C	********					
C						
C						
	IMPLICIT INTEGER (A-Z)
	CHARACTER ALF				
	PARAMETER (N=10)				
	DIMENSION Q(N,N), R(N,N), U(N), RIL(N), RDLPM1(N), S(N), VIL(N),VIL1(N)					
	DIMENSION LIST(N), LIST1(N), VMQ(N)					
	EQUIVALENCE (VMQ,RDLPM1)					
C	 					
C	INIZIO					
C						
	
	PRINT *, 'ENTER Q'
	READ(5,200) ((Q(I,J),I=1,N),J=1,N)
	PRINT *, 'ENTER R'		
	READ(5,200) ((R(I,J),I=1,N),J=1,N)					
	PRINT 300					
	ALF='Q'					
	PRINT 310, ALF,Q					
	ALF='R'				
	PRINT 310, ALF,R					
C						
C	1. COSTRUISCI IL VETTORE-MOLTEPLICITA' -DIAG(Q): VMQ					
C						
	DO 20 I=1,N					
	CONQ=0					
	CONR=0					
	DO 10 J=1,N					
	IF(I.EQ.Q(J,j)) CONQ=CONQ+1					
	IF(I.EQ.R(J,j)) CONR=CONR+1					
 10	CONTINUE					
	VMQ(I)=CONQ					
	IF(CONR.NE.VMQ(I)) GOTO 500					
 20	CONTINUE					
C						
C	VMQ==VMR					
C						
C	2. PRENDI L'ELEMENTO CON MOLTEPLICITA' MINORE					
C						
	MIN=N+1					
	IBT=1					
	DO 30 I=1,N					
	IF((MIN.LE.VMQ(I)).OR.(VMQ(I).EQ.0)) GOTO 30
	MIN=VMQ(I)					
	IBT=I					
 30	CONTINUE					
	BT=MIN					
C						
C	CERCA L'INDICE DI IBT IN DIAG(Q)					
C						
	DO 40 I=1,N					
	IF(IBT.NE.Q(I,I)) GO TO 40					
	IDQ=I					
	GO TO 50					
 40	CONTINUE					
 50	CONTINUE					
C						
C	3. COSTRUISCI LA MASK DI DIAGR( R ): U					
C						
	J=0					
	DO 60 I=1,N					
	IF(R(I,I).NE.IBT) GOTO 60					
	J=J+1					
	U(J)=I					
 60	CONTINUE					
C						
C	COSTRUITO U					
C						
C						
C						
C	COSTRUISCI RIL					
C						
	DO 70 J=1,N					
 70	RIL(J)=Q(J,IDQ)					
C						
C	ITERAZIONE					
C						
	DO 150 I=1,BT					
	D=U(I)					
C						
C	COSTRUISCI -RDLPH1-					
C						
	DO 80 J=1,N					
	K=R(J,D)					
 80	RDLPM1(K)=J					
C						
C	ESEGUI IL PRODOTTO: RIL*RDLPN1					
C						
	DO 90 J=1,N					
	IND=RIL(J)					
 90	S(J)=RDLPM1(IND)					
C						
C	TEST: -S- E' LA PERMUTAZIONE CERCATA?					
C						
	DO 130 K=1,N					
	CALL LISTVA(Q,K,VIL,N)					
	CALL LISTVA(R,K,VIL1,N)					
C	VIL, VIL1: LISTE DEI VALORI IN Q  ED R					
C	ESEGUI I PRODOTTI: (SM1*VIL)*S := LIST					
	DO 100 J=1,N					
	IND=S(J)					
 100	LIST1(IND)=VIL(J)					
	DO 110 J=1,N					
	IND=LIST1(J)					
 110	LIST(J)=S(IND)					
C						
C	TEST: LIST.EQ.VIL1?					
C						
	DO 120 J=1,N					
	IF(VIL1(J).NE.LIST(J)) GOTO 150	! .TRUE. : Q.NISO.R	
 120	CONTINUE					
 130	CONTINUE					
C						
C	Q.ISO.R  =>  -S- E' LA PERMUTAZIONE					
C						
	GO TO 400				! : Q.ISO.R	
C						
 150	CONTINUE					
C						
 500	PRINT 210					
	STOP					
C						
 400	PRINT 220					
	PRINT 240, (K,S(K),K=1,N)					
	PRINT 230					
	STOP					
C						
C	FORMATI I/O					
C						
 200	FORMAT(10I2)					
 210	FORMAT(1H1,10X, 'Q ED R NON ISOMORFI.', //,10X,'ELABORAZIONE TERMINATA.')					
						
 220	FORMAT(1H1, 10X,'Q ED R SONO ISOMORFI <ALFA, ALFA>',//,10X,'LA PERMUTAZIONE ALFA ', 2HE',' LA SEGUENTE :')					
						
 230	FORMAT(1H0, //,10X,'ELABORAZIONE TERMINATA.')					
 240	FORMAT(1H0, //,(5X,I5,4H -->  , I5//))					
 300	FORMAT(1H1, //, 14X,'* TEST DI ISOMORFISMO TRA QUADRATI LATINI *')					
						
 310	FORMAT(1H1,10X, 'QUADRATO  ', A2,2H :,///,(5X, 10I5//))					
C						
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
 10	CONTINUE
	STOP 1
 20	L(J)=K
	RETURN
	END
