      PROGRAM ANPASS
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    MULTI-DIMENSIONAL POLYNOMIAL LEAST-SQUARES FIT AND STATIONARY
C....    POINT DETERMINATION PROGRAM.
C
C....                                     PETER R. TAYLOR
C....                                       MELBOURNE AND MALLACOOTA
C....                                         OCT '82
C
      CHARACTER*4 WORD, VOCAB(0:5)
      CHARACTER*20 INPUT, OUTPUT
      LOGICAL BUMF,SKIP
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      COMMON WORK(100000000)
      DATA MAXMEM /100000000/
      DATA VOCAB/'!SKI' , '!INP', '!FIT', '!STA', '!EVA' , '!END'/
      DATA BUMF/.TRUE./ , SKIP/.FALSE./
      DATA NUMVOC/5/

      WRITE(JOBOUT,2000)
      CALL DATIM
10    READ(JOBIN,1000,END=20) WORD
      IF( SKIP ) THEN
      SKIP = WORD.NE.VOCAB(5)
      GOTO 10
      ENDIF
      DO 200 I = 0,NUMVOC
      II = I
      IF (VOCAB(I) .EQ. WORD) GOTO 300
200   CONTINUE
      IF( .NOT. BUMF ) THEN
      WRITE(JOBOUT,'('' =====> '',A)') WORD
      STOP ' DIRECTIVE MISSPELLED OR OUT OF SEQUENCE - ANPASS'
      ENDIF
      GOTO 10
300   CONTINUE
      IF( II.EQ.0 ) THEN
      SKIP = .TRUE.
      GOTO 10
      ENDIF
      GOTO (1,2,3,4,5), II
1     CONTINUE
C
C....    PROCESS INPUT
C
      BUMF=.FALSE.
      CALL READIN
      CALL INLIST
      CALL BIAS
      GOTO 10
2     CONTINUE
C
C....    ALLOCATE WORKING STORAGE AND PERFORM FIT
C
      IW1 = 0
      IW2 = IW1 + NUNK*NUNK
      IW3 = IW2 + NUNK
      IW4 = IW3 + NUNK
      MEMT = IW4 + NUNK
      IF (MEMT .GT. MAXMEM)
     1     STOP ' INSUFFICIENT MEMORY FOR FIT - ANPASS'
      CALL FIT(WORK(IW1+1), WORK(IW2+1), WORK(IW3+1), WORK(IW4+1),NUNK)
      GOTO 10
3     CONTINUE
C
C....    ALLOCATE WORKING STORAGE AND DETERMINE STATIONARY POINT
C
      BUMF=.FALSE.
      IW1 = 0
      IW2 = IW1 + NUNK
      IW3 = IW2 + (NUNK*(NUNK+1))/2
      IW4 = IW3 + NUNK*NUNK
      IW5 = IW4 + NUNK
      MEMT = IW5 + NUNK
      IF (MEMT .GT. MAXMEM)
     1     STOP ' INSUFFICIENT MEMORY FOR NEWTON - ANPASS'
      CALL NEWTON(WORK(IW1+1), WORK(IW2+1), WORK(IW3+1),
     1            WORK(IW4+1), WORK(IW5+1), NUNK)
C
C....    WORK(1) CONTAINS STATIONARY POINT
C
      CALL STATIO(WORK(IW1+1), WORK(IW2+1), WORK(IW3+1),
     1            WORK(IW4+1), WORK(IW5+1), NUNK)
      GOTO 10
4     CONTINUE
C
C....    EVALUATE FUNCTION AT A POINT
C
      BUMF = .FALSE.
      CALL EVALX
      GOTO 10
5     CONTINUE
      BUMF = .TRUE.
      GOTO 10
20    STOP ' END ANPASS '
1000  FORMAT(A4)
2000  FORMAT(///,20X,'M O L E C U L E - A N P A S S',//,
     1      10X,'LEAST-SQUARES POLYNOMIAL FIT IN SEVERAL DIMENSIONS',/,
     2      40X,'PETER R. TAYLOR',/,
     3      42X,'MELBOURNE AND MALLACOOTA',/,
     4      44X,'OCTOBER 1982',//)
      END
      SUBROUTINE BIAS
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    BIAS POINTS
C
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      DO 10 I = 1,NPTS
      E(I) = E(I) - EBIAS
      DO 20 J = 1,NVBL
      R(J,I) = R(J,I) - RBIAS(J)
20    CONTINUE
10    CONTINUE
      RETURN
      END
      BLOCK DATA
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    INITIALIZE PARAMETERS
C
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      DATA JOBIN/5/, JOBOUT/6/
      DATA TITLE/11*'      '/
      DATA MAXVBL/50/, MAXPTS/25000/, MAXUNK/5000/
      DATA D0/0.D00/, D1/1.D00/, D2/2.D00/
      END
      SUBROUTINE DATIM
      CHARACTER*9 DBUF
      CHARACTER*8 TBUF
!      CALL DATE(DBUF)
!      CALL TIME(TBUF)
      WRITE(6,2000) DBUF, TBUF
      RETURN
2000  FORMAT('0 Job run on ',A9,
     1       ' at ',A8)
      END
      SUBROUTINE DMINV(N,NMAX,A)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     THIS SUBROUTINE INVERTS THE MATRIX A OF DIMENSION N,N
C
      DIMENSION A(NMAX,NMAX)
      D=1.
      DO 80 K=1,N
      BIGA=A(K,K)
   48 DO 55 I=1,N
      IF (I-K) 50,55,50
   50 A(I,K)=-A(I,K)/BIGA
   55 CONTINUE
      DO 65 I=1,N
      HOLD=A(I,K)
      DO 65 J=1,N
      IF((I-K)*(J-K)) 62,65,62
   62 A(I,J)=HOLD*A(K,J)+A(I,J)
   65 CONTINUE
      DO 75 J=1,N
      IF(J-K)70,75,70
   70 A(K,J)=A(K,J)/BIGA
   75 CONTINUE
      D=D*BIGA
   80 A(K,K)=1./BIGA
      RETURN
      END
      REAL*8 FUNCTION EVAL (X, COEFF)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    EVALUATE FUNCTION AT X, WITH COEFFICIENTS IN VECTOR COEFF
C
      DIMENSION X(*), COEFF(*)
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      DATA THR /1.D-10/
      SUM = D0
      DO 10 J = 1,NUNK
      COJ = COEFF(J)
      IF (DABS(COJ) .LT. THR) GOTO 10
      DO 20 K = 1,NVBL
      IEXP = IEXPO(K,J)
      XKI = D1
      IF (IEXP .NE. 0) XKI = X(K)**IEXP
      COJ = COJ*XKI
20    CONTINUE
      SUM = SUM + COJ
10    CONTINUE
      EVAL = SUM
      RETURN
      END
      SUBROUTINE EVALX
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    EVALUATE THE POYNOMIAL FUNCTION AT A POINT
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1               W(25000),IEXPO(50,5000),RBIAS(50),EBIAS,XCO(5000),
     2               XEVAL(50)

      DO 10,L10=1,NVBL
         XEVAL(L10) = XEVAL(L10)-RBIAS(L10)
   10 CONTINUE
      VALUE = EVAL(XEVAL,XCO) + EBIAS
      DO 20,L20=1,NVBL
         XEVAL(L20) = XEVAL(L20)+RBIAS(L20)
   20 CONTINUE
      WRITE(JOBOUT,1000) VALUE, (XEVAL(K),K = 1,NVBL)
1000  FORMAT(//,'0',20X,'E V A L U A T E D',//,
     1    ' FUNCTION VALUE IS ',E16.8,/,10X,'AT',4X,F12.8,/,(16X,F12.8))
      RETURN
      END
      SUBROUTINE FIT (G,B,SOL,COEFF,NMAX)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    PERFORM LEAST-SQUARES FIT
C
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1               W(25000),IEXPO(50,5000),RBIAS(50),EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

        dimension ictmp(4)
      DIMENSION G(NMAX,NMAX), B(NMAX), SOL(NMAX), COEFF(NMAX)
C
C....    SET UP RIGHT-HAND SIDES IN B, AND MATRIX IN G.  VECTOR SOL IS
C....    USED FOR TEMPORARY STORAGE
C
      DO 10 I = 1,NUNK
      B(I) = D0
      COEFF(I) = D0
      DO 20 J = 1,NUNK
      G(J,I) = D0
20    CONTINUE
10    CONTINUE
      DO 30 I = 1,NPTS
      WI = W(I)
      EI = E(I)
      DO 40 J = 1,NUNK
      COEFF(J) = D1
      SOL(J) = EVAL(R(1,I), COEFF)
      COEFF(J) = D0
40    CONTINUE
      IF (IPRINT .GE. 40) WRITE(JOBOUT,2800) I, (SOL(J),J = 1,NUNK)
      DO 50 J = 1,NUNK
      SJ = SOL(J)
      B(J) = B(J) + WI*EI*SJ
      DO 60 K = 1,J
      T = WI*SJ*SOL(K)
      G(J,K) = G(J,K) + T
      IF (J .EQ. K) GOTO 60
      G(K,J) = G(K,J) + T
60    CONTINUE
50    CONTINUE
30    CONTINUE
      IF (IPRINT .GE. 20)
     1  WRITE(JOBOUT,2500) ((G(K,J),J = 1,NUNK), K = 1,NUNK)
      IF (IPRINT .GE. 20) WRITE(JOBOUT,2600) (B(K),K = 1,NUNK)
C
C....    INVERT G IN PLACE
C
      CALL DMINV(NUNK,NMAX,G)
      IF (IPRINT .GE. 20)
     1  WRITE(JOBOUT,2700) ((G(K,J),J = 1,NUNK), K = 1,NUNK)
C
C....    MULTIPLY INTO RHS TO GIVE SOLUTION
C
      DO 70 J = 1,NUNK
      SUM = D0
      DO 80 K = 1,NUNK
      SUM = SUM + G(J,K)*B(K)
80    CONTINUE
      XCO(J) = SUM
70    CONTINUE
      WRITE(JOBOUT,2100) (XCO(J),J = 1,NUNK)

        write(9901,*)NUNK
        write(9902,*)NUNK
        write(9903,*)NUNK
        write(9905,*)NUNK

        do i=1,NUNK
          ictmp=0
          ifact=1
          iccount=0
          do j=NVBL,1,-1
            if(iexpo(j,i).eq.2)ifact=ifact*2
            if(iexpo(j,i).eq.3)ifact=ifact*6
            if(iexpo(j,i).eq.4)ifact=ifact*24
            if(iexpo(j,i).gt.0)then
              do k=1,iexpo(j,i)
                ictmp(iccount+k)=j
              end do
              iccount=iccount+iexpo(j,i)
            end if
          end do
!         write(9901,'(6I5,D24.16)')(IEXPO(j,i),j=1,NVBL),xco(i)
          ffcc=xco(i)*ifact*4.359813653D0
          write(9901,'(6I5,D24.16)')(IEXPO(j,i),j=1,NVBL),ffcc
!         write(9902,'(6I5,F20.12)')(IEXPO(j,i),j=1,NVBL),ffcc
          write(9903,'(4I5,F20.12)')(ictmp(j),j=1,4),ffcc

          ffcctmp1=ffcc
          if(i.eq.1)ffcctmp1=0.d0
          write(9902,'(6I5,F20.12)')(IEXPO(j,i),j=1,NVBL),ffcctmp1

          imn0=4
          if(ictmp(4).eq.0)imn0=imn0-1
          if(ictmp(3).eq.0.and.imn0.eq.3)imn0=imn0-1
          if(ictmp(2).eq.0.and.imn0.eq.2)imn0=imn0-1
!        if(imn0.eq.4)write(9905,'(4I5,F20.12)')(ictmp(j),j=1,4),ffcc
!        if(imn0.eq.3)write(9905,'(3I5,5x,F20.12)')(ictmp(j),j=1,3),ffcc
!        if(imn0.eq.2)write(9905,'(2I5,10x,F20.12)')(ictmp(j),j=1,2),ffcc
!        if(imn0.eq.1)write(9905,'(1I5,15x,F20.12)')(ictmp(j),j=1,1),ffcc

        write(9905,'(4I5,F20.12)')(ictmp(j),j=1,4),ffcc

        end do
        close(9901)
        close(9902)
        close(9903)
        close(9905)
        call system('cp fort.9901 fort.9901.toread')    
        call system('cp fort.9902 fort.9902.toread')
        call system('cp fort.9903 fort.9903.toread')
        call system('cp fort.9905 fort.9905.toread')

C
C....    EVALUATE RESIDUALS
C
      RSQ = D0
      WRITE(JOBOUT,2200)
      DO 100 I = 1,NPTS
      EFI = EVAL(R(1,I),XCO)
      DD = EFI - E(I)
      EFI = EFI + EBIAS
      EI = E(I) + EBIAS
      WRITE(JOBOUT,2300) I, EFI, EI, DD
      RSQ = RSQ + W(I)*DD*DD
100   CONTINUE
      WRITE(JOBOUT,2400) RSQ
      RETURN
2100  FORMAT('0EXPANSION COEFFICIENTS',/,(1X,4F22.12))
2200  FORMAT('0',10X,'FITTED RESULTS',/,
     1       ' POINT',7X,'COMPUTED',10X,'OBSERVED',12X,'RESIDUAL')
2300  FORMAT(1X,I5,6X,F18.12,6X,F18.12,6X,1PE16.8)
2400  FORMAT('0WEIGHTED SUM OF SQUARED RESIDUALS IS ',1PE16.8)
2500  FORMAT('0G-MATRIX',/,(1X,6F11.5))
2600  FORMAT('0B-VECTOR (RHS OF LINEAR EQNS)',/,(1X,6F11.5))
2700  FORMAT('0INVERSE OF G-MATRIX',/,(1X,6F11.5))
2800  FORMAT('0TERM VALUES AT POINT',I5,/,(1X,6F11.5))
      END
      SUBROUTINE GRAD (X, COEFF, GRD)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    VECTOR OF DERIVATIVES AT X
C
      DIMENSION X(*), COEFF(*), GRD(*)
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      DATA THR /1.D-10/

      DFLOAT(I) = DBLE(FLOAT(I))

      DO 50 I = 1, NVBL
      SUM = D0
      DO 10 J = 1,NUNK
      COJ = COEFF(J)*DFLOAT(IEXPO(I,J))
      IF (DABS(COJ) .LT. THR) GOTO 10
      IF (IEXPO(I,J) .NE. 1) COJ = COJ*X(I)**(IEXPO(I,J) - 1)
      DO 20 K = 1,NVBL
      IF (K .NE. I .AND. IEXPO(K,J) .NE. 0) COJ = COJ*X(K)**IEXPO(K,J)
20    CONTINUE
      SUM = SUM + COJ
10    CONTINUE
      GRD(I) = SUM
50    CONTINUE
      RETURN
      END
      SUBROUTINE HESS (X, COEFF, HESIAN)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    LOWER TRIANGLE OF DERIVATIVE MATRIX
C
      DIMENSION X(*), COEFF(*), HESIAN(*)
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      DATA THR /1.D-10/

      DFLOAT(I) = DBLE(FLOAT(I))

      IJ = 0
      DO 10 I = 1,NVBL
      DO 20 L = 1,I
      IJ = IJ + 1
      SUM = D0
      IF (I .EQ. L) GOTO 30
C
C....    OFF-DIAGONAL
C
      DO 40 J = 1,NUNK
      COJ = COEFF(J)*DFLOAT(IEXPO(I,J))*DFLOAT(IEXPO(L,J))
      IF (DABS(COJ) .LT. THR) GOTO 40
      IF (IEXPO(I,J) .NE. 1) THEN
        IF (IEXPO(L,J) .NE. 1) THEN
          COJ = COJ*(X(I)**(IEXPO(I,J) - 1))*(X(L)**(IEXPO(L,J) - 1))
        ELSE
          COJ = COJ*(X(I)**(IEXPO(I,J) - 1))
        ENDIF
      ELSE
        IF (IEXPO(L,J) .NE. 1) THEN
          COJ = COJ*(X(L)**(IEXPO(L,J) - 1))
        ENDIF
      ENDIF
      DO 60 K = 1,NVBL
      IF (K .NE. I .AND. K .NE. L .AND. IEXPO(K,J) .NE. 0)
     1 COJ = COJ*X(K)**IEXPO(K,J)
60    CONTINUE
      SUM = SUM + COJ
40    CONTINUE
      GOTO 50
30    CONTINUE
C
C....    DIAGONAL
C
      DO 70 J = 1,NUNK
      COJ = COEFF(J)*DFLOAT(IEXPO(I,J)*(IEXPO(I,J) - 1))
      IF (DABS(COJ) .LT. THR) GOTO 70
      IF (IEXPO(I,J) .NE. 2) COJ = COJ*X(I)**(IEXPO(I,J) - 2)
      DO 80 K = 1,NVBL
      IF (K .NE. I .AND. IEXPO(K,J) .NE. 0) COJ = COJ*X(K)**IEXPO(K,J)
80    CONTINUE
      SUM = SUM + COJ
70    CONTINUE
50    CONTINUE
      HESIAN(IJ) = SUM
20    CONTINUE
10    CONTINUE
      RETURN
      END
      SUBROUTINE JACO (F,V,NB,NMAX,BIG,JBIG)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION BIG(*) ,JBIG(*)
      DIMENSION F(*),V(*)
      DATA ROOT2 /0.707106781186548D0/
      DATA C1,C2,C3,C4,C5,C6/1.D-10,1.D-10,1.D-22,1.D-16,1.D-11,1.D-7/
      IF(NB.EQ.1) RETURN
      II=0
      DO 190 I=1,NB
      BIG(I)=0.E0
      JBIG(I)=0
      IF(I.EQ.1) GO TO 190
      J=MIN0(I-1,NMAX)
      DO 18 K=1,J
      IF( DABS(BIG(I)) . GE .  DABS(F(II+K))) GO TO  18
      BIG(I)=F(II+K)
      JBIG(I)=K
   18 CONTINUE
  190 II=II+I
  410 SD=1.05
      DO 220 J=1,NMAX
      JJ12=J*(J+1)/2
      FJJ12=F(JJ12)
      DAB=DABS(FJJ12)
      IF(SD . GT . DAB)  SD=DAB
  220 SD=MAX(SD,DAB)
      SD=MAX(C1,C2*SD)
      T=0.E0
      DO 230 I=2,NB
      IF(T . GE .  DABS(BIG(I))) GO TO 230
      T= DABS(BIG(I))
      IB=I
  230 CONTINUE
    2 IF(T.LT.SD) GO TO 420
      IA=JBIG(IB)
      IAA=IA*(IA-1)/2
      IBB=IB*(IB-1)/2
      JAA=(IA-1)*NB
      JBB=(IB-1)*NB
      DIF=F(IAA+IA)-F(IBB+IB)
      IF( DABS(DIF) . GT . C3) GO TO 271
      SX=ROOT2
      CX=ROOT2
      GO TO 270
  271 T2X2=BIG(IB)/DIF
      T2X25=T2X2*T2X2
      IF(T2X25 . GT . C4) GO TO 240
      CX=1.E0
      SX=T2X2
      GO TO 270
  240 IF(T2X25 . GT . C5) GO TO 250
      SX=T2X2*(1.E0-1.5*T2X25)
      CX=1.E0-0.5*T2X25
      GO TO 270
  250 IF(T2X25 . GT . C6) GO TO 260
      CX=1.E0+T2X25*(T2X25*1.375 - 0.5)
      SX= T2X2*(1.0 + T2X25*(T2X25*3.875 - 1.5))
      GO TO 270
  260 T=0.25  / SQRT(0.25   + T2X25)
      CX= SQRT(0.5   + T)
      SX=SIGN( SQRT(0.5   - T),T2X2)
  270 IEAR=IAA+1
      IEBR=IBB+1
      DO 390 IR=1,NB
      T=F(IEAR)*SX
      F(IEAR)=F(IEAR)*CX+F(IEBR)*SX
      F(IEBR)=T-F(IEBR)*CX
      IF(IR-IA) 380,280,290
  280 TT=F(IEBR)
      IEAA=IEAR
      IEAB=IEBR
      F(IEBR)=BIG(IB)
      IEAR=IEAR+IR-1
      IF(JBIG(IR)) 360,380,360
  290 T=F(IEAR)
      IT=IA
  300 IEAR=IEAR+IR-1
      IF(IR-IB) 340,310,320
  310 F(IEAA)=F(IEAA)*CX+F(IEAB)*SX
      F(IEAB)=TT*CX+F(IEBR)*SX
      F(IEBR)=TT*SX-F(IEBR)*CX
      IEBR=IEBR+IR-1
      GO TO 360
  320 IF(  DABS(T) . GE .  DABS(F(IEBR))) GO TO 330
      T=F(IEBR)
      IT=IB
  330 IEBR=IEBR+IR-1
  340 IF(  DABS(T) . LT .  DABS(BIG(IR))) GO TO 350
      BIG(IR) = T
      JBIG(IR) = IT
      GO TO 380
  350  IF(IA . NE . JBIG(IR) . AND . IB . NE . JBIG(IR))  GO TO 380
  360 K=IEAR - IR - IA +2
      BIG(IR)=0.E0
      IR1=MIN0 (IR-1,NMAX)
      DO 370 I=1,IR1
      IF(DABS(BIG(IR)) . GE . DABS(F(K)))  GO TO 370
      BIG(IR) = F(K)
      JBIG(IR)=I
  370 K=K+1
  380 IEAR=IEAR+1
  390 IEBR=IEBR+1
      DO 400 I=1,NB
      T=V(JBB+I)*SX
      V(JBB+I)=V(JAA+I)*SX - V(JBB+I)*CX
  400 V(JAA+I)=V(JAA+I)*CX+T
      GO TO 410
  420 RETURN
      END
      SUBROUTINE NEWTON (X, HE, H, GRD, DEL, NMAX)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    DETERMINE STATIONARY POINT OF FUNCTION SPECIFIED BY XCO AND
C....    IEXPO.  THIS IS A BRUTE-FORCE NEWTON-RAPHSON WITH INVERSION
C....    OF THE HESSIAN.
C
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      DIMENSION X(*), HE(*), H(NMAX,NMAX), GRD(*), DEL(*)
C
C....    STARTING GUESS IS ZERO VECTOR
C....    (THAT IS, RBIAS, EBIAS IN THE OLD SCALE)
C
      DO 10 I = 1,NVBL
      X(I) = D0
10    CONTINUE
      DO 20 ITER = 1,100
      CALL GRAD(X,XCO,GRD)
      CALL HESS(X,XCO,HE)
C
C....    EXPAND TRIANGULAR HESSIAN TO SQUARE
C
      IJ = 0
      DO 30 I = 1,NVBL
      DO 40 J = 1,I
      IJ = IJ + 1
      H(I,J) = HE(IJ)
      H(J,I) = HE(IJ)
40    CONTINUE
30    CONTINUE
      CALL DMINV(NVBL,NMAX,H)
C
C....    NR UPDATE USING A STEP LENGTH OF 1/2
C
      ICONV = 1
      DO 50 I = 1,NVBL
      SUM = D0
      DO 60 J = 1,NVBL
      SUM = SUM + 0.5D0*H(I,J)*GRD(J)
60    CONTINUE
      DEL(I) = SUM
      IF (DABS(SUM) .GT. 0.00000001d0) ICONV = 0
50    CONTINUE
      IF (ICONV .GT. 0) GOTO 100
C
C....    NOT YET CONVERGED
C
      IF (IPRINT .GE. 10) WRITE(JOBOUT,2000) ITER, (DEL(I),I = 1,NVBL)
      DO 70 I = 1,NVBL
      X(I) = X(I) - DEL(I)
70    CONTINUE
20    CONTINUE
      STOP ' TOO MANY NEWTON-RAPHSON ITERATIONS - NEWTON'
100   CONTINUE
C
C....    VECTOR X NOW CONTAINS THE MINIMUM
C
      RETURN
2000  FORMAT(' ITERATION ',I4,' UPDATE VECTOR',/,(1X,5F12.8))
      END
      SUBROUTINE READIN
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    READ AND PROCESS INPUT DATA
C
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      CHARACTER*6 KEY(9), IWORD
      CHARACTER*80 FRMT
      CHARACTER*1 CHRINT
      CHARACTER*6 JFRMT
      DATA THR/1.D-10/
      DATA NUMKEY /9/
      DATA KEY/'END OF', 'TITLE', 'PRINT', 'INDEPE', 'DATA P',
     1         'UNKNOW', 'FUNCTI', 'STATIO' , 'POINT'/
      IERR = 0
      IBIAS = 0
C
C....    READ INPUT KEYWORDS
C
100   CONTINUE
      READ(JOBIN,1000) IWORD
      DO 200 I = 1,NUMKEY
      II = I
      IF (KEY(I) .EQ. IWORD) GOTO 300
200   CONTINUE
      WRITE(JOBOUT,2000) IWORD
      STOP ' KEYWORD NOT IN DICTIONARY - READIN'
300   CONTINUE
      GOTO (1,2,3,4,5,6,7,8,9), II
2     CONTINUE
C
C....    READ TITLE
C
      READ(JOBIN,1100) TITLE
      GOTO 100
3     CONTINUE
C
C....    READ PRINT LEVEL
C
      READ(JOBIN,1200) IPRINT
      GOTO 100
4     CONTINUE
C
C....    READ NUMBER OF INDEPENDENT VARIABLES
C
!      READ(JOBIN,1200) NVBL
        read(jobin,*)nvbl
      IF (NVBL .LE. MAXVBL) GOTO 100
      WRITE(JOBOUT,2100) NVBL, MAXVBL
      IERR = IERR + 1
      GOTO 100
5     CONTINUE
C
C....    READ DATA POINTS
C
!      READ(JOBIN,1200) NPTS, KFRMT
        read(jobin,*)npts,kfrmt
C
C....    SET UP INPUT FORMAT
C
      WRITE(CHRINT,'(I1)') NVBL
      JFRMT = 'F10.2,'
      IF (KFRMT .EQ. 1) THEN
        JFRMT = 'F20.2,'
        FRMT = '(' // CHRINT // 'F10.2,' // JFRMT // 'F10.2)'
      ENDIF
C
C....    READ INPUT FORMAT IF REQUIRED.  NOTE THAT FLAG IS NEGATIVE
C....    IF WEIGHTS ARE NOT READ.  (THEY ARE THEN TAKEN AS UNITY)
C
      IF (IABS(KFRMT) .EQ. 2) THEN
!        READ(JOBIN,1400) FRMT
         read(jobin,'(A80)')FRMT
      ENDIF
      IF (NPTS .LE. MAXPTS) GOTO 505
      WRITE(JOBOUT,2200) NPTS, MAXPTS
      IERR = IERR + 1
C
C....    READ OVER INPUT DATA POINTS, READING INTO FIRST AND SUBSEQUENT
C....    COLUMNS OF THE ARRAY.
C
      DO 510 I = 1,NPTS
      IF (KFRMT .GE. 0) THEN
!        READ(JOBIN,FRMT) (R(J,1),J = 1,NVBL), E(1), W(1)
         read(jobin,*)(r(j,1),j=1,nvbl),e(1),w(1)
      ELSE
!        READ(JOBIN,FRMT) (R(J,1),J = 1,NVBL), E(1)

         read(jobin,*)(R(J,1),J=1,NVBL),E(1)

      ENDIF
510   CONTINUE
      GOTO 100
505   CONTINUE
C
C....    READ DATA POINTS
C
      DO 515 I = 1,NPTS
      IF (KFRMT .GE. 0) THEN
        READ(JOBIN,FRMT) (R(J,I),J = 1,NVBL), E(I), W(I)
         read(jobin,*)(r(j,i),j=1,nvbl),e(i),w(i)
      ELSE
!        READ(JOBIN,FRMT) (R(J,I),J = 1,NVBL), E(I)
         read(jobin,*)(r(j,i),j=1,nvbl),e(i)
        W(I) = D1
      ENDIF
515   CONTINUE
      GOTO 100
6     CONTINUE
C
C....    READ NUMBER OF UNKNOWN COEFFICIENTS
C
!      READ(JOBIN,1200) NUNK
        read(jobin,*)nunk
      IF (NUNK .LE. MAXUNK) GOTO 100
      WRITE(JOBOUT,2300) NUNK, MAXUNK
      IERR = IERR + 1
      GOTO 100
7     CONTINUE
C
C....    READ FUNCTION EXPONENTS
C
      IF (NUNK .GT. MAXUNK) GOTO 705

      DO 710 I = 1,NVBL
!      READ(JOBIN,1200) (IEXPO(I,J),J = 1,NUNK)
        read(jobin,*)(iexpo(i,j),j=1,nunk)
710   CONTINUE

!       do 710 i=1,nunk
!         read(jobin,*)(iexpo(j,i),j=1,nvbl)
!710    end do

      GOTO 100
705   CONTINUE
C
C....    READ OVER FUNCTION
C
      DO 715 I = 1,NVBL
      READ(JOBIN,1200) (IEXPO(J,1),J = 1,NUNK)
715   CONTINUE
      GOTO 100
8     CONTINUE
C
C....    READ GUESS AT STATIONARY POINT
C
!      READ(JOBIN,FRMT) (RBIAS(I), I = 1,NVBL), EBIAS
        read(jobin,*)(rbias(i),i=1,nvbl),ebias
      IBIAS = 1
      GOTO 100
9     CONTINUE
C
C....    READ POINT AT WHICH FUNCTION IS TO BE EVALUATED
C
      READ(JOBIN,1300) (XEVAL(I), I = 1,NVBL)
      GOTO 100
1     CONTINUE
C
C....    REPROCESS INPUT DATA AS REQUIRED
C
C....    RESET WEIGHTS AND FIND MINIMUM FOR DEFAULT BIASING
C
      EMIN = 10 000.
      DO 400 I = 1,NPTS
      IF (W(I) .LT. THR) W(I) = D1
      IF (EMIN .LT. E(I)) GOTO 400
      EMIN = E(I)
      II = I
400   CONTINUE

        goto 9876
        do i=1,npts
          if(sum(dabs(R(:,i))).le.1.d-6)then
            EMIN=E(i); II=i; goto 9876
          end if
        end do
9876    continue
        write(4928,'(12F7.3,F20.12)')R(1:12,II),E(II)
        close(4928)
        call system('mv fort.4928 minimum.in.fort.3008')

      IF (IBIAS .EQ. 1) GOTO 500
      DO 600 I = 1,NVBL
      RBIAS(I) = R(I,II)
600   CONTINUE
      EBIAS = E(II)
500   CONTINUE
      RETURN
1000  FORMAT(A6)
1100  FORMAT(11A6)
1200  FORMAT(16I5)
1300  FORMAT(8F10.2)
1400  FORMAT(A)
2000  FORMAT('0INPUT KEYWORD ',A6,' NOT IN DICTIONARY')
2100  FORMAT('0NUMBER OF INDEPENDENT VARIABLES INPUT IS',I4,/,
     1       ' WHICH EXCEEDS THE INSTALLED LIMIT OF    ',I4)
2200  FORMAT('0NUMBER OF DATA POINTS INPUT IS',I4,/,
     1       ' WHICH EXCEEDS THE INSTALLED LIMIT OF    ',I4)
2300  FORMAT('0NUMBER OF UNKNOWN COEFFICIENTS TO BE DETERMINED IS',I4,
     1  /,   ' WHICH EXCEEDS THE INSTALLED LIMIT OF              ',I4)
      END
      SUBROUTINE STATIO (X, HE, VEC, ISCRAP, SCRAP, NMAX)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    CHARACTERIZE STATIONARY POINT X BY COMPUTING HESSIAN AND
C....    DETERMINING ITS EIGENVALUES
C
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1              W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      DIMENSION X(*), HE(*), VEC(*), ISCRAP(*), SCRAP(*)
C
C....    COMPUTE HESSIAN
C
      CALL HESS (X, XCO, HE)
C
C....    SET UP UNIT MATRIX AS GUESS AT EIGENVECTORS
C
      IJ = 0
      DO 10 I = 1,NVBL
      DO 20 J = 1,NVBL
      IJ = IJ + 1
      VEC(IJ) = D0
      IF (J .EQ. I) VEC(IJ) = D1
20    CONTINUE
10    CONTINUE
      CALL JACO (HE, VEC, NVBL, NVBL, SCRAP, ISCRAP)
C
C....    ORDER EIGENVALUES AND EIGENVECTORS
C
      NVBL1 = NVBL - 1
      II = 0
      INI = -NVBL
      DO 30 I = 1,NVBL1
      II = II + I
      INI = INI + NVBL
      I1 = I + 1
      JJ = II
      JNJ = INI
      DO 40 J = I1,NVBL
      JJ = JJ + J
      JNJ = JNJ + NVBL
      IF (HE(II) .LT. HE(JJ)) GOTO 40
C
C....    SWAP EIGENVALUES
C
      HOLD = HE(II)
      HE(II) = HE(JJ)
      HE(JJ) = HOLD
C
C....    SWAP VECTORS
C
      DO 50 K = 1,NVBL
      HOLD = VEC(INI+K)
      VEC(INI+K) = VEC(JNJ+K)
      VEC(JNJ+K) = HOLD
50    CONTINUE
40    CONTINUE
30    CONTINUE
      ESTAT = EVAL(X,XCO) + EBIAS
      DO 55 K = 1,NVBL
      SCRAP(K) = X(K) + RBIAS(K)
55    CONTINUE
C
C....    TEST CHARACTER OF STATIONARY POINT.  IF ALL EIGENVALUES ARE
C....    POSITIVE, THIS IS A MINIMUM, IF ALL ARE NEGATIVE THIS IS A
C....    MAXIMUM.  IF ONE (ONLY) IS NEGATIVE, THIS IS A SADDLE-POINT.
C....    IT SHOULD BE NOTED THAT IF THE SPACE OF INDEPENDENT VARIABLES
C....    USED CONTAINS ANY ROTATIONS OR TRANSLATIONS THESE WILL GIVE
C....    RISE TO ZERO EIGENVALUES WHICH WILL UPSET THE NEWTON-RAPHSON
C....    STEP.
C
      IMIN = 1
      IMAX = (NVBL*(NVBL+1))/2
      IF (HE(IMAX) .LT. D0) GOTO 80
      IF (HE(IMIN) .GT. D0) GOTO 70
C
C....    AT LEAST ONE NEGATIVE EIGENVALUE
C
      IF (HE(3) .LT. D0) GOTO 60
C
C....    SADDLE-POINT
C
      WRITE(JOBOUT,2000) ESTAT, (SCRAP(K),K = 1,NVBL)
        write(jobout,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT+EBIAS
        write(9904,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT+EBIAS

      NPR = 1
      GOTO 100
60    CONTINUE
C
C....    COMPLICATED STATIONARY POINT
C
      WRITE(JOBOUT,2100) ESTAT, (SCRAP(K),K = 1,NVBL)

        write(jobout,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT-EBIAS
        write(9904,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT-EBIAS
        
      NPR = 0
      GOTO 100
70    CONTINUE
C
C....    MINIMUM
C
      WRITE(JOBOUT,2200) ESTAT, (SCRAP(K),K = 1,NVBL)
!       write(*,*)2200
        write(jobout,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT-EBIAS

        write(9904,'(A16)')'STATIONARY POINT'
        write(9904,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT-EBIAS
        write(9904,'(A11)')'END OF DATA'
        write(9904,'(A4)')'!FIT'
        write(9904,'(A4)')'!END'
        close(9904) 

        write(9906,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT-EBIAS
        close(9906)

      NPR = NVBL
      GOTO 100
80    CONTINUE
C
C....    MAXIMUM
C
      WRITE(JOBOUT,2300) ESTAT, (SCRAP(K),K = 1,NVBL)
        write(*,*)2300
        write(jobout,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT+EBIAS
        write(9904,'(25F20.12)')(SCRAP(K),K = 1,NVBL),ESTAT+EBIAS

      NPR = 0
100   CONTINUE
      IF (IPRINT .GE. 5) NPR = NVBL
      IF (NPR .EQ. 0) RETURN
      WRITE(JOBOUT,2400)
      II = 0
      INI = -NVBL
      DO  110 I = 1,NPR
      II = II + I
      INI = INI + NVBL
      WRITE(JOBOUT,2500) I, HE(II), (VEC(INI+K),K = 1,NVBL)
110   CONTINUE
      RETURN
2000  FORMAT(//,'0',20X,'S A D D L E   P O I N T',//,
     1   ' WHERE ENERGY IS ',F20.12,/,10X,'AT',4X,F14.10,/,(16X,F14.10))
2100  FORMAT(//,'0',6X,'STATIONARY POINT (HESSIAN WITH 2 OR MORE ',
     1                 'NEGATIVE EIGENVALUES)',//,
     2   ' WHERE ENERGY IS ',F20.12,/,10X,'AT',4X,F14.10,/,(16X,F14.10))
2200  FORMAT(//,'0',20X,'M I N I M U M',//,
     1   ' WHERE ENERGY IS ',F20.12,/,10X,'AT',4X,F14.10,/,(16X,F14.10))
2300  FORMAT(//,'0',20X,'M A X I M U M',//,
     1   ' WHERE ENERGY IS ',F20.12,/,10X,'AT',4X,F14.10,/,(16X,F14.10))
2400  FORMAT('0EIGENVALUE(S) OF HESSIAN, STARTING WITH LOWEST')
2500  FORMAT('0EIGENVALUE',4X,I2,4X,E16.8,/,(6X,4(4X,F12.8)))
      END
      SUBROUTINE INLIST
      IMPLICIT REAL*8 (A-H,O-Z)
C
C....    PRINT OUT INPUT DATA
C
C
C....    COMMON STORAGE
C
      COMMON /FILES/ JOBIN, JOBOUT

      COMMON /LIT/ TITLE(11)
      CHARACTER*6 TITLE

      COMMON /PARAMS/ MAXVBL, MAXPTS, MAXUNK

      COMMON /STORE/ IPRINT, NVBL, NPTS, NUNK, R(50,25000), E(25000),
     1             W(25000),IEXPO(50,5000), RBIAS(50), EBIAS,XCO(5000),
     2               XEVAL(50)
      COMMON /CONST/ D0, D1, D2

      CHARACTER*34 FRMT
      CHARACTER*1 JFRMT

      DATA THR/1.D-10/
      DATA NUMKEY /9/
C
C....    PRINT TITLE
C
      WRITE(JOBOUT,2000) TITLE
C
C....    PRINT PRINT LEVEL
C
      WRITE(JOBOUT,2100) IPRINT
C
C....    WRITE NUMBER OF INDEPENDENT VARIABLES
C
      WRITE(JOBOUT,2200) NVBL
C
C....    WRITE DATA POINTS
C
      WRITE(JOBOUT,2300) NPTS
      NVBLA = MIN(NVBL,2)
      WRITE(JFRMT,'(I1)') NVBLA
      FRMT = '(I4,4X,' // JFRMT // '(4X,F10.6),' // 'F20.12,4X,F8.4)'

!       emini=minval(e(1:Npts))

!       emini=EBIAS
!       e(1:Npts)=e(1:Npts)-emini

      DO 515 I = 1,NPTS
!        WRITE(JOBOUT,FRMT) I,(R(J,I),J = 1,NVBLA), E(I), W(I)
      write(jobout,'(i8,12F7.3,F20.12,F8.4)')I,(R(j,I),j=1,NVBL),e(i),
     $w(i)

        IF (NVBL .GT. 2) THEN
!          WRITE(JOBOUT,2450) (R(J,I),J = 3,NVBL)
        ENDIF

515   CONTINUE
C
C....    PRINT NUMBER OF UNKNOWN COEFFICIENTS AND EXPONENTS USED
C
      WRITE(JOBOUT,2500) NUNK
      DO 710 I = 1,NVBL
      WRITE(JOBOUT,2600) I, (IEXPO(I,J),J = 1,NUNK)
710   CONTINUE
C
C....    PRINT GUESS AT STATIONARY POINT
C
      WRITE(JOBOUT,2700) EBIAS

!      WRITE(JOBOUT,2425) (RBIAS(I), I = 1,NVBLA)
!      IF (NVBL .GT. 3) THEN
!        WRITE(JOBOUT,2450) (RBIAS(I), I = 4,NVBL)
!      ENDIF
      write(jobout,'(12F8.3)')(RBIAS(I),I=1,NVBL)

      RETURN
2000  FORMAT(///,1X,70('*'),/,1X,'*',68X,'*',/,
     1       1X,'*',1X,11A6,1X,'*',/,1X,'*',68X,'*',/,1X,70('*'),///)
2100  FORMAT('0    PRINT LEVEL FOR THIS RUN IS ',I4)
2200  FORMAT('0    FIT IN ',I2,' DIMENSIONS')
2300  FORMAT('0    NUMBER OF DATA POINTS ',I4,//,20X,' DATA POINTS')
2400  FORMAT(I4,4X,42X,4X,F12.8,4X,F8.4)
2425  FORMAT('+',7X,1(4X,F14.10))
2450  FORMAT((8X,3(4X,F14.10)))
2500  FORMAT('0    NUMBER OF UNKNOWN COEFFICIENTS ',I4,//,
     1       10X, 'POLYNOMIAL ORDER',/)
2600  FORMAT('     DIMENSION ',I4,6X,(20I4))
2700  FORMAT('0    INITIAL GUESS AT STATIONARY POINT IS ',/
     1       54X,F20.12)
      END


