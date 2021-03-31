       LOGICAL FUNCTION DETPROX( COMPARTYP, DISMAX, NVTORI, NVTOBJ,
     +                           ORITYP,OBJTYP,IH,IDS,JH,JDS)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    FUNCTION :  DETERMINES IF TWO ENTITIES ARE WITHIN
C                PROXIMITY.   CALLED FROM PROXIMITY.
C
C        INPUT:  COMPARTYP  INDICATES ORIGIN AND OBJECT MAP TYPES
C                            TO  COMPARE.  1 THRU 9
C                            1 = POINT TO POINT
C                            2 = POINT TO LINE
C                            3 = POINT TO POLYGON
C                            4 = LINE TO POINT
C                            5 = LINE TO LINE
C                            6 = LINE TO POLYGON
C                            7 = POLYGON TO POINT
C                            8 = POLYGON TO LINE
C                            9 = POLYGON TO POLYGON
C                DISMAX     THE PROXIMITY DISTANCE
C                NVTORI     NUMBER OF VERITICES FOR ORIGIN ENTITY
C                NVTOBJ     NUMBER OF VERITICES FOR OBJECT ENTITY
C                ORITYP     ORIGIN MAP TYPE ( 1, 2 OR 3)
C                OBJTYP     OBJECT MAP TYPE ( 1, 2 OR 3)
C                IH         NUM OF ISLAND FOR ORIGIN ENTITY
C                IDS        TABLE OF INFO FOR ORIGIN ISLANDS
C                JH         NUM OF ISLANDS FOR OBJECT ENTITY
C                JDS        TABLE OF INFO FOR OBJECT ENTITY
C
C        OUTPUT: DETPROX   ( TRUE OR FALSE)
C
C        HISTORY: THIS IS A METAMORPHISED VERSION OF THE PROXIMITY
C                 LOGIC WHICH DETERMINES THE DISTANCES BETWEEN
C                 OBJECTS.
C                 SYNERGIZED FROM  AUTOMETRIC AND TGS ATTEMPS.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        COMMON /IO/    NPRNT,IOIN
C
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
        INTEGER COMPARTYP
        INTEGER NVTORI, NVTOBJ
        INTEGER ORITYP, OBJTYP
        INTEGER IDS(353), JDS(353), IH, JH
        REAL    DISMAX
        DOUBLE PRECISION PP, QQ, XXT, YYT
C
C    FUNCTIONS
        LOGICAL FLINE,FLIN2,FPOINT
        INTEGER PNPOLY,PNPL2
C
C    MAP TYPES
        PARAMETER (POINT = 1)
        PARAMETER (LINE = 2)
        PARAMETER (POLYGON = 3)
C
C********************************************************************
C
C
        GO TO (2100,2200,2300,2400,2500,2600,2700,2600,2600) COMPARTYP
C
C       ----- POINT TO POINT -----
 2100	CONTINUE
	   DETPROX = .FALSE.
           call spoints('X ',x,x1,1,1)
           call spoints('Y ',y,y1,1,1)
           call spoints('P ',p,p1,1,1)
           call spoints('Q ',q,q1,1,1)
           IF (FPOINT(X1, Y1, P1, Q1, DISMAX))DETPROX=.TRUE.
	   GOTO 9000
C
C	----- POINT TO LINE -----
 2200   CONTINUE
	   DETPROX = .FALSE.
           call spoints('X ',x,x1,1,1)
           call spoints('Y ',y,y1,1,1)
	   IF (FLIN2(X1, Y1, NVTOBJ, DISMAX, IQ)) DETPROX = .TRUE.
	   GOTO 9000
C
C	----- POINT TO POLYGON -----
 2300	CONTINUE
	   DETPROX = .TRUE.
           call spoints('X ',x,x1,1,1)
           call spoints('Y ',y,y1,1,1)
	   IF (PNPL2(X1, Y1, NVTOBJ) .EQ. 1) GOTO 9000
	   IF (FLIN2(X1, Y1, NVTOBJ, DISMAX, IQ)) GO TO 9000
	   DETPROX = .FALSE.
	   GOTO 9000
C
C 	----- LINE TO POINT -----
 2400	CONTINUE
	   DETPROX = .FALSE.
           call spoints('P ',p,p1,1,1)
           call spoints('Q ',q,q1,1,1)
	   IF (FLINE(P1, Q1, NVTORI, DISMAX, IQ)) DETPROX = .TRUE.
	   GOTO 9000
C
C	----- LINE TO LINE -----
 2500	CONTINUE
	   DETPROX = .TRUE.
	   DO 2550 LK = 1, NVTORI
              call spoints('X ',x,xlk,lk,1)
              call spoints('Y ',y,ylk,lk,1)
	      IF (FLIN2(XLK, YLK, NVTOBJ, DISMAX, IQ)) GO TO 9000
 2550	   CONTINUE
	   DO 2560 LK = 1, NVTOBJ
              call spoints('P ',p,plk,lk,1)
              call spoints('Q ',q,qlk,lk,1)
	      IF (FLINE(PLK, QLK, NVTORI, DISMAX, IQ)) GO TO 9000
 2560	   CONTINUE
	   DETPROX = .FALSE.
	   GOTO 9000
C
C	--- LINE TO POLYGON, POLYGON TO LINE, POLYGON TO POLYGON --
 2600	CONTINUE
	   DETPROX = .TRUE.
	   DO 2650 LK = 1, NVTOBJ
              call spoints('P ',p,plk,lk,1)
              call spoints('Q ',q,qlk,lk,1)
	      IF (FLINE(PLK, QLK, NVTORI, DISMAX, IQ)) GO TO 9000
 2650	   CONTINUE
	   DO 2660 LK = 1, NVTORI
              call spoints('X ',x,xlk,lk,1)
              call spoints('Y ',y,ylk,lk,1)
	      IF (FLIN2(XLK, YLK, NVTOBJ, DISMAX, IQ)) GO TO 9000
 2660	   CONTINUE
C
C..........CHECK EACH POINT IN THE OBJECT TO SEE IF IT'S IN THE POLYGON
	   IF (ORITYP .NE. POLYGON) GO TO 2670
              DO 2662 LK = 1,NVTOBJ
                  call spoints('P ',p,plk,lk,1)
                  call spoints('Q ',q,qlk,lk,1)
                  PP = DBLE(PLK)
                  QQ = DBLE(QLK)
                  IND = 0
                  CALL PIP2(PP, QQ, IND, NVTORI, IH, IDS)
	          IF (IND .EQ. 1) GOTO 9000
 2662         CONTINUE
           DETPROX = .FALSE.
           GO TO 9000
C
C..........CHECK EACH ITEM IN THE ORIGIN TO SEE IF IT'S IN THE POLYGON
 2670      DO 2672 LK = 1,NVTORI
              call spoints('X ',x,xlk,lk,1)
              call spoints('Y ',y,ylk,lk,1)
              XXT = DBLE(XLK)
              YYT = DBLE(YLK)
              IND = 0
              CALL PIP1(XXT, YYT, IND, NVTOBJ, JH, JDS)
	      IF (IND .EQ. 1) GOTO 9000
 2672      CONTINUE
	   DETPROX = .FALSE.
	   GOTO 9000
C
C 	----- POLYGON TO POINT -----
 2700	CONTINUE
           DETPROX = .TRUE.
           call spoints('P ',p,p1,1,1)
           call spoints('Q ',q,q1,1,1)
           IF (PNPOLY(P1, Q1, NVTORI) .EQ. 1) GOTO 9000
           IF (FLINE(P1,Q1,NVTORI,DISMAX,IQ)) GOTO 9000
           DETPROX = .FALSE.
C
C
 9000   CONTINUE
C
C
        RETURN
        END
