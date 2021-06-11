C*********************************************************************
C
C     NAME:  PROGRAM COMPUTE
C
C     FUNCTION:  PARSER FOR COMPUTE COMMAND WHICH CREATES AN ATTRIBUTE FIELD
C                BY COMBINING TWO OR MORE INTEGER OR REAL ATTRIBUTES
C                AND NUMERIC VALUES USING MATHEMATICAL OPERATIONS AND
C                MATHEMATICAL FUNCTIONS.
C**********************************************************************
C
       COMMON /IO/     NPRNT,IOIN
       COMMON /FLS/    IUNIT,IFILE(5)
$Include:'strng.inc'
       COMMON /WHMAP/  ISTRCT(300),NSTRCT
       COMMON /TYPE/   IDATYP(40),NACTS
$Include:'mfast.inc'
$Include:'dbchan.inc'
       COMMON /MAXS/   MX(16),RMAX
       COMMON /DSBUF/  MLEN,IDATA(8192)
       COMMON /STRING/ INPUT(80),KI
       COMMON /WORK/   IBUFF(128),MAPP(25),KEY(10),JCOM(10)
       common/ unit/ chanel,atrbte,polygon,adress
       INTEGER IWORD(10),W,SUBNO,MAPNO,IER,IWM,chanel,atrbte,polygon,
     1         NAME(15),LENA,MBUFF(128),
     2         IMAP(64),IM,ISTAK(2,128),JSTAK(2,128),IS,NSYMB,IPREV,NPAR,
     3         MVFLAG,adress
       INTEGER SUBLET(120),NEWREC(128),JATTR(13,200)
       LOGICAL MCOMP,MCOMPH,ONEATT,PROMPT
       REAL RMAP(64),RNUM
C......COMMON MAXIMUMS ......
C       DATA MX   /2,16,8,18,10,16,32767,64,0,1024,0,0,0,0,0,256/
C     + ,RMAX/9999999./
C......COMMON PHRASES .....
       DATA SUBLET/2HFO,2HR ,2H!,,2H!(,2H !,2H)!,2H+ ,2H!-,2H !,2H* ,
     1 2H!/,2H!*,2H*!,2HMA,2HXI,2H !,2HMI,2HNI,2H !,2HCO,2HVE,2H !,
     2 2HAV,2HER,2H !,2HSQ,2HRT,2H !,2HLO,2HG ,2H!N,2HLO,2HG ,
     3 2H!I,2HNT,2H !,2HAN,2HIN,2HT ,2H!A,2HBS,2H !,2HTA,2HN ,
     4 2H!S,2HIN,2H !,2HCO,2HS ,2H!A,2HTA,2HN ,2H!A,2HSI,2HN ,
     5 2H!A,2HCO,2HS ,2H!@,61*0/
C
C......INITIALIZE VARIABLES ......
C
       LINES = 0
       IS = 0
       IM = 0
       IWM = 1
       NPAR = 0
       MAPNO = 0
       MVFLAG = 0
       ONEATT = .FALSE.
       PROMPT = .FALSE.
       MLEN = 8192
       ICHMAP = 20
       ICHATT = 31
       ICHOUT = 22
       W = 1
       chanel=ichatt
       call unpack(sublet,120)
C
C......READ IN COMMON BLOCKS ......
       CALL initl(ier)
       if(ier.ne.0)stop
C
C......GET THE ACTIVE MAP ID ......
       CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,LENA,IER)
       IF (IER .NE. 0) GO TO 80060
C
       IF (LENA .LT. 1  .OR. RNUM .EQ. 0.0) PROMPT = .TRUE.
       IF (PROMPT) W = 0
       IF (PROMPT) WRITE(NPRNT,90010)
90010  FORMAT(' ENTER ACTIVE MAP ID TO COMPUTE'\)
       IF (PROMPT) CALL READIN(ICARD,lenicd,icardp,istop)
       if(icard(1).eq.0) go to 90000
       IF (PROMPT)CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,
     +  LENA,IER)
       IF (IER .NE. 0) GO TO 80060
C
       IDD = ((INT(RNUM)-1)*7)+1
       IF (IDD .LT. 0  .OR.  IDD .GT. NSTRCT)  GO TO 80000
C
C......MAKE SURE SOME FEATURES ARE SELECTED FOR THE ACTIVE ID .....
       CALL APGET(IDD,I,J,N)
       IF (N .LT. 1) GO TO 80010
C
C......SEE IF ITS A VECTOR MAP ......
       IDTY = IDATYP(IDD/7 + 1)
       IF (IDTY .gt.3)go to 80020
C
C......GET THE MAP NAME AND OPEN THE MAP FILE ......
       CALL GTNAM(IDD,0,2h  ,NAME,IER)
CC       CALL DSOPN(ICHMAP,NAME,1,1,1,128,2048,IER)
       call openf(ichmap,name,1,256,2,ier)
       IF (IER .NE. 0) GO TO 85020
C
C......READ THE HEADER AND SEE IF THE MAP HAS MULTIPLE ATTRIBUTES ......
CC       CALL DSIN(ICHMAP,MBUFF,1.0,IER)
       call rdblk(ichmap,1,mbuff,1,ier)
       IF (IER .NE. 0) GO TO 85040
       IF (MBUFF(76) .NE. 1) GO TO 80030
       NFEATS = MBUFF(63)
C
C......GET THE ATTRIBUTE NAME AND OPEN THE ATTRIBUTE FILE ......
       CALL GTNAM(IDD,4,2h  ,NAME,IER)
CC       CALL DSOPN(ICHATT,NAME,1,1,1,128,2048,IER)
       call openf(ichatt,name,1,256,2,ier)
       IF (IER .NE. 0) GO TO 85020
C
C......READ THE NUMBER OF ATTRIBUTES AND SEE IF ATT FILE IS FULL ......
CC       CALL DSIN(ICHATT,MBUFF,1.0,IER)
       call initial
       NUMATT = atrbte
       IF (NUMATT .LT. 1  .OR.  NUMATT .GE. 200) GO TO 80040
C
C......MAKE SURE WE CAN WRITE TO THE ATTRIBUTE FILE IN CASE OF WRITE PROTECT..
C       CALL DSOUT(ICHATT,MBUFF,1.0,IER)
C       IF (IER .NE. 1) GO TO 85060
C       CALL DSIN (ICHATT,MBUFF,203.0,IER)
C       IF (IER .NE. 1) GO TO 85040
C       CALL DSOUT (ICHATT,MBUFF,203.0,IER)
C       IF (IER .NE. 1) GO TO 85060
C
C......OPEN THE ACTIVE MAP FEATURE RECORD FILE ......
C
       CALL OPENF (icdcfa,idesfa,1,512,2,IER)
       IF (IER .NE. 0) GO TO 85020
C
C......LOAD THE ATTRIBUTE NAMES, TYPE, LENGTH, AND FIRST RECORD POINTER ......
       DO 56000 I = 1,NUMATT
            CALL rdblk(ICHATT,I+1,MBUFF,1,IER)
            IF (IER .NE. 0) GO TO 85040
C            CALL PACKC(MBUFF(2),JATTR(1,I),10)
            do 2 l=1,10
    2       jattr(l,I)=mbuff(l+1)
            JATTR(11,I) = MBUFF(84)
            JATTR(12,I) = MBUFF(85)
            JATTR(13,I) = MBUFF(86)
56000       CONTINUE
C
C......ZERO-FILL MAP INFORMATION ARRAY ......
       DO 56010  I=1,MX(8)
            IMAP(I) = 0
            RMAP(I) = 0.0
56010       CONTINUE
C
C......ZERO-FILL STACK ARRAY ......
       DO 56020  I=1,MX(8)*2
            ISTAK(1,I) = 0
            ISTAK(2,I) = 0
56020       CONTINUE
C
C
C......SEE IF THEY ONLY PUT IN AN ID .....
       J = icardp
       CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,LENA,IER)
       IF (IER .NE. 0) GO TO 80060
       icardp = J
       IF (LENA .LT. 1) PROMPT = .TRUE.
C
C
C......SET-UP NEW ATTRIBUTE RECORD ......
       CALL CLEAR(NEWREC,128)
       NEWREC(1) = NUMATT + 1
       IF (.NOT.(PROMPT)) GO TO 1090
 1005  WRITE(NPRNT,1010)
 1010  FORMAT(' PLEASE ENTER A NEW KEY (MAX 10 CHARACTERS) ',\)
       CALL RECEIVE(KEY,10)
       NEWLEN = KI
C
C......CHECK FOR THE NULL STRING (A BLANK RETURN).  THIS WILL NOT DO.
C
       IF (KI .GT. 0) GO TO 1020
       WRITE(NPRNT,1011)
 1011  FORMAT(' THE KEY MUST BE AT LEAST ONE CHARACTER LONG',/)
       GO TO 1005
C
C......NOW LOOP THROUGH ALL OF THE KEYS AND CHECK FOR DUPLICATES
C
 1020  DO 1040 II = 2,NUMATT + 1
          CALL rdblk(ICHATT,II,IBUFF,1,IER)
          IF (IER .NE. 0) GO TO 85040
          DO 1030 III = 1,10
             IF (KEY(III) .NE. IBUFF(III+1)) GO TO 1040
 1030     CONTINUE
          GO TO 1050
 1040  CONTINUE
C
C......IF WE ARE HERE (AND NOT AT 1050) THEN KEY IS OK (NOT DUP.)
C
       GO TO 1060
C
C......IF WE ARE HERE AT 1050 THEN KEY WAS DUPED
C
 1050  WRITE(NPRNT,1015) (KEY(II),II=1,10)
 1015  FORMAT(' KEY ',10A2,' ALREADY EXISTS.  PLEASE TRY AGAIN',/)
       GO TO 1005
C
 1060  DO 1070 II = 1,10
          NEWREC(II+1) = KEY(II)
 1070  CONTINUE
       GO TO 90909
C
C......SET NEW ATTRIBUTE NAME TO DEFAULT OF 'THATATTR'.....
 1090  NEWREC(2) = ichar('T')
       NEWREC(3) = ichar('H')
       NEWREC(4) = ichar('A')
       NEWREC(5) = ichar('T')
       NEWREC(6) = ichar('A')
       NEWREC(7) = ichar('T')
       NEWREC(8) = ichar('T')
       NEWREC(9) = ichar('R')
       NEWLEN = 8
C
C......PROMPT THEM IF THEY ONLY PUT IN AN ID .....
90909  IF (PROMPT) WRITE(NPRNT,95604)
95604  FORMAT(' ENTER THE COMPUTATION EXPRESSION'\)
       IF (PROMPT) CALL READIN(ICARD,lenicd,icardp,istop)
       IF (PROMPT) LINES = LINES + 2
       IF (PROMPT) W = 0
       if(prompt)icardp=0
C......MOVE COMPUTATION INTO DESCRIPTION .....
       J = 12
       DO 56030 I=icardp+1,72
            IF(ICARD(I).EQ.0)GO TO 56031
             NEWREC(J) = ICARD(I)
             J = J + 1
56030        CONTINUE
56031      IHOLDJ=J-1
C......SET TYPE,LENGTH,FIRST RECORD, AND NUMBER OF RECORDS FOR NEW ATTR .....
       NEWREC(84) = 2
       NEWREC(85) = 2
       NEWREC(86) = MBUFF(86) + MBUFF(87)
       NEWREC(87) = ( (NFEATS-1) / (128/NEWREC(85)) ) + 1
56040  CONTINUE
C......RETRIEVE NEXT WORD FROM COMMAND LINE & MATCH WITH LEGAL PHRASES ......
       CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,LENA,IER)
       SUBNO = SUBNO + 1
       IF (IER .NE. 0) GO TO 80060
C
C......JUMP TO PROCESS PHRASE ......
56050  CONTINUE
C......        ATTR  FOR   ,     (     )     +     -     *     /   .......
C......        **    MAXI  MINI  COVE  AVER  SQRT  LOG   NLOG  INT .......
C......        ANINT  ABS  TAN   SIN   COS   ATAN  ASIN  ACOS  @   .......
       GO TO (56100,56200,56300,56400,56500,56600,56600,56600,56600,
     1        56600,56600,56600,56600,56600,56700,56700,56700,56700,
     2        56700,56700,56700,56700,56700,56700,56700,56700,56800),
     3        SUBNO
C
56100  CONTINUE
C...........ATT-NAME OR VALUE ......
            IPREV = 1
C...........CHECK FOR FIRST PHRASE .......
            IF (IS .GE. 1)  GO TO 56150
C................CHECK FOR A NUMERIC VALUE ......
                 IF (RNUM .EQ. 0.0)  GO TO 56450
                      MVFLAG = 2
                      GO TO 56150
56450            CONTINUE
C................SEARCH FOR MAP KEYS (AREA,LENGTH,PERIMETER) ......
                 IWM = 0
                 IF (MCOMP(IWORD,'AREA',4,IER)) IWM = -(1)
                 IF (MCOMP(IWORD,'LENG',4,IER)) IWM = -(2)
                 IF (MCOMP(IWORD,'PERI',4,IER)) IWM = -(3)
                 IF (IWM .NE. 0)  GO TO 56460
C................SEARCH FOR THE ATTRIBUTE NAMES ......
                 DO 56155 IWM=1,NUMATT
C                      IF (.NOT. MCOMPH(IWORD,JATTR(1,IWM),LENA,IER))
C     +                  GO TO 56155
                       do 3 l=1,lena
                       if(iword(l).ne.jattr(l,iwm))go to 56155
    3                  continue
C.....................FOUND THE MULTIPLE ATTRIBUTE...IT MUST BE INTEGER OR REAL
                      IF (JATTR(11,IWM) .NE. 1.AND.JATTR(11,IWM) .NE. 2)
     1                     GO TO 80080
                           GO TO 56460
56155                 CONTINUE
                 GO TO 81000
56460            CONTINUE
C................SET THE FLAG FOR NAME NOT NUMBER ......
                 MVFLAG = 1
                 GO TO 56150
C
56150       CONTINUE
C...........INCREMENT MAP-STACK INDEX, CHECK FOR MAXIMUM ......
            IM = IM + 1
            IF (IM .GT. MX(8))  GO TO 80240
C...........CHECK MAP/VALUE FLAG AND PUSH VALUE OR FLAG,NUMBER ONTO STACK ...
            IF (MVFLAG .EQ. 1) IMAP(IM) = IWM
            IF (MVFLAG .EQ. 2) RMAP(IM) = RNUM
C...........SET FLAG TO INDICATE MAP ON STACK ......
            IF (MVFLAG .EQ. 1) ONEATT = .TRUE.
C
C...........PUSH MAP/VALUE INDEX & FLAG ONTO STACK ......
            IS = IS + 1
            IF (IS .GT. (MX(8)*2))  GO TO 80260
            ISTAK(1,IS) = IM
C...........FLAG INDICATES PRIORITY & IF ENTRY IS MAP OR VALUE ......
            ISTAK(2,IS) = MVFLAG
C
56180       CONTINUE
C...........RETRIEVE NEXT WORD FROM COMMAND LINE & MATCH WITH LEGAL PHRASES ...
            CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,LENA,IER)
            SUBNO = SUBNO + 1
            IF (IER .NE. 0) GO TO 80060
C...........CHECK FOR ILLEGAL PHRASES ......
            IF ((SUBNO .EQ. 1) .OR. (SUBNO .EQ. 4) .OR.
     1         ((SUBNO .GE. 15) .AND. (SUBNO .LE. 26)))  GO TO 81100
            GO TO 56050
C
56200  CONTINUE
C...........FOR - SPECIFIES NAME OF NEWLY CREATED MAP .....
            IPREV = 2
            CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,NEWREC(2),RNUM,
     +       LENA,IER)
            IF (IER .NE. 0) GO TO 80060
            NEWLEN = LENA
            GO TO 56040
C
56300  CONTINUE
C..........., - CONTINUE COMMAND LINE ......
            IPREV = 3
            WRITE(*,56301)
56301 FORMAT(1X,'CONTINUE EXPRESSION ',\)
            CALL READIN (ICARD,lenicd,icardp,istop)
C...........SET WORD COUNTER TO BEGINNING AND COUNT MORE LINES .....
            W = 0
            LINES = LINES + 2
C...........CHECK FOR ABANDON INDICATOR .....
            IF (MCOMP(ICARD,'BACK',4,IER))  GO TO 90000
C...........RETURN TO PREVIOUS PHRASE PROCESSING ......
            J=IHOLDJ
            DO 56302 IL=1,72
            IF(ICARD(IL).EQ.0)GO TO 56303
            NEWREC(J)=ICARD(IL)
            J=J+1
56302       CONTINUE
56303       IHOLDJ=J-1
            GO TO 56040
C
56400  CONTINUE
C...........( - LEFT PARENS .......
            IPREV = 4
C...........PUSH PARENS ONTO STACK ......
            IS = IS + 1
            IF (IS .GT. (MX(8)*2))  GO TO 80260
            ISTAK(1,IS) = -1
            ISTAK(2,IS) = -1
C...........INCREMENT PARENS COUNTER ......
            NPAR = NPAR + 1
56420       CONTINUE
C...........RETRIEVE NEXT WORD FROM COMMAND LINE & MATCH WITH LEGAL PHRASES ...
            CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,LENA,IER)
            SUBNO = SUBNO + 1
            IF (IER .NE. 0) GO TO 80060
C...........CHECK FOR LEGAL PHRASE ......
            IF (SUBNO .EQ. 27)  GO TO 81020
            IF ((SUBNO .EQ.4).OR.((SUBNO.GE.15).AND. (SUBNO .LE. 26)))
     1      GO TO 56050
C................CHECK FOR A NUMERIC VALUE ......
                 IF (RNUM .EQ. 0.0)  GO TO 56130
                      MVFLAG = 2
                      GO TO 56050
56130            CONTINUE
C................SEARCH FOR MAP KEYS (AREA,LENGTH,PERIMETER) ......
                 IWM = 0
                 IF (MCOMP(IWORD,'AREA',4,IER)) IWM = -(1)
                 IF (MCOMP(IWORD,'LENG',4,IER)) IWM = -(2)
                 IF (MCOMP(IWORD,'PERI',4,IER)) IWM = -(3)
                 IF (IWM .NE. 0)  GO TO 56140
C................SEARCH FOR THE ATTRIBUTE NAMES ......
                 DO 56135 IWM=1,NUMATT
C                      IF (.NOT. MCOMPH(IWORD,JATTR(1,IWM),LENA,IER))
C     +                  GO TO 56135
                      do 4 l=1,lena
                      if(iword(l).ne.jattr(l,iwm))go to 56135
    4                 continue
C.....................FOUND THE MULTIPLE ATTRIBUTE...IT MUST BE INTEGER OR REAL
                      IF (JATTR(11,IWM) .NE. 1.AND.JATTR(11,IWM) .NE. 2)
     1                     GO TO 80080
                           GO TO 56140
56135                 CONTINUE
                 GO TO 81400
56140            CONTINUE
C................SET THE FLAG FOR NAME NOT NUMBER ......
                 MVFLAG = 1
                 GO TO 56150
C
56500  CONTINUE
C...........) - RIGHT PARENS .....
            IPREV = 5
C...........CHECK FOR FIRST PHRASE ......
            IF (IS .LT. 1)  GO TO 81000
C...........PUSH PARENS ONTO STACK ......
            IS = IS + 1
            IF (IS .GT. (MX(8)*2))  GO TO 80260
            ISTAK(1,IS) = -2
            ISTAK(2,IS) = -2
C...........DECREMENT PARENS COUNTER ......
            NPAR = NPAR - 1
56520       CONTINUE
C...........RETRIEVE NEXT WORD FROM COMMAND LINE & MATCH WITH LEGAL PHRASES ...
            CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,LENA,IER)
            SUBNO = SUBNO + 1
            IF (IER .NE. 0) GO TO 80060
C...........CHECK FOR ILLEGAL PHRASE ......
            IF ((SUBNO .EQ. 1) .OR. (SUBNO .EQ. 4) .OR.
     1         ((SUBNO .GE. 15) .AND. (SUBNO .LE. 26)))  GO TO 81500
            GO TO 56050
C
56600  CONTINUE
C...........+ - * / ** MAXI MINI COVE AVER - SPECIFIES THE OPERATOR ......
            IPREV = 6
C...........CHECK FOR FIRST PHRASE ......
            IF (IS .LT. 1)  GO TO 81000
C...........PUSH OPERATOR ONTO STACK ......
            IS = IS + 1
            IF (IS .GT. (MX(8)*2))  GO TO 80260
            ISTAK(1,IS) =  - (SUBNO-3)
C...........ASSIGN PRIORITY -3 TO + - MAXI MINI COVE AVER ......
            ISTAK(2,IS) = -3
C...........ASSIGN PRIORITY -4 TO * / .......
            IF ((SUBNO .EQ. 8) .OR. (SUBNO .EQ. 9))  ISTAK(2,IS) = -4
C...........ASSIGN PRIORITY -5 TO ** .......
            IF (SUBNO .EQ. 10)  ISTAK(2,IS) = -5
56620       CONTINUE
C...........RETRIEVE NEXT WORD FROM COMMAND LINE & MATCH WITH LEGAL PHRASES ...
            CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,LENA,IER)
            SUBNO = SUBNO + 1
            IF (IER .NE. 0) GO TO 80060
C...........CHECK FOR LEGAL PHRASE ......
            IF (SUBNO .EQ. 27)  GO TO 81020
            IF ((SUBNO .EQ. 3) .OR. (SUBNO .EQ. 4) .OR. (SUBNO .GE.15))
     1      GO TO 56050
C................CHECK FOR A NUMERIC VALUE ......
                 IF (RNUM .EQ. 0.0)  GO TO 56650
                      MVFLAG = 2
                      GO TO 56050
56650            CONTINUE
C................SEARCH FOR MAP KEYS (AREA,LENGTH,PERIMETER) ......
                 IWM = 0
                 IF (MCOMP(IWORD,'AREA',4,IER)) IWM = -(1)
                 IF (MCOMP(IWORD,'LENG',4,IER)) IWM = -(2)
                 IF (MCOMP(IWORD,'PERI',4,IER)) IWM = -(3)
                 IF (IWM .NE. 0)  GO TO 56660
C................SEARCH FOR THE ATTRIBUTE NAMES ......
                 DO 56655 IWM=1,NUMATT
C                      IF (.NOT. MCOMPH(IWORD,JATTR(1,IWM),LENA,IER))
C     +                  GO TO 56655
                       do 8 l=1,lena
                       if(iword(l).ne.jattr(l,iwm))go to 56655
    8                  continue
C.....................FOUND THE MULTIPLE ATTRIBUTE...IT MUST BE INTEGER OR REAL
                      IF (JATTR(11,IWM) .NE. 1  .AND.JATTR(11,IWM).NE.2)
     1                     GO TO 80080
                           GO TO 56660
56655                 CONTINUE
                 GO TO 81600
56660            CONTINUE
C................SET THE FLAG FOR NAME NOT NUMBER ......
                 MVFLAG = 1
                 GO TO 56050
C
56700  CONTINUE
C...........FUNCTION - SQRT,LOG,NLOG,INT,ANINT,ABS,SIN,COS,TAN,ASIN,ACOS,ATAN
            IPREV = 7
C...........PUSH FUNCTION & PRIORITY ONTO STACK ......
            IS = IS + 1
            IF (IS .GT. (MX(8)*2))  GO TO 80260
            ISTAK(1,IS) = - (SUBNO-3)
            ISTAK(2,IS) = -6
56720       CONTINUE
C...........RETRIEVE NEXT WORD FROM COMMAND LINE & MATCH WITH LEGAL PHRASES ...
            CALL COMPM(SUBLET,ICARD,icardp,W,SUBNO,IWORD,RNUM,LENA,IER)
            SUBNO = SUBNO + 1
            IF (IER .NE. 0) GO TO 80060
C...........CHECK FOR LEGAL PHRASE ......
            IF (SUBNO .EQ. 27)  GO TO 81020
            IF (SUBNO .NE. 4)  GO TO 81700
            GO TO 56050

56800  CONTINUE
C...........END OF COMMAND LINE ......
C...........CHECK FOR UNMATCHED PARENTHESIS ......
            IF (NPAR .NE. 0)  GO TO 80200
C...........CHECK FOR EMPTY STACK OF MAPS ......
            IF (.NOT. ONEATT)  GO TO 80220
C...........SEARCH FILE FOR NEWNAME......
                 DO 56820 IWM=1,NUMATT
C                      IF (MCOMPH(NEWREC(2),JATTR(1,IWM),NEWLEN,IER))
C     +                  GO TO 84980
                       do 9 l=1,newlen
                       if(newrec(1+l).ne.jattr(l,iwm))go to 56820
    9                  continue
                       go to 84980
56820                 CONTINUE
C...........WRITE THE NEW ATTRIBUTE HEADER RECORD ......
            CALL wrblk (ICHATT,newrec(1)+1,NEWREC,1,IER)
            IF (IER .NE. 0)  GO TO 85060
C
C
C...........TRANSLATE MATHEMATICAL EXPRESSION INTO REVERSE POLISH NOTATION.....
            CALL COMPRN (ISTAK,JSTAK,IS,NSYMB,IER)
            IF (IER .NE. 0)  GO TO 85120
C
C...........PROCESS MATH OPERATIONS ......
            CALL COMPP (IDD,ICHOUT,ICHMAP,ICHATT,NEWREC,JATTR,NFEATS,
     1      NUMATT,ISTAK,NSYMB,IMAP,RMAP,IER)
            IF (IER .NE. 0)  GO TO 86000
C
C...........CLOSE UP FILES ......
            close(ICHATT,iostat=IER)
            IF (IER .NE. 0) GO TO 85080
            close(ICHMAP,iostat=IER)
            IF (IER .NE. 0) GO TO 85080
            CLOSE(icdcfa,iostat=IER)
C
C...........WRITE FINAL MESSAGE .......
            WRITE(NPRNT,95680)  NEWREC(1),(NEWREC(I),I=2,11)
95680       FORMAT(/,' OK  COMPUTED FOR ATTRIBUTE ',I3,1X,10A2)
            LINES = LINES + 2
            GO TO 90000
C
C......ERROR RETURNS ......
C
80000  CONTINUE
       IER = -5600
       WRITE (NPRNT,98000)  IDD/7,IER
98000  FORMAT (' *COMPU*  ILLEGAL ACTIVE MAP ID',I5,'  ERROR',I5)
       GO TO 89000
C
80010  CONTINUE
       IER = -5601
       WRITE (NPRNT,98001)  IER
98001  FORMAT (' *COMPU*  NO FEATURES SELECTED FOR ACTIVE ID-ERROR',I5)
       GO TO 89000
C
80020  CONTINUE
       IER = -5602
       WRITE (NPRNT,98002)  IDTY,IER
98002  FORMAT (' *COMPU*  CANNOT COMPUTE WITH MAPS OF TYPE ',I5,/,
     1         '        VALID TYPES FOR COMPUTE ARE  1 2 3  ERROR',I5)
       GO TO 89000
C
80030  CONTINUE
       IER = -5603
       WRITE (NPRNT,98003) IER
98003  FORMAT (' *COMPU*  MAP DOES NOT HAVE MULTIPLE ATTRIBUTES-ERROR',
     +  I5)
       GO TO 89000
C
80040  CONTINUE
       IER = -5604
       WRITE (NPRNT,98004)  NUMATT,IER
98004  FORMAT (' *COMPU*  TOO MANY ATTRIBUTES',I5,
     +  ' TO ADD ANOTHER  ERROR',I5)
       GO TO 89000
C
80060  CONTINUE
       WRITE (NPRNT,98006)  IER
98006  FORMAT (' *COMPU*  ERROR FROM COMPM  ERROR',I5)
       GO TO 89000
C
80080  CONTINUE
       IER = -5608
       WRITE (NPRNT,98009)  IER
98009  FORMAT (' *COMPU*CAN ONLY USE INTEGER OR REAL ATTRIBUTES-ERROR',
     +  I5)
       GO TO 89000
C
80200  CONTINUE
       IER = -5604
       WRITE (NPRNT,98020)  IER
98020  FORMAT (' *COMPU*  UNMATCHED PARENTHESIS IN COMMAND  ERROR',I5)
       GO TO 90000
C
80220  CONTINUE
       IER = -5606
       WRITE (NPRNT,98022)  IER
98022  FORMAT (' *COMPU*  COMMAND REQUIRES SPECIFYING AN',
     +  ' ATTRIBUTE-ERROR',I5)
       GO TO 90000
C
80240  CONTINUE
       IER = -5608
       WRITE (NPRNT,98024)  MX(8),IER
98024  FORMAT (' *COMPU*NUMBER OF ATTRIBUTES OR VALUES EXCEEDS MAXIMUM',
     1         ' OF',I5,'  ERROR',I5)
       GO TO 90000
C
80260  CONTINUE
       IER = -5610
       WRITE (NPRNT,98026)  (MX(8)*2),IER
98026  FORMAT (' *COMPU*  NUMBER OF PHRASES EXCEEDS MAXIMUM OF',I5,
     1         '  ERROR',I5)
       GO TO 90000
C
81000  CONTINUE
       IER = -5612
       WRITE (NPRNT,98100)  IER
98100  FORMAT (' *COMPU*  LEGAL BEGINNING PHRASES FOR COMPUTE ARE:  ',/,
     1 '          FOR  ,  ATTRIBUTE  VALUE  (  FUNCTION   ERROR',I5)
       GO TO 89000
C
81020  CONTINUE
       IER = -5614
       WRITE (NPRNT,98102)  IER
98102  FORMAT (' *COMPU*  LEGAL ENDING PHRASES FOR COMPUTE ARE:  ',/,
     1 '          FOR  ,  ATTRIBUTE  VALUE  )   ERROR',I5)
       GO TO 89000
C
81100  CONTINUE
       IER = -5615
       WRITE (NPRNT,98110)  IER
98110 FORMAT (' *COMPU*LEGAL PHRASES FOLLOWING ATTRIBUTE OR VALUE ARE:',
     1       /,'          FOR  ,  )  OPERATION  ERROR',I5)
       GO TO 89000
C
81400  CONTINUE
       IER = -5618
       WRITE (NPRNT,98140)  IER
98140  FORMAT (' *COMPU*  LEGAL PHRASES FOLLOWING  (  ARE: ',/,
     1 '          ,  ATTRIBUTE  VALUE  (  FUNCTION   ERROR',I5)
       GO TO 89000
C
81500  CONTINUE
       IER = -5620
       WRITE (NPRNT,98150)  IER
98150  FORMAT (' *COMPU*  LEGAL PHRASES FOLLOWING  )  ARE: ',/,
     1 '          FOR  ,  )  OPERATION  ERROR',I5)
       GO TO 89000
C
81600  CONTINUE
       IER = -5622
       WRITE (NPRNT,98160)  IER
98160  FORMAT (' *COMPU*  LEGAL PHRASES FOLLOWING  AN OPERATION  ARE: ',
     1 /,'          ,  ATTRIBUTE  VALUE  (  FUNCTION   ERROR',I5)
       GO TO 89000
C
81700  CONTINUE
       IER = -5624
       WRITE (NPRNT,98170)  IER
98170  FORMAT (' *COMPU*  LEGAL PHRASES FOLLOWING A FUNCTION  ARE: ',/,
     1 '          ,  (    ERROR',I5)
       GO TO 89000
C
84980  CONTINUE
       IER = -5626
       WRITE (NPRNT,98498)  IER
98498  FORMAT (' *COMPU* THE NAME FOR THE NEW ATTRIBUTE ALREADY EXISTS',
     1  'ERROR',I5)
       GO TO 90000
C
85020  CONTINUE
       WRITE (NPRNT,98502) (NAME(I),I=1,10),IER
98502  FORMAT (' *COMPU*  ERROR FROM <OPENF> OF ',10A2,'  ERROR',I5)
       GO TO 90000
C
85040  CONTINUE
       WRITE (NPRNT,98504) IER
98504  FORMAT (' *COMPU*  ERROR FROM <readblk>  ERROR',I5)
       GO TO 90000
C
85060  CONTINUE
       WRITE (NPRNT,98506) IER
98506  FORMAT (' *COMPU*  ERROR WRITING TO ATTRIBUTE FILE  ERROR',I5)
       GO TO 90000
C
85080  CONTINUE
       WRITE (NPRNT,98508) IER
98508  FORMAT (' *COMPU*  ERROR FROM DSCLS  ERROR',I5)
       GO TO 90000
C
85120  CONTINUE
       WRITE (NPRNT,98512)  IER
98512  FORMAT (' *COMPU*  ERROR FROM COMPRN  ERROR',I5)
       GO TO 90000
C
86000  CONTINUE
       WRITE (NPRNT,98600)  IER
98600  FORMAT (' *COMPU*  ERROR FROM COMPPR  ERROR',I5)
       GO TO 90000
C
89000  CONTINUE
       WRITE (NPRNT,99000)  W,(IWORD(I),I=1,MX(5))
99000  FORMAT (' *COMPU*  ERROR OCCURRED AT WORD',I5,1X,10A2)
       LINES = LINES + 3
       icardp = 0
       ICARD(1) = 0
       write(*,*)
       write(*,*)'Try again - Please'
       GO TO 90909
C
C......NORMAL RETURN ......
C
90000  CONTINUE
       LINES = LINES + 1
       RETURN
       END
