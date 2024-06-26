      SUBROUTINE SELEXT(IBUFF,NHITS,ICARD,ICARDP,LINES,IER)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C           S U B R O U T I N E    S E L E X T
C
C  FUNCTION:  THIS ROUTINE ALLOWS THE USER TO SPECIFY
C             ALTERNATIVE INPUT FILES OF MAP NAMES, ACTIONS,
C             AND SEARCH CRITERIA.  THIS ALLOWS THE USER TO
C             SELECT MAP DATA FROM A LARGE NUMBER OF MAPS IN
C             ONE PASS!
C
C  INPUT PARAMETERS:
C
C             IBUFF  =  128 WORD WORK ARRAY
C             NHITS  =  RETURNED TOTAL NUMBER OF HITS
C             ICARD  =  80 CHARACTER  WORK SPACE
C             ICARDP =  CHARACTER POINTER INTO ICARD
C             LINES  =  CURSOR COUNTER
C             IER    =  ERROR RETURN (0=NORMAL)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      COMMON /PNTFET/ NPOINT
      COMMON /TYPE/ IDATYP(40),NACTS
      COMMON /IO/ NPRNT,IOIN
      DIMENSION ICOM(12),JCOM(12),IBUFF(128),ICARD(80)
      DIMENSION MAPNAM(25),IDESC(15),mapname(25)
      LOGICAL MCOMP,SCOMP
C
C  INITIALIZE FOR DESCRIBE.DT
C
       DO 5 KK=1,15
         IDESC(KK)=2H
   5   CONTINUE
C
C  FIRST, LETS SEE IF USER ENTER MAP NAMES  FILE
C
      LINES=0
      CALL DECIP(ICARD,ICOM,ICARDP)
      IF(ICOM(1).NE.0) GO TO 10
C
C  NOP.  PROMPT FOR ACTION
C
      WRITE(NPRNT,1000)
 1000 FORMAT(' PLEASE ENTER NAME OF FILE CONTAINING MAP NAMES'\)
      CALL READIN(ICARD,80,ICARDP,ISTOP)
      CALL DECIP(ICARD,ICOM,ICARDP)
      LINES=LINES+2
      GO TO 15
C
C  HAS USER ENTERED SEARCH STRING INPUT FILE?
C
  10  CONTINUE
      CALL DECIP(ICARD,JCOM,ICARDP)
      IF(JCOM(1).NE.0) GO TO 20
C
C  ASK USER FOR SEARCH STRING INPUT FILE
C
  15  CONTINUE
      WRITE(NPRNT,1001)
 1001 FORMAT(' PLEASE ENTER NAME OF SEARCH STRNG FILE'\)
      LINES=LINES+3
      CALL READIN(ICARD,80,ICARDP,istop)
      CALL DECIP(ICARD,JCOM,ICARDP)
C
C  USER INPUT COMPLETE.  LETS TRY TO OPEN FILES
C
  20  CONTINUE
      IER=0
      CALL PACKC(ICOM,MAPNAM,10)
      CALL OPENF(31,MAPNAM,1,2,0,IER)
      IF(IER.NE.0) GO TO 900
      CALL CLEAR(MAPNAM,10)
      CALL PACKC(JCOM,MAPNAM,10)
      CALL OPENF(32,MAPNAM,1,2,0,IER)
      IF(IER.NE.0) GO TO 910
C
C  STORE FOR <project>.DT
C
       CALL PACKC(ICOM,IDESC,10)
       CALL PACKC(JCOM,IDESC(6),10)
C
C  INPUT FILES ARE OPEN.  LETS ENTER MAIN LOOP
C
      NPSAVE=NPOINT+1
      KOUNT=0
      ICHAN6=10
  200 CONTINUE
      IOIN=31
      CALL READIN(ICARD,80,ICARDP,istop)
C
C  CHECK FOR END OF MAP NAMES INPUT
C
      IF(MCOMP(ICARD(1),'STOP',4,IER)) GO TO 300
C
C  CHECK FOR VALID MAP NAME
C
      CALL CLEAR(MAPNAM,25)
      CALL DECIP(ICARD,MAPNAM,ICARDP)
      CALL MCHEK(MAPNAM,IFIL,ITEMS,ITYPE,IER)
      IF (IFIL .EQ. 0) GO TO 920
C
C capture map name for active table
C
      do 921 ill=1,10
 921  icom(ill)=mapnam(ill)
      call packc(icom,jcom,10)
C
C  MAP FOUND SOMEWHERE.  CHECK FOR CELL DATA.
C
  30  CONTINUE
      IF(ITYPE .GE. 6  .AND. ITYPE .LE. 9)  GO TO 930
C
C  CHECK FOR ARCHIVED MAP
C
      IF(ITEMS.LT.0) GO TO 940
C
C  OPEN MAP FILE.
C
      call adddir(mapnam,mapname)
      CALL OPENF(ichan6,mapname,1,256,2,IER)
      IF(IER.NE.0) GO TO 950
C
C
C  TELL USER WE ARE PROCESSING A MAP
C
      LINES=LINES+2
      KOUNT=KOUNT+1
      IF(KOUNT.GE.41) GO TO 960
      WRITE(NPRNT,1005) (mapname(i),i=1,12)
 1005 FORMAT(' PROCESSING MAP ',12a2)
C
C  MAP IS OPEN.  LETS GET SUBJECT (ITEM) SEARCH STRING
C
      KHITS=0
 100  CONTINUE
      IOIN=32
      CALL READIN(ICARD,80,ICARDP,istop)
      IF(MCOMP(ICARD,'STOP',4,IER)) GO TO 150
C
C  CALL SUBJECT MATCHER
C
      NHITS=0
      CALL MATCH(IBUFF,ICARD,1,NHITS,ICHAN6,IER)
      KHITS=KHITS+NHITS
      GO TO 100
C
 150  CONTINUE
      REWIND IOIN
      CLOSE(ichan6,iostat=IER)
      IF(KHITS.EQ.0) GO TO 200
      CALL ADSTAD(IER,jcom,KHITS,NPSAVE,IDESC,ichan6)
      NACTS=NACTS+1
      IDATYP(NACTS)=ITYPE
      NPSAVE=NPOINT+1
      WRITE(NPRNT,1024) KHITS,NACTS
 1024 FORMAT(1X,I5,' HITS FOR ACTIVE ID ',I3)
      LINES=LINES+1
      GO TO 200
C
 300  CONTINUE
C
C  SEARCH COMPLETE.  CLOSE INPUT FILES AND RETURN
C
      CLOSE(31,iostat=IER)
      CLOSE(32,iostat=IER)
 999  CONTINUE
      IOIN=5
      RETURN
C
C  ERROR RETURNS
C
 900  WRITE(NPRNT,3000)
 3000 FORMAT(' *SELEXT* COULD NOT OPEN MAP NAMES FILE')
      IER=1
      LINES=LINES+2
      GO TO 999
 910  WRITE(NPRNT,3010)
 3010 FORMAT(' *SELEXT* COULD NOT OPEN SEARCH STRING FILE')
      LINES=LINES+2
      IER=1
      CLOSE(31,iostat=IER)
      GO TO 999
 920  WRITE(NPRNT,3020) (MAPNAM(I),I=1,10)
 3020 FORMAT(' *SELEXT*  COULD NOT FIND MAP ',10a1)
      LINES=LINES+2
      IER=1
      GO TO 999

 930  WRITE(NPRNT,3030)
 3030 FORMAT(' *SELEXT* CANNOT DO THIS ON RASTER DATA')
      IER=1
      LINES=LINES+2
      GO TO 999
 940  WRITE(NPRNT,3040)
 3040 FORMAT(' *SELEXT* MAP HAS BEEN ARCHIVED ' )
      LINES=LINES+2
      IER=1
      GO TO 999
 950  WRITE(NPRNT,3050) (ICOM(I),I=1,10),IER
 3050 FORMAT(' *SELEXT* COULD NOT OPEN MAP ',10A2,'  ERROR',I5)
      IER=1
      LINES=LINES+2
      GO TO 999
 960  WRITE(NPRNT,3060)
 3060 FORMAT(' *SELEXT* ABORT. MORE THEN 40 ACTIVE MAPS')
      IER=1
      LINES=LINES+2
      GO TO 999
C
      END
