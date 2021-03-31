C *****************************************************************
C
C          R O U T I N E    A U D I T
C
C  FUNCTION - GENERATE A LISTING OF ALL THE ITEMS AND THEIR BASIC
C             CHARACTERISTICS FOR A MAP IN A MASTER MAP FILE
C
C  INPUT PARAMETERS:
C
C  REQUIRED SUBROUTINES:
C        READIN, DECIP
C
C
C *****************************************************************
C
$Include:'IO.inc'
$Include:'STRNG.inc'
$Include:'WORK.inc'
      COMMON /FLS/   IUNIT,IFILE(5)
C
      INTEGER MAPNAME(25),ISUB(15)
      integer*4 nsum
C
      CHARACTER*32 DASHES
      LOGICAL MCOMP,HARD
C
      DIMENSION IACRS(2),IPERM(2),ICNTR(2),ITYPO(5,6),ICAT(5)
      DIMENSION IBUF(128)
C 
      EQUIVALENCE (ACRES,IACRS(1)),(PERIM,IPERM(1)),(CENTER,ICNTR(1))
      EQUIVALENCE (RLENTH,IBUFF(7))
C
      DATA ICAT/5*2H  /
      DATA ITYPO / 2HPO,2HIN,2HTS,2*2H  ,
     +             2HLI,2HNE,2HS ,2*2H  ,
     +             2HPO,2HLY,2HGO,2HNS,2H  ,
     +             2HAR,2HCS,3*2H  ,
     +             2HEL,2HEV,2HAT,2HIO,2HNS,
     +             2HIT,2HEM,2HS ,2*2H  /
C
      ICHAN=14
      SUMA = 0.
      SUMP = 0.
      ISUM = 0
      NSUM = 0
      DASHES = '--------------------------------'
C
C   GENERAL INITIALIZATION, READING COMMON AREA IN FROM INCM
      CALL INITL(IER)
      IF ( IER .NE. 0 ) GO TO 8600
C
      HARD=.FALSE.
C
C  Get name of file to audit
      CALL DECIP(ICARD,JCOM,ICARDP)
      IF (JCOM(1) .EQ. 0) THEN
C
C    Prompt the user to obtain the map name
         WRITE(NPRNT,200)
200      FORMAT(' ENTER THE NAME OF THE MAP THAT YOU WISH TO EXAMINE'\)
         CALL READIN(ICARD,LENICD,ISTART,IEND)
         IF ( IEND .EQ. 0 ) GO TO 9999
         CALL DECIP(ICARD,JCOM,ISTART)
      endif
c
c allow HARD.CPY to be given even at filename prompt
c
          KK=ICARDP
          CALL DECIP(ICARD,MAP,KK)
          IF(MCOMP(MAP,'HARD',4,IER)) HARD=.TRUE.
          IF(HARD) ICARDP=KK
          IF(HARD) then
          NPRNT=9
          CALL OPENF(NPRNT,8HHARD.CPY,0,2,0,IER)
          IF( IER .NE. 0) GO TO 8500
          endif
C
 112  CONTINUE
C
C
      NPAGES=1
      LINES=0
C
C   Check to see if it is in the directory
       CALL MCHEK(JCOM,IFIL,ITEMS,ITYPE,IER)
       IF(IFIL.EQ.0) GO TO 8100
       IF(ITYPE.LE.0.OR.ITYPE.GT.5) GO TO 8400
C
C   Open data channel 14 to map
       CALL CLEAR(MAPNAME,25)
C
C    If requested map is in MASTER directory, add path to name
       IF ( IFIL .EQ. 1 ) THEN
          jcom(1)=-jcom(1)
          CALL ADDDIR(JCOM,MAPNAME )
       else
          call adddir(jcom,mapname)
       ENDIF
       CALL OPENF(ICHAN,MAPNAME,1,256,2,IER)
       IF( IER .NE. 0) GO TO 8200
C
C   Read in map header to calculate first map entry info
       CALL RDBLK(ICHAN,1,IBUFF,1,IER)
       IPOINT=IBUFF(64)+IBUFF(65)+2
C
C   Print out some preliminary information
       CALL PRHEAD(' AUDIT              ',LINES,NPAGES)
       WRITE(NPRNT,300) mapname,ITEMS,(ITYPO(I,ITYPE),I=1,5)
300    FORMAT(' FOR THE MAP ',25A1,'  THERE ARE ',I5,1X,5A2)
C
C    PRINT OUT A HEADING ACCORDING TO THE DATA TYPE
C    VIZ. 1=POINT, 2=LINE, 3=POLYGON, 4=ARC, 5=ELEVATION
C
       IF (ITYPE .EQ. 6 ) THEN
          CALL RDBLK(ICHAN,IPOINT,IBUFF,1,IER)
          ITYPE=IBUFF(6)
       ENDIF
C
       IF ( ITYPE .EQ. 1 ) THEN
          WRITE(NPRNT,1111)
1111      FORMAT(12X,'SUBJECT',T33,'ITEM')
       ELSEIF ( ITYPE .EQ. 2 ) THEN
          WRITE(NPRNT,1112)
1112      FORMAT(T38,'LENGTH',6X ,'CENTER')
          WRITE(NPRNT,1113)
1113      FORMAT(12X,7HSUBJECT,T33,4HITEM,1X,7H(MILES),1X,
     +           7H(MILES),3X,6HPOINTS)
       ELSEIF ( ITYPE .EQ. 3 ) THEN
          WRITE(NPRNT,1114)
1114      FORMAT(T40,5HPERIM,6X,4HAREA)
          WRITE(NPRNT,1115)
1115      FORMAT(12X,7HSUBJECT,T33,4HITEM,1X,7H(MILES),3X,
     +           7H(ACRES),3X,7HISLANDS,1X,6HPOINTS)
       ELSEIF (ITYPE .EQ. 4 ) THEN
          WRITE(NPRNT,1116)
1116      FORMAT(12X,7HSUBJECT,T32,7HELEVATN,T43,4HITEM)
       ENDIF 
       WRITE(NPRNT,1117)
1117   FORMAT(/)
C
C    PRINT OUT THE CHARACTERISTICS OF EACH ITEM ACCORDING TO ITS DATA TYPE
C    FIRST READ EACH RECORD IN THE MAP (1 RECORD/ITEM) INTO THE ARRAY IBUFF
C    THEN RETRIEVE THE DATA OF INTEREST FROM THE RECORD FOR EACH DATA FIELD
C    THIS IS ALL DONE WITHIN A LOOP UNTIL ALL ITEMS HAVE BEEN EVALUATED
C
      LINES=LINES+6
      IP=0
      DO 500 I=1,ITEMS
C
C  Get a record.
         CALL RDBLK(ICHAN,IPOINT,IBUFF,1,IER)
         IF ( IER .NE. 0 ) THEN
            WRITE(NPRNT,1118) I
1118        FORMAT(38H BAD FIELD REQUEST MADE FOR RECORD NO.,I5)
            GO TO 90
         ENDIF
C
C  Retrieve the data that is common to all types (point,polygon, et. al.
         CALL GETSUB(ICHAN,IBUFF(24),IBUF,ISUB,NITMS)
       call fixchr(isub,30)
C
C    DETERMINE THE DATA TYPE AND PRINT OUT THE SPECIFIC INFORMATION
C
         GO TO (10,20,20,90,20), ITYPE
C
10       WRITE(NPRNT,1119) ISUB,IBUFF(5)
1119        FORMAT(1X,15A2,I5)
            GO TO 90
C
C   For both polygon and point data types, the data fields are the same,
C   although they represent different values. 
20       NSUM=NSUM+ IBUFF(25)
            IPERM(1)=IBUFF(9)
            IPERM(2)=IBUFF(10)
            IF(ITYPE.EQ.3) THEN
               IACRS(1)=IBUFF(7)
               IACRS(2)=IBUFF(8)
               SUMP=SUMP + PERIM
               SUMA=SUMA + ACRES
               ISUM=ISUM + IBUFF(26)
               WRITE(NPRNT,1120) ISUB,IBUFF(5),PERIM,ACRES,IBUFF(26),
     +                           IBUFF(25)
1120           FORMAT(1X,15A2,I5,F8.2,F12.2,2I8)
               GO TO 90
            ENDIF
            IF ( ITYPE .EQ. 5 ) THEN
               WRITE(NPRNT,1123) ISUB,PERIM,IBUFF(5)
1123           FORMAT(1X,15A2,F10.2,I5)
               GO TO 90
            ENDIF
            ICNTR(1)=IBUFF(11)
            ICNTR(2)=IBUFF(12)
            SUMP=SUMP + RLENTH
            CENTER=0.0
            WRITE(NPRNT,1121) ISUB,IBUFF(5),RLENTH,CENTER,IBUFF(25)
1121        FORMAT(1X,15A2,I5,2F8.2,I8)
C
90       CONTINUE
         IPOINT=IBUFF(1)
C
C  UPDATE CURSOR INFO AND CHECK FOR FULL SCREEN
C
         LINES=LINES+1
         IF(HARD) THEN
            IF ( LINES .GT. 60 ) THEN
               IP=1
               NPAGES=NPAGES+1
               LINES=1
               WRITE(NPRNT,1009)
1009           FORMAT(//)
               CALL PRHEAD(' AUDIT  ',LINES,NPAGES)
            ENDIF
C
         ELSE
            LINES=LINES-1
            CALL CONTIN(LINES,ISTOP)
            IF(ISTOP.EQ.1) GO TO 9999
         ENDIF
C
500   CONTINUE
C
C   Print out the totals for the line and polygon  types
       IF ( ITYPE .EQ. 2) THEN
          WRITE(NPRNT,1125) DASHES(1:16),DASHES(1:16)
1125      FORMAT(T39,A16,T55,A16)
          WRITE(NPRNT,1126) SUMP,NSUM
1126      FORMAT(T37,F8.2,T53,I8)
       ENDIF
       IF ( ITYPE .EQ. 3 ) THEN
          WRITE(NPRNT,1127) DASHES
1127      FORMAT(T39,A32)
          WRITE(NPRNT,1128) SUMP,SUMA,ISUM,NSUM
1128      FORMAT(34x,F10.2,F12.2,2i8)
       ENDIF
C
       GO TO 9999
C
C*****************************************************************************
C    ERROR MESSAGES
C
 8100   WRITE(NPRNT,98100) mapname
98100   FORMAT(1X,25a1,' DOES NOT EXIST IN CURRENT DIRECTORY')
        GO TO 9999
C
 8200   WRITE(NPRNT,98200)IER,MAPNAME
98200   FORMAT(' *AUDIT*  ERROR OPENING MAP',25A1,'  ERR=',I5)
        GO TO 9999
C
C? 8300   WRITE(NPRNT,98300)
C?98300   FORMAT(' *AUDIT* ERROR ON TEKTRONIX OPEN')
C?        GO TO 9999
C
 8400   WRITE(NPRNT,98400)
98400   FORMAT(' *AUDIT* CAN ONLY AUDIT VECTOR MAPS')
        LINES=LINES+2
        GO TO 9999
C
 8500  NPRNT=10
       WRITE(NPRNT,98500)
98500  FORMAT(' *AUDIT*  COULD NOT OPEN LINE.PTR FILE')
       GO TO 9999
C
 8600  WRITE(NPRNT,98600)
98600  FORMAT(' *AUDIT*   ERROR FROM INITL, TROUBLE WITH INTERPROC')
       GO TO 9999
C
C******************************************************************************
C
9999  CONTINUE
      LINES = LINES + 4
C
      CALL TYPESC
C
C?      CLOSE(IUNIT)
      CLOSE(ICHAN)
C
      END
