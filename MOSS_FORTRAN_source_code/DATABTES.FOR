C**************************************************************************
C
C     PROGRAM  D A T A B T S T
C
C    FUNCTION :   PROCESSES THE CATALOGUE OF FILES ( .DT )  IN DIRECTORIES
C                 ALSO ALLOWS USERS TO QUERY ENTRIES.
C
C**************************************************************************
$Include:'DNAMES.inc'
$Include:'STRNG.inc'
       common/dbmap/dbnmap(2)
       DIMENSION IHEAD(256),ICOM(16),MAP(25),NAME(16)
       INTEGER FIRST,wrk(2),dbnmap
C
       ICHAN1 = 1
       ICHAN2 = 2
       ICHAN3 = 10
C
C  FIRST GET NAME OF PROJECT DIRECTORY
C
       Call Initl(ier)
       if(ier.ne.0)stop
C
C Clean up for "Delete" on number of maps in .dt
C
      LAST=dbnmap(2)+1
C Starts LAST off with the right value
C
C
C  READ IN DIRECTORY OPERATING PARAMETERS
C
C  MAIN MENU
C
        Call Decip(Icard,icom,ICARDP)
  10   CONTINUE
       WRITE(NPRNT,90013)dbNMAP(2),(iwork(l),l=1,25)
90013  FORMAT(/,1X,I5,' MAPS IN PROJECT FILE: ',25a1)
       icomc=1
       if(icom(1).eq.0)then
        write(nprnt,90014)
90014  format(6X,' 1-FINISH 2-ADD 3-FIND 4-LIST 5-DELETE '\)
C
        call clear(icard,80)
        CALL READIN(Icard,lenicd,icardp,istop)
        call decip(icard,icom,icardp)
       endif
C
   11  CALL INUM(Icom,IANS,icomc)
       icom(1)=0
       IF(IANS.LT.1.OR.IANS.GT.6) GO TO 10
       GO TO(999,20,30,40,60),IANS
C
C**     ADD
C
  20   CONTINUE
       call clear(icom,16)
       CALL DECIP(ICARD,ICOM,ICARDP)
       IF (ICOM(1) .EQ. 0) then
         WRITE(NPRNT,1000)
 1000  FORMAT(' ENTER MAP NAME '\)
         do 112 l=1,10
         icard(l)=32
  112    icom(l)=32
         CALL READIN(ICARD,LENICD,ICARDP,istop)

         CALL DECIP(ICARD,ICOM,ICARDP)
       Endif
       CALL FNDMAP(dbnmap(2),ICOM,ICHAN2,ILOC1,ILOC2)
       IF (ILOC1 .EQ. 0) GO TO 22
       WRITE(NPRNT,1001)
1001   FORMAT(' THIS NAME ALREADY EXISTS IN THE NAMES LIST')
       icom(1)=0
       GO TO 10
C
C  NAME NOT IN LIST...TRY TO OPEN ON DISK AND PUT IN
C
22     CONTINUE
C
C  TRY TO OPEN VECTOR FILE
C
       CALL GTNAM(-1,0,ICOM,MAP,IER)
       CALL OPENF(ICHAN3,MAP,1,256,2,IER)
       IF (IER .EQ. 0) GO TO 23
C  TRY TO OPEN CELL MAP
       CALL GTNAM(-1,5,ICOM,MAP,IER)
       CALL OPENF(ICHAN3,MAP,1,256,2,IER)
       IF (IER .EQ. 0) GO TO 23
C
C  MAP NOT ON DISK
C
       WRITE(NPRNT,1002) (icom(I),I=1,10)
1002   FORMAT(' CAN NOT OPEN MAP ',10A2)
       icom(1)=0
       GO TO 10
C
C  READ HEADER RECORDS
C
23     CALL RDBLK(ICHAN3,1,IHEAD,1,IER)
       IF (IER .NE. 0) GO TO 24
       CALL RDBLK(ICHAN3,2,IHEAD(129),1,IER)
       IF (IER .NE. 0) GO TO 24
       GO TO 25
C  COULDN'T READ HEADERS SO QUIT
24     WRITE(NPRNT,1004)
1004   FORMAT(' COULD NOT READ THE HEADERS FOR THIS MAP')
       CLOSE(ICHAN3,iostat=IER)
       icom(1)=0
       GO TO 10
C
C  IT'S HERE SO PROBABLY ADD IT
C
25     CLOSE(ICHAN3,iostat=IER)
       IF(IHEAD(62) .LT. 6  .OR.  IHEAD(62) .GT. 9 .AND.
     + IHEAD(62) .LT. 13)
     + WRITE (NPRNT,1005) IHEAD(62),IHEAD(63),IHEAD(67)
1005   FORMAT(' TYPE IS ',I5,'  # OF ITEMS ',I5,' # OF SUBJ ',I5)
       IF (IHEAD(62) .GT. 5  .AND.  IHEAD(62) .LT. 10)
     + WRITE(NPRNT,1006) IHEAD(62),IHEAD(60),IHEAD(61),IHEAD(67)
1006   FORMAT(' TYPE IS',I5,' # OF ROWS',I5,' # OF COLS',I5,
     + ' # OF ATTS',I5)
       IF (IHEAD(62) .GT. 15  .AND. IHEAD(62) .LT. 19)
     + WRITE(NPRNT,1007) IHEAD(62)
1007   FORMAT(' TYPE IS',I5)
       IF (IHEAD(62) .LT. 1  .OR. IHEAD(62) .GT. 18)
     + WRITE(NPRNT,1008) IHEAD(62)
1008   FORMAT(' TYPE IS',I5,
     + ' TYPE IS ILLEGAL FOR A MAP...IT CAN NOT BE ADDED')
       IF (IHEAD(62) .LT. 1  .OR.  IHEAD(62) .GT. 18)  GO TO 10
       CALL TUPDIR(FIRST,LAST,dbnmap(2),ICOM,IHEAD,iwork,ICHAN2,IER)
       IF (IER .EQ. 0) WRITE(NPRNT,1009)
1009   FORMAT(' OK!  MAP ADDED')
       icom(1)=0
       GO TO 10
C
C**     FIND
C
  30   Call Decip(Icard,Name,ICARDP)
       If(Name(1).eq.0)then
         WRITE(NPRNT,1000)
         CALL READIN(ICARD,LENICD,ICARDP,istop)
         CALL DECIP(ICARD,NAME,ICARDP)
       Endif
       CALL FNDMAP(dbnmap(2),NAME,ICHAN2,ILOC1,ILOC2)
       IF (ILOC1 .EQ. 0) WRITE(NPRNT,90030)
90030  FORMAT(' MAP NOT FOUND')
       icom(1)=0
       IF(ILOC1.EQ.0) GO TO 10
       WRITE(NPRNT,90032) (NAME(J),J=1,8),NAME(10),NAME(12),NAME(13),NAME(14)
90032  FORMAT(1X,8A2,' TYPE',I5,' SUBJs',I5,' ITEMS',2I5)
       icom(1)=0
       GO TO 10
C
C**     LIST WITH FULL RECORD PRINT OUT
C
  40   CONTINUE
       WRITE(NPRNT,1042)
 1042  FORMAT(' NAME            BITS/STATUS TYPE SUBJ ITEM ITRO COLS',
     +' BYTE')
       icom(1)=0
       IF (dbnmap(2) .LE. 0) GO TO 10
       DO 48 I=2,dbnmap(2)+1
         CALL GETMAP(ICHAN2,I,NAME,IER)
         wrk(1)=name(9)
         call unpack(wrk,2)
         do 113 l=1,8
         if(name(l).eq.0)name(l)=8224
 113     if(name(l).lt.256)name(l)=8192+name(l)
         WRITE(NPRNT,1044) (NAME(J),J=1,8),wrk(1),wrk(2),
     +name(10),name(12),name(13),name(14),name(15),name(16)
 1044    FORMAT(1X,8A2,i3,3x,i3,7I5)
 48    CONTINUE
       GO TO 10
C
C**     RENAME
C
  50   CONTINUE
       WRITE(NPRNT,90050)
90050  FORMAT(' SORRY RENAMING IS NOT AVAILABLE')
       icom(1)=0
       GO TO 10
C
C**     DELETE
C
  60   CONTINUE
       call clear(name,16)
       CALL DECIP(ICARD,NAME,ICARDP)
       if(name(1).eq.0)then
         WRITE(NPRNT,1000)
         call clear(icard,lenicd)
         CALL READIN(ICARD,LENICD,ICARDP,istop)
         call clear(name,16)
         CALL DECIP(ICARD,NAME,ICARDP)
       Endif
       CALL DELMAP(FIRST,LAST,dbnmap(2),NAME,ICHAN2,IER)
       icom(1)=0
       GO TO 10
C
C**     FINISH
C
 999   CONTINUE
       CLOSE(ICHAN1,iostat=IER)
       GO TO 90000
C
C**     ERROR PROCESSING
C
 900   WRITE(NPRNT,3000) (MAP(I),I=1,10),IER
 3000  FORMAT(' COULD NOT OPEN ',10A2,'  ERROR',I5)
C
90000  CONTINUE
       END
