C*************************************************************************** 
C 
C           P R O G R A M     E X P O R T 
C 
C  FUNCTION - THIS ROUTINE IS THE LOGICAL DRIVER FOR THE EXPORT 
C             COMMAND. ITS FUNCTION IS TO ALLOW A USER TO EXPORT 
C             A DATASET TO ANOTHER COMPUTER INSTALLATION 
C             THIS COMMAND DOES NOT APPLY TO CELL FILES. 
C  ROUTINES 
C  CALLED:    Qincm,Qapget,IGETIS,RESET,OPEN,DAGET,GTMAP,GETSUB,OUTCM, 
C             GETWND,BACK,Qrdblk,CPLAC,Qreadin,Qgetndx,POSTF,DFILW,CFILW. 
C 
C 
C****************************************************************************** 
C 
       COMMON /WORK/  IBUFF(128),MAP(25),ICOM(10),JCOM(10) 
       COMMON /FLS/   IUNIT,IFILE(5) 
       COMMON /IO/    NPRNT,IOIN 
       COMMON /TYPE/  IDATYP(40),NACTS 
       COMMON /WHMAP/ ISTRCT(300),NSTRCT 
       COMMON /STRNG/ ICARD(80),ICARDP,LENICD 
C   STATIC CHANNEL NUMBERS FOR "FAST FILES", SET IN Qmossblk 
       COMMON /DBCHAN/ ICMAST,ICWORK,ICDCFA,ICDCDT,IDESFA(10),IDESDT(10) 
       COMMON /MFAST/ IFBUFF(256),INCORE,IWTRN,IFCHAN 
C 
       DIMENSION WIND(4),IR3(2) 
       DIMENSION IARR(20),ISLAND(350),IHEAD(256),LABEL(10) 
       DIMENSION IBUFF1(128),IBUFF2(128),IDESC(7),IMBR(8) ,mapname(25)
       REAL      MBR(4) 
       LOGICAL   scomp,MCOMP,LOGO,LLPROJ,SOL 
       DOUBLE PRECISION XXT,YYT 
       CHARACTER*25 IFLE 
       CHARACTER*1 KKK 
       CHARACTER*10 CBBB 
       DATA IDESC /2HDE,2HSC,2HRI,2HBE,2H.F,2HA ,0/ 
C 
       SOL = .FALSE. 
       call initl(IER) 
C 
      LOGO=.FALSE. 
      LLPROJ = .FALSE. 
      IF(ICARD(80).EQ.1) LOGO=.TRUE. 
C 
      CALL WHWND(MBR(1),MBR(3),MBR(2),MBR(4)) 
C 
C.....CHANGE INT FILENAME TO CHAR FILENAME.. 
C      CALL CHRBLD(IFILE,IFLE) 
      IUNIT = NPRNT 
C      OPEN(UNIT=IUNIT,FILE=IFLE,IOSTAT=IER) 
C      IF(IER.NE.0) GO TO 903 
C      call openf(15,IDESC(1),2,2,2,IER) 
C      IF(IER.NE.0) GO TO 903 
C 
c      call openf(ICDCFA,IDESC(1),1,512,2,IER) 
c      IF ( IER .NE. 0 ) GOTO 903 
      IPROMT=0 
      LINES=0 
      CALL DAGET(ICARD,ICARDP,IARR,20,MACTS) 
      IF(MACTS.NE.0) GO TO 8 
        IPROMT=1 
    5   CONTINUE 
        WRITE(NPRNT,1000) 
 1000   FORMAT(' ENTER UP TO 20 ACTIVE DATA SET IDS TO EXPORT',
     +         '[CR=EXIT]'\) 
        call readin(ICARD,80,ICARDP,LENICD) 
        CALL DAGET(ICARD,ICARDP,IARR,20,MACTS) 
        LINES=LINES+3 
        IF(MACTS.EQ.0) GO TO 200 
C 
    8 CONTINUE 
C 
C.....LOOP OVER ALL ID'S 
      DO 100 JJ=1,MACTS 
        INDEX=IARR(JJ) 
        ICTYPE=IDATYP((INDEX/7)+1) 
        IF(ICTYPE.EQ.6.OR.ICTYPE.EQ.7) GO TO 101 
        IKTYPE=IDATYP((INDEX/7)+1) 
        call apget(INDEX,ISTART,ISTOP,NTOTAL) 
        IF(NTOTAL.LE.0) GO TO 106 
        CALL CLEAR(ICOM,10) 
        CALL GTMAP(ICOM,INDEX) 
C 
C.......GET A GOOD FILENAME 
        call decip ( ICARD, JCOM, ICARDP ) 
        IF ( JCOM(1).NE.0 ) GO TO 13 
   11     WRITE(NPRNT,1010) (ICOM(I),I=1,5) 
 1010     FORMAT(' FOR ',5A2,', EXPORT FILENAME [EXPORT]'\) 
          call readin ( ICARD, 80, ICARDP, LENICD ) 
          call decip ( ICARD, JCOM, ICARDP ) 
       IF ( ICTYPE .EQ. 1 ) THEN 
          WRITE(NPRNT,1710) 
 1710     FORMAT('  EXPORT FILE ON SINGLE LINE? [CR = NO]') 
       KKK ='N' 
       READ(*,1711) KKK 
 1711     FORMAT(A1) 
          IF (KKK .EQ. 'y' .OR. KKK .EQ. 'Y') SOL = .TRUE. 
       ENDIF 
C 
C.........FILENAME ACTUALLY ENTERED? 
          IF ( JCOM(1).EQ.0 ) GO TO 14 
   13   CALL PACKC ( JCOM, MAP, 10 ) 
        GO TO 15 
C 
C.......USE DEFAULT 
   14   MAP(1) = 2HEX 
        MAP(2) = 2HPO 
        MAP(3) = 2HRT 
        MAP(4) = 0 
C 
 15     CONTINUE 
C 
C.......CLOSE CHANNEL FOR THIS MAP 
        CLOSE(4,IOSTAT=IER) 
C 
        CALL openf (4,MAP,1,1,0,IER ) 
      IF ( IER.ne.0 ) GO TO 21 
C.........CREATIOn ERROR 
          WRITE(NPRNT,1011) (MAP(I),I=1,5) 
 1011     FORMAT(' FILE "',5A2,'" ALREADY EXISTS.  OVERWRITE IT (Y/N)', 
     +           ' [Y]') 
          call readin ( ICARD, 80,ICARDP,LENICD ) 
          IF ( MCOMP(ICARD,'N',1,IER) ) GO TO 11 
   21   call openf ( 4, MAP, 0, 1, 0, IER ) 
        WRITE(NPRNT,1002) NTOTAL 
 1002   FORMAT(' NUMBER OF DATA ITEMS TO BE EXPORTED = ',I5) 
        LINES=LINES+2 
C 
        ICHAN1=1 
        call adddir(icom,mapname)
        call openf(ICHAN1,mapname,1,256,2,IER) 
        IF(IER.NE.0) GO TO 902 
        call rdblk(ICHAN1,1,IHEAD,1,IER) 
        call rdblk(ICHAN1,2,IHEAD(129),1,IER) 
        IF (IHEAD(193) .EQ. 0) LLPROJ = .TRUE. 
        NREC=IHEAD(64)+IHEAD(65)+2 
        THEYX=MBR(2)-MBR(1) 
        THEYY=MBR(4)-MBR(3) 
        THEY=AMAX1(THEYX,THEYY) 
        THEY=1.0/(THEY) 
C 
C.......NOW EXPORT DATA 
        KOUNT=1 
        WRITE(NPRNT,1008) 
 1008   FORMAT(' EXECUTING.  PLEASE WAIT . . . ') 
C 
      DO 30 I=ISTART,ISTOP 
             CALL GETFAST(I,IMBR,INDEX,ISUBJ,IATT,IEXT,IER) 
             IF(IER.NE.0) GO TO 30 
             call rdblk(ICHAN1,INDEX,IBUFF,1,IER) 
             IPOINT=IBUFF(24) 
             NVERTS=IBUFF(25) 
             CALL SCAGET(IBUFF,SCALE) 
             CALL IGETIS(IBUFF,ISLAND,NSLAND,ICHAN1,INDEX) 
             IF(NSLAND.EQ.0) ISLAND(1)=0 
             CALL GETSUB(ICHAN1,IPOINT,IBUFF1,MAP,ITS) 
             KNT = KOUNT 
             IF (LLPROJ) KNT = -KNT 
          ISOL = KNT 
C ...HEADER 
          IF (.NOT. SOL ) THEN 
          WRITE(4,1006) KNT,(IBUFF1(JK),JK=1,15),NVERTS 
 1006        FORMAT(I5,10X,15A2,5X,I5) 
          ENDIF 
             KOUNT=KOUNT+1 
C 
C.........TRANSFER COORDINATE DATA 
             CALL GETWND(INDEX,WIND,ICHAN1) 
             KPOINT=1 
             IWORD=0 
             NREC=INDEX+1+((NSLAND+28)/128) 
             call rdblk(ICHAN1,NREC,IBUFF,1,IER) 
             DO 22 KL=1,NVERTS 
               IWORD=IWORD+1 
               XT=IBUFF(IWORD) 
               IWORD=IWORD+1 
               YT=IBUFF(IWORD) 
               XT=ANINT(XT) 
               YT=ANINT(YT) 
               IF(IWORD.LT.128) GO TO 3 
               IWORD=0 
               NREC=NREC+1 
               call rdblk(ICHAN1,NREC,IBUFF,1,IER) 
    3          CONTINUE 
C         WRITE(*,*) ' X,Y,SCALE ',X,Y,SCALE 
C         WRITE(*,*) ' WIND ',WIND(1),WIND(2),WIND(3),WIND(4) 
               XT=XT*SCALE+WIND(1) 
               YT=YT*SCALE+WIND(3) 
               IF(LOGO) XT=(XT-MBR(1))*THEY 
               IF(LOGO) YT=(YT-MBR(3))*THEY 
               IF (LLPROJ) XT = XT - 180 
               IF (LLPROJ) YT = YT -  90 
C         IF(LLPROJ) WRITE(*,*) ' LLPROJ IS TRUE' 
               KNT = 0 
C 
               IF(KL.NE.ISLAND(KPOINT)) GO TO 23 
               KPOINT=KPOINT+1 
               KNT = 1 
               IF (.NOT. LLPROJ) XT=-XT 
   23          CONTINUE 
               IF(SOL .AND. .NOT. LLPROJ) THEN 
CSSK        WRITE(4,1706) KSOL,(IBUFF1(JK),JK=1,15),NVERTS,XT,YT 
               WRITE(4,1706) XT,YT,(IBUFF1(JK),JK=1,15) 
CSSK 1706      FORMAT(I5,10X,15A2,5X,I5,2F11.2) 
 1706          FORMAT(2F11.2,1X,15A2) 
            GOTO 22 
               ENDIF 
               IF(SOL .AND. LLPROJ) THEN 
C           WRITE(4,1776) KSOL,(IBUFF1(JK),JK=1,15),NVERTS,XT,YT,KNT 
C 1776          FORMAT(I5,10X,15A2,5X,I5,2F11.2,I2) 
            WRITE(4,1776) XT,YT, (IBUFF1(JK),JK=1,15) 
 1776          FORMAT(2F11.2,1X,15A2) 
          
            GOTO 22 
               ENDIF 
               IF (.NOT. LLPROJ) WRITE(4,1007) XT,YT 
 1007       FORMAT(2F11.2) 
               IF ( LLPROJ) WRITE(4,1009) XT,YT,KNT 
 1009       FORMAT(2F10.5,I2) 
   22          CONTINUE 
   30      CONTINUE 
C 
        CLOSE(ICHAN1,IOSTAT=IER) 
C 
C  .... JUMP OVER IN LOOP ERRORS 
      GOTO 100 
C.....ERROR PROCESSING 
  101 WRITE(NPRNT,1003) 
 1003 FORMAT(' *EXPORT* THIS COMMAND DOES NOT PROCESS RASTER DATA') 
      GO TO 100 
  106 WRITE(NPRNT,3006) 
 3006 FORMAT(' BAD apget CALL') 
      GO TO 100 
  107 WRITE(NPRNT,3007) IER 
 3007 FORMAT(' *EXPORT*  ERROR FROM CREATE OF EXPORT FILE  ERROR',I5) 
      GO TO 100 
  901 WRITE(NPRNT,3011) (MAP(II),II=1,5),IER 
 3011 FORMAT(' *EXPORT*  ERROR FROM OPEN OF EXPORT FILE ',5A2, 
     +       '  ERROR',I5) 
      GO TO 100 
  902 WRITE(NPRNT,3009) (ICOM(II),II=1,10),IER 
 3009 FORMAT(' *EXPORT* ERROR OPENING INPUT MAP',10A2,'  ERROR',I5) 
      GO TO 100 
C 
C.....NEXT ACTIVE ID....... 
 100  CONTINUE 
C 
C.....DATA SUCCESSFULLY TRANSFERED. 
  200 continue 
      CLOSE(ICHAN1,IOSTAT=IER) 
      GO TO 90000 
C 
C.....ERROR RETURN..... 
C 
90000 CONTINUE 
      END 
