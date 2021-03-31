C*************************************************************************** 
C 
C                M A I N   C O N T O U R 
C 
C  FUNCTION -    LOGICAL DRIVER FOR THE CONTOUR COMMAND 
C 
C  REQUIRED 
C  SUBROUTINES - BDRAW,  BOXF,   Qcalerr, CFILW,  CLEAR,  DAGET,  Qdecip, 
C                DFILW,  DSCLS,  DSIN,   DSOPN,  FNUM,   Qgtnam,  Qincm, 
C                INUM,   MCHEK1, MCOMP,  MERASE, Qopenf,  OUTCM,  PACKC, 
C                Qrdblk,  Qreadin, Qready,  SETWND, TUPDIR, Qwhtcal, WRBLK 
C 
C****************************************************************************** 
C 
$INCLUDE: 'XYPNTS.INC' 
       COMMON /GRIDC/   ICHAN,LEN,IBW 
       COMMON /IO/      NPRNT,IOIN 
       COMMON /WORK/    IBUFF(128),MAP(25),ICOM(10),JCOM(10) 
       COMMON /WHMAP/   ISTRCT(300),NSTRCT 
       COMMON /TYPE/    IDATYP(40),NACTS 
       COMMON /STRNG/   ICARD(80),ICARDP,LENICD 
       COMMON /FLS/     IUNIT,IFILE(5) 
       COMMON /DNAMES/  MAST(25),IWORK(25),IDIRC(8),JNAMES(3,12),LENDIR 
       COMMON /RUNDATA/ XMIN,YMAX,DX,DY,NCOLS,NROWS,SAVE,ITEMID, 
     +                  IDISP,ICTSUB 
       COMMON /COF/     COFS 
       COMMON /DSBUF/   MLEN,IDATA(8192) 
C 
       integer*2   pass1,pass2,ione,mapx(10),mapname(25) 
       INTEGER*4   IV4 
       REAL        RMIN
       DIMENSION   LCON(128),LABEL(25),IHEAD(256),ITEMP(5) 
       LOGICAL     MCOMP,DEF,SAVE 
       EQUIVALENCE (IHEAD(68),XM),  (IHEAD(74),YM), 
     +             (IHEAD(80),YS),  (IHEAD(82),XS), 
     +             (IHEAD(88),ZMIN),(IHEAD(90),ZMAX) 
C 
      IBLNK = 8224 
      IBEL   =  1799 
      mlen=8193
C 
C.....LOAD COMMON 
      call initl(IER) 
      IF(IER .NE. 0) GO TO 80000 
C 
C 
      mapX(1)=2HLM 
      mapX(2)=2h 
      call openf(70,mapX,0,0,1,ier) 
C 
C.....SET WINDOW & ERASE SCREEN 
      call initt(1,iunit,ifile)
      CALL SETWND 
C 
C.....SET SOME VARIABLES 
      IPRMT=0 
      ITEMID=0 
      LINES=1 
      SAVE=.FALSE. 
C 
C.....GET ACTIVE DATA SET TO CONTOUR 
    5 CALL DAGET(ICARD,ICARDP,MAP,25,MACTS) 
      IF(MACTS.NE.0) GO TO 10 
C 
C.......PROMPT FOR ACTIVE CELL MAP TO CONTOUR 
        IPRMT=1 
        WRITE(NPRNT,1000) 
 1000   FORMAT(' Enter active map ID to CONTOUR (CR=QUIT)',\)
        call readin(ICARD,80,ICARDP,LENICD) 
        IF(ICARD(1) .EQ. 0) GOTO 9999
        GO TO 5 
C 
C.....VALID DATA SET ? 
   10 CONTINUE 
      IF(MACTS.LE.0.OR.MACTS.GT.42) GO TO 900 
      INDEX=MAP(1) 
      I = IDATYP(INDEX/7+1) 
      IF ( I.NE.8 ) GO TO 904 
C 
C.....GET EXTENSION FOR CELL HEADER FILE AND OPEN DATA CHANNEL 
      call gtnam(INDEX,5,IBLNK,MAP,IER) 
      ICHAN = 12 
      call adddir(map,mapname)
      CALL DSOPN(ICHAN,MAPname,1,1,1,256,256,IER) 
      IF(IER.NE.0) GO TO 85020 
      IV4 = 1 
      IRECL = 512 
      CALL DSIN(ICHAN,IHEAD,1.0,IER) 
      IF (IER .NE. 0) GO TO 85040 
      CALL DSCLS(ICHAN,IER) 
      I = IHEAD(62) 
      IF ( I.NE.8 ) GO TO 904 
C 
C.....GET REST OF INFO (AND REASSIGN EQUIVALENCED) FROM HEADER 
      NROWS = IHEAD(60) 
      NCOLS = IHEAD(61) 
      XMIN = XM 
      YMAX = YM 
      DX   = XS 
      DY   = YS 
C.....COFS IS DATAOFFSET FROM ZERO, WILL BE USED LATER TO TRANSPOSE 
C     CONTOUR DATA TO ALL POSITIVE NUMBERS 
      ZER = 0.0 
      COFS= FLOAT(INT(ABS(AMIN1(ZMIN,ZER)))) 
      MINZ = INT(ZMIN+COFS)+1 
      MAXZ = INT(ZMAX+COFS) 
C 
C      LEN = BYTE(IHEAD(185),1) 
c      LEN = GTBYT(IHEAD(185),1) 
      pass2 = 0 
      pass1 = ihead(185) 
      ione = 1 
      call   GTB( pass2, pass1, ione ) 
      LEN = PASS2 
      IBW = 2 
      IF ( LEN.GE.16 ) IBW = 1 
      IF ( IBW.EQ.1  ) LEN = LEN / 16 
C 
C.....OPEN CELL DATA FILE 
      call gtnam(INDEX,I,IBLNK,MAP,IER) 
      CALL DSOPN(ICHAN,MAP,1,1,IBW,LEN,128,IER) 
      IF (IER .NE. 0) GO TO 85020 
      IF(IPRMT.EQ.1) GO TO 15 
C 
C.......GET CONTOUR INTERVAL INPUT TYPE 
   12   call decip(ICARD,LABEL,ICARDP) 
        LTYPE=LABEL(1) 
        IF ( LTYPE.NE.0 ) GO TO 30 
          IPRMT=1 
C 
C.....PROMPT FOR CONTOUR INTERVAL TYPE 
   15 CONTINUE 
      WRITE(NPRNT,1001) 
 1001 FORMAT(' User defined or Calculated intervals (U/C)',\)
      call readin(ICARD,80,ICARDP,LENICD) 
C 
C.....VALID CONTOUR SPECIFICATION ? 
   17 CONTINUE 
      call decip ( ICARD, LABEL, ICARDP ) 
      LTYPE = LABEL(1) 
      IF ( .NOT.(MCOMP(LTYPE,'C',1,IER).OR. 
     +           MCOMP(LTYPE,'U',1,IER)) ) GO TO 15 
      IF(IPRMT.EQ.1) GO TO 40 
C 
C.....SHOULD CONTOURS BE DISPLAYED?  (Y or N) 
   30 CONTINUE 
      IDISP=0 
      call decip(ICARD,LABEL,ICARDP) 
      IF(MCOMP(LABEL(1),'Y',1,IER)) IDISP=1 
      IF(LABEL(1).NE.0) GO TO 50 
        IPRMT=1 
C 
C.......PROMPT FOR DISPLAY 
   40   WRITE(NPRNT,1003) 
 1003   FORMAT(' Do you want contours displayed  (Y/(N))',\) 
        call readin(ICARD,80,ICARDP,LENICD) 
        call decip(ICARD,LABEL,ICARDP) 
        IF(MCOMP(LABEL(1),'Y',1,IER)) IDISP=1 
        GO TO 55 
C 
C.....SAVE THE RESULT OF THE CONTOURING ? 
   50 CONTINUE 
      call decip(ICARD,JCOM,ICARDP) 
      IF(JCOM(1).NE.0) GO TO 60 
        IPRMT=1 
C 
C.......PROMPT FOR SAVE SWITCH 
   55   CONTINUE 
        WRITE(NPRNT,1005) 
 1005   FORMAT(' Save the result of this effort? (Y/(N))',\) 
        call readin(ICARD,80,ICARDP,LENICD) 
        call decip(ICARD,JCOM,ICARDP) 
C 
C.....SET SAVE SWITCH 
   60 CONTINUE 
      SAVE = .FALSE. 
      IF(MCOMP(JCOM(1),'Y',1,IER)) SAVE=.TRUE. 
C 
C.....IF SAVING THE RESULT, GET THE SAVE NAME 
       IF( .NOT. SAVE) GO TO 70 
        call decip(ICARD,LABEL,ICARDP) 
        IF(LABEL(1).NE.0) GO TO 65 
C 
C.........PROMPT FOR FILE NAME 
   62     call whtcal(LABEL,IER) 
C 
C.......CHECK FOR VALID FILE NAME 
   65   CONTINUE 
        call mchek(LABEL,IFIL,NENTS,ITYPE,IER) 
        IF(IFIL.EQ.0) GO TO 70 
          WRITE(NPRNT,66) 
   66     FORMAT(' THIS MAP NAME ALREADY EXISTS.  TRY AGAIN') 
          GO TO 62 
C 
   70 CONTINUE 
      IF (IDISP.EQ.0 .AND. .NOT.SAVE) GO TO 9999 
C 
C.....ALL PARAMETER INPUT IS DONE AND CHECKED.  NOW, IF LTYPE=USER 
C.....GET CONTOUR LEVELS FROM THE USER 
C 
      IF(MCOMP(LTYPE,'C',1,IER)) GO TO 80 
        WRITE(NPRNT,1007) MINZ-INT(COFS), MAXZ-INT(COFS) 
 1007   FORMAT(' Enter contour interval values (',I5,' TO ',I6, 
     +         ') (CR=Enough)',\) 
        NINTS = 0 
        DO 75 I=1,128 
          LCON(I) = 0 
          CALL INUM(ICARD,LCON(I),ICARDP) 
C.........RESCALE SO ENTIRE RANGE IS POSITIVE 
          IF(LCON(I).NE.0) WRITE(NPRNT,71) I,LCON(I) 
   71     FORMAT(' VALUE #',I3,' =',I5) 
          IF(LCON(I).NE.0) GO TO 74 
   72       WRITE(NPRNT,1008) I 
 1008       FORMAT(' VALUE # ',I3,' = ') 
            call readin(ICARD,80,ICARDP,LENICD) 
            IF ( ICARD(1).EQ.0 ) GO TO 86 
              LCON(I) = 0 
              CALL INUM(ICARD,LCON(I),ICARDP) 
   74     CONTINUE 
          IF ( LCON(I).LT.MINZ-INT(COFS) .OR. LCON(I).GT.MAXZ-INT(COFS)) 
     +         WRITE(NPRNT,743) IBEL 
  743     FORMAT(I2) 
          IF (LCON(I).LT.MINZ-INT(COFS) .OR. LCON(I).GT.MAXZ-INT(COFS)) 
     +         GO TO 72 
          NINTS = NINTS + 1 
          LCON(I)=LCON(I)+COFS 
   75   CONTINUE 
        WRITE(NPRNT,76) 
   76   FORMAT(' MAXIMUM CONTOUR INTERVALS ENTERED, CONTINUING . . .') 
        GO TO 86 
C 
C.....CALCULATED CHOSEN:  USER DEFINED MIN AND INCREMENT 
   80 RMIN = 0. 
      UINC = 0. 
      WRITE(NPRNT,81) MINZ-INT(COFS), MAXZ-INT(COFS), MINZ-INT(COFS) 
   81 FORMAT(' Minimum contour value desired (',I6,' to ',I6,') [', 
     +       I7,']',\) 
      call readin ( ICARD,80, ICARDP,LENICD ) 
      DEF=.FALSE. 
      CALL FNUM ( ICARD, 80, RMIN, ICARDP ) 
      if(rmin.lt.minz-INT(COFS).and.icard(1).ne.0)then
       write(*,888)
888    format(' New min. must be within current min. and max range',/)
       go to 80
      endif
      IF (ICARDP.EQ.1) DEF=.TRUE. 
        WRITE(NPRNT,83) 
   83   FORMAT(' Contour increment value desired',\) 
   84   call readin ( ICARD,80, ICARDP,LENICD ) 
        CALL FNUM ( ICARD, 80, UINC, ICARDP ) 
        IF ( UINC.GT.0. ) GO TO 86 
          WRITE(NPRNT,85) UINC 
   85     FORMAT(' INVALID INCREMENT ',F6.3,', RE-ENTER ',\) 
          GO TO 84 
C 
C.....OPEN TEMP FILES FOR SAVWRK IF REQUIRED 
   86 CONTINUE 
      ZMAX2=ZMAX 
      IF(.NOT.SAVE) GO TO 88 
      ITEMP(1) = 2HTE 
      ITEMP(2) = 2HMP 
      ITEMP(3) = 2HSU 
      ITEMP(4) = 2h.z 
        ICTSUB=17 
        call openf(ICTSUB,ITEMP,3,256,2,IER) 
        IF( IER .NE. 0) GO TO 85030 
      ITEMP(4) = 2h.b 
        call openf(18,ITEMP,3,1,1,IER) 
        IF( IER .NE. 0) GO TO 85030 
        IHEAD(64)=1 
        IHEAD(65)=IHEAD(77) 
        IHEAD(67)=0 
        IHEAD(66)=0 
        IHEAD(60) = 0 
        IHEAD(61) = 0 
        CALL CLEAR(IHEAD(77),51) 
        CALL PACKC(LABEL,JCOM,10) 
        IFTYP = 2 
        IHEAD(62) = IFTYP 
        WRITE(18) (JCOM(II),II=1,10) 
        WRITE(18) (LABEL(II),II=1,25) 
        WRITE(18) (LABEL(II),II=1,25) 
        WRITE(18) (IHEAD(II),II=1,65) 
        WRITE(18) IFTYP 
C 
C.....CALCULATE INTERVALS 
   88 CONTINUE 
      IF(MCOMP(LTYPE,'U',1,IER)) GO TO 92 
        LCON(1)=INT(RMIN+COFS) 
        IF (DEF) LCON(1)=MINZ 
        IF(UINC.LE.0.99) UINC=1. 
        NINTS = 1 
        DO 90 I=2,128 
          LCON(I)=LCON(1) +  IFIX((UINC*FLOAT(I-1))+.5) 
          NINTS = NINTS + 1 
          IF (FLOAT(LCON(I)).GE.ZMAX2+COFS ) GO TO 91 
   90   CONTINUE 
        WRITE(NPRNT,903) LCON(1), LCON(NINTS) 
  903   FORMAT(' MAXIMUM INTERVALS BEING USED - CONTINUING ',2I5) 
        GO TO 92 
   91   LCON(NINTS) = 0 
        NINTS = NINTS - 1 
C 
C.....IF IDISP IS YES, ERASE SCREEN 
   92 CONTINUE 
      IF(IDISP .EQ. 1) LINES=1 
      WRITE(NPRNT,909) NINTS,(INT(LCON(I)-INT(COFS)),I=1,NINTS) 
  909 FORMAT(' ',20X,I5,' INTERVALS BEING USED ARE ',/,13(/,10I8),/) 
C 
C.....DO THE CONTOURING 
      if(idisp.eq.1)then
      write(*,*)
      write(*,*) 'After viewing contoured map - hit CR to continue'
      write(*,*)
      write(*,*) 'Please wait....'
      endif
      CALL BOXF(LCON,NINTS)
      call vecmod
C 
C.....DRAW BORDER AROUND CONTOURS 
      IF (IDISP .EQ. 1) then
         CALL BDRAW 
         read(*,'(a1)')ans
         write(*,*)char(27),char(12)
      endif
C 
C.....CLOSE THE CELL FILE 
      CALL DSCLS(ICHAN,IER) 
C 
C.....IF SAVE IS TRUE, UPDATE DIRECTORY AND WRITE 
      IF(.NOT.SAVE) GO TO 9999 
        ICHAN8 = 18 
        ICHAN5 = 15 
        call openf(ICHAN5,JCOM(1),0,256,2,IER) 
        IF(IER.NE.0) GO TO 85030 
        CALL Savwrk(ictsub,nints,ichan8,ichan5) 
        CLOSE(ICTSUB) 
        CLOSE(ICHAN8) 
        CLOSE(ICHAN5) 
C.......ADD RESULT TO WORK FILE 
c        call gtnam(0,1,IWORK,MAP,IER) 
c        ICHAN3=14 
c        call openf(ICHAN3,MAP,2,512,2,IER) 
c        IF(IER.NE.0) GO TO 85020 
C 
        call gtnam(-1,0,LABEL,MAP,IER) 
        call openf(ICHAN,MAP,1,256,2,IER) 
        IF(IER.NE.0) GO TO 85020 
        call rdblk(ICHAN,1,IHEAD(1),1,IER) 
        IF(IER .NE. 0) GO TO 85060 
        CALL WRBLK(ICHAN,2,IHEAD(129),1,IER) 
        IF(IER .NE. 0) GO TO 85080 
        CLOSE(ICHAN) 
C 
        call gtnam(0,2,IWORK,MAP,IER) 
        CALL TUPDIR(ITP1,ITP2,ITP3,LABEL,IHEAD,iwork,2,IER) 
        CLOSE(2) 
C 
C.......NORMAL EXIT 
        GO TO 9999 
C 
C.....ERROR RETURNS 
C 
  900 write(*,*)' Map ID is not valid - 1 through 40' 
      GO TO 9999 
C 
  904 WRITE(NPRNT,3004) I 
 3004 FORMAT(' MUST USE CONTINUOUS CELL MAPS NOT TYPE',I5) 
C      LINES=LINES+2 
      GO TO 9999 
C 
80000 CONTINUE 
      WRITE(NPRNT,98500) IER 
98000 FORMAT(' *CONTOUR*  ERROR FROM Qinitl   ERROR ',I5) 
      GO TO 9999 
C 
85000 CONTINUE 
      WRITE(NPRNT,98500) IER 
98500 FORMAT(' *CONTOUR*  ERROR FROM SAVE   ERROR ',I5) 
      GO TO 9999 
C 
85020 CONTINUE 
      WRITE(NPRNT,98502) (MAP(I),I=1,15),IER 
98502 FORMAT(' *CONTOUR*  ERROR FROM OPEN OF ',15A2,'  ERROR',I5) 
C      LINES = LINES + 2 
      GO TO 9999 
C 
85030 CONTINUE 
      WRITE(NPRNT,98503) IER 
98503 FORMAT(' *CONTOUR*  ERROR FROM OPEN OF TEMP FILE   ERROR',I5) 
C      LINES = LINES + 2 
      GO TO 9999 
C 
85040 CONTINUE 
      WRITE(NPRNT,98504) IER 
98504 FORMAT(' *CONTOUR*  ERROR FROM DSIN  ERROR',I5) 
C      LINES = LINES + 2 
      GO TO 9999 
C 
85060 CONTINUE 
      WRITE(NPRNT,98506) IER 
98506 FORMAT(' *CONTOUR*  ERROR FROM rdblk  ERROR',I5) 
      GO TO 9999 
C 
85080 CONTINUE 
      WRITE(NPRNT,98508) IER 
98508 FORMAT(' *CONTOUR*  ERROR FROM WRBLK  ERROR',I5) 
C 
C.....JUMP OUT 
 9999 CONTINUE 
      END 
