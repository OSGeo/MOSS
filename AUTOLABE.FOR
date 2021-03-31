C*************************************************************************** 
C 
C  Name:    P R O G R A M   A U T O L B 
C 
C  Purpose: THIS PROGRAM WILL AUTOMATICALLY CONTOUR LABEL A LINE 
C           MAP.  THE OUTPUT MAY BE IN THE FORM OF TWO NEW FILES, 
C           ONE OF TEXT (TYPE 10) AND ANOTHER OF LINES WITH THE 
C           LABELED AREAS MASKED OFF. 
C****************************************************************************** 
C 
$INCLUDE: 'XYPNTS.INC' 
$INCLUDE: 'YRYR.INC' 
$INCLUDE: 'DBCHAN.INC' 
       COMMON /MFAST/  IFBUFF(256),INCORE,IWRTN,IFCHAN 
       COMMON /IO/     NPRNT,IOIN 
       COMMON /FLS/    IUNIT,IFILE(5) 
       COMMON /OSAVE/  ISUB(15),NPOLYS,LTYPE2,IDISP,NOSAVE,ICTSUB,ITREC 
       COMMON /WORK/   IBUFF(128),LNMASK(25),LLCOM(10),KCOM(10) 
       COMMON /ATTFET/ KAT(86), IBAT 
       COMMON /LOCO1/  BOX(4,4), LENGTH, HGT, SMDIF, NSTART, NSTOP,IDO, 
     +                 LOSER,NBOX,MAXBOX,IATT,IATYP,MWORDS,LOCATE,NVERT, 
     +                NCHAR,LREF(4), MREF(4) 
       COMMON /LOCO2/  MBR(8), XMID, YMID, IQUAD(4) 
       COMMON /LOCO3/  NEND, ITEMS, TSCAL, ICHAN2 
       COMMON /LOCO4/  ISUBJ(128), NSUBS 
C 
       DIMENSION   MTXT(25), ICOM(25), IHEAD(256), ITEMP(5), IDESCB(6), 
     +             IPOLY(6),LCOM(25),MAPX(10) 
       CHARACTER*128 INRD 
       REAL        LENGTH 
       INTEGER*2   FONT(6),CHAN15,mapname(25) 
       LOGICAL     MCOMP, FOUND, NOSAVE 
       EQUIVALENCE ( MBR(1), XMIN ), ( MBR(3), XMAX ), 
     +             ( MBR(5), YMIN ), ( MBR(7), YMAX ), 
     +             ( IHEAD(84), XORG ), ( IHEAD(86), YORG ), 
     +             ( IHEAD(92), XEND ), ( IHEAD(94), YEND ) 
C 
      ITREC = 0 
c 
c 
      IBLNK= 8224 
      CHAN15=15
C 
C open <lastmap> to be drawn 
C 
      mapX(1)=2HLM 
      mapX(2)=2h 
      CALL openf(70,mapX,0,0,1,ier) 
C 
C.....READ IN COMMON BLOCK FROM INTERPRO 
c 
      CALL initl(IER) 
      IF(IER .NE. 0) GO TO 9000 
C 
C.....OPEN GRAPHIC DEVICE, AND MOSS REFERENCE FILES 
      call initt( 1, iunit, ifile ) 
c 
c      CALL openf(ICDCFA,idesfa,1,512,2,IER) 
c      IF (IER .NE. 0) GO TO 3030 
C 
C.....SET WINDOW, AND INSURE NON-ZERO 
      CALL SETWND 
      CALL WHWND ( XMIN, YMIN, XMAX, YMAX ) 
      IF ( (XMAX-XMIN).LE..0001 .AND. (YMAX-YMIN).LE..0001 ) GO TO 3010 
      XMID = ( XMAX + XMIN ) / 2. 
      YMID = ( YMAX + YMIN ) / 2. 
C 
C.....GET ALL OF THE PARAMETERS FOR THIS COMMAND 
      CALL LBPARM ( NOSAVE,LOSER,CHRSZ,IMINI,IINTER,MTXT,LNMASK, 
     1     IDD,IER ) 
      IF ( IER.NE.0 ) GO TO 3020 
      if(nosave) then
      write(*,101)
101   format(/,' After viewing labeled map - Hit CR to continue',/,
     + /,' Hit CR to proceed.')
      read(*,'(a1)')ans
      endif
C 
C.....CALCULATE CHARACTER HEIGHT BASED ON SCALE OF MAP 
      TSCAL  = AMAX1 ( (XMAX-XMIN), (YMAX-YMIN) ) 
      TSCAL  = 10.14 / TSCAL 
      PENWID =   .01 / TSCAL 
      HGT    = ( CHRSZ / TSCAL ) + .1 
C 
C.....IF MASKED, THEN PUT ON .MS EXTENSION 
      IF ( NOSAVE ) GO TO 200 
C.......TEMPORARY FILE SETUP FOR SAVWRK 
        ICTSUB = 29 
      ITEMP(1) = 2HTE 
      ITEMP(2) = 2HMP 
      ITEMP(3) = 2HSU 
      ITEMP(4) = 2h.B 
        CALL openf(ICTSUB,ITEMP,3,256,2,IER) 
        IF( IER .NE. 0) GO TO 3030 
      ITEMP(4) = 2h.S 
        CALL openf(18,ITEMP,3,0,1,IER) 
        IF( IER .NE. 0) GO TO 3030 
C 
 200  continue 
C.....INITIALIZE FONTS FILE 
C 
      CALL plotsym ( 0, -61, 0.0, 0.0, 0.0, 0.0,5hFONTS, 0.0, 0.0, 0.0, 
     +              0, 5, IER ) 
      IF ( IER.NE.0 ) GO TO 3040 
C 
C.....GET SOURCE MAPNAME AND OPEN IT 
      CALL gtnam(IDD,0,IBLNK,LCOM,IER) 
      call adddir(lcom,mapname) 
      CALL openf(CHAN15,mapname,1,256,2,IER) 
      IF (IER .NE. 0) GO TO 3060 
C.....READ HEADER 
      CALL rdblk (CHAN15, 1, IBUFF, 1, IER ) 
      if(ier .ne. 0) go to 4000 
C.....CHECK MULTIPLE ATTRIBUTE VARIABLES 
      NFEAT  = IBUFF(63) 
      NSUBS  = IBUFF(67) 
      IATTFL = IBUFF(76) 
      IATT   = KAT((IDD/7)*2+1) 
      MWORDS = ABS ( IATT ) 
      LOCATE = KAT((IDD/7)*2+2) 
C 
C.....CHECK FOR MULTIPLE ATTRIBUTE FILE, IF THERE, OPEN IT. 
c      IF ((IATT .EQ. 0) .OR. (IATTFL .NE. 1)) GO TO 250 
c        CALL gtnam(IDD,4,IBLNK,ICOM,IER) 
c        IF (IER .NE. 0) GO TO 3070 
C.......OPEN MULTIPLE ATTRIBUTE FILE 
c        CALL openf(26,ICOM,2,256,2,IER) 
c        IF (IER .NE. 0) GO TO 3080 
c        IF ( IATT.LT.4 ) GO TO 250 
c          CALL initial 
c          CALL RETRVL ( FOUND, IBUFF ) 
c          IF ( .NOT. FOUND ) GO TO 3090 
c            IATYP  = IBUFF(84) 
c            MWORDS = IBUFF(85) 
c            LOCATE = IBUFF(86) 
C.....RE-READ HEADER INTO IHEAD FOR TUPDIR LATER 
  250 CALL rdblk ( CHAN15, 1, IBUFF, 1, IER ) 
      IDATA     = IBUFF(62) 
      IBUFF(58) = IBUFF(127) 
      IBUFF(59) = IBUFF(128) 
      IBUFF(60) = 0 
      IBUFF(61) = 0 
      IBUFF(62) = 2 
      IBUFF(64) = 1 
      IBUFF(65) = IBUFF(77) 
      IBUFF(66) = 0 
      IBUFF(67) = 0 
C 
C.....IF REQUIRED SETUP TEMP FILE FOR SAVWRK TO COME 
      IF ( NOSAVE ) GO TO 300 
      WRITE(18)(LNMASK(II),II=1,10) 
      WRITE(18)(LNMASK(II),II=1,25) 
      WRITE(18)(LNMASK(II),II=1,25) 
      WRITE(18)(IBUFF(II),II=1,65) 
      WRITE(18)IDATA 
C 
C.......SETUP TEXT MAP 
        ICHAN2 =22 
        CALL openf ( ICHAN2, MTXT, 0, 256, 2, IER ) 
       if(ier .ne. 0) go to 4020 
        CALL CLEAR (IHEAD(1), 256) 
        IHEAD(62)  = 10 
        IHEAD(64)  =  1 
        IHEAD(65)  =  1 
        IHEAD(119) =  1 
C.......DESCRIPTION 
        DO 260 I = 2, 49 
  260     IHEAD(I) = IBUFF(I) 
C.......MINIMUM BOUNDING RECTANGLE 
        DO 270 I = 1, 8 
  270     IHEAD(67+I) = MBR(I) 
        DO 280 I = 1, 4 
          IHEAD(79+I) = IBUFF(79+I) 
  280     IHEAD(87+I) = IBUFF(87+I) 
        NREC = IHEAD(64) + IHEAD(65) + 2 
        CALL WRBLK ( ICHAN2,1, IHEAD(1), 1, IER ) 
        CALL WRBLK ( ICHAN2,2, IHEAD(129), 1, IER ) 
        CALL WRBLK ( ICHAN2, NREC, IHEAD( 1 ), 1, IER ) 
        ITEMS = 0 
        NEND = NREC + ITEMS 
C 
C.....NOW CALL THE ROUTINE TO PROCESS THE CONTOUR INFORMATION 
C 
  300 CALL LBGENR ( CHAN15, PENWID, IMINI, IINTER, IDD, IER ) 
      IF ( IER.ne.0) GO TO 9000 
C 
C.....AFTERMATH - CREATE NEW MAP(S)? 
      IF(NOSAVE)THEN
        call bdraw
        call flsh
        read(*,'(a1)')ans
        write(*,*)char(27),char(12)
        GO TO 9000
      ENDIF
C.......WRAP UP TEXT MAP 
        IHEAD( 1) = NEND - 1 
        IHEAD(63) = ITEMS 
        IHEAD(67) = 0 
        CALL WRBLK ( ICHAN2, 1, IHEAD( 1 ), 1, IER ) 
        CALL WRBLK ( ICHAN2, 2, IHEAD(129), 1, IER ) 
        CLOSE ( ICHAN2, IOSTAT=IER ) 
        CLOSE (CHAN15, IOSTAT=IER ) 
        CALL UNPACK ( MTXT, 24 ) 
        CALL TUPDIR ( ITP1, ITP2, ITP3, MTXT, IHEAD, 
     1      iwork, 2, IER ) 
C 
      ICHAN5 = 15 
      ICHAN8 = 18 
        CALL openf (ichan5,LNMASK,0,256,2,IER) 
        CALL Savwrk(ictsub,npolys,ichan8,ichan5) 
        CLOSE ( ICTSUB, IOSTAT=IER ) 
        CLOSE ( ICHAN8, IOSTAT=IER ) 
C.......READ HEADER FOR UPDATING WORK FILE SHORTLY 
        CALL CLEAR ( IHEAD, 256 ) 
        CALL rdblk ( ICHAN5, 1, IHEAD( 1 ), 1, IER ) 
        CALL rdblk ( ICHAN5, 2, IHEAD(129), 1, IER ) 
C.......NOW FOR ASSIGNING THE MAP AND SPECIFIC SUBJECTS 
        IHEAD(120) = 5 
        CALL WRBLK ( ICHAN5, 1, IHEAD( 1 ), 1, IER ) 
C.......NOW FOR SUBJECTS 
        N = IHEAD(67) 
        CALL CLEAR ( IBUFF(1), 128 ) 
        DO 500 I = 3, N+2 
          CALL rdblk ( ICHAN5, I, IBUFF, 1, IER ) 
          WRITE(INRD,397) ICC 
          READ(INRD,397) ICC 
  397     FORMAT(I5) 
          IBUFF(20) = 1 
          IBUFF(21) = 1 
          DO 400 IDD = 1, NSUBS 
            IF ( ICC.NE.ISUBJ(IDD) ) GO TO 400 
              IBUFF(20) = 5 
              GO TO 425 
  400     CONTINUE 
  425     CALL WRBLK ( ICHAN5, I, IBUFF, 1, IER ) 
  500   CONTINUE 
        CLOSE (ICHAN5,IOSTAT=IIER) 
C.......UNPACK MAPNAME AND UPDATE WORK FILE 
        CALL UNPACK ( LNMASK, 24 ) 
        CALL TUPDIR ( ITP1, ITP2, ITP3, LNMASK, IHEAD, 
     1     iwork,2, IER )
        close(icwork)
        write(*,501)(lnmask(i),i=1,8),(mtxt(i),i=1,13)
501   format(/,' MASKED LINE MAP CREATED: ',8A1,/,
     + ' ELEVATION TEXT FILE CREATED: ',13A1)
        GO TO 9000 
C 
C.....ERROR RETURNS 
C 
 3000 WRITE(NPRNT,3001) IER 
 3001 FORMAT(' ERROR ',I5,' OPENING FAST FILE  *AUTOLB*') 
      GO TO 9000 
C 
 3010 WRITE(NPRNT,3011) 
 3011 FORMAT(' ERROR:  NO WINDOW SET  *AUTOLB*') 
      GO TO 9000 
C 
 3020 WRITE(NPRNT,3021) IER 
 3021 FORMAT(' ERROR ',I5,' FROM LBPARM  *AUTOLB*') 
      GO TO 9000 
C 
 3030 WRITE(NPRNT,3031) IER 
 3031 FORMAT(' ERROR ',I5,' OPENING TEMP- FILES  *AUTOLB*') 
      GO TO 9000 
C 
 3040 WRITE(NPRNT,3041) IER 
 3041 FORMAT(' ERROR ',I5,' INITIALIZING FONTS FILE  *AUTOLB*') 
      GO TO 9000 
C 
 3050 WRITE(NPRNT,3051) IER 
 3051 FORMAT(' ERROR ',I5,' OPENING DISK ARRAYS  *AUTOLB*') 
      GO TO 9000 
C 
 3060 WRITE(NPRNT,3500) IER,(LCOM(I),I=1,15) 
 3500 FORMAT(' ERROR ',I5,' OPENING FILE ',15A2,'  *AUTOLB*') 
      GO TO 9000 
C 
 3070 WRITE(NPRNT,3071) IER 
 3071 FORMAT(' ERROR ',I5,' FROM  gtnam OF M.A. FILE  *AUTOLB*') 
      GO TO 9000 
C 
 3080 WRITE(NPRNT,3081) IER 
 3081 FORMAT(' ERROR ',I5,' OPENING OF M.A. FILE  *AUTOLB*') 
      GO TO 9000 
C 
 3090 WRITE(NPRNT,3091) 
 3091 FORMAT(' ERROR:  MULTIPLE ATTRIBUTE NOT FOUND  *AUTOLB*') 
      go to 9000 
C 
 4000 WRITE(NPRNT,4001) ier 
 4001 FORMAT(' ERROR  FROM  rdblk  ERROR',I5,'  *AUTOLB*') 
      go to 9000 
C 
 4020 WRITE(NPRNT,4021) ier 
 4021 FORMAT(' ERROR  FROM  openf OF TEXT MAP  ERROR',I5,'  *AUTOLB*') 
C 
C.....NOW CLOSE ALL THE OPEN CHANNELS, WRITE OUT COMMONS AND RETURN TO MOSS 
 9000 CONTINUE 
C 
      END 
