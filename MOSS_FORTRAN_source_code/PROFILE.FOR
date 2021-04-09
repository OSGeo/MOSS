C
C           P R O G R A M   P R O F I L E
C
C   FUNCTION - GENERATE A PROFILE PLOT FROM EITHER
C              CELL OR LINE MAP
C
C**************************************************************
C
      PROGRAM PROFILE
C
C
$Include:'mfast.inc'
      COMMON /IO/ NPRNT,IOIN
      COMMON /WHMAP/ ISTRCT(300),NSTRCT
      COMMON /WORK/  IBUFF(128),MAP(15),ICOM(10),JCOM(10)
      COMMON /STRNG/ ICARD(80),ICARDP,LENICD
      COMMON /TYPE/  IDATYP(40),NACTS
      COMMON /DBCHAN/ ICMAST,ICWORK,ICDCFA,ICDCDT,IDESFA(10),IDESDT(10)
      COMMON /PVALSS/XDATA(512),YDATA(512),ZDATA(512),NVALS,MXVALS
      COMMON /FLS/   IUNIT,IFILE(5)
      COMMON /PRFDATA/ ZMAX,ZMIN,MAXDIST,VERTX,ZRINC,ZRANGE,ZFAC,XRINC
      common /dsbuf/ mlen,idata(8192)
      INTEGER*2 IHEAD(256),MAPX(6),FONT(10),len(2),ixxx(2),mapnam(25)
      INTEGER*4 IV4
      CHARACTER*1 ANS
      EQUIVALENCE (IHEAD(80),YS), (IHEAD(82),XS)
      DIMENSION MAPHOLD(40),NAME(12),MAPNAME(40)
      DATA SMVAL /.00001/
C
C  SET ARRAY DIMENSIONS
C
      zero = 0.0
      mlen=8193
      izero = 0
      IDEB = 0
      IBLNK = 8224
      IUNIT  = 0
      MXVALS = 512
      MAXIDS = 40
      ZMIN = 9999999.
      ZMAX = -ZMIN
      TOTDIST = 0.0
      MAXDIST = 0
      ISUM = 0
C
C
C  SET CURSOR COUNTER
C
      LINES=4
C
C  LOAD COMMON
C
      call initl(IER)
c
C  RETRIEVE CURRENT VIEWING WINDOW
C
      CALL WHWND(XMIN,YMIN,XMAX,YMAX)
C
C  GET ACTIVE MAP TO PROFILE
C
      CALL DAGET(ICARD,ICARDP,MAPNAME,40,MACTS)
         call plotsym(iunit,-61,zero,zero,zero,zero,
     +        FONT,zero,zero,zero,izero,5,IER)
c
C
C  CHECK FOR INPUT
C
      IF(MACTS.GT.0) GO TO 10
C
C  PROMPT FOR ACTIVE MAP IDs
C
       WRITE(NPRNT,1000)
 1000 FORMAT(' ENTER ACTIVE MAP IDs TO PROFILE'\)
      call readin(ICARD,80,ICARDP,LENICD)
      CALL DAGET(ICARD,ICARDP,MAPNAME,40,MACTS)
      IF (MACTS.LE.0) GO TO 900
      IF (MACTS.GT.MAXIDS) GO TO  85050
C
  10  CONTINUE
c
C........GET ACTIVE MAP IDS AND CHECK DATA TYPE FOR VALIDITY & ALL SAME TYPE
C
      DO 11 LL=1 , MACTS
         MAPHOLD(LL)=MAPNAME(LL)
         MAPID=MAPHOLD(LL)
         ITYPE=IDATYP((MAPID/7)+1)
         IF (ITYPE.NE.8  .AND.  ITYPE .NE. 2) GO TO 901
         ISUM=ISUM + ITYPE
         IF ( ISUM .NE. ITYPE * LL) GOTO 85060
 11   CONTINUE
C
C.....PROMPT FOR EXAGGERATION
C
c 771  WRITE(NPRNT,1004)
c 1004 FORMAT(' Enter VERTICAL exaggeration '\)
c      LINES = LINES + 2
c      call readin(ICARD,80,ICARDP,LENICD)
c      CALL FNUM(ICARD,80,VERTX,ICARDP)
c      IF ( ABS(VERTX).LT.SMVAL ) GO TO 771
C
C  SET CURSOR COUNTER
C
C
      WRITE(NPRNT,11000)
11000 FORMAT(/,' Last Map Plotted will REDRAW - Do the following:',/,
     + ' 1) POINT TO TWO LOCATIONS with CURSOR.',/
     + ' 2) Use SPACE BAR to mark the END POINTS.',/,
     + ' 3) After Viewing SURFACE TRACE, hit CR to produce PROFILE.',/,
     + ' 4) After viewing PROFILE - hit CR to quit.',//,
     + '      Hit CR to Proceed.'\)
C
      call initt(1,iunit,ifile)
      CALL SETWND
      LINES=1
      call readin(icard,lenicd,icardp,istop)
C
C
C initilize tek display & open last map
C
991      mapX(1)=2HLM
      jchan=70
      call openf(jchan,mapX,1,0,1,ier)
C
      rewind(jchan)
      write(*,*)char(27),char(12)
      call drfast(jchan,ier)
      close(jchan)
      mapx(1)=2hzs
      call openf(jchan,mapx,3,0,1,ier)
c
      CALL VCURSR(IT,X1,Y1)
      CALL XXX(X1,Y1)
      call flsh
C
      CALL VCURSR(IT,X2,Y2)
      CALL XXX(X2,Y2)
C
      CALL MOVEA(X2,Y2)
      CALL DRAWA(X1,Y1)
C      call flsh
C
C WAIT FOR USER
C
      call anmode
      read(*,'(a1)')ans
      write(*,*)char(27),char(12)
C
C SWITCH IF X1 GT X2
C
      IF (ITYPE .NE. 2) GO TO 28
      IF(Y2.GT.Y1) GO TO 28
      TP=X1
      X1=X2
      X2=TP
      TP=Y1
      Y1=Y2
      Y2=TP
 28   CONTINUE
C
C.....ERASE THE SCREEN , CALCULATE AND PLOT THE DATA
C
c      call openf(icdcfa,IDESFA,1,512,2,IER)
c      IF ( IER.NE.0 ) GO TO 950
C
      DO 8 NN = 1 , 2
        DO 6 LL = 1 , MACTS
C
C........GET ACTIVE MAP IDS AND CHECK DATA TYPE FOR VALIDITY & ALL SAME TYPE
C
         MAPID=MAPHOLD(LL)
C
C........IF ITS A LINE MAP
C
         IF (ITYPE .EQ. 2) THEN
           call gtnam(MAPID,0,IBLNK,MAPNAME,IER)
C
           CALL XCONT(X1,Y1,X2,Y2,MAPID,IER)
           if(ier.eq.99)go to 991
           if(ier.eq.98) goto 90000
           IF (IER .NE. 0 ) GOTO 80100
c
         ELSE
C
C..........NOPE ITS A CELL MAP
           call gtnam(MAPID,5,IBLNK,MAPNAME,IER)
           ICHAN2=2
C
           call adddir(mapname,mapnam)
           CALL DSOPN(ICHAN2,MAPNAM,1,1,1,256,256,IER)
           IF(IER.NE.0) GO TO 85020
C
           CALL DSIN(ICHAN2,IHEAD,1.0,IER)
           IF (IER .NE. 0) GO TO 85040
           CALL DSCLS(ICHAN2,IER)
C
C
C..........GET INFO FROM CELL MAP FILE HEADER
C
           NROWS=IHEAD(60)
           NCOLS=IHEAD(61)
           XSIDE = XS
           YSIDE = YS
           ICTY = IHEAD(62)
C
           CALL  byteit(ihead(185),LEN)
           IBW = 2
           IF (LEN(1) .GE. 16) IBW = 1
           IF (IBW .EQ. 1) LEN(1) = LEN(1)/16
C
C..........NOW OPEN DATA CHANNEL TO CELL FILE
C
           call gtnam(MAPID,ICTY,IBLNK,MAPNAME,IER)
           CALL DSOPN(ICHAN2,MAPNAME,1,1,IBW,LEN(1),128,IER)
           IF (IER.NE.0) GO TO 85020
C
C..........GET THE DATA POINTS
C
           CALL PROFIT(X1,Y1,X2,Y2,XSIDE,YSIDE,XMIN,YMIN,
     +                 NROWS,NCOLS,LEN,ICHAN2,IER)
           IF (IER .NE. 0) GO TO 80100
           CALL DSCLS(ICHAN2,IER)
         END IF
C
 20      IF (NN .EQ. 1) THEN
C
C...........FIRST PASS CALCULATE ZMIN, ZMAX , & MAXDIST
c
            DO 4 II = 1 , NVALS
              ZMIN = MIN (ZMIN,ZDATA(II))
              ZMAX = MAX (ZMAX,ZDATA(II))
 4          CONTINUE
C
            TOTDIST = SQRT((XDATA(1)-XDATA(NVALS))**2+
     +                     (YDATA(1)-YDATA(NVALS))**2)
            MAXDIST = MAX ( TOTDIST,MAXDIST )
            if(maxdist.eq.0)maxdist=1
         ELSE
C
C...........SECOND PASS PLOT EACH PROFILE
            DIST= SQRT((XDATA(1)-XDATA(NVALS))**2+
     +                 (YDATA(1)-YDATA(NVALS))**2)
C
C...........COMPENSATE FOR VARIABLE CELL SIZES
c            STRECH=MAXDIST / DIST
            STRECH = float(MAXDIST) / DIST
C
            DO 30 I=1,NVALS
               YT=(ZDATA(I)-ZMIN)*ZFAC + .8
               DIST= SQRT((XDATA(1)-XDATA(I))**2+
     +                    (YDATA(1)-YDATA(I))**2)
               XT= ( 8. * (DIST * STRECH)/ MAXDIST ) + .75
c        XT = XT * 110.0
c        YT = YT + 100.0
               IF(I.EQ.1) CALL TKPLOT(XT,YT,3)
               IF(I.GT.1) CALL TKPLOT(XT,YT,2)
  30        CONTINUE
C
C...........PLOT THE MAP NAME
C
            II = NVALS/2
            YT=(ZDATA(II)-ZMIN)*ZFAC +.8
            DIST= SQRT((XDATA(1)-XDATA(II))**2+
     +                 (YDATA(1)-YDATA(II))**2)
            XT= ( 8. * (DIST * STRECH)/ MAXDIST ) + .75
            MAPID=MAPHOLD(LL)
            call gtnam(MAPID,0,IBLNK,MAPNAME,IER)
            if( ier .ne. 0 ) goto 85080
            kcnt=0
            DO 53 I = 1, 14
               CALL byteit(mapname(i),IXXX)
               do 54 ill=1,2
               IF ( IXXX(ill) .EQ. 32 .OR. IXXX(ill) .EQ. 0 ) GOTO 55
               kcnt=kcnt+1 
   54       continue
   53       CONTINUE
   55       I = I - 1
            YMATH = YT + 0.05
c            YMATH = YT + 5.0
            ione = 1
            pvar1 = 0.12
            pvar2 = 0.12
            call plotsym(iunit,1,XT,YMATH,pvar1,pvar2,MAPNAME,
     +                   zero,zero,zero,izero,kcnt,IER)
c            call anmode
         END IF
C
  6    CONTINUE
C
       IF ( NN .NE. 2 ) THEN
         call initt(1,iunit,ifile)
         CALL PRFPLT
c
         xx1 =  0.0
         xx2 = 10.0
         yy2 = ( ( zmax - zmin ) * zfac ) + 2.0
c
          call wncht( xx1,xx2, xx1,yy2 )
c
         call plotsym(iunit,-61,zero,zero,zero,zero,5hfonts,
     +               zero,zero,zero,izero,5,IER)
         CALL PRFAXIS
       ENDIF
  8   CONTINUE
C
           call flsh
           read(*,'(a1)')ans
           write(*,*)char(27),char(12)
      GO TO 90000
C
C.....ERROR RETURNS
C
 900  WRITE(NPRNT,3000)
 3000 FORMAT(' *PROFILE*   NO ACTIVE DATA TO PLOT')
      GO TO 90000
C
 901   WRITE(NPRNT,3001) ITYPE
 3001  FORMAT(' *PROFILE*   MUST USE LINE OR CONTINUOUS CELL ',
     +        'MAPS NOT TYPE',I5)
       GO TO 90000
C
 950   WRITE (NPRNT,3003) IER
 3003  FORMAT (' ERROR NUMBER IS ',I5)
       write(*,*)'Could not open .FA file' 
       IER=-1
       GO TO 90000
C
80100  CONTINUE
       WRITE(NPRNT,98010) IER
98010  FORMAT(' *PROFILE*   ERROR FROM PROFIT OR XCONT  ERROR',I5)
       GO TO 90000
C
85020  CONTINUE
       WRITE (NPRNT,98502) (MAPNAME(I),I=1,15),IER
98502  FORMAT(' *PROFILE*   ERROR OPENING FILE ',15A2,'  ERROR',I5)
       GO TO 90000
C
85040  CONTINUE
       WRITE (NPRNT,98504) IER
98504  FORMAT(' *PROFILE*   ERROR FROM DSIN  ERROR',I5)
       GO TO 90000
C
85050  CONTINUE
       WRITE (NPRNT,98505) MAXIDS
98505  FORMAT(' *PROFILE*   MAXIMUM NUMBER OF IDs is ',I5)
       GO TO 90000
C
85060  CONTINUE
       WRITE (NPRNT,98506)
98506  FORMAT(' *PROFILE*   NO MIXING OF CELL AND LINE MAPS ALLOWED ')
       GO TO 90000
85080  CONTINUE
       WRITE (NPRNT,98508) IER
98508  FORMAT(' *PROFILE*   ERROR FROM qgtnam  ERROR',I5)
       GO TO 90000
C
C
C  FINISH UP
C
90000 CONTINUE
      call plotsym(iunit,-999,0.0,0.0,0.0,0.0,FONT,0.0,0.0,0.0,0,0,IER)
      END
