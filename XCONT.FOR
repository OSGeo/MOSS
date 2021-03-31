CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SUBROUTINE XCONT
C
C     FUNCTION: FIND THE INTERSECTIONS OF CONTOUR LINES WITH A PROFILE
C
C     PARAMETER  USE      DESCRIPTION
C     ---------  ---      -----------
C
C        X1      INPUT    X COORD OF PROFILE START POINT
C        Y1      INPUT    Y           "
C        X2      INPUT    X           "      END     "
C        Y2      INPUT    Y           "
C        MAPID   INPUT    ACTIVE MAP ID
C        IER     OUTPUT   ERROR INDICATOR
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE XCONT(X1,Y1,X2,Y2,MAPID,IER)
$INCLUDE: 'XYPNTS.INC'
$include: 'mfast.inc'
      COMMON /IO/ NPRNT,IOIN
      COMMON /PVALSS/ XDATA(512),YDATA(512),ZDATA(512),NVALS,MXVALS
C
      INTEGER IBUFF(128),MAP(25),mapname(25)
      REAL IWIND(4), XLINE(2), YLINE(2), XI(2), YI(2)
C.....XLINE, YLINE HOLD ENDPTS OF CROSS SECTION LINE
C.....XI, YI HOLD SEGMENT ENDPOINTS
      INTEGER OLDVALS
      INTEGER ELEVATION,EL
      INTEGER SUBJECT(80)
C.....30 CHAR SUBJECT
       character*1 ans
C.....INITIALIZE
      IBLNK = 8224
      OLDVALS = 0
      ier=0
C
      call apget(MAPID,IND1,ISTOP,NTOTAL)
C
C.....OPEN DATA CHANNEL TO THIS MAP
C
      ICHAN=5
      call gtnam(MAPID,0,IBLNK,MAP,IER)
      call adddir(map,mapname)
      call openf(ICHAN,MAPname,1,256,2,IER)
      IF(IER.NE.0) GO TO 900
C
C.....GET DATA
C
      NVALS = 0
      DO 15 J=IND1,ISTOP
         CALL GETFAST(J,IWIND,IDNDEX,ISTPT,IEX1,IEX2,IER)
         IF(IER.NE.0) GO TO 910
C
C  CHECK MBR OVERLAP BETWEEN PROFILE LINE AND FEATURE MBR
C
         Y1TEST = Y1
         Y2TEST = Y2
         X1TEST = X1
         X2TEST = X2
         IF( Y2 .LT. Y1 ) Y1TEST = Y2
         IF( Y2 .LT. Y1 ) Y2TEST = Y1
         IF( X2 .LT. X1 ) X1TEST = X2
         IF( X2 .LT. X1 ) X2TEST = X1
C
C........FEATURE WITH PLACES POINTED TO ?
C
         IF( INVWND( X1TEST, X2TEST, Y1TEST, Y2TEST, IWIND(1), IWIND(2),
     1       IWIND(3),IWIND(4)).EQ.0) GO TO 15
C
C........MBRS OVERLAP  FOR THIS ITEM.
C........STORE SOME RATHER CRITICAL INFORMATION IN CASE THIS IS A HIT
C
         call rdblk(ICHAN,IDNDEX,IBUFF,1,IER)
         ITEMN=IBUFF(5)
         ICTPT=IBUFF(23)
         ISTPT=IBUFF(24)
C
C  SET UP COORD ARRAYS TO PASS TO SINTER
C
        call cordxy(IBUFF,NPTS,IDNDEX,IWIND(1),IWIND(3),ICHAN)
C
C    ENSURE PROFILE LINE IS NOT EXACTLY VERTICAL
C
         IF( X1 .EQ. X2 ) X2 = X2 + 1
         XLINE(1) = X1
         XLINE(2) = X2
         YLINE(1) = Y1
         YLINE(2) = Y2
C
C  FIND INTERSECTION POINTS, IF ANY
C
         DO 10 NUMSEG = 1, NPTS - 1
           call spoints('X ',x,xi(1),numseg,1)
           call spoints('Y ',y,yi(1),numseg,1)
           call spoints('X ',x,xi(2),numseg+1,1)
           call spoints('Y ',y,yi(2),numseg+1,1)
c           XI(1) = X(NUMSEG)
c           YI(1) = Y(NUMSEG)
c           XI(2) = X(NUMSEG + 1)
c           YI(2) = Y(NUMSEG + 1)
           CALL SINTER(XI,YI,XLINE,YLINE,XINT,YINT,IFLAG,1)
           IF( IFLAG .EQ. 0 )
     +        GO TO 10
C          ELSE   
C THERE IS AN INTERSECTION - RECORD COORDS
              IF (NVALS .GT. MXVALS) GO TO 80020
              NVALS = NVALS + 1
              XDATA(NVALS) = XINT
              YDATA(NVALS) = YINT
   10    CONTINUE
C
C.........RETRIEVE SUBJECT FOR CONTOUR LINE + CONVERT TO NUMERIC Z VAL
C
         IF( NVALS .LE. OLDVALS )
     +     GO TO 15
C........ELSE
           FIRSTNEW = OLDVALS + 1
C..........READ SUBJECT RECORD
           CALL GETSUB( ICHAN, ISTPT, IBUFF, SUBJECT, NUMITEMS )
           CALL UNPACK( SUBJECT, 80 )
           K=0
           DO 11 K=0,10
             IF( SUBJECT(K+1) .NE. 32 ) then
               kkk = k + 1
               GO TO 12
               endif
c
   11      CONTINUE
C
   12   k=1
        CALL INUM( SUBJECT(kkk), ELEVATION, k )
        if(k.lt.0)then
          Write(*,*)' Not able to continue - SUBJECTS must be numeric.'
          ier=1
          return
        endif
C
        DO 13 ITEM = FIRSTNEW, NVALS
   13     ZDATA(ITEM) = ELEVATION
        OLDVALS = NVALS
   15 CONTINUE
C
C  BUBBLESORT BY X COORD, MAKING PARALLEL SWAPS IN ALL THREE ARRAYS
C
C Must have at least two crossings
C 
      if(nvals.lt.2)then
       Write(*,*)' Must have at least two different line crossings.'
       write(*,*)' Hit <CR> to continue or Q<CR2> to quit PROFILE:'
       read(*,'(a1)')ans
       ier=99
       if ((ans .eq. 'Q') .or. (ans .eq. 'q')) ier=98
      return
      endif
C
      DO 21 LIM = 1, NVALS - 1
        DO 20 EL = NVALS - 1, LIM, -1
          IF( XDATA( EL ) .LE. XDATA( EL + 1 ) )
     &      GO TO 19
C         ELSE
            TEMP = XDATA( EL )
            XDATA( EL ) = XDATA( EL + 1 )
            XDATA( EL + 1 ) = TEMP
            TEMP = YDATA( EL )
            YDATA( EL ) = YDATA( EL + 1 )
            YDATA( EL + 1 ) = TEMP
            TEMP = ZDATA( EL )
            ZDATA( EL ) = ZDATA( EL + 1 )
            ZDATA( EL + 1 ) = TEMP
   19     CONTINUE
C END IF
   20   CONTINUE
C END DO
   21 CONTINUE
C END DO
C
      CLOSE (ICHAN,IOSTAT=IIER)
      GO TO 90000
C
C  ERROR RETURNS
C
 900  WRITE (NPRNT,901) IER
 901  FORMAT (' ERROR NUMBER IS ',I5)
      write(*,*)'Error on accessing input file'
      IER = -1
      GO TO 90000
C
  910 WRITE(NPRNT,3000) IER
3000  FORMAT(' *XCONT*  ERROR FROM GETFAST  ERROR',I5)
      GO TO 90000
C
80020  CONTINUE
       WRITE(NPRNT,98002) MXVALS
98002  FORMAT(' *XCONT*  TO MANY VALUES ALONG VECTOR  MAXIMUM IS',I5)
       IER = 3
       GO TO 90000
C
90000  CONTINUE
       RETURN
       END
