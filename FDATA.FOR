C***********************************************************************
C
C     NAME:  SUBROUTINE FDATA
C
C     FUNCTION:  THIS ROUTINE DETERMINES THE STATUS OF EACH ENDPOINT
C                OF EACH FAULT LINE.  THE ENDPOINTS MAY EITHER BE "FREE"
C                OR IT MAY INTERSECT ANOTHER FAULT LINE OR THE MAP MBR.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IER       O      INTEGER
C                  ERROR INDICATOR (1 => NORMAL)
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  GETFAST, RDBLK, CORDXY, SINTER
C
C     CALLING ROUTINES:  GRIDMK
C
C***********************************************************************
C
      SUBROUTINE FDATA(IER)
C
$Include:'xypnts.inc'
       COMMON /FLTDAT/ FMAPID,ICHF,ISTAR,ISTOP,NTOT,IFDAT(2,64)
       COMMON /WINDOW/ AWIND(4)
C
       INTEGER     FMAPID,IBUFF(128),IWIND(8),IWND(8)
       REAL        XII(2),YII(2),XIN(2),YIN(2),WIND(4),WND(4),
     +             XL(2),YL(2)
       EQUIVALENCE (IWIND(1),WIND(1)),(IWND(1),WND(1))
C
       PARAMETER   (TOL = 0.5)
       PARAMETER   (PTOL = 0.50)
       IER = 0
       IFLT = 0
C
C....USE THE WINDOW OF THE ACTIVE DATA SETS AS MBR
C
       XMIN = AWIND(1)
       XMAX = AWIND(2)
       YMIN = AWIND(3)
       YMAX = AWIND(4)
C
C....INITIALIZE FAULTS TO BE "FREE"
C
       DO 5 I = 1,NTOT
         DO 4 J = 1, 2
    4      IFDAT(J,I) = 0
    5  CONTINUE
C
       DO 10 I = ISTAR,ISTOP
         IFLT = IFLT + 1
         CALL GETFAST(I,IWIND,INDX,ISUBP,IEX1,IEX2,IER)
         IF (IER .NE. 0) GO TO 9000
         CALL RDBLK(ICHF,INDX,IBUFF,128,IER)
         IF ( IER.NE.0 ) GO TO 9010
         CALL CORDXY(IBUFF,NPNTS,INDX,WIND(1),WIND(3),ICHF)
C
         call spoints('X ',x,xii(1),1,1)
         call spoints('X ',x,xii(2),2,1)
         call spoints('Y ',y,yii(1),1,1)
         call spoints('Y ',y,yii(2),2,1)
c         XII(1) = X(1)
c         XII(2) = X(2)
c         YII(1) = Y(1)
c         YII(2) = Y(2)
         call spoints('X ',x,xin(1),npnts,1)
         call spoints('X ',x,xin(2),npnts-1,1)
         call spoints('Y ',y,yin(1),npnts,1)
         call spoints('Y ',y,yin(2),npnts-1,1)
c         XIN(1) = X(NPNTS)
c         XIN(2) = X(NPNTS - 1)
c         YIN(1) = Y(NPNTS)
c         YIN(2) = Y(NPNTS - 1)
C
C....FOR EACH END OF THE FAULT BEING CONSIDERED, DETERMINE IF IT
C....INTERSECTS WITH A MAP EDGE.
C
         DO 15 JN = 1,2
C
C....CALCULATE THE X,Y ENDPOINTS OF THE LINE FORMED BY THE MAP MBR.
C....DO THIS 4 TIMES - ONCE FOR EACH SIDE OF THE MAP.
C
           DO 20 IN = 1,4
             IF (IN .GT. 1) GO TO 21
             XL(1) = XMIN
             XL(2) = XMAX
             YL(1) = YMIN
             YL(2) = YMIN
             GO TO 25
 21          IF (IN .GT. 2) GO TO 22
             XL(1) = XMIN
             XL(2) = XMIN
             YL(1) = YMIN
             YL(2) = YMAX
             GO TO 25
 22          IF (IN .GT. 3) GO TO 23
             XL(1) = XMIN
             XL(2) = XMAX
             YL(1) = YMAX
             YL(2) = YMAX
             GO TO 25
 23          XL(1) = XMAX
             XL(2) = XMAX
             YL(1) = YMIN
             YL(2) = YMAX
C
 25          IF (JN .EQ. 2)  GO TO 26
             CALL SINTER(XII,YII,XL,YL,XTMP,YTMP,IFLAG,TOL)
             IF (IFLAG .EQ. 0) GO TO 20
             IFDAT(JN,IFLT) = 1
             GO TO 15
C            GO TO 20
C
 26          CALL SINTER(XIN,YIN,XL,YL,XTMP,YTMP,IFLAG,TOL)
             IF (IFLAG .EQ. 0) GO TO 20
             IFDAT(JN,IFLT) = 1
             GO TO 15
C
 20        CONTINUE
 15      CONTINUE
C
C....IF BOTH ENDS OF THE FAULT INTERSECT THE MAP BOUNDARY, THEN GO TO
C.... NEXT FAULT LINE.
C
         IF  (IFDAT(1,IFLT) .EQ. 1 .AND. IFDAT(2,IFLT) .EQ. 1) GO TO 10
C
C....AT LEAST ONE OF THE ENDS OF THE FAULT DOES NOT INTERSECT THE MAP.
C....NOW CHECK TO SEE IF IT INTERSECTS ANY OTHER FAULTS.
C
C...LOOP THROUGH ALL OTHER FAULTS
C
         DO 45 J = ISTAR,ISTOP
           IF (I .EQ. J) GO TO 45
           CALL GETFAST(J,IWND,INDX,ISUBP,IEX1,IEX2,IER)
           IF (IER .NE. 0) GO TO 9000
C
C....DETERMINE IF THE 2 MBR'S INTERSECT. IF SO, INVWND = 1.
           IF ( INVWND(WND(1), WND(2), WND(3), WND(4),
     +                 WIND(1),WIND(2),WIND(3),WIND(4)).NE.1 ) GO TO 45
             CALL RDBLK(ICHF,INDX,IBUFFT,128,IER)
             CALL CORDXY(IBUFFT,NPNTS,INDX,WND(1),WND(3),ICHF)
C
C...NOW DETERMINE IF THE 2 LINE SEGMENTS ACTUALLY INTERSECT.
C
             call spoints('X ',x,xl(2),1,1)
             call spoints('Y ',y,yl(2),1,1)
c             XL(2) = X(1)
c             YL(2) = Y(1)
           DO 55 N = 2,NPNTS
             XL(1) = XL(2)
             call spoints('X ',x,xl(2),n,1)
c             XL(2) = X(N)
             YL(1) = YL(2)
             call spoints('Y ',y,yl(2),n,1)
c             YL(2) = Y(N)
             IF (IFDAT(1,IFLT) .EQ. 1) GO TO 43
             CALL SINTER(XII,YII,XL,YL,XTMP,YTMP,IFLAG,TOL)
             IF (IFLAG .EQ. 0)  GO TO 43
             IFDAT(1,IFLT) = 1
             GO TO 50
C
 43          IF (IFDAT(2,IFLT) .EQ. 1) GO TO 47
             CALL SINTER(XIN,YIN,XL,YL,XTMP,YTMP,IFLAG,TOL)
             IF (IFLAG .EQ. 0)  GO TO  47
             IFDAT(2,IFLT) = 1
             GO TO 50
C
 47          IF (IFDAT(1,IFLT).EQ.1 .AND. IFDAT(2,IFLT).EQ.1) GO TO 10
C
 55        CONTINUE
C
 50        IF (IFDAT(1,IFLT) .EQ. 1 .AND. IFDAT(2,IFLT) .EQ. 1) GO TO 10
C
 45      CONTINUE
C
 10    CONTINUE
C
C.....NORMAL RETURN
C
      GO TO 90000
C
 9000 WRITE(*,9001) IER
 9001 FORMAT(' *FDATA* ERROR FROM GETFAST - ERROR ',I6)
      GO TO 90000
C
 9010 WRITE(*,9011) IER
 9011 FORMAT(' *FDATA* ERROR FROM RDBLK   - ERROR ',I6)
90000 RETURN
      END
