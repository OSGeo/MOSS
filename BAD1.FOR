C***************************************************************************
C
C  Name:     B A D 1
C
C  Purpose:  THIS ROUTINE COUNTS THE LINE SEGMENTS OF THE CONTOUR THAT
C            INTERSECT WITH ANY EDGE OF THE LABEL BOX. IF ANY LONG EDGE
C            IS INTERSECTED, OR MORE THAN TWO INTERSECTION ON SHORT
C            EDGES, BAD1 IS SET TO TRUE (BAD BOX LOCATION).
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     BAD1              Out           Logical Entry - Result of function
C     TEMPBX            In            Real Array(8) - Box containing label
C     IL                In            Integer - 'Left' index into disk arrays
C     IR                In            Integer - 'Right' index into disk arrays
C     NCORDS            In            Integer - # of coordinates in feature
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C
C  -- Stack Variables --
C     KRRNT                           Integer
C     I                               Integer
C     IFLAG                           Integer
C     PREV                            Integer
C     LONGCT                          Integer
C     SHRTCT                          Integer
C     FIRST                           Logical
C     SECOND                          Logical
C     THIRD                           Logical
C     FOURTH                          Logical
C     XI                              Real Array             4
C     YI                              Real Array             4
C     XL                              Real Array             4
C     YL                              Real Array             4
C     DUM1                            Real
C     DUM2                            Real
C
C******************************************************************************
C
      LOGICAL FUNCTION bad1(tempbx,il,ir,ncords,ier)
C
$Include:'xypnts.inc'
      COMMON /io    / nprnt , ioin
C
      INTEGER krrnt , i , iflag , prev , ncords , longct , shrtct
      DIMENSION tempbx(8) , xi(2) , yi(2) , xl(2) , yl(2)
      REAL tempbx , xi , yi , xl , yl , dum1 , dum2
      LOGICAL first , second , third , fourth
C
      ier = 0
      longct = 0
      shrtct = 0
      first = .TRUE.
      second = .FALSE.
      third = .FALSE.
      fourth = .FALSE.
C
      IF ( ncords.LE.2 ) THEN
         bad1 = .FALSE.
         GOTO 200
      ELSE
C
C.......LOOP THRU ALL SEGS AGAINST FIRST LONG EDGE OF LABEL BOX
         xi(1) = tempbx(1)
         xi(2) = tempbx(7)
         yi(1) = tempbx(2)
         yi(2) = tempbx(8)
         krrnt = 1
      ENDIF
C
 100  CONTINUE
      prev = krrnt
C
      krrnt = krrnt + 1
      IF ( krrnt.EQ.il+1 ) krrnt = ir + 1
      IF ( krrnt.EQ.ir+1 ) prev = ir
      IF ( krrnt.LE.ncords ) THEN
C
         CALL spoints('X ',x,xl(1),krrnt,1)
         CALL spoints('X ',x,xl(2),prev,1)
         CALL spoints('Y ',y,yl(1),krrnt,1)
         CALL spoints('Y ',y,yl(2),prev,1)
c        XL(1) = X(KRRNT)
c        XL(2) = X(PREV)
c        YL(1) = Y(KRRNT)
c        YL(2) = Y(PREV)
C
C.......SEE IF CURRENT SEGMENT INTERSECTS WITH THIS LONG EDGE
C
         CALL sinter(xi,yi,xl,yl,dum1,dum2,iflag,0.)
C
         IF ( (iflag.GT.0) .AND. ((first) .OR. (second)) )
     &         longct = longct + 1
         IF ( (iflag.GT.0) .AND. ((third) .OR. (fourth)) )
     &         shrtct = shrtct + 1
         IF ( (longct.GT.1) .OR. (shrtct.GT.0) ) THEN
            bad1 = .TRUE.
            GOTO 200
C
         ELSEIF ( krrnt.LT.ncords ) THEN
            GOTO 100
         ENDIF
      ELSEIF ( longct.GT.1 .OR. shrtct.GT.0 ) THEN
C
         bad1 = .TRUE.
         GOTO 200
      ENDIF
C
C.....DO PASS TO WITH ALTERNATE LONG EDGE IF HAVE NOT DONE IT YET
C
      IF ( first ) THEN
C
C.....IF FIRST TIME THRU, MAKE LONG EDGE OTHER LONG EDGE OF BOX
C
         first = .FALSE.
         xi(1) = tempbx(3)
         xi(2) = tempbx(5)
         yi(1) = tempbx(4)
         yi(2) = tempbx(6)
         second = .TRUE.
         krrnt = 1
         GOTO 100
C
C.....DO PASS TO WITH FIRST SHORT EDGE IF HAVE NOT DONE IT YET
C
      ELSEIF ( second ) THEN
C
C.....IF SECOND TIME THRU, MAKE SHORT EDGE FIRST SHORT EDGE OF BOX
C
         second = .FALSE.
         xi(1) = tempbx(1)
         xi(2) = tempbx(3)
         yi(1) = tempbx(2)
         yi(2) = tempbx(4)
         third = .TRUE.
         krrnt = 1
         GOTO 100
C
C.....DO PASS TO WITH SECOND SHORT EDGE IF HAVE NOT DONE IT YET
C
      ELSEIF ( .NOT.(third) ) THEN
C
         bad1 = .FALSE.
      ELSE
C
C.....IF THIRD TIME THRU, MAKE SHORT EDGE OTHER SHORT EDGE OF BOX
C
         third = .FALSE.
         xi(1) = tempbx(5)
         xi(2) = tempbx(7)
         yi(1) = tempbx(6)
         yi(2) = tempbx(8)
         fourth = .TRUE.
         krrnt = 1
         GOTO 100
      ENDIF
C
 200  CONTINUE
      RETURN
      END
