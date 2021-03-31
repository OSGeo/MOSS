      INTEGER FUNCTION pnpoly(xp,yp,n)
C
C *****************************************************
C
C            F U N C T I O N   P N P O L Y
C FUNCTION:
C          THIS ROUTINE IS LOOKING TO SEE IF POINT (XP,YP) IS IN
C          THE PLOYGON OF (X,Y).
C PARAMETERS:
C       (XP,YP)   IN - POINT TO TEST FOR INCLUSION IN POLYGON
C       N         IN - NUMBER OF COORDINATES IN POLYGON
C       PNPOLY     OUT- 0,IF POINT NOT IN POLYGON
C ***********************************************************
C
C      COMMON /DEBUG/ IDEB
C      COMMON /IO/ NPRNT,IOIN
$Include:'xypnts.inc'
      LOGICAL ix , iy , ipip , ltest
C
      ipip = .FALSE.
      ltest = .FALSE.
C
      nsides = n - 1
C
      pnpoly = -1
C
      DO i = 1 , nsides
         ix = .FALSE.
         iy = .FALSE.
         CALL spoints('X ',x,x1,i,1)
         CALL spoints('Y ',y,y1,i,1)
         CALL spoints('X ',x,x2,i+1,1)
         CALL spoints('Y ',y,y2,i+1,1)
c      X1=X(I)
c      Y1=Y(I)
c      X2=X(I+1)
c      Y2=Y(I+1)
         IF ( ((y1.LE.yp) .AND. (yp.LT.y2)) .OR. 
     &        ((y2.LE.yp) .AND. (yp.LT.y1)) ) iy = .TRUE.
         IF ( ((x1.LE.xp) .AND. (xp.LE.x2)) .OR. 
     &        ((x2.LE.xp) .AND. (xp.LE.x1)) ) ix = .TRUE.
         IF ( .NOT.(ix .AND. iy) ) THEN
            IF ( .NOT.(((y2.EQ.y1) .AND. ix) .OR. ((x2.EQ.x1) .AND. iy)
     &            .OR. ((x2.NE.x1) .AND. ix)) ) GOTO 50
         ENDIF
         IF ( abs((y2-y1)*(xp-x1)-(yp-y1)*(x2-x1)).LT.0.0001 ) THEN
            ltest = .TRUE.
            GOTO 100
         ENDIF
 50      CONTINUE
         IF ( iy ) THEN
            IF ( x1+(yp-y1)*(x2-x1)/(y2-y1).GT.xp ) ipip = .NOT.ipip
         ENDIF
      ENDDO
 100  CONTINUE
      IF ( ipip ) pnpoly = 1
      IF ( ltest ) pnpoly = 1
 
C
C
      RETURN
      END
