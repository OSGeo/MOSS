      FUNCTION inpgon(low,ihi,xp,yp)
C***********************************************************************
C          PURPOSE = DETERMINE IF (XP,YP) IS INSIDE A POLYGON
C***********************************************************************
C          (X,Y)   = ARRAY DEFINING POLYGON
C          NUM     = # POINTS IN POLYGON
C          (XP,YP) = POINT IN QUESTION
C***********************************************************************
C          INPGON = 0 = POINT LIES OUTSIDE POLYGON
C                 = 1 = POINT LIES INSIDE POLYGON
C                 = 2 = POINT LIES ON THE POLYGON BOUNDARY
C***********************************************************************
C
C          COUNT # OF TIMES A HORIZONTAL LINE FROM (XP,YP)-(XP,YP+++)
C          CROSSES THE POLYGON
C
$Include:'xypnts.inc'
      inpgon = 0
      low2 = low + 1
      ihi2 = ihi
      DO i = low2 , ihi2
C          CONSIDER LINE (X(I-1),Y(I-1)) - (X(I),Y(I))
C          IF (XP,YP) IS AN ENDPOINT, IT IS INSIDE POLYGON!!
         CALL spoints('X ',x,x1,i-1,1)
         CALL spoints('Y ',y,y1,i-1,1)
         IF ( xp.EQ.x1 .AND. yp.EQ.y1 ) GOTO 100
C          IS IT POSSIBLE FOR HORIZONTAL LINE TO CROSS THIS LINE?
c        X1=X(I-1)
c        Y1=Y(I-1)
         CALL spoints('X ',x,x2,i,1)
         CALL spoints('Y ',y,y2,i,1)
c        X2=X(I)
c        Y2=Y(I)
         IF ( y2.LT.y1 ) THEN
            CALL spoints('X ',x,x1,i,1)
            CALL spoints('Y ',y,y1,i,1)
            CALL spoints('X ',x,x2,i-1,1)
            CALL spoints('Y ',y,y2,i-1,1)
         ENDIF
c        X1=X(I)
c        Y1=Y(I)
c        X2=X(I-1)
c        Y2=Y(I-1)
         IF ( yp.GT.y1 .AND. yp.LE.y2 ) THEN
            dx = x2 - x1
            IF ( dx.NE.0. ) THEN
C          CALCULATE INTERSECTION
               xint = x1 + (yp-y1)*dx/(y2-y1)
C                    LEFT    , ON LINE , RIGHT
               IF ( xp.LT.xint ) THEN
                  inpgon = 1 - inpgon
               ELSEIF ( xp.EQ.xint ) THEN
                  GOTO 100
               ENDIF
C          VERTICAL LINE IS EASY
C                  LEFT    , ON LINE , RIGHT
            ELSEIF ( xp.LT.x1 ) THEN
               inpgon = 1 - inpgon
            ELSEIF ( xp.EQ.x1 ) THEN
               GOTO 100
            ENDIF
         ELSEIF ( yp.EQ.y1 .AND. yp.EQ.y2 ) THEN
C          HORIZONTAL LINE, DOES (XP,YP) LIE ON THIS LINE??
C  YES
            IF ( xp.GE.x1 .AND. xp.LE.x2 ) GOTO 100
C  YES
            IF ( xp.GE.x2 .AND. xp.LE.x1 ) GOTO 100
         ENDIF
      ENDDO
      RETURN
C  ON THE POLYGON
 100  CONTINUE
      inpgon = 2
      RETURN
      END
