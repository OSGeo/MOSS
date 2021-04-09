      SUBROUTINE bweedt(nvert,tol,nisls,island,rxx,sxx)
$Include:'xypnts.inc'
      COMMON /io    / nprnt , ioin
$Include:'rspnts.inc'
C        DIMENSION R(1),S(1)
      DIMENSION island(1)
C
C *************************************************************
C
C          S U B R O U T I N E     I W E E D I T
C
C FUNCTION:   KILL UN-NEEDED POINTS FROM A LINE.
C
C PARAMETERS:
C           NVERT  IN = NUMBER OF COORDINATES IN INPUT STRING
C                  OUT= RETURNED NUMBER OF COORDINATES IN OUTPUT STRING
C           TOL    IN = WEEDING TOLERENCE IN METERS
C           NISLS   = # ISLANDS IN POLYGON DATA
C           ISLANDS   = OLD ISLAND POINTERS ON INPUT
C***********************************************************************
C          ALGORITHM USED CONSIDERS 3 DATA POINTS
C                 (X1,Y1) - (XP,YP) - (X2,Y2)
C                 The distance between (XP,YP) and the line connecting
C                 (X1,Y1) - (X2,Y2) is calculated by:
C                      DIST=ABS(A*XP + B*YP + C)/SQRT(A**2+B**2)
C                      where the line is of the format:
C                           A*X + B*Y + C = 0
C                      thus:
C                           A = Y1-Y2
C                           B = X2-X1
C                           C = (Y2-Y1)*X1 - (X2-X1)*Y1
C                  Any point (XP,YP) whose DIST>TOL will be ignored.
C***********************************************************************
C
C          CHANGE THE LAST POINT OF EACH POLYGON TO BE -X VALUE
C
      IF ( nisls.NE.0 ) THEN
         DO isl = 1 , nisls
            i = island(isl) - 1
            CALL spoints('X ',x,xi,i,1)
            CALL spoints('X ',x,-xi,i,2)
         ENDDO
      ENDIF
      CALL spoints('X ',x,xnvert,nvert,1)
      CALL spoints('X ',x,-xnvert,nvert,2)
C
C  ENTER WEEDING LOOP
C
      j = 0
      kount = 0
 100  CONTINUE
      j = j + 1
      kount = kount + 1
      CALL spoints('X ',x,xlast,j,1)
      CALL spoints('Y ',y,ylast,j,1)
c        XLAST=X(J)
c        YLAST=Y(J)
      CALL spoints('R ',r,xlast,kount,2)
      CALL spoints('S ',s,ylast,kount,2)
c        R(KOUNT)=XLAST
c        S(KOUNT)=YLAST
 200  CONTINUE
      j = j + 1
      IF ( x(j+1).LT.0. ) THEN
C          THIS IS THE LAST POINT OF A POLYGON  (J+1) IS
C          STORE J & J+1
         DO i = 0 , 1
            kount = kount + 1
            CALL spoints('X ',x,xji,j+i,1)
            CALL spoints('Y ',y,yji,j+i,1)
            CALL spoints('R ',r,xji,kount,2)
            CALL spoints('S ',s,yji,kount,2)
c       R(KOUNT)=X(J+I)
c        S(KOUNT)=Y(J+I)
         ENDDO
         j = j + 1
         IF ( j.NE.nvert ) GOTO 100
      ELSEIF ( j.LT.nvert ) THEN
         CALL spoints('X ',x,xp,j,1)
         CALL spoints('Y ',y,yp,j,1)
c        XP=X(J)
c        YP=Y(J)
         x1 = xlast
         y1 = ylast
         CALL spoints('X ',x,x2,j+1,1)
         CALL spoints('Y ',y,y2,j+1,1)
c        X2=X(J+1)
c        Y2=Y(J+1)
         a = y1 - y2
         b = x2 - x1
         c = -a*x1 - b*y1
         dist = abs(a*xp+b*yp+c)/sqrt(a*a+b*b)
         IF ( dist.GT.tol ) THEN
            kount = kount + 1
            CALL spoints('R ',r,xp,kount,2)
            CALL spoints('S ',s,yp,kount,2)
c        R(KOUNT)=XP
c        S(KOUNT)=YP
            xlast = xp
            ylast = yp
         ENDIF
         GOTO 200
      ENDIF
      isl = 0
      DO i = 1 , kount
         CALL spoints('R ',r,rr,i,1)
         CALL spoints('S ',s,ss,i,1)
c        RR=R(I)
c        SS=S(I)
         IF ( rr.LE.0. ) THEN
            rr = -rr
            IF ( i.NE.kount ) THEN
               isl = isl + 1
               island(isl) = i + 1
            ENDIF
         ENDIF
         CALL spoints('X ',x,rr,i,2)
c  100             x(i)=rr
         CALL spoints('Y ',y,ss,i,2)
C                  y(i)=ss
      ENDDO
      nvert = kount
      RETURN
      END
