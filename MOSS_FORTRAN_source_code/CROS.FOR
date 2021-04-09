      SUBROUTINE cros(x1,y1,x2,y2,npts,xa,ya,nseg)
      COMMON /io    / nprnt , ioin
$Include:'xypnts.inc'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C                  S U B R O U T I N E   C R O S
C
C  FUNCTION:  DETERMINE ALL INTERSECTIONS OF A POLYGON WITH A LINE
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      COMMON /dot   / nslands , jh , ids(1)
      DIMENSION xa(1) , ya(1) , xi(2) , yi(2) , xl(2) , yl(2)
      nseg = 1
      xa(1) = x1
      ya(1) = y1
      xi(1) = x1
      yi(1) = y1
      xi(2) = x2
      yi(2) = y2
      CALL spoints('X ',x,xl(2),1,1)
      CALL spoints('Y ',y,yl(2),1,1)
c        XL(2)=X(1)
c        YL(2)=Y(1)
      isl = 1
      nsl = nslands
C  LOOP # SEGMENTS IN POLYGON
      DO is = 2 , npts
         xl(1) = xl(2)
         yl(1) = yl(2)
         CALL spoints('X ',x,xl(2),is,1)
         CALL spoints('Y ',y,yl(2),is,1)
c        XL(2)=X(IS)
c        YL(2)=Y(IS)
C          SKIP ISLANDS HERE
C***********************************************************************
C  NO ISLANDS
         IF ( nsl.NE.0 ) THEN
            IF ( ids(isl).EQ.is ) THEN
C  SKIP LINE BETWEEN ISLANDS
               isl = isl + 1
               IF ( isl.GT.nslands ) nsl = 0
               GOTO 100
            ENDIF
         ENDIF
C***********************************************************************
         CALL sinter(xi,yi,xl,yl,xint,yint,iflag,0.)
C          IFLAG=0=NO INTERSECTION
         IF ( iflag.NE.0 ) THEN
            IF ( nseg.GT.299 ) THEN
               WRITE (*,*) 
     &               ' Exceeded 300 segments in CROS called from LBOOLE'
               GOTO 200
            ENDIF
            nseg = nseg + 1
            xa(nseg) = xint
            ya(nseg) = yint
         ENDIF
 100  ENDDO
 200  CONTINUE
      xa(nseg+1) = x2
      ya(nseg+1) = y2
C  NO NEED TO SORT
      IF ( nseg.GT.2 ) THEN
C          SORT INTERSECTIONS BY DISTANCE FROM 1st POINT
 250     CONTINUE
         iflag = 0
         DO i = 2 , nseg - 1
            d1 = (xa(i)-x1)**2 + (ya(i)-y1)**2
            d2 = (xa(i+1)-x1)**2 + (ya(i+1)-y1)**2
            IF ( d1.GT.d2 ) THEN
C  SWITCH ORDER
               iflag = 1
               t = xa(i)
               xa(i) = xa(i+1)
               xa(i+1) = t
               t = ya(i)
               ya(i) = ya(i+1)
               ya(i+1) = t
            ENDIF
         ENDDO
C  SORT AGAIN
         IF ( iflag.NE.0 ) GOTO 250
      ENDIF
C          ELIMINATE DUPLICATE POINTS
      nseg = nseg + 1
      is = 0
 300  CONTINUE
      is = is + 1
      IF ( is.NE.nseg ) THEN
         d = (xa(is)-xa(is+1))**2 + (ya(is)-ya(is+1))**2
C  >= .1 METER APART
         IF ( d.GE..01 ) GOTO 300
C          ELIMINATE POINT
         nseg = nseg - 1
         IF ( is.NE.nseg ) THEN
            DO i = is + 1 , nseg
               xa(i) = xa(i+1)
               ya(i) = ya(i+1)
            ENDDO
            GOTO 300
         ENDIF
      ENDIF
      nseg = nseg - 1
      RETURN
      END
