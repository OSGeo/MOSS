C**
C          DRAW A DASHED LINE FROM WHERE YOU ARE (WHERE(XL,YL))
C          TO (X,Y)
C          BUT ALL THIS CURRENTLY DOES IS DRAW SOLID LINES
C
C          IDASH = 2 = SOLID LINE
C          IDASH = 3 = PEN UP
C          IDASH = 4 = DOTTED DASHED
C          IDASH = 5 = CENTERLINE (LONG-SHORT)
C          IDASH = 6 = MEDIUM DASHES
C          IDASH = 7 = LONG   DASHES
C**
C
C       XPAGE, YPAGE ARE THE COORIDINATES OF THE POINT TO WHICH THE
C               DASHED LINE IS TO BE DRAWN.
C       DASH IS THE LENGTH OF THE DASH AND SPACE BETWEEN DASHES.
C
C       A DASHED LINE IS DRAWN (INCHES) FROM CURRENT PEN POSITION TO
C       THE SPECIFIED XPAGE, YPAGE.  THE SIZE OF THE DASH WILL BE AS
C       CALLED FOR EXCEPT IF THE LINE LENGTH IS LESS THAN DOUBLE THE
C       DASH SIZE THE DASH IS ADJUSTED TO HALF THE LINE LENGTH.
C       DETERMINE CURRENT PEN POSITION.
C
      SUBROUTINE comdash(xpage,ypage,idash)
      COMMON /mdevice/ idev
      COMMON /cmdash/ xt , yt , ds , iseg , ipen
      DIMENSION dlseg(4,4)
      DOUBLE PRECISION dx , dy , dds , ddx , ddy , s , dl , ds
C
C/*  LT=1 = DOTTED
C/*  LT=2 = CENTERLINE
C/*  LT=3 = DASHED LINE
      DATA dlseg/.04 , .05 , .04 , .05 , .04 , .10 , .25 , .10 , .10 , 
     &     .07 , .10 , .07 , .25 , .10 , .25 , .10/
C/*  LT=4 = LONG DASHED LINE
      IF ( idash.GT.7 .OR. idash.LT.2 ) THEN
         WRITE (*,*) 'IDASH=' , idash , '  IN COMDASH'
         RETURN
      ELSEIF ( idash.GT.3 ) THEN
         dl = dlseg(iseg,idash-3)
C       COMPUTE DELTAX AND DELTAY
         dx = xpage - xt
         dy = ypage - yt
         s = dsqrt(dx*dx+dy*dy)
 50      CONTINUE
         IF ( s+ds.LT.dl ) THEN
            CALL plotw(xpage,ypage,ipen)
            xt = xpage
            yt = ypage
            ds = ds + s
            RETURN
         ELSE
            dds = (dl-ds)/s
            ddx = dx*dds
            ddy = dy*dds
            xt = xt + ddx
            yt = yt + ddy
            CALL plotw(xt,yt,ipen)
            ipen = 5 - ipen
            s = s - (dl-ds)
            ds = 0.
            dx = dx - ddx
            dy = dy - ddy
            iseg = iseg + 1
            IF ( iseg.GT.4 ) iseg = 1
            dl = dlseg(iseg,idash-3)
            GOTO 50
         ENDIF
      ELSE
         CALL plotw(xpage,ypage,idash)
         xt = xpage
         yt = ypage
         ds = 0.
         iseg = 1
         ipen = 2
         IF ( idash.EQ.2 ) ipen = 3
         RETURN
      ENDIF
      END
