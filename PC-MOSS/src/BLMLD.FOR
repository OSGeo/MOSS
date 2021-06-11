      SUBROUTINE blmld(iout,xpage,ypage,idash)
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
      COMMON /blmldsh/ xt , yt , ds , iseg , ipen , dlseg(4,4)
C        COMMON /CHRSTU/   DX,DY,DDS,DDX,DDY,S,DL,XY,YT,DS,ISEG,IPEN,DLSEG
C        DOUBLE PRECISION DX,DY,DDS,DDX,DDY,S,DL,DS
C
C.......LT=1=DOTTED, LT=2=CENTERLINE, LT=3=DASHED LINE, LT=4=LONG DASHED LINE..
C
C.......CHECK FOR ILLEGAL DASH CODE...
      IF ( idash.GT.7 .OR. idash.LT.2 ) THEN
C
C.......ERROR RETURN......
         WRITE (iout,99001) idash
99001    FORMAT (' *BLMLD*    ILLEGAL IDASH CODE',I5,'  IN BLMLD')
C.......FIRST TIME THROUGH (IDASH=2) IS JUST INITIALIZATION AND MOVE TO POINT...
      ELSEIF ( idash.GT.3 ) THEN
C
C.......PROCESS DASHED LINES.....
         dl = dlseg(iseg,idash-3)
C.......COMPUTE DELTAX AND DELTAY
         dx = xpage - xt
         dy = ypage - yt
         s = sqrt(dx*dx+dy*dy)
 50      CONTINUE
         IF ( s+ds.LT.dl ) THEN
            CALL thick(xpage,ypage,ipen)
            xt = xpage
            yt = ypage
            ds = ds + s
         ELSE
            dds = (dl-ds)/s
            ddx = dx*dds
            ddy = dy*dds
            xt = xt + ddx
            yt = yt + ddy
            CALL thick(xt,yt,ipen)
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
         CALL thick(xpage,ypage,idash)
         xt = xpage
         yt = ypage
         ds = 0.
         iseg = 1
         ipen = 2
         IF ( idash.EQ.2 ) ipen = 3
      ENDIF
C
C.......JUMP OUT.....
      RETURN
      END
