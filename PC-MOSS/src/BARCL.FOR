      SUBROUTINE barcl(xpag,ypag,th,h,w,ihat,ity)
C       FUNCTION:   PRODUCE BAR SCALES OF THREE DIFFERENT TYPES
C
C       PARAMETERS:
C                   XPAG - LOCATION TO START BAR
C
C                   YPAG - LOCATION TO START BAR
C
C                   TH - ANGLE AT WHICH TO DRAW
C
C                   H - HEIGHT OF BAR
C
C                   W - WIDTH OR LENGTH OF BAR
C
C                   IHAT - HATCH CODE FOR INTERMEDIATE BAR
C                   IHAT 1=DRAW BAR ONLY NO SHADE    2=SOLID SHADE BAR
C
C                   ITY TYPE OF BAR   MUST BE   1 2 OR 3
C                     **  **  **  **  **
C              1    **  **  **  **  **  *
C
C                       **      **
C              2    **      **      **  *
C
C                    * * * * *          **********
C              3    * * * * * **********
C
C
      COMMON /barth / ct , st , sh , it , w2
C
      ct = th*.017453292
      st = sin(ct)
      ct = cos(ct)
      th3 = th + 180.
      IF ( th3.GT.360. ) th3 = th3 - 360.
C
C**     DRAW BLANK BAR WITH NO INTERMEDIATE BAR
C
      sh = 0.0
      x2 = (xpag*ct) - (ypag*st)
      y2 = (ypag*ct) - (xpag*st)
      CALL bar(x2,y2,th,h,w,sh,ihat,npi)
C
      IF ( ihat.GT.1 ) ihat = 2
      sh = h/2.
      npi = 100
      it = 1
      IF ( ity.LT.1 ) ity = 1
      IF ( ity.LT.3 ) THEN
         w2 = w/2.
         IF ( ity.EQ.2 ) it = 2
      ELSE
         ity = 3
         w2 = w/3.
      ENDIF
      w3 = w2/10.
C
C**     ENTER LOOP TO CALL BAR TO ALTERNATELY SHADE LEFT HALF OF BAR
C
      DO i = 1 , 10 , it
         x1 = xpag + ((i-1)*w3)
         y1 = ypag
         th2 = th
         IF ( ity.EQ.2 ) THEN
            IF ( i.EQ.3 .OR. i.EQ.7 ) GOTO 50
         ENDIF
C**     ODD OR EVEN ?  ODD THEN GO TO 70
         IF ( i/2.NE.i/2.0 ) GOTO 100
C**     ADD WIDTH TO X AND HEIGHT TO Y FOR 180 ROTATION
 50      CONTINUE
         th2 = th3
         x1 = x1 + w3
         y1 = y1 + h
 100     CONTINUE
         x2 = (x1*ct) - (y1*st)
         y2 = (y1*ct) + (x1*st)
         CALL bar(x2,y2,th2,h,w3,sh,ihat,npi)
      ENDDO
C
C**     DRAW RIGHT HALF OF BAR WITH SPECIFIED SHADING
      x1 = xpag + w2
      x2 = (x1*ct) - (ypag*st)
      y2 = (ypag*ct) + (x1*st)
      CALL bar(x2,y2,th,h,w2,sh,ihat,npi)
      IF ( ity.EQ.3 ) THEN
         x1 = xpag + w
         y1 = ypag + h
         th2 = th3
         x2 = (x1*ct) - (y1*st)
         y2 = (y1*ct) + (x1*st)
         CALL bar(x2,y2,th2,h,w2,sh,ihat,npi)
      ENDIF
      RETURN
      END
