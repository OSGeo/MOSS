      SUBROUTINE pnumber(xpage,ypage,height,fpn,angle,ndec)
C
C.....     XPAGE,YPAGE COORDINATES OF LOWER LEFT CORNER OF NUMBER.
C.....     HEIGHT   HEIGHT OF PLOTTED NUMBER.
C.....     FPN      FLOATING POINT NUMBER TO BE PLOTTED.
C.....     ANGLE    ANGLE AT WHICH NUMBER IS PLOTTED, IN DEGREES.
C.....     NDEC     NUMBER OF DECIMAL PLACES TO BE DRAWN.
C
C
      CHARACTER*20 anum , fmt
      INTEGER*2 ivar(10) , ipar(20)
      EQUIVALENCE (anum(1:1),ivar(1))
C
      nd = max(ndec,0)
      nnx = 19 - nd
      WRITE (fmt,99001) nnx , nd
99001 FORMAT ('(F',I2,'.',I2,')')
      WRITE (anum,fmt) fpn
      CALL chrjst(anum,-1)
C
      ididy = 0
      DO i = 20 , 1 , -1
         IF ( anum(i:i).NE.' ' ) THEN
            js = i
            IF ( ididy.EQ.0 ) THEN
               l = i
               ididy = 1
            ENDIF
         ENDIF
      ENDDO
C      L=20-JS
C
      IF ( ndec.LT.0 ) l = l - 1
C
      DO il = 1 , 10
         ipar(il) = ivar(il)
      ENDDO
c
      CALL plotsym(0,1,xpage,ypage,height,height,ipar(1),angle,0.0,
     &             angle,0,l,ier)
C
      RETURN
      END
