      SUBROUTINE bsort(npt,xx,isim)
      INTEGER xx(1)
C
C ****************************************************
C
C       S U B R O U T I N E   B S O R T
C
C  FUNCTION - SORT IN ASCENDING OR DESCENDING ORDER
C             ON X AND RE-ORDER Y AND W ACCORDINGLY
C
C  INPUT PARAMETERS -
C
C      NPT   = NUMBER OF COORDINATES IN X,Y,W
C      NVAR  = UNUSED
C      (X,Y,W)=ARRAYS TO SORT
C      ISIM  = 0 = ASCENDING SORT
C            = 1 = DESCENDING SORT
C
C  PROGRAMMED BY CARTOGRAPHIC LAB, U. OF MICHIGAN
C
C *****************************************************
C
      jn = npt - 1
      DO i = 1 , jn
         jl = i + 1
         DO j = jl , npt
            IF ( isim.NE.0 ) THEN
               IF ( xx(i).LE.xx(j) ) GOTO 50
            ELSEIF ( xx(i).GE.xx(j) ) THEN
               GOTO 50
            ENDIF
C
C  INTERCHANGE
C
            temp = xx(i)
            xx(i) = xx(j)
            xx(j) = temp
 50      ENDDO
      ENDDO
C
      RETURN
      END
