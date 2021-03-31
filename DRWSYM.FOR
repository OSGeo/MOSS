      SUBROUTINE drwsym(npts)
C      DIMENSION XX(NPTS),YY(NPTS)
$Include:'xypnts.inc'
      CALL movea(x(1),y(1))
      DO i = 2 , npts
         IF ( x(i).LT.0 ) THEN
            CALL movea(x(i),y(i))
         ELSE
            CALL drawa(x(i),y(i))
         ENDIF
      ENDDO
C
      RETURN
      END
