      SUBROUTINE setptr(ihh,ids,ih)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C  NAME:     S  E  T  P  T  R
C
C  FUNCTION:  THIS ROUTINE SETS UP THE POINTER TABLE FOR THE POLYGON
C
C  PARAMETERS:
C    INPUT:   IHH   =  NO. OF ENTRIES IN TABLE OF POINTERS
C             IDS   =  TABLE OF ENTRIES ( THE POINTERS )
C    OUTPUT:  IDS   =  READJUSTED TABLE OF ENTRIES (POINTERS)
C             IH    =  NO. OF ENTRIES IN THE TABLE
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      INTEGER ids(1) , jkl
C
      ih = 1
      IF ( ihh.GT.0 ) THEN
         loop = ihh + 1
         DO i = 1 , ihh
            ids(loop) = ids(loop-1)
            loop = loop - 1
         ENDDO
         DO jkl = 1 , ihh , 1
            ih = ih + 1
            ids(ih) = ids(ih) - 1
         ENDDO
      ENDIF
      RETURN
      END
