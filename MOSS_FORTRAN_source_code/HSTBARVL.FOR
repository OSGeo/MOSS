      SUBROUTINE hstbarvl(value,barbrk,barfrq,nbars,maptype,change)
C**********************************************************************
C
C  FIGURE OUT WHICH BAR CELL VALUE FALLS IN
C  LOGIC BRANCHES ON THE MAP TYPE
C  IF DESCRETE MAP INTERVALS HAVE BEEN ALTERED, FREQUENCY BIN CHECK
C  IS DIFFERENT
C
C**********************************************************************
      INTEGER nbars
C
      REAL value
      REAL barfrq(100)
      REAL barbrk(100,2)
C
      LOGICAL change
C
      IF ( .NOT.((maptype.EQ.8) .OR. (change)) ) THEN
C         SO IF RANGES ARE INTEGERS, IE DESCRETE MAPS
         IF ( .NOT.(change) ) THEN
            index = aint(value-barbrk(1,1)+1)
            IF ( index.NE.0 ) THEN
               barfrq(index) = barfrq(index) + 1.
            ENDIF
            GOTO 100
         ENDIF
      ENDIF
C
C map is continuous or uneven interval
      DO i = 1 , nbars
         IF ( value.LE.barbrk(i,2) ) THEN
            IF ( value.GE.barbrk(i,1) ) THEN
c        WRITE(*,*) ' HBV2: MAX,MIN = ',BARBRK(I,1),BARBRK(I,2)
               barfrq(i) = barfrq(i) + 1.
               GOTO 100
            ENDIF
         ENDIF
      ENDDO
C
C
C
 100  CONTINUE
      RETURN
      END
