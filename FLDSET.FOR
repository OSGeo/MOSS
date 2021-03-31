      SUBROUTINE fldset(var,start,stop,value)
C ---------------------------------------------------------------------
C
C     VAR         OUTPUT VARIABLE NAME
C     START       STARTING BIT OF VARIABLE TO BE SET
C     STOP        LAST BIT OF VARIABLE TO BE SET
C     VALUE       THE VALUE TO BE STORED IN THE SPECIFIED BITS OF VAR
C
C     BIT ARE NUMBERED 1 THROUGH N FROM LEFT TO RIGHT, 1 BEING THE
C     MOST SIGNIFICANT AND N BEING THE LEAST SIGNIFICANT.
C
C
C ---------------------------------------------------------------------
C
      INTEGER var , start , stop , value , temp , tempa , j , nbit
      LOGICAL what
C
      IF ( start.LE.15 .AND. start.GE.0 ) THEN
         IF ( stop.LE.15 .AND. stop.GE.0 ) THEN
C
            jbit = 0
            DO i = stop , start , -1
               jbit = jbit + 1
c       CALL BITSET (17-I,GETBIT (JBIT,VALUE,1),VAR,1)
               what = btest(var,i)
               IF ( .NOT.what .AND. value.EQ.1 ) var = ibset(var,i)
               IF ( value.EQ.0 .AND. (what) ) var = ibclr(var,i)
            ENDDO
            RETURN
         ENDIF
      ENDIF
C
C ----ERROR CONDITION
C
      WRITE (1,99001)
99001 FORMAT (' ERROR - INVALID BIT RANGE SPECIFIED (FLDSET).')
C
      RETURN
      END
