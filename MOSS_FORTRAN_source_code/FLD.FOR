      INTEGER*2 FUNCTION fld(var,start,stop)
C
C     VAR         INPUT VARIABLE NAME
C     START       STARTING BIT OF VARIABLE TO BE EXTRACTED
C     STOP        LAST BIT OF VARIABLE TO BE EXTRACTED
C
C     BIT ARE NUMBERED 1 THROUGH N FROM LEFT TO RIGHT, 1 BEING THE
C     MOST SIGNIFICANT AND N BEING THE LEAST SIGNIFICANT.
C
C     THIS ROUTINE IS AN EMULATION OF THE DG FLD FUNCTION.
C
      INTEGER*2 var , start , stop , temp , j
C
      fld = var
      IF ( start.LE.15 ) THEN
         IF ( stop.LE.15 ) THEN
C
            temp = var
C
C ----TRUNCATE THE LEFT MOST BITS TO THE STARTING BIT
C
c      J = 16 - START
            IF ( j.NE.0 ) THEN
               DO il = start , 15
                  temp = ibclr(temp,j)
               ENDDO
            ENDIF
C
C ----SHIFT THE REST OF THE BITS TO THE RIGHT
C
            j = 15 - stop
            IF ( j.NE.0 ) temp = ishl(temp,-stop)
C
C ----SAVE THE RESULT
C
            fld = temp
            RETURN
         ENDIF
      ENDIF
C
C ----ERROR CONDITION
C
      WRITE (1,99001) start , stop
99001 FORMAT (' *FLD* - INVALID BIT RANGE = ',2I6)
C
      RETURN
      END
