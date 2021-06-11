      SUBROUTINE compresc(inarray,idimin,outarray,idimout)
C*******************************************************************
C
C  ROUTINE COMPRESSES NULLS AND SPACES OUT OF CHARACTER STRING
C  AND STRIPS OFF AN ENDING COMMA IF PRESENT.
C
C  INPUT: INARRAY(IDIMIN)  INTEGER ARRAY OF CHARACTERS
C         IDIMIN           DIMENSION OF INARRAY
C  OUTPUT: OUTARRAY(IDIMOUT) INTEGER ARRAY OF CHARACTERS COMPRESSED
C          IDIMOUT         INDEX OF LAST FILLED ELEMENT OF OUTARRAY
C
C**************************************************************
      INTEGER inarray(idimin) , outarray(idimin) , gtbyt , holdbyte , 
     &        izer
      INTEGER*2 ipass1 , ipass2 , ione
C
      icount = 0
      idimout = 0
      izer = 0
      ione = 1
C
      DO i = 1 , idimin
         ipass1 = inarray(i)
         ipass2 = 0
         CALL gtb(ipass2,ipass1,ione)
         holdbyte = ipass2
         IF ( (holdbyte.NE.32) .AND. (holdbyte.NE.0) ) THEN
            icount = icount + 1
C            BYTE( OUTARRAY(1), ICOUNT ) = HOLDBYTE
            CALL poff(outarray(1),icount,holdbyte,1)
         ENDIF
c
         CALL gtb(holdbyte,inarray(i),2)
         IF ( (holdbyte.NE.32) .AND. (holdbyte.NE.0) ) THEN
            icount = icount + 1
C            BYTE( OUTARRAY(1), ICOUNT ) = HOLDBYTE
            CALL poff(outarray(1),icount,holdbyte,1)
         ENDIF
      ENDDO
C
C If passed character string ends with a comma -
C This is stripped off.
      holdbyte = 0
      CALL goff(holdbyte,icount,outarray(1))
      IF ( holdbyte.EQ.44 ) THEN
         icount = icount - 1
      ENDIF
C
C  PACK A NULL OR TWO ONTO END
      IF ( mod(icount,2).EQ.0 ) THEN
         icount = icount + 1
C          BYTE( OUTARRAY(1), ICOUNT ) = 0
         CALL poff(outarray(1),icount,izer,1)
         icount = icount + 1
C          BYTE( OUTARRAY(1), ICOUNT ) = 0
         CALL poff(outarray(1),icount,izer,1)
      ELSE
         icount = icount + 1
C          BYTE( OUTARRAY(1), ICOUNT ) = 0
         CALL poff(outarray(1),icount,izer,1)
      ENDIF
C
      idimout = int(icount/2)
C
      RETURN
      END
