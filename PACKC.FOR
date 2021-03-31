      SUBROUTINE packc(in,iout,nwords)
C
C *****************************************************************************
C
C             S U B R O U T I N E     P A C K C
C FUNCTION:
C             THIS ROUTINE PACKS TWO CHARACTERS PER VARIABLE.
C PARAMETERS:
C             IN        IN  - THE ARRAY OF CHARACTERS TO BE PACKED.
C             IOUT      OUT - THE ARRAY OF PACKED CHARACTERS.
C             NWORDS    IN  - THE IN ARRAY SIZE.
C ROUTINES
C CALLED:     ISHFT.
C
C *****************************************************************************
C
      DIMENSION in(nwords) , iout(1)
      LOGICAL master
C
C  FUNCTION - PACK TWO CHARS PER WORD
C
      iword = 0
C
C test if first char neg. if yes may be a master dir filename
      IF ( in(1).LT.0 ) master = .TRUE.
C
      DO i = 1 , nwords , 2
         iword = iword + 1
         iout(iword) = 0
         ilch = iabs(in(i))
         irch = ishft(in(i+1),8)
         iout(iword) = ilch + irch
 
      ENDDO
C
      IF ( master ) THEN
         iout(1) = (-iout(1))
         master = .FALSE.
      ENDIF
      RETURN
      END
