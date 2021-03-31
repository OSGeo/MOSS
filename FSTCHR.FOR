C * Subroutine FSTCHR locates the first filled position in a given array.
 
      SUBROUTINE fstchr(array,length,index)
 
C# Length = the total length or the length of the portion to be checked.
      INTEGER length
C# Array = the array to check.
      CHARACTER*80 array
C# Index = the first non-blank byte in the array.
      INTEGER index
 
C * While ( Array(Index:index) .eq. ' ' ) <statements> Repeat.
 
 100  CONTINUE
      index = index + 1
      IF ( index.LE.length ) THEN
         IF ( array(index:index).EQ.' ' ) GOTO 100
      ENDIF
 
      RETURN
      END
