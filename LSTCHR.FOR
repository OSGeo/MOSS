C * Subroutine LSTCHR locates the last filled position in a given array.
 
      SUBROUTINE lstchr(array,length,index)
 
C# Length = the total length or the length of the portion to be checked.
      INTEGER length
C# Array = the array to check.
      CHARACTER*80 array
C# Index = the first blank byte in the array.
      INTEGER index
 
C * While ( Array(Index) .ne. ' ' ) <statements> Repeat.
 
      index = length
 100  CONTINUE
      index = index - 1
      IF ( index.NE.1 ) THEN
         IF ( array(index:index).EQ.' ' ) GOTO 100
         index = index + 1
      ENDIF
      RETURN
      END
