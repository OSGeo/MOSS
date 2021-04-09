C * Subroutine PARSE parses a line of text, moving the pointer to the initial
C   position for the "next" word on each successive call, until there are no
C   new words.
 
      SUBROUTINE parse(answer,length,pointer,error)
 
C# Length = maximum length of response array to be searched.
      INTEGER length
C# Answer = The text line to be parsed.
      CHARACTER*80 answer
C# Pointer = Initial position of current word to be processed.
      INTEGER pointer
C# Error = Variable to hold logical error status on return (.True./.False.).
      LOGICAL error
 
C * Initialize entry conditions.
 
      error = .FALSE.
 
C * Now do something with the text line passed in.
 
      IF ( length.GT.0 ) THEN
         CALL fstchr(answer,length,pointer)
      ENDIF
 
      IF ( pointer.EQ.0 ) error = .TRUE.
 
      RETURN
      END
