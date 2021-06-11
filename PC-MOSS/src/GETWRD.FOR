C * Subroutine GETWRD uses PARSE to pick out individual words in a multi-word
C   line.
 
      SUBROUTINE getwrd(arrayi,lngthi,arrayo,lngtho,wrdsst,wrdsfd)
 
C# LengthIn = Length of incoming array (defaults to 132).
      INTEGER lngthi
C# ArrayIn = The line to be parsed.
      CHARACTER*80 arrayi
C# LengthOut = Max allowed length of each word (defaults to 132).
      INTEGER lngtho
C# WordsSought = Max words allowed on a line.
      INTEGER wrdsst
C# ArrayOut = The 'commands' passed back to calling routine;
C             ArrayOut(LengthOut,WordsSought).
      CHARACTER*12 arrayo(wrdsst)
C# WordsFound = 'Commands' received/words parsed.
      INTEGER wrdsfd
 
C# Index/counter variables.
      INTEGER index , i , j , k
C# Position of current word.
      INTEGER pointer
C# Error for call to PARSE.
      LOGICAL error
 
C * Initialize variables for parsing.
 
      pointer = 0
      error = .FALSE.
      wrdsfd = 0
 
C * While ( I .le. WrdsSt ) <statements> Repeat.  End if:  words found = words
C   sought; no words found; pointer has passed length of array.
 
      DO i = 1 , wrdsst
         CALL parse(arrayi,lngthi,pointer,error)
         IF ( .NOT.error ) THEN
            IF ( pointer.LE.lngthi ) THEN
               wrdsfd = wrdsfd + 1
               index = pointer
               CALL nxtchr(arrayi,lngthi,pointer)
               IF ( pointer.GT.lngthi ) THEN
                  pointer = lngthi
               ENDIF
               arrayo(i) = ' '
               k = 1
               DO j = index , (pointer-1)
                  arrayo(i)(k:k) = arrayi(index+(k-1):index+(k-1))
                  k = k + 1
               ENDDO
            ENDIF
         ENDIF
         IF ( pointer.GT.lngthi ) GOTO 100
      ENDDO
 
 100  CONTINUE
      RETURN
      END
