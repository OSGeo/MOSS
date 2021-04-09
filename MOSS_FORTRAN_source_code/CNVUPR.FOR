C * Subroutine CNVUPR converts lower case to upper case for those systems that
C   don't handle lower case automatically.
 
      SUBROUTINE cnvupr(textln,length)
 
C# Length = the number of elements in the text array.
      INTEGER length
C# TextLn = the character array to be converted to upper case.
      CHARACTER*80 textln
 
C# Index variable for looping.
      INTEGER i
 
C * Now, convert each lowercase alpha character to uppercase.
 
      DO i = 1 , length
         IF ( ichar(textln(i:i)).GT.96 ) THEN
            IF ( ichar(textln(i:i)).LT.123 ) THEN
               textln(i:i) = char(ichar(textln(i:i))-32)
            ENDIF
         ENDIF
      ENDDO
 
      RETURN
      END
