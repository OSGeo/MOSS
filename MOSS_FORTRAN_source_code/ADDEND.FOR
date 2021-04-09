C * Subroutine ADDEND adds the appropriate ending to the program name so that
C   the proper file will be accessed.
 
      SUBROUTINE addend(array,length,suffix,slngth)
 
C# Length = Number of elements in array.
      INTEGER length
      INTEGER prfxln
C# Array = file name on entry; file name + suffix on exit.
      CHARACTER array*39 , hold*39
C# SLngth = Number of elements in Suffix.
      INTEGER slngth
C# Suffix = Suffix for file name.
      CHARACTER*3 suffix
 
C# Some useful index/counter variables.
      INTEGER i , index
 
C * Establish entry conditions.
 
      index = 0
      prfxln = length - slngth
      hold = '\GIS\INTRFACE\'
C * Find the first blank position.
 
      IF ( length.NE.0 ) THEN
         CALL lstchr(array,prfxln,index)
      ENDIF
 
C * Affix the separator to the file name.
 
      IF ( index.GT.0 ) THEN
         array(index:index) = '.'
      ENDIF
 
C * Add the suffix to the file name.
 
      IF ( slngth.GT.0 ) THEN
         DO i = 1 , slngth
            array(index+i:index+i) = suffix(i:i)
         ENDDO
      ENDIF
C
C  ADD PREFIX FOR DIRECTORY CONTROL
C
      ilen = index + slngth
      j = 14
      DO l = 1 , ilen
         j = j + 1
         hold(j:j) = array(l:l)
      ENDDO
      array = ' '
      array = hold
      RETURN
      END
