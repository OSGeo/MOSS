      SUBROUTINE ldcmd
 
$Include:'GLOBAL.inc'
 
 
C * Add menu suffix to end of program name and find file.
 
      CALL addend(filenm,12,'MNU',3)
C
      OPEN (srchio,STATUS='OLD',FILE=filenm,ERR=300)
 
      ionum = nulcmd
 100  CONTINUE
      READ (srchio,99001,END=200) rvrsn(ionum+1) , ctype(ionum+1) , 
     &                            imenu(ionum+1)
99001 FORMAT (F5.2,A1,A80)
C
C get length of that menue line
C
      len1 = 0
      CALL lstchr(imenu(ionum+1),80,len1)
      imenu(ionum+1)(80:80) = char(len1)
C * Get identity from first card.
 
      IF ( ctype(ionum+1).EQ.'W' ) THEN
         whoiam = imenu(ionum+1)
         vrsion = rvrsn(ionum+1)
      ENDIF
      IF ( ctype(ionum+1).NE.'W' ) THEN
         ionum = ionum + 1
      ENDIF
C
      IF ( ionum.LT.128 ) GOTO 100
 
C * Close file if found.
 
 200  CONTINUE
      CLOSE (srchio)
 
C * No file found.  There is no warning given.
 
 300  CONTINUE
      retlbl = 0
      RETURN
      END
