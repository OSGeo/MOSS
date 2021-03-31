C * Subroutine WrtMnu presents those commands to the user that were in the
C   <program name>.cmd file (if one existed).
 
      SUBROUTINE wrtmnu
 
$Include:'GLOBAL.inc'
      CHARACTER*1 jj
C
      IF ( display ) CALL banner
      IF ( display .OR. help ) THEN
         numlin = 0
         DO i = 1 , ionum
            len1 = ichar(imenu(i)(80:80))
            WRITE (usrout,99002) i , imenu(i)(1:len1)
            numlin = numlin + 1
C
            IF ( numlin.EQ.22 ) THEN
               WRITE (usrout,99001)
99001          FORMAT (' Continue ? <cr = yes>',\)
               READ (*,'(a)') jj
               IF ( jj.GT.char(32) ) GOTO 100
               numlin = 0
            ENDIF
C
         ENDDO
      ENDIF
C flip switch back off, if on
 100  CONTINUE
      display = .FALSE.
99002 FORMAT (I3,'.  ',A)
 
      RETURN
      END
