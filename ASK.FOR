      SUBROUTINE ask
C
C  gives or not gives a prompt for action
C
$Include:'Global.inc'
C
C
C        ********** START **********
 
C * Write menu to screen unless user has requested menu not be displayed.
 
      IF ( help ) THEN
         WRITE (usrout,*)
         WRITE (usrout,99001)
      ELSEIF ( display ) THEN
         WRITE (usrout,*)
         WRITE (usrout,99002)
      ELSE
         WRITE (usrout,*)
         WRITE (usrout,99003) whoiam(1:cstart)
      ENDIF
 
99001 FORMAT (/,' For which command do you want HELP ? ',\)
99002 FORMAT (/,' Enter Command: ',\)
99003 FORMAT (1X,A,'-->',\)
      RETURN
      END
