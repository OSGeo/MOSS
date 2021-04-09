      SUBROUTINE vargout(iout,ioin,ichfrg,ier)
C   FUNCTION: DISPLAYS VARIOGRAM ON FILE 'TEMPVARG.FGA',ASKS
C    USER IF THEY WANT TO KEEP IT; IF SO, RENAMES FILE.
C
C  DUMMY ARGUMENTS: IOUT   -CHANNEL FOR OUTPUT TO TERMINAL
C                   IOIN   -CHANNEL FOR TERMINAL INPUT
C                   ICHVRG -CHANNEL FOR VARIOGRAM DISK FILE
C                   IER    -ERROR CODE; 0 MEANS NO ERROR
C
C      DIMENSION ICARD(40),NAME(5)
      INTEGER*2 itempv(7) , name(5)
      CHARACTER*2 icard(40) , tempv(6)
      CHARACTER*1 ians
      CHARACTER*8 name1
      EQUIVALENCE (name(1),name1)
C
      REWIND (ichfrg)
      line = 0
 100  CONTINUE
      READ (ichfrg,99001,END=200) icard
99001 FORMAT (39A2,A1)
      WRITE (iout,99002) icard
      line = line + 1
      IF ( line.GT.21 ) THEN
         CALL contin(line,istop)
         IF ( istop.EQ.1 ) GOTO 200
         line = 0
      ENDIF
99002 FORMAT (39A2,A1)
      GOTO 100
 200  CONTINUE
      WRITE (iout,99003)
99003 FORMAT (1X,'DO YOU WISH TO SAVE THIS VARIOGRAM ',/,' ?'\)
      READ (ioin,99004) ians
99004 FORMAT (A1)
      IF ( (ians.NE.'Y') .AND. (ians.NE.'y') ) GOTO 500
 300  CONTINUE
      WRITE (iout,99005)
99005 FORMAT (1X,'GIVE AN 8 CHARACTER OR LESS FILE NAME:',/,' ?'\)
      READ (ioin,99006) (name(ii),ii=1,5)
99006 FORMAT (5A2)
C      CALL RENAM (ITEMPV,NAME,IER)
      IF ( ier.EQ.3 .OR. ier.EQ.5 .OR. ier.EQ.17 ) WRITE (iout,99007)
99007 FORMAT (1X,'YOU ENTERED INVALID FILENAME CHARACTERS. TRY AGAIN. ')
C      IF (IER.EQ.25) WRITE (IOUT,200)
C  200 FORMAT (1X,'THIS FILE ALREADY EXISTS.  PLEASE TRY AGAIN.  ')
C      IF ((IER.EQ.23).OR.(IER.EQ.25)) GOTO 165
      IF ( ier.EQ.3 .OR. ier.EQ.5 .OR. ier.EQ.17 ) GOTO 300
      IF ( ier.NE.0 ) THEN
C      RETURN
C
C  ERROR RETURN
C
         WRITE (iout,99008) ier
99008    FORMAT (1X,'***VARGOUT*** ERROR RENAMEING FILE   ERROR ',I5)
         GOTO 500
      ELSE
         ichout = 29
         OPEN (ichout,FILE=name1)
         REWIND (ichfrg)
 350     CONTINUE
         READ (ichfrg,99001,END=400) icard
         WRITE (ichout,99001) icard
         GOTO 350
      ENDIF
 400  CONTINUE
      CLOSE (ichfrg,STATUS='DELETE')
      CLOSE (ichout)
C     WRITE (IOUT,205)
C 205 FORMAT (' About to print file name')
      WRITE (iout,99009) name
99009 FORMAT (1X,//,' VARIOGRAM HAS BEEN STORED ON DISK',//,
     &        ' FILE NAME IS ',5A2,'.  USE THE DOS "TYPE" COMMAND',
     &        ' TO DISPLAY IT AT ANY TIME.')
cssk      WRITE(IOUT,280) IER
99010 FORMAT (1X,'***VARGOUT*** ERROR OPENING TEMPVARG.FGA  ERROR ',I5)
C
 500  CONTINUE
      RETURN
      END
