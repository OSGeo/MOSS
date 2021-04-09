C * Subroutine READLN accepts a user response and converts alpha characters to
C   upper case when requested to do so.  This routine expects the accessed file
C   to have been opened for reading "padded records" (i.e. blank filled).
 
      SUBROUTINE readln(ichan,answer,length,error,lnull,default,upperc)
 
C# IChan = the user input/response/output channel
      INTEGER ichan
C# Actual length of Answer array.
      INTEGER length
      INTEGER lencnv
C# Answer = the user response array.
      CHARACTER*80 answer
C# Error = Variable to hold logical error status on return (.True./.False.).
      LOGICAL error
C# Null = .True./.False. null response okay.  Effectively .True. if default.
      LOGICAL lnull
C# Default = alpha string containing default response - blanked on return.
      CHARACTER*80 default
C# Upper case = .True. convert all alpha characters to upper case. [.FALSE.].
      LOGICAL upperc
 
C# Index variables.
      INTEGER i
C# Line to fill.
      CHARACTER*127 rdlin
 
C * Initialize the entry conditions.
 
 100  CONTINUE
      rdlin = ' '
      answer = ' '
      error = .TRUE.
      i = 0
 
C * Now, get the answer.
 
C        Write ( *, '(a,\)') '? '
      READ (ichan,99001,END=300,ERR=400) rdlin
99001 FORMAT (A127)
 
 200  CONTINUE
      IF ( rdlin(i+1:i+1).NE.' ' ) error = .FALSE.
      answer(i+1:i+1) = rdlin(i+1:i+1)
      i = i + 1
      IF ( i.NE.length ) GOTO 200
 
C * And, do something with it.
 
      IF ( error ) THEN
         rdlin = default
         IF ( rdlin(1:1).NE.' ' ) THEN
            DO i = 1 , length
               answer(i:i) = rdlin(i:i)
            ENDDO
            error = .FALSE.
         ENDIF
      ENDIF
      IF ( .NOT.(error) ) THEN
 
         IF ( upperc ) THEN
            IF ( length.GT.0 ) THEN
               CALL cnvupr(answer,length)
            ENDIF
         ENDIF
         RETURN
      ENDIF
 
C * End of file with no record received.
 
 300  CONTINUE
      IF ( lnull ) THEN
         error = .FALSE.
      ENDIF
      RETURN
 
C * Invalid character received.
 
 400  CONTINUE
      WRITE (*,*) 'Invalid character in response.  Please retry.'
 
cssk 9100    Write ( *, * ) 'Response too long.  Please retry.'
      GOTO 100
      END
