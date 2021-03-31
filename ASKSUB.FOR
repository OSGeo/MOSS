      SUBROUTINE asksub(ipromt,merlab,mlpnt,nchars,irev,ier)
$Include:'io.inc'
$Include:'strng.inc'
      DIMENSION icom(10)
      LOGICAL mcomp
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C               S U B R O U T I N E    A S K S U B
C
C  FUNCTION:  ASK USER FOR SUBJECT MERGE INFORMATION (USED IN ALL
C             OVERLAY FUNCTIONS.
C
C  INPUT PARAMETERS:
C
C             IPROMT = PROMPTING FLAG (0=UNPROMPTED, 1=PROMT)
C             MLPNT  = NUMBER CHARACTERS SUBJECT 1 (RETURNED)
C             NCHARS = NUMBER CHARACTERS SUBJECT 2 (RETURNED)
C             MERLAB = MERGE TYPE (0=DEFAULT , 1 = USER SPECIFIED)
C             IER    = ERROR RETURN (1=NORMAL)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      merlab = 0
      ier = 0
      mlpnt = 15
      irev = 0
      nchars = 15
C
      IF ( ipromt.NE.1 ) THEN
C
C  GET MERGE SPECIFICATION
C
         CALL decip(icard,icom,icardp)
         IF ( icom(1).NE.0 ) GOTO 100
         ipromt = 1
      ENDIF
C
C  PROMPT FOR SUBJECT MERGE OPTION
C
      WRITE (nprnt,99001)
99001 FORMAT (
     &       ' Do you want to specify SUBJECT MERGE parameters [CR=NO] '
     &       ,/'    [NO = 14 chars from 1st and 16 chars from 2nd]',
     &       /'    [YES= input # chars from each subject]',
     &       /'    [REV= reverse order of subjects      ]        '\)
C
      CALL readin(icard,lenicd,icardp,istop)
      CALL decip(icard,icom,icardp)
C
C  CHECK USER INPUT OPTIONS
C
 100  CONTINUE
      IF ( mcomp(icom(1),'Y',1,ier) ) merlab = 1
      IF ( mcomp(icom(1),'R',1,ier) ) irev = 1
      IF ( irev.EQ.1 ) merlab = 1
C
C  BASED ON SPECIFIED ACTION, CONTINUE
C
      IF ( merlab.EQ.0 ) GOTO 700
C
 200  CONTINUE
      IF ( ipromt.NE.1 ) THEN
C
         CALL inum(icard,mlc,icardp)
         IF ( mlc.GT.0 ) GOTO 400
C
C  PROMPT FOR NUMBER OF CHARACTER FROM SUBJECT 1
C
         ipromt = 1
      ENDIF
 300  CONTINUE
      WRITE (nprnt,99002)
99002 FORMAT (' Please enter number of character to keep for subject 1'\
     &        )
      CALL readin(icard,lenicd,icardp,istop)
      CALL inum(icard,mlc,icardp)
C
C  VALIDATE INPUT VALUES
C
 400  CONTINUE
      IF ( mlc.LT.0 .OR. mlc.GT.30 ) GOTO 300
C
C  GET SECOND VALUE
C
      IF ( ipromt.NE.1 ) THEN
         CALL inum(icard,nlc,icardp)
         IF ( nlc.GT.0 ) GOTO 600
         ipromt = 1
      ENDIF
C
C  PROMPT FOR SECOND VALUE
C
 500  CONTINUE
      WRITE (nprnt,99003)
99003 FORMAT ('Please enter number of characters to keep for subject 2'\
     &        )
      CALL readin(icard,lenicd,icardp,istop)
      CALL inum(icard,nlc,icardp)
C
C  VALIDATE SECOND INPUT
C
 600  CONTINUE
      IF ( nlc.LT.0 .OR. nlc.GT.30 ) GOTO 500
      mlpnt = mlc
      nchars = nlc
C
C  CHECK FOR INPROPER COMBINATION
C
      mx = mlpnt + nchars
      IF ( mx.LE.0 ) THEN
         WRITE (nprnt,99004)
99004    FORMAT (' Both values equal to 0.  Please try again')
         GOTO 200
      ELSEIF ( mx.GT.30 ) THEN
         WRITE (nprnt,99005)
99005    FORMAT (' Total characters specified is greater than 30.',
     &           '  Try Again')
         GOTO 200
      ENDIF
C
C
 700  CONTINUE
      IF ( irev.EQ.1 ) mlpnt = nlc
      IF ( irev.EQ.1 ) nchars = mlc
C
      RETURN
      END
