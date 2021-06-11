      SUBROUTINE openf(ichan,name,ityp,nbytes,ifl,ier)
C
C *****************************************************************************
C
C             S U B R O U T I N E     O P E N F
C FUNCTION:
C             THIS ROUTINE OPENS DISK FILES.
C PARAMETERS:
C             ICHAN     IN  - THE CHANNEL NUMBER TO USE TO OPEN THE FILE.
C             NAME      IN  - THE NAME ARRAY CONTAINING THE FILE NAME.
C             ITYP      IN  - CREATE CODE
C                           - 0=create regardless(overlay if exist)
C                           - 1=must exist, error if not
C                           - 2=create only if not already exist
C                           - 3=create scratch only
C             NBTYES    IN  - THE NUMBER OF BYTES IN FILE.
C             IFL       IN  - THE FILE TYPE,
C                             IFL=0, IF A SEQUENTIAL TO OPEN, FORMATTED
C                             IFL=1,IF A SEQUENTIAL TO OPEN, UNFORMATTED
C                             IFT=2,IF A RANDOM TO OPEN.
C             IER       OUT - THE ERROR RETURN FOR WRITING TO, READING,
C                             OPENING, OR CLOSING A FILE,
C                             IER = 0, IF NO ERROR.
C ROUTINES
C CALLED:
C
C *****************************************************************************
C
      LOGICAL fexist
      CHARACTER fname*25
      DIMENSION name(25)
C
      ier = 0
C
C  CHANGE INTEGER FILENAME TO CHARACTER FILENAME
C
      CALL chrbld(name,fname)
      IF ( fname(1:1).LT.'0' ) THEN
         ier = 1
         GOTO 100
      ENDIF
C
C  SEE IF FILE ALREADY EXISTS
C
      INQUIRE (FILE=fname,EXIST=fexist)
C
C  IF ITYP=0 OR 3 AND THE FILE EXISTS - GET RID OF IT
C
      IF ( (ityp.EQ.0 .OR. ityp.EQ.3) .AND. fexist ) THEN
         OPEN (ichan,FILE=fname,STATUS='OLD',IOSTAT=ier,ERR=100)
         READ (ichan,IOSTAT=ier,ERR=50,END=50) ii
 50      CONTINUE
         CLOSE (ichan,STATUS='DELETE',IOSTAT=ier,ERR=100)
      ENDIF
C
C   OPEN OR CREATE THE FILE
C
      IF ( ityp.EQ.0 .OR. (ityp.EQ.2 .AND. .NOT.fexist) ) THEN
         IF ( ifl.EQ.0 ) THEN
            OPEN (ichan,FILE=fname,STATUS='NEW',IOSTAT=ier,ERR=100,
     &            ACCESS='SEQUENTIAL',FORM='FORMATTED')
            RETURN
         ELSEIF ( ifl.EQ.1 ) THEN
            OPEN (ichan,FILE=fname,STATUS='NEW',IOSTAT=ier,ERR=100,
     &            ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            RETURN
         ELSEIF ( ifl.EQ.2 ) THEN
            OPEN (ichan,FILE=fname,ACCESS='DIRECT',STATUS='NEW',
     &            RECL=nbytes,IOSTAT=ier,ERR=100)
            RETURN
         ENDIF
      ENDIF
C
      IF ( ityp.EQ.1 .OR. (ityp.EQ.2 .AND. fexist) ) THEN
C
         IF ( ifl.EQ.0 ) THEN
            OPEN (ichan,FILE=fname,STATUS='OLD',IOSTAT=ier,ERR=100,
     &            ACCESS='SEQUENTIAL',FORM='FORMATTED')
            RETURN
         ELSEIF ( ifl.EQ.1 ) THEN
            OPEN (ichan,FILE=fname,STATUS='OLD',IOSTAT=ier,ERR=100,
     &            ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            RETURN
         ELSEIF ( ifl.EQ.2 ) THEN
            OPEN (ichan,FILE=fname,ACCESS='DIRECT',RECL=nbytes,
     &            IOSTAT=ier,STATUS='OLD',ERR=100)
            RETURN
         ENDIF
      ENDIF
C
      IF ( ityp.EQ.3 ) THEN
         IF ( ifl.EQ.0 ) THEN
            OPEN (ichan,STATUS='SCRATCH',IOSTAT=ier,ERR=100,
     &            ACCESS='SEQUENTIAL',FORM='FORMATTED')
            RETURN
         ELSEIF ( ifl.EQ.1 ) THEN
            OPEN (ichan,STATUS='SCRATCH',IOSTAT=ier,ERR=100,
     &            ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            RETURN
         ELSEIF ( ifl.EQ.2 ) THEN
            OPEN (ichan,ACCESS='DIRECT',RECL=nbytes,IOSTAT=ier,
     &            STATUS='SCRATCH',ERR=100)
            RETURN
         ENDIF
      ENDIF
C
C  ERRORS are returned to the calling program
C
 100  CONTINUE
      RETURN
      END
