      SUBROUTINE rslcom(kchan,macts,fname,ier)
      COMMON /whmap / istrct(300) , nstrct
$Include:'strng.inc'
      COMMON /table / itable(40)
      INTEGER fname(25)
      COMMON /io    / nprnt , ioin
      COMMON /work  / ibuff(128) , map(25) , icom(10) , jcom(10)
      LOGICAL over , mcomp
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                  S U B R O U T I N E   r s l C O M
C
C  FUNCTION:  PARSE USER INPUT FOR THE RESOLVE COMMAND
C
C  INPUT PARAMETERS:
C
C             MACTS  = RETURED NUMBER OF ACTIVE MAPS TO PROCESS
C             LINES  = CURSOR CONTROL
C             IER    = ERROR RETURN ( 0=NORMAL)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      ier = 0
C
C
C     GET ACTIVE DATA SETS TO RESOLVE.
C     FIRST ID IS ORIGINAL FILE BEFORE BUFFERING.
C     SECOND ID IS BUFFERED FILE TO BE RESOLVED.
C
      CALL daget(icard,icardp,itable,40,macts)
C
C     CHECK FOR OCCURENCE OF ACTIVE DATA SETS
C
      IF ( macts.EQ.0 ) THEN
C
C     USER DID NOT SPECIFY ACTIVE DATA SET I.D.@S. GET THEM
C
         WRITE (nprnt,99002)
         ipromt = 1
         CALL readin(icard,lenicd,icardp,istop)
         CALL daget(icard,icardp,itable,40,macts)
         lines = lines + 3
      ENDIF
c       GO TO 151
C
C   DESIGNATE NAME FOR THE SAVE FILE
C
      CALL decip(icard,fname,icardp)
      IF ( fname(1).EQ.0 ) THEN
 
         ipromt = 1
         CALL whtcal(fname,ier)
      ENDIF
C
 100  CONTINUE
      CALL mchek(fname,ifil,nents,its,ier)
      IF ( ifil.EQ.0 ) RETURN
      WRITE (nprnt,99001)
99001 FORMAT (' THIS MAP ALREADY EXISTS.  TRY AGAIN')
      CALL whtcal(fname,ier)
      GOTO 100
c
c  assign work file name
c
C
99002 FORMAT (' PLEASE ENTER ACTIVE MAP ID TO UNION '\)
C
C  ERRORS
C
      RETURN
C
      END
