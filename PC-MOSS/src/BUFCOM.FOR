C
      SUBROUTINE bufcom(kchan,macts,bufz,lines,ier)
C
      COMMON /whmap / istrct(300) , nstrct
$Include:'strng.inc'
      COMMON /table / itable(40)
      COMMON /io    / nprnt , ioin
      COMMON /work  / ibuff(128) , map(25) , icom(10) , jcom(10)
      LOGICAL over , mcomp
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                  S U B R O U T I N E   B U F C O M
C
C  FUNCTION:  PARSE USER INPUT FOR THE BUFFER COMMAND
C
C  INPUT PARAMETERS:
C
C             MACTS  = RETURED NUMBER OF ACTIVE MAPS TO PROCESS
C             BUFZ   = RETURNED BUFFER SIZE IN METERS
C             LINES  = CURSOR CONTROL
C             IER    = ERROR RETURN ( 0=NORMAL)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      ier = 0
C
C
C     GET ACTIVE DATA SET TO BUFFER
C
      CALL daget(icard,icardp,itable,40,macts)
C
C     CHECK FOR OCCURENCE OF ACTIVE DATA SETS
C
      IF ( macts.NE.0 ) THEN
C
C   DESIGNATE NAME FOR THE SAVE FILE
C
         CALL decip(icard,map,icardp)
         IF ( map(1).EQ.0 ) THEN
            ipromt = 1
            CALL whtcal(map,ier)
         ENDIF
      ELSE
C
C     USER DID NOT SPECIFY ACTIVE DATA SET I.D.@S. GET THEM
C
         WRITE (nprnt,99003)
         ipromt = 1
         CALL readin(icard,lenicd,icardp,istop)
         CALL daget(icard,icardp,itable,40,macts)
         lines = lines + 3
         CALL whtcal(map,ier)
      ENDIF
C
 100  CONTINUE
      CALL mchek(map,ifil,nents,its,ier)
      IF ( ifil.EQ.0 ) THEN
         CALL packc(map,jcom,10)
         WRITE (kchan) jcom
C
C     GET BUFFER WIDTH FROM USER
C
         IF ( ipromt.NE.1 ) THEN
            CALL fnum(icard,80,bufz,icardp)
            IF ( bufz.GT.0.0 ) GOTO 300
C
C     PROMPT FOR BUFFER ZONE
C
            ipromt = 1
         ENDIF
      ELSE
         WRITE (nprnt,99001)
99001    FORMAT (' THIS MAP ALREADY EXISTS.  TRY AGAIN')
         lines = lines + 2
C
         ipromt = 1
         CALL whtcal(map,ier)
         GOTO 100
      ENDIF
 200  CONTINUE
      WRITE (nprnt,99004)
      CALL readin(icard,lenicd,icardp,istop)
      CALL fnum(icard,80,bufz,icardp)
      IF ( bufz.LE.0.0 ) GOTO 200
C
C  CHECK FOR UNITS
C
 300  CONTINUE
      CALL chkunts(icard,icardp,iunits,bufz)
      IF ( iunits.LT.0 ) THEN
C
C  ERRORS
C
         WRITE (nprnt,99002)
99002    FORMAT (' Illegal units of measure for buffer distance')
         GOTO 200
      ELSE
C
C INPUT COMPLETE
C
C
         RETURN
C
C
99003    FORMAT (' PLEASE ENTER ACTIVE MAP IDs TO BUFFER'\)
99004    FORMAT (' PLEASE ENTER BUFFER ZONE IN PROPER UNITS '\)
      ENDIF
c      RETURN
C
      END
