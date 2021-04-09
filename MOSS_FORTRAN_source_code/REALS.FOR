      SUBROUTINE reals
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC NAME : SUBROUTINE REALS
CC
CC FUNCTION : TO READ IN THE POLYGON VALUES WHEN THE VALUES ARE OF TYPE
CC            REAL
CC
CC PARAMETERS :
CC
CC FILES : THE USER IS CAPABLE OF ENTERING THE FILE'S NAME. IT IS OPEN
CC         AS A SEQUENCIAL FILE. THE USER NEEDS TO ALSO TYPE IN THE FORMAT.
CC
CC ROUTINES REQUIRED :
CC ROUTINES REQUIRED : SUBROUTINE WRBLK, SUBROUTINE READIN,
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
$Include:'IO.inc'
$Include:'STRNG.inc'
      COMMON /batat / imap(25) , kmap(25) , batch
      COMMON /unit  / chanel , atrbte , polygon , adress
CC
      REAL bufer(64)
CC
      LOGICAL batch
      CHARACTER*20 form
CC
      INTEGER chanel , atrbte , polygon , action , adress , total , 
     &        subscpt , record(128) , output , key(10) , wrk(25)
      DIMENSION input(80)
      EQUIVALENCE (record(1),bufer(1))
CC
CC TOTAL IS THE NUMBER OF POLYGON VALUES PER RECORD( 64POLYGOS/RECORD )
CC
      total = 64
      index = 1
      IF ( .NOT.(batch) ) THEN
         WRITE (nprnt,99001)
99001    FORMAT (' ENTER 1 IF VALUES WILL BE ENTERED FROM TERMINAL ',
     &           /' ENTER 2 IF VALUES WILL BE ENTERED FROM DISK'\)
         CALL readin(icard,lenicd,icardp,istop)
         CALL inum(icard,record(1),icardp)
         IF ( record(1).GT.1 ) THEN
CC
CC
CC THE POLYGON VALUES SHALL BE READ IN FROM AN EXTERNAL FILE
CC THE USER MUST TYPE IN THE FILES NAME AND THE INPUT FORMAT STATEMENT.
CC
            WRITE (nprnt,99002)
99002       FORMAT (' PLEASE ENTER NAME OF EXTERNAL INPUT FILE'\)
            CALL readin(input,80,k,istop)
            CALL decip(input,record,k)
            CALL packc(record,wrk,10)
            CALL openf(51,wrk,1,0,0,ier)
            IF ( ier.NE.0 ) THEN
CC
               WRITE (nprnt,99003)
99003          FORMAT (' THE EXTERNAL INPUT FILE DID NOT OPEN CORRECTLY'
     &                 )
               RETURN
            ELSE
CC
               WRITE (nprnt,99004)
99004          FORMAT (
     &              ' PLEASE ENTER THE INPUT FORMAT. REMEMBER THAT REAL'
     &              ,/' NUMBERS REQUIRE F FORMAT (Example (F4.1)',/,
     &              ' ? '\)
            ENDIF
         ELSE
CC
CC THE VALUES FOR EACH POLYGON IS ENTERED IN FROM THE TERMINAL
CC
            DO subscpt = 1 , polygon
CC
               IF ( index.GT.total ) THEN
CC
CC 64 POLYGON VALUES READ IN TIME TO WRITE OUT THE VALUES TO DATA BASE
CC
                  CALL wrblk(chanel,adress,record,1,ier)
                  adress = adress + 1
                  index = 1
               ENDIF
CC
 
               WRITE (nprnt,99005) subscpt
99005          FORMAT (' PLEASE ENTER VALUE FOR FEATURE ',I5,\)
               CALL readin(icard,lenicd,icardp,istop)
               CALL fnum(icard,80,bufer(index),icardp)
               index = index + 1
CC
            ENDDO
CC
            IF ( index.NE.1 ) THEN
               CALL wrblk(chanel,adress,record,1,ier)
               adress = adress + 1
            ENDIF
CC
            RETURN
         ENDIF
      ENDIF
      READ (ioin,99006) form
99006 FORMAT (A20)
CC
CC READING IN THE VALUES FOR EACH POLYGON
CC
      DO subscpt = 1 , polygon
CC
         IF ( index.GT.total ) THEN
            CALL wrblk(chanel,adress,record,1,ier)
            adress = adress + 1
            index = 1
         ENDIF
CC
         READ (51,form) bufer(index)
         index = index + 1
CC
      ENDDO
CC
      IF ( index.NE.1 ) THEN
         CALL wrblk(chanel,adress,record,1,ier)
         adress = adress + 1
      ENDIF
CC
      IF ( .NOT.batch ) CLOSE (51,IOSTAT=ier)
      RETURN
      END
