      SUBROUTINE diget
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC NAME : SUBROUINE DIGET
CC
CC FUNCTION : TO READ IN THE POLYGON VALUES  OF AN ATTRIBUTE. SPECIFICALLY
CC            WHEN THE VALUES ARE OF TYPE INTEGER.
CC
CC PARAMETERS :
CC
CC FILES : WHEN THE USER WISHES TO ENTER THE POLYGON VALUES FROM A FILE
CC         THE USER MUST SPECIFY THE FILE'S NAME AND THE INPUT FORMAT. THE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
$Include:'IO.inc'
$Include:'STRNG.inc'
      COMMON /batat / imap(25) , kmap(25) , batch
      COMMON /string/ input(80) , k
      COMMON /unit  / chanel , atrbte , polygon , adress
      INTEGER chanel , atrbte , polygon , action , adress , total , 
     &        subscpt , record(128) , key(25)
      CHARACTER formatr*20
      LOGICAL batch
CC
CC TOTAL IS THE NUMBER OF POLYGON VALUES PER RECORD
CC
      total = 128
      index = 1
      IF ( .NOT.(batch) ) THEN
         WRITE (nprnt,99001)
99001    FORMAT (' PLEASE TYPE IN ',
     &           //'  1  IF ATTRIBUTES WILL BE ENTERED VIA THE KEYBOARD'
     &           /'  2  IF ATTRIBUTES WILL BE READ IN FROM DISK FILE'\)
         CALL readin(icard,lenicd,icardp,istop)
         CALL inum(icard,record(1),icardp)
         IF ( record(1).GT.1 ) THEN
CC
CC TYPE IN THE POLYGON VALUES FILE AND THE INPUT FORMAT STATEMENT
CC
            WRITE (nprnt,99002)
99002       FORMAT (' PLEASE ENTER NAME OF EXTERNAL DISK FILE '\)
            CALL readin(input,80,k,istop)
            CALL decip(input,record,k)
            CALL packc(record,key,10)
            CALL openf(52,key,1,0,0,ier)
            IF ( ier.NE.0 ) THEN
CC
               WRITE (nprnt,99003)
99003          FORMAT (' THE EXTERNAL DATA INPUT FILE DID NOT OPEN')
               RETURN
            ELSE
CC
               WRITE (nprnt,99004)
99004          FORMAT (
     &        ' PLEASE ENTER INPUT FORMAT (REMEMBER TO USE A FORMAT FOR'
     &        /' CHARACTER DATA, I FORMAT FOR INTEGER, AND F FOR REAL).'
     &        ,'(Example (I5)',/,' ? ',\)
            ENDIF
         ELSE
CC
CC READ IN THE POLYGON VALUES THOURGH THE TERMINAL
CC
            DO subscpt = 1 , polygon
CC
               IF ( index.GT.total ) THEN
CC
CC THE RECORD IS FULL MUST WRITE IT OUT TO THE DATA BASE
CC
                  CALL wrblk(chanel,adress,record,1,ier)
                  adress = adress + 1
                  index = 1
               ENDIF
CC
 
               WRITE (nprnt,99005) subscpt
99005          FORMAT (' PLEASE ENTER VALUE FOR FEATURE ',I4,\)
               CALL readin(icard,lenicd,icardp,istop)
               CALL inum(icard,record(index),icardp)
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
      READ (ioin,99006,END=100) formatr
99006 FORMAT (A20)
CC
CC
CC READIND IN THE VALUES FOR EACH POLYGON
CC
      DO subscpt = 1 , polygon
CC
         IF ( index.GT.total ) THEN
            CALL wrblk(chanel,adress,record,1,ier)
            adress = adress + 1
            index = 1
         ENDIF
CC
         READ (51,formatr,END=100) record(index)
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
 100  CONTINUE
      WRITE (nprnt,99007)
99007 FORMAT (' END OF FILE ON ATTRIBUTES INPUT FILE (DIGET)')
      RETURN
      END
