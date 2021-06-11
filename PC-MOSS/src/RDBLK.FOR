      SUBROUTINE rdblk(ichan,ipoint,ibuff,nrecs,ier)
C
C ***************************************************************
C
C              S U B R O U T I N E    R D B L K
C FUNCTION:
C             THIS ROUTINE READS A BLOCK FROM DISK FILE.
C PARAMETERS:
C             ICHAN     IN  - THE FILE CHANNEL.
C             IPOINT    IN  - THE RECORD TO READ.
C             IBUFF     IN  - THE RETURNED ARRAY.
C             IRECS     IN  - THE NUMBER OF RECORDS TO READ.
C             IER       OUT - THE ERROR RETURN FOR WRITING TO, READING,
C                             OPENNING, OR CLOSING A FILE,
C                             IER = 0, IF NO ERROR,
C                             IER > 1, IF AN ERROR.
C ROUTINES                    IER = -1 IF EOF W/O ERROR
C CALLED:     NONE.
C
C **************************************************************
C
      COMMON /io    / nprnt , ioin
      DIMENSION ibuff(128)
      ier = 0
      IF ( ipoint.GT.0 ) THEN
         READ (ichan,REC=ipoint,IOSTAT=ier,ERR=100) ibuff
         GOTO 200
      ENDIF
C
C  ERROR RETURN
C
 100  CONTINUE
      WRITE (nprnt,99001) ier , ipoint , ichan
99001 FORMAT (' *RDBLK*  ERROR',I5,' READING RECORD',I5,
     &        ' FROM CHANNEL ',I5)
C
 200  CONTINUE
      RETURN
      END
