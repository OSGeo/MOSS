C     *****************************************************
C
C         S U B R O U T I N E    H S T P O L
C
C     ROUTINE - SUBORDINATE TO STATISTICS.  HANDELS
C               POLYGON PROCESSING FOR HISTOGRAMS
C
C     INPUT PARAMETERS:
C
C       LINES   = CURSOR UPDATE COUNTER
C       IDD     = STRUCTURE TABLE I.D. OF ACTIVE MAP OF INTEREST
C       MAPTYPE = 1 OR 3
C
C
C***********************************************************
      SUBROUTINE hstpol(lines,idd,maptype,ier)
C
      COMMON /io    / nprnt , ioin
C
      INTEGER map(25) , numb(200) , mapname(25)
      INTEGER ibuff(256)
      INTEGER title(30) , itemp(15)
      INTEGER plotprm(100,2)
C
      REAL freq(100)
      REAL barbrk(100,2)
      REAL area(100)
C
      LOGICAL change , ok
C
      ier = 0
      ichan = 15
      ndim = 100
      iblnk = 8224
C
C  SET DEVICE TYPE
C
      idevic = 1
C
C   GET POLYGON MAP AND ITS POINTERS
C
      CALL gtnam(idd,0,iblnk,map,ier)
      CALL adddir(map,mapname)
      CALL openf(ichan,mapname,2,256,2,ier)
      IF ( ier.NE.0 ) THEN
C
         WRITE (nprnt,99001) (map(i),i=1,10) , ier
99001    FORMAT (' *HSTPOL* ERROR FROM OPEN OF MAP ',10A2,'  ERROR',I5)
         ier = 1
      ELSE
C
C   GET TITLE
         CALL rdblk(ichan,1,ibuff,1,ier)
         IF ( ier.NE.0 ) THEN
C
            WRITE (nprnt,99002) ier
99002       FORMAT (' *HSTPOL* ERROR READING HEADER  ERROR',I5)
            ier = 1
         ELSE
            DO i = 1 , 30
               title(i) = ibuff(i+19)
            ENDDO
            len = 30
            CALL lastc(title,len)
            plotprm(11,1) = len
C      DO 400 I=1,(LEN+1)/2
            DO i = 1 , len
               plotprm(i+11,1) = title(i)
            ENDDO
C
            nbyts = 15
C      IF (IER .NE. 0) GO TO 85020
            CALL apget(idd,istart,istop,ntotal)
C
C  GET FREQUENCY OF EACH SUBJECT TYPE
C
            jtype = 2
            jdim = ndim*2
            CALL unsub(jtype,istart,istop,numb,jdim,nsubs,area,ndim,
     &                 ichan)
            IF ( nsubs.LE.0 ) THEN
C
C  ERROR RETURNS
C
               WRITE (nprnt,99003)
99003          FORMAT (' *HSTPOL* NO VALUES TO PLOT')
               ier = 1
            ELSE
C
C  GET NEW TITLE (OR POTENTIONALLY OTHER PARAMETERS)
C
               CALL mosprm(plotprm(1,1),barbrk,chang,ok)
C
C  PUT COUNT IN ANOTHER ARRAY
C
               kn = 0
               IF ( nsubs.GT.ndim ) nsubs = ndim
               DO i = 1 , nsubs
                  kn = kn + 2
                  freq(i) = numb(kn)
               ENDDO
C
               CALL hstplt(freq,area,nsubs,plotprm(11,1),idevic)
C
C  Display Histogram Legend
c
C
               CALL hstleg(idd,ichan,idevic,nsubs,maptype,nbyts,barbrk,
     &                     freq,numb)
            ENDIF
         ENDIF
      ENDIF
C
cssk       WRITE(NPRNT,98504) IER
cssk       ier=1
99004 FORMAT (' *HSTPOL* ERROR FROM DSIN  ERROR',I5)
C
C JUMP OUT
C
      RETURN
      END
