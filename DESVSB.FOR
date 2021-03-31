      SUBROUTINE desvsb(ihead,mapnam,ichan,lines,ier)
C***********************************************************************
C
C     NAME:  SUBROUTINE DESVSB
C
C     FUNCTION:  DISPLAYS THE SUBJECTS FOR A VECTOR MAP.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IHEAD     I      I*2         256        +INTEGER
C                  HEADER INFORMATION
C                  -------------------------------------------------
C                  MAPNAM    I      I*2         10         CHAR
C                  MAP NAME
C                  -------------------------------------------------
C                  ICHAN     I      I*2                    +INTEGER
C                  CHANNEL THE MAP IS OPENED ON
C                  -------------------------------------------------
C                  LINES     I/O    I*2                    +INTEGER
C                  NUMBER OF LINES OF OUTPUT
C                  -------------------------------------------------
C                  IER       O      I*2                    INTEGER
C                  ERROR FLAG
C                  -------------------------------------------------
C
C     FILES:  MAP FILE AND OUTPUT FILE
C
C     ROUTINES REQUIRED:  OPENF  RDBLK  CLOSE
C
C     CALLING ROUTINE:  DESC  LABHDR
C
C     SYSTEM DEPENDENCE:  2-BYTE INTEGER
C
C     LANGUAGE DEPENDENCE:  FORTRAN
C
C***********************************************************************
C
$Include:'IO.inc'
$Include:'STRNG.inc'
      DIMENSION ihead(256) , ibuff(128) , mapnam(25) , linepr(25)
      LOGICAL mcomp , hard
      DATA linepr/'li' , 'ne' , '.p' , 'rn' , 21*32/
C
      istop = 0
C
C CHECK FOR HARD OPTION
C
      hard = .FALSE.
      kk = icardp
      CALL decip(icard,ibuff,icardp)
      IF ( mcomp(ibuff(1),'HARD',4,ier) ) hard = .TRUE.
      icardp = kk
C
C  IF HARD, CHANGE NPRNT AND OPEN LINE.PRINTER
C
      IF ( hard ) THEN
         nprnt = 100
         CALL openf(nprnt,linepr,0,0,0,ier)
      ENDIF
C
C  CALCULATE ADDRESS OF FIRST SUBJECT AND GET NUMBER OF SUBJECTS
C
      nrec = ihead(64) + 2
      nsubs = ihead(67)
C
C  ENTER PRINT OUT LOOP
C
      WRITE (nprnt,99001) (mapnam(i),i=1,12) , nsubs
99001 FORMAT (/,' FOR MAP ',12A2,' THERE ARE ',I5,' SUBJECTS',
     &        /' SUBJECT                          NUMBER'/)
      lines = lines + 5
      IF ( .NOT.hard ) CALL contin(lines,istop)
      IF ( istop.NE.1 ) THEN
C
         DO i = 1 , nsubs
            CALL rdblk(ichan,nrec,ibuff,1,ier)
            WRITE (nprnt,99002) (ibuff(j),j=1,15) , ibuff(17)
99002       FORMAT (1X,15A2,5X,I5)
            nrec = nrec + 1
            IF ( .NOT.hard ) CALL contin(lines,istop)
            IF ( istop.EQ.1 ) GOTO 100
         ENDDO
      ENDIF
C
 100  CONTINUE
      IF ( hard ) CLOSE (nprnt,IOSTAT=ier)
      IF ( hard ) nprnt = 6
C
      RETURN
      END
