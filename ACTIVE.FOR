C *****************************************************************
C
C         SUBROUTINE    A C T I V E
C
C  FUNCTION - TELL THE USER WHAT DATA SETS ARE PRESENTLY ACTIVE
C
C
C ****************************************************************
$Include:'DBCHAN.inc'
$Include:'IO.inc'
$Include:'WHMAP.inc'
      COMMON /TYPE/  IDATYP(40),NACTS
      COMMON /FLS/   IUNIT,IFILE(5)
C
      DIMENSION IDESC(31),MAPNAME(5),iwrk(30),map(10)
      LOGICAL MCOMP
C
      CHARACTER*10 ACTION(11)
      character*1 ma,ans
C
       ACTION(1) = 'ALL       '
       ACTION(2) = 'SUBJECTS  '
       ACTION(3) = 'ITEMS     '
       ACTION(4) = 'ATTRIBUTE '
       ACTION(5) = 'ELEVATION '
       ACTION(6) = 'FROM      '
       ACTION(7) = 'SIZE      '
       ACTION(8) = 'CONTIGUITY'
       ACTION(9) = 'SAMPLE    '
       ACTION(10)= 'FROM      '
       ACTION(11)= '??????????'
C
C
C   General initialization, reading common area in from incm
      CALL INITL(IER)
      IF ( IER .NE. 0 ) GO TO 8200
C
C  Anything active?
       IF ( NSTRCT .LT. 1 ) THEN
          WRITE(NPRNT,1111)
1111      FORMAT('  CURRENTLY THERE ARE NO ACTIVE MAPS' )
          GO TO 15
       ENDIF
C
C  Open describe.DT  file
       CALL OPENF(ICDCDT,IDESDT,2,62,2,IER)
       IF ( IER .NE. 0 ) GO TO 8100
C
C  Print active table header
      WRITE(NPRNT,1112)
1112  FORMAT(/,' THE FOLLOWING MAPS ARE CURRENTLY ACTIVE',/)
      write(*,*)'  * = map is in MASTER directory'
      write(*,*)
      WRITE(NPRNT,1113)
1113  FORMAT(' ID    MAP     TYPE  ITEMS   SELECTION CRITERIA')
      WRITE(NPRNT,1114)
1114  FORMAT(1X,35(2H--))
C
C  Write active data set information
      lines=6
      K=0
      NTOT=0
      ip=nstrct/7+1
      DO 100 I=1,NSTRCT,7
         K=K+1
         READ(ICDCDT,REC=K,IOSTAT=IER) IDESC
         IF ( IER .NE. 0) THEN
            CALL FILLC(IDESC,30,'  ')
         ENDIF
         IF (IDESC(31) .LT. 1  .OR.  IDESC(31) .GT. 10) IDESC(31)=11
C
C  if type is items - need to convert to actual numbers
C
      if(idesc(31).eq.3)then
      do 3 ll=1,30
   3  if(idesc(ll).ne.0)kj=ll
      endif
C
C  Pad ISTRCT with spaces for nice output
         DO 50 LOOP=1,5
               MAPNAME(LOOP) = IABS(ISTRCT(I+1+LOOP))
 50      CONTINUE
C
         call fixchr(mapname,10)
C
         ma=' '
         if(istrct(I+2).lt.0)ma='*'
         if(idesc(31).ne.3)then
           WRITE(NPRNT,1115) K,ma,(MAPNAME(ll),ll=1,5),IDATYP(K),
     +              ISTRCT(I+1),ACTION(IDESC(31)),(IDESC(KL),KL=1,15)
1115       FORMAT(1X,I2,2X,a1,5a2,1x,I2,3X,I5,3X,A10,2X,15A2)
         else
           write(nprnt,1116)k,ma,(mapname(ll),ll=1,5),idatyp(k),
     +              istrct(i+1),action(idesc(31)),(idesc(kl),kl=1,kj)
 1116      format(1x,i2,2x,a1,5a2,1x,i2,3x,i5,3x,a10,2x,6i5,/,16i5,/,
     +            8i5)
         endif
C
          lines=lines+1
          if(lines.gt.20)then
           write(*,1161)
1161       format(1x,'Continue (Y)/N ?',\)
           read(*,'(a1)')ans
            if((ans.ne.'Y'.and.ans.ne.'y').and.ans.ne.' ')go to 15
C           write(*,*)char(27),char(12)
C            WRITE(NPRNT,1112)
C            write(*,*)
C            write(*,*)'  * = map is in MASTER directory'
C            write(*,*)
C            WRITE(NPRNT,1113)
C            WRITE(NPRNT,1114)
            lines=1
         endif
C
 99      NTOT=NTOT+ISTRCT(I+1)
 100  CONTINUE
C
      WRITE(NPRNT,1114)
      CALL CONTIN(LINES,ISTOP)
      IF(ISTOP.EQ.1) GO TO 15
      WRITE(NPRNT,1117) NTOT
 1117 FORMAT(' TOTAL ITEMS',10x,i5)
C
C UPDATE CURSOR POSITION
C
  15  CONTINUE
      CLOSE(ICDCDT)
      GO TO 99999
C
C ERRORS
C
 8100  WRITE(NPRNT,98100)IER
98100  FORMAT(' *ACTIVE* ERROR OPENNING DESCRIBE.DT FILE, IER=',I5)
       GO TO 99999
C
 8200  WRITE(NPRNT,98200)
98200  FORMAT(' *ACTIVE*   ERROR FROM INITL, TROUBLE WITH INTERPROC')
       GO TO 99999
C
99999  continue
       END
