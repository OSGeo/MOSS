C**********************************************************
C
C        S U B R O U T I N E  H S T L E G
C
C       ROUTINE DETERMINES AND WRITES LEGEND FOR HSTOGRAMS
C
C       INPUTS: IDD  = STRUCTURE TABLE I.D. OF ACTIVE MAP OF INTEREST
C              LINES = CURSOR UPDATE COUNTER
C              ICHAN = OPEN CHANGE
C              DEVICE = DEVICE NUMBER
C              NSUBS = NUMBER OF BARS OR DIVISIONS
C              MAPTYPE = 7 DESCRETE, 8 CONTINUES
C              NATRBYT = NUMBER OF ATRIBUTE BYTES
C              BARBRK = INTERVAL BOUNDS FOR BARS
C              BARFRQ = FREQUENCY OF EACH BAR
C              NUMB = FREQUENCY AND RECORD POINTER TO ATTRIBUTE
C
C     ********************************************************
C
      SUBROUTINE hstleg(idd,ichan,idevice,ns,maptype,natrbyt,barbrk,
     &                  barfrq,numb)
C
      COMMON /io    / nprnt , ioin
C
      INTEGER ibuff(256) , map(15)
      INTEGER ix(100) , numb(200)
C
      REAL area(100)
      REAL barbrk(100,2)
      REAL barfrq(100)
C
      LOGICAL change
C
C     DISPLAY HISTOGRAM LEGEND
C
      nsubs = ns
      OPEN (12,FILE='histogrm.lgn',STATUS='UNKNOWN')
      REWIND (12)
      irecl = natrbyt*2
      change = .FALSE.
C      call ready (LINES )
      CALL gtnam(idd,0,'  ',map,ier)
C
C  check if discrete map intervals are special
      change = .FALSE.
      IF ( .NOT.((maptype.EQ.8) .OR. ((maptype.GE.1) .AND. (maptype.LE.3
     &     ))) ) THEN
         DO i = 1 , nsubs
            IF ( barbrk(i,2).NE.i ) change = .TRUE.
         ENDDO
      ENDIF
C
C  print headers
      WRITE (12,99001) (map(l),l=1,10)
99001 FORMAT (/,10X,' LEGEND FOR HISTOGRAM OF ',10A2,/)
      IF ( (maptype.NE.8) .AND. (.NOT.change) ) THEN
         WRITE (12,99002)
99002    FORMAT (//,3X,'BAR',3X,'FREQUENCY',2X,'ATTRIBUTE')
      ELSE
         WRITE (12,99003)
99003    FORMAT (//,3X,'BAR',5X,' FROM',9X,' TO',8X,' FREQUENCY')
      ENDIF
C
C  Print different values for Descrete and Continuous
      k1 = -1
      lines = 5
      istop = 0
      DO i = 1 , nsubs
         k1 = k1 + 2
         lines = lines + 1
C         IF ( LINES .GT. 22) CALL CONTIN(LINES,ISTOP)
         IF ( istop.EQ.1 ) GOTO 100
         IF ( .NOT.((maptype.NE.8) .AND. (.NOT.change)) ) THEN
            WRITE (12,99004) i , barbrk(i,1) , barbrk(i,2) , barfrq(i)
99004       FORMAT (2X,I3,4X,F10.3,2X,F10.3,2X,F10.0)
C
         ELSEIF ( maptype.NE.7 ) THEN
C
C ELSE ITS POLYGON
            CALL getsub(ichan,numb(k1),ibuff,ix,items)
            WRITE (12,99005) i , barfrq(i) , (ix(j),j=1,natrbyt)
C
99005       FORMAT (2X,I3,2X,F10.0,3X,32A2)
         ELSE
            riv4 = i
            CALL dsin(ichan,ix,riv4,ier)
            IF ( ier.NE.1 ) THEN
C
               WRITE (*,99006) ier
99006          FORMAT (' *HSTLEG* ERROR FROM DSIN  ERROR',I5)
               lines = lines + 2
               GOTO 200
            ELSE
               WRITE (12,99005) i , barfrq(i) , (ix(j),j=1,natrbyt)
            ENDIF
         ENDIF
      ENDDO
C
 100  CONTINUE
      CLOSE (12)
C
C  ERROR RETURNS
C
C
cssk       WRITE(*,98502) (MAP(I),I=1,10),IER
cssk       LINES = LINES + 2
99007 FORMAT (' *HSTLEG* ERROR FROM OPEN OF MAP ',10A2,'  ERROR',I5)
C
C JUMP OUT
C
 200  CONTINUE
      RETURN
      END
