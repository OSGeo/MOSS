C********************************************************************** 
C 
C          S U B R O U T I N E   C E L P R M 
C 
C  ROUTINE GETS THE PARAMETERS FOR CELL MAP HISTOGRAMS 
C    BARBRK HOLDS MIN AND MAX VALUES OF EACH BAR 
C 
C      PLOTPRM (1) = NUMBER OF INTERVALS 
C              (2) = MAPTYPE 
C              (3) = MIN X 
C              (5) = MAX X 
C              (7) = MIN Y 
C              (9) = MAX Y 
C             (11) = TITLE LEN 
C          (12-21) = MAP NAME 
C********************************************************************** 
C 
       SUBROUTINE CELPRM (PLOTPRM,BARBRK,CHANGE,OK) 
C 
       COMMON /IO/    NPRNT,IOIN 
C 
       REAL BARBRK(100,2) 
       REAL INC 
       REAL XMIN,XMAX,YMIN,YMAX,RHOLD 
C 
       INTEGER PLOTPRM(100),HOLDPRM(100) 
       INTEGER*2 OUTARRAY(100) 
       CHARACTER*100 ORA 
       INTEGER PACKED(100) 
       INTEGER ICARD(80),ICOM(80),K 
       INTEGER MAPTYPE 
       INTEGER NSUBS,IHOLD 
C 
       LOGICAL OK,CHANGE 
C 
       EQUIVALENCE (OUTARRAY(1),ORA) 
       EQUIVALENCE (HOLDPRM(1),NSUBS) 
       EQUIVALENCE (HOLDPRM(3),XMIN) 
       EQUIVALENCE (HOLDPRM(5),XMAX) 
       EQUIVALENCE (HOLDPRM(7),YMIN) 
       EQUIVALENCE (HOLDPRM(9),YMAX) 
C 
       CHANGE = .FALSE. 
       IBLNK  = 8224 
C 
C   TRANSFER PARAMETERS TO ENABLE MANIPULATION IN ROUTINE 
       DO 100 I = 1,100 
          HOLDPRM(I) = PLOTPRM(I) 
100    CONTINUE 
C 
       MAPTYPE = HOLDPRM(2) 
C 
C  Histogram TITLE 
       LEN = (HOLDPRM(11)+1)/2 
       IF (LEN .GT. 15 ) GO TO 130 
          WRITE(NPRNT,1120) (HOLDPRM(I),I =12,11+15) 
1120      FORMAT(' ENTER TITLE ( ',15A2, ') : '\) 
          GO TO 140 
130    WRITE(NPRNT,1130) 
          WRITE(NPRNT,1140) (HOLDPRM(I),I =12,11+30 ) 
1130      FORMAT(' ENTER TITLE: '\) 
1140      FORMAT(3X,'( ',30A2,')'\) 
C 
140    call readin(ICARD,80,K,LENICD) 
       CALL PACKC(ICARD,OUTARRAY,80) 
       k=100
       CALL LASTC(OUTARRAY,K) 
       IF ( K .LT. 1 ) GO TO 150 
          LEN = K 
          HOLDPRM(11) = LEN 
          DO 200 I=1,LEN+1/2 
             HOLDPRM(11+I) = OUTARRAY(I) 
200       CONTINUE 
C 
C 
150    WRITE(NPRNT,1150)HOLDPRM(1) 
1150   FORMAT(' NUMBER OF INTERVALS (', I3,' ) : '\) 
       call readin (ICARD,80,K,LENICD) 
       CALL INUM(ICARD,IHOLD,K) 
       IF (IHOLD .EQ. 0 ) GO TO 160 
           NSUBS = IHOLD 
           CHANGE = .TRUE. 
C 
160    WRITE(NPRNT,1160)XMIN 
1160   FORMAT(' ENTER MINIMUM X VALUE (',F10.2,') :'\) 
       call readin (ICARD,80,K,LENICD) 
       CALL FNUM(ICARD,80,RHOLD,K) 
       IF (RHOLD .EQ. 0 ) GO TO 170 
          XMIN = RHOLD 
          CHANGE = .TRUE. 
C 
170    WRITE(NPRNT,1170)XMAX 
1170   FORMAT(' ENTER MAXIMUM X VALUE (',F10.2,') :'\) 
       call readin (ICARD,80,K,LENICD) 
       CALL FNUM(ICARD,80,RHOLD,K) 
       IF (RHOLD .EQ. 0 ) GO TO 300 
          XMAX = RHOLD 
          CHANGE = .TRUE. 
C 
C  calculate interval values 
C 
300    INC =  (XMAX + 1. - XMIN)/FLOAT(HOLDPRM(1)) 
       BARBRK(1,1) = XMIN 
       IF (INC .GE. 1 ) GO TO 350 
          BARBRK(1,2) = XMIN + INC 
          GO TO 360 
350    CONTINUE 
          INC = AINT(INC) 
          BARBRK(1,2) = XMIN - 1 + INC 
360    DO 400 I=2,HOLDPRM(1) 
          BARBRK(I,1) = BARBRK(I-1,2) 
          BARBRK(I,2) = BARBRK(I-1,2) + INC 
400    CONTINUE 
       BARBRK(HOLDPRM(1),2) = XMAX 
C 
C  The following mess is basically for the benifit of a 
C  pretty prompt line.  The values of BARBRK are encoded to a 
C  character string.  Then that string is stripped of the spaces 
C  that encode puts in between numbers. 
C 
500    call fillc(OUTARRAY,100,IBLNK) 
       IF (MAPTYPE .EQ. 8) GO TO 550 
C          ENCODE(OUTARRAY,1210) ( AINT(BARBRK(I,2)),I=1,NSUBS) 
       WRITE(ORA,1210) ( HFIX(BARBRK(I,2)),I=1,NSUBS) 
1210      FORMAT(100(I6,',') ) 
          CALL COMPRESC(OUTARRAY,100,PACKED,NDIM) 
          GO TO 700 
550    IF (NSUBS .GT. 7 ) GO TO 600 
C 
C          ENCODE(OUTARRAY,1220) (BARBRK(I,2),I=1,NSUBS) 
       WRITE(ORA,1220) ( (BARBRK(I,2)),I=1,NSUBS) 
1220      FORMAT(100(F10.3,',') ) 
          CALL COMPRESC(OUTARRAY,100,PACKED,NDIM) 
          GO TO 700 
600    CONTINUE 
          WRITE(ORA,1230) (BARBRK(I,2),I=1,3),BARBRK(NSUBS,2) 
1230      FORMAT(3(F10.3,','),'. . .',F10.3) 
          CALL COMPRESC(OUTARRAY,100,PACKED,NDIM) 
C 
700    CONTINUE 
C ENDIF 
C 
       WRITE(NPRNT,1240) 
1240   FORMAT(' ENTER RANGE DELIMITERS ( ex: 2,4 TO 9, ,100 ):'\) 
c       WRITE (NPRNT,1250) (PACKED(I),I=1,NDIM) 
1250   FORMAT(5X,80A2,\) 
C 
       call readin(ICARD,80,K,LENICD) 
       call decip(ICARD,ICOM,K) 
       IF (ICOM(1) .EQ. 0 ) GO TO 750 
          CHANGE = .TRUE. 
          CALL PARSPRMS(ICARD,80,BARBRK,NSUBS,OK) 
          IF ( .NOT. OK ) GO TO 500 
C 
          call fillc(OUTARRAY,100,IBLNK) 
          IF (MAPTYPE .EQ. 8 ) GO TO 720 
             WRITE(OUTARRAY(1),1210) (HFIX(BARBRK(I,2)),I=1,NSUBS) 
             GO TO 740 
720       IF ( NSUBS .GT. 7 ) GO TO 730 
             WRITE(OUTARRAY(1),1220) (BARBRK(I,2),I=1,NSUBS) 
             GO TO 740 
730       CONTINUE 
             WRITE(OUTARRAY(1),1230) (BARBRK(I,2),I=1,3),BARBRK(NSUBS,2) 
740       CALL COMPRESC(OUTARRAY,100,PACKED,NDIM) 
          WRITE (NPRNT,1250) (PACKED(I),I=1,NDIM) 
C 
750    DO 800 I=1,100 
          PLOTPRM(I) = HOLDPRM(I) 
800    CONTINUE 
C 
      write(*,71)
71    format(/,' Histogram will be drawn to the screen. The Histogram',
     +/,' legend will be written to a file named: "HISTOGRM.LGN".  You',
     +/,' will need to copy the Histogram to a printer in order to',
     +/,' compare it with its legend. After viewing and/or printing',
     +/,' the histogram - Hit CR to continue.',//,' Hit CR to proceed'\)
       call readin(iget,1,ic,is)
C
       write(*,*)
       WRITE (*,*) '  HISTOGRAM RUNNING ' 
C 
9000   CONTINUE 
       RETURN 
       END 
