C**********************************************************************
C
C    PROGRAM  SUBEDIT
C
C    FUNCTION:  Routine edits and lists existing subjects in a map
C
C**********************************************************************
C
$Include:'IO.inc'
$Include:'STRNG.inc'
C
      INTEGER MAPNAME(25),ICOM(10),JCOM(10)
      INTEGER IBUFF(128),ISUB(60),JSUB(60)
      LOGICAL MCOMP
C
      ICHAN = 10
C
C   GENERAL INITIALIZATION, READING COMMON AREA IN FROM INCM
      CALL INITL(IER)
      IF ( IER .NE. 0 ) GO TO 8100
C
C  Get name of file to audit off of input line
100   CALL DECIP(ICARD,JCOM,ICARDP)
      IF (JCOM(1) .EQ. 0) THEN
C
C    Prompt the user to obtain the map name
         WRITE(NPRNT,1111)
1111     FORMAT(' Enter the name of the map you wish to edit',
     +          '  ( CR TO EXIT )'\)
         CALL READIN(ICARD,LENICD,ISTART,IEND)
         IF ( IEND .EQ. 0 ) GO TO 9999
         CALL DECIP(ICARD,JCOM,ISTART)
      ENDIF
C
      NPAGES=1
      LINES=0
C
C   Check to see if map exists
      CALL MCHEK(JCOM,IFIL,ITEMS,ITYPE,IER)
      IF( IFIL .EQ. 0 ) THEN
         WRITE(NPRNT,1112) JCOM
1112     FORMAT( ' Map ',A10,' Not found')
         call fillc(icard, 80, ' ' )
         GO TO 100
      ENDIF
      IF(ITYPE.LE.0.OR.ITYPE.GT.5) GO TO 8400
C
C   Open data channel 10 to map
       CALL CLEAR(MAPNAME,25)
C 
C    If requested map is in MASTER directory, add path to name
       IF ( IFIL .EQ. 1 ) THEN 
           CALL PACKC(JCOM,ICOM,10)
           CALL ADDDIR( ICOM,MAPNAME )
       ELSE
           CALL PACKC(JCOM,MAPNAME,10)
       ENDIF
       CALL OPENF(ICHAN,MAPNAME,1,256,2,IER)
       IF( IER .NE. 0) GO TO 8200
C
      CALL RDBLK(ICHAN,1,IBUFF,1,IER)
      IF ( IER .NE. 0) GO TO 8300
      KPOINT=IBUFF(64)+2
      NSUBS=IBUFF(67)
      ITYPE=1
C
C  LOOP FOR SEVERAL POSSIBLE CHANGES
C
200   CONTINUE
         WRITE(NPRNT,1113)
1113     FORMAT(' 1 = EXIT ',/,
     +          ' 2 = CHANGE BY SUBJECT STRING',/,
     +          ' 3 = CHANGE BY SUBJECT NUMBER',/,
     +          ' 4 = LIST SUBJECTS '\)
         CALL READIN(ICARD,LENICD,ISTART,ISTOP)
         CALL INUM(ICARD,IANS,ISTART)
         IF(IANS .LT. 1 .OR. IANS .GT. 4) GO TO 200
         IF ( IANS .EQ. 1 ) GOTO 9999
         IF ( IANS .EQ. 2 ) THEN
            IREC=KPOINT
            WRITE( NPRNT,1114)
1114        FORMAT( ' ENTER SEARCH STRING',
     +              ' ( less than or equal to 30 characters',/,
     +              '------------------------------')
            CALL READIN(ISUB,30,ISTART,IEND)
C            CALL UNPACK(ISUB,60)
            DO 90 I=1,NSUBS
               CALL RDBLK(ICHAN,IREC,IBUFF,1,IER)
               IF( IER .NE. 0 ) GO TO 8300
C
C****          OLD WAY
C
C              DO 50 J=1,15
C
C****          NEW WAY
C
               DO 50 J=1,30
C
                  JSUB(J)=IBUFF(J)
50             CONTINUE
               CALL UNPACK(JSUB,30)
               do 51 ll=1,iend
                if(isub(ll).ne.jsub(ll))go to 53
51             continue
C
C****         OLD WAY
C
C             write(nprnt,52)(jsub(ll),ll=1,15)
C
C****         NEW WAY
C
              write(nprnt,52)(jsub(ll),ll=1,30)
52    format(/,1x,'Possible MATCH ??:',30a1,' <CR = Yes> '\)
              call readin(iget,1,istart,inumbr)
              if((inumbr.ne.0).and.(.not.mcomp(iget,'Y',1,ier)))go to 53
                  CALL UPSUBJ(ICHAN,IREC,IBUFF,IER)
                  IF (IER .NE. 0 ) GO TO 8400
                  GO TO 200
53               IREC=IREC+1
90          CONTINUE
            WRITE(NPRNT,1115)
1115        FORMAT(' NO MATCH FOUND ')
         ELSE IF ( IANS .EQ. 3 ) THEN
C
C     USER ENTERS RECORD NUMBER.  EASY WAY TO DO IT
C
  400       CONTINUE
            WRITE( NPRNT,1116)
1116        FORMAT( ' Enter record number for subject record',
     +              ' (CR=Return to select options menu) '\)
            CALL READIN(ICARD,LENICD,ISTART,IEND)
            IF(IEND .LE. 0) GO TO 200
            CALL INUM(ICARD,IREC,ISTART)
            IF( IREC .GT. NSUBS ) THEN
               WRITE(NPRNT,1117)
1117           FORMAT( I5,' Greater than number of subjects')
               GO TO 400
            ENDIF
            IREC=IREC+2
            CALL RDBLK(ICHAN,IREC,IBUFF,1,IER)
            IF ( IER .EQ. 0 ) THEN
               CALL UPSUBJ(ICHAN,IREC,IBUFF,IER)
            ELSE
               WRITE(NPRNT,1118)IREC
1118           FORMAT(' UNABLE TO FIND RECORD ',I5)
               GO TO 400
            ENDIF
         ELSE IF ( IANS .EQ. 4 ) THEN
            IREC = KPOINT
            WRITE(NPRNT,1119)
1119        FORMAT(/)
            DO 93  I = 1, NSUBS
               CALL RDBLK(ICHAN,IREC,IBUFF,1,IER)
               WRITE(NPRNT,1120) I,(IBUFF(J),J=1,15)
C
C****          OLD WAY
C
C1120           FORMAT(' 1) ',I3,1X,15A2)
C
C****          NEW WAY
C
1120           FORMAT(' ',I3,')',1X,15A2)
C
               IREC = IREC+1
93          CONTINUE
C
C****       NEW WAY
C
            WRITE(NPRNT,1121)
1121        FORMAT(/)
         ENDIF
C
      GO TO 200
C**********************************************************************
C    ERROR EXITS
C**********************************************************************
 8100  WRITE(NPRNT,98100)
98100  FORMAT(' *SUBEDIT*   ERROR FROM INITL, TROUBLE WITH INTERPROC')
       GO TO 9999
C
 8200   WRITE(NPRNT,98200)IER,MAPNAME
98200   FORMAT(' *SUBEDIT*  ERROR OPENING MAP',A25,'  ERR=',I5)
        GO TO 9999
C
 8300   WRITE(NPRNT,98300)IREC,IER
98300   FORMAT(' *SUBEDIT* ERROR READING REC ',I5,
     +         ' FROM RDBLK ERROR ',I5)
        GO TO 9999
C
 8400   WRITE(NPRNT,98400)
98400   FORMAT(' *SUBEDIT* CAN ONLY AUDIT VECTOR MAPS')
        LINES=LINES+2
        GO TO 9999
C
9999    CLOSE (ICHAN)
        END
