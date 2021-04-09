C *********************************************************************
C
C                S U B R O U T I N E  S A V M A T
C
C  Purpose:  SAVE THE SPECIFIED MULTIPLE ATTRIBUTES FROM TWO MAP FILES
C            IN A NEW MULTIPLE ATTRIBUTE FILE.
C
C  Parameters:
C     NAME       Use        Type         Dimension       Description
C     ----       ---        ----         ---------   ------------------
C
C     ITEMS      I          INT                      NUMBER OF FEATURES IN NEW
C                                                    MAP
C
C     MARRY      I          INT          42          INPUT MULTIPLE ATT ARRAY
C                                                    1=# OF SAVED ATTS FOR ONE
C                                                      0-NONE -1-ALL
C                                                    2=# OF SAVED ATTS FOR TWO
C                                                      0-NONE -1-ALL
C                                                    3-22 ATT ID'S SAVED FOR ONE
C                                                    23-42ATT ID'S SAVED FOR TWO
C
C     NAME0      I          INT          10          PACKED NAME OF NEW MAP
C
C     NAME1      I          INT          10          PACKED NAME OF MAP ONE
C
C     NAME2      I          INT          10          PACKED NAME OF MAP TWO
C
C     ICHAN      I          INT                      FREE CHANNEL # TO USE
C
C     ICHAN1     I          INT                      FREE CHANNEL # TO USE
C
C     IER        O          INT                      OUTPUT ERROR 1 IS NO ERROR
C
C
C  Files:    Logical Unit       File NAME or description
C            ------------       ------------------------
C            ICHAN              MULTIPLE ATTRIBUTES FILE
C **********************************************************************
C
      SUBROUTINE iSAVMAT(ITEMS,MARRY,NAME0,NAME1,NAME2,ICHAN5,ICHAN1,
     1 IER)
$Include:'io.inc'
$Include:'pqpnts.500'
       INTEGER MARRY(42),NAME1(1),NAME2(1),NAME0(1),NAME(25),
     +         IBUFF(128),IBUFF1(128),ITEM(2),MAP(80)
       REAL RTEM
       LOGICAL IFLAG
       EQUIVALENCE (ITEM(1),RTEM)
C
C  INITALIZE ATTRIBUTE COUNT AND LOCATION FOR ATTRIBUTES
C
       KOUNT = 0
       LOCATE = 203
C
C  CREATE AND OPEN NEW MAP'S MULTIPLE ATTRIBUTE FILE
C
       ichan=40
       CALL GTNAM(0,4,NAME0,NAME,IER)
       CALL OPENF(ICHAN,NAME,0,256,2,IER)
       IF (IER .NE. 0) GO TO 85020
C
       CALL CLEAR(IBUFF,128)
       IBUFF(1) = MARRY(1) + MARRY(2)
       IBUFF(2) = ITEMS
C
       CALL WRBLK(ICHAN,1,IBUFF,1,IER)
       IF (IER .NE. 0) GO TO 85060
C
C  DO FIRST MAP IF REQUIRED
C
       IF (MARRY(1) .EQ. 0) GO TO 4000
C
       CALL GTNAM(0,4,NAME1,NAME,IER)
       CALL OPENF(ICHAN1,NAME,1,256,2,IER)
       IF (IER .NE. 0) GO TO 85020
C
       CALL RDBLK(ICHAN1,1,IBUFF,1,IER)
       IF (IER .NE. 0) GO TO 85040
C
       IFLAG = .FALSE.
       IF (MARRY(1) .EQ. -1) IFLAG = .TRUE.
       IF (MARRY(1) .EQ. -1) MARRY(1) = IBUFF(1)
C
C  LOAD THE ATTRIBUTE HEADER RECORDS
C
       DO 3800 I=1,MARRY(1)
         IF (.NOT. IFLAG) IREC = MARRY(2+I) + 1
         IF (IFLAG      ) IREC = I+1
         CALL RDBLK(ICHAN1,IREC,IBUFF,1,IER)
         IF (IER .NE. 0) GO TO 85040
C
         KOUNT = KOUNT + 1
         ITYP   = IBUFF(84)
         MWORDS = IBUFF(85)
         LOCOLD = IBUFF(86)
         LOCNEW = LOCATE
         IBUFF(1)  = KOUNT
         IBUFF(86) = LOCATE
         IBUFF(87) = ( (ITEMS-1) / (128/MWORDS) ) + 1
         LOCATE = LOCATE + IBUFF(87)
C  WRITE ATTRIBUTE HEADER RECORD
         CALL WRBLK(ICHAN,KOUNT+1,IBUFF,1,IER)
         IF (IER .NE. 0) GO TO 85060
C  WRITE END RECORD FOR THIS ATTRIBUTE DATA TO AVOID EOF IN PUTMAF
         IREC = IBUFF(86) + IBUFF(87)
         CALL CLEAR(IBUFF,128)
         CALL WRBLK(ICHAN,IREC,IBUFF,1,IER)
         IF (IER .NE. 0) GO TO 85060
C
C  LOAD THE ACTUAL ATTRIBUTE DATA
C
         IF (ITYP .EQ. 3) MWORDS = -(MWORDS)
         DO 2800 J = 1,ITEMS
                call sspoints('P ',p,rtem,j,1)
C           RTEM = P(J)
           CALL GETMAF(ICHAN1,MWORDS,LOCOLD,IBUFF,ITEM(1),MAP,VAL)
           IF (ITYP .EQ. 3) CALL UNPACK(MAP,ABS(MWORDS))
           CALL PUTMAF(ICHAN ,MWORDS,LOCNEW,IBUFF1,J,     MAP,VAL)
 2800    CONTINUE
 3800  CONTINUE
       CLOSE(ICHAN1,iostat=IER)
C
C  DO SECOND MAP IF REQUIRED
C
 4000  CONTINUE
       IF (MARRY(2) .EQ. 0) GO TO 8000
C
C  INITIALIZE THE INCORE STUFF IN CASE ON SAME RECORDS AS FIRST MAP
       CALL GTNAM(0,4,NAME2,NAME,IER)
       CALL OPENF(ICHAN1,NAME,1,256,2,IER)
       IF (IER .NE. 0) GO TO 85020
C
       CALL RDBLK(ICHAN1,1,IBUFF,1,IER)
       IF (IER .NE. 0) GO TO 85040
       IFLAG = .FALSE.
       IF (MARRY(2) .EQ. -1) IFLAG = .TRUE.
       IF (MARRY(2) .EQ. -1) MARRY(2) = IBUFF(1)
C
C  SEE IF TOO MANY
C
       IF (KOUNT + MARRY(2) .GT. 200) WRITE(NPRNT,94000) 200 - KOUNT
94000  FORMAT(' *SAVMAT*  TOO MANY ATTRIBUTES...ONLY SAVING',I5,
     +     ' FROM MAP 2')
       IF (KOUNT + MARRY(2) .GT. 200) MARRY(2) = 200 -KOUNT
C
       DO 7800 I=1,MARRY(2)
         IF (.NOT. IFLAG) IREC = MARRY(22+I) + 1
         IF (IFLAG      ) IREC = I+1
         CALL RDBLK(ICHAN1,IREC,IBUFF,1,IER)
         IF (IER .NE. 0) GO TO 85040
C
         KOUNT = KOUNT + 1
         ITYP   = IBUFF(84)
         MWORDS = IBUFF(85)
         LOCOLD = IBUFF(86)
         LOCNEW = LOCATE
         IBUFF(1)  = KOUNT
         IBUFF(86) = LOCATE
         IBUFF(87) = ( (ITEMS-1) / (128/MWORDS) ) + 1
         LOCATE = LOCATE + IBUFF(87)
C  WRITE THE ATTRIBUTE HEADER RECORD
         CALL WRBLK(ICHAN,KOUNT+1,IBUFF,1,IER)
         IF (IER .NE. 0) GO TO 85060
C  WRITE END RECORD FOR THIS ATTRIBUTE DATA TO AVOID EOF IN PUTMAF
         IREC = IBUFF(86) + IBUFF(87)
         CALL CLEAR(IBUFF,128)
         CALL WRBLK(ICHAN,IREC,IBUFF,1,IER)
C
C  LOAD THE ACTUAL ATTRIBUTE DATA
C
         IF (ITYP .EQ. 3) MWORDS = -(MWORDS)
         DO 6800 J = 1,ITEMS
                call sspoints('P ',p,rtem,j,1)
c           RTEM = P(J)
           CALL GETMAF(ICHAN1,MWORDS,LOCOLD,IBUFF,ITEM(2),MAP,VAL)
           IF (ITYP .EQ. 3) CALL UNPACK(MAP,ABS(MWORDS))
           CALL PUTMAF(ICHAN ,MWORDS,LOCNEW,IBUFF1,J,     MAP,VAL)
 6800    CONTINUE
         IF (IER .NE. 0) GO TO 85060
 7800  CONTINUE
       CLOSE(ICHAN1,iostat=IER)
C
C  RESET THE HEADER(FOR -1 CASE) AND CLOSE UP THE NEW ATTRIBUTE FILE
C
 8000  CONTINUE
       CALL RDBLK(ICHAN,1,IBUFF,1,IER)
       IBUFF(1) = MARRY(1) + MARRY(2)
       CALL WRBLK(ICHAN,1,IBUFF,1,IER)
       CLOSE(ICHAN,iostat=IER)
C
C  OPEN THE MAP FILE AND SET TO HAVING ATTRIBUTES
C
C       CALL GTNAM(0,0,NAME0,NAME,IER)
C       CALL OPENF(ICHAN,NAME,1,256,2,IER)
C       IF (IER .NE. 0) GO TO 85020
C
       CALL RDBLK(ICHAN5,1,IBUFF,1,IER)
       IF (IER .NE. 0) GO TO 85040
       IBUFF(76) = 1
       CALL WRBLK(ICHAN5,1,IBUFF,1,IER)
C       CLOSE(ICHAN,iostat=IER)
C
       GO TO 90000
C
C  ERROR RETURNS
C
85020  CONTINUE
       WRITE(NPRNT,98502) (NAME(I),I=1,15),IER
98502  FORMAT(' *SAVMAT*  ERROR OPENING FILE ',15A2,'  ERROR',I5)
       GO TO 90000
C
85040  CONTINUE
       WRITE(NPRNT,98504) IER
98504  FORMAT(' *SAVMAT*  ERROR READING ATTRIBUTE FILE  ERROR',I5)
       CLOSE(ICHAN,iostat=IER)
       CLOSE(ICHAN1,iostat=IER)
       GO TO 90000
C
85060  CONTINUE
       WRITE(NPRNT,98506) IER
98506  FORMAT(' *SAVMAT*  ERROR WRITING ATTRIBUTE FILE  ERROR',I5)
       CLOSE(ICHAN,iostat=IER)
       CLOSE(ICHAN1,iostat=IER)
       GO TO 90000
C
C  JUMP OUT
C
90000  CONTINUE
       RETURN
       END
