      COMMON /IO/ NPRNT,IOIN
$Include:'strng.inc'
C
      INTEGER IBUFF(128),MAP(25),NAME(25),ICOM(10),JCOM(10),
     +        OUTBUF(72),ITYP(10),lenth(10),KEY(5,10),IDESC(30,10),
     +        bufout(72)
      character dumb*10,lasta*3,lengtha*2,formt*72
C
      LOGICAL MCOMP,CKLEN
C
C ************************************************************
C
C               P R O G R A M    S U B T O A T T
C
C  FUNCTION:  THIS ROUTINE ALLOWS THE PC-MOSS USER TO CREATE A
C             MULTIPLE ATTRIBUTES INPUT FILE FROM AN EXISTING
C             LIST OF SUBJECTS IN AN EXISTING PC-MOSS MAP.
C *************************************************************
C
      call initl(ier)
      if(ier.ne.0)stop
C
      ICHAN1=10
      ICHAN2=51
      IBLNK = 2h
      CKLEN = .FALSE.
      MXATT = 10
      ISUBLN= 30
C
      WRITE(NPRNT,90020)
90020 FORMAT('    PC-MOSS SUBJECT TO ATTRIBUTE INPUT REFORMATER',//
     +       ' Please enter the name of the desired PC-MOSS map'\)
C
      CALL READIN(ICARD,lenicd,icardp,istop)
      CALL DECIP(ICARD,MAP,icardp)
      CALL GTNAM(-1,0,MAP,NAME,IER)
C
C  SEE IF THIS FILE EXISTS ON DISK.
C
      CALL OPENF(ICHAN1,NAME,1,256,2,IER)
      IF(IER.NE.0) GO TO 85020
C
C  GET NAME FOR ATTRIBUTES INPUT FILE
C
      WRITE(NPRNT,90040)
90040 FORMAT(' What do you wish to call the attribute input file? '\)
C
      CALL READIN(ICARD,lenicd,icardp,istop)
      CALL DECIP(ICARD,MAP,icardp)
      CALL GTNAM(-1,0,MAP,NAME,IER)
C
C  TRY TO CREATE NEW ENTRY ON DISK
C
C
      CALL OPENF(ICHAN2,NAME,0,0,0,IER)
      IF(IER.NE.0) GO TO 85020
C
C  NOW GET CONVERSION PARAMETERS FROM USER
C
      WRITE(NPRNT,90060)
90060 FORMAT(/,'  Translation information:',/,
     + ' How many attribute fields are contained in the subject?'\)
      CALL READIN(ICARD,lenicd,icardp,istop)
      CALL INUM(ICARD,NVARS,icardp)
      IF(NVARS.LE.0  .OR.  NVARS.GT.MXATT) GO TO 80000
C
      CALL CLEAR(ICOM,MXATT)
      CALL CLEAR(JCOM,MXATT)
      CALL CLEAR(ITYP,MXATT)
      CALL CLEAR(lenth, MXATT)
C
      DO 400 I=1,NVARS
        WRITE(NPRNT,90220) I
90220   FORMAT(/,' Enter information for field ',I5,/,
     +           ' Please enter a key '\)
        CALL READIN(ICARD,lenicd,icardp,istop)
        do 2 l=istop+1,10
    2   icard(l)=32
        CALL PACKC(ICARD,KEY(1,I),10)
C
        WRITE(NPRNT,90240)
90240   FORMAT(' Please enter a descripion '\)
        CALL READIN(ICARD,lenicd,icardp,istop)
        do 3 l=istop+1,60
    3   icard(l)=32
        CALL PACKC(ICARD,IDESC(1,I),60)
C
C
 260    CONTINUE
        WRITE(NPRNT,90260)
90260   FORMAT(' Enter 1 for integer, 2 for real, or 3 for character '\)
        CALL READIN(ICARD,lenicd,icardp,istop)
        CALL INUM(ICARD,ITYP(I),icardp)
        IF(ITYP(I).LE.0.OR.ITYP(I).GT.3) GO TO 260
C
        WRITE(NPRNT,90280)
90280   FORMAT(' Please Enter :',/
     +'         a field delimiter (CR=space)',/
     +' or      a negative number for number of characters (ie.-3) '\)
C
        CALL READIN(ICARD,lenicd,icardp,istop)
        IF(MCOMP(ICARD(1),'-',1,IER)) GO TO 360
       if(icard(1).eq.0)icard(1)=32
C
C  DELIMITER
        JCOM(I)=ICARD(1)
        GO TO 380
C
C  COUNT
 360    CONTINUE
          ICOM(I)=1
          CALL INUM(ICARD(2),JCOM(I),icardp)
          IF (JCOM(I) .LT. 1  .OR.  JCOM(I) .GT. ISUBLN) GO TO 80020
C
C  SET OUTPUT LENGTH
C  INTEGER VALUES WILL BE OUTPUT AS I6
C  REAL VALUES WILL BE OUTPUT AS F10.4
C  CHARACTERS WILL BE OUTPUT AT LENGTH SPECIFIED OR MAX IF DELIMITER
C
 380    CONTINUE
          IF (ITYP(I) .EQ. 1) lenth(I) = 6
          IF (ITYP(I) .EQ. 2) lenth(I) = 10
          IF (ITYP(I) .EQ. 3  .AND.  ICOM(I) .EQ. 1)
     +      lenth(I) = JCOM(I)
          IF (ITYP(I) .EQ. 3  .AND.  ICOM(I) .EQ. 0) CKLEN = .TRUE.
 400  CONTINUE
C
C  READ MAP HEADER AND EXTRACT SOME PERTINENT INFO.
C
      NREC = 1
      CALL RDBLK(ICHAN1,NREC,IBUFF,1,IER)
      IF (IER .NE. 0) GO TO 85040
      NEXT=IBUFF(64)+IBUFF(65)+2
      NFEATS=IBUFF(63)
      NSUBS=IBUFF(67)
C
C  IF DELIMITERS USED FOR CHARS FIND MAX OUTPUT LEN FOR THE VARIABLE
C
       IF (.NOT. CKLEN) GO TO 1000
       NREC = 3
C
       DO 800 I=1,NSUBS
         CALL RDBLK(ICHAN1,NREC,IBUFF,1,IER)
         IF (IER .NE. 0) GO TO 85040
C
         CALL UNPACK(IBUFF,ISUBLN)
         NREC = NREC + 1
C
         K=0
         DO 780 J=1,NVARS
           MC=0
           IF (ICOM(J) .EQ. 1) GO TO 740
C
C  DELIMITER
 720       CONTINUE
             K=K+1
             IF(K.GT.ISUBLN  .AND. J .EQ. NVARS) GO TO 760
             IF(K .GT. ISUBLN) GO TO 800
             IF (IBUFF(K) .EQ. JCOM(J)) GO TO 760
             MC = MC + 1
             GO TO 720
C
C  COUNT
 740       CONTINUE
             K = K + JCOM(J)
             GO TO 780
C
C  RESET LENGTH IF CHARACTER AND DELIMITED
C
 760       CONTINUE
           IF (ITYP(J) .NE. 3) GO TO 780
           IF (MC .GT. LENth(J)) lenth(J) = MC
 780     CONTINUE
 800   CONTINUE

C
C  CALCULATE RECORD LENGTH
C
 1000  CONTINUE
       ITLEN=0
       DO 1020 I=1,NVARS
         ITLEN = ITLEN + lenth(I)
 1020  CONTINUE
C
C  ENTER MAIN LOOP TO PROCESS ALL SUBJECTS OVER ALL FEATURES
C  TO BUILD AN ASCII ATTRIBUTES INPUT FILE
C
      DO 2000 I=1,NFEATS
C
C  READ A RECORD OF DATA
C
        NREC = NEXT
        CALL RDBLK(ICHAN1,NREC,IBUFF,1,IER)
        IF (IER .NE. 0) GO TO 85040
C
C  GET POINTER TO NEXT DATA RECORD
C
        NEXT=IBUFF(1)
        ITEM=IBUFF(5)
C
C  GET SUBJECT POINTER
C
        NREC=IBUFF(24)
C
C  READ SUBJECT RECORD
C
        CALL RDBLK(ICHAN1,NREC,IBUFF,1,IER)
        IF (IER .NE. 0) GO TO 85040
C
C  UNPACK SUBJECT INTO SINGLE CHARS,SET INPUT & OUTPUT COUNTERS, PREPARE OUTPUT
C
        CALL UNPACK(IBUFF,ISUBLN)
        KPOINT = 1
        DO 1040 K=1,72
          OUTBUF(K) = IBLNK
 1040   CONTINUE
        do 1041 k=1,72
1041     bufout(k)=32
        K=0
C
C  PARSE SUBJECT ACCORDING TO USER RULES
C
        DO 1800 J=1,NVARS
C
C  SET & CLEAR WORK ARRAY,BRANCH FOR SCAN TYPE-CHAR,KOUNT
C
          MC=0
          CALL CLEAR(ICARD,ISUBLN)
          GO TO(1100,1200),ICOM(J) + 1

C
C  SCAN FOR DELIMETER
C
 1100     CONTINUE
            K=K+1
            IF(K.GT.ISUBLN.AND.J.EQ.NVARS) GO TO 1400
            IF(K.GT.ISUBLN) WRITE(NPRNT,91100) J,JCOM(J),ITEM
91100       FORMAT(' DELIMITER',I3,' CHAR ',A2,
     + ' NOT FOUND IN FEATURE',I5)
            IF(K.GT.ISUBLN) GO TO 1820
C
            IF(IBUFF(K).EQ.JCOM(J)) GO TO 1400
            MC=MC+1
            ICARD(MC)=IBUFF(K)
          GO TO 1100
C
C  CHARACTER COUNT
C
 1200      CONTINUE
             DO 1220 L=1,JCOM(J)
               K=K+1
               IF(K.GT.ISUBLN.AND.J.EQ.NVARS) GO TO 1400
               IF(K.GT.ISUBLN) GO TO 1400
               MC=MC+1
               ICARD(MC)=IBUFF(K)
1220        CONTINUE
C
C  WE HAVE A TOKEN.  LETS PREPARE IT AND OUTPUT IT.
C
 1400      CONTINUE
C
C  INTEGER
           L=0
           dumb='          '
           IF(ITYP(J).EQ.1)then
              CALL FNUM(ICARD,80,VAL,L)
              IVAL = VAL
              write(dumb,91500) IVAL
91500      FORMAT(I6)
              do 7 l=1,6
    7         bufout((kpoint-1)+l)=ichar(dumb(l:l))
           endif
C
C  REAL
           IF(ITYP(J).EQ.2)then
              CALL FNUM(ICARD,80,VAL,L)
              write(dumb,91600) VAL
91600      FORMAT(F10.3)
              do 4 l=1,10
   4          bufout((kpoint-1)+l)=ichar(dumb(l:l))
           endif
C
C  CHARACTER
           IF(ITYP(J).EQ.3) then
           do 6 l=1,mc
    6      bufout((kpoint-1)+l)=icard(l)
           endif
C
           KPOINT = KPOINT + lenth(J)
C       write(*,*)' LOOP,J,K,KPOINT',I,J,K,KPOINT,VAL
C       WRITE(NPRNT,92000) (bufout(L),L=1,KPOINT)
 1800   CONTINUE
C
C  OUTPUT BUFFER FOR THIS FEATURE
C
 1820 CONTINUE
      WRITE(ICHAN2,92000) (bufout(L),L=1,ITLEN)
92000 FORMAT(72a1)
C
 2000 CONTINUE
C
C  CLOSE UP MAP AND ATTRIBUTE INPUT FILES
C
       CLOSE(ICHAN2,iostat=IER)
       CLOSE(ICHAN1,iostat=IER)
C
C  CREATE AND OPEN ATTRIBUTE DESCRIPTION FILE
C
       CALL GTNAM(-1,12,MAP,NAME,IER)
       CALL OPENF(ICHAN2,NAME,0,0,0,IER)
       IF (IER .NE. 0) GO TO 85020
C
C  TELL THEM THE RECORD LENGTH OF THE ATT INPUT FILE
C
C****  OLD WAY
C
C      WRITE(NPRNT,93000) ITLEN * 2,(NAME(I),I=1,15)
C
C****  NEW WAY
C
       WRITE(NPRNT,93000) ITLEN,(NAME(I),I=1,15)
C
93000  FORMAT(/,' THE RECORD LENGTH OF THE ATTRIBUTE INPUT FILE IS',I5,/,
     +          ' THE NAME OF THE BATCH ADD DEFINITION FILE IS ',15A2)

C
C  WRITE THE DESCRIPTION FILE FOR BATCH ATTRIBUTE ADD
C
       LAST = 0
       LENGTH = 0
       L = 1
C
       DO 3400 I=1,NVARS
         WRITE(ICHAN2,93200) (KEY(J,I),J=1,5)
93200    FORMAT(5A2)
         WRITE(ICHAN2,93220) (IDESC(J,I),J=1,30)
93220    FORMAT(30A2)
         WRITE(ICHAN2,93240) ITYP(I)
93240    FORMAT(I1)
C
         LAST = LAST + LENGTH
C
         LENGTH = LENth(I)
C
         write(lasta,'(i3)')last
         write(lengtha,'(i2)')length
         ll=0
         ik=0
         do 81 il=1,3
81       if(lasta(il:il).eq.' ')ll=il
         do 82 il=1,2
82       if(lengtha(il:il).eq.' ')ik=il
         ll=ll+1
         ik=ik+1
         IF (ITYP(I) .EQ. 3) then
           WRITE(ICHAN2,93260) LENGTHa(ik:)
93260      FORMAT(A)
         endif
C
         IF (ITYP(I) .EQ. 1) then
          if(last.gt.0)then
             Formt='('//lasta(ll:)//'X,I'//lengtha(ik:)//')'
          else
             Formt='(I'//lengtha(ik:)//')'
          endif
             WRITE(ICHAN2,93282) Formt
93282    FORMAT(A)
         elseIF (ITYP(I) .EQ. 2) then
            if(last.gt.0)then
             Formt='('//lasta(ll:)//'X,F'//lengtha(ik:)//'.3)'
            else
             Formt='(F'//lengtha(ik:)//'.3)'
            endif
             WRITE(ICHAN2,93284) Formt
93284    FORMAT(A)
         elseIF (ITYP(I) .EQ. 3) then
          if(last.gt.0)then
             Formt='('//lasta(ll:)//'X,'//lengtha(ik:)//'A1)'
          else
             Formt='('//lengtha(ik:)//'A1)'
          endif
             WRITE(ICHAN2,93286) Formt
93286    FORMAT(A)
         endif
         IF (I .EQ. NVARS) L = 2
         WRITE(ICHAN2,93288) L
93288    FORMAT(I1)
 3400  CONTINUE
C
       CLOSE(ICHAN2,iostat=IER)
C
       GO TO 90000
C
C  ERROR PROCESSING
C
80000  WRITE(NPRNT,98000) MXATT
98000  FORMAT('*SUB2AT*  The number of attributes must be between 1 and'
     +   ,I3)
       CLOSE(ICHAN2,status='DELETE',iostat=IER)
       GO TO 90000
C
80020  WRITE(NPRNT,98002)
98002  FORMAT(' *SUB2AT* NUMBER OF CHARACTERS MUST BE BETWEEN 1 AND 30')
       GO TO 90000
C
85000  CONTINUE
       WRITE (NPRNT,98500) (NAME(I),I=1,15),IER
98500  FORMAT(' *SUB2AT*  ERROR CREATING FILE ',15A2,'  ERROR',I5)
       GO TO 90000
C
85020  CONTINUE
       WRITE (NPRNT,98502) (NAME(I),I=1,15),IER
98502  FORMAT(' *SUB2AT*  ERROR OPENING FILE ',15A2,'  ERROR',I5)
       GO TO 90000
C
85040  CONTINUE
       WRITE (NPRNT,98504) NREC,IER
98504  FORMAT(' *SUB2AT*  ERROR READING RECORD',I5,'  ERROR',I5)
       GO TO 90000
C
C  JUMP OUT
C
90000 continue
      END
