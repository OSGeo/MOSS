      SUBROUTINE ATTDSC(IER)
C***************************************************************************
C
C   NAME:         PROGRAM ATTDESCRIBE
C
C   FUNCTION:     INTERACTIVELY QUERIES A USER ABOUT THE MULTIPLE ATTRIBUTE
C                 FILE AND SETS UP THE MA DEFINITION FILE.  THIS DEFINITION
C                 FILE CAN THEN BE USED WITH MOSS'S ATTRIBUTE PROGRAM AS A
C                 BATCH DEFINITION FILE.
C
C   FILES:        USER-DEFINED MULTIPLE ATTRIBUTE DEFINITION FILE.
C
C   ROUTINES
C
C   SYSTEM
C   DEPENDENCE:   16 BIT  2 BYTE WORD
C
C***************************************************************************
C
       INTEGER ATTKEY(10),DESCF(25),DESCR(60),FIELD,NUMCHR,MORE,ALEN,ADIG,
     .         MANY,DIGITR
       LOGICAL SECOND
$Include:'strng.inc'
       COMMON/IO/NPRNT,IOIN
       character lasta*3,lengtha*2,digtr*2
       character formt*72
C
       DATA IYES /1HY/, ISP /1H /
C
C
      ICHAN = 51
      IER=0
      CALL DECIP(ICARD,DESCF,KK)
      IF(DESCF(1).EQ.0)THEN
C
    5      WRITE (NPRNT,6000)
 6000  FORMAT (////,T10,'*** ATTRIBUTE DESCRIPTION PROGRAM ***',////,
     .       ' WHAT DO YOU WISH TO CALL DEFINITION FILE'\)
           IEND=25
           CALL READIN(DESCF,25,KK,IEND)
C
       ENDIF
    8  CONTINUE
       CALL OPENF(ICHAN,DESCF,0,0,0,IER)
       IF(IER.NE.0)GO TO 80020
C
   10  WRITE (NPRNT,6010)
 6010  FORMAT (/,' HOW MANY ATTRIBUTE KEYS DO YOU HAVE? (1-200) '\)
       IEND=80
       CALL READIN(ICARD,lenicd,KK,IEND)
       CALL INUM(ICARD,MANY,kk)
         IF (MANY .LT. 1 .OR. MANY .GT. 200) GO TO 10
C
   16  LAST = 0
       LENGTH = 0
       MORE = 1
       DO 1000 I = 1,MANY
C
          SECOND = .FALSE.
          IF (I .EQ. MANY) MORE = 2
   20     WRITE (NPRNT,6020) I
 6020     FORMAT (///,' PROVIDE A TEN CHARACTER DESCRIPTION FOR "KEY"',
     +     I6,\)
          IEND=10
          CALL READIN(ATTKEY,10,KK,IEND)
          IF ( SECOND ) GO TO 70
C
   30     WRITE (NPRNT,6030)
 6030     FORMAT (/,' PROVIDE A 60 CHARACTER DESCRIPTION OF THIS "KEY"',
     .    /,60(1H-))
          IEND=60
          CALL READIN(DESCR,60,KK,IEND)
          IF ( SECOND ) GO TO 70
C
   40     WRITE (NPRNT,6040)
 6040     FORMAT (/,' THE FIELD TYPE OF THIS "KEY" IS',/,
     .              '  1 - INTEGER',/,
     .              '  2 - FLOATING POINT',/,
     .              '  3 - CHARACTER :'\)
          IEND=80
          CALL READIN(ICARD,lenicd,IC,IEND)
          CALL INUM(ICARD,FIELD,ic)
            IF (IC .LT. 1 .OR. FIELD .LT. 1 .OR. FIELD .GT. 3) GO TO 40
C
          IF (.NOT.SECOND) LAST = LAST + LENGTH
   50     WRITE (NPRNT,6050)
 6050     FORMAT (/,' WHAT IS THE FIELD LENGTH OF THIS "KEY" '\)
          IEND=80
          CALL READIN(ICARD,lenicd,IC,IEND)
          CALL INUM(ICARD,LENGth,ic)
            IF (IC.LT.1 .OR. LENGTH.LT.1 .OR. LENGTH.GT.72) GO TO 50
C
          IF (FIELD .NE. 2) GO TO 70
   60     WRITE (NPRNT,6060)
 6060 FORMAT (/,' HOW MANY DIGITS TO THE RIGHT OF THE DECIMAL POINT '\)
          IEND=80
          CALL READIN(ICARD,lenicd,IC,IEND)
          CALL INUM(ICARD,DIGITR,ic)
            IF (IC.LT.1 .OR. DIGITR.LT.0 .OR. DIGITR.GE.LENGTH) GO TO 60
C
   70     CONTINUE
C
C  VERIFY INPUT
C
        WRITE(NPRNT,6061) I,(ATTKEY(l),l=1,10), (DESCR(l),l=1,60)
 6061   FORMAT(' ATTRIBUTE:    ',i3,3X,' KEY:      ',10a2,/
     +         ' DESCRIPTION:  ',60a1)
        IF (FIELD .EQ. 1) WRITE(NPRNT,60611) LENGTH
60611  FORMAT(' TYPE:       INTEGER',3X,' LENGTH:   ',I5)
        IF (FIELD .EQ. 2) WRITE(NPRNT,60612) LENGTH, DIGITR
60612  FORMAT(' TYPE:          REAL',3X,' LENGTH:   ',I5,'.',I2)
        IF (FIELD .EQ. 3) WRITE(NPRNT,60613) LENGTH
60613  FORMAT(' TYPE:     CHARACTER',3X,' LENGTH:   ',I5)
        write(nprnt,'(//)')
        if(more.eq.1)WRITE(NPRNT,6063) (I+1)
        write(nprnt,6066)
 6063   FORMAT(' EDIT OPTIONS:  0 - PROCEED TO ATTRIBUTE',I4)
 6066   format('                1 - EDIT ATTRIBUTE KEY',/
     +         '                2 - EDIT ATTRIBUTE DESCRIPTION',/
     +         '                3 - EDIT FIELD TYPE AND LENGTH',/
     +         '                7 - ABORT THIS RUN '\)
        if(more.ne.1)write(*,6067)
6067    format(' or <CR> - FINISH UP'\)
        IEND=80
        CALL READIN(ICARD,lenicd,IC,IEND)
        CALL INUM(ICARD,MGOING,ic)
C
        MGOING = MGOING + 1
        SECOND = .TRUE.
        GO TO ( 80, 20, 30, 40 ), MGOING
        IF (MGOING .EQ. 8) GO TO 1100
        WRITE(NPRNT,6064)
 6064   FORMAT(' INVALID RESPONSE - TRY AGAIN . . .')
        GO TO 70
C
C  OUTPUT SECTION
C
80        write(lasta,'(i3)')last
          write(lengtha,'(i2)')length
          write(digtr,'(i2)')digitr 
        WRITE (ICHAN,'(10A1)') (ATTKEY(L),L=1,10)
        WRITE (ICHAN,'(60A1)') (DESCR(L),L=1,60)
        WRITE (ICHAN,6041) FIELD
 6041   FORMAT (I1)
          do 81 il=1,2
           if(field.eq.2.and.digtr(il:il).eq.' ')ik=il
81         if(lengtha(IL:IL).eq.' ')li=il
         li=li+1
         ik=ik+1 
        IF (FIELD .EQ. 3) then
          WRITE (ICHAN,6071) LENGTHa(li:)
 6071   FORMAT (a)
        endif
C
          do 82 il=1,3
82        if(lasta(il:il).eq.' ')ll=il
          ll=ll+1
          GO TO (100,200,300) FIELD
C
  100     if(last.gt.0)then
            Formt='('//lasta(ll:)//'X,I'//lengtha(li:)//')'
          else
            Formt='(I'//lengtha(li:)//')'
          endif
          WRITE (ICHAN,101) Formt
  101     FORMAT (A)
          GO TO 400
C
  200 if(last.gt.0)then
      Formt='('//lasta(ll:)//'X,F'//lengtha(li:)//'.'//digtr(ik:)//')'
      else
      Formt='(F'//lengtha(li:)//'.'//digtr(ik:)//')'
      endif
          WRITE (ICHAN,201) Formt
  201     FORMAT (A)
          GO TO 400
C
  300     if(last.gt.0)then
           Formt='('//lasta(ll:)//'X,'//lengtha(li:)//'A1)'
          else
           Formt='('//lengtha(li:)//'A1)'
          endif
          WRITE (ICHAN,301) Formt
  301     FORMAT (A)
          GO TO 400
C
C
  400     WRITE (ICHAN,6041) MORE
C
 1000  CONTINUE
C
C  NORMAL RETURN
C
 1100  IF(MGOING.LT.8)CLOSE (ICHAN,IOSTAT=IER)
       IF (MGOING .EQ. 8) CLOSE(ICHAN,STATUS='DELETE',IOSTAT=IER)
       IF (IER .NE. 0) GO TO 80030
       GO TO 90000
C
C  ERROR TRAPS AND MESSAGES
C
80020  WRITE (NPRNT,90010) (DESCF(L),L=1,25),IER
90010  FORMAT ('UNABLE TO CREATE FILE ',25A1,2X,I7)
       GO TO 90000
C
80030  CONTINUE
       WRITE(NPRNT,90030) (DESCF(L),L=1,25),IER
90030  FORMAT(' ATTDESCRIBE ERROR DELETING FILE ',25A1,2X,I7)
C
90000  RETURN
       END
