      SUBROUTINE upsubj(ichan,irec,ibuff,ier)
C**********************************************************************
C
C    S U B R O U T I N E  UPSUBJ
C
C    FUNCTION:  Routine queries user to update subject.
C
C    PARAMETERS:  ICHAN  - IN - Channel map is openned on
C                 IREC   - IN - Record number to update
C                 IBUFF  - IN - Buffer of record associated with IREC
C                 IER    - OUT- ERROR flag,  IER = 0 OK, else error.
C
C    CALLED FROM: SUBEDIT
C
C**********************************************************************
$Include:'IO.inc'
      INTEGER ichan , irec , ibuff(128) , ier
      INTEGER subnew(60) , subpac(60) , icard(10)
C
C     IER = 0
C
      WRITE (nprnt,99001)
99001 FORMAT (' ENTER REPLACEMENT STRING',/,
     &        '------------------------------')
      CALL readin(subnew,30,istart,iend)
C      WRITE(NPRNT,2222)(SUBNEW(I),I=1,30)
C2222  FORMAT(' AFTER READIN ',30(A2,' '))
C      CALL UNPACK(SUBNEW,30)
      CALL packc(subnew,subpac,30)
      WRITE (nprnt,99002) (subpac(j),j=1,15)
99002 FORMAT (5X,15A2)
C
      WRITE (nprnt,99003)
99003 FORMAT (' 1 = UPDATE SUBJECT STRING ',/,' 2 = LEAVE UNCHANGED - ')
      CALL readin(icard,10,istart,istop)
      CALL inum(icard,ians,istart)
      IF ( ians.EQ.1 ) THEN
         DO j = 1 , 15
            ibuff(j) = subpac(j)
         ENDDO
         CALL wrblk(ichan,irec,ibuff,1,ier)
         IF ( ier.NE.0 ) THEN
C
C  ERROR RETURNS
C
            WRITE (nprnt,99004) ier , irec
99004       FORMAT (' *UPSUBJ* ERROR FROM WRBLK, REC ',I5,' ERROR ',I5)
         ENDIF
C
      ENDIF
C
      RETURN
      END
