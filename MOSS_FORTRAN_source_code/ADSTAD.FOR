      SUBROUTINE adstad(ier,idsn,nents,index,isel,itype)
C
C  ****************************************************************
C
C     S U B R O U T I N E    A D S T A D
C
C  FUNCTION - ADD A NEW ACTIVE DATA SET ENTRY TO THE ACTIVE DATA
C             SET TABLE
C
C  INPUT PARAMETERS:
C
C    IER  = ERROR RETURN = 2 IF TABLE IS FULL, 3 IF ALMOST FULL
C
C    IDSN = ACTIVE DATA SET NAME
C    NENTS= NUMBER OF ITEMS IN THIS ACTIVE DATA SET
C    INDEX= STARTING LOCATION IN POINTER TABLE FOR THIS DATA SET
C    ISEL = 15 CHARACTER DESCRIPTION OF ACTIVE DATA SET
C    ITYPE= SELECTION TYPE 1-ALL,2-SUB,3-ITEM,4-ATT,6-ELEV,7-FROM
C
C *****************************************************************
$Include:'DBCHAN.inc'
$Include:'IO.inc'
$Include:'WHMAP.inc'
      DIMENSION nsel(31) , iwrk1(30)
      DIMENSION idsn(10) , isel(30)
C
C  FIRST CHECK FOR TABLE FULL
C
      IF ( nstrct+7.GT.300 ) THEN
C
C  TABLE FULL. GIVE MESSAGE AND SET ERROR FLAG
C
         WRITE (nprnt,99001)
99001    FORMAT (' DANGER. ACTIVE TABLE FULL, FREE SOMETHING')
         ier = 2
         CALL whttrm
         RETURN
      ELSE
C
C  NOW ADD THE ENTRIES
C
         istrct(nstrct+1) = index
         istrct(nstrct+2) = nents
         istrct(nstrct+3) = idsn(1)
         istrct(nstrct+4) = idsn(2)
         istrct(nstrct+5) = idsn(3)
         istrct(nstrct+6) = idsn(4)
         istrct(nstrct+7) = idsn(5)
         nstrct = nstrct + 7
C
C OPEN DESCRIBE.DT AND  SAVE DESCRIPTION
C
         CALL openf(icdcdt,idesdt,1,62,2,ier)
         IF ( ier.NE.0 ) THEN
            WRITE (nprnt,99002) ier
99002       FORMAT (' *ADSTAD* COULD NOT OPEN DESCRIBE.DT  ERROR',I5)
            lines = lines + 2
            GOTO 200
         ELSE
C
C  REMOVE THE NULLS ETC.
C
            IF ( itype.NE.3 ) THEN
               DO l = 1 , 15
                  iwrk1(l) = isel(l)
               ENDDO
               CALL unpack(iwrk1,30)
               DO i = 1 , 30
                  IF ( iwrk1(i).LT.32 ) iwrk1(i) = 32
               ENDDO
               CALL packc(iwrk1,nsel,30)
            ELSE
               DO l = 1 , 30
                  nsel(l) = isel(l)
               ENDDO
            ENDIF
            nsel(31) = itype
            np = (nstrct/7)
            WRITE (icdcdt,REC=np,ERR=100,IOSTAT=ier) nsel
            CLOSE (icdcdt,IOSTAT=ier)
C
C  IS TABLE ALMOST FULL ?
C
            IF ( nstrct+21.GE.300 ) THEN
C
C  TABLE ALMOST FULL.  GIVE WARNING
C
               WRITE (nprnt,99003)
99003          FORMAT (' DANGER.  ACTIVE TABLE ALMOST FULL')
               CALL whttrm
               RETURN
            ELSE
C
               RETURN
            ENDIF
         ENDIF
      ENDIF
C
C  ERRORS
C
 100  CONTINUE
      WRITE (nprnt,99004) ier
99004 FORMAT (' *ADSTAD* ERROR UPDATING DESCRIBE.DT  ERROR',I5)
      lines = lines + 2
      CLOSE (8,IOSTAT=ier)
C
 200  CONTINUE
      CALL whttrm
      RETURN
C
      END
