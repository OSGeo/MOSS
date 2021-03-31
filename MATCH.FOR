      SUBROUTINE match(ibuff,string,itype,nhits,ichan,ier)
      COMMON /io    / nprnt , ioin
      INTEGER string(80) , pat(30) , ibuff(128) , ink(2,30)
C
C *****************************************************************
C
C          S U B R O U T I N E    M A T C H
C
C  FUNCTION - LOOK FOR A CHARACTER STRING IN EITHER
C             THE SUBJECT OR THE CATEGORY TABLES
C
C  INPUTPARAMETERS:
C
C        IBUFF  - WORK SPACE
C        STRING - CHARACTER STRING THAT WE ARE LOOKING FOR
C        ITYPE  - 1 FOR SUBJECT, 2 FOR ALL SUBJECTS BUT ONE
C        NHITS  - NUMBER OF ITEMS HAVING THIS STRING
C        ICHAN  - DATA CHANNEL TO LOOK ON FOR DATA
C        IER    - ERROR RETURN (1=NORMAL)
C ***************************************************************
C
C  SET IER
C
      ier = 0
      idelim = 33
C
C  SET NHITS T0 ZERO
C
      nhits = 0
C
C  READ IN MAP HEADER
C
      CALL rdblk(ichan,1,ibuff,1,ier)
C
C  SUBJECT SETUP
C
      ipoint = ibuff(64) + 2
      n = ibuff(67)
      ioff = 3
C
C  WE ARE NOW READY TO CONTINE
C
C
C  NOW DETERMINE SEARCH STRING LENGTH
C
      j = 81
 100  CONTINUE
      j = j - 1
      IF ( j.NE.0 ) THEN
         IF ( string(j).EQ.32 .OR. string(j).EQ.0 .OR. string(j)
     &        .EQ.idelim ) GOTO 100
         islen = j
C
C   FIND OUT IF THEY PUT IN MULTIPLE SEARCHES...ELEIM START & END WITH DELIMITOR
C
         knt = 1
         ink(2,knt) = islen
         i = itype
         IF ( string(i).EQ.idelim ) i = i + 1
         ink(1,knt) = i
         DO j = i , islen
            IF ( string(j).EQ.idelim ) ink(2,knt) = j - 1
            IF ( string(j).EQ.idelim ) knt = knt + 1
            IF ( string(j).EQ.idelim ) ink(1,knt) = j + 1
         ENDDO
         ink(2,knt) = islen
         IF ( itype.EQ.2 ) knt = 1
C
C  NOW ENTER MAIN SEARCH LOOP
C
         DO i = 1 , n
C
C  READ RECORD
C
            CALL rdblk(ichan,ipoint,ibuff,1,ier)
            ipoint = ipoint + 1
C
C  GET INFO ABOUT THIS SUBJECT OR NOT SUBJECT
C
            ihead = ibuff(16)
            nrecs = ibuff(17)
C
C  UNPACK STRING
C
            CALL unpack(ibuff,80)
C
C  MOVE UNPACKED STRING FROM IBUFF TO PAT
C
            DO j = 1 , 30
               pat(j) = ibuff(j)
            ENDDO
C
C  DETERINE LENGTH OF PATTERN
C
            j = 31
 120        CONTINUE
            j = j - 1
            IF ( j.LE.0 ) GOTO 200
            IF ( pat(j).EQ.32 .OR. pat(j).EQ.0 ) GOTO 120
            iplen = j
C
C  GO THROUGH MULTIPLE SEARCHES
C
            DO j = 1 , knt
               islen = ink(2,j) - ink(1,j) + 1
C
C  IS SEARCH STRING LONGER THAN PATTERN ?
C
               IF ( islen.GT.iplen .AND. itype.EQ.1 ) GOTO 140
               IF ( islen.LE.iplen .OR. itype.NE.2 ) THEN
C
C  NO. LETS DO CHARACTER MATCH
C
                  CALL match2(string(ink(1,j)),pat,islen,iplen,ifind)
C
C  A MATCH FOR THIS SUBJECT OR NOT SUBJECT ?
C
                  IF ( ifind.EQ.0 .AND. itype.EQ.1 ) GOTO 140
                  IF ( ifind.EQ.1 .AND. itype.EQ.2 ) GOTO 140
               ENDIF
C
C  YES. CALL LINK UP
C
               CALL linkup(ichan,ihead,nrecs,ibuff,ioff)
C
C  INCREMENT NUMBER OF HITS AND QUIT SO FEATURE IS NOT SELECTED MORE THAN ONCE
C
               nhits = nhits + nrecs
               GOTO 150
C
C  DO NEXT SEARCH STRING
C
 140        ENDDO
C
C  DO NEXT RECORD
C
 150     ENDDO
      ENDIF
C
C  JUMP OUT
C
 200  CONTINUE
      RETURN
      END
