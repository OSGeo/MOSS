      SUBROUTINE attopn(ichan1,ichan2,map1,map2,ntotal,natt,ier)
C *********************************************************************
C
C  SUBROUTINE ATTOPN
C
C  Purpose:  Open input and output attribute files (.AT).  Copy
C            attribute headers from input map to new map.
C
C
C  Parameters    Use    Type   Dimension       Description
C     ----       ---    ----   ---------   ------------------
C
C     ICHAN1     I      INT                 CHANNEL FOR INPUT MULTIPLE
C                                           ATTRIBUTE FILE
C
C     ICHAN2     I      INT                 CHANNEL FOR NEW MULTIPLE
C                                           ATTRIBUTE FILE
C
C     MAP1       I      INT       10        MAP NAME OF INPUT MULTIPLE
C                                           ATTRIBUTE FILE
C
C     MAP2       I      INT       10        MAP NAME OF NEW MULTIPLE
C                                           ATTRIBUTE FILE
C
C     NTOTAL     I      INT                 THE NUMBER OF ITEMS IN THE
C                                           MAP
C
C     NATT       O      INT                 THE NUMBER OF ATTRIBUTE TO
C                                           BE SAVED
C
C  Files:    Logical Unit       File name or description
C            ------------       ------------------------
C            ICHAN1             INPUT MULTIPLE ATTRIBUTES FILE
C            ICHAN2             NEW MULTIPLE ATTRIBUTES FILE
C
C  Calls:  GTNAM, OPENF, CLEAR, RDBLK, WRBLK
C
C  Called by: SAVE
C
C  Language/System: Fortran V, Data General AOS or AOS/VS
C
C  Written by: Mike Carsella (TGS/BLM)
C
C  Date: August 1985
C
C **********************************************************************
C
$Include:'IO.inc'
$Include:'STRNG.inc'
      INTEGER map1(25) , map2(25) , name(25) , iarr(128)
      INTEGER ibuff1(128) , attrec , natt , ntotal
C
      CALL clear(ibuff1,128)
C
C  OPEN INPUT ATTRIBUTE FILE
C
      CALL gtnam(0,4,map1,name,ier)
C
      CALL openf(ichan1,name,1,256,2,ier)
      IF ( ier.EQ.0 ) THEN
C
         CALL rdblk(ichan1,1,iarr,1,ier)
         IF ( ier.NE.0 ) GOTO 100
C
C  OPEN NEW ATTRIBUTE FILE
C
         CALL gtnam(0,4,map2,name,ier)
C
         CALL openf(ichan2,name,2,256,2,ier)
         IF ( ier.EQ.0 ) THEN
C
C      ASSIGN THE NUMBER OF ATTRIBUTES AND ITEMS FOR THE NEW FILE
C
            natt = iarr(1)
            ibuff1(1) = iarr(1)
            ibuff1(2) = ntotal
            CALL wrblk(ichan2,1,ibuff1,1,ier)
            IF ( ier.NE.0 ) GOTO 200
C
C      COPY ATTRIBUTE HEADERS TO THE NEW ATTRIBUTE FILE
C
            attrec = 204
            DO i = 1 , natt
               CALL rdblk(ichan1,i+1,ibuff1,1,ier)
               IF ( ier.NE.0 ) GOTO 100
               ibuff1(86) = attrec
C
C        CALCULATE NUMBER OF DATA RECORDS FOR THIS ATTRIBUTE
C
               ibuff1(87) = (ntotal-1)/(128/ibuff1(85)) + 1
C
               CALL wrblk(ichan2,i+1,ibuff1,1,ier)
               IF ( ier.NE.0 ) GOTO 200
               attrec = attrec + ibuff1(87)
C
            ENDDO
            GOTO 300
         ENDIF
      ENDIF
C
C  ERROR RETURNS
C
      WRITE (nprnt,99001) (name(i),i=1,25) , ier
99001 FORMAT (' *ATTOPN*  ERROR OPENING FILE ',25A2,'  ERROR',I5)
      GOTO 300
C
 100  CONTINUE
      WRITE (nprnt,99002) ier
99002 FORMAT (' *ATTOPN*  ERROR READING ATTRIBUTE FILE  ERROR',I5)
      CLOSE (ichan1)
      GOTO 300
C
 200  CONTINUE
      WRITE (nprnt,99003) ier
99003 FORMAT (' *ATTOPN*  ERROR WRITING ATTRIBUTE FILE  ERROR',I5)
      CLOSE (ichan2)
C
C  JUMP OUT
C
 300  CONTINUE
      RETURN
      END
