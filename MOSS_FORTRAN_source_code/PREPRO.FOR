      SUBROUTINE prepro(ichan,istar1,istop1,istar2,istop2,atol1,atol2,
     &                  vwind,ier)
$Include:'io.inc'
$Include:'work.inc'
      REAL vwind(4) , mbr1(4) , mbr2(4)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C          S U B R O U T I N E    P R E P R O
C
C  FUNCTION:  THIS ROUTINE IS USED TO DETERMINE CANDIATE FEATURES
C             IN ACTIVE MAP 1 FOR COMPARISON TO FEATURES IN
C             ACTIVE MAP 2.
C
C  INPUT PARAMETERS:
C
C             ICHAN  =  DATA CHANNEL TO PREPRO OUTPUT FILE
C             ISTAR1 =  STARTING LOCATION FAST FILE ACTIVE MAP 1
C             ISTOP1 =  ENDING LOCATION FAST FILE ACTIVE MAP 1
C             ISTAR2 =  STARTING LOCATION FAST FILE ACTIVE MAP 2
C             ISTOP2 =  ENDING LOCATION FAST FILE ACTIVE MAP2
C             ATOL1  =  TOLERANCE FOR ACTIVE MAP1 (IF WANT TO EXPAND
C                       MBRS)
C             ATOL2  =  TOLERANCE FOR ACTIVE MAP 2
C             VWIND  =  CURRENT VIRTUAL WINDOW ON DATABASE
C             IER    =  ERROR RETURN (1=NORMAL)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C  SET VARIABLES.  IER = ERROR FLAG, NTOTAL =
C  TOTAL VALID MAJOR POLYGONS (ARCS), NREC= RECORD POINTER
C  FOR PREPRO.DT, INDEX = POINTER FOR MINORS ASSOCIATED WITH
C  MAJORS
C
      ier = 0
      ntotal = 0
      nrec = 1
C
      DO ii = istar1 , istop1
C
         index = 2
         CALL clear(ibuff,128)
C
         CALL getfast(ii,mbr1,idd,isubp,iex1,iex2,ier)
         IF ( ier.NE.0 ) GOTO 100
C
         ibuff(1) = idd
         lastrec = nrec + 1
         kount = 0
C
C  ADD TOLERANCE TO MBR MAJOR FEATURE
C
         mbr1(1) = mbr1(1) - atol1
         mbr1(2) = mbr1(2) + atol1
         mbr1(3) = mbr1(3) - atol1
         mbr1(4) = mbr1(4) + atol1
C
C
C  CHECK MBR OF MAJOR AGAINST VIRTUAL WINDOW ON DATABASE
C
         IF ( mbr1(1).LE.vwind(2) ) THEN
            IF ( mbr1(2).GE.vwind(1) ) THEN
               IF ( mbr1(3).LE.vwind(4) ) THEN
                  IF ( mbr1(4).GE.vwind(3) ) THEN
C
C  UPDATE MAJOR COUNT
C
                     ntotal = ntotal + 1
C
C  MBR MAJOR FEATURE IN VIRTUAL WINDOW.  NOW SET LOOP ON MINOR FEATURE
C
                     DO jj = istar2 , istop2
C
C  GET INFO FOR MINOR POLY
C
                        CALL getfast(jj,mbr2,kdd,isubp,iex1,iex2,ier)
                        IF ( ier.NE.0 ) GOTO 100
C
C  ADD TOLERANCE TO MINOR FEATURE MBR
C
                        mbr2(1) = mbr2(1) - atol2
                        mbr2(2) = mbr2(2) + atol2
                        mbr2(3) = mbr2(3) - atol2
                        mbr2(4) = mbr2(4) + atol2
C
C  CHECK MBR OF MINOR AGAINST MBR OF MAJOR FOR ANY POSSIBLE OVERLAP
C
                        IF ( mbr2(1).LE.mbr1(2) ) THEN
                           IF ( mbr2(2).GE.mbr1(1) ) THEN
                              IF ( mbr2(3).LE.mbr1(4) ) THEN
                                 IF ( mbr2(4).GE.mbr1(3) ) THEN
C
C  MINOR FEATURE IS CANDIDATE.  UPDATE PREPROCESS FILE
C
                                    kount = kount + 1
                                    index = index + 1
                                    IF ( index.GT.128 ) THEN
C
                                       index = 1
                                       nrec = nrec + 1
                                       CALL wrblk(ichan,nrec,ibuff,1,
     &                                       ier)
                                       CALL clear(ibuff,128)
                                    ENDIF
C
C
                                    ibuff(index) = kdd
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
C
                     ENDDO
C
C  ERITE OUT CURRENT BUFFER
C
                     nrec = nrec + 1
                     CALL wrblk(ichan,nrec,ibuff,1,ier)
C
C  READ IN HEADER RECORD FOR THIS MAJOR AND ADD INFO
C
                     CALL rdblk(ichan,lastrec,ibuff,1,ier)
                     ibuff(2) = kount
                     CALL wrblk(ichan,lastrec,ibuff,1,ier)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
C
C  CREATE HEADER AND WRITE TO FILE
C
      CALL clear(ibuff,128)
      ibuff(1) = nrec
      ibuff(2) = ntotal
      CALL wrblk(ichan,1,ibuff,1,ier)
C
      ier = 0
      RETURN
C
C  ERRORS
C
 100  CONTINUE
      WRITE (nprnt,99001)
99001 FORMAT (' *PREPRO* ERROR ON READING IN GETFAST ')
      ier = 3
      RETURN
C
      END
