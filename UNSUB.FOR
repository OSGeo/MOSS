C ************************************************************
C
C             S U B R O U T I N E    U N S U B
C FUNCTION:
C             THIS ROUTINE BUILDS A TABLE OF AREAS, FREQUENCIES, LENGTHS,
C             OR PERIMETERS.
C PARAMETERS:
C       JTYPE  IN  - WHAT TO DO
C       ISTART IN  - STARTING LOCATION IN POINTER TABLE FOR THIS MAP
C                    SET.
C       ISTOP  IN  - ENDING LOCATION IN POINTER TABLE
C       NUMB   IN  - ARRAY IN WHICH TABLE INFO IS BUILT
C       JDIM   IN  - DIMENSION OF NUMB
C       POLSIZ IN  - REAL ARRAY IN WHICH ACREAGES AND LENGTHS ARE
C                    STORED.
C       NSUBS OUT  - NUMBER OF UNIQUE SUBJECTS IN TABLE (RETURNED)
C       ICHAN  IN  - DATA CHANNEL ON WHICH MAP RESIDES
C       NDIM   IN  - THE DIMENSION POLSIZ.
C
C ***********************************************************
C
      SUBROUTINE unsub(jtype,istart,istop,numb,jdim,nsubs,polsiz,ndim,
     &                 ichan)
      DIMENSION ibuff(128) , i1(2) , numb(jdim) , polsiz(ndim) , imbr(8)
      EQUIVALENCE (xt,i1)
C
C  SET NSUBS TO ZERO
C
      nsubs = 0
C
C  ZERO OUT POLSIZ AND NUMB
C
      DO i = 1 , jdim
         numb(i) = 0
      ENDDO
C
      DO i = 1 , ndim
         polsiz(i) = 0.0
      ENDDO
C
C  ENTER TABLE BUILD LOOP
C
      DO ii = istart , istop
C
C  GET POINTER TABLE INDEX
C
C        call getndx(II,INDEX,IER)
         CALL getfast(ii,imbr,index,isubj,iatt,iext,ier)
         IF ( ier.NE.0 ) THEN
C
            WRITE (*,*) ' *UNSUB*  ERROR FROM GETFAST    ERROR ' , ier
            GOTO 200
         ELSE
C
C  GET ITEM HEADER
C
            CALL rdblk(ichan,index,ibuff,1,ier)
            IF ( ier.NE.0 ) THEN
C
               WRITE (*,*) ' *UNSUB*  ERROR FROM Qrdblk    ERROR ' , ier
               GOTO 200
            ELSE
C
C  GET AREA, PERIMETER, OR LENGTH ACCORDING TO THE
C  VALUE OF JTYPE
C
               IF ( jtype.NE.3 ) THEN
                  IF ( jtype.EQ.4 ) THEN
                     i1(1) = ibuff(9)
                     i1(2) = ibuff(10)
                  ELSE
                     i1(1) = ibuff(7)
                     i1(2) = ibuff(8)
                  ENDIF
               ENDIF
C
C  GET SUBJECT TABLE LINK
C
               nrec = ibuff(24)
C
C  HAS THIS SUBJECT BEEN FOUND ALREADY ?
C (ALSO CHECK FOR FIRST ENTRY)
C
               IF ( nsubs.NE.0 ) THEN
C
                  DO i = 1 , nsubs
                     k = (i*2) - 1
                     IF ( nrec.EQ.numb(k) ) THEN
C
C  UPDATE EXISTING ENTRY
C
                        polsiz(i) = polsiz(i) + xt
                        k = (i*2) - 1
                        numb(k+1) = numb(k+1) + 1
                        GOTO 100
                     ENDIF
                  ENDDO
               ENDIF
C
C  SUBJECT NOT  IN TABLE.  BUILD ENTRY
C
               nsubs = nsubs + 1
               IF ( nsubs.GT.ndim ) GOTO 200
               polsiz(nsubs) = xt
               k = (nsubs*2) - 1
               IF ( k+1.GT.jdim ) GOTO 200
               numb(k) = nrec
               numb(k+1) = numb(k+1) + 1
            ENDIF
         ENDIF
C
 100  ENDDO
C
 200  CONTINUE
      RETURN
      END
