      SUBROUTINE decip(icard,icom,k)
      COMMON /io    / nprnt , ioin
      DIMENSION icard(1) , icom(1)
      LOGICAL mcomp
C
C ********************************************************************
C
C          S U B R O U T I N E    D E C I P
C
C  FUNCTION: GET A CHARACTER STRING FROM A CHARACTER STRING?
C
C  PARAMETERS:
C             ICARD     IN - STRING ARRAY.
C             ICOM      OUT- RETURNED ARRAY.
C             K         IN - FIRST CHARACTER POSITION IN ICARD ARRAY.
C ROUTINES
C CALLED:     READIN.
C
C *****************************************************************
C
 100  CONTINUE
      DO i = 1 , 10
         icom(i) = 0
      ENDDO
C
C  ENTER STRING DEFINITION LOOP.  A SUB-STRING WITHIN A STRING MAY
C  BE DELIMITTED BY BLANKS, PARENS, COMMAS, OR SEMI-COLANS
C
      nc = 0
      IF ( k.EQ.0 ) k = 1
      IF ( k.NE.1 ) THEN
         IF ( icard(k).EQ.0 ) k = k + 1
         IF ( mcomp(icard(k),' ',1,ier) ) k = k + 1
         IF ( mcomp(icard(k),'(',1,ier) ) GOTO 300
         IF ( mcomp(icard(k),')',1,ier) ) GOTO 300
         IF ( mcomp(icard(k),',',1,ier) ) k = k + 1
         IF ( mcomp(icard(k),';',1,ier) ) k = k + 1
      ENDIF
 
 
C
C  END OF SUB-STRING ?
C
 200  CONTINUE
      IF ( icard(k).EQ.0 ) RETURN
      IF ( mcomp(icard(k),' ',1,ier) ) RETURN
      IF ( .NOT.(mcomp(icard(k),'(',1,ier)) ) THEN
         IF ( .NOT.(mcomp(icard(k),')',1,ier)) ) THEN
            IF ( mcomp(icard(k),',',1,ier) ) RETURN
            IF ( mcomp(icard(k),';',1,ier) ) RETURN
C
C  ADD A NEW CHARACTER TO THE ARRAY ICOM
C
            nc = nc + 1
            IF ( nc.EQ.13 ) THEN
C
C  SUB-STRING TOO LONG.  TELL THE USER AND GET NEW STRING.
C
               nlast = k
               nfirst = k - nc + 1
               WRITE (nprnt,99001) (icard(i),i=nfirst,nlast)
99001          FORMAT (' WORD TOO LONG ',20A1,' PLEASE RE-ENTER ')
               CALL readin(icard,80,k,istop)
               GOTO 100
            ELSE
               icom(nc) = icard(k)
               k = k + 1
               GOTO 200
            ENDIF
         ENDIF
      ENDIF
C
 300  CONTINUE
      icom(1) = icard(k)
      RETURN
      END
