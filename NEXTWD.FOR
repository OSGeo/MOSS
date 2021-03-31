      SUBROUTINE nextwd(icard,icom,dicom,k)
      COMMON /io    / nprnt , ioin
      INTEGER dicom
      DIMENSION icard(1) , icom(dicom)
      LOGICAL mcomp
C
C ********************************************************************
C
C          S U B R O U T I N E    N E X T W D
C
C  FUNCTION: GET A CHARACTER STRING FROM A CHARACTER STRING?
C
C  PARAMETERS:
C             ICARD     IN - STRING ARRAY.
C             ICOM      OUT- RETURNED ARRAY.
C             K         IN - FIRST CHARACTER POSITION IN ICARD ARRAY.
C             DICOM     IN - DIMENSION OF ICOM
C *****************************************************************
C
      kstart = k + 1
C
C
C  ENTER STRING DEFINITION LOOP.  A SUB-STRING WITHIN A STRING MAY
C  BE DELIMITTED BY BLANKS, PARENS, COMMAS, OR SEMI-COLANS
C
      nc = 0
 100  CONTINUE
      k = k + 1
C
C  END OF SUB-STRING ?
C
      IF ( .NOT.((icard(k).NE.0) .AND. (.NOT.mcomp(icard(k),' ',1,ier)))
     &      ) THEN
         IF ( kstart.NE.k ) RETURN
         kstart = kstart + 1
         GOTO 100
      ELSEIF ( .NOT.(mcomp(icard(k),'(',1,ier)) ) THEN
         IF ( .NOT.(mcomp(icard(k),')',1,ier)) ) THEN
            IF ( mcomp(icard(k),',',1,ier) ) RETURN
            IF ( mcomp(icard(k),';',1,ier) ) RETURN
C
C  ADD A NEW CHARACTER TO THE ARRAY ICOM
C
            nc = nc + 1
            IF ( nc.GT.dicom ) THEN
C
C  SUB-STRING TOO LONG.  TELL THE USER AND GET NEW STRING.
C
               nlast = k
               nfirst = k - nc + 1
               WRITE (nprnt,99001) (icard(i),i=nfirst,nlast)
99001          FORMAT (' WORD TOO LONG ',10A2,' PLEASE RE-ENTER ')
               CALL readin(icard,80,k,lenicd)
               kstart = k + 1
               nc = 0
            ELSE
               icom(nc) = icard(k)
            ENDIF
            GOTO 100
         ENDIF
      ENDIF
C
      icom(1) = icard(k)
      RETURN
      END
