      SUBROUTINE templ(icom,plate,mat)
      INTEGER icom(10) , plate(10) , case
C
C ****************************************************************
C
C           S U B R O U T I N E     T E M P L
C
C  FUNCTION - SCAN A CHARACTER STRING A TRY TO MATCH IT
C             AGAINST A TEMPLATE
C
C  INPUT PARAMETERS:
C
C        ICOM   = CHARACTER STRING (MAX 10 CHARS LONG)
C        PLATE  = TEMPLATE (MAX OF TEN CHARS IN LENGTH)
C        MAT    = RETURNED 0 IF NO MATCH, 1 OTHERWISE
C
C  THE TEMPLATE CASES ARE :
C
C          A+
C          +A
C          +A+
C          A+A
C
C *******************************************************************
C
C  SET MATCH TO 0
C
      mat = 0
C
C    FIND LOCATIONS OF +(S) IN TEMPLATE STRING
C
      icount = 0
      DO i = 1 , 10
C  look for a + sign <43>
         IF ( plate(i).EQ.43 ) THEN
            icount = icount + 1
            IF ( icount.EQ.1 ) ipoint = i
            ipoint2 = i
         ENDIF
      ENDDO
C
C  CHECK FOR CASE WHERE USER DID NOT SPECIFY ANY +'S
C
      IF ( icount.GT.0 ) THEN
C
C DETERMINE PROPER CASE
C
         case = 0
         IF ( icount.EQ.2 ) case = 3
         IF ( case.NE.3 ) THEN
            IF ( ipoint.EQ.1 ) case = 2
            IF ( case.NE.2 ) THEN
               IF ( plate(ipoint+1).EQ.0 ) case = 1
               IF ( case.EQ.0 ) case = 4
            ENDIF
         ENDIF
C
C  EXECUTE PROPER CASE
C
         IF ( case.EQ.2 ) THEN
         ELSEIF ( case.EQ.3 ) THEN
C
C  CASE 3
C
            loop = ((ipoint2-1)-(ipoint+1)) + 1
            istart = ipoint + 1
            istop = 10 - loop
            IF ( istop.LE.0 ) RETURN
            DO i = 2 , istop
               IF ( icom(i).EQ.plate(istart) ) THEN
                  l = 0
                  iend = i + loop - 1
                  IF ( iend.GT.10 ) RETURN
                  kstart = i
                  DO j = kstart , iend
                     IF ( icom(j).NE.plate(istart+l) ) GOTO 20
                     l = l + 1
                  ENDDO
                  mat = 1
                  RETURN
               ENDIF
 20         ENDDO
            RETURN
         ELSEIF ( case.EQ.4 ) THEN
C
C  CASE 4
C
            loop = ipoint - 1
            DO i = 1 , loop
               IF ( icom(i).NE.plate(i) ) RETURN
            ENDDO
         ELSE
C
C  CASE 1
C
            loop = ipoint - 1
            DO i = 1 , loop
               IF ( icom(i).NE.plate(i) ) RETURN
            ENDDO
            mat = 1
            RETURN
         ENDIF
      ELSE
         mat = 1
         RETURN
      ENDIF
C
C CASE 2
C
      istart = ipoint + 1
      DO i = 1 , 10
         IF ( icom(i).EQ.0 ) GOTO 100
      ENDDO
 100  CONTINUE
      kend = i
      DO i = 2 , 10
         IF ( plate(i).EQ.0 ) GOTO 200
      ENDDO
 200  CONTINUE
      istop = i
      kstart = kend - (istop-istart)
      DO i = istart , istop
         IF ( kstart.NE.11 ) THEN
            IF ( icom(kstart).NE.plate(i) ) RETURN
            kstart = kstart + 1
         ENDIF
      ENDDO
      mat = 1
      RETURN
      END
