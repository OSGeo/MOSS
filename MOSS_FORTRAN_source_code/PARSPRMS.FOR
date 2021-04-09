      SUBROUTINE parsprms(inarray,idimin,outarray,nsubs,ok)
C**********************************************************************
C  ROUTINE PARSES THE CHARACTER STRING ITEMS DELIMINATED BY COMMAS OUT
C  OF INARRAY AND PUTS THEM BACK AS REALS INTO OUTARRAY.
C  DEVELOPED AS A TOOL TO OVERWRITE PARAMETERS. IF STRING HAS
C  CONSECUTIVE COMMAS VALUE ASSUMED TO DEFAULT TO PRIOR VALUE.
C  NUMBERS MUST BE ENTERED IN ASCENDING ORDER. ALL VALUES ASSUMMED
C  TO BE UPPER BOUNDS, IF SEPARATED BY A "TO" THEN VALUES ASSUMMED
C  TO BE LOWER AND UPPER BOUND
C        INARRAY EX:  -2, 5 TO 9, , 100
C
C  INPUT: INARRAY(IDIMIN) CHARACTER STRING
C         IDIMIN   DIMENSION OF INARRAY
C         OUTARRAY(100,2) REAL VALUES WHICH HAVE ALREADY BEEN
C                           ASSIGNED SOMEWHERE.  OVERWRITTEN BY
C                           VALUES IN INARRAY.
C         NSUBS  NUMBER OF INTERVALS
C  OUTPUT: OUTARRAY
C
C  CALLED FROM GETHSTPRMS
C**********************************************************************
      INTEGER inarray(idimin)
      INTEGER iword(80)
      INTEGER nsubs
      INTEGER outindex , startwrd , zero
C
      REAL outarray(100,2)
      LOGICAL ok , change , mcomp
C
      outindex = 0
      startwrd = 0
      valhold = outarray(1,1)
      ok = .TRUE.
C
C  begin parsing out string
 100  CONTINUE
      IF ( startwrd.LE.idimin ) THEN
         CALL nextwd(inarray,iword,80,startwrd)
         vallast = valhold
         IF ( mcomp(iword,'TO',2,ier) ) THEN
            outindex = outindex - 1
            CALL nextwd(inarray,iword,80,startwrd)
         ENDIF
C
         zero = 0
         CALL fnum(iword,idimin,valhold,zero)
         outindex = outindex + 1
         IF ( outindex.GT.nsubs ) THEN
C  if new values are not greater that last, print warning
            DO i = 1 , nsubs
               IF ( outarray(i,1).LE.outarray(i,2) ) ok = .TRUE.
            ENDDO
            IF ( .NOT.ok ) WRITE (nprnt,99001)
99001       FORMAT (' ERROR VALUES MUST BE IN ASCENDING ORDER ')
C
C
            RETURN
C
C  check if ", ,"
         ELSEIF ( valhold.EQ.0 ) THEN
            IF ( valhold.LE.vallast ) THEN
               valhold = outarray(outindex,2)
            ENDIF
         ENDIF
      ENDIF
C
      outarray(outindex,1) = vallast
      outarray(outindex,2) = valhold
      change = .TRUE.
      GOTO 100
      END
