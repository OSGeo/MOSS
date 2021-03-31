C**********************************************************************
C
C     NAME:  SUBROUTINE COMPM
C
C     FUNCTION:  EXTRACT WORD FROM INPUT, COUNT CHARS, AND TRY TO DECODE THEN
C                PERFORM A MATCHING PROCESS BY COMPARING THE CURRENT
C                WORD FROM THE COMMAND LINE WITH THOSE PHRASES WHICH
C                ARE VALID FOR THE CURRENT COMMAND, THEN RETURNS A VALUE
C                INDICATING WHICH PHRASE THE WORD MATCHED OR INDICATING
C                THAT NO MATCH WAS MADE.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IARRY     I      I*2        VARIES        CHAR
C                  ARRAY CONTAINING VALID PHRASES FOR CURRENT COMMAND
C                  -------------------------------------------------
C                  ICARD     I      I*2        80            CHAR
C                  ARRAY CONTAINING THE INPUT COMMAND LINE
C                  -------------------------------------------------
C                  icardp         I/O    I*2                      INTE
C                  POSITION IN ICARD OF THE LAST ENCOUNTERED BLANK CHARACTER
C                  -------------------------------------------------
C                  W         I/O    I*2                      INTE
C                  WORD NUMBER ON THE COMMAND LINE FOR IWORD
C                  -------------------------------------------------
C                  INDEX     O      I*2                    +INTEGER
C                  MATCHING PHRASE INDICATOR
C                  0   - NO MATCH FOUND
C                  N   - INDEX TO MATCHING PHRASE
C                  N+1 - END OF COMMAND LINE
C                  -------------------------------------------------
C                  IWORD     O      I*2          10          CHAR
C                  ARRAY CONTAINING THE EXTRACTED COMMAND LINE WORD
C                  ARRAY IS NULL FILLED RIGHT AND CHARS ARE ONE PER WORD
C                  -------------------------------------------------
C                  RNUM      O      R*4                      REAL
C                  VALUE OF IWORD IF IT CAN BE DECODED TO A NUMBER
C                  -------------------------------------------------
C                  LENA      O      I*2                      INTE
C                  NUMBER OF CHARACTERS IN IWORD ARRAY
C                  -------------------------------------------------
C                  IER       O      I*2                      INTEGER
C                  ERROR INDICATOR
C                  -------------------------------------------------
C***********************************************************************
C
      SUBROUTINE compm(iarry,icard,icardp,w,indx,iword,rnum,lena,ier)
      COMMON /io    / nprnt , ioin
      INTEGER iarry(1) , icard(80) , icardp , w , indx , iword(10) , 
     &        lena , ier , nxtchr , iend , iblank , idelim , wpntr , 
     &        ibyte
      REAL rnum
      LOGICAL found
C
      IF ( icardp.EQ.1 ) icardp = 0
      ier = 0
C
C......INCREMENT THE WORD COUNTER ......
      w = w + 1
C
C......INITIALIZE THE OUTPUT WORD, CHAR COUNT, REAL VALUE ......
      lena = 0
      rnum = 0.0
      DO i = 1 , 10
         iword(i) = 0
      ENDDO
C
C......MOVE AND COUNT CHARACTERS  ALA DECIP .....
 100  CONTINUE
      icardp = icardp + 1
      IF ( icard(icardp).NE.0 ) THEN
         IF ( icard(icardp).NE.32 ) THEN
            lena = lena + 1
            IF ( lena.LE.10 ) iword(lena) = icard(icardp)
            GOTO 100
         ENDIF
      ENDIF
C
C......SEE IF CAN MAKE A NUMBER OUT OF IT ......
      IF ( lena.GT.10 ) lena = 10
      j = 0
      CALL fnum(iword,10,rnum,j)
C
C......INITIALIZE VARIABLES  '@',' ','!' ......
      iend = 64
      iblank = 0
      idelim = 33
      found = .TRUE.
      wpntr = 0
      indx = 1
      ibyte = 0
C
C......COMPARE EACH CHARACTER OF COMMAND WORD & PHRASE .....
 200  CONTINUE
      wpntr = wpntr + 1
      ibyte = ibyte + 1
      nxtchr = iarry(ibyte)
C......CHECK FOR @ INDICATING END OF VALID PHRASES FOR COMMAND .......
      IF ( nxtchr.EQ.iend ) THEN
C
C......END OF VALID PHRASES ......
C......CHECK FOR BLANK WORD INDICATING END OF COMMAND LINE .....
         IF ( iword(1).NE.iblank ) THEN
C...........NO MATCH FOUND .......
            indx = 0
         ENDIF
C...........CHECK FOR ! INDICATING END OF CURRENT PHRASE .....
      ELSEIF ( nxtchr.EQ.idelim ) THEN
C
C...........END OF CURRENT PHRASE ......
         IF ( .NOT.(found) ) THEN
C................NO MATCH YET, COMPARE TO NEXT PHRASE .....
            found = .TRUE.
            indx = indx + 1
            wpntr = 0
            GOTO 200
         ENDIF
      ELSE
C................SET FLAG TO INDICATE NON-MATCHING PHRASE .....
         inpu = iword(wpntr)
C.......DECIP CHANGES BLANKS TO NULLS ......
         IF ( inpu.EQ.0 ) inpu = 32
         IF ( nxtchr.NE.inpu ) found = .FALSE.
         GOTO 200
      ENDIF
C
      RETURN
      END
