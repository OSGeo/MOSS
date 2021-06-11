      SUBROUTINE decip1(istrng,icom,k,numchr,maxcom,ier)
      COMMON /io    / nprnt , ioin
      DIMENSION istrng(1) , icom(maxcom)
      LOGICAL mcomp
C
C *****************************************************************
C
C          S U B R O U T I N E    D E C I P 1
C
C  FUNCTION: GET A SUBSTRING FROM A CHARACTER STRING.  THE SUBSTRING
C            MAY BE IN QUOTES, BUT IF NOT, THEN A SPACE IS THE END
C            DELIMITER.
C
C  PARAMETERS:
C             ISTRNG    IN - STRING ARRAY.
C             ICOM      OUT- RETURNED ARRAY.
C             K         IN - FIRST CHARACTER POSITION IN ISTRNG ARRAY.
C             NUMCHR    OUT- THE NUMBER OF CHARACTERS IN ICOM
C             MAXCOM    IN - LENGTH OF ICOM
C             IER       OUT- IER = 2 IF THERE IS NO CLOSING QUOTE
C
C *****************************************************************
C
      ier = 0
      iquote = 0
      numchr = 0
C
      DO i = 1 , maxcom
         icom(i) = 0
      ENDDO
C
C  CHECK FOR AN INITIAL QUOTE
C
      k = k + 1
      IF ( istrng(k).EQ.34 ) iquote = iquote + 1
      IF ( istrng(k).NE.34 ) THEN
C
C  NO QUOTE, SO PUT THE FIRST CHARACTER INTO ICOM
C
         icom(1) = istrng(k)
         numchr = numchr + 1
      ENDIF
C
C  LOOP THROUGH TILL ANOTHER QUOTE, OR A SPACE IF NO INITIAL QUOTE
C
 100  CONTINUE
      k = k + 1
      IF ( istrng(k).EQ.34 ) THEN
C
C  GOT A QUOTE, CHECK FOR THE INITIAL ONE
C
         iquote = iquote + 1
         IF ( iquote.NE.2 ) ier = 2
      ELSEIF ( (istrng(k).NE.32) .OR. (iquote.NE.0) ) THEN
         IF ( istrng(k).EQ.0 ) THEN
C
C  WE HIT THE END OF THE INPUT LINE... WAS THERE AN INITIAL QUOTE?
C
            IF ( iquote.NE.0 ) ier = 2
         ELSEIF ( mcomp(istrng(k),')',1,ier) ) THEN
C
C  WE HIT AN END PAREN, SUBTRACT ONE FROM THE CHAR COUNT SO THE CALLING
C  ROUTINE KNOWS IT'S THERE  (WE ALREADY READ PAST IT HERE)
            k = k - 1
         ELSEIF ( numchr.NE.maxcom ) THEN
            numchr = numchr + 1
            icom(numchr) = istrng(k)
            GOTO 100
         ENDIF
      ENDIF
C
      RETURN
      END
