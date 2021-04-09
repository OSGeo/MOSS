      SUBROUTINE newsub(klpnt,nchars,title1,title2,isub)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C  NAME:   N  E  W  S  U  B
C
C  FUNCTION:  THIS ROUTINE CREATES A MERGED SUBJECT NAME FROM THE
C             DESIGNATED PORTION OF TWO TITLES
C
C  PARAMETERS:
C    INPUT:   MLPNT = INDEX IN THE COMBINED SUBJECT NAME WHERE THE
C                     TITLE2 WORDS ARE STARTED FOR INSERTION
C             NCHARS= NUMBER WORDS FROM TITLE2 TO BE INSERTED
C             TITLE1= ARRAY OF 15 WORDS - PACKED CHAR.- FOR THE TITLE
C             TITLE2= ARRAY OF 15 WORDS - PACKED CHAR.- FOR 2ND TITLE
C    OUTPUT:  ISUB  = ARRAY OF 15 WORDS CREATED FROM THE COMBINED
C                     PORTIONS OF TITLE1 AND TITLE2
C  LOCAL VARIABLE:
C       I    =  COUNTER FOR A LOOP
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      INTEGER klpnt , nchars , title1(1) , title2(1) , isub(1)
C
C                            -FILL IN SUBJECT ARRAY WITH FIRST TITLE
      DO l = 1 , 15
         isub(l) = 32
      ENDDO
      IF ( klpnt.NE.0 .OR. nchars.NE.0 ) THEN
         IF ( klpnt.NE.0 ) THEN
            loop = klpnt/2 + 1
C
            DO i = 1 , loop
               isub(i) = title1(i)
            ENDDO
         ENDIF
C                            -INSERT TITLE2 WORDS FROM STARTING POINT KLPNT
C
C
         IF ( nchars.NE.0 ) THEN
            DO jkl = 1 , (nchars/2+1)
C
               isub(klpnt+jkl) = title2(jkl)
C
            ENDDO
         ENDIF
      ENDIF
C
C        CALL PAD(ISUB,15)
C
      RETURN
      END
