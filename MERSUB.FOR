      SUBROUTINE mersub(title1,title2,isub)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C  NAME:    M  E  R  S  U  B
C
C  FUNCTION: THIS ROUTINE CREATES A MERGED SUBJECT NAME FROM
C            THE EQUAL PORTIONS OF TWO TITLES
C
C  PARAMETERS:
C    OUTPUT:
C             ISUB   = ARRAY OF 15 WORDS OF PACKED CHAR. CREATED FROM
C                      THE FIRST 7 WORDS FROM TITLE1 AND THE FIRST 8
C                      WORDS FROM TITLE2 IN THAT RESPECTIVE ORDER
C
C  LOCAL VARIABLES:
C        I     =  COUNTER FOR A LOOP
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      INTEGER title1(1) , title2(1) , isub(1) , i
C
      DO i = 1 , 15
         isub(i) = ('  ')
      ENDDO
      DO i = 1 , 7
         isub(i) = title1(i)
         isub(i+7) = title2(i)
      ENDDO
C     ENDDO
C
      isub(15) = title2(8)
C
      RETURN
      END
