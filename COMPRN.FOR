C*******************************************************************
C
C     NAME:     SUBROUTINE COMPRN
C
C     FUNCTION:  ORDERS A STRING CONTAINING OPERANDS, OPERATORS, AND
C                PARENTHESIS INTO REVERSE POLISH NOTATION (POST-FIX)
C                FORM WHERE EACH OPERATOR FOLLOWS THE OPERAND(S) IT
C                WORKS UPON, TO AID IN PROCESSING FOR BOOLEAN AND
C                MATH COMMANDS.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  ---------------------------------------------------
C                  ISTAK    I/O     I*2                    +INTEGER
C                           INPUT BOOLEAN EXPRESSION
C                  ---------------------------------------------------
C                  JSTAK    I       I*2                    +INTEGER
C                           INPUT WORK ARRAY FOR STACK
C                  ---------------------------------------------------
C                  IS       I       I*2                    +INTEGER
C                           SIZE OF BOOLEAN EXPRESSION INPUT (ISTAK)
C                  ---------------------------------------------------
C                  NSYMB    O       I*2                    +INTEGER
C                           SIZE OF BOOLEAN EXPRESSION OUTPUT (ISTAK)
C                  ---------------------------------------------------
C                  IER      O       I*2                     INTEGER
C                           ERROR INDICATOR
C                  ---------------------------------------------------
C********************************************************************
C
      SUBROUTINE comprn(istak,jstak,is,nsymb,ier)
      INTEGER is , nsymb , outptr , jptr , inptr , istak(2,128) , 
     &        jstak(2,128)
C
C......INITIALIZE VARIABLES AND TEMPORARY ARRAY ......
C
      outptr = 0
      jptr = 0
      DO i = 1 , is
         jstak(1,i) = 0
         jstak(2,i) = 0
      ENDDO
C
C......PROCESS EACH SYMBOL IN INPUT ARRAY .......
C
      DO inptr = 1 , is
         IF ( istak(2,inptr).GT.0 ) THEN
C................OPERAND, PUSH ONTO OUTPUT STACK .....
            outptr = outptr + 1
            istak(1,outptr) = istak(1,inptr)
            istak(2,outptr) = istak(2,inptr)
            GOTO 100
         ELSEIF ( istak(2,inptr).EQ.-1 ) THEN
C................OPENING PARENS, PUSH ONTO TEMPORARY STACK .....
            jptr = jptr + 1
            jstak(1,jptr) = istak(1,inptr)
            jstak(2,jptr) = istak(2,inptr)
            GOTO 100
         ELSEIF ( istak(2,inptr).NE.-2 ) THEN
C...........OPERATOR, COMPARE WITH TOP OF TEMPORARY STACK .....
 20         CONTINUE
            IF ( jptr.NE.0 ) THEN
               IF ( istak(2,inptr).GE.jstak(2,jptr) ) THEN
C.....................POP PRIORITY OPERATORS FROM TEMPORARY STACK...
                  outptr = outptr + 1
                  istak(1,outptr) = jstak(1,jptr)
                  istak(2,outptr) = jstak(2,jptr)
                  jptr = jptr - 1
                  GOTO 20
               ENDIF
            ENDIF
         ELSE
C................CLOSING PARENS, CHECK TOP OF TEMPORARY STACK .....
 40         CONTINUE
            IF ( jstak(2,jptr).EQ.-1 ) THEN
C................POP OPENING PARENS FROM TEMPORARY STACK .....
               jptr = jptr - 1
               GOTO 100
            ELSE
C.....................POP FROM TEMPORARY STACK TIL HIT OPENING PARENS...
               outptr = outptr + 1
               istak(1,outptr) = jstak(1,jptr)
               istak(2,outptr) = jstak(2,jptr)
               jptr = jptr - 1
               GOTO 40
            ENDIF
         ENDIF
C................PUSH NEW OPERATOR ONTO TEMPORARY STACK .....
         jptr = jptr + 1
         jstak(1,jptr) = istak(1,inptr)
         jstak(2,jptr) = istak(2,inptr)
 100  ENDDO
C
C......CHECK FOR EMPTY TEMPORARY STACK .....
C
 200  CONTINUE
      IF ( jptr.EQ.0 ) THEN
C
C......ASSIGN NUMBER OF SYMBOLS ON OUTPUT STACK ......
         nsymb = outptr
         RETURN
      ELSE
C...........POP REMAINING SYMBOLS FROM TEMPORARY STACK .....
         IF ( jstak(2,jptr).EQ.-1 ) THEN
            jptr = jptr - 1
         ELSE
            outptr = outptr + 1
            istak(1,outptr) = jstak(1,jptr)
            istak(2,outptr) = jstak(2,jptr)
            jptr = jptr - 1
         ENDIF
         GOTO 200
      ENDIF
      END
