C*******************************************************************
C
C     NAME:     SUBROUTINE RPN
C
C     FUNCTION:  THIS ROUTINE ORDERS A STRING CONTAINING OPERANDS,
C                OPERATORS, AND PARENTHESIS INTO REVERSE POLISH
C                NOTATION (OR POST-FIX).  THIS FORM IS WHERE EACH
C                OPERATOR FOLLOWS THE OPERAND(S) IT WORKS UPON.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
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
C
C     FILES:  NONE
C********************************************************************
C
      SUBROUTINE rpn(istak,jstak,is,nsymb,ier)
      INTEGER is , nsymb , outptr , jptr , inptr , istak(128) , 
     &        jstak(128)
C
C......INITIALIZE VARIABLES AND TEMPORARY ARRAY ......
C
      outptr = 0
      jptr = 0
      DO i = 1 , is
         jstak(i) = 0
      ENDDO
C
C......PROCESS EACH SYMBOL IN INPUT ARRAY .......
C
      DO inptr = 1 , is
         IF ( istak(inptr).GT.0 ) THEN
C................OPERAND, PUSH ONTO OUTPUT STACK .....
            outptr = outptr + 1
            istak(outptr) = istak(inptr)
            GOTO 100
         ELSEIF ( istak(inptr).EQ.-1 ) THEN
C................OPENING PARENS, PUSH ONTO TEMPORARY STACK .....
            jptr = jptr + 1
            jstak(jptr) = istak(inptr)
            GOTO 100
         ELSEIF ( istak(inptr).NE.-2 ) THEN
C...........OPERATOR, COMPARE WITH TOP OF TEMPORARY STACK .....
 20         CONTINUE
            IF ( jptr.NE.0 ) THEN
               IF ( istak(inptr).GE.jstak(jptr) ) THEN
C.....................POP LOWER PRIORITY OPERATORS FROM TEMPORARY STACK...
                  outptr = outptr + 1
                  istak(outptr) = jstak(jptr)
                  jptr = jptr - 1
                  GOTO 20
               ENDIF
            ENDIF
         ELSE
C................CLOSING PARENS, CHECK TOP OF TEMPORARY STACK .....
 40         CONTINUE
            IF ( jstak(jptr).EQ.-1 ) THEN
C................POP OPENING PARENS FROM TEMPORARY STACK .....
               jptr = jptr - 1
               GOTO 100
            ELSE
C.....................POP FROM TEMPORARY STACK TIL HIT OPENING PARENS...
               outptr = outptr + 1
               istak(outptr) = jstak(jptr)
               jptr = jptr - 1
               GOTO 40
            ENDIF
         ENDIF
C................PUSH NEW OPERATOR ONTO TEMPORARY STACK .....
         jptr = jptr + 1
         jstak(jptr) = istak(inptr)
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
         IF ( jstak(jptr).EQ.-1 ) THEN
            jptr = jptr - 1
         ELSE
            outptr = outptr + 1
            istak(outptr) = jstak(jptr)
            jptr = jptr - 1
         ENDIF
         GOTO 200
      ENDIF
      END
