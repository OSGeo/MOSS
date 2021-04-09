C************************************************************************
C
C     NAME:  SUBROUTINE MATHSTK
C
C     FUNCTION:  PROCESSES A MATHEMATICAL EXPRESSION IN REVERSE POLISH
C                NOTATION (POST-FIX FORM) AND BREAKS THE EXPRESSION UP
C                INTO PARTIAL STACKS WHICH MEET TWO CRITERIA--REQUIRING
C                LESS THAN A MAXIMUM NUMBER OF MAPS, AND ENDING WITH A
C                SINGLE RESULTING MAP WHICH EVENTUALLY RESULTS IN THE
C                FINAL MAP FOR MATH COMMAND.  THE PURPOSE OF THESE PARTIAL
C                STACKS IS TO SAVE ON DISK ACCESSES TO RESULTANT MAPS
C                BY ALLOWING OPENING OF SEVERAL INPUT MAPS.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  ISTAK     I      I*2                    +INTEGER
C                  ARRAY OF SYMBOLS
C                  -------------------------------------------------
C                  NSYMB     I      I*2                    +INTEGER
C                  NUMBER OF SYMBOLS IN ORIGINAL STACK
C                  -------------------------------------------------
C                  MXMAP     I      I*2                    +INTEGER
C                  MAXIMUM NUMBER OF MAPS IN PARTIAL STACK
C                  -------------------------------------------------
C                  ISTRT    I/O     I*2                    +INTEGER
C                  FIRST NON-ZERO ELEMENT IN ORIGINAL STACK
C                  -------------------------------------------------
C                  NUSED     O      I*2                    +INTEGER
C                  NUMBER OF MAPS USED IN PARTIAL STACK
C                  -------------------------------------------------
C                  IUSED     O      I*2        MXMAP        +INTEGER
C                  INDEX OF MAPS USED IN PARTIAL STACK
C                  -------------------------------------------------
C                  IBEGN     O      I*2                    +INTEGER
C                  INDEX INTO ORIGINAL STACK WHERE PARTIAL BEGINS
C                  -------------------------------------------------
C                  ILAST     O      I*2                    +INTEGER
C                  INDEX INTO ORIGINAL STACK WHERE PARTIAL ENDS
C                  -------------------------------------------------
C                  IER       O      I*2                     INTEGER
C                  ERROR INDICATOR
C                  -------------------------------------------------
C*************************************************************************
C
       SUBROUTINE MATHSTK (ISTAK,NSYMB,MXMAP,ISTRT,NUSED,IUSED,
     +  IBEGN,ILAST,IER)
       COMMON /IO/    NPRNT,IOIN
       COMMON /MAXS/  MX(16),RMAX
       INTEGER ISTRT,IBEGN,ILAST,MXMAP,ILESS,NSYMB,OP1,OP2,REM,PKG,
     1         IU,IT,ISTAK(2,128),ITEMP(64),IUSED(1)
       LOGICAL SET
C
C......INITIALIZE VARIABLES ......
C
       DO 100  I=1,MX(8)
            ITEMP(I) = 0
  100       CONTINUE
       DO 150  I=1,MXMAP
            IUSED(I) = 0
  150       CONTINUE
C
       IBEGN = NSYMB + 1
       ILESS = NSYMB + 1
       ILAST = 0
       NUSED = 0
       OP1 = 0
       OP2 = 0
       REM = 0
       PKG = 0
       IU = 0
       IT = 0
       SET = .FALSE.
C
C......PROCESS STACK TO GET PARTIAL STACK ......
C
       DO 1200  I=ISTRT,NSYMB
C...........CHECK FOR ZERO STACK ELEMENT ......
            IF (ISTAK(1,I) .EQ. 0)  GO TO 1200
            IF (SET)  GO TO 200
C................SET START TO FIRST NON-ZERO ELEMENT ......
                 ISTRT = I
                 SET = .TRUE.
  200       CONTINUE
C...........CHECK FOR A FUNCTION ......
            IF (ISTAK(2,I) .EQ. -6)  GO TO 1200
C...........CHECK FOR AN OPERATOR ......
            IF (ISTAK(2,I) .LT. 1)  GO TO 400
C................PROCESS OPERAND .......
C................PUSH MAPNAME OR VALUE INDEX ONTO TEMPORARY STACK ......
                 IT = IT + 1
                 IF (IT .GT. MX(8)) GO TO 81100
                 ITEMP(IT) = I
C................CHECK FOR FIRST OPERAND ......
                 IF (OP1 .EQ. 1)  GO TO 250
C.....................SET FIRST OPERAND ......
                      OP1 = 1
                      GO TO 1200
  250            CONTINUE
C................CHECK FOR SECOND OPERAND ......
                 IF (OP2 .EQ. 1)  GO TO 300
C....................SET SECOND OPERAND ......
                     OP2 = 1
                     GO TO 1200
  300            CONTINUE
C................INCREMENT REMAINDER OPERAND ......
                 REM = REM + 1
                 GO TO 1200
C
  400       CONTINUE
C...........PROCESS OPERATOR ......
            IF (OP2 .EQ. 1)  GO TO 600
C................OBTAIN SECOND OPERAND ......
                 IF (PKG .LT. 1)  GO TO 500
C.....................OPERAND IS LAST PACKAGE ......
                      PKG = PKG - 1
                      GO TO 600
  500            CONTINUE
C................CHECK FOR REMAINDERS ......
                 IF (REM .LT. 1)  GO TO 81200
C................OPERAND IS LAST REMAINDER ......
                 REM = REM - 1
C................CHECK IF OPERAND IS MAPNAME OR VALUE ......
                 IF (IT .LT. 1)  GO TO 81200
                 IF (ISTAK(2,ITEMP(IT)) .GT. 1)  GO TO 550
C.....................OPERAND IS MAP, PUSH INDEX ONTO USED STACK ......
                      IU = IU + 1
                      IF (IU .GT. MXMAP)  GO TO 1300
                      IUSED(IU) = ITEMP(IT)
  550            CONTINUE
C................CHECK AGAINST CURRENT LEAST INDEX ......
                 IF (ITEMP(IT) .LT. ILESS)  ILESS = ITEMP(IT)
C................POP INDEX FROM TEMPORARY STACK ......
                 IT = IT - 1
                 GO TO 600
  600       CONTINUE
C...........CHECK FOR EXISTING FIRST OPERAND ......
            IF (OP1 .EQ. 1)  GO TO 800
C................OBTAIN FIRST OPERAND ......
                 IF (PKG .LT. 1)  GO TO 700
C.....................OPERAND IS LAST PACKAGE ......
                      PKG = PKG - 1
                      GO TO 800
  700            CONTINUE
C................CHECK FOR REMAINDERS ......
                 IF (REM .LT. 1)  GO TO 81200
C................OPERAND IS LAST REMAINDER ......
                 REM = REM - 1
C................CHECK IF OPERAND IS MAPNAME OR VALUE ......
                 IF (IT .LT. 1)  GO TO 81200
                 IF (ISTAK(2,ITEMP(IT)) .GT. 1)  GO TO 750
C.....................OPERAND IS MAP, PUSH INDEX ONTO USED STACK ......
                      IU = IU + 1
                      IF (IU .GT. MXMAP) GO TO 1300
                      IUSED(IU) = ITEMP(IT)
  750            CONTINUE
C................CHECK AGAINST CURRENT LEAST INDEX ......
                 IF (ITEMP(IT) .LT. ILESS)  ILESS = ITEMP(IT)
C................POP INDEX FROM TEMPORARY STACK ......
                 IT = IT - 1
                 GO TO 800
  800       CONTINUE
C...........INCREMENT NUMBER OF RESULT PACKAGES ......
            PKG = PKG + 1
            IF (PKG .GT. 1)  GO TO 1300
            IF (OP1 .EQ. 0)  GO TO 900
C................CHECK IF OPERAND IS MAPNAM OR VALUE ......
                 IF (IT .LT. 1)  GO TO 81200
                 IF (ISTAK(2,ITEMP(IT)) .GT. 1)  GO TO 850
C.....................OPERAND IS MAP, PUSH INDEX ONTO USED STACK ......
                      IU = IU + 1
                      IF (IU .GT. MXMAP)  GO TO 1300
                      IUSED(IU) = ITEMP(IT)
  850            CONTINUE
C................CHECK AGAINST CURRENT LEAST INDEX ......
                 IF (ITEMP(IT) .LT. ILESS)  ILESS = ITEMP(IT)
C................POP INDEX FROM TEMPORARY STACK ......
                 IT = IT - 1
                 GO TO 900
  900       CONTINUE
            IF (OP2 .EQ. 0)  GO TO 1000
C................CHECK IF OPERAND IS MAPNAME OR VALUE ......
                 IF (IT .LT. 1)  GO TO 81200
                 IF (ISTAK(2,ITEMP(IT)) .GT. 1)  GO TO 950
C.....................OPERAND IS MAP, PUSH INDEX ONTO USED STACK ......
                      IU = IU + 1
                      IF (IU .GT. MXMAP)  GO TO 1300
                      IUSED(IU) = ITEMP(IT)
  950            CONTINUE
C................CHECK AGAINST CURRENT LEAST INDEX ......
                 IF (ITEMP(IT) .LT. ILESS)  ILESS = ITEMP(IT)
C................POP INDEX FROM TEMPORARY STACK ......
                 IT = IT - 1
                 GO TO 1000
 1000       CONTINUE
            OP1 = 0
            OP2 = 0
C...........CHECK FOR MAXIMUM MAPS ......
            IF (IU .GT. MXMAP)  GO TO 1300
C...........CHECK FOR A SINGLE RESULT PACKAGE ......
            IF (PKG .NE. 1)  GO TO 1200
C................SAVE BEGINNING & LAST STACK POINTER & #MAPS ......
                 IF (ILESS .LT. IBEGN)  IBEGN = ILESS
                 ILAST = I
                 NUSED = IU
 1200       CONTINUE
C
C......CHECK IF NUSED HAS BEEN SET ......
       IF (NUSED .NE. 0)  GO TO 1300
C...........SET VARIABLES FOR SINGLE MAP EXPRESSION ......
            NUSED = 1
            IBEGN = 1
            ILAST = NSYMB
            IUSED(1) = ITEMP(IT)
 1300  CONTINUE
C......NORMAL RETURN ......
       GO TO 90000
C
C......ERROR RETURNS .......
C
81100  CONTINUE
       IER = -5630
       WRITE (NPRNT,98110)  MX(8),IER
98110  FORMAT (' *MATHSTK*NUMBER OF TEMPORARY MAPS EXCEEDS MAXIMUM OF',
     1      I5,'  ERROR ',I5)
       GO TO 90000
C
81200  CONTINUE
       IER = -5632
       WRITE (NPRNT,98120)  IER
98120  FORMAT (' *MATHSTK*NO REMAINING OPERANDS TO MATCH OPERATOR',
     1         '  ERROR',I5)
       GO TO 90000
C
C......NORMAL RETURN ......
C
90000  CONTINUE
       RETURN
       END

