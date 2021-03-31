C************************************************************************* 
C 
C     NAME:  SUBROUTINE FASTCONT 
C 
C     FUNCTION:  GENERATES A CONTOUR PLOT FOR DISPLAY ONLY OF AN 
C                INTEGER-VALUE CONTINUOUS MAP BY PROCESSING ROW-BY-ROW 
C                FOR CONTOUR COMMAND.  IT REQUIRES THE CURRENT ROW AND 
C                THE PREVIOUS ROW OF ELEVATION VALUES WITH EACH PASS. 
C 
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE 
C                  ------------------------------------------------- 
C                  NCOL1     I      I*2        NCOLS       INTEGER 
C                  FIRST PROFILE OF ELEVATION DATA 
C                  ------------------------------------------------- 
C                  NCOL2     I      I*2        NCOLS       INTEGER 
C                  SECOND PROFILE OF ELEVATION DATA 
C                  ------------------------------------------------- 
C                  NCOLS     I      I*2                    +INTEGER 
C                  NUMBER OF ELEVATION VALUES PER ROW (OR PROFILE) 
C                  ------------------------------------------------- 
C                  VMIN      I      R*4                     REAL 
C                  ACTUAL MINIMUM VALUE OF MAP 
C                  ------------------------------------------------- 
C                  VMAX      I      R*4                     REAL 
C                  ACTUAL MAXIMUM VALUE OF MAP 
C                  ------------------------------------------------- 
C                  DELTX     I      R*4                     +REAL 
C                  NUMBER OF METERS IN X PER CELL 
C                  ------------------------------------------------- 
C                  DELTY     I      R*4                     +REAL 
C                  NUMBER OF METERS IN Y PER CELL 
C                  ------------------------------------------------- 
C                  XORG      I      R*4                      REAL 
C                  X ORIGIN OF DEM 
C                  ------------------------------------------------- 
C                  YORG      I      R*4                      REAL 
C                  Y ORIGIN OF DEM 
C                  ------------------------------------------------- 
C                  INTVAL    I      I*2                     +INTEGER 
C                  CONTOUR INTERVAL 
C                  ------------------------------------------------- 
C                  IROW      I      I*2                     +INTEGER 
C                  NUMBER OF CURRENT ROW 
C                  ------------------------------------------------- 
C                  IER       O      I*2                      INTEGER 
C                  ERROR INDICATOR (0=NORMAL) 
C                  ------------------------------------------------- 
C************************************************************************* 
C 
      SUBROUTINE FASTCONT (NCOL1,NCOL2,NCOLS,VMIN,VMAX,DELTX,DELTY, 
     1                     XORG,YORG,INTVAL,IROW,IER) 
C 
      COMMON /WORK/   IBUFF(128),MAP(25),ICOM(10),JCOM(10) 
      DIMENSION LINES(5) 
      INTEGER*2 MAPX(25) 
      INTEGER*2 NCOL1(1),NCOL2(1) 
      REAL IDEL1,IDEL2,IADD,VMIN,VMAX 
      REAL LINES,LX(4),LY(4) 
C 
           MINELV=32767 
           MAXELV=-32767 
C 
           ISTART=1 
           ISTOP=NCOLS 
           DO 100 I =ISTART,ISTOP 
C...............RESET ZERO-VALUE TO ACTUAL MINIMUM ...... 
                IF (NCOL1(I) .LT. HFIX(VMIN))  NCOL1(I) = HFIX(VMIN) 
                IF (NCOL2(I) .LT. HFIX(VMIN))  NCOL2(I) = HFIX(VMIN) 
                IF (NCOL1(I) .GT. HFIX(VMAX))  NCOL1(I) = HFIX(VMAX) 
                IF (NCOL2(I) .GT. HFIX(VMAX))  NCOL2(I) = HFIX(VMAX) 
                IELEV1 = NCOL1(I) 
                IELEV2 = NCOL2(I) 
                IF (IELEV1 .LT. MINELV) MINELV = IELEV1 
                IF (IELEV1 .GT. MAXELV) MAXELV = IELEV1 
                IF (IELEV2 .LT. MINELV) MINELV = IELEV2 
                IF (IELEV2 .GT. MAXELV) MAXELV = IELEV2 
  100      CONTINUE 
CC 
CC 
CC COMPUTE THE MINIMUM AND MAXIMUM CONTOUR OF THE 2 PROFILES 
CC 
CC 
c           MINCNT = HFIX((FLOAT(MINELV)/FLOAT(INTVAL)) * FLOAT(INTVAL)) 
c           MAXCNT = HFIX((FLOAT(MAXELV)/FLOAT(INTVAL)) * FLOAT(INTVAL)) 
C PRIME MODS 
      MINCNT = (MINELV/INTVAL)*INTVAL 
      MAXCNT = (MAXELV/INTVAL)*INTVAL 
C..........CHECK FOR ZERO MINIMUM  ...... 
C           IF (MINCNT .EQ. 0)  GO TO 80100 
CC 
CC ***************************************************************** 
CC LOOP ON CONTOURS IN THE 2 PROFILES 
CC 
CC 
           ISTART = 0 
           ISTOP = ISTOP - 1 
           call vecmod
           DO 260 ICONT = MINCNT,MAXCNT,INTVAL 
C 
C  ALTER ELEVATIONS IF NECESSARY 
C 
             CALL ALTER(NCOL1,NCOLS,ICONT) 
             CALL ALTER(NCOL2,NCOLS,ICONT) 
CC 
CC ******************************************************************* 
CC LOOP ON ELEVATIONS IN THE TWO PROFILES 
CC 
                DO 240 ICOL = ISTART,ISTOP 
CC 
CC CHECK FOR A CONTOUR CROSSING ON THE BOTTOM HORIZONTAL LINE 
CC 
                   K = 0 
C          CHECK FOR SPECIAL CASE OF LEFT COL PLOT 
                   IF (ICOL .NE. 0) GO TO 110 
                      IDEL1 = ICONT - NCOL1(1) 
                      IDEL2 = ICONT - NCOL2(1) 
                      IF (IDEL1*IDEL2 .GT. 0) GO TO 120 
                      K = K + 1 
                      IF( NCOL2(1)-NCOL1(1).EQ.0) THEN 
                        IADD = DELTY*IDEL1 
                      ELSE 
                        IADD = (DELTY*IDEL1)/(NCOL2(1)-NCOL1(1)) 
                      ENDIF 
                      GO TO 115 
110                CONTINUE 
                   IDEL1 = ICONT - NCOL1(ICOL) 
                   IDEL2 = ICONT - NCOL2(ICOL) 
                   IF (IDEL1*IDEL2 .GT. 0) GO TO 120 
                   K = K + 1 
C                   IADD=(DELTY*IDEL1)/(NCOL2(ICOL)-NCOL1(ICOL)) 
                      IF( NCOL2(ICOL)-NCOL1(ICOL).EQ.0) THEN 
                        IADD = DELTY*IDEL1 
                      ELSE 
                        IADD = (DELTY*IDEL1)/(NCOL2(ICOL)-NCOL1(ICOL)) 
                      ENDIF 
115                CONTINUE 
                   LX(1) = ICOL*DELTX 
                   LY(1) = (IROW)*DELTY - IADD 
CC 
CC CHECK FOR A CONTOUR CROSSING ON THE LEFT VERTICAL LINE 
CC 
 120               CONTINUE 
CC CHECK NOT NECESSARY FOR LEFT COLUMN 
                   IF (ICOL .EQ. 0) GO TO 140 
                   IDEL1 = ICONT - NCOL1(ICOL) 
                   IDEL2 = ICONT - NCOL1(ICOL+1) 
                   IF (IDEL1*IDEL2 .GT. 0) GO TO 140 
                   K = K + 1 
C                   IADD=(DELTX*IDEL1)/(NCOL1(ICOL+1)-NCOL1(ICOL)) 
                      IF( NCOL1(ICOL+1)-NCOL1(ICOL).EQ.0) THEN 
                        IADD = DELTX*IDEL1 
                      ELSE 
                      IADD = (DELTX*IDEL1)/(NCOL1(ICOL+1)-NCOL1(ICOL)) 
                      ENDIF 
                   LX(K) = ICOL * DELTX + IADD 
                   LY(K) = IROW * DELTY 
CC 
CC CHECK FOR A CONTOUR CROSSING ON THE TOP HORIZONTAL LINE 
CC 
 140               CONTINUE 
C                   CHECK FOR SPECIAL CASE OF FIRST LEFT COL TO PLOT 
                   IF (ICOL .NE. 0) GO TO 150 
                      IDEL1 = ICONT - NCOL1(1) 
                      IDEL2 = ICONT - NCOL2(1) 
                      IF (IDEL1*IDEL2 .GT. 0) GO TO 160 
                      K = K+1 
C                      IADD = (DELTY*IDEL1)/(NCOL2(1)-NCOL1(1)) 
                      IF( NCOL2(1)-NCOL1(1).EQ.0) THEN 
                        IADD = DELTY*IDEL1 
                      ELSE 
                        IADD = (DELTY*IDEL1)/(NCOL2(1)-NCOL1(1)) 
                      ENDIF 
                      GO TO 155 
150                CONTINUE 
                   IDEL1 = ICONT - NCOL1(ICOL+1) 
                   IDEL2 = ICONT - NCOL2(ICOL+1) 
                   IF (IDEL1*IDEL2 .GT. 0) GO TO 160 
                   K = K + 1 
          IF (NCOL2(ICOL+1)-NCOL1(ICOL+1).EQ.0) THEN 
             IADD = DELTY*IDEL1 
          ELSE 
                   IADD=(DELTY*IDEL1)/(NCOL2(ICOL+1)-NCOL1(ICOL+1)) 
          ENDIF 
155                CONTINUE 
                   LX(K) = (ICOL+1) * DELTX 
                   LY(K) = (IROW)*DELTY - IADD 
CC 
CC CHECK FOR A CONTOUR CROSSING ON THE RIGHT VERTICAL LINE 
CC 
 160               CONTINUE 
CC CHECK NOT NECESSARY FOR LEFT COLUMN 
                   IF (ICOL .EQ. 0) GO TO 180 
                   IDEL1 = ICONT - NCOL2(ICOL) 
                   IDEL2 = ICONT - NCOL2(ICOL+1) 
                   IF (IDEL1*IDEL2 .GT. 0) GO TO 180 
                   K = K + 1 
C                   IADD=(DELTX*IDEL1)/(NCOL2(ICOL+1)-NCOL2(ICOL)) 
                      IF( NCOL2(ICOL+1)-NCOL2(ICOL).EQ.0) THEN 
                        IADD = DELTX*IDEL1 
                      ELSE 
                      IADD = (DELTX*IDEL1)/(NCOL2(ICOL+1)-NCOL2(ICOL)) 
                      ENDIF 
                   LX(K) = (ICOL)*DELTX+IADD 
                   LY(K) = (IROW-1) * DELTY 
CC 
CC ANY CONTOUR CROSSINGS ? 
CC 
  180              CONTINUE 
                   IF (K .EQ. 2) GO TO 220 
                   IF (K .NE. 4) GO TO 240 
CC 
CC  4 CONTOUR CROSSINGS, KEEP 2 OUT OF THE 4 POSSIBLE CONTOUR LINES 
CC 
                   IDEL1 = (LX(1) - LX(2))**2 + (LY(1) - LY(2))**2 
                   IDEL2 = (LX(1) - LX(4))**2 + (LY(1) - LY(4))**2 
                   IF (IDEL1 .LT. IDEL2) GO TO 200 
                   CALL tkplot (LX(1)+XORG,LY(1)+YORG,3) 
                   CALL tkplot (LX(4)+XORG,LY(4)+YORG,2) 
CC 
CC 
                  CALL tkplot (LX(2)+XORG,LY(2)+YORG,3) 
                  CALL tkplot (LX(3)+XORG,LY(3)+YORG,2) 
                  GO TO 230 
CC 
CC 
 200              CONTINUE 
                  CALL tkplot (LX(3)+XORG,LY(3)+YORG,3) 
                  CALL tkplot (LX(4)+XORG,LY(4)+YORG,2) 
CC 
CC 2 CONTOUR CROSSINGS, KEEP THE CONTOUR LINE 
CC 
  220             CONTINUE 
                  CALL tkplot (LX(1)+XORG,LY(1)+YORG,3) 
                  CALL tkplot (LX(2)+XORG,LY(2)+YORG,2) 
CC 
CC CHECK IF THE CONTOUR LINES BUFFER IS FULL 
CC IF FULL, LINK AND WRITE THE CONTOUR LINES 
CC 
 230              CONTINUE 
CC 
CCC ADVANCE TO NEXT ELEVATION IN THE 2 PROFILES 
C 
  240           CONTINUE 
CC 
CC ADVANCE TO NEXT CONTOUR IN THE 2 PROFILES 
CC 
  260           CONTINUE 
CC 
CC NORMAL RETURN 
CC 
       call flsh 
       IER = 0 
       GO TO 90000 
CC 
CC ERROR RETURN 
CC 
80100  CONTINUE 
       IER = -100 
       WRITE (*,98010)  IER 
98010  FORMAT (' *FASTCONT*ZERO-VALUE MINIMUM MAY CAUSE DIVISION BY', 
     1         ' ZERO',/,'  ERROR',I5) 
       GO TO 90000 
CC 
CC RETURN 
90000  CONTINUE 
       RETURN 
       END 
