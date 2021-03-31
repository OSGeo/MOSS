C**************************************************************************** 
C 
C             S U B R O U T I N E   C O N P L T . F R 
C 
C FUNCTION:   PLOTS CONTINUOUS CELL MAPS VIA FAST AND DIRTY CONTOURING 
C 
C****************************************************************************** 
C 
       SUBROUTINE CONPLT 
C 
        COMMON /IO/     NPRNT,IOIN 
        COMMON /WORK/   IBUFF(128),MAP(25),ICOM(10),JCOM(10) 
C        COMMON /DSBUF/  MLEN,IDATA(8192) 
        COMMON /MLOCAL/  NRWCOL(4),NROWS,NCOLS,NBYTES,LEN,ICHAN,INTRVL, 
     +                  WNDW(4) 
C        COMMON /MAILB/  MBX2DO, LTIME 
        INTEGER*2 IROW1(1024),IROW2(1024),INDX1,INDX2 
        INTEGER*4 jes,kes 
        REAL    RROW1(512),RROW2(512),MWND(4) 
c        LOGICAL LTIME,ok 
        EQUIVALENCE (MWND(1),IBUFF(68)), 
     +              (IBUFF(80),YSIZE),(IBUFF(82),XSIZE), 
     +              (IBUFF(88),VMIN ),(IBUFF(90),VMAX ), 
     +              (IROW1(1),RROW1(1)),(IROW2(1),RROW2(1)) 
C 
C.....SET DEFAULTS FOR FIRST-ROW, LAST-ROW, FIRST-COLUMN, LAST-COLUMN ..... 
      NRWCOL(1) = 1 
      NRWCOL(2) = NROWS 
      NRWCOL(3) = 1 
      NRWCOL(4) = NCOLS 
C 
C.....CALCULATE FIRST-ROW, LAST-ROW, FIRST-COLUMN, LAST-COLUMN FOR CLIP ... 
C.....WNDW() IS CURRENT WINDOW AND MWND() IS MAPS MBR 
C.....WNDW(4) AND MWND(4) - YMAXs  [STARTING ROW]  NRWCOL(1) 
C.....WNDW(2) AND MWND(3) - YMINs  [ENDING   ROW]  NRWCOL(2) 
C.....WNDW(1) AND MWND(1) - XMINs  [STARTING COL]  NRWCOL(3) 
C.....WNDW(3) AND MWND(2) - XMAXs  [ENDING   COL]  NRWCOL(4) 
C 
      IF (WNDW(4) .LT. MWND(4)) 
     +    NRWCOL(1) = ANINT((MWND(4)-WNDW(4))/YSIZE + .5) 
      IF (WNDW(2) .GT. MWND(3)) 
     +    NRWCOL(2) = NRWCOL(2) - IFIX((WNDW(2)-MWND(3))/YSIZE) 
      IF (WNDW(1) .GT. MWND(1)) 
     +    NRWCOL(3) = ANINT((WNDW(1)-MWND(1))/XSIZE + .5) 
      IF (WNDW(3) .LT. MWND(2)) 
     +    NRWCOL(4) = NRWCOL(4) - IFIX((MWND(2)-WNDW(3))/XSIZE) 
      IF ((NRWCOL(2) .LT. NRWCOL(1)) .OR. 
     1    (NRWCOL(4) .LT. NRWCOL(3)))  GO TO 80100 
C 
C.....ASSIGN STARTING COLUMN, # OF COLUMNS ...... 
      ICOL = NRWCOL(3) 
      NCOLS = NRWCOL(4) - NRWCOL(3) + 1 
C.....ASSIGN X-ORIGIN, Y-ORIGIN ...... 
      XORG = MWND(1) + (FLOAT(NRWCOL(3) - 1) * XSIZE) 
      YORG = MWND(3) + (FLOAT(NROWS - NRWCOL(2)) * YSIZE) 
C 
C.....INITIALIZE BUFFER INDICES ...... 
      INDX1 = 1 
      INDX2 = 2 
C.....INPUT STARTING ROW OF CELL-VALUES ...... 
C      IF (.NOT. OK)  GO TO 80600 
      recn = NRWCOL(1) 
c 
      kes=(nbytes/(2*len))
      jes=(nrwcol(1)-1)*kes
      recn=jes
      do 113 il=1,(nbytes/len),2
      recn=recn+1
      CALL DSIN(ICHAN,IROW1(il),recn,IER) 
      IF(IER .NE. 0) GO TO 80600 
 113  continue
C 
C.....MAP REALS TO INTEGER IF REQUIRED ..... 
      IF (LEN .EQ. 1) GO TO 440 
        ZMIN = AINT(VMIN/FLOAT(INTRVL))*FLOAT(INTRVL) 
        FACT = 10000. / (VMAX - ZMIN) 
        IF (LEN .EQ. 2) VMIN = 0. 
        IF (LEN .EQ. 2) VMAX = 10000. 
        IF (LEN .EQ. 2) INTRVL = INT(FLOAT(INTRVL)*FACT) 
        IF (INTRVL .LT. 1) INTRVL = 1
        jj=0
        DO 420 J=NRWCOL(3),NRWCOL(4) 
        jj=jj+1
          IROW1(jj) = INT( (RROW1(J)-ZMIN) * FACT ) 
  420   CONTINUE 
  440 CONTINUE 
C 
C.....PROCESS EACH ROW IN WINDOW ...... 
      nnrow=(nrwcol(2)-nrwcol(1))+1
      i=nrwcol(1)-1
      DO 580  Ii=1,nnrow
      i=i+1
C.......INPUT NEXT ROW OF CELL-VALUES ...... 
      kes=(nbytes/(2*len))
      jes=(i-1)*kes
      recn=jes
      do 114 il=1,(nbytes/len),2
      recn=recn+1
      IF(MOD(Ii,2) .NE. 0) THEN 
            CALL DSIN(ICHAN,IROW2(il),recn,IER) 
       ELSE 
            CALL DSIN(ICHAN,IROW1(il),recn,IER) 
      ENDIF 
        IF(IER .NE. 0) GO TO 80600 
 114  continue     
C 
C.......MAP REALS TO INTEGER IF REQUIRED ..... 
        IF (LEN .EQ. 1) GO TO 540 
        jj=0
          DO 520 J=NRWCOL(3),NRWCOL(4) 
        jj=jj+1
         IF(MOD(iI,2) .NE. 0) THEN 
                IROW2(JJ) = INT( (RROW2(J)-ZMIN) * FACT ) 
           ELSE 
                IROW1(JJ) = INT( (RROW1(J)-ZMIN) * FACT ) 
         ENDIF 
  520     CONTINUE 
  540   CONTINUE 
C 
C.......PERFORM CONTOUR FOR CURRENT TWO ROWS ...... 
        NROW = NRWCOL(2) - I + 1 
        IF ( INDX1.EQ.1) THEN 
        CALL FASTCONT (IROW1,IROW2,NCOLS, 
     1              VMIN,VMAX,XSIZE,YSIZE,XORG,YORG,INTRVL,NROW,IER) 
        ELSE 
        CALL FASTCONT (IROW2,IROW1,NCOLS, 
     1              VMIN,VMAX,XSIZE,YSIZE,XORG,YORG,INTRVL,NROW,IER) 
       ENDIF 
        IF (IER .NE. 0)  GO TO 80300 
C.......FLIP INDEX VALUES ...... 
        INDX1 = MOD(INDX1,2) + 1 
        INDX2 = MOD(INDX2,2) + 1 
  580 CONTINUE 
      GO TO 90000 
C 
C.....ERROR TRAPS 
C 
80100 CONTINUE 
      WRITE (NPRNT,98101) 
98101 FORMAT (' *CONPLT* ERROR WITH WINDOW') 
      GO TO 90000 
C 
80300 CONTINUE 
      WRITE (NPRNT,98301) IER 
98301 FORMAT (' *CONPLT* ERROR FASTCONT ERROR ',I5) 
      GO TO 90000 
C 
80600 CONTINUE 
C      CALL GETERR(IER) 
      WRITE (NPRNT,98601) IER 
98601 FORMAT (' *CONPLT* ERROR FROM in  ERROR',I5) 
      GO TO 90000 
C 
C.....LET MAIN KNOW ALL IS DONE AND COMMIT SUICIDE INSTEAD OF BEING KILLED 
90000 CONTINUE 
      END 
