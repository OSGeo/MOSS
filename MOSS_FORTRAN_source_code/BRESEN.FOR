C**********************************************************************
C
C     SUBROUTINE BRESEN
C
C     FUNCTION:
C
C     THIS ALGORITHM FINDS THE CELLS LYING ALONG A LINE BETWEEN TWO PTS.
C
C     REFERENCES:
C
C     BRESENHAM'S ALGORITHM
C     FROM 'FUNDAMENTALS OF COMPUTER GRAPHICS' - FOLEY & VAN DAM
C     PAGES 433-436
C
C     PARAMETER      USE      DESCRIPTION
C     ---------      ---      -----------
C       IX1         INPUT      X VALUE FOR FIRST POINT
C       IY1         INPUT      Y VALUE FOR FIRST POINT
C       IX2         INPUT      X VALUE FOR SECOND POINT
C       IY2         INPUT      Y VALUE FOR SECOND POINT
C       IDX         INPUT      NUMBER OF CELLS BETWEEN IX1 AND IX2
C       IDY         INPUT      NUMBER OF CELLS BETWEEN IY1 AND IY2
C       XSIDE       INPUT      CELL WIDTH
C       YSIDE       INPUT      CELL HEIGHT
C       XMIN        INPUT      MINIMUM BOUNDING X
C       YMIN        INPUT      MINIMUM BOUNDING Y
C       NROWS       INPUT      NUMBER OF ROWS IN MAP
C       NCOLS       INPUT      NUMBER OF COLUMNS IN MAP
C       ICHAN       INPUT      CHANNEL NUMBER OF SOURCE MAP
C       VRTCL       INPUT      LOGICAL FLAG FOR INPUT ORIENTATION
C       LEN         INPUT      FLAG INDICATING DATA TYPE (0=INT,1=REAL)
C       IER         OUTPUT     ERROR INDICATOR
C
C     CALLED BY:  PROFIT
C
C**********************************************************************
      SUBROUTINE bresen(ix1,iy1,ix2,iy2,idx,idy,xside,yside,xmin,ymin,
     &                  nrows,ncols,ichan,vrtcl,len,ier)
      COMMON /pvalss/ xdata(512) , ydata(512) , zdata(512) , nvals , 
     &                mxvals
      COMMON /io    / nprnt , ioin
C
      INTEGER ival(2) , id , idy , idx , incr1 , incr2 , iyinc , ixinc , 
     &        nvals , mxvals
      INTEGER ix , iy , ix1 , iy1 , ix2 , iy2 , len , ichan , nrows , 
     &        ncols , ixend , nprnt
      INTEGER irow , icol , itp , ier
      INTEGER*4 iv4 , ivx(3)
      LOGICAL vrtcl
      REAL*4 valxside , yside , xmin , ymin , cell , xdata , ydata , 
     &       zdata
      EQUIVALENCE (ival(1),val)
C
C.....SET INITIAL VALUE FOR ID ......
C
      id = 2*idy - idx
C
C.....SET CONSTANT USED FOR INCREMENT IF ID < 0 ......
C
      incr1 = 2*idy
C
C.....SET CONSTANT USED FOR INCREMENT IF ID >= 0 ......
C
      incr2 = 2*(idy-idx)
C
C.....INITIALIZE ARRAY COUNTER ......
C
      nvals = 1
C
C.....SET X,Y INCREMENT VALUES ......
C
      iyinc = 1
      IF ( iy1.GT.iy2 ) iyinc = -1
      ixinc = 1
      IF ( ix1.GT.ix2 ) ixinc = -1
C
C.....SET INITIAL X,Y, AND ENDING VALUES ......
C
      ix = ix1
      iy = iy1
      ixend = ix2
C
C.....BEGIN CALCS FOR FIRST POINT ON LINE ......
C
      xdata(1) = float(ix)*xside + xmin
      ydata(1) = float(iy)*yside + ymin
C
C.....CONVERT X,Y TO ROW,COL EQUIVALENTS ......
C
      irow = nrows - iy
      icol = ix
      IF ( vrtcl ) THEN
C
C.....SLOPE IS GREATER THAN 1 - RE-DO IROW ......
C
         irow = nrows - ix
         icol = iy
      ENDIF
C
C.....COMPUTE CELL LOCATION IN FILE ......
C
      cell = ((float(irow)-1)*float(ncols)) + float(icol)
C
C.....RETRIEVE CELL VALUE ......
C
      CALL dsin(ichan,ival,cell,ier)
      IF ( ier.EQ.0 ) THEN
C
cskg      IF (LEN .EQ. 1) VAL = FLOAT(IVAL(1))
         zdata(1) = val
C
C.....MAIN LOOP - FIND REMAINING CELLS ON LINE ......
C
 50      CONTINUE
         IF ( ix.EQ.ixend ) GOTO 100
         ix = ix + ixinc
         IF ( id.GE.0 ) THEN
            iy = iy + iyinc
            id = id + incr2
         ELSE
            id = id + incr1
         ENDIF
         nvals = nvals + 1
         IF ( nvals.LE.mxvals ) THEN
            xdata(nvals) = float(ix)*xside + xmin
            ydata(nvals) = float(iy)*yside + ymin
C
C.......CONVERT X,Y TO ROW,COL EQUIVALENTS ......
C
            irow = nrows - iy
            icol = ix
            IF ( vrtcl ) THEN
C
C.......SLOPE IS GREATER THAN 1 - RE-DO IROW ......
C
               irow = nrows - ix
               icol = iy
            ENDIF
C
C.......COMPUTE CELL LOCATION IN FILE ......
C
            cell = ((float(irow)-1)*float(ncols)) + float(icol)
            CALL dsin(ichan,ival,cell,ier)
            IF ( ier.EQ.0 ) THEN
C       IF (LEN .EQ. 1) VAL = FLOAT(IVAL(1))
               zdata(nvals) = val
               GOTO 50
            ENDIF
         ELSE
            nvals = nvals - 1
            GOTO 100
         ENDIF
      ENDIF
C
C.......ERROR SECTION ......
C
      WRITE (nprnt,99001) ier
99001 FORMAT (' *BRESEN* ERROR FROM DSIN  ERROR',I5)
C
 100  CONTINUE
      RETURN
      END
