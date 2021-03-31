C*******************************************************************
C
C                 S U B R O U T I N E    P R O F I T
C
C  FUNCTION: GENERATE A PROFILE LINE
C
C*******************************************************************
C
      SUBROUTINE profit(x1,y1,x2,y2,xside,yside,xmin,ymin,nrows,ncols,
     &                  len,ichan,ier)
C
      COMMON /io    / nprnt , ioin
      COMMON /pvalss/ xdata(512) , ydata(512) , zdata(512) , nvals , 
     &                mxvals
      INTEGER ival(2) , iyflag
      LOGICAL vrtcl
      REAL val
      EQUIVALENCE (ival(1),val)
C
C.....CHANGE INPUT POINTS TO ROW AND COLUMNS .....
      ix1 = aint((x1-xmin)/xside)
      iy1 = aint((y1-ymin)/yside)
      ix2 = aint((x2-xmin)/xside)
      iy2 = aint((y2-ymin)/yside)
C
      idx = abs(ix2-ix1)
      idy = abs(iy2-iy1)
      IF ( idx.LT.idy ) THEN
C
C......SLOPE IS GREATER THAN 1 ......
C......NOTE: X,Y PARAMETERS ARE PASSED IN X,Y ORDER ......
C
         vrtcl = .TRUE.
         CALL bresen(iy1,ix1,iy2,ix2,idy,idx,yside,xside,ymin,xmin,
     &               nrows,ncols,ichan,vrtcl,len,ier)
C
C......FLIP-FLOP XDATA,YDATA ARRAYS ......
C
C
C......NORMAL RETURN .....
C
         CALL proflip
      ELSE
C
C.....SLOPE IS LESS THAN 1 ......
C
C.....NOTE: X,Y PARAMETERS ARE PASSED IN Y,X ORDER ......
         vrtcl = .FALSE.
         CALL bresen(ix1,iy1,ix2,iy2,idx,idy,xside,yside,xmin,ymin,
     &               nrows,ncols,ichan,vrtcl,len,ier)
      ENDIF
C
C......ERROR RETURNS .....
C
cssk       WRITE(*,98000)
cssk       IER = 2
99001 FORMAT (' *PROFIT* NEAR ZERO LENGTH VECTOR SPECIFIED')
C
cssk       WRITE(*,98002) MXVALS
99002 FORMAT (' *PROFIT* WARNING - TOO MANY CELLS ALONG VECTOR - ',
     &        'MAXIMUM IS',I5)
C
cssk 85040  WRITE(*,98040) IER
99003 FORMAT (' *PROFIT* ERROR FROM DSIN  ERROR',I5)
C
C......NORMAL RETURN ......
C
      RETURN
      END
