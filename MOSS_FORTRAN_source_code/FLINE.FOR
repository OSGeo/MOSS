      LOGICAL FUNCTION fline(ix,iy,npts,atol,i)
      REAL ix , iy
$Include:'xypnts.inc'
C
C **********************************************************
C
C        L O G I C A L   F U N C T I O N   F L I N E
C
C FUNCTION: DETERMINE IF THE POINT IX,IY IS WITHin ATOL
C           UNITS OF THE LINE STORED IN X AND Y
C INPUT PARAMETERS:
C      (IX,IY)  = POINT TO TEST
C      NPTS     = NUMBER OF X,Y COORDINATES
C      ATOL     = SEARCH TOLERENCE
C      I        = LINE SEGMENT NUMBER, IF POINT (IX,YI) WITHIN
C                 ATOL UNITS OF THIS LINE SEGMENT.
C *******************************************************
C
      fline = .FALSE.
      n = npts - 1
      DO i = 1 , n
         CALL spoints('X ',x,x1,i,1)
         CALL spoints('Y ',y,y1,i,1)
         CALL spoints('X ',x,x2,i+1,1)
         CALL spoints('Y ',y,y2,i+1,1)
c          X1=X(I)
c          Y1=Y(I)
c          X2=X(I+1)
c          Y2=Y(I+1)
         CALL poise(ix,iy,x1,y1,x2,y2,dsq)
C
C  NOW DETERMINE IF POINT WITHIN ATOL UNITS OF THIS LINE SEGMENT
C
         IF ( dsq.LE.atol ) THEN
            fline = .TRUE.
            RETURN
         ENDIF
      ENDDO
C
      RETURN
      END
