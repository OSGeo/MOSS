      LOGICAL FUNCTION flin2(ix,iy,npts,atol,i)
$Include:'pqpnts.inc'
      REAL ix , iy
      LOGICAL fpoint
C
C **********************************************************
C
C        L O G I C A L   F U N C T I O N   F L I N 2
C
C FUNCTION: DETERMINE IF THE POINT IX,IY IS WITH ATOL
C           UNITS OF THE LINE STORED IN X AND Y
C INPUT PARAMETERS:
C      (IX,IY)  = POINT TO TEST
C      NPTS     = NUMBER OF X,Y COORDINATES
C      ATOL     = SEARCH TOLERENCE
C      I        = LINE SEGMENT NUMBER, IF POINT (IX,YI) WITHIN
C                 ATOL UNITS OF THIS LINE SEGMENT.
C ROUTINES
C CALLED:    P,Q,POISE,FPOINT.
C *******************************************************
C
      flin2 = .FALSE.
      n = npts - 1
      DO i = 1 , n
         CALL spoints('P ',p,x1,i,1)
         CALL spoints('Q ',q,y1,i,1)
         CALL spoints('P ',p,x2,i+1,1)
         CALL spoints('Q ',q,y2,i+1,1)
c          X1=P(I)
c          Y1=Q(I)
c          X2=P(I+1)
c          Y2=Q(I+1)
         CALL poise(ix,iy,x1,y1,x2,y2,dsq)
C
C  NOW DETERMINE IF POINT WITHIN ATOL UNITS OF THIS LINE SEGMENT
C
         IF ( dsq.LE.atol ) THEN
            flin2 = .TRUE.
C
            RETURN
         ENDIF
      ENDDO
      RETURN
      END
