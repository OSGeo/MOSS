      SUBROUTINE symdrw(xcirc,ycirc,ndim,xcen,ycen)
C
C *****************************************************************************
C
C             S U B R O U T I N E     S Y M D R W
C FUNCTION:
C             THIS ROUTINE DRAWS A CIRCLE ABOUT POINT XCE, YCEN
C PARAMETERS:
C     XCIRC  = X COORDINATE STRING OF CIRCLE
C     YCIRC  = Y COORDINATE STRING OF CIRCLE
C     NDIM   = DIMENSION OF XCIRC AND YCIRC
C     XCEN   = X COORDINATE OF CIRCLE CENTER
C     YCEN   = Y COORDINATE OF CIRCLE CENTER
C ROUTINES
C CALLED:     DRAWA,MOVEA.
C *****************************************************************************
C
C      COMMON /IO/ NPRNT,IOIN
      DIMENSION xcirc(ndim) , ycirc(ndim)
      CALL movea(xcirc(1)+xcen,ycirc(1)+ycen)
C
      DO i = 2 , ndim
         CALL drawa(xcirc(i)+xcen,ycirc(i)+ycen)
      ENDDO
C
      RETURN
      END
