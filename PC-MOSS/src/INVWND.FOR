      FUNCTION invwnd(xpmin,xpmax,ypmin,ypmax,xwmin,xwmax,ywmin,ywmax)
C
C **************************************************************
C
C            I N T E G E R    F U N C T I O N     I N V W N D
C FUNCTION:
C         POLYGON P IS CONTAINED IN THE MININUM
C         BOUNDING RECTANGLE DEFINED BY XPMIN,XPMAX,YPMIN,YPMAX
C         INSIDE THE VIEWING RECTANGLE DEFINED BY XWMIN,XWMAX,
C         YWMIN,YWMAX.  INVWND IS RETURNED AS 1
C         IF THE POLYGON OR THE LINE IS INSIDE
C         THE VIEWING RECTANGLE AND AS 0 IF THE POLYGON OR THE
C         LINE IS OUTSIDE THE VIEWING RECTANGLE.
C PARAMETERS:
C             XPMIN,YPMIN,XPMAX,YPMAX IN  - MININUM BOUNDING
C                                           RECTANGLE VALUES.
C             XMIN,YMIN,XMAX,YMAX     IN  - VIEWING RECTANGLE
C                                           VALUES.
C             INVWND    OUT - 1, IF POLYGON OR LINE IS INSIDE,
C                             0, IF POLYGON OR LINE IS OUTSIDE.
C ROUTINES
C CALLED:     NONE.
C ****************************************************
C
      invwnd = 0
      IF ( xpmin.GT.xwmax ) RETURN
      IF ( xpmax.LT.xwmin ) RETURN
      IF ( ypmin.GT.ywmax ) RETURN
      IF ( ypmax.LT.ywmin ) RETURN
C
      invwnd = 1
C
      RETURN
      END
