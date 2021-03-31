 
      SUBROUTINE plotdv(xx,yy,np,idevic)
C
C ***********************************************************
C
C             S U B R O U T I N E    P L O T D V
C FUNCTION:
C             A DEVICE INDEPENDENT GRAPHICS DRIVER FOR VECTOR
C             DISPLAY. THE DEVICE TO BE USED IS CONTAINED IN
C             IDEVIC.
C PARAMETERS:
C             (X,Y)     IN  - THE POINT TO MOVE OR DRAW TO.
C                             D2
C             NP        IN  - THE PEN NUMBER, 2 FOR DOWN, 3 FOR UP.
C                             D4
C             IDEVIC    OUT - THE DEVICE TO BE USED,
C                             1,IF TEKTRONIX CRT,
C                             2,IF CALCOMP PLOTTER,
C                             3,IF VERSATEC PLOTTER.
C                             DUMMY VARIABLE FOR THE CURRENT TIME.
C ROUTINES
C CALLED:    tkPLOT.
C
C ***************************************************************
C
C  CALL DEVICE ROUTINE CALLED 'PLOT'
C
      CALL tkplot(xx,yy,np)
      RETURN
      END
