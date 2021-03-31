C******************************************************************************
C
C     NAME:  SUBROUTINE FAULT
C
C     FUNCTION:  THIS ROUTINE PROCESS FAULT LINES.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  TARGET    I      REAL          (4)
C                  ARRAY OF THE COORDINATES OF THE TARGET CELL
C                  -------------------------------------------------
C                  XCELL     I      REAL
C                  THE X COORDINATE OF THE CENTER OF THE TARGET
C                  -------------------------------------------------
C                  YCELL     I      REAL
C                  THE Y COORDINATE OF THE CENTER OF THE TARGET
C                  -------------------------------------------------
C                  RVMBR     I     REAL ARRAY       (4)
C                  YMIN,YMAX,XMIN,XMAX OF THE MBR FOR ROVING WINDOW
C                  -------------------------------------------------
C                  ALONE     O      BOOLEAN
C                  TELLS IF TARGET CELL IS ISOLATED
C                  -------------------------------------------------
C                  IER       O      INTEGER
C                  ERROR INDICATOR (NORMAL = 1)
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  CHKFLT,  FCHECK2, BLOCK,  FCHECK3
C
C     CALLING ROUTINES:   GRIDMK
C
C******************************************************************************
C
      SUBROUTINE fault(target,xcell,ycell,rvmbr,alone,ier)
C
      COMMON /fltdat/ fmapid , ichf , istrt , istp , ntot , ifdat(2,64)
      COMMON /reltn / jeltn(50,4) , npoint(64) , nflag , npts
C
      INTEGER fmapid
      REAL rvmbr(4)
      LOGICAL alone , fcheck3
C
      txcell = xcell
      tycell = ycell
      alone = .FALSE.
C
C.....INITIALIZE THE ARRAY EVEN IF THERE AREN'T ANY FAULTS
      DO i = 1 , npts
         jeltn(i,1) = 1
         jeltn(i,2) = 1
         jeltn(i,3) = 0
         jeltn(i,4) = 0
      ENDDO
C
C.....DETERMINE THE CANDIDATE FAULTS FOR THIS ROVING WINDOW.
C..... FIND THE RELATIONSHIP BETWEEN THE CANDIDATE POINTS AND THE
C..... CANDIDATE FAULTS.
      CALL chkflt(rvmbr,target,xcell,ycell,txcell,tycell,ier)
      IF ( ier.NE.0 ) THEN
C
C.....ERROR RETURNS
C
         WRITE (*,99001) ier , istar , istop
99001    FORMAT (' *FAULT* ERROR FROM CHKFLT - ERROR ',I6,
     &           ' ISTAR & OP = ',2I5)
C
cssk 9005 WRITE(*,9006) IER
99002    FORMAT (' *FAULT* ERROR FROM FCHECK - ERROR ',I6)
      ELSEIF ( nflag.NE.0 ) THEN
C
C.......CHECK TO SEE IF ANY DATA POINTS LIE ON FAULT LINES
         CALL fcheck2
C
C.......NOW CHECK TO SEE IF ANY POINT THAT LIES ACROSS 1 FAULT LINE IS
C....... BLOCKED BECAUSE THE FAULT IS CLOSED OFF OR THE ANGLES ARE LESS THAN
C....... 60 DEGREES.
         CALL block(txcell,tycell,ier)
         IF ( ier.NE.0 ) THEN
C
            WRITE (*,99003) ier
99003       FORMAT (' *FAULT* ERROR FROM BLOCK - ERROR ',I6)
         ELSE
C
C.......CHECK FOR AN ISOLATED TARGET CELL.
            alone = fcheck3(ier)
         ENDIF
      ENDIF
C
      RETURN
      END
