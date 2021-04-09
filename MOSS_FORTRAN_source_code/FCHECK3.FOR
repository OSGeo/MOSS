C******************************************************************************
C
C     NAME:  LOGICAL FUNCTION FCHECK3
C
C     FUNCTION:  THIS ROUTINE CHECKS FOR THE SPECIAL CASE OF AN
C                ISOLATED TARGET CELL.  IF THE CELL IS ISOLATED, THE
C                ROUTINE WILL BE TRUE.  IF NOT, THE ROUTINE WILL
C                RETURN FALSE.  THIS ROUTINE ASSUMES THAT THE ROVING
C                WINDOW WILL EXPAND UNTIL AT LEAST 2 POINTS ARE
C                CONTAINED.  HENCE, IF NO FAULTS ARE IN THE ROVING
C                WINDOW THE TARGET CELL WILL NEVER BE ISOLATED.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  IER       O      INTEGER                +/-
C                  RETURN ERROR CODE (NO ERROR = 1)
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  NONE
C
C     CALLING ROUTINES:   FAULT
C******************************************************************************
C
      LOGICAL FUNCTION fcheck3(ier)
C
      COMMON /reltn / jeltn(50,4) , npoint(64) , nflag , npts
C
      ier = 0
      fcheck3 = .TRUE.
      DO i = 1 , npts
         IF ( jeltn(i,2).NE.2 ) THEN
            fcheck3 = .FALSE.
            RETURN
         ENDIF
      ENDDO
C
      RETURN
      END
