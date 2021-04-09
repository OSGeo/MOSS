C
C******************************************************************************
C
C       NAME:           SUBROUTINE HELP
C
C       FUNCTION:       THE FOLLOWING ROUTINE PROVIDES GENERAL PURPOSE,
C                       DATA BASE, DISPLAY, ANALYSIS, AND SPATIAL
C                       RETRIEVAL COMMAND INFORMATION FOR THE MOSS USER.
C
C       PARAMETERS:     NAME    USE    TYPE    DIMENSION    RANGE    UNITS
C                       -------------------------------------------------------
C                       IER      I     I*2         1        0,1
C                               THE ERROR CODE FOR THE HELP COMMAND WHERE
C                               0=BAD AND 1=OK.
C                               -----------------------------------------------
C                       ICARD    I     I*2        80        USER
C                               THE USER COMMAND INPUT FROM THE CONSOLE.
C                               -----------------------------------------------
C                       KLL      I     I*2         1        1..15
C                               THE ICARD INDEX WHERE FIRST CHARACTER STARTS
C                               -----------------------------------------------
C
C       FILES:          NONE
C*******************************************************************************
C
C
      SUBROUTINE mhelp(ier,ichrin,kll)
      COMMON /io    / nprnt , ioin
      DIMENSION icard(80) , ihelp(15) , ihelpk(15)
      CHARACTER ichrin*80 , ifarr*18
C
      DO i = 1 , 15
         ihelp(i) = 0
         ihelpk(i) = 0
      ENDDO
      DO l = 1 , 6
         IF ( ichrin(l:l).EQ.' ' ) GOTO 100
         ihelp(l) = ichar(ichrin(l:l))
      ENDDO
C
C        GENERAL HELP OR HELP ON A SPECIFIC COMMAND?
C
 100  CONTINUE
      ib = l - 1
      IF ( ihelp(1).NE.32 ) THEN
C
C
C
C        A MATCH, THE COMMAND INFORMATION FILE IS PRINTED.
C
         DO l = 1 , ib
            ihelpk(l+2) = ihelp(l)
         ENDDO
      ELSE
         ihelpk(3) = 72
         ihelpk(4) = 69
         ihelpk(5) = 76
         ihelp(6) = 80
         ihelp(7) = 0
      ENDIF
 
      ihelpk(1) = 77
      ihelpk(2) = 72
      ifarr = '\gis\help\'
      DO l = 1 , ib + 2
         k = l + 10
         ifarr(k:k) = char(ihelpk(l))
      ENDDO
      CALL tyfil(60,nprnt,ifarr,k)
C
      RETURN
      END
