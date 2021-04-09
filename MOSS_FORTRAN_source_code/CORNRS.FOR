C******************************************************************************
C
C  Name:    C O R N R S
C
C  Purpose: COMPUTES CORNER COORDS OF A RECTANGULAR BOX OF SQUARE CHARS
C           KNOWING A REF POINT TO LEFT AND TO RIGHT TO ALIGN BOX WITH,
C           AND CENTER POINT TO START WITH; PUTS COORDS INTO 'TEMPBX'.
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     NCHAR             In            Integer - Number of characters in label
C     HGT               In            Real    - Height of characters
C     XL                In            Real    - 'Left' X value
C     YL                In            Real    - 'Left' Y value
C     XR                In            Real    - 'Right' X value
C     YR                In            Real    - 'Right' Y value
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C
C  -- DEBUG --
C     IDEB                            Integer
C
C  -- LOCAL --
C     CENTRX                          Real
C     CENTRY                          Real
C     THETA                           Real
C     TEMPBX                          Real Array            16
C
C  -- Stack Variables --
C     COND1                           Logical
C     COND2                           Logical
C     COND3                           Logical
C     IPT                             Integer
C     I                               Integer
C     IER                             Integer
C     XDIFF1                          Real
C     XDIFF2                          Real
C     YDIFF1                          Real
C     YDIFF2                          Real
C
C  -- External Subprograms --
C     MOVEA          DRAWA          Qanmode
C
C  Files:
C       Unit    File name or description
C       ------  -------------------------------------------------------
C        0      Graphic device or @Console
C       10      "@CONSOLE" for output
C
C  Calling:          LIMIT
C
C******************************************************************************
C
      SUBROUTINE cornrs(nchar,hgt,xl,yl,xr,yr)
C
      COMMON /io    / nprnt , ioin
      COMMON /local / centrx , centry , theta , tempbx(8)
C
      LOGICAL cond1 , cond2 , cond3
C
      ier = 0
C
C.....DECIDE ON BOX'S ORIENTATION
      cond1 = ((xl.LT.xr) .AND. (yr.LE.yl))
      cond2 = ((xr.LT.xl) .AND. (yr.GE.yl))
      cond3 = (abs(xr-xl).LT.1.E-05)
C
C.....FIND ANGLE FROM HORIZONTAL (UNLESS CLOSE TO VERTICAL IN WHICH
C.....CASE - ASSUME VERTICAL) AND USE TO COMPUTE OFFSETS TO CORNER
C.....COORDS
C
      xdiff1 = float(nchar)*(hgt/2.)*cos(theta)
      xdiff2 = (hgt/2.)*sin(theta)
      ydiff1 = float(nchar)*(hgt/2.)*sin(theta)
      ydiff2 = (hgt/2.)*cos(theta)
C
C.....IF BOX HAS "GRAPHICAL" RIGHT END HIGHER THAN LEFT CONTINUE
C.....OTHERWISE FIGURE SW CORNER DIFFERENTLY
C
      IF ( .NOT.(cond3) ) THEN
C
         IF ( (cond1) .OR. (cond2) ) THEN
C
C.....THIS FIGURES CORNERS FOR A BOX 'FALLING' DOWN TO RIGHT
C
            tempbx(1) = centrx - xdiff1 - xdiff2
            tempbx(2) = centry + ydiff1 - ydiff2
            tempbx(3) = centrx - xdiff1 + xdiff2
            tempbx(4) = centry + ydiff1 + ydiff2
            tempbx(5) = centrx + xdiff1 + xdiff2
            tempbx(6) = centry - ydiff1 + ydiff2
            tempbx(7) = centrx + xdiff1 - xdiff2
            tempbx(8) = centry - ydiff1 - ydiff2
            ipt = 250
            GOTO 100
         ENDIF
      ENDIF
C
      tempbx(1) = centrx - xdiff1 + xdiff2
      tempbx(2) = centry - ydiff1 - ydiff2
      tempbx(3) = centrx - xdiff1 - xdiff2
      tempbx(4) = centry - ydiff1 + ydiff2
      tempbx(5) = centrx + xdiff1 - xdiff2
      tempbx(6) = centry + ydiff1 + ydiff2
      tempbx(7) = centrx + xdiff1 + xdiff2
      tempbx(8) = centry + ydiff1 - ydiff2
C
      ipt = 150
cssk  can't ever execute following code so i commented it out
cssk        CALL MOVEA ( TEMPBX(1), TEMPBX(2) )
cssk        CALL DRAWA ( TEMPBX(3), TEMPBX(4) )
cssk        CALL DRAWA ( TEMPBX(5), TEMPBX(6) )
cssk        CALL DRAWA ( TEMPBX(7), TEMPBX(8) )
cssk        CALL DRAWA ( TEMPBX(1), TEMPBX(2) )
cssk  end sor ssk commenting
C        CALL anmode
C
 100  CONTINUE
      RETURN
      END
