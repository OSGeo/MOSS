      FUNCTION yi(i)
C
C     Given the numeric order of the Y-coordinate of a grid point,
C     calculates the actual y-coordinate. Based on computation of
C     YCELL in MOSS GRID as of September 1985.
C     George Heine, BLM Denver Service Center, Oct. 1985
C
      INTEGER*4 i
      COMMON /gridpts/ xmin , xside , ymax , yside
      yi = ymax - float(i)*yside + (0.5)*yside
      RETURN
      END
