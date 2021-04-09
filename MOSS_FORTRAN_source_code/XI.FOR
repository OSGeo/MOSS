      FUNCTION xi(i)
C
C     Given the numeric order of the x-coordinate of a grid point,
C     calculates the actual x-coordinate. Based on computation of
C     XCELL in MOSS GRID as of September 1985.
C     George Heine, BLM Denver Service Center, Oct. 1985
C
      INTEGER*4 i
      COMMON /gridpts/ xmin , xside , ymin , yside
      xi = xmin + float(i-1)*xside + (0.5)*xside
      RETURN
      END
