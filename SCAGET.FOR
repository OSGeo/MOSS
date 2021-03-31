      SUBROUTINE scaget(ibuff,scale)
C**************************************************************
C   EXTRACTS SCALE VALUE FROM FEATURE HEADER
C**************************************************************
C
      DIMENSION ibuff(128) , ir(2)
      EQUIVALENCE (st,ir(1))
C
      ir(1) = ibuff(127)
      ir(2) = ibuff(128)
      scale = st
      IF ( scale.LE.0 ) scale = 1.0
      RETURN
      END
