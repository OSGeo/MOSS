 
 
      SUBROUTINE xxx(xc,yc)
C
C  DRAW A X AT A SPECIFIED VIRTUAL SCREEN LOCATION
C
      CALL movea(xc,yc)
      CALL movrel(-4,-8)
      CALL drwrel(8,16)
      CALL movrel(-8,0)
      CALL drwrel(8,-16)
C
      RETURN
      END
