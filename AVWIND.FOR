      SUBROUTINE avwind(xorgin,xlen,yorgin,ylen)
C
C FUNCTION:  SET GRAPHICS WINDOW IN WORLD COORDINATE SYSTEM
C  THIS VERSION FOR TEKTRONIX PLOT10 COMPATIBLE TERMINALS
C
      CALL vwindo(xorgin,xlen,yorgin,ylen)
      CALL scrset(xorgin,xlen,yorgin,ylen)
C
      RETURN
      END
