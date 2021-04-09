      FUNCTION rgrid(icol,irow)
      COMMON /io    / nprnt , ioin
      COMMON /gridc / ichan , len , ibw
      COMMON /rundata/ xmin , ymin , dx , dy , ncols , nrows , nosave , 
     &                 itemid , idisp , ictsub
      COMMON /cof   / cofs
      LOGICAL nosave
      DIMENSION ival(2)
      REAL val
      EQUIVALENCE (ival(1),val)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C                  S U B R O U T I N E   R G R I D
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      i41 = irow
      i42 = ncols
      i43 = icol
      IF ( irow.EQ.0 .OR. icol.LE.0 ) THEN
C
C  ERROR RETURNS
C
         CALL anmode
         WRITE (nprnt,99001) irow , icol
99001    FORMAT (' *RGRID*  ROW OR COLUMN LESS THAN ZERO ROW,COL',2I5)
         STOP
      ELSE
C
C  READ THE SPECIFIED CELL
C
C       CELL =  (FLOAT(IROW-1) * FLOAT(NCOLS)) + ICOL
         cell = ((irow-1)*ncols) + icol
         CALL dsin(ichan,ival,cell,ier)
         IF ( ier.NE.0 ) THEN
C
            CALL anmode
            WRITE (nprnt,99002) irow , icol , ichan , ier
99002       FORMAT (' *RGRID*  ERROR FROM DSIN ON ROW,COL',2I5,
     &              ' & CHANNEL',I5,' ERROR',I5)
            STOP
         ELSE
            IF ( len.EQ.1 ) val = float(ival(1))
            rgrid = val + cofs
C
            RETURN
         ENDIF
      ENDIF
      END
