      SUBROUTINE igridf(icol,irow,ival)
      COMMON /io    / nprnt , ioin
      COMMON /gridf / lugf , len , ibw
      COMMON /rundata/ xmin , ymin , dx , dy , ncols , nrows , nosave , 
     &                 itemid , idisp , ictsub
      DIMENSION ival(1)
C
C  READ
C
      IF ( irow.GT.0 .AND. icol.GT.0 ) THEN
C        CELL =  (FLOAT(IROW-1) * FLOAT(NCOLS)) + ICOL
         cell = ((irow-1)*ncols) + icol
         irecl = len*2
         CALL dsin(lugf,ival,cell,ier)
C        write(*,*) 'igridf: ival=',ival
         IF ( ier.NE.0 ) THEN
C
C  ERROR RETURNS
C
C
            CALL anmode
            WRITE (nprnt,99001) irow , icol , ier
99001       FORMAT (' *IGRIDF*  ERROR FROM DSIN ON ROW,COL',2I5,
     &              '  ERROR',I5)
            STOP
         ENDIF
      ENDIF
C
      RETURN
      END
