      SUBROUTINE igridp(icol,irow,ival)
      COMMON /io    / nprnt , ioin
      COMMON /gridf / lugf , len
      COMMON /rundata/ xmin , ymin , dx , dy , ncols , nrows , nosave , 
     &                 itemid , idisp , ictsub
      INTEGER itnam1(4)
      DIMENSION ival(2)
      LOGICAL nosave
      DATA itnam1/'TE' , 'MP' , '01' , 0/
C
      IF ( ival(1).LT.0 .AND. icol.EQ.0 .AND. irow.EQ.0 ) THEN
C
C  INITIALIZE
C
         IF ( ival(1).EQ.-2 ) THEN
C
C  FINISH
C
            CALL dscls(lugf,ier)
         ELSE
            lugf = 31
            len = ival(2)
            CALL dsopn(lugf,itnam1,3,3,1,len,128,ier)
            IF ( ier.NE.0 ) THEN
C
               CALL anmode
               WRITE (nprnt,99001) (itnam1(i),i=1,4) , ier
99001          FORMAT (' *IGRIDP*  ERROR FROM DSOPN OF FILE',4A2,
     &                 '  ERROR',I5)
               STOP
            ENDIF
         ENDIF
C
C  WRITE
C
      ELSEIF ( irow.LT.0 .OR. icol.LT.0 ) THEN
C
C  ERROR RETURNS
C
         CALL anmode
         WRITE (nprnt,99002) irow , icol
99002    FORMAT (' *IGRIDP*  ROW OR COLUMN LESS THAN ZERO ROW,COL',2I5)
         STOP
      ELSE
C        CELL =  (FLOAT(IROW-1) * FLOAT(NCOLS)) + ICOL
         cell = ((irow-1)*ncols) + icol
         irecl = 2*len
         CALL dsout(lugf,ival,cell,ier)
         IF ( ier.NE.0 ) THEN
C
            CALL anmode
            WRITE (nprnt,99003) irow , icol , ier
99003       FORMAT (' *IGRIDP*  ERROR FROM DSOUT ON ROW,COL',2I5,
     &              '  ERROR',I5)
            STOP
         ENDIF
      ENDIF
C
      RETURN
      END
