C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C                S U B R O U T I N E   C S D E S C R
C
C  FUNCTION:  CALCULATE DESCRIPTIVE STATISTICS FOR A RASTER MAP
C             (DISCRETE OR MVG)
C
C  INPUT PARAMETERS:
C
C             LINES = CURSOR CONTROL
C             IDD   = ACTIVE MAP I.D.
C             ITYPE = DATA TYPE
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      SUBROUTINE sdesc(lines,idd)
      COMMON /io    / nprnt , ioin
      INTEGER ihead(256) , map(15) , ivalu(4)
      INTEGER*4 iv4
      REAL rvalu(2) , crack
      EQUIVALENCE (ivalu(1),rvalu(1))
      DATA iblnk/'  '/
      ichan = 5
C
C  CREATE PATHNAME TO CELL HEADER FILE
C
      CALL gtnam(idd,5,iblnk,map,ier)
      CALL dsopn(ichan,map,1,1,1,256,256,ier)
      IF ( ier.EQ.0 ) THEN
C
C  PRINT BANNER
C
         CALL gtnam(idd,0,iblnk,map,ier)
         WRITE (nprnt,99001) (map(i),i=1,5)
99001    FORMAT (' DESCRIPTIVE STATISTICS FOR RASTER MAP ',5A2)
C
C  READ HEADER AND EXTRACT PERTINENT INFO
C
         CALL dsin(ichan,ihead,1.0,ier)
         IF ( ier.NE.0 ) GOTO 100
         CALL dscls(ichan,ier)
         levels = ihead(67)
         nrows = ihead(60)
         ncols = ihead(61)
         total = float(nrows)*float(ncols)
         CALL dscls(ichan,ier)
C
C  OPEN THE VALUE COUNT FILE
C
         CALL gtnam(idd,11,iblnk,map,ier)
         CALL dsopn(ichan,map,1,1,1,4,128,ier)
         IF ( ier.EQ.0 ) THEN
C
C  INITIALIZE FOR LOOP THROUGH SUBJECTS
C
            xmin = 9999999.
            xmax = -(xmin)
            tfreq = 0.0
            crack = 0.0
            irecl = 8
C
            DO i = 1 , levels
               cell = i
               CALL dsin(ichan,ivalu,cell,ier)
               IF ( ier.NE.0 ) GOTO 100
               tfreq = tfreq + rvalu(2)
               crack = crack + (rvalu(1)*rvalu(2))
               xmin = amin1(xmin,rvalu(1))
               xmax = amax1(xmax,rvalu(1))
            ENDDO
C
            CALL dscls(ichan,ier)
            range = xmax - xmin
            xmean = crack/total
            ymean = crack/tfreq
C
C  PRINT OUT STATISTICS
C
            WRITE (nprnt,99002) total
99002       FORMAT (' TOTAL NUMBER OF CELLS IN MAP     = ',F13.4)
            WRITE (nprnt,99003) tfreq
99003       FORMAT (' TOTAL NON-ZERO CELLS IN MAP      = ',F13.4)
            WRITE (nprnt,99004) xmin
99004       FORMAT (' MININUM CELL VALUE IN MAP        = ',F13.4)
            WRITE (nprnt,99005) xmax
99005       FORMAT (' MAXINUM CELL VALUE IN MAP        = ',F13.4)
            WRITE (nprnt,99006) range
99006       FORMAT (' RANGE OF VALUES IN MAP           = ',F13.4)
            WRITE (nprnt,99007) xmean
99007       FORMAT (' AVERAGE VALUE FOR MAP (WITH 0 S) = ',F13.4)
            WRITE (nprnt,99008) ymean
99008       FORMAT (' AVERAGE VALUE FOR MAP (NO 0 S)   = ',F13.4)
            lines = lines + 8
            GOTO 200
         ENDIF
      ENDIF
C
C  ERRORS
C
      WRITE (*,99009) (map(i),i=1,10) , ier
99009 FORMAT (' *SDESC*   ERROR FROM OPEN OF',10A2,'  ERROR',I5)
      lines = lines + 2
      GOTO 200
C
 100  CONTINUE
      WRITE (*,99010) ier
99010 FORMAT (' *SDESC*  ERROR FROM DSIN  ERROR',I5)
      lines = lines + 2
C
C  JUMP OUT
C
 200  CONTINUE
      RETURN
      END
