      SUBROUTINE headisp(ihead,iopt,iflag)
C
C     DISPLAYS HEADER INFORMATION FOR NEW CELL MAP;
C     ASKS USER IF THEY WANT TO RE-ENTER
C
$INCLUDE: 'IO.INC'
C      COMMON/IO/NPRNT,IOIN
      CHARACTER*1 ians
      DOUBLE PRECISION tcells
      DIMENSION ihead(128) , itmp(128) , idesc(30)
      EQUIVALENCE (maxr,itmp(60)) , (maxc,itmp(61)) , (xmin,itmp(68)) , 
     &             (xmax,itmp(70)) , (ymin,itmp(72)) , (ymax,itmp(74)) , 
     &             (unit,itmp(77)) , (size,itmp(78)) , (heit,itmp(80)) , 
     &             (wdth,itmp(82)) , (idesc(1),itmp(20))
      DO i = 1 , 128
         itmp(i) = ihead(i)
      ENDDO
      iflag = 0
      ratio = heit/wdth
      tcells = dble(float(maxr))*dble(float(maxc))
      WRITE (nprnt,99001) idesc
      WRITE (nprnt,99002) size , maxr , heit , maxc , wdth , tcells , 
     &                    ratio
C     WRITE (NPRNT,30) XMIN,XMAX,YMIN,YMAX
      IF ( iopt.EQ.1 ) WRITE (nprnt,99006)
      IF ( iopt.EQ.2 ) WRITE (nprnt,99007)
      IF ( iopt.EQ.3 ) WRITE (nprnt,99008)
      IF ( iopt.EQ.4 ) WRITE (nprnt,99009)
      WRITE (nprnt,99004)
      READ (ioin,99005) ians
C      IANS = IANS/256
      IF ( (ians.EQ.'Y') .OR. (ians.EQ.'y') ) iflag = 1
      RETURN
99001 FORMAT (//,4X,30A2,/)
99002 FORMAT (3X,'ACRES/CELL: ',F10.3,4X,'NO OF ROWS:  ',4X,I6,/,3X,
     &        'CELL HEIGHT: ',F9.3,4X,'NO OF COLS:  ',4X,I6,/,3X,
     &        'CELL WIDTH:  ',F9.3,4X,'TOTAL CELLS: ',F11.0/,3X,
     &        'RATIO Y/X:   ',2X,F7.3)
99003 FORMAT (/,3X,'BOUNDING RECTANGLE:',/,4X,'MIN X: ',F16.3,/,4X,
     &        'MAX X: ',F16.3,/,4X,'MIN Y: ',F16.3,/,4X,'MAX Y: ',F16.3)
99004 FORMAT (/,3X,'DO YOU WISH TO RE-ENTER PARAMETERS [CR=N]',/,' ? '\)
99005 FORMAT (A1)
99006 FORMAT (/,'   GRIDDING OPTION: 4 POINT QUAD WEIGHTED AVERAGE')
99007 FORMAT (/,'   GRIDDING OPTION: 8 NEAREST POINTS WEIGHTED AVERAGE')
99008 FORMAT (/,'   GRIDDING OPTION: KRIGING')
99009 FORMAT (/,'   GRIDDING OPTION: QUINTIC-SPLINE TYPE')
      END
