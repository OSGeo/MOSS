      SUBROUTINE selelp(iin,iout,iflg,type,ier)
C
C  ALLOW THE SELECTION OF A SPECIFIC ELLIPSE TYPE.
C
C  IIN    -  I/     INT - CONSOLE INPUT CHANNEL
C  IOUT   -  I/     INT - CONSOLE OUTPUT CHANNEL
C  IFLG   -  I/     INT - +=DON'T PRINT MENU
C  TYPE   -  O/     INT - TYPE OF ELLIPSE
C  IER    -  O/     INT - 1=NO ERROR
C
C   0.. CLARKE 1866  (DEFAULT)         10.. MODIFIED EVEREST
C   1.. CLARKE 1880                    11.. MODIFIED AIRY
C   2.. BESSEL                         12.. WALBECK
C   3.. NEW INTERNATIONAL 1967         13.. SOUTHEAST ASIA
C   4.. INTERNATIONAL 1909             14.. AUSTRAILIAN NATIONAL
C   5.. WGS 72                         15.. KRASOVSKY
C   6.. EVEREST                        16.. HOUGH
C   7.. WGS 66                         17.. MERCURY 1960
C   8.. GRS 1980                       18.. MODIFIED MERCURY 1968
C   9.. AIRY                           19.. SPHERE OF RADIUS 6370977 M
C
C
C  REFERENCES: ROUTINE SPHD$0 FROM USGS PROJECTION PACKAGE
C
C
      INTEGER type , ier , iin , iout
C
 100  CONTINUE
      IF ( iflg.LT.0 ) WRITE (iout,99001)
99001 FORMAT (/,16X,'ELLIPSOID MENU',//,/,
     &        ' 0 CLARKE 1866 (default)   10 MODIFIED EVEREST',/,
     &        ' 1 CLARKE 1880             11 MODIFIED AIRY',/,
     &        ' 2 BESSEL                  12 WALBECK',/,
     &        ' 3 NEW INTERNATIONAL 1967  13 SOUTHEAST ASIA',/,
     &        ' 4 INTERNATIONAL 1909      14 AUSTRAILIAN NATIONAL',/,
     &        ' 5 WGS 72                  15 KRASOVSKY',/,
     &        ' 6 EVEREST                 16 HOUGH',/,
     &        ' 7 WGS 66                  17 MERCURY 1960',/,
     &        ' 8 GRS 1980                18 MODIFIED MERCURY 1968',/,
     &        ' 9 AIRY                    19 SPHERE OF RADIUS 6370997 M'
     &        )
C
      WRITE (iout,99002)
99002 FORMAT (/,' ELLIPSOID(0-19) '\)
C
      CALL uiin(iin,iout,type,ic)
      IF ( ic.EQ.0 ) type = 0
      IF ( type.LT.0 .OR. type.GT.19 ) GOTO 100
C
      RETURN
      END
