      SUBROUTINE elipse(type,eccen,axis,ier)
C
C  GIVEN AN ELIPSE TYPE, RETURN THE ECCENTRICITY SQUARED AS WELL AS THE
C  SEMI-MAJOR AXIS.
C
C  TYPE   -  I/     INT - TYPE OF ELIPSE
C  ECCEN  -  O/     DBLE- ECCENTRICITY SQUARED
C  AXIS   -  O/     DBLE- SEMI MAJOR AXIS
C  IER    -  O/     INT - 1=NO ERROR, -1=TYPE OUT OF RANGE
C
C   0.. CLARKE 1866                    10.. MODIFIED EVEREST
C   1.. CLARKE 1880                    11.. MODIFIED AIRY
C   2.. BESSEL                         12.. WALBECK
C   3.. NEW INTERNATIONAL 1967         13.. SOUTHEAST ASIA
C   4.. INTERNATIONAL 1909             14.. AUSTRAILIAN NATIONAL
C   5.. WGS 72                         15.. KRASOVSKY
C   6.. EVEREST                        16.. HOUGH
C   7.. WGS 66                         17.. MERCURY 1960
C   8.. GRS 1980                       18.. MODIFIED MERCURY 1968
C   9.. AIRY                           19.. SPHERE OF RADIUS 6370997 M
C
C
C
C
      INTEGER type
      DOUBLE PRECISION axis , eccen , major(20) , minor(20) , b
C
      DATA major/6378206.4D0 , 6378249.145D0 , 6377397.155D0 , 
     &     6378157.5D0 , 6378388.0D0 , 6378135.0D0 , 6377276.3452D0 , 
     &     6378145.0D0 , 6378137.0D0 , 6377563.396D0 , 6377304.063D0 , 
     &     6377341.89D0 , 6376896.0D0 , 6378155.0D0 , 6378160.0D0 , 
     &     6378245.0D0 , 6378270.0D0 , 6378166.0D0 , 6378150.0D0 , 
     &     6370997.0D0/
      DATA minor/6356583.8D0 , 6356514.86955D0 , 6356078.96284D0 , 
     &     6356772.2D0 , 6356911.94613D0 , 6356750.519915D0 , 
     &     6356075.4133D0 , 6356759.769356D0 , 6356752.31414D0 , 
     &     6356256.91D0 , 6356103.039D0 , 6356036.143D0 , 
     &     6355834.8467D0 , 6356773.3205D0 , 6356774.719D0 , 
     &     6356863.0188D0 , 6356794.343479D0 , 6356784.283666D0 , 
     &     6356768.337303D0 , 6370977.0D0/
C
      ier = 1
      IF ( type.LT.0 .OR. type.GT.19 ) ier = -1
      IF ( type.GE.0 .AND. type.LE.19 ) THEN
C
         b = minor(type+1)
C
         axis = major(type+1)
         eccen = (axis*axis-b*b)/(axis*axis)
      ENDIF
C
      RETURN
      END
