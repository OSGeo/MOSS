C*********************************************************************
C
C     NAME:  REAL FUNCTION MATHFN
C
C     FUNCTION:  PERFORMS TRIGONOMETRIC AND ARITHMETIC OPERATIONS INCLUDING
C                SQUARE ROOT, LOGARITHM, NATURAL LOGARITHM, TRUNCATED
C                INTEGRAL, ROUNDED INTEGRAL, ABSOLUTE VALUE, TANGENT, SINE,
C                COSINE, ARCTANGENT, ARCSINE, ARCCOSINE, DIVISION, ADDITION,
C                MULTIPLICATION, EXPONENTIATION, SUBTRACTION, MAXIMIZATION,
C                MINIMIZATION, COVERING AND AVERAGING ON ONE OR TWO INDIVIDUAL
C                CELL VALUES FROM EXISTING CONTINUOUS OR DISCRETE MAPS FOR
C                MATH COMMAND.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -----------------------------------------------------
C                  VALU1     I      R*4                     REAL
C                  FIRST CELL VALUE TO PROCESS
C                  -----------------------------------------------------
C                  VALU2     I      R*4                     REAL
C                  SECOND CELL VALUE TO PROCESS
C                  -----------------------------------------------------
C                  ARRY      I      R*4           1         REAL
C                  ADDITIONAL INFORMATION REQUIRED FOR PROCESSING
C                  FOR AVERAGE, CONTAINS TOTAL-WEIGHT, CURRENT-WEIGHT
C                  -----------------------------------------------------
C                  IFLAG     I      I*2                    +INTEGER
C                  INDICATES FUNCTION TO PERFORM
C                  1 - SQRT   2 - LOG    3 - NTLOG  4 - INT  5 - ANINT
C                  6 - ABS    7 - TAN    8 - SIN    9 - COS   10 - ATAN
C                 11 - ASIN  12 - ACOS  13 - ADD   14 - SUBT  15 - MULT
C                 16 - DIVI  17 - EXPON 18 - MAXI  19 - MINI  20 - COVER
C                 21 - AVER
C                 ------------------------------------------------------
C************************************************************************
C
      REAL FUNCTION mathfn(valu1,valu2,arry,iflag)
      INTEGER iflag
      REAL valu1 , valu2 , arry(1)
C
C......INITIALIZE FUNCTION VALUE ......
      mathfn = 0.0
C......BRANCH ON FLAG FOR PROCESSING ......
      IF ( iflag.EQ.2 ) THEN
C
C......LOG ......
         IF ( valu1.GT.0.0 ) mathfn = alog10(valu1)
      ELSEIF ( iflag.EQ.3 ) THEN
C
C......NATURAL LOG ......
         IF ( valu1.GT.0.0 ) mathfn = alog(valu1)
      ELSEIF ( iflag.EQ.4 ) THEN
C
C......TRUNCATED INTEGRAL VALUE ......
         mathfn = aint(valu1)
      ELSEIF ( iflag.EQ.5 ) THEN
C
C......ROUNDED INTEGRAL VALUE ......
         mathfn = anint(valu1)
      ELSEIF ( iflag.EQ.6 ) THEN
C
C......ABSOLUTE VALUE ......
         mathfn = abs(valu1)
      ELSEIF ( iflag.EQ.7 ) THEN
C
C......TANGENT ......
         offset = -(sign(valu1,0.0000001))
         IF ( abs(valu1).EQ.90.0 ) valu1 = valu1 + offset
         mathfn = tan(valu1)
      ELSEIF ( iflag.EQ.8 ) THEN
C
C......SINE ......
         mathfn = sin(valu1)
      ELSEIF ( iflag.EQ.9 ) THEN
C
C......COSINE ......
         mathfn = cos(valu1)
      ELSEIF ( iflag.EQ.10 ) THEN
C
C......ARCTANGENT ......
         mathfn = atan(valu1)
      ELSEIF ( iflag.EQ.11 ) THEN
C
C......ARCSINE ......
         mathfn = asin(valu1)
      ELSEIF ( iflag.EQ.12 ) THEN
C
C......ARCCOSINE ......
         mathfn = acos(valu1)
      ELSEIF ( iflag.EQ.13 ) THEN
C
C......ADD ......
         mathfn = valu1 + valu2
      ELSEIF ( iflag.EQ.14 ) THEN
C
C......SUBTRACT ......
         mathfn = valu1 - valu2
      ELSEIF ( iflag.EQ.15 ) THEN
C
C......MULTIPLY ......
         mathfn = valu1*valu2
      ELSEIF ( iflag.EQ.16 ) THEN
C
C......DIVIDE ......
         IF ( valu2.NE.0.0 ) mathfn = valu1/valu2
      ELSEIF ( iflag.EQ.17 ) THEN
C
C......EXPONENTIATE ......
         IF ( (valu1.LT.0.) .AND. (valu2.EQ.0.) ) mathfn = 1.0
         IF ( valu1.GT.0.0 ) mathfn = valu1**valu2
      ELSEIF ( iflag.EQ.18 ) THEN
C
C......MAXIMIZE ......
         mathfn = amax1(valu1,valu2)
      ELSEIF ( iflag.EQ.19 ) THEN
C
C......MINIMIZE ......
         mathfn = amin1(valu1,valu2)
      ELSEIF ( iflag.EQ.20 ) THEN
C
C......COVER ......
         mathfn = valu1
         IF ( valu2.NE.0. ) mathfn = valu2
      ELSEIF ( iflag.EQ.21 ) THEN
C
C......AVERAGE ......
         IF ( (arry(1)+arry(2)).NE.0.0 ) THEN
            mathfn = ((valu1*arry(1))+(valu2*arry(2)))/(arry(1)+arry(2))
         ENDIF
      ELSE
C
C......SQUARE ROOT ......
         IF ( valu1.GT.0.0 ) mathfn = sqrt(valu1)
      ENDIF
C
C......NORMAL RETURN ......
C
      RETURN
      END
