      FUNCTION idxchg(x,y,i1,i2,i3,i4)
C THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO
C TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION
C BY C. L. LAWSON.
C THE INPUT PARAMETERS ARE
C     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA
C           POINTS,
C     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2,
C           P3, AND P4 THAT FORM A QUADRILATERAL WITH P3
C           AND P4 CONNECTED DIAGONALLY.
C THIS FUNCTION RETURNS AN INTEGER VALUE 1 (ONE) WHEN AN EX-
C CHANGE IS NECESSARY, AND 0 (ZERO) OTHERWISE.
C DECLARATION STATEMENTS
C
C   The following line is included to make this code
C   immune to the /INTEGER= compiler switch on DG
      IMPLICIT INTEGER*4(i-n)
C
$large
      DIMENSION x(500) , y(500)
      EQUIVALENCE (c2sq,c1sq) , (a3sq,b2sq) , (b3sq,a1sq) , (a4sq,b1sq)
     &              , (b4sq,a2sq) , (c4sq,c3sq)
C PRELIMINARY PROCESSING
      x1 = x(i1)
      y1 = y(i1)
      x2 = x(i2)
      y2 = y(i2)
      x3 = x(i3)
      y3 = y(i3)
      x4 = x(i4)
      y4 = y(i4)
C CALCULATION
      idx = 0
      u3 = (y2-y3)*(x1-x3) - (x2-x3)*(y1-y3)
      u4 = (y1-y4)*(x2-x4) - (x1-x4)*(y2-y4)
      IF ( u3*u4.GT.0.0 ) THEN
         u1 = (y3-y1)*(x4-x1) - (x3-x1)*(y4-y1)
         u2 = (y4-y2)*(x3-x2) - (x4-x2)*(y3-y2)
         a1sq = (x1-x3)**2 + (y1-y3)**2
         b1sq = (x4-x1)**2 + (y4-y1)**2
         c1sq = (x3-x4)**2 + (y3-y4)**2
         a2sq = (x2-x4)**2 + (y2-y4)**2
         b2sq = (x3-x2)**2 + (y3-y2)**2
         c3sq = (x2-x1)**2 + (y2-y1)**2
         s1sq = u1*u1/(c1sq*amax1(a1sq,b1sq))
         s2sq = u2*u2/(c2sq*amax1(a2sq,b2sq))
         s3sq = u3*u3/(c3sq*amax1(a3sq,b3sq))
         s4sq = u4*u4/(c4sq*amax1(a4sq,b4sq))
         IF ( amin1(s1sq,s2sq).LT.amin1(s3sq,s4sq) ) idx = 1
      ENDIF
      idxchg = idx
      RETURN
      END
