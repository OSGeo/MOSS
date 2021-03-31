      SUBROUTINE ptboole(npts1,ind)
      COMMON /dot   / nslands , jh , ids(1)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C  NAME:   P  T  B  O  O  L  E
C
C  FUNCTION:  FINDS THE INTERSECTION OF A POINT WITHIN A POLYGON
C
C  PARAMETER: NPTS1  =  NO. OF COORD. PAIRS IN THE POLYGON
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
$Include:'pqpnts.inc'
$Include:'rspnts.inc'
      CALL spoints('P ',p,p1,1,1)
      CALL spoints('Q ',q,q1,1,1)
      ind = inpoly(nslands,ids,p1,q1)
C  POINT NOT INSIDE POLYGON
      IF ( ind.NE.1 ) RETURN
C point inside
      CALL spoints('R ',r,p1,1,2)
C                        r(1)= p(1)
      CALL spoints('S ',s,q1,1,2)
C                        s(1)= q(1)
      CALL inout(1,ipx,1)
      RETURN
      END
