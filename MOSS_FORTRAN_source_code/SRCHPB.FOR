 
      SUBROUTINE srchpb(xc,yc,ns,nk,r2,rmax,istart,kskip)
C***  CODED BY W. D. GRUNDY  MAY 1984
C***
C***  THIS SUBROUTINE FINDS THE NEAREST NS HOLES WITHIN DISTANCE
C***  RMAX OF THE CENTROID OF THE POLYGON OR FROM A POINT BEING KRIGED.
C***  THE NUMBER OF HOLES NS CANNOT EXCEED NK.
C***  THIS SUBROUTINE IS CALLED BY SS2DGRID AND SS2DDRES.
C***  SUBROUTINE REQUIRED: DSORT
C***
$INCLUDE: 'MYST.INC'
      CHARACTER*8 iholid
C      CHARACTER*4 IVIB
      INTEGER*4 ivib
      INTEGER*2 ex , ey
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      LOGICAL test1 , test2
      DIMENSION dist(151)
      ns = 0
      DO l = istart , nhol
         dy = yc - yn(l)
         test1 = dy.EQ.abs(dy)
         test2 = abs(dy).GT.rmax
         IF ( test1 .AND. test2 ) GOTO 100
         IF ( test2 ) GOTO 100
         dx = abs(xc-xe(l))
         IF ( rmax.LT.dx ) GOTO 100
         dis = dx*dx + dy*dy
         IF ( r2.LT.dis ) GOTO 100
         IF ( ns.GT.0 ) THEN
            IF ( ns.GE.nk ) THEN
               IF ( dist(ns).LT.dis ) GOTO 100
            ENDIF
         ENDIF
         ns = ns + 1
         dist(ns) = dis
         ihol(ns) = l
         IF ( 1.LT.ns ) THEN
C***  ARRANGE HOLES IN INCREASING ORDER OF DISTANCE
            CALL dsort(dist,ihol,ns)
            IF ( nk.LT.ns ) THEN
               ns = nk
            ENDIF
         ENDIF
 100  ENDDO
      RETURN
      END
