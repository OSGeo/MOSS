      SUBROUTINE deloop(n,long,dum1,dum2)
      COMMON /destuf/ i , lond , ibeg , iend
$Include:'rspnts.inc'
C      DIMENSION R(1),S(1)
C
C ***********************************************************
C
C           S U B R O U T I N E    D E L O O P
C
C  FUNCTION - REMOVE SELF-CROSSING LOOPS IN POLYGONS CREATED
C             BY THE BUFFERING PROCESS
C
C
C  INPUT PARAMETERS-
C
C      (X,Y)  X-Y COORDINATE ARRAYS
C      N      NUMBER OF COORDINATE PAIRS
C      LONG   NUMBER OF SEGMENTS TO LOOK AHEAD
C ************************************************************
C
      tol = .001
      IF ( n.LE.3 ) RETURN
      lond = long
      IF ( lond.LT.1 .OR. lond.GT.25 ) lond = 10
      IF ( lond.GT.n ) lond = n - 1
      nm = n - 1
C
C
      i = 0
 100  CONTINUE
      i = i + 1
      IF ( i.LE.nm ) THEN
C
C  CALCULATE INDEXES FOR LOOK-AHEAD OF LOND UNITS
C
         next = i + 2
         nend = next + lond
         IF ( nend.GT.nm ) nend = nm
         IF ( next.LE.nend ) THEN
C
C
C  ENTER LOOK AHEAD LOOP
C
            DO k = next , nend
               CALL spoints('R ',r,ri,i,1)
               CALL spoints('S ',s,si,i,1)
               CALL spoints('R ',r,ri1,i+1,1)
               CALL spoints('S ',s,si1,i+1,1)
               CALL spoints('R ',r,rk,k,1)
               CALL spoints('S ',s,sk,k,1)
               CALL spoints('R ',r,rk1,k+1,1)
               CALL spoints('S ',s,sk1,k+1,1)
c        RI=R(I)
c        SI=S(I)
c        RI1=R(I+1)
c        SI1=S(I+1)
c        RK=R(K)
c        SK=S(K)
c        RK1=R(K+1)
c        SK1=S(K+1)
               CALL crosy(ri,si,ri1,si1,rk,sk,rk1,sk1,xt,yt,nix)
               IF ( nix.EQ.2 ) THEN
C
C  MATCH. REMOVE LOOP
C
                  ibeg = i + 1
                  iend = k
                  DO kl = ibeg , iend
                     CALL spoints('R ',r,xt,kl,2)
                     CALL spoints('S ',s,yt,kl,2)
c      R(KL)=XT
c       S(KL)=YT
                  ENDDO
                  i = k
                  GOTO 100
               ENDIF
C
C  NO MATCH.  CHECK NEXT SEGMENT
C
            ENDDO
            GOTO 100
         ENDIF
      ENDIF
C
      RETURN
      END
