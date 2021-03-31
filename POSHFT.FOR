      SUBROUTINE poshft(ipasses,ipsign,jj,istar,istop,item2)
$Include:'io.inc'
$Include:'pqpnts.500'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C              S U B R O U T I N E    P O S H F T
C
C  FUNCTION:  SLIGHTLY SHIFT MINOR POLYGON IN POLYGON OVERLAY.  REQUIRED
C             TO SOMETIMES RESOLVE OVERLAY PROBLEMS
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      WRITE (nprnt,99001) ipasses + 1 , item2
99001 FORMAT (3X,I5,' ADJUSTING MINOR POLYGON - ITEM',I5)
C
      ipasses = ipasses + 1
      ipsign = -ipsign
      poff = float(ipsign)*float(ipasses)
      DO jkl = istar , istop
         CALL sspoints('P ',p,xt,jkl,1)
C      XT=P(JKL)
         xt = xt + poff
         CALL sspoints('P ',p,xt,jkl,2)
C      p(jkl)=xt
         CALL sspoints('Q ',q,xt,jkl,1)
C      XT=Q(JKL)
         xt = xt + poff
         CALL sspoints('Q ',q,xt,jkl,2)
c      q(jkl)=xt
      ENDDO
C
      RETURN
      END
