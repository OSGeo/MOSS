      SUBROUTINE rlncln(n,dum1,dum2)
$Include:'rspnts.inc'
C       DIMENSION R(1),S(1)
C
C  REMOVE DUPLICATE POINTS FROM A LINE
C
      kount = 1
      DO i = 2 , n
         CALL spoints('R ',r,r1,i,1)
         CALL spoints('R ',r,r2,kount,1)
         CALL spoints('S ',s,s1,i,1)
         CALL spoints('S ',s,s2,kount,1)
c        R1=R(I)
c        R2=R(KOUNT)
c        S1=S(I)
c        S2=S(KOUNT)
         IF ( r1.NE.r2 .OR. s1.NE.s2 ) THEN
            kount = kount + 1
            CALL spoints('R ',r,r1,kount,2)
            CALL spoints('S ',s,s1,kount,2)
         ENDIF
c      R(KOUNT)=R1
c      S(KOUNT)=S1
      ENDDO
C
      n = kount
C
      RETURN
      END
