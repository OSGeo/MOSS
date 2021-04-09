      SUBROUTINE criss(lg,lr,kgr,tgr,nlst,maxlst)
      COMMON /ippy  / ippyy(3) , iofx , iofy , iofp , iofq
$Include:'io.inc'
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C CHECKS FOR POLYGON CROSSINGS.  CALLED BY GROPE.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      DIMENSION kgr(2,1)
      DIMENSION xt(2,2) , yt(2,2)
      REAL tgr(2,1) , t(2,2)
      CALL spoints('X ',x,xt(1,1),(lg+iofx),1)
c                         XT(1,1)=X(LG+IOFX)
      CALL spoints('X ',x,xt(1,2),(lg+1+iofx),1)
c                         XT(1,2)=X(LG+1+IOFX)
      CALL spoints('Y ',y,xt(2,1),(lg+iofy),1)
c                         XT(2,1)=Y(LG+IOFY)
      CALL spoints('Y ',y,xt(2,2),(lg+1+iofy),1)
c                         XT(2,2)=Y(LG+1+IOFY)
      CALL spoints('P ',p,yt(1,1),(lr+iofp),1)
c                         YT(1,1)=P(LR+IOFP)
      CALL spoints('P ',p,yt(1,2),(lr+iofp+1),1)
c                         YT(1,2)=P(LR+IOFP+1)
      CALL spoints('Q ',q,yt(2,1),(lr+iofq),1)
c                         YT(2,1)=Q(LR+IOFQ)
      CALL spoints('Q ',q,yt(2,2),(lr+iofq+1),1)
c                         YT(2,2)=Q(LR+IOFQ+1)
      CALL cross(xt,yt,t,kross)
      IF ( kross.EQ.0 ) GOTO 200
      m = 0
      DO i = 1 , 4
         CALL spoints('X ',x,xlgi1,(lg+i-1+iofx),1)
         CALL spoints('P ',p,plri1,(lr+i-1+iofp),1)
         IF ( xlgi1.NE.plri1 ) GOTO 100
      ENDDO
      m = 1
 100  CONTINUE
      IF ( m.NE.1 ) THEN
         DO j = 1 , kross
            nlst = nlst + 1
            IF ( nlst.LE.maxlst ) THEN
               kgr(1,nlst) = lg
               kgr(2,nlst) = lr
               tgr(1,nlst) = t(1,j)
               tgr(2,nlst) = t(2,j)
            ENDIF
         ENDDO
      ENDIF
 200  CONTINUE
      RETURN
      END
