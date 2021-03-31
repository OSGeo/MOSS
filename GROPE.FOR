      SUBROUTINE grope(lg,boxg,nr,kgr,tgr,nlst,maxlst)
      COMMON /ippy  / ippyy(3) , iofx , iofy , iofp , iofq
$Include:'io.inc'
$Include:'pqpnts.inc'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C  SCANS A POLYGON LOOKING FOR CROSSINGS BY AN EDGE OF ANOTHER POLYGON.
C CALLED BY INLIST.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      DIMENSION boxg(4) , kgr(2,1)
      REAL tgr(2,1)
      nnr = nr - 1
      DO l = 1 , nnr
         CALL spoints('P ',p,rl,(l+iofp),1)
         CALL spoints('P ',p,rl2,(l+1+iofp),1)
         CALL spoints('Q ',q,rl1,(l+iofq),1)
         CALL spoints('Q ',q,rl3,(l+1+iofq),1)
c      RL=P(L+IOFP)
c      RL2=P(L+1+IOFP)
c      RL1=Q(L+IOFQ)
c      RL3=Q(L+1+IOFQ)
         IF ( amin1(rl,rl2).LE.boxg(3) ) THEN
            IF ( amax1(rl,rl2).GE.boxg(1) ) THEN
               IF ( amin1(rl1,rl3).LE.boxg(4) ) THEN
                  IF ( amax1(rl1,rl3).GE.boxg(2) ) THEN
                     lr = l
                     CALL criss(lg,lr,kgr,tgr,nlst,maxlst)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END
