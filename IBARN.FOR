      SUBROUTINE ibarn(ng,nr,iog,kase)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C                 S U B R O U T I N E   B A R N
C
C
C THIS IS CALLED BY CORRAL TO RESOLVE 4 OF THE NINE POSSIBLE CASES
C DETECTED THEREIN. THE 4 CASES ARISE WHEN EACH VERTEX OF ONE POLYGON
C LIES ON THE BOUNDARY OF THE OTHER BUT NOT CONVERSELY.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      COMMON /ippy  / ipyy(3) , iofx , iofy , iofp , iofq
$Include:'io.inc'
$Include:'xypnts.500'
$Include:'pqpnts.500'
      DIMENSION inc(2,8) , pt(2) , case(3,3)
      DATA inc/1 , 0 , 0 , 1 , -1 , 0 , -1 , 0 , 0 , -1 , 0 , -1 , 1 , 
     &     0 , 1 , 0/
      DATA case/3 , 1 , 1 , 1 , 3 , 2 , 1 , 4 , 2/
      lg = 0
      DO kr = 1 , nr
         IF ( lg.NE.0 ) THEN
            kg = lg - 1
            IF ( kg.EQ.0 ) kg = ng - 1
            CALL sspoints('P ',p,ppr,kr+iofp,1)
            CALL sspoints('X ',x,xxr,kg+iofx,1)
            CALL sspoints('Q ',q,qqr,kr+iofq,1)
            CALL sspoints('Y ',y,yyr,kg+iofy,1)
            IF ( ppr.EQ.xxr .AND. qqr.EQ.yyr ) GOTO 200
            kg = lg + 1
            IF ( kg.GT.ng ) kg = 2
            CALL sspoints('P ',p,ppr,kr+iofp,1)
            CALL sspoints('X ',x,xxr,kg+iofx,1)
            CALL sspoints('Q ',q,qqr,kr+iofq,1)
            CALL sspoints('Y ',y,yyr,kg+iofy,1)
            IF ( ppr.EQ.xxr .AND. qqr.EQ.yyr ) GOTO 300
         ENDIF
         DO kg = 1 , ng
            CALL sspoints('P ',p,ppr,kr+iofp,1)
            CALL sspoints('X ',x,xxr,kg+iofx,1)
            CALL sspoints('Q ',q,qqr,kr+iofq,1)
            CALL sspoints('Y ',y,yyr,kg+iofy,1)
            IF ( ppr.EQ.xxr .AND. qqr.EQ.yyr ) THEN
               lg = kg
               GOTO 100
            ENDIF
         ENDDO
c  20  IF(P(KR+IOFP).EQ.X(KG+IOFX).AND.Q(KR+IOFQ).EQ.Y(KG+IOFY))
c     1 GO TO 30
         kg = 0
         lg = kg
 100  ENDDO
      DO kr = 1 , nr
         DO j = 1 , 8
            CALL sspoints('P ',p,ppr,kr+iofp,1)
            CALL sspoints('Q ',q,qqr,kr+iofq,1)
            pt(1) = ppr + inc(1,j)
            pt(2) = qqr + inc(2,j)
            CALL iinside(pt,nr,inr,2)
            CALL iinside(pt,ng,ing,1)
            k = case(ing+1,inr+1)
            IF ( k.EQ.1 ) GOTO 200
            IF ( k.EQ.2 ) GOTO 300
            IF ( k.EQ.4 ) THEN
               kase = 5
               GOTO 400
            ENDIF
         ENDDO
      ENDDO
      kase = 5
      GOTO 400
 200  CONTINUE
      kase = 1
      IF ( iog.EQ.0 ) kase = 5
      GOTO 400
 300  CONTINUE
      kase = 3
      IF ( iog.EQ.0 ) kase = 2
 400  CONTINUE
      RETURN
      END
