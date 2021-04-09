      SUBROUTINE barn1(ng,nr,iog,kase)
C
C  SEE BARN
C
      COMMON /ippy  / ippyy(3) , iofx , iofy , iofp , iofq
$Include:'io.inc'
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
      DIMENSION inc(2,8) , pt(2) , case(3,3)
      DATA inc/1 , 0 , 0 , 1 , -1 , 0 , -1 , 0 , 0 , -1 , 0 , -1 , 1 , 
     &     0 , 1 , 0/
      DATA case/3 , 1 , 1 , 1 , 3 , 2 , 1 , 4 , 2/
      lg = 0
      DO kr = 1 , nr
         IF ( lg.NE.0 ) THEN
            kg = lg - 1
            IF ( kg.EQ.0 ) kg = ng - 1
            CALL spoints('X ',x,xxr,kr+iofx,1)
            CALL spoints('P ',p,ppr,kg+iofp,1)
            CALL spoints('Y ',y,yyr,kr+iofy,1)
            CALL spoints('Q ',q,qqr,kg+iofq,1)
            IF ( xxr.EQ.ppr .AND. yyr.EQ.qqr ) GOTO 200
C     IF(X(KR+IOFX).EQ.P(KG+IOFP).AND.Y(KR+IOFY).EQ.Q(KG+IOFQ))
C    1 GO TO 50
            kg = lg + 1
            IF ( kg.GT.ng ) kg = 2
            CALL spoints('X ',x,xxr,kr+iofx,1)
            CALL spoints('P ',p,ppr,kg+iofp,1)
            CALL spoints('Y ',y,yyr,kr+iofy,1)
            CALL spoints('Q ',q,qqr,kg+iofq,1)
            IF ( xxr.EQ.ppr .AND. yyr.EQ.qqr ) GOTO 300
         ENDIF
c      IF(X(KR+IOFX).EQ.P(KG+IOFP).AND.Y(KR+IOFY).EQ.Q(KG+IOFQ))
c     1 GO TO 60
         DO kg = 1 , ng
            CALL spoints('X ',x,xxr,kr+iofx,1)
            CALL spoints('P ',p,ppr,kg+iofp,1)
            CALL spoints('Y ',y,yyr,kr+iofy,1)
            CALL spoints('Q ',q,qqr,kg+iofq,1)
            IF ( xxr.EQ.ppr .AND. yyr.EQ.qqr ) THEN
               lg = kg
               GOTO 100
            ENDIF
         ENDDO
c  20  IF(X(KR+IOFX).EQ.P(KG+IOFP).AND.Y(KR+IOFY).EQ.Q(KG+IOFQ))
c     1 GO TO 30
         kg = 0
         lg = kg
 100  ENDDO
      DO kr = 1 , nr
         DO j = 1 , 8
            CALL spoints('X ',x,xxr,kr+iofx,1)
            CALL spoints('Y ',y,yyr,kr+iofy,1)
            pt(1) = xxr + inc(1,j)
            pt(2) = yyr + inc(2,j)
            CALL inside(pt,nr,inr,1)
            CALL inside(pt,ng,ing,2)
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
