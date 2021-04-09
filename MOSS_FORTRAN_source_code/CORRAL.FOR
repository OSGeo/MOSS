 
      SUBROUTINE corral(na,nb,kase)
      COMMON /ippy  / ippyy(3) , iofx , iofy , iofp , iofq
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C GIVEN TWO CLOSED BOUNDARIES (TYPE 3 ITEMS), WE COMPUTE WHETHER ONE IS
C CONTAINED IN THE OTHER OR VICE-VERSA, OR WHETHER THEY ARE DISJOINT, OR
C IDENTICAL.  WE ASSUME THAT NEITHER PARTIALLY OVERLAPS THE OTHER.
C
C SPECIFICALLY, LET A BE A TYPE-3 ITEM CONTAINING NA POINTS AND LET B
C BE A TYPE-3 ITEM WITH NB POINTS.  WE RETURN KASE AS FOLLOWS.  (LET C
C BE THE SET-THEORETIC INTERSECTION OF A AND B.)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      DIMENSION pp(2)
$Include:'io.inc'
      INTEGER case(3,3)
      DATA ((case(i,j),i=1,3),j=1,3)/1 , 2 , 2 , 3 , 4 , 2 , 3 , 3 , 5/
      DO j = 1 , na
         CALL spoints('X ',x,pp(1),(j+iofx),1)
         CALL spoints('Y ',y,pp(2),(j+iofy),1)
c      PP(1)=X(J+IOFX)
c      PP(2)=Y(J+IOFY)
         CALL inside(pp,nb,ina,2)
         IF ( ina.NE.1 ) GOTO 100
      ENDDO
 100  CONTINUE
      DO j = 1 , nb
         CALL spoints('P ',p,pp(1),(j+iofp),1)
         CALL spoints('Q ',q,pp(2),(j+iofq),1)
c      PP(1)=P(J+IOFP)
c      PP(2)=Q(J+IOFQ)
         CALL inside(pp,na,inb,1)
         IF ( inb.NE.1 ) GOTO 200
      ENDDO
 200  CONTINUE
      kase = case(ina+1,inb+1)
      IF ( ina.EQ.1 .OR. inb.EQ.1 ) THEN
         IF ( ina.NE.1 .OR. inb.NE.1 ) THEN
            iog = 0
            IF ( inb.EQ.1 ) THEN
               IF ( ina.EQ.0 ) iog = 1
               CALL barn(na,nb,iog,kase)
            ELSE
               IF ( inb.EQ.0 ) iog = 1
               CALL barn1(nb,na,iog,kase)
               IF ( kase.EQ.2 .OR. kase.EQ.3 ) kase = 5 - kase
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END
