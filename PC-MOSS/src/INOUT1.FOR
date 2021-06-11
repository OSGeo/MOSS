      SUBROUTINE inout1(n,ids,ih,isw)
      COMMON /itemms/ item1 , item2
      COMMON /offst / xoff , yoff
      COMMON /osave / isub(15) , npolys , ltype2 , idisp , nosave , 
     &                ictsub
$Include:'io.inc'
$Include:'xypnts.500'
$Include:'pqpnts.500'
$Include:'rspnts.500'
      DIMENSION icoord(4) , ids(*) , icat(5) , ihead(128)
      EQUIVALENCE (xt,icoord(1)) , (yt,icoord(3))
      DATA icat/5*'  '/
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C            S U B R O U T I N E    I N O U T 1
C
C  WRITE OUT THE INTERSECTED POLYGON
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IF ( n.LE.0 ) RETURN
C
C  WRITE SUBJECT OUT TO TEMPSUB FILE
C
C      WRITE(ICTSUB,1001) (ISUB(I),I=1,15)
C 1001 FORMAT(15A2)
C
C INCREMENT ITEM COUNTER
C
      npolys = npolys + 1
C
C  STORE DATA ON DISK
C
      CALL clear(ihead,128)
      DO l = 1 , 15
         ihead(l) = isub(l)
      ENDDO
      CALL wrblk(ictsub,npolys,ihead,idum,ier)
      icat(1) = item1
      icat(2) = item2
      WRITE (56) (isub(i),i=1,15) , (icat(i),i=1,5) , n , ih
      IF ( ih.GT.0 ) WRITE (56) (ids(ll),ll=1,ih)
C
      DO i = 1 , n
         IF ( isw.EQ.2 ) THEN
C
            CALL sspoints('P ',p,pi,i,1)
            CALL sspoints('Q ',q,qi,i,1)
            xt = pi + xoff
            yt = qi + yoff
            WRITE (56) xt , yt
         ELSEIF ( isw.EQ.3 ) THEN
C
            CALL sspoints('R ',r,ri,i,1)
            CALL sspoints('S ',s,si,i,1)
            xt = ri + xoff
            yt = si + yoff
            WRITE (56) xt , yt
         ELSE
C
            CALL sspoints('X ',x,xi,i,1)
            CALL sspoints('Y ',y,yi,i,1)
            xt = xi + xoff
            yt = yi + yoff
            WRITE (56) xt , yt
         ENDIF
C
      ENDDO
C
C  AND RETURN
C
      RETURN
      END
