      SUBROUTINE wrtenty(nvert,ids,ih,isub,map1or2,ichtsub,ichtemp,
     &                   npolys)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C            S U B R O U T I N E    W R T E N T Y
C
C  WRITE OUT THE ENTITY TO A TEMPORARY FILE FOR SAVWRK
C
C  THIS WAS DIRECTLY TAKEN AND SLIGHTLY MODIFIED FROM INOUT1
C
C       INPUT:     NVERT = NUMBER OF VERTICES
C                  IDS   = ISLAND INFO TABLE
C                  IH    = ISLAND COUNTER
C                  ISUB  = SUBJECT INFO
C                  MAP1OR2 = SWITCH FOR X,Y OR P,Q POINTS
C                  ICHTSUB  = CHANNEL TEMPSUB OPENNED ON
C                  ICHTEMP = CHANNEL TEMP OPENNED ON
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      COMMON /itemms/ item1 , item2
      COMMON /offst / xoff , yoff
$Include:'xypnts.inc'
$Include:'pqpnts.inc'
      INTEGER isub(15) , ibuff(128)
      DIMENSION icoord(4) , ids(1) , icat(5)
      EQUIVALENCE (xt,icoord(1)) , (yt,icoord(3))
C
      DATA icat/5*'  '/
C
C
      IF ( nvert.LE.0 ) RETURN
C
C  WRITE SUBJECT OUT TO TEMPSUB FILE
C
      DO ll = 1 , 15
         ibuff(ll) = isub(ll)
      ENDDO
      CALL wrblk(ichtsub,npolys,ibuff,1,ier)
99001 FORMAT (15A2)
C
C
C  STORE DATA ON DISK
C
      icat(1) = item1
      icat(2) = item2
      WRITE (ichtemp) (isub(i),i=1,15) , (icat(i),i=1,5) , nvert , ih
      IF ( ih.GT.0 ) WRITE (ichtemp) (ids(ll),ll=1,ih*2)
C
      DO i = 1 , nvert
         IF ( map1or2.EQ.2 ) THEN
C
            CALL spoints('P ',p,pi,i,1)
            CALL spoints('Q ',q,qi,i,1)
            xt = pi + xoff
            yt = qi + yoff
            WRITE (ichtemp) xt , yt
         ELSE
C
            CALL spoints('X ',x,xi,i,1)
            CALL spoints('Y ',y,yi,i,1)
            xt = xi + xoff
            yt = yi + yoff
            WRITE (ichtemp) xt , yt
         ENDIF
C
      ENDDO
C
C  AND RETURN
C
      RETURN
      END
