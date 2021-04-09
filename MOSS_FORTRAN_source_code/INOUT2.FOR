      SUBROUTINE inout2(kchan,n,ids,ih,isw)
      COMMON /offst / sxmin , symin
      COMMON /osave / isub(15) , npolys , ltype2 , idisp , nosave , 
     &                ictsub
      COMMON /io    / nprnt , ioin
$Include:'rspnts.inc'
$Include:'xypnts.inc'
      DIMENSION ids(ih) , xtemp(2) , ihead(128)
      DIMENSION icat(5)
      LOGICAL nosave
C
      DO i = 1 , 5
         icat(i) = '  '
      ENDDO
C
C  WRITE OUT THE INTERSECTED POLYGON
C
C
C
C  USER WISHES TO SAVE RESULT. INCREMENT ITEM COUNTER
C
      npolys = npolys + 1
C
C  PUSH UP ISLAND POINTER STACK TOCONFORM TO MOSS DATA RECORD
C  STRUCTURE
C
      nslnd = ih - 1
      IF ( nslnd.NE.0 ) THEN
         nslnd = nslnd - 1
         DO i = 1 , nslnd
            ids(i) = ids(i+1) + 1
         ENDDO
      ENDIF
C
C  STORE RECORD DATA ON DISK
C
      WRITE (kchan) (isub(i),i=1,15) , (icat(i),i=1,5) , n , nslnd
      CALL clear(ihead,128)
      DO l = 1 , 15
         ihead(l) = isub(l)
      ENDDO
      CALL wrblk(ictsub,npolys,ihead,idum,ier)
C      WRITE (ICTSUB,2085,ERR=905) (ISUB(I),I=1,15)
C 2085 FORMAT (15A2)
      IF ( nslnd.GT.0 ) WRITE (kchan) (ids(i),i=1,nslnd)
      DO i = 1 , n
         IF ( isw.EQ.1 ) THEN
            CALL spoints('R ',r,ri,i,1)
            CALL spoints('S ',s,si,i,1)
         ELSE
            CALL spoints('X ',x,ri,i,1)
            CALL spoints('Y ',y,si,i,1)
         ENDIF
         xtemp(1) = ri + sxmin
         xtemp(2) = si + symin
         WRITE (kchan) xtemp
      ENDDO
C
C  AND RETURN
C
      RETURN
C
C  ERROR RETURN
C
cssk  905 WRITE (NPRNT,3005) ICTSUB,IER
99001 FORMAT (' *INOUT2* WRITE ERROR: CHAN,IER',2I7)
cssk      RETURN
      END
