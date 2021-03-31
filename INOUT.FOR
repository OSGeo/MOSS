      SUBROUTINE inout(n,ids,ih)
      COMMON /itemms/ item1 , item2
      COMMON /offst / sxmin , symin
      COMMON /osave / isub(15) , npolys , ltype2 , idisp , nosave , 
     &                ictsub
$Include:'io.inc'
$Include:'rspnts.inc'
      DIMENSION ids(1) , rr(2) , ibuff(128)
      DIMENSION icat(5)
      LOGICAL nosave
      DATA icat/'  ' , '  ' , '  ' , '  ' , '  '/
C
C  WRITE OUT THE INTERSECTED POLYGON
C
C
C  RESTORE LOCAL COORDINATES TO GLOBaL COORDINATES
C
      IF ( n.LE.0 ) RETURN
      DO i = 1 , n
         CALL spoints('R ',r,ri,i,1)
         xtemp = ri + sxmin
         IF ( nosave ) xtemp = xtemp - float(idisp)
         CALL spoints('R ',r,xtemp,i,2)
c                     r(i)=xtemp
         CALL spoints('S ',s,si,i,1)
         xtemp = si + symin
         IF ( nosave ) xtemp = xtemp - float(idisp)
         CALL spoints('S ',s,xtemp,i,2)
c                    s(i)=xtemp
      ENDDO
C
C  USER WISH TO SAVE THE RESULT ?
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
      icat(1) = item1
      icat(2) = item2
      DO l = 1 , 15
         ibuff(l) = isub(l)
      ENDDO
      CALL wrblk(ictsub,npolys,ibuff,1,ier)
C      WRITE (ICTSUB,2085,ERR=905,iostat=ier) (ISUB(I),I=1,15)
99001 FORMAT (15A2)
      WRITE (18) (isub(i),i=1,15) , (icat(i),i=1,5) , n , nslnd
      IF ( nslnd.GT.0 ) WRITE (18) (ids(i),i=1,nslnd)
      DO i = 1 , n
         CALL spoints('R ',r,rr(1),i,1)
         CALL spoints('S ',s,rr(2),i,1)
C      RR(1)=R(I)
C      RR(2)=S(I)
         WRITE (18) rr(1) , rr(2)
      ENDDO
C
C  AND RETURN
C
      RETURN
C
C  ERROR RETURN
C
cssk  905 WRITE (NPRNT,3905) IER,ICTSUB
99002 FORMAT ('INOUT: IER,ICTSUB',2I7)
cssk      RETURN
      END
