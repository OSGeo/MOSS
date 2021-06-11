      SUBROUTINE rdxyz(infil,xd,yd,zd,nndp,ier)
C     Initializes arrays XD, YD, ZD from data in disk files
C
      IMPLICIT INTEGER*4(i-n)
      INTEGER*2 idummy(20) , ndp , m , i , j , ier
C
      DIMENSION xd(500) , yd(500) , zd(500)
C
C  Open STATPAC file and read header record
C    convert NDP to integer*4 for return to calling program
C
      ier = 0
      READ (infil,IOSTAT=ier,ERR=100) (idummy(j),j=1,4) , ndp , m , 
     &                                (idummy(j),j=1,12)
C        READ (INFIL,IOSTAT=IER,ERR=9900) (IDUMMY(J),J=1,12)
      nndp = ndp
C
C  Read data records and fill arrays
C
      DO i = 1 , ndp
         READ (infil,IOSTAT=ier,ERR=200) (idummy(j),j=1,9) , nloc1 , 
     &         nloc2 , xd(i) , yd(i) , zd(i) , idummy(14)
      ENDDO
      GOTO 300
C
C  Error exit
C
 100  CONTINUE
      WRITE (*,*) ' error reading TEMPKRIG.STP   ERROR ' , ier
 200  CONTINUE
      WRITE (*,*) 'error' , ier , 
     &            ' while trying to read TEMPKRIG.STP,' , ' record ' , i
 300  CONTINUE
      RETURN
      END
