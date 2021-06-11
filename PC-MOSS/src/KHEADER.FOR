      SUBROUTINE kheader(iunit,rmax,nk)
C***  THIS SUBROUTINE WRITES OUT VARIOGRAM PARAMETERS AND DRIFT TERMS
C***  FOR PROGRAMS SS2DXVAL, SS2DDRES, AND SS2DGRID.
$INCLUDE: 'MYST.INC'
      CHARACTER*8 iholid
C      CHARACTER*4 IVIB
      INTEGER*4 ivib
      INTEGER*2 ex , ey
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      CHARACTER*4 ivrtyp(5)
      DATA ivrtyp/'SPHR' , 'EXPO' , 'LIN ' , 'GAUS' , 'CUB '/
      IF ( nest.NE.0 ) THEN
         WRITE (iunit,99001) c0 , sill , nest , 
     &                       (i,ivrtyp(itype(i)),c(i),angle(i),afh(i),
     &                       avalu(i),slope(i),i=1,nest)
99001    FORMAT (29X,'VARIOGRAM PARAMETERS',/,9X,'NUGGET = ',G11.5,4X,
     &           'SILL = ',G12.6,1X,'NO.STRUCTURES = ',I1,/,
     & ' VARIOGRAM  C-VALUE    ANGLE     ANISOTROPY  A-VALUE      SLOPE'
     & ,/,
     &'  NO. TYPE           ANISOTROPY    FACTOR              (IF LINEAR
     &)',//,(I4,2X,A4,1X,G11.5,1X,F8.3,5X,F5.3,3X,G10.4,1X,G9.3))
         WRITE (iunit,99002) rmax , nk
99002    FORMAT (/,1X,'SEARCH RADIUS (IN METERS) = ',G10.4,/,1X,
     &           'MAXIMUM NUMBER OF HOLES USED = ',I5)
      ELSE
         WRITE (iunit,99003) c0
99003    FORMAT (//,' NUGGET ONLY MODEL --- NUGGET= ',G10.4)
      ENDIF
C***  IF ORDINARY KRIGING IS WANTED, DO NOT LIST THE DRIFT TERMS
      IF ( nt.NE.1 ) THEN
         WRITE (iunit,99004) nt , (i,ex(i),ey(i),i=1,nt)
99004    FORMAT (/,24X,'NUMBER OF DRIFT TERMS =  ',I2,/,20X,'TERM NO.',
     &           3X,'X-EXPONENT',3X,'Y-EXPONENT',/,
     &           (23X,I2,11X,I2,11X,I2))
      ENDIF
      RETURN
      END
