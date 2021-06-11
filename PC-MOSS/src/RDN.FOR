      SUBROUTINE rdn(ki,ier)
C***
C***  THIS SUBROUTINE READS  DATA FROM A STATPAC INPUT FILE
C***  FOR PROGRAM SS2DGAMH.
C***  THIS PROGRAM REJECTS QUALIFIED VALUES.
C***
C***  PROGRAM REVISED FROM ORIGINAL STATPAC PROGRAM BY
C***  G W HEINE, BLM, DENVER, MAY 1985
C***  COLUMN NUMBERS ARE CONSTANT RATHER THAN USER INPUT:
C***        1  IE  COLUMN NUMBER OF EAST COORDINATE
C***        2  NO  COLUMN NUMBER OF NORTH COORDINATE
C***        3  LD  COLUMN NUMBER OF ASSAY VALUE
C***
      CHARACTER*4 idstp , name(4) , ivid(99,2) , head
      CHARACTER*1 ia(99)
      INTEGER*4 loc(2)
      INTEGER*4 numcum
      COMMON /char  / head(20) , idstp(4)
      COMMON /xsxs  / xe(5000) , yn(5000) , gd(5000) , numcum(5,20) , 
     &                log , clas , dlim , ang(5) , gdbar , vari , std , 
     &                n , ipc , ll , spr(5) , iscal , tmax , nrel , 
     &                iout , ipch , maxn , ndir , isel , xmin , xmax , 
     &                ymin , ymax , avgdis(5,20) , gammah(5,20) , 
     &                ctrmnt(5,20) , drift(5,20) , idata(5000)
      DIMENSION z(99)
      nhol = 0
      READ (ki) idstp(1) , idstp(2) , n , m , 
     &          (ivid(i,1),ivid(i,2),i=1,m)
      ie = 1
      no = 2
      ld = 3
      idstp(3) = ivid(ld,1)
      idstp(4) = ivid(ld,2)
C***  GET CURRENT DATE FOR POSTING ON OUTPUT
      CALL getdate(idstp(1))
C      WRITE(*,130)
99001 FORMAT (9X,'NOW READING RECORDS INTO VARIOGRAM')
      DO i = 1 , n
         CALL jkgtls(ki,irow,name,loc,z,ia,m,ier)
         IF ( ier.NE.0 ) STOP 'UNEXPECTED END OF FILE'
         IF ( isel.NE.0 ) THEN
            IF ( (z(ie).LT.xmin) .OR. (z(ie).GT.xmax) ) GOTO 100
            IF ( (z(no).LT.ymin) .OR. (z(no).GT.ymax) ) GOTO 100
         ENDIF
C***  REJECT QUALIFIED VALUES
         IF ( ia(ld).EQ.' ' .AND. ia(no).EQ.' ' .AND. ia(ie).EQ.' ' )
     &         THEN
            nhol = nhol + 1
            IF ( nhol/10*10.EQ.nhol ) WRITE (*,99002) nhol
99002       FORMAT ('+',' Now Reading Records into Variogram ',I4)
            IF ( nhol.GT.maxn ) GOTO 200
            gd(nhol) = z(ld)
            yn(nhol) = z(no)
            xe(nhol) = z(ie)
         ENDIF
 100  ENDDO
      WRITE (*,99002) nhol
      irej = n - nhol
      IF ( irej.NE.0 ) WRITE (*,99003) irej
99003 FORMAT (' ',I4,' QUALIFIED DATA POINTS HAVE BEEN REJECTED')
      IF ( nhol.GT.1 ) THEN
         RETURN
      ELSE
         DO i = 1 , 5
            WRITE (*,*) char(bel)
         ENDDO
      ENDIF
 200  CONTINUE
      WRITE (iout,99004) nhol
99004 FORMAT (' INPUT DATA ERROR',3X,I5,' DATA POINTS STORED')
      STOP ' IMPOSSIBLE TO CONTINUE BECAUSE OF DATA PROBLEMS'
      END
