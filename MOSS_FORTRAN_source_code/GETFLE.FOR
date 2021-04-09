      SUBROUTINE getfle(igd,ier)
C***  THIS SUBPROGRAM INPUTS DATA FROM A STATPAC DATA FILE
C***  FOR PROGRAMS SS2DXVAL, SS2DBLOK, AND SS2DGRID.
C***  THIS SUBROUTINE WAS WRITTEN BY W.D. GRUNDY, JANUARY, 1980
C***  LINKS REQUIRED: IOS,BEEP
C***   (COLUMN NUMBERS FOR EAST COORD, NORTH COORD, AND ASSAY VAL ARE
C***   CONSTANTS RATHER THAN USER INPUT)
C
C
$INCLUDE: 'MYST.INC'
      CHARACTER*8 iholid , idum8
C      CHARACTER*4 IVIB
      INTEGER*4 ivib
      INTEGER*2 ex , ey
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      CHARACTER*8 name
      CHARACTER*4 ids
      CHARACTER*1 ia , ians
      INTEGER*4 loc
      DIMENSION ids(2) , name(2) , loc(2) , x(99) , ia(99)
C
      DO i = 1 , 99
         ia(i) = ' '
      ENDDO
C
      nhol = 0
      REWIND (ipt)
      READ (ipt) ids , n , m , ivib(1,1) , ivib(1,2) , ivib(2,1) , 
     &           ivib(2,2) , idum8
99001 FORMAT (5(1X,I2,')',2A4))
      ier = 0
      ieast = 1
      inorth = 2
      igd = 3
C     IMEAS=1
      WRITE (*,99002)
99002 FORMAT (//,' READING DATA FROM SORTED FILE',//)
      DO i = 1 , n
C      write(*,*) 'getfle: i,n = ',i,n
         CALL jkgtls(ipt,irow,name,loc,x,ia,m,ier)
         IF ( ier.NE.0 ) THEN
            WRITE (*,*) 'UNEXPECTED END OF DATA FILE'
            GOTO 200
C***  REJECT ANY QUALIFIED VALUES
         ELSEIF ( ia(igd).EQ.' ' .AND. ia(inorth).EQ.' ' .AND. ia(ieast)
     &            .EQ.' ' ) THEN
            nhol = nhol + 1
            IF ( nhol/10*10.EQ.nhol ) WRITE (*,99003) nhol
99003       FORMAT ('+',I4)
            IF ( nhol.GT.maxh ) THEN
               WRITE (*,99004)
99004          FORMAT (10X,'***THERE ARE MORE DATA POINTS THAN ALLOWED',
     &                 /,10X,'***IGNORE THE REMAINING DATA POINTS',/)
               nhol = maxh
               GOTO 100
            ELSE
C     IHOLID(NHOL)=NAME(1)
               yn(nhol) = x(inorth)
               xe(nhol) = x(ieast)
               gd(nhol) = x(igd)
            ENDIF
         ENDIF
C     ERMEAS(NHOL)=0.
C     GO TO(210,200), IMEAS
C 200 ERMEAS(NHOL)=X(IMS)
      ENDDO
      irej = n - nhol
      IF ( irej.NE.0 ) WRITE (*,99005) irej
99005 FORMAT (' ',I4,' QUALIFIED DATA POINTS HAVE BEEN REJECTED')
      IF ( nhol.LE.nt ) THEN
         DO i = 1 , 5
            WRITE (*,*) char(bel)
         ENDDO
         WRITE (*,*) 'NOT ENOUGH DATA POINTS.  CANNOT CONTINUE.'
         GOTO 200
      ENDIF
 100  CONTINUE
      RETURN
 200  CONTINUE
      ier = 1
      RETURN
      END
