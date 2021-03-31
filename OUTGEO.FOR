      SUBROUTINE outgeo(iscrch,idst,idsv,igd,n,m,x,u10,u14,u21)
C***  THIS SUBROUTINE OUTPUTS GEOSTATISTICAL RESULTS TO A STATPAC FILE.
C***  IT IS CALLED BY PROGRAMS SS2DXVAL AND SS2DGRID
C***  IF M = 4, CALL IS FROM SS2DGRID
C***       = 7, CALL IS FROM SS2DXVAL
C***  THIS SUBROUTINE WAS WRITTEN BY W.D. GRUNDY, AUGUST 1981
C***  LINK REQUIRED: IOS
C
$INCLUDE: 'MYST.INC'
      CHARACTER*32 u10 , u14 , u21
      CHARACTER*8 name
C      CHARACTER*4 IDST,IDSV
      CHARACTER*1 ia
      INTEGER*4 loc
      DIMENSION idst(2) , name(2) , loc(2) , idsv(2,m) , x(m) , ia(7)
      CHARACTER*8 iholid
C      CHARACTER*4 IVIB
      INTEGER*4 ivib , idst , idsv
      INTEGER*2 ex , ey
      COMMON /cov4  / p(1830) , id(60) , w(60) , ihol(51) , r(60)
      DATA name/2*'        '/ , loc/2*0/ , ia/7*' '/
      iwtf = n
      REWIND iscrch
c      WRITE(*,*) 'IN OUTGEO: N =',N
      IF ( m.NE.4 ) THEN
         idsv(1,3) = ivib(1,igd)
         idsv(2,3) = ivib(2,igd)
      ENDIF
CWRITE(IOUT) IDST,N,M,(IDSV(1,L),IDSV(2,L),L=1,M)
C      WRITE(*,777)IDST
C 777  FORMAT(' OUTGEO: IDST =',8A2)
      IF ( m.EQ.4 ) THEN
         DO i = 1 , iwtf
            READ (iscrch,END=100) (x(l),l=1,m)
            CALL jkptua(iout,i,x,ia,m)
         ENDDO
      ELSE
         DO i = 1 , n
            READ (iscrch,END=100) name(1) , (x(l),l=1,m)
            CALL jkptua(iout,i,x,ia,m)
         ENDDO
      ENDIF
      WRITE (*,99001)
99001 FORMAT (' NUMERIC TRACE IS IN THE DOS FILE "TEMPKRIG.FGR"')
      CLOSE (iscrch,STATUS='DELETE')
C      CLOSE(IOUT)
      RETURN
 100  CONTINUE
      WRITE (*,99002)
99002 FORMAT (' AN END OF FILE WAS ENCOUNTERED IN SCRATCH FILE13')
      STOP
      END
