      SUBROUTINE perr(inier)
      COMMON /osave / isub(15) , npolys , ltype2 , idisp , nosave , 
     &                ictsub
$Include:'io.inc'
      ier = inier
      lines = 1
C
      IF ( ier.EQ.1 ) GOTO 100
      IF ( ier.EQ.2 ) GOTO 100
      IF ( ier.EQ.3 ) THEN
         WRITE (nprnt,99001)
99001    FORMAT (' WORKFILE DIRECTORY FULL ')
      ELSEIF ( ier.EQ.4 ) THEN
         WRITE (nprnt,99002)
99002    FORMAT (' YOU FORGOT TO SET WINDOW')
         lines = lines + 2
      ELSEIF ( ier.EQ.5 ) THEN
         WRITE (nprnt,99003)
99003    FORMAT (' THE MAPS MUST BE POLYGON.')
         lines = lines + 2
      ELSEIF ( ier.EQ.6 ) THEN
         WRITE (nprnt,99004)
99004    FORMAT ('*POVERLY*  ERROR ON OPEN OF MAP ONE')
      ELSEIF ( ier.EQ.7 ) THEN
         WRITE (nprnt,99005)
99005    FORMAT ('*POVERLY*  ERROR ON OPEN OF MAP TWO')
      ELSEIF ( ier.EQ.8 ) THEN
         WRITE (nprnt,99006)
99006    FORMAT (' *POVERLY*  ERROR ON OPEN OF POINT.DT')
         lines = lines + 2
      ELSEIF ( ier.EQ.9 ) THEN
         WRITE (nprnt,99007)
99007    FORMAT (' *POVERLY*  ERROR FROM OPEN OF A DISK ARRAY FILE')
         lines = lines + 2
      ELSEIF ( ier.EQ.10 ) THEN
         WRITE (nprnt,99008) ier
99008    FORMAT (' *OVERLAY* ERROR ON OPEN OF TEMP1 ERROR ',I5)
      ELSEIF ( ier.EQ.11 ) THEN
         WRITE (nprnt,99009) ier
99009    FORMAT (' *OVERLAY* ERROR ON CREATE OF TEMP: ERROR ',I5)
      ELSEIF ( ier.EQ.12 ) THEN
         WRITE (nprnt,99010) ier
99010    FORMAT (' *OVERLAY* ERROR ON TEMP OPEN ',I5)
      ELSEIF ( ier.EQ.13 ) THEN
         WRITE (nprnt,99011)
99011    FORMAT (' *OVERLAY* ERROR ON OPENING POLYGON.DT')
      ELSEIF ( ier.EQ.14 ) THEN
         WRITE (nprnt,99012)
99012    FORMAT (' OR AND NOT ARE FOR SINGLE ITEMS ONLY !')
      ELSEIF ( ier.EQ.15 ) THEN
         WRITE (nprnt,99013)
99013    FORMAT (' *OVERLAY* FAILURE IN SWAPPING SAVWRK')
      ELSEIF ( ier.EQ.16 ) THEN
         WRITE (nprnt,99014) npolys
99014    FORMAT (/,' INTERSECTED POLYGONS =    ',I5)
      ELSE
         WRITE (nprnt,*) ' *PERR* NO ERROR #' , ier
      ENDIF
      GOTO 99999
C
C  ERROR CALL BACKS
C
 100  CONTINUE
      WRITE (nprnt,99015)
99015 FORMAT (' NO POLYGONS INTERSECT')
      lines = lines + 2
C      CALL COSTSM(IER,LINES)
C       CALL DFILW('PREPRO',IER)
C       CALL DFILW('STACK',IER)
C      GO TO 999
C
C        CALL RESET
C        CALL OUTCM
C        CALL BACK
C
99999 CONTINUE
      END
