C**********************************************************************
C
C          S U B R O U T I N E   M O S P R M
C
C  ROUTINE GETS THE PARAMETERS FOR POINT AND POLYGON MAP HISTOGRAMS
C
C      PLOTPRM(11) = TITLE LEN
C          (12-21) = MAP NAME
C**********************************************************************
C
      SUBROUTINE mosprm(plotprm,barbrk,change,ok)
C
      COMMON /io    / nprnt , ioin
C
      REAL barbrk(100,2)
      REAL inc
C
      INTEGER plotprm(100)
      INTEGER outarray(100)
      INTEGER packed(100)
      INTEGER icard(80) , icardp , lenicd
C
      LOGICAL ok , change
C
C
      change = .FALSE.
C
C
C  Histogram TITLE
      len = (plotprm(11)+1)/2
      IF ( len.LE.15 ) THEN
         WRITE (nprnt,99001) (plotprm(i),i=12,11+15)
99001    FORMAT (' ENTER TITLE (',15A2,') '\)
      ELSE
         WRITE (nprnt,99002)
         WRITE (nprnt,99003) (plotprm(i),i=12,11+30)
99002    FORMAT (' ENTER TITLE: '\)
99003    FORMAT (3X,' (',30A2,') '\)
      ENDIF
C
      CALL readin(icard,80,icardp,lenicd)
      CALL packc(icard,outarray,80)
      icardp = 100
      CALL lastc(outarray,icardp)
      IF ( icardp.GE.1 ) THEN
         len = icardp
         plotprm(11) = len
         DO i = 1 , len + 1/2
            plotprm(11+i) = outarray(i)
         ENDDO
      ENDIF
C
      WRITE (*,99004)
99004 FORMAT (/,' Histogram will be drawn to the screen. The Histogram',
     &        /,
     &   ' legend will be written to a file named: "HISTOGRM.LGN".  You'
     &   ,/,' will need to copy the Histogram to a printer in order to',
     &   /,' compare it with its legend. After viewing and/or printing',
     &   /,' the histogram - Hit CR to continue.',//,
     &   ' Hit CR to proceed'\)
      CALL readin(iget,1,ic,is)
C
      WRITE (*,*)
      WRITE (*,*) '  HISTOGRAM RUNNING '
C
      RETURN
      END
