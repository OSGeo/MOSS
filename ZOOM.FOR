      PROGRAM zoom
$Include:'strng.inc'
$Include:'vwnd.inc'
      COMMON /fls   / iunit , ifile(5)
      COMMON /io    / nprnt , ioin
      INTEGER map(25)
C
      DIMENSION ipt(2) , ige(2)
      LOGICAL mcomp , geog , box , point
C
C ****************************************************
C
C           S U B R O U T I N E     B L O W
C
C   ALLOW THE USER TO MAGINFY A SPECIFIED SECTION
C   OF THE ACTIVE DATA.
C
C
C PARAMETERS:
C            IER   - 1, IF AN ERROR
C                  - 0, IF NO ERROR
C
C ****************************************************
C
      ier = 0
      CALL initl(ier)
      IF ( ier.NE.0 ) STOP
C
C  SET ZOOM TYPE FLAGS
C
      geog = .FALSE.
      point = .FALSE.
      box = .TRUE.
C
C initilize display
C
      map(1) = 'LM'
      map(2) = '  '
      jchan = 81
      CALL openf(jchan,map,1,0,1,ier)
      IF ( ier.NE.0 ) THEN
         WRITE (nprnt,99001)
99001    FORMAT (/,1X,'You must do the <PLOT> command first')
         RETURN
      ENDIF
C
      map(1) = 'Z '
      CALL openf(70,map,3,0,1,ier)
C
      CALL initt(1,iunit,ifile)
C
C  MAKE SURE VIRTUAL AND SCREEN WINDOWS ARE SET
C
      IF ( ibw.EQ.1 ) THEN
         CALL vwindo(xstart,xlen,ystart,ylen)
         CALL scrset(xstart,xlen,ystart,ylen)
      ELSE
         CALL vwindo(xorgin,xalen,yorgin,yalen)
         CALL scrset(xorgin,xalen,yorgin,yalen)
      ENDIF
C      CALL FLSH
C
C  CHECK FOR ZOOM TYPE
C
      ige(1) = 'GE'
      ipt(1) = 'PO'
      IF ( mcomp(icard(k+1),'G',1,ier) ) geog = .TRUE.
      IF ( mcomp(icard(k+1),'P',1,ier) ) point = .TRUE.
      IF ( point .OR. geog ) box = .FALSE.
C
C  CALL APPROPRIATE ZOOM SUBROUTINE
C
      IF ( box ) CALL zoombx(jchan)
C
C  needs proj package
C
C      IF(GEOG)  CALL ZOOMGE(jchan)
      IF ( point ) CALL zoompt(jchan)
C
      CALL outcm
      END
