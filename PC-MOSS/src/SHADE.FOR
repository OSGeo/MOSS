C*******************************************************************************
C
C          M A I N    S H A D E
C FUNCTION:
C          THIS ROUTINE SHADES A SET OF POLYGONS ACCORDING TO
C          USER SPECIFICATIONS.
C PARAMETERS:
C             IER       OUT - THE ERROR RETURN FOR WRITING TO, READING,
C                             OPENNING, OR CLOSING A FILE,
C                             IER = 0, IF AN ERROR,
C******************************************************************************
      PROGRAM shade
C
C       SHADPR - SHADE COMMAND IN MOSS
C
$Include:'dbchan.inc'
      COMMON /fls   / iunit , ifile(5)
      COMMON /plact / macts
      COMMON /io    / nprnt , ioin
      COMMON /whmap / istrct(300) , nstrct
$Include:'strng.inc'
      COMMON /table / itable(40)
$Include:'vwnd.inc'
$Include:'mfast.inc'
C      COMMON /RBR/    IRBGR,ICHR
C      COMMON /RBX/    IRBGX,ICHX
C      COMMON /RBY/    IRBGY,ICHY
C      COMMON /RBS/    IRBGS,ICHS
      COMMON /locla / xinc , rot , vx1 , vx2 , vy1 , vy2 , scale , 
     &                ichan , lpinch
      COMMON /clines/ icline , ibatch
      COMMON /mailb / mbx2do , ltime
      INTEGER icolor(2) , map(25)
      LOGICAL ltime
C
      CALL initl(ier)
      IF ( ier.NE.0 ) STOP
      ibatch = 0
C
C.....SET LINES TO 0
C
      lines = 0
C
C.....SET WINDOW
C
C      CALL SETWND
      ichan = 10
      CALL openf(icdcfa,idesfa,1,512,2,ier)
C
C.....SET STARTING ROTATION AND LINES PER INCH
C
      rot = -5.0
      lpinch = 8
C
C.....SET VIEWING WINDOW SIZE - THIS IS REQUIRED
C.....TO DETERMINE WHETHER A POLYGON OR LINE FALLS
C.....IN THE VIEWING WINDOW.  THIS IS A MONEY SAVING
C.....FUNCTION
C
      CALL whwnd(vx1,vy1,vx2,vy2)
      IF ( vx1.EQ.vx2 .AND. vy1.EQ.vy2 ) THEN
C
C.....ERROR RETURNS
C
         WRITE (*,*) 
     &           ' Display window not set. Set window to active map(s).'
      ELSE
         CALL scalit(xorg,yorg,xxxx,yyyy,scale,yinc,xr,yr,12.0)
C
C.....GET ACTIVE DATA TO BE PLOTTED
C
         CALL daget(icard,icardp,itable,40,macts)
C
C.....MAKE SURE THERE IS SOMETHING TO SHADE.
C
         IF ( macts.EQ.0 ) THEN
C
C.....USER DID NOT SPECIFY ACTIVE DATA SET I.D.'S. GET THEM
C
            WRITE (nprnt,99001)
99001       FORMAT (' ENTER ACTIVE MAP I.D.(S) TO SHADE'\)
            lines = lines + 3
            CALL readin(icard,lenicd,icardp,istop)
            CALL daget(icard,icardp,itable,40,macts)
         ENDIF
C
C.....CALCULATE THE INITIAL SPACING (IN METERS)
C.....BETWEEN THE CROSS HATCH LINES
C.....THE VIEWING WINDOW IS USED TO CALCULATE THIS.
C
         IF ( ibw.EQ.0 ) xmax = amax1(xalen,yalen)
         IF ( ibw.EQ.1 ) xmax = amax1(xlen,ylen)
C
C.....CALCULATE METERS PER INCH OF SCREEN
C
         xinc = xmax/10.0
C
C.....START THINGS ROLLING
C
C
C initilize display
C
         map(1) = 'Zz'
         CALL openf(70,map,3,0,1,ier)
         CALL initt(0,iunit,ifile)
         CALL setwnd
         map(1) = 'LM'
         jchan = 81
         CALL openf(jchan,map,1,0,1,ier)
         IF ( ier.NE.0 ) THEN
            WRITE (nprnt,99002)
99002       FORMAT (/,1X,'You must do the <PLOT> command first')
            RETURN
         ENDIF
C
         REWIND (jchan)
         CALL drfast(jchan,ier)
 
         CALL shader
         READ (*,'(a1)') ijij
         WRITE (*,*) char(27) , char(12)
      ENDIF
C
cssk  903 write(*,*)' Could not open disk arrays'
C
C.....WAY OUT
C
      CLOSE (icdcfa,IOSTAT=ier)
      CLOSE (70,STATUS='DELETE',IOSTAT=ier)
      lines = lines + 4
C      CALL CPLAC(LINES)
C      CALL POSTF
      icolor(1) = 1
C      CALL SETCOL(IUNIT,ICOLOR(1),IER)
C      CALL OUTCM
C      CALL BACK
      END
