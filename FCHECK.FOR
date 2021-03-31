 
C******************************************************************************
C
C     NAME:  SUBROUTINE FCHECK
C
C     FUNCTION:  THIS ROUTINE FILLS THE DATA STRUCTURE JELTN(NPTS,4)
C                WITH ALL THE NECESSARY INFORMATION.  FOR EACH DATA
C                POINT, THE STRUCTURE HOLDS WHETHER THE POINT IS OFF/ON
C                THE FAULT (1/0), WHETHER IT IS BLOCKED/CLOSE/AWAY
C                FROM THE TARGET CELL (2/1/0),  IF AWAY, THE FAULT THAT
C                LIES BETWEEN THE TARGET AND POINT, AND FINALLY, THE
C                NUMBER OF FAULTS BETWEEN THE TARGET AND THE POINT.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  J         I     INTEGER                +INTEGER
C                  THE NUMBER OF CURRENT FAULT
C                  -------------------------------------------------
C                  NPNTS     I     INTEGER                +INTEGER
C                  THE NUMBER OF POINTS IN CURRENT FAULT
C                  -------------------------------------------------
C                  WIND      I    REAL ARRAY
C                  THE MBR OF THE FAULT LINE
C                  -------------------------------------------------
C                  WNDOW     I    REAL ARRAY
C                  THE MBR OF THE ROVING WINDOW
C                  -------------------------------------------------
C                  XCELL     I      REAL
C                  THE X COORDINATE OF THE CENTER OF THE CELL
C                  -------------------------------------------------
C                  YCELL     I      REAL
C                  THE Y COORDINATE OF THE CENTER OF THE CELL
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  ISIDE
C
C     CALLING ROUTINES:   CHKFLT
C******************************************************************************
C
      SUBROUTINE fcheck(j,npnts,wind,wndow,xcell,ycell)
C
$Include:'xypnts.inc'
      COMMON /fltdat/ fmapid , ichf , istar , istop , ntot , ifdat(2,64)
      COMMON /reltn / jeltn(50,4) , npoint(64) , nflag , npts
      COMMON /gwindo/ band(3,50)
      COMMON /work  / ibuff(128) , map(25) , icom(10) , jcom(10)
C
      INTEGER fmapid
      REAL vect(4) , wind(4) , wndow(2,2) , xi(2) , yi(2) , xl(2) , 
     &     yl(2)
      REAL iside
      LOGICAL inters
      PARAMETER (atol=.00005)
      PARAMETER (tol=0.0)
C
C
C.....FOR ALL DATA POINTS
      DO i = 1 , npts
         IF ( jeltn(i,2).NE.2 ) THEN
            rnumb = iside(band(1,i),band(2,i),npnts,wndow,iclip,vect)
            IF ( abs(rnumb).LE.atol ) THEN
C
C.........THE POINT LIES ON THE FAULT LINE
               jeltn(i,1) = 0
               IF ( (band(1,i).LT.wind(1) .OR. band(1,i).GT.wind(2))
     &               .OR. 
     &              (band(2,i).LT.wind(3) .OR. band(2,i).GT.wind(4)) )
     &               jeltn(i,1) = 1
               IF ( jeltn(i,1).NE.1 ) THEN
                  jeltn(i,3) = j
               ENDIF
            ENDIF
C
C.......CREATE A MBR AROUND THE LINE SEGMENT FORMED BY JOINING THE TARGET
C....... CELL CENTER AND THE DATA POINT.  SEE IF THIS INTERSECTS WITH THE
C....... MBR OF THE FAULT LINE.
            xmin = min(xcell,band(1,i))
            xmax = max(xcell,band(1,i))
            ymin = min(ycell,band(2,i))
            ymax = max(ycell,band(2,i))
            IF ( invwnd(wind(1),wind(2),wind(3),wind(4),xmin,xmax,ymin,
     &           ymax).EQ.1 ) THEN
               inters = .FALSE.
               CALL spoints('X ',x,xk1,1,1)
               CALL spoints('Y ',y,yk1,1,1)
c          XK1 = X(1)
c          YK1 = Y(1)
C
C.........FOR EACH LINE SEGMENT WITHIN THE LINE FEATURE, CREATE A MBR.  SEE
C......... IF IT INTERSECTS THE MBR OF THE TARGET CELL CENTER AND THE DATA
C......... POINT.
               DO k = 2 , npnts
                  xk = xk1
                  CALL spoints('X ',x,xk1,k,1)
c            XK1 = X(K)
                  yk = yk1
                  CALL spoints('Y ',y,yk1,k,1)
c            YK1 = Y(K)
                  fxmin = min(xk,xk1)
                  fxmax = max(xk,xk1)
                  fymin = min(yk,yk1)
                  fymax = max(yk,yk1)
                  IF ( invwnd(xmin,xmax,ymin,ymax,fxmin,fxmax,fymin,
     &                 fymax).EQ.1 ) THEN
C.............THE MBRs INTERSECT.  SEE IF THE LINE SEGMENTS WITHIN THE MBRs
C............. INTERSECT.
                     xi(1) = xk
                     xi(2) = xk1
                     yi(1) = yk
                     yi(2) = yk1
                     xl(1) = xcell
                     xl(2) = band(1,i)
                     yl(1) = ycell
                     yl(2) = band(2,i)
                     CALL sinter(xi,yi,xl,yl,xtmp,ytmp,iflag,tol)
                     IF ( iflag.NE.0 ) THEN
C...............THE DATA POINT LIES ON THE OTHER SIDE OF A FAULT - IT IS "AWAY"
                        jeltn(i,2) = 0
                        IF ( jeltn(i,3).EQ.0 ) jeltn(i,3) = j
                        inters = .TRUE.
                        GOTO 10
                     ENDIF
                  ENDIF
C
               ENDDO
 10            CONTINUE
               IF ( inters ) jeltn(i,4) = jeltn(i,4) + 1
               IF ( jeltn(i,4).GT.1 ) jeltn(i,2) = 2
            ENDIF
         ENDIF
      ENDDO
C
C.....NORMAL RETURN
      RETURN
      END
