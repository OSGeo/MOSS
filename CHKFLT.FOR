C******************************************************************************
C
C     NAME:  SUBROUTINE CHKFLT
C
C     FUNCTION:  THIS ROUTINE FINDS THE CANDIDATE FAULTS, IF ANY.  IT
C                RETURNS THE NUMBER OF CANDIDATE FAULTS IN "NFLAG" AND
C                FAST FILE POINTERS TO THE FAULTS IN THE ARRAY "NPOINT".
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  RVMBR    I    REAL ARRAY         (4)
C                  THE MBR FOR THE CANDIDATE DATA POINTS
C                  -------------------------------------------------
C                  TARGET    I    REAL ARRAY    (4)
C                  THE MBR FOR THE TARGET CELL
C                  -------------------------------------------------
C                  XCELL     I      REAL
C                  THE X COORDINATE OF THE TARGET CELL
C                  -------------------------------------------------
C                  YCELL     I      REAL
C                  THE Y COORDINATE OF THE TARGET CELL
C                  -------------------------------------------------
C                  TXCELL    I      REAL
C                  THE X COORDINATE OF THE CENTER OF THE CELL
C                  -------------------------------------------------
C                  TYCELL    I      REAL
C                  THE Y COORDINATE OF THE CENTER OF THE CELL
C                  -------------------------------------------------
C                  IER       O     INTEGER
C                  ERROR INDICATOR (NORMAL = 1)
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  INVWND, GETFAST, CORDXY, IFLINE, CLIP,
C                         RDBLK,  ISIDE,   FCHECK
C
C     CALLING ROUTINES:  FAULT
C******************************************************************************
C
      SUBROUTINE chkflt(rvmbr,target,xcell,ycell,txcell,tycell,ier)
C
$Include:'xypnts.inc'
      COMMON /fltdat/ fmapid , ichf , istar , istop , notno , 
     &                ifdat(2,64)
      COMMON /reltn / jeltn(50,4) , npoint(64) , nflag , npts
      COMMON /work  / ibuff(128) , map(25) , icom(10) , jcom(10)
C
      INTEGER fmapid , iwind(8)
      REAL vect(4) , wndow(2,2) , wind(4) , rvmbr(4) , target(4) , 
     &     trget(2,2) , iside
      EQUIVALENCE (iwind(1),wind(1))
C
      PARAMETER (atol=.00005)
C
      ier = 0
      nflag = 0
C
C.....FIND ALL FAULT LINE MBR'S THAT INTERSECT THE MBR FOR THE DATA POINTS.
      DO i = istar , istop
         CALL getfast(i,iwind,indx,isubp,iex1,iex2,ier)
         IF ( ier.NE.0 ) THEN
C
C.....ERROR RETURNS...
C
            WRITE (*,99001) ier
99001       FORMAT (' *CHKFLT* ERROR FROM GETFAST - ERROR ',I6)
            GOTO 100
C
C.......DETERMINE IF THE 2 MBRs INTERSECT.  IF SO, INVWND = 1.
         ELSEIF ( invwnd(wind(1),wind(2),wind(3),wind(4),rvmbr(1),
     &            rvmbr(2),rvmbr(3),rvmbr(4)).EQ.1 ) THEN
C.........READ FEATURE, GET COORDINATES
            CALL rdblk(ichf,indx,ibuff,128,ier)
            IF ( ier.NE.0 ) THEN
C
               WRITE (*,99002) ier , i , indx
99002          FORMAT (' *CHKFLT* ERROR FROM RDBLK - ERROR ',I5,
     &                 ' I & INDX = ',2I5)
               GOTO 100
            ELSE
               CALL cordxy(ibuff,npnts,indx,wind(1),wind(3),ichf)
C
C.........FIND CLOSEST LINE SEGMENT TO THE TARGET CELL.
               CALL ifline(xcell,ycell,npnts,ind1,ind2,vect(1),vect(2))
               CALL spoints('X ',x,vect(1),ind1,1)
               CALL spoints('Y ',y,vect(2),ind1,1)
               CALL spoints('X ',x,vect(3),ind2,1)
               CALL spoints('Y ',y,vect(4),ind2,1)
c          VECT(1) = X(IND1)
c          VECT(2) = Y(IND1)
c          VECT(3) = X(IND2)
c          VECT(4) = Y(IND2)
C
               wndow(1,1) = rvmbr(2)
               wndow(1,2) = rvmbr(1)
               wndow(2,1) = rvmbr(4)
               wndow(2,2) = rvmbr(3)
C
               CALL clip(wndow,vect,iclip)
C
C.........LINE LIES OUTSIDE THE WINDOW
               IF ( iclip.NE.-1 ) THEN
C...........DEFINITE CANDIDATE FAULT
                  nflag = nflag + 1
                  npoint(nflag) = i
C
C...........CODE FROM FCHECK1 - GWF FEB 86
C........... REORGANIZE THE MBR FOR THE TARGET CELL TO SEND TO "ISIDE"
                  trget(1,1) = target(4)
                  trget(1,2) = target(3)
                  trget(2,1) = target(2)
                  trget(2,2) = target(1)
                  rnum = iside(txcell,tycell,npnts,trget,iclip,vect)
                  IF ( iclip.NE.-1 ) THEN
C.............CHECK TO SEE IF IT IS WITHIN THE TOLERANCE.
                     IF ( abs(rnum).LE.atol ) THEN
C.............TARGET CELL LIES  ON THE FAULT LINE.  TEMPORARILY ASSIGN IT TO
C.............EITHER THE LOWER LEFT OR UPPER LEFT CORNER OF THE TARGET CELL MBR
                        txcell = target(3)
                        tycell = target(1)
                        IF ( tycell.LT.vect(2) .OR. tycell.GT.vect(4) )
     &                        THEN
                           tycell = target(2)
                        ENDIF
                     ENDIF
                  ENDIF
C
C...........DETERMINE RELATIONSHIP BETWEEN CURRENT FAULT AND DATA POINTS
                  CALL fcheck(i,npnts,wind,wndow,txcell,tycell)
               ENDIF
            ENDIF
         ENDIF
C
C.....NORMAL RETURN...
      ENDDO
C
 100  CONTINUE
      RETURN
      END
