C************************************************************************
C
C  Name:    L I M I T
C
C  Purpose: CALCULATE (X,Y) ENDPOINTS OF THE LABEL BETWEEN THE REFERENCE
C           POINTS DETERMINED BY THE CALLING ROUTINE.
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C
C  -- DEBUG --
C     IDEB                            Integer
C
C  -- LOCO1 --
C     BOX                             Real Array            32
C     LENGTH                          Real
C     HGT                             Real
C     SMDIF                           Real
C     NSTART                          Integer
C     NSTOP                           Integer
C     IDO                             Integer
C     LOSER                           Integer
C     NBOX                            Integer
C     MAXBOX                          Integer
C     IATT                            Integer
C     IATYP                           Integer
C     MWORDS                          Integer
C     LOCATE                          Integer
C     NVERT                           Integer
C     NCHAR                           Integer
C     LREF                            Integer Array          4
C     MREF                            Integer Array          4
C
C  -- LOCAL --
C     CENTRX                          Real
C     CENTRY                          Real
C     THETA                           Real
C     TEMPBX                          Real Array            16
C
C  -- Stack Variables --
C     BAD                             Logical
C     I                               Integer
C     M                               Integer
C     J                               Integer
C     IR                              Integer
C     TEMPLB                          Real Array             8
C     RBM                             Real Array             8
C     XDIF1                           Real
C     YDIF1                           Real
C
C  --   external Subprograms --
C     X              Y              CORNRS         BAD1
C     BAD2           BAD3
C
C  Files:
C       Unit    File name or description
C       ------  -------------------------------------------------------
C******************************************************************************
C
      SUBROUTINE limit(ier)
C
$INCLUDE: 'XYPNTS.INC'
      COMMON /io    / nprnt , ioin
      COMMON /local / centrx , centry , theta , tempbx(8)
      COMMON /loco1 / box(4,4) , length , hgt , smdif , nstart , nstop , 
     &                ido , loser , nbox , maxbox , iatt , iatyp , 
     &                mwords , locate , nvert , nchar , lref(4) , 
     &                mref(4)
C
      REAL length , templb(4) , rbm(4)
      LOGICAL bad , bad1 , bad2 , bad3
C
      ier = 0
      ir = nbox + 1
C
C.....CALCULATE CENTER OF REFERENCE POINTS
      CALL spoints('X ',x,xmrefir,mref(ir),1)
      CALL spoints('X ',x,xlrefir,lref(ir),1)
      CALL spoints('Y ',y,ymrefir,mref(ir),1)
      CALL spoints('Y ',y,ylrefir,lref(ir),1)
      centrx = (xmrefir+xlrefir)/2.
      centry = (ymrefir+ylrefir)/2.
C
C.....DETERMINE ANGLE OF LABELS ORIENTATION
      theta = 1.50796327
      IF ( abs(xmrefir-xlrefir).GE.1.E-05 )
     &      theta = atan(abs(ymrefir-ylrefir)/abs(xmrefir-xlrefir))
C.....CALCULATE OFFSETS TO ADD TO CENTER POINT
      xdif1 = (length/2.)*cos(theta)
      ydif1 = (length/2.)*sin(theta)
C
C.....DETERMINE WHICH CASE
      IF ( xlrefir.GT.xmrefir ) THEN
C.......LEFT X VALUE GREATER THAN RIGHT X VALUE
         templb(1) = centrx + xdif1
         templb(3) = centrx - xdif1
      ELSE
C.......RIGHT X VALUE GREATER THAN OR EQUAL TO LEFT X VALUE
         templb(1) = centrx - xdif1
         templb(3) = centrx + xdif1
      ENDIF
      IF ( ylrefir.GT.ymrefir ) THEN
C.......LEFT Y VALUE GREATER THAN RIGHT Y VALUE
         templb(2) = centry + ydif1
         templb(4) = centry - ydif1
      ELSE
C.......RIGHT Y VALUE GREATER THAN OR EQUAL TO LEFT VALUE
         templb(2) = centry - ydif1
         templb(4) = centry + ydif1
      ENDIF
C
C.....ARE WE RESOLVING CONFLICTS?
      IF ( loser.NE.0 ) THEN
         CALL cornrs(nchar,hgt,xlrefir,ylrefir,xmrefir,ymrefir)
C
C........CHECK LABEL AGAINST CURRENT FEATURE
         IF ( loser.GE.1 ) bad = bad1(tempbx,lref(ir),mref(ir),nvert,
     &                           ier)
         IF ( bad ) THEN
            ier = -1
            GOTO 100
         ELSE
C.........CHECK LABEL AGAINST ALL OTHER FEATURES
            IF ( loser.GE.2 ) bad = bad2(nstart,nstop,ido,tempbx,
     &                              lref(ir),mref(ir),nvert,rbm,ier)
            IF ( bad ) THEN
               ier = -1
               GOTO 100
            ELSE
C...........CHECK LABEL AGAINST ALL OTHER LABELS
               IF ( loser.GE.3 ) bad = bad3(tempbx,rbm,ier)
               IF ( bad ) THEN
C
C.....ERROR EXIT
                  ier = -1
                  GOTO 100
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C.....WHEN WE GET HERE WE'VE GOT A GOOD BOX, INCREMENT BOX COUNT
      nbox = nbox + 1
      DO i = 1 , 4
         box(nbox,i) = templb(i)
      ENDDO
C
C.....MORE THAN 1 BOX, COMPARE FOR DUPLICATES
      IF ( nbox.GE.2 ) THEN
C.......FOR EACH PREVIOUS BOX
         m = nbox - 1
         DO i = 1 , m
C.........COMPARE PREVIOUS BOX(ES) WITH CURRENT ONE
            DO j = 1 , 4
C...........IF NOT TOO CLOSE, TRY NEXT BOX
               IF ( abs(box(i,j)-box(nbox,j)).GT.smdif+length ) GOTO 50
            ENDDO
C.........SAME BOX - REMOVE CURRENT BOX, DECREMENT BOX COUNT & DO NEXT QUADRANT
            DO j = 1 , 4
               box(nbox,j) = 0.
            ENDDO
            nbox = nbox - 1
            GOTO 100
 50      ENDDO
      ENDIF
C
 100  CONTINUE
      RETURN
      END
