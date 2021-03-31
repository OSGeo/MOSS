C******************************************************************************
C
C  Name:    C L O S E D
C
C  Purpose: FINDS SEG WITH SMALLEST ANGLE TO HORIZONTAL AND CYCLES FROM
C           EXTERIOR POINT TO LEFT AROUND LOOP
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
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
C  -- Stack Variables --
C     GOOD1                           Logical
C     I                               Integer
C     INC                             Integer
C     IB                              Integer
C     IR                              Integer
C     LOCAS                           Integer Array         10
C     BEST                            Real
C     SEGLEN                          Real
C     THETA                           Real
C
C  Calling:          LBLBOX
C
C
      SUBROUTINE closed(ier)
C
$INCLUDE: 'XYPNTS.INC'
      COMMON /io    / nprnt , ioin
      COMMON /loco1 / box(4,4) , length , hgt , smdif , nstart , nstop , 
     &                ido , loser , nbox , maxbox , iatt , iatyp , 
     &                mwords , locate , nvert , nchar , lref(4) , 
     &                mref(4)
C
      DIMENSION locas(10)
      REAL length
      LOGICAL good1
C
      ier = 0
      ir = nbox + 1
C.....IF TO FEW COORDINATES, CANT DO MUCH
      IF ( nvert.GT.3 ) THEN
C
C.....CYCLE THRU POINTS AND SAVE THE ONE WITH THE SMALLEST ANGLE
         inc = 0
 50      CONTINUE
         good1 = .FALSE.
         inc = inc + 1
         IF ( inc.LE.nvert/2 ) THEN
            best = 10.
            ib = 0
C
            DO i = inc + 1 , nvert , inc
C
               CALL spoints('X ',x,xi,i,1)
               CALL spoints('Y ',y,yi,i,1)
               CALL spoints('X ',x,xiinc,i-inc,1)
               CALL spoints('Y ',y,yiinc,i-inc,1)
               seglen = sqrt(((xi-xiinc)**2)+((yi-yiinc)**2))
               seglen = seglen - (seglen/15.)
C  FUDGE FACTOR
               IF ( seglen.GE.length ) THEN
                  theta = 1.570796327
                  IF ( abs(xi-xiinc).GE.1.E-05 )
     &                  theta = atan(abs(yi-yiinc)/abs(xi-xiinc))
                  IF ( theta.LT.best ) THEN
                     best = theta
                     IF ( ib.LT.10 ) ib = ib + 1
                     locas(ib) = i
                     good1 = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
C
            IF ( .NOT.good1 ) GOTO 50
C
C.....PLACE LABEL AT ONE OF PRE-DETERMINED LOCATIONS
            DO i = ib , 1 , -1
               lref(ir) = locas(i) - inc
               mref(ir) = locas(i)
               IF ( lref(ir).LT.1 ) lref(ir) = nvert + lref(ir)
C
               CALL limit(ier)
               IF ( ier.NE.0 ) GOTO 100
            ENDDO
         ENDIF
      ENDIF
C
 100  CONTINUE
      ier = 0
      RETURN
      END
