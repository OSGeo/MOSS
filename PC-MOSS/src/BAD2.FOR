C***************************************************************************
C
C  Name:     B A D 2
C
C  Purpose:  CHECK FOR OTHER FEATURES OVERLAPPING CURRENT LABEL BOX
C
C  Commons/Variables    In/Out        Attributes   Function/Size
C
C  -- Dummy Arguments --
C     BAD2              Out           Logical Entry - Result of function
C     NSTART            In            Integer - Beginning feature pointer
C     NSTOP             In            Integer - Ending feature pointer
C     KUR               In            Integer - Current feature pointer
C     TEMPBX            In            Real Array(8) - Current label box coords
C     IL                In            Integer - 'Left' index into disk arrays
C     IR                In            Integer - 'Right' index into disk arrays
C     NVERT             In            Integer - # of coordinates in feature
C     RBM               Out           Real Array(4) - MBR of label's box
C     IER               Out           Integer - Returned error code
C
C  -- IO --
C     NPRNT                           Integer
C     IOIN                            Integer
C
C  -- WORK --
C     IBUFF                           Integer Array        128
C     NAME                            Integer Array         15
C     ICOM                            Integer Array         10
C     JCOM                            Integer Array         10
C
C  -- Stack Variables --
C     BAD                             Logical
C     TWO                             Logical
C     I                               Integer
C     IDO                             Integer
C     INDEX                           Integer
C     IEX1                            Integer
C     IEX2                            Integer
C     NCORDS                          Integer
C     MAP                             Integer Array         15
C     IWIND                           Integer Array          8
C     WIND                            Real Array             8
C     XMIN                            Real
C     YMIN                            Real
C     XMAX                            Real
C     YMAX                            Real
C
C  -- External Subprograms --
C     GETFAST        INVWND         Qrdblk          Qcordxy         BAD1
C
C  Files:
C       Unit    File name or description
C       ------  -------------------------------------------------------
C        5      Input contour LINE map
C       10      "@CONSOLE" for output
C******************************************************************************
C
      LOGICAL FUNCTION bad2(nstart,nstop,kur,tempbx,il,ir,nvert,rbm,ier)
C
      COMMON /io    / nprnt , ioin
      COMMON /work  / ibuff(128) , name(25) , icom(10) , jcom(10)
      DIMENSION map(15) , iwind(8) , wind(4) , tempbx(8) , rbm(4)
      EQUIVALENCE (iwind(1),wind(1))
      LOGICAL bad , bad1 , two
C
C.....SETUP LABEL BOX'S MBR
      bad2 = .FALSE.
      xmin = 9999999.
      ymin = 9999999.
      xmax = -9999999.
      ymax = -9999999.
      two = .FALSE.
      DO i = 1 , 8
         IF ( .NOT.(two) ) THEN
            xmin = amin1(xmin,tempbx(i))
            xmax = amax1(xmax,tempbx(i))
         ENDIF
         IF ( two ) THEN
            ymin = amin1(ymin,tempbx(i))
            ymax = amax1(ymax,tempbx(i))
         ENDIF
         two = .NOT.two
      ENDDO
      rbm(1) = xmin
      rbm(2) = xmax
      rbm(3) = ymin
      rbm(4) = ymax
C
C.....LOOP THROUGH THE FEATURE RECORDS
      DO ido = nstart , nstop
C
C.......IF CURRENT FEATURE IS "CURRENT" FEATURE, SKIP IT
         IF ( ido.NE.kur ) THEN
C.........GET POINTER FROM FAST FILE
            CALL getfast(ido,iwind,index,map,iex1,iex2,ier)
            IF ( ier.NE.0 ) GOTO 200
C.........CHECK FOR OVERLAP IN WINDOW
            IF ( invwnd(wind(1),wind(2),wind(3),wind(4),xmin,xmax,ymin,
     &           ymax).NE.0 ) THEN
C  101       FORMAT('      BAD2:2 IDO & INDEX = ',2I6)
C...........READ FEATURE INFORMATION
               CALL rdblk(15,index,ibuff,1,ier)
               ncords = ibuff(25)
C...........READ IN FEATURE COORDINATES
               CALL cordxy(ibuff,ncords,index,wind(1),wind(3),15)
C...........CALL WORK HOG FOR OVERLAP DETERMINATION
               bad = bad1(tempbx,-1,-1,ncords,ier)
               IF ( bad ) THEN
                  bad2 = .TRUE.
                  GOTO 100
               ENDIF
            ENDIF
         ENDIF
      ENDDO
C
C.....CLEANUP AND RESTORE DISK ARRAYS
 100  CONTINUE
      CALL getfast(kur,iwind,index,map,iex1,iex2,ier)
      IF ( ier.EQ.0 ) THEN
         CALL rdblk(15,index,ibuff,1,ier)
         ncords = ibuff(25)
         CALL cordxy(ibuff,ncords,index,wind(1),wind(3),15)
         GOTO 300
      ENDIF
C
C.....ERROR RETURN
C
 200  CONTINUE
      WRITE (nprnt,99001) ier , ido , index
99001 FORMAT (' ERROR ',I5,',FROM GETFAST WITH INDEX =',2I7,'  *BAD2*')
C
 300  CONTINUE
      RETURN
      END
