      SUBROUTINE IDGRID(XD, YD, NT, IPT, NL, IPL, NXI, NYI,NGP)
C
C THIS SUBROUTINE ORGANIZES GRID POINTS FOR SURFACE FITTING BY
C SORTING THEM IN ASCENDING ORDER OF TRIANGLE NUMBERS AND OF THE
C BORDER LINE SEGMENT NUMBER.
C THE INPUT PARAMETERS ARE
C     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y
C           COORDINATES OF THE DATA POINTS, WHERE NDP IS THE
C           NUMBER OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE
C           POINT NUMBERS OF THE END POINTS OF THE BORDER
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE
C           NUMBERS,
C     NXI = NUMBER OF GRID POINTS IN THE X COORDINATE,
C     NYI = NUMBER OF GRID POINTS IN THE Y COORDINATE,
C     XI,YI = ARRAYS OF DIMENSION NXI AND NYI CONTAINING
C           THE X AND Y COORDINATES OF THE GRID POINTS,
C           RESPECTIVELY.
C Modification by GWH, 11/15/85, replaced these arrays with functions
C THE OUTPUT PARAMETERS ARE
C     NGP = INTEGER ARRAY OF DIMENSION 2*(NT+2*NL) WHERE THE
C           NUMBER OF GRID POINTS THAT BELONG TO EACH OF THE
C           TRIANGLES OR OF THE BORDER LINE SEGMENTS ARE TO
C           BE STORED,
C     IGP = INTEGER ARRAY OF DIMENSION NXI*NYI WHERE THE
C           GRID POINT NUMBERS ARE TO BE STORED IN ASCENDING
C           ORDER OF THE TRIANGLE NUMBER AND THE BORDER LINE
C           SEGMENT NUMBER.
C   (in modification by GWH, 11/13/85, this array is replaced by
C    a disk file connected to unit ICHIGP).
C Modification by GWH, 11/26/85: arrays IPT,IPL,NGP given the
C non-standard type INTEGER*2 to reduce storage space
C DECLARATION STATEMENTS
C   The following line is included to make this code
C   immune to the /INTEGER= compiler switch on DG
      IMPLICIT INTEGER*4 (I-N)
C
C.........original code by Akima:.......
C     DIMENSION XD(100), YD(100), IPT(585), IPL(300), XI(101),
C    *  YI(101), NGP(800), IGP(10201)
C.........modification by GWH, 10/21/85 and 11/13/85:
      DIMENSION XD(500), YD(500)
      INTEGER*2 IPL(1500),NGP(4000),IPT(2985)
      COMMON/GRIDPTS/XMIN,XSIDE,YMIN,YSIDE
      COMMON/VARRAY/IZFIL,ICHIGP
C.........end modification..............
C
C STATEMENT FUNCTIONS
      SIDE(U1,V1,U2,V2,U3,V3) = (U1-U3)*(V2-V3) - (V1-V3)*(U2-U3)
      SPDT(U1,V1,U2,V2,U3,V3) = (U1-U2)*(U3-U2) + (V1-V2)*(V3-V2)
C PRELIMINARY PROCESSING
      NT0 = NT
      NL0 = NL
      NXI0 = NXI
      NYI0 = NYI
      NXINYI = NXI0*NYI0
C...Two small modifications by GWH 5/14/86
C...Initialize IGP file, use variable parameter for function calls
      WRITE (ICHIGP,REC=NXINYI+1) 0
      IFIRST = 1
      XIMN = AMIN1(XI(IFIRST),XI(NXI0))
      XIMX = AMAX1(XI(IFIRST),XI(NXI0))
      YIMN = AMIN1(YI(IFIRST),YI(NYI0))
      YIMX = AMAX1(YI(IFIRST),YI(NYI0))
C DETERMINES GRID POINTS INSIDE THE DATA AREA.
      JNGP0 = 0
      JNGP1 = 2*(NT0+2*NL0) + 1
      JIGP0 = 0
      JIGP1 = NXINYI + 1
      DO 160 IT0=1,NT0
        NGP0 = 0
        NGP1 = 0
        IT0T3 = IT0*3
        IP1 = IPT(IT0T3-2)
        IP2 = IPT(IT0T3-1)
        IP3 = IPT(IT0T3)
        X1 = XD(IP1)
        Y1 = YD(IP1)
        X2 = XD(IP2)
        Y2 = YD(IP2)
        X3 = XD(IP3)
        Y3 = YD(IP3)
        XMN = AMIN1(X1,X2,X3)
        XMX = AMAX1(X1,X2,X3)
        YMN = AMIN1(Y1,Y2,Y3)
        YMX = AMAX1(Y1,Y2,Y3)
        INSD = 0
        DO 20 IXI=1,NXI0
          IF (XI(IXI).GE.XMN .AND. XI(IXI).LE.XMX) GO TO 10
          IF (INSD.EQ.0) GO TO 20
          IXIMX = IXI - 1
          GO TO 30
   10     IF (INSD.EQ.1) GO TO 20
          INSD = 1
          IXIMN = IXI
   20   CONTINUE
        IF (INSD.EQ.0) GO TO 150
        IXIMX = NXI0
   30   DO 140 IYI=1,NYI0
          YII = YI(IYI)
          IF (YII.LT.YMN .OR. YII.GT.YMX) GO TO 140
          DO 130 IXI=IXIMN,IXIMX
            XII = XI(IXI)
            L = 0
            IF (SIDE(X1,Y1,X2,Y2,XII,YII)) 130, 40, 50
   40       L = 1
   50       IF (SIDE(X2,Y2,X3,Y3,XII,YII)) 130, 60, 70
   60       L = 1
   70       IF (SIDE(X3,Y3,X1,Y1,XII,YII)) 130, 80, 90
   80       L = 1
   90       IZI = NXI0*(IYI-1) + IXI
            IF (L.EQ.1) GO TO 100
            NGP0 = NGP0 + 1
            JIGP0 = JIGP0 + 1
C.........original code by Akima:.......
C           IGP(JIGP0) = IZI
C.........modification by GWH, 11/13/85:
            WRITE (ICHIGP,REC=JIGP0) IZI
C.........end modification..............
C
            GO TO 130
  100       IF (JIGP1.GT.NXINYI) GO TO 120
            DO 110 JIGP1I=JIGP1,NXINYI
C.........original code by Akima:.......
C             IF (IZI.EQ.IGP(JIGP1I)) GO TO 130
C.........modification by GWH, 11/13/85:
              READ (ICHIGP,REC=JIGP1I) IZTEMP
              IF (IZI.EQ.IZTEMP) GO TO 130
C.........end modification..............
C
  110       CONTINUE
  120       NGP1 = NGP1 + 1
            JIGP1 = JIGP1 - 1
C.........original code by Akima:.......
C           IGP(JIGP1) = IZI
C.........modification by GWH, 11/13/85:
            WRITE (ICHIGP,REC=JIGP1) IZI
C.........end modification..............
C
  130     CONTINUE
  140   CONTINUE
  150   JNGP0 = JNGP0 + 1
        NGP(JNGP0) = NGP0
        JNGP1 = JNGP1 - 1
        NGP(JNGP1) = NGP1
  160 CONTINUE
C DETERMINES GRID POINTS OUTSIDE THE DATA AREA.
C - IN SEMI-INFINITE RECTANGULAR AREA.
      DO 450 IL0=1,NL0
        NGP0 = 0
        NGP1 = 0
        IL0T3 = IL0*3
        IP1 = IPL(IL0T3-2)
        IP2 = IPL(IL0T3-1)
        X1 = XD(IP1)
        Y1 = YD(IP1)
        X2 = XD(IP2)
        Y2 = YD(IP2)
        XMN = XIMN
        XMX = XIMX
        YMN = YIMN
        YMX = YIMX
        IF (Y2.GE.Y1) XMN = AMIN1(X1,X2)
        IF (Y2.LE.Y1) XMX = AMAX1(X1,X2)
        IF (X2.LE.X1) YMN = AMIN1(Y1,Y2)
        IF (X2.GE.X1) YMX = AMAX1(Y1,Y2)
        INSD = 0
        DO 180 IXI=1,NXI0
          IF (XI(IXI).GE.XMN .AND. XI(IXI).LE.XMX) GO TO 170
          IF (INSD.EQ.0) GO TO 180
          IXIMX = IXI - 1
          GO TO 190
  170     IF (INSD.EQ.1) GO TO 180
          INSD = 1
          IXIMN = IXI
  180   CONTINUE
        IF (INSD.EQ.0) GO TO 310
        IXIMX = NXI0
  190   DO 300 IYI=1,NYI0
          YII = YI(IYI)
          IF (YII.LT.YMN .OR. YII.GT.YMX) GO TO 300
          DO 290 IXI=IXIMN,IXIMX
            XII = XI(IXI)
            L = 0
            IF (SIDE(X1,Y1,X2,Y2,XII,YII)) 210, 200, 290
  200       L = 1
  210       IF (SPDT(X2,Y2,X1,Y1,XII,YII)) 290, 220, 230
  220       L = 1
  230       IF (SPDT(X1,Y1,X2,Y2,XII,YII)) 290, 240, 250
  240       L = 1
  250       IZI = NXI0*(IYI-1) + IXI
            IF (L.EQ.1) GO TO 260
            NGP0 = NGP0 + 1
            JIGP0 = JIGP0 + 1
C.........original code by Akima:.......
C           IGP(JIGP0) = IZI
C.........modification by GWH, 11/13/85:
            WRITE (ICHIGP,REC=JIGP0) IZI
C.........end modification..............
C
            GO TO 290
  260       IF (JIGP1.GT.NXINYI) GO TO 280
            DO 270 JIGP1I=JIGP1,NXINYI
C.........original code by Akima:.......
C             IF (IZI.EQ.IGP(JIGP1I)) GO TO 290
C.........modification by GWH, 11/13/85:
              READ (ICHIGP,REC=JIGP1I) IZTEMP
              IF (IZI.EQ.IZTEMP) GO TO 290
C.........end modification..............
C
  270       CONTINUE
  280       NGP1 = NGP1 + 1
            JIGP1 = JIGP1 - 1
C.........original code by Akima:.......
C           IGP(JIGP1) = IZI
C.........modification by GWH, 11/13/85:
            WRITE (ICHIGP,REC=JIGP1) IZI
C.........end modification..............
C
  290     CONTINUE
  300   CONTINUE
  310   JNGP0 = JNGP0 + 1
        NGP(JNGP0) = NGP0
        JNGP1 = JNGP1 - 1
        NGP(JNGP1) = NGP1
C - IN SEMI-INFINITE TRIANGULAR AREA.
        NGP0 = 0
        NGP1 = 0
        ILP1 = MOD(IL0,NL0) + 1
        ILP1T3 = ILP1*3
        IP3 = IPL(ILP1T3-1)
        X3 = XD(IP3)
        Y3 = YD(IP3)
        XMN = XIMN
        XMX = XIMX
        YMN = YIMN
        YMX = YIMX
        IF (Y3.GE.Y2 .AND. Y2.GE.Y1) XMN = X2
        IF (Y3.LE.Y2 .AND. Y2.LE.Y1) XMX = X2
        IF (X3.LE.X2 .AND. X2.LE.X1) YMN = Y2
        IF (X3.GE.X2 .AND. X2.GE.X1) YMX = Y2
        INSD = 0
        DO 330 IXI=1,NXI0
          IF (XI(IXI).GE.XMN .AND. XI(IXI).LE.XMX) GO TO 320
          IF (INSD.EQ.0) GO TO 330
          IXIMX = IXI - 1
          GO TO 340
  320     IF (INSD.EQ.1) GO TO 330
          INSD = 1
          IXIMN = IXI
  330   CONTINUE
        IF (INSD.EQ.0) GO TO 440
        IXIMX = NXI0
  340   DO 430 IYI=1,NYI0
          YII = YI(IYI)
          IF (YII.LT.YMN .OR. YII.GT.YMX) GO TO 430
          DO 420 IXI=IXIMN,IXIMX
            XII = XI(IXI)
            L = 0
            IF (SPDT(X1,Y1,X2,Y2,XII,YII)) 360, 350, 420
  350       L = 1
  360       IF (SPDT(X3,Y3,X2,Y2,XII,YII)) 380, 370, 420
  370       L = 1
  380       IZI = NXI0*(IYI-1) + IXI
            IF (L.EQ.1) GO TO 390
            NGP0 = NGP0 + 1
            JIGP0 = JIGP0 + 1
C.........original code by Akima:.......
C           IGP(JIGP0) = IZI
C.........modification by GWH, 11/13/85:
            WRITE (ICHIGP,REC=JIGP0) IZI
C.........end modification..............
C
            GO TO 420
  390       IF (JIGP1.GT.NXINYI) GO TO 410
            DO 400 JIGP1I=JIGP1,NXINYI
C.........original code by Akima:.......
C             IF (IZI.EQ.IGP(JIGP1I)) GO TO 420
C.........modification by GWH, 11/13/85:
              READ (ICHIGP,REC=JIGP1I) IZTEMP
              IF (IZI.EQ.IZTEMP) GO TO 420
C.........end modification..............
C
  400       CONTINUE
  410       NGP1 = NGP1 + 1
            JIGP1 = JIGP1 - 1
C.........original code by Akima:.......
C           IGP(JIGP1) = IZI
C.........modification by GWH, 11/13/85:
            WRITE (ICHIGP,REC=JIGP1) IZI
C.........end modification..............
C
  420     CONTINUE
  430   CONTINUE
  440   JNGP0 = JNGP0 + 1
        NGP(JNGP0) = NGP0
        JNGP1 = JNGP1 - 1
        NGP(JNGP1) = NGP1
  450 CONTINUE
c                                   ITSUM = 0
c                                   ITMAX = 2*(NT+2*NL)
c                                   DO 5000 ITEST = 1,ITMAX
c                                     ITSUM = ITSUM + NGP(ITEST)
c 5000                              CONTINUE
C                                   PRINT *, ITSUM
      RETURN
      END
