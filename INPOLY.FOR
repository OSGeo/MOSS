      FUNCTION inpoly(nisl,np,xp,yp)
C***********************************************************************
C          PURPOSE = DETERMINE IF POINT IS INSIDE A POLYGON
C***********************************************************************
C          (X,Y) = MOSS DISK ARRAYS
C          NISL  = # OF ISLANDS  (=0 IF NO ISLANDS)
C          NP    = ISLAND ARRAY FROM MOSS
C                 (NP(i)=index of first point of i'th island
C                 (NP(NISL+1)=#DATA POINTS+1 INCLUDING ISLANDS)
C          (XP,YP) = POINT IN QUESTION
C***********************************************************************
C          INPOLY = 0 = OUTSIDE POLYGON
C                 = 1 = INSIDE PRIMARY POLYGON
C                 =I+1= ON THE BOUNDARY OF POLYGON # I
C                             =2=ON PRIMARY BOUNDARY
C                             =3=INSIDE PRIMARY, ON ENCLOSED BOUNDARY #1
C                 =-I = INSIDE ENCLOSED POLYGON # I
C***********************************************************************
      DIMENSION np(1)
C
C          SEE IF (XP,YP) IS INSIDE PRIMARY POLYGON
C  # POLYGON = #ISLANDS+1
      npoly = nisl + 1
      inpoly = inpgon(1,np(1)-1,xp,yp)
C  NOT INSIDE PRIMATY POLYGON
      IF ( inpoly.EQ.0 ) RETURN
C  ON BOUNDARY OF PRIMARY POLYGON
      IF ( inpoly.EQ.2 ) RETURN
C  ONLY PRIMARY POLYGON EXISTS
      IF ( npoly.EQ.1 ) RETURN
C          SEE IF (XP,YP) IS INSIDE AN ENCLOSED POLYGON
      DO ip = 2 , npoly
         inp = inpgon(np(ip-1),np(ip)-1,xp,yp)
C  INSIDE OF ENCLOSED POLYGON # IP
         IF ( inp.EQ.1 ) THEN
C  INSIDE ENCLOSED POLYGON # IP
            inpoly = -ip
            RETURN
C  OUTSIDE OF THIS POLYGON
         ELSEIF ( inp.NE.0 ) THEN
C          ON THE BOUNDARY OF ENCLOSED POLYGON # IP
C               BUT IT IS STILL INSIDE PRIMARY POLYGON
            inpoly = ip + 1
            RETURN
         ENDIF
      ENDDO
C  INSIDE PRIMARY-NOT INSIDE ENCLOSED
      RETURN
      END
