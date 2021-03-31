      SUBROUTINE blmsym(xp,yp,hite,wide,angle,icode,ipen1,ipen2)
C
      COMMON /blmsc / iopen , lu , ip(3,42) , ixtra(2)
      CHARACTER symfile*20
      INTEGER ipp(128) , ipip(2) , maps(10)
      EQUIVALENCE (ip,ipp) , (ipp(127),ixtra(1)) , (symfile,maps(1))
C
C      DATA IOPEN,LU/0,47/
      x(ii) = float(ii)*wide/200.
      y(ii) = float(ii)*hite/200.
      xr(kx,ky) = x(kx)*ca - y(ky)*sa + xp
      yr(kx,ky) = y(ky)*ca + x(kx)*sa + yp
C
      CALL plotw(xp,yp,3)
      IF ( icode.LE.0 .OR. icode.EQ.99 ) RETURN
C/*  NO SYMBOL DESIRED
      symfile = '\GIS\intrface\BLMSYM.DT'
      IF ( iopen.EQ.0 ) THEN
         CALL openf(47,maps,1,256,2,ier)
         IF ( ier.NE.0 ) THEN
            WRITE (*,*) 'Could not OPEN BLMSYM.DT'
            RETURN
         ENDIF
      ENDIF
      iopen = 1
      CALL rdblk(47,icode,ipp,1,ier)
      IF ( ier.NE.0 ) THEN
         WRITE (*,*) 'Error READing in BLMSYM   ICODE=' , icode
         RETURN
      ELSE
         lastpen = 0
         a = angle*.0174533
         ca = cos(a)
         sa = sin(a)
         xtr = xr(100,100)
         ytp = yr(100,100)
         i = 0
      ENDIF
 100  CONTINUE
      i = i + 1
      IF ( i.GT.42 ) THEN
         WRITE (*,*) '>42 lines in BLMSYM.DT    ICODE=' , icode
         RETURN
      ELSE
         CALL byteit(ip(1,i),ipip)
         k = ipip(1)
C/*  GET PEN CODE
         IF ( k.EQ.0 ) THEN
            CALL plotw(xp,yp,3)
            RETURN
C/*  DONE WITH THIS SYMBOL
         ELSEIF ( k.NE.lastpen ) THEN
            IF ( k.EQ.1 .AND. ipen1.EQ.0 ) GOTO 100
C/*  NO PEN
            IF ( k.EQ.2 .AND. ipen2.EQ.0 ) GOTO 100
C/*  NO PEN
            IF ( k.EQ.1 .AND. ipen1.GT.0 ) CALL newpen(ipen1)
            IF ( k.EQ.2 .AND. ipen2.GT.0 ) CALL newpen(ipen2)
            lastpen = k
         ENDIF
      ENDIF
      ipen = ipip(2)
C/*  GET DRAW CODE
      ix = ip(2,i)
      iy = ip(3,i)
      IF ( ipen.EQ.2 ) THEN
         CALL plotw(xr(ix,iy),yr(ix,iy),ipen)
      ELSEIF ( ipen.EQ.3 ) THEN
C          SIMPLE MOVE (UP OR DOWN)
         CALL plotw(xr(ix,iy),yr(ix,iy),ipen)
      ELSEIF ( ipen.EQ.4 ) THEN
C          DRAW AN ARC OF A CIRCLE
         i = i + 1
         CALL byteit(ip(1,i),ipip)
         ir = ipip(2)
C/*  GET RADIUS
         ia1 = ip(2,i)
C/*  START ANGLE
         ia2 = ip(3,i)
C/*  END   ANGLE
         IF ( ia1.EQ.ia2 ) ia2 = ia1 + 360
         lseg = 21
         IF ( ia1.EQ.ip(3,i) ) lseg = 22
C/*  LOOP A LITTLE OVER FOR FULL CIRCLE
         jpen = 3
C          DRAW ARC WITH 20 SEGMENTS
         DO k = 1 , lseg
            ia = ia1 + float(ia2-ia1)*float(k-1)/20.
            th = float(ia)*.0174533
            jx = ir*cos(th) + ix
            jy = ir*sin(th) + iy
            CALL plotw(xr(jx,jy),yr(jx,jy),jpen)
            jpen = 2
         ENDDO
      ELSE
C          DRAW A DOT AT (X,Y)
         CALL plotw(xr(ix+5,iy+5),yr(ix+5,iy+5),3)
         CALL plotw(xr(ix+5,iy-5),yr(ix+5,iy-5),2)
         CALL plotw(xr(ix-5,iy-5),yr(ix-5,iy-5),2)
         CALL plotw(xr(ix-5,iy+5),yr(ix-5,iy+5),2)
         CALL plotw(xr(ix+5,iy+5),yr(ix+5,iy+5),2)
      ENDIF
      GOTO 100
      END
