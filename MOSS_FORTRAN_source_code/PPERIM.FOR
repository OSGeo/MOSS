      SUBROUTINE pperim(npts,island,ns,pperm)
$Include:'xypnts.inc'
C
C *********************************************************
C
C              S U B R O U T I N E   P P E R I M
C FUNCTION:
C             FIND THE PERIMETER OF A POLYGON, INCLUDING
C             ITS ISLANDS. DATA ARE REAL.
C PARAMETERS:
C       NPTS   IN -  NUMBER OF COORDINATE PAIRS
C       ISLAND IN -  ARRAY CONTAINING ISLAND POINTERS
C       NS     IN -  NUMBER OF ISLANDS
C       PPERM  OUT-  RETURNED PERIMETER
C ********************************************************
C
      DIMENSION island(ns)
      perim = 0.0
      IF ( npts.LE.1 ) RETURN
C
C  FIND PERIMETER OF MAIN POLYGON
C
      istart = 1
      istop = npts - 1
      IF ( ns.GT.0 ) istop = iabs(island(1)) - 2
      DO i = istart , istop
         CALL spoints('X ',x,xi,i,1)
         CALL spoints('Y ',y,yi,i,1)
         CALL spoints('X ',x,xi1,i+1,1)
         CALL spoints('Y ',y,yi1,i+1,1)
         perim = perim + (sqrt((xi-xi1)**2+(yi-yi1)**2))/1609.35
      ENDDO
      pperm = perim
C
C  IF NO ISLANDS, RETURN
C
      IF ( ns.EQ.0 ) RETURN
C
C  ADD IM PERIMETER OF ISLANDS
C
      DO i = 1 , ns
         istart = iabs(island(i))
         IF ( island(i).LT.0 ) GOTO 100
         IF ( i.EQ.ns ) THEN
            istop = npts - 1
         ELSE
            istop = iabs(island(i+1)) - 2
            IF ( island(i+1).LT.0 ) GOTO 100
         ENDIF
         DO j = istart , istop
            CALL spoints('X ',x,xj,j,1)
            CALL spoints('Y ',y,yj,j,1)
            CALL spoints('X ',x,xj1,j+1,1)
            CALL spoints('Y ',y,yj1,j+1,1)
            perim = perim + (sqrt((xj-xj1)**2+(yj-yj1)**2))/1609.35
         ENDDO
 100  ENDDO
C
      pperm = perim
C
      RETURN
      END
