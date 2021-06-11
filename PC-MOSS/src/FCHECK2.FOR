C***********************************************************************
C
C     NAME:  SUBROUTINE FCHECK2
C
C     FUNCTION:  THIS ROUTINE HANDLES THE SPECIAL CASE OF A DATA
C                POINT FALLING ON A FAULT LINE.  IF THIS OCCURS IT IS
C                ASSIGNED TO THE SIDE WHOSE VALUES (ELEVATIONS) ARE MOST
C                CLOSELY RELATED TO THE DATA POINT.  THIS IS BASED ON
C                THE AVERAGE OF SAMPLE POINTS ON EACH SIDE OF THE FAULT.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  NONE.
C
C     ROUTINES REQUIRED:  NONE
C
C     CALLING ROUTINES:  FAULT
C***********************************************************************
C
      SUBROUTINE fcheck2
C
      COMMON /reltn / jeltn(50,4) , npoint(64) , nflag , npts
      COMMON /gwindo/ band(3,50)
C
      INTEGER ccnt , acnt
      DOUBLE PRECISION csq , asq , cvar , avar , cstdv , astdv
C
      DO i = 1 , npts
C
C....IF THE POINT IS OFF THE FAULT, GO TO NEXT POINT. OTHERWISE,
C....PROCESS IT.
C
         IF ( jeltn(i,1).NE.1 ) THEN
            IF ( jeltn(i,2).NE.2 ) THEN
               iflt = jeltn(i,3)
               close = 0
               away = 0
               csq = 0
               asq = 0
               ccnt = 0
               acnt = 0
               DO j = 1 , npts
                  IF ( jeltn(j,2).NE.2 ) THEN
                     IF ( jeltn(j,1).NE.0 ) THEN
                        IF ( i.NE.j ) THEN
                           IF ( jeltn(j,2).EQ.0 .AND. jeltn(j,3)
     &                          .EQ.iflt ) THEN
C
                              away = away + band(3,j)
                              acnt = acnt + 1
                              asq = asq + (band(3,j)**2)
                           ELSE
C
                              IF ( jeltn(j,2).EQ.1 ) close = close + 
     &                             band(3,j)
                              IF ( jeltn(j,2).EQ.1 ) ccnt = ccnt + 1
                              IF ( jeltn(j,2).EQ.1 ) csq = csq + 
     &                             (band(3,j)**2)
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
C
C....FIND THE VARIANCE AND STANDARD DEVIATION OF THE POINTS ON BOTH
C....SIDES OF THE FAULT LINE.
C
               astdv = 0
               cstdv = 0
C
               IF ( ccnt.GT.1 ) THEN
                  cvar = ((ccnt*csq)-(close**2))/(ccnt*(ccnt-1))
                  cstdv = sqrt(cvar)
               ENDIF
               IF ( acnt.GT.1 ) THEN
                  avar = ((acnt*asq)-(away**2))/(acnt*(acnt-1))
                  astdv = sqrt(avar)
               ENDIF
               IF ( acnt.NE.0 ) away = away/acnt
               IF ( ccnt.NE.0 ) close = close/ccnt
               dif1 = abs(band(3,i)-close)
               dif2 = abs(band(3,i)-away)
               jeltn(i,1) = 1
C
C.....SPECIAL CASE:  EITHER ONE OR BOTH SIDES HAVE NO POINTS.
C
               IF ( ccnt.EQ.0 .AND. acnt.EQ.0 ) jeltn(i,2) = 2
               IF ( ccnt.NE.0 .OR. acnt.NE.0 ) THEN
                  IF ( ccnt.EQ.1 .AND. acnt.EQ.1 ) jeltn(i,2) = 2
                  IF ( ccnt.NE.1 .OR. acnt.NE.1 ) THEN
                     IF ( ccnt.EQ.0 .AND. dif2.LE.astdv ) jeltn(i,2) = 1
                     IF ( ccnt.EQ.0 .AND. dif2.GT.astdv ) jeltn(i,2) = 0
                     IF ( acnt.EQ.0 .AND. dif1.LE.cstdv ) jeltn(i,2) = 0
                     IF ( acnt.EQ.0 .AND. dif1.GT.cstdv ) jeltn(i,2) = 1
                     IF ( ccnt.NE.0 .AND. acnt.NE.0 ) THEN
C
C....ASSIGN THE POINT TO THE SIDE WHOSE DIFFERENCE BETWEEN THE AVERAGE
C....VALUES AND DATA POINT VALUE LIES WITHIN ITS STANDARD DEVIATION.
C
                        IF ( dif1.LE.cstdv .AND. dif2.GT.astdv )
     &                        jeltn(i,2) = 1
                        IF ( dif1.GT.cstdv .AND. dif2.LE.astdv )
     &                        jeltn(i,2) = 0
                        IF ( dif1.LE.cstdv .AND. dif2.LE.astdv )
     &                        jeltn(i,2) = 1
                        IF ( dif1.GT.cstdv .AND. dif2.GT.astdv )
     &                        jeltn(i,2) = 2
                     ENDIF
C
                     IF ( jeltn(i,2).EQ.1 ) jeltn(i,3) = 0
                     IF ( jeltn(i,2).EQ.1 ) jeltn(i,4) = 0
                     IF ( jeltn(i,2).EQ.0 ) jeltn(i,3) = iflt
                     IF ( jeltn(i,2).EQ.0 ) jeltn(i,4) = 1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C
      ENDDO
C
      RETURN
      END
