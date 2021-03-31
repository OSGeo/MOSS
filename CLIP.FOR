C*********************************************************************
C
C     NAME:  SUBROUTINE CLIP
C
C     FUNCTION:  CLIPS A VECTOR IF IT EXTENDS BEYOND THE BOUNDS
C                OF A SPECIFIED PORT, AND INDICATES WHERE THE
C                VECTOR WAS CLIPPED.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  ------------------------------------------------
C                  DPORT    R       R*4          4          REAL
C                           MBR SPECIFYING VIEWPORT BOUNDARIES
C                  ------------------------------------------------
C                  DVCTR    R       R*4          4          REAL
C                           START & END POINTS OF VECTOR TO BE CLIPPED
C                  ------------------------------------------------
C                  ICLIP    I       I*2                    -1 TO 3
C                           FLAG TO INDICATE TYPE OF CLIPPING
C                          -1--INVISIBLE, NOT CLIPPED
C                           0--VISIBLE, NOT CLIPPED
C                           1--VISIBLE, CLIPPED LEAVING
C                           2--VISIBLE, CLIPPED ENTERING
C                           3--VISIBLE, CLIPPED ENTERING & LEAVING
C                  -----------------------------------------------
C
C**************************************************************************
C
      SUBROUTINE clip(dport,dvctr,iclip)
C
      REAL dport(2,2) , dvctr(1) , dxy(2) , dlmt , dcurr(4)
C
C**     DPORT(1,1) = XMAX   (1,2) = XMIN   (2,1) = YMAX   (2,2) = YMIN
C**     DVECTR(1-2) = X,Y START      (3-4) =X,Y END
C
      DO kk = 1 , 4
         dcurr(kk) = dvctr(kk)
      ENDDO
C
      dxy(1) = dvctr(1) - dvctr(3)
      dxy(2) = dvctr(2) - dvctr(4)
C
      iclip = 0
C
C...  TEST EACH COORDINATE
C
      DO i = 1 , 3 , 2
C
         nerr = 0
C
C...  TEST INDIVIDUAL ORDINATES
C
         DO j = 1 , 2
C
            k = j + i - 1
            m = 3 - i + j
C
C...  TEST UPPER LIMITS
            dlmt = dport(j,1)
C
C...  UPPER LIMIT OK?
            IF ( dvctr(k).LE.dlmt ) THEN
C
C
C...  TEST LOWER LIMITS
               dlmt = dport(j,2)
C
C...  LOWER LIMIT OK?
               IF ( dvctr(k).GE.dlmt ) GOTO 50
C
C...  LOWER LIMIT BAD?
               IF ( dvctr(m).LT.dlmt ) GOTO 20
C
C...  UPPER LIMIT BAD?
            ELSEIF ( dvctr(m).GT.dlmt ) THEN
               GOTO 20
            ENDIF
C
C...  ORDINATE OUT OF RANGE - ATTEMPT INTERSECTION
            nerr = nerr + 1
C
C...  INTERSECTION FAILURE?
            IF ( nerr.LE.2 ) THEN
C
               l = 3 - j
               dvctr(k) = dlmt
               mm5 = 5 - m
C
               dvctr(mm5) = (dxy(l)/dxy(j))*(dlmt-dcurr(j)) + dcurr(l)
C
               IF ( i.EQ.1 ) iclip = 2
               IF ( i.EQ.3 .AND. iclip.EQ.0 ) iclip = 1
C
               IF ( i.EQ.3 .AND. iclip.GE.2 ) iclip = 3
               GOTO 50
            ENDIF
C
C...  INTERSECTION FAILURE
C
 20         CONTINUE
            iclip = -1
C
            DO kk = 1 , 4
               dvctr(kk) = dcurr(kk)
C
            ENDDO
            GOTO 100
C
 50      ENDDO
      ENDDO
C
 100  CONTINUE
      RETURN
      END
