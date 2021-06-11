C***********************************************************************
C
C     NAME:  REAL FUNCTION ISIDE
C
C     FUNCTION:  THIS ROUTINE DETERMINES IF A POINT (XP,YP) LIES ON THE
C                LEFT OR RIGHT SIDE OF A LINE SEGMENT.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  XP        I      REAL                   +REAL
C                  THE X COORDINATE OF THE DATA POINT
C                  -------------------------------------------------
C                  YP        I      REAL                   +REAL
C                  THE Y COORDINATE OF THE DATA POINT
C                  -------------------------------------------------
C                  NPNTS      I     INTEGER                +INTEGER
C                  NUMBER OF POINTS IN THE LINE FEATURE
C                  -------------------------------------------------
C                  WINDO      I    REAL ARRAY    (2,2)
C                  COORDINATES DEFINING MBR AROUND (XP,YP)
C                  -------------------------------------------------
C                  ICLIP      I     INTEGER
C                  THE RESULTS OF THE CLIP ROUTINE
C                  -------------------------------------------------
C
C     ROUTINES REQUIRED:  IFLINE, CLIP
C
C     CALLING ROUTINES:  FAULT, FCHECK1
C***********************************************************************
C
      REAL FUNCTION iside(xp,yp,npnts,windo,iclip,vect)
C
$Include:'xypnts.inc'
C
      REAL windo(2,2) , vect(4)
C
      iside = 0
C
C   GET THE 2 ENDPOINT INDICES THAT DEFINE THE CLOSEST LINE SEGMENT
C      TO THE CANDIDATE POINT.  THE INDICES ARE USED TO ACCESS (X,Y)
C      COORDINATES FROM THE DISK ARRAY
C
      CALL ifline(xp,yp,npnts,iseg1,iseg2,xval,yval)
C
      CALL spoints('X ',x,vect(1),iseg1,1)
      CALL spoints('Y ',y,vect(2),iseg1,1)
      CALL spoints('X ',x,vect(3),iseg2,1)
      CALL spoints('Y ',y,vect(4),iseg2,1)
c      VECT(1) = X(ISEG1)
c      VECT(2) = Y(ISEG1)
c      VECT(3) = X(ISEG2)
c      VECT(4) = Y(ISEG2)
      IF ( vect(1).GT.vect(3) .OR. vect(2).GT.vect(4) ) THEN
         IF ( vect(1).GT.vect(3) ) THEN
            r1 = vect(1)
            vect(1) = vect(3)
            vect(3) = r1
         ENDIF
         IF ( vect(2).GT.vect(4) ) THEN
            r1 = vect(2)
            vect(2) = vect(4)
            vect(4) = r1
         ENDIF
      ENDIF
C
C   CLIP THE LINE SEGMENT IF EITHER ENDPOINT LIES OUTSIDE THE
C     ROVING WINDOW.
C
      CALL clip(windo,vect,iclip)
C
      iside = ((vect(4)-vect(2))*(xp-vect(1)))
     &         - ((vect(3)-vect(1))*(yp-vect(2)))
C
      RETURN
      END
