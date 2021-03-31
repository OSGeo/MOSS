      SUBROUTINE chkunts(icard,k,iunits,value)
      COMMON /io    / nprnt , ioin
      DIMENSION icard(1)
      LOGICAL mcomp
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C            S U B R O U T I N E   C H K U N T S
C
C
C  FUNCTION:  THIS ROUTINE CHECKS USER SPECIFIED UNITS
C             OF MEASUREMENT ON ANY DISTANCE INPUT
C             COMMAND STRINGS.
C
C  INPUT PARAMETERS:
C
C             ICARD   = INPUT COMMAND STRING OF CHARACTER
C                       DATA IN RIGHT JUSTIFIED FORMAT
C
C             K       = POINTER INTO ARRAY ICARD OF CHECK
C                       UNITS LOCATION.
C
C             IUNITS  = RETURNED UNITS OF MEASUREMENT.
C
C             VALUE   = IN = VALUE TO CHANGE
C                       OUT = VALUE IN PROPER UNITS (METERS)
C
C                       -1    = ILLEGAL UNITS OF MEASURE
C                        0 ( )= MILES
C                        1 (F)= FEET
C                        2 (I)=INCHES
C                        3 (M)=METERS
C                        4 (K)=KILOMETERS
C * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      iunits = -1
      IF ( icard(k).EQ.0 .OR. icard(k).EQ.12 ) iunits = 0
      IF ( mcomp(icard(k),' ',1,ier) ) iunits = 0
      IF ( mcomp(icard(k),'F',1,ier) ) iunits = 1
      IF ( mcomp(icard(k),'I',1,ier) ) iunits = 2
      IF ( mcomp(icard(k),'M',1,ier) ) iunits = 3
      IF ( mcomp(icard(k),'K',1,ier) ) iunits = 4
C
      IF ( iunits.EQ.-1 ) THEN
C
C
c      WRITE(NPRNT,1000)
c 1000 FORMAT(' Illegal units of measure.')
c
         RETURN
      ELSE
         IF ( iunits.GT.0 ) k = k + 1
C
C  CHANGE VALUE ACCORDING TO UNITS OF INPUT
C
         IF ( iunits.EQ.0 ) value = value*1609.34
         IF ( iunits.EQ.1 ) value = (value/5280)*1609.34
         IF ( iunits.EQ.2 ) value = (value/(5280.*12.))*1609.34
         IF ( iunits.EQ.4 ) value = value*1000.
C
         RETURN
      ENDIF
      END
