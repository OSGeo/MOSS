C**********************************************************************
C
C     NAME:  SUBROUTINE FBISEA
C
C     FUNCTION:  PERFORMS THE UTILITY OF SEARCHING THROUGH THE LIST OF
C                RECORDS STORED IN SORTED ORDER TO FIND THE LOCATION OF AN
C                EXISTING RECORD OR TO FIND THE LOCATION WHERE A NEW RECORD
C                SHOULD BE INSERTED IN SORTED ORDER.
C
C     PARAMETERS:  NAME     USE     TYPE     DIMENSION     RANGE
C                  -------------------------------------------------
C                  NRECS     I       I*2         1         +INTEGER
C                  NUMBER OF RECORDS IN FILE
C                  -------------------------------------------------
C                  ICHAN     I       I*2         1         +INTEGER
C                  CHANNEL THE FILE OF RECORDS IS OPEN ON
C                  -------------------------------------------------
C                  IOERR     I       I*2         1         +INTEGER
C                  CHANNEL TO WRITE ERROR MESSAGES ON
C                  -------------------------------------------------
C                  RECIN     I       R*4         NDIM      +/-REAL
C                  RECORD CONTAIN KEY TO SEARCH FOR
C                  -------------------------------------------------
C                  KLEN      I       I*2         1         +INTEGER
C                  NUMBER OF WORDS IN SEARCH KEY
C                  -------------------------------------------------
C                  NDIM      I       I*2         1         +INTEGER
C                  DIMENSION OF RECIN AND RECIO
C                  -------------------------------------------------
C                  MAPNO     O       I*2                   +INTEGER
C                  IF FOUND, LOCATION NUMBER OF RECORD
C                  IF NOT FOUND, MAPNO = 0
C                  -------------------------------------------------
C                  RECIO     O       R*4        NDIM       +/-REAL
C                  RETURNED RECORD IF MATCH IS FOUND
C                  -------------------------------------------------
C                  NEWLOC    O       I*2                   +INTEGER
C                  LOCATION NUMBER OF POSITION TO INSERT NEW RECORD
C                  -------------------------------------------------
C                  IER       O       I*2                   +INTEGER
C                  ERROR INDICATOR
C                  -------------------------------------------------
C
C     FILES:  CHANNELS LISTED ABOVE
C
C     ROUTINES REQUIRED:  DSIN
C
C     CALLING ROUTINES:  INTESC
C
C************************************************************************
C
      SUBROUTINE fbisea(nrecs,ichan,ioerr,recin,klen,ndim,mapno,recio,
     &                  newloc,ier)
      EXTERNAL dsin
      INTEGER*2 nrecs , ichan , ioerr , klen , ndim , mapno , newloc , 
     &          ier , lbound , ubound , indx
      REAL recin(ndim) , recio(ndim)
C
C......INITIALIZE VARIABLES .......
      lbound = 0
      ilenz = 16
C......UPPER BOUND IS #RECORDS + 1; IF NO RECORDS UPPER IS 1 .....
      ubound = nrecs + 1
      mapno = 0
      newloc = 0
      indx = 0
C
C......CHECK FOR COMPLETED SEARCH ......
 100  CONTINUE
      IF ( (ubound-lbound).LT.2 ) THEN
C
C......SET NEW LOCATION TO THE END OF FILE .......
         IF ( newloc.EQ.0 ) newloc = indx + 1
      ELSE
C...........SET INDEX TO MID-POINT BETWEEN BOUNDS ......
         indx = ((ubound+lbound)/2)
C...........INPUT THE INDEXED RECORD ......
         recn = indx
         CALL dsin(ichan,recio(1),recn,ier)
         IF ( ier.NE.0 ) THEN
C
C......ERROR RETURNS ......
C
            WRITE (ioerr,99001) ier
99001       FORMAT (' *FBISEA*  ERROR FROM DSIN  ERROR',I5)
         ELSE
C...........COMPARE RECORDS .....
            DO i = 1 , klen
               IF ( recin(i).LT.recio(i) ) THEN
C
C................RESET UPPER BOUND ......
                  newloc = indx
                  ubound = indx
                  GOTO 100
               ELSEIF ( recin(i).GT.recio(i) ) THEN
C...........RESET LOWER BOUND ......
                  lbound = indx
                  GOTO 100
               ENDIF
            ENDDO
C.....................RECIN IS FOUND, SET LOCATION ......
            mapno = indx
         ENDIF
      ENDIF
C
C......NORMAL RETURN ......
C
      RETURN
      END
