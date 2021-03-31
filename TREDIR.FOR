      SUBROUTINE tredir(first,last,nmaps,ichan,jcom,ier)
      COMMON /dirsy / lastc , lastbk , iwtrn , kdir(256)
      COMMON /io    / nprnt , ioin
      DIMENSION name(16) , irec(16) , ifile(10) , ihead(256) , jcom(25)
      INTEGER first
C
C  MAKE SURE WE READ THE BLOCK
C
      lastc = -1
      iwtrn = -1
C
C**     READ THE # OF MAPS & CHECK FOR ERROR
C
      CALL getmap(ichan,1,irec,ier)
      IF ( ier.NE.0 ) THEN
C
C**     NEW FILE
C
         WRITE (nprnt,99001) (jcom(i),i=1,25)
99001    FORMAT (' INITIALIZING Project File: ',25A1)
         ier = 0
         nmaps = 0
         first = 2
         last = 1
         CALL clear(irec,16)
         irec(16) = last
         CALL putmap(ichan,1,irec,ier)
         IF ( ier.NE.0 ) THEN
            WRITE (nprnt,99002) ier
99002       FORMAT (' *TREDIR*  ERROR FROM PUTMAP ',I5)
         ELSE
            WRITE (ichan,REC=1) kdir
         ENDIF
      ELSE
C
C**     CHECK FOR REFORMAT OR REGULAR OPEN
C
         first = 2
         last = irec(16)
         nmaps = last - 1
      ENDIF
C
      RETURN
      END
