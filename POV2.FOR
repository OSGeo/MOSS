      SUBROUTINE pov2(map,itp3,ichan3)
$Include:'io.inc'
$Include:'strng.inc'
      COMMON /table / itable(40)
$Include:'whmap.inc'
      DIMENSION map(10)
C
C ERASE THE SCREEN
C
C      CALL FLASH
C  GET ACTIVE DATA SETS
C
      CALL daget(icard,icardp,itable,40,macts)
      IF ( macts.LE.1 ) THEN
C
C  PROMPT USER FOR ACTIVE DATA SETS
C
 50      CONTINUE
         WRITE (nprnt,99001)
99001    FORMAT (' Enter active data set I.D.s to INTERSECT '\)
         CALL readin(icard,lenicd,icardp,istop)
         CALL daget(icard,icardp,itable,40,macts)
         lines = lines + 3
         IF ( macts.LT.2 ) GOTO 50
      ENDIF
C
C  ASK WHAT TO CALL IT
C
      CALL decip(icard,map,icardp)
      IF ( map(1).EQ.0 ) THEN
         CALL whtcal(map,ier)
      ENDIF
C
      IF ( ier.NE.0 ) STOP
C
C  INPUT IS FINISHED
C
C
      RETURN
      END
