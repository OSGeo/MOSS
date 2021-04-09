      SUBROUTINE whtcal(newname,ier)
C
C *************************************************************
C
C         S U B R O U T I N E    W H T C A L
C
C  FUNCTION - ASK THE USER WHAT THEY WISH TO CALL A NEW SAVED
C             TEMP OR CELL FILE
C
C  PARAMETERS:  NEWNAME - OUT - NEW MAP NAME
C                   IER - OUT - ERROR  0 = OK
C
C *************************************************************
C
$Include:'IO.inc'
      DIMENSION newname(25)
      INTEGER string(80) , ifilr(13)
      CHARACTER*13 newmap
      LOGICAL iexist
      EQUIVALENCE (newmap,ifilr(1))
C
      ier = 0
C
 100  CONTINUE
      WRITE (nprnt,99001)
99001 FORMAT (' WHAT DO YOU WISH TO CALL THE NEW MAP (CR=QUIT)'\)
      CALL readin(string,80,istop,iend)
      kount = istop - iend
      IF ( string(1).EQ.0 ) THEN
         ier = -99
         RETURN
      ENDIF
c
c check for period in name - illegal
c
      DO l = 1 , iend
         IF ( string(l).EQ.46 ) THEN
            WRITE (*,*) ' Illegal map name - can not contain an extent.'
            WRITE (*,*) ' again - please'
            ier = 0
            GOTO 100
         ENDIF
      ENDDO
C
C  EXTRACT NAME FROM PATHNAME
C
      CALL decip(string,newname,istop)
      CALL packc(newname,ifilr,25)
C
C  Does this map file name already exist ?
c       inquire(file=newmap,exist=iexist)
c       if(iexist)then
c        write(*,114)newmap
c114     format(/,' File: ',a13,' already exists - overwrite Y/(N)'\)
c        call readin(iget,1,ic,is)
c        if(char(iget).ne.'Y')go to 5
c       endif
C
      RETURN
C
      END
